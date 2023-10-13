;;; spiel.el --- Interactive Fiction Library  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;
(require 'cl-lib)
(require 'spiel-structs)

(defgroup spiel nil "Interactive Fiction Library." :group 'applications :prefix "spiel-")

(defface spiel-command
  '((t (:inherit success :weight bold)))
  "Face for printed user commands.")

(defface spiel-current-room
  '((t (:inherit success :height 2.0)))
  "Face for current room.")

(defface spiel-command-context
  '((t (:inherit font-lock-comment-face :height 0.95 :weight bold :slant italic)))
  "Face for command context.")

(defface spiel-question
  '((t (:slant italic :weight bold)))
  "Face for game questions.")

;;(defcustom spiel-collection nil "Alist of form (TITLE . INIT-FN)." :type 'alist)

(defcustom spiel-want-typing t "When non-nil, simulate animate output." :type 'boolean)
(defcustom spiel-type-delay 0.0045 "Seconds between each printed character."
  :type (or 'float 'integer))

(defcustom spiel-command-aliases
  '(("r" . "reset")
    ("q" . "quit")
    ("n" . "go north")
    ("s" . "go south")
    ("e" . "go east")
    ("w" . "go west"))
  "Alist of form: (ALIAS . EXPANSION)."
  :type 'alist)

(defconst spiel--unlimited-capacity most-positive-fixnum)

(defconst spiel--prepositions '("in" "on" "at" "around" "behind" "inside" "beside"))

(defvar-local spiel-buffer "*game*")
(defvar-local spiel-entities nil "List of game entities.")
(defvar-local spiel-player nil "Game player object.")
(defvar-local spiel-pending-question nil)
(defvar-local spiel-print-cursor-timer nil "Timer to debounce post-printing cursor display.")

(defmacro spiel-entity-name (entity)
  "Return ENTITY's name."
  `(car (spiel-named<-names (spiel-ensure-entity ,entity))))

(defmacro spiel-context-get (entity key)
  "Return ENTITY's context slot value matching KEY."
  `(let ((o (spiel-ensure-entity ,entity)))
     (alist-get ,key (spiel-entity<-context o) nil nil #'equal)))

(defun spiel--look (pattern)
  "Look PATTERN."
  (let ((name (spiel-entity-name spiel-player)))
    (pcase pattern
      ((or '("inventory") `((or "in" "at") "inventory"))
       (spiel--describe-inventory spiel-player))
      ('("away") (format "%s averts his eyes." name))
      ('("out") (format "%s jolts and says \"huh!?\"" name))
      ('("inward") (spiel--look `("in" ,(list spiel-player))))
      (`(,(or "at" "in" "behind")) "Be more specific...")
      (`(,(or "at" "in") "room") (spiel-room-description))
      ((or `(,(or "around" "here" "room")) 'nil) (spiel-room-description))
      ((and strings (guard (cl-every #'stringp strings)))
       (format "%s cant do that." name))
      (`("at" . ,rest) (spiel--look (car rest)))
      (`(,(or "in" "into" "inside") ,objs)
       (spiel--disambiguate
        objs #'spiel-object-in-room-p
        (lambda (obj)
          (if (not (spiel-object-p obj))
              (format "%s can't do that." name)
            (cond
             ((eq obj spiel-player) (format "%s takes a deep breath and looks inward..." name))
             ((spiel-actor-p obj) (format "%s does not have x-ray vision." name))
             (t (spiel--describe-inventory obj)))))))
      ((or (and `(,objs) (guard (spiel-objects-p objs))) (and objs (pred spiel-objects-p)))
       (spiel--disambiguate objs #'spiel-object-in-room-p #'spiel--look))
      ((or (and obj (pred spiel-object-p) (pred spiel-object-in-room-p))
           (and `(,obj) (guard (and (spiel-object-p obj) (spiel-object-in-room-p obj)))))
       (or (spiel--do pattern obj)
           (spiel-object<-details obj)
           (spiel-object<-description obj)
           ;;@FIX: Replace with non-developer message.
           (format "@TODO: give %s details or description" (spiel-object<-id obj))))
      (_ (format "%s can't do that." name)))))

(defvar spiel-go-hook nil "Hook run after player changes destination via the \"go\" verb.")
(defun spiel--go (pattern)
  "Go PATTERN."
  (pcase pattern
    ('nil "Where?")
    ;;@MAYBE: implement "back" to go to last location?
    (_ (if-let ((destination (spiel--do pattern (spiel-object-room))))
           (progn
             (spiel-object-give destination spiel-player)
             (run-hooks 'spiel-go-hook)
             (spiel-print "\n" (spiel-room-description) "\n\n")
             (spiel-insert-prompt)
             (throw 'turn-over t))
         (format "Can't go %S." (spiel--pattern-to-query pattern))))))

(defvar-local spiel-verbs
    (list
     (spiel-verb :names '("look" "glance" "gaze" "stare" "see" "peer" "peek" "watch" "examine" "describe" "study" "inspect" "scan" "scrutinize")
                 :actions #'spiel--look)
     (spiel-verb :names '("say" "tell" "shout" "whisper" "tell"))
     (spiel-verb :names '("go" "move" "walk" "run" "crawl")
                 :actions #'spiel--go)
     (spiel-verb :names '("attack" "hit"))
     (spiel-verb :names '("inventory"))
     (spiel-verb :names '("pull"))
     (spiel-verb :names '("use"))
     (spiel-verb :names '("quit") :actions (lambda (_) (spiel-quit)))
     (spiel-verb :names '("reset") :actions (lambda (_) (spiel-reset)))
     (spiel-verb :names '("give"))))

(defun spiel-create-entity (type &rest args)
  "Return entity of TYPE with ARGS."
  (cl-loop for key in '(:names :adjectives) do
           (when-let ((declared (plist-get args key))
                      ((not (listp declared))))
             (setq args (plist-put args key (list declared)))))
  (let* ((id (or (plist-get args :id)
                 (when-let ((name (car (plist-get args :names))))
                   (intern (replace-regexp-in-string " +" "-" (downcase name))))))
         (found (cl-find id spiel-entities :key #'spiel-entity<-id)))
    (if (member this-command '(eval-last-sexp eval-buffer eval-defun))
        (setq spiel-entities (cl-remove id spiel-entities :key #'spiel-entity<-id))
      (when found (error "Duplicate entity id %S %S" id found)))
    (setq args (plist-put args :id id))
    (car (push (apply (intern (format "spiel-%s" (symbol-name type))) args) spiel-entities))))

(defun spiel-ensure-entity (id-or-entity)
  "Return ENTITY from ID-OR-ENTITY."
  (if (spiel-entity-p id-or-entity) id-or-entity
    (unless (symbolp id-or-entity) (signal 'wrong-type-argument `(symbolp ,id-or-entity)))
    (cl-find id-or-entity spiel-entities :key #'spiel-entity<-id)))

(defvar spiel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'spiel-send-input)
    map))

(define-derived-mode spiel-mode fundamental-mode "game"
  "Major mode for inputting commands."
  ;;@FIX: Should this be an actor?
  (spiel-create-entity 'actor :id 'prompt))

(defun spiel-display ()
  "Display game's output buffer."
  (with-current-buffer (get-buffer-create spiel-buffer)
    (pop-to-buffer (current-buffer)
                   '((display-buffer-reuse-window display-buffer-same-window)))))

(defun spiel-prompt-string (format &optional placeholder)
  "Return PROMPT string from FORMAT.
If PLACEHOLDER is non-nil, prompt's display is set to that."
  (let ((segments (string-split format "|" 'omit-nulls))
        (prompt (propertize " " 'rear-nonsticky t)))
    (when (> (length segments) 2)
      (error "Ambiguous prompt format: %S" format))
    (add-text-properties 0 (length prompt) `(display ,(or placeholder "")) prompt)
    (concat (propertize (car segments) 'read-only t 'rear-nonsticky t 'front-sticky t)
            prompt
            (when (cadr segments) (propertize (cadr segments) 'read-only t)))))

(defun spiel--goto-prompt ()
  "Position point at editable portion of prompt."
  (with-current-buffer spiel-buffer
    (goto-char (cadr (spiel--input-region)))))

(defun spiel--prompt-pending-p ()
  "Return t if prompt is pending, otherwise nil."
  (and spiel-pending-question
       (eq (spiel-question<-asker spiel-pending-question) 'prompt)))

;;@FIX: How to make generic?
(defun spiel-insert-prompt (&optional speech)
  "Insert prompt.
IF SPEECH is non-nil insert speaker prompt."
  (with-current-buffer (get-buffer-create spiel-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (spiel-prompt-string
               (if speech
                   (format "\n%s \"|\""
                           (if-let ((avatar (spiel-actor<-avatar spiel-player)))
                               (propertize " " 'display avatar)
                             (format "%s:" (car (spiel-actor<-names spiel-player)))))
                 (propertize "> " 'face 'spiel-command))
               (when speech (propertize "..." 'face 'font-lock-comment-face))))
      (spiel--goto-prompt))))

(defvar spiel-initialize-hook nil "Hook run when game started or reset.")
(defun spiel-initialize ()
  "Set initial state of the game."
  (run-hooks 'spiel-initialize-hook)
  t)

(defun spiel-reset ()
  "Reset game."
  (interactive)
  (spiel-initialize)
  (ignore-errors (throw 'turn-over t)))

(defun spiel-object-room (&optional entity)
  "Return ENTITY's room. ENTITY defaults to player."
  (let ((obj (spiel-ensure-entity (or entity spiel-player)))
        container last)
    (while (setq last container container (spiel-ensure-entity (cdr (spiel-object<-location obj))))
      (setq obj container))
    last))

(defun spiel-object-inventory (object &optional _recursive)
  "Return list of objects in OBJECT.
If RECURSIVE is non-nil, return inventory of all returned objects."
  (cl-loop with id = (if (symbolp object) object (spiel-object<-id object))
           with target = (cons 'in id)
           for obj in (cl-remove-if-not #'spiel-object-p spiel-entities)
           when (equal (spiel-object<-location obj) target)
           collect obj))

(defun spiel-object-has-p (subject predicate)
  "Return t if SUBJECT object has PREDICATE object."
  (eq (cdr (spiel-object<-location (spiel-ensure-entity predicate)))
      (spiel-object<-id (spiel-ensure-entity subject))))

(defun spiel-object-capacity (object)
  "Return OBJECT's numeric capcity."
  (let* ((declared (spiel-object<-capacity (spiel-ensure-entity object))))
    (cond ((eq declared t) most-positive-fixnum)
          ((null declared) 0)
          (t declared))))

;;@FIX: consider object sizes, allow "in" or "on"?
(defun spiel-object-give (object &rest objects)
  "Move OBJECTS to OBJECT's inventory."
  (cl-loop with destination = (spiel-ensure-entity object)
           with capacity = (spiel-object-capacity destination)
           with inventory = (spiel-object-inventory destination)
           for e in (mapcar #'spiel-ensure-entity objects)
           do (if (> (+ (spiel-object-capacity e) (length inventory)) capacity)
                  (error "Cannot fit %s" (spiel-object<-id e))
                (setf (spiel-object<-location e) (cons 'in (spiel-object<-id destination))))))

(defun spiel--type (&rest strings)
  "Type STRINGS in game's output buffer."
  (with-current-buffer (get-buffer-create spiel-buffer)
    (let ((inhibit-read-only t))
      (cl-loop for string in strings
               for tokens = (split-string string "" 'omit-nulls)
               do (cl-loop for token in tokens
                           do (goto-char (point-max))
                           (insert token)
                           (set-window-point (get-buffer-window spiel-buffer) (point-max))
                           (sit-for spiel-type-delay))))))

(defun spiel-print (&rest args)
  "Print ARGS in the GAME's output buffer."
  (setq args (mapcar (lambda (el) (propertize el 'read-only t)) (delq nil args)))
  (when spiel-print-cursor-timer (cancel-timer spiel-print-cursor-timer))
  (with-current-buffer (get-buffer-create spiel-buffer)
    (let ((inhibit-read-only t))
      (internal-show-cursor (selected-window) nil) ;;hide cursor
      (unwind-protect
          (progn (goto-char (point-max))
                 (apply (if spiel-want-typing #'spiel--type #'insert) args)
                 (set-window-point (get-buffer-window spiel-buffer) (point-max)))
        (setq spiel-print-cursor-timer
              (run-at-time spiel-type-delay nil
                           (lambda (window) (internal-show-cursor window t)) (selected-window))) ;;show cursor
        nil))))

(defun spiel-say (actor string)
  "Print ACTOR dialogue STRING."
  (let* ((actor (spiel-ensure-entity actor))
         (indicator (or (spiel-actor<-avatar actor)
                        (when-let ((name (spiel-entity-name actor)))
                          (propertize (format "%s:" name) 'face '(:weight bold)))
                        ""))
         (avatar (propertize " " 'display indicator)))
    (spiel-print (format "\n%s %s" avatar string))))

(defun spiel-ask (id &optional entity)
  "Set `spiel-pending-question' to question with ID.
If ENTITY is non-nil, set question asker."
  (let* ((q (spiel-ensure-entity id))
         (speaker (spiel-question<-asker q))
         (i 0))
    (when entity (setf (spiel-question<-asker q) (spiel-ensure-entity entity)))
    (setq spiel-pending-question q)
    (with-current-buffer spiel-buffer
      (when-let ((question (propertize (spiel-question<-text q) 'face 'spiel-question))
                 (speaker))
        (spiel-say speaker question)
        (spiel-print "\n"))
      (when-let ((options (spiel-question<-options q)))
        (spiel-print (propertize "  options:\n" 'face 'spiel-command-context))
        (mapc (lambda (option)
                (spiel-print (format "  %d. %s\n"
                                     (cl-incf i)
                                     (if (stringp option) option (car option)))))
              options)))))

(defun spiel--verb (name)
  "Return VERB object with NAME."
  (cl-find name spiel-verbs :key #'spiel-verb<-names :test #'member))

;;@OPTIMIZE: lots of downcasing here and in parser.
(defun spiel-objects-matching (string slot)
  "Return list of objects where STRING is member of SLOT."
  (cl-remove-if-not (apply-partially #'member (downcase string))
                    (cl-remove-if-not #'spiel-object-p spiel-entities)
                    :key (lambda (o) (mapcar #'downcase (funcall slot o)))))

(defun spiel--pattern (string)
  "Return ojbects pattern from STRING."
  (let ((tokens (string-split string " " 'omit-nulls))
        (escapep nil))
    (when (string-prefix-p "/" (car tokens))
      (setf escapep t (car tokens) (substring (car tokens) 1)))
    (if-let (((or (not spiel-pending-question) escapep))
             (alias (alist-get (car tokens) spiel-command-aliases nil nil #'equal)))
        (let ((spiel-pending-question nil))
          (spiel--pattern alias))
      (if (and spiel-pending-question (not escapep))
          (list spiel-pending-question tokens)
        (cl-loop
         with (acc verb described result)
         for token in tokens
         unless (member token '("the")) do
         (setq acc (string-trim (concat acc (when acc " ") token)))
         (cond ((null verb)
                (when-let ((v (spiel--verb (downcase acc))))
                  (push v result)
                  (setq verb t acc nil)))
               ((member token spiel--prepositions)
                (push token result)
                (setq acc nil))
               (t (if-let ((named (spiel-objects-matching (downcase acc) #'spiel-object<-names))
                           (possible (if described (cl-intersection described named) named)))
                      (setq possible
                            (mapc (lambda (o) (setf (spiel-named<-as o) acc)) possible)
                            result (cons possible result)
                            acc nil described nil)
                    (if-let ((possible (spiel-objects-matching (downcase acc) #'spiel-object<-adjectives)))
                        (setq described
                              (mapc (lambda (o)
                                      (let ((ctx (spiel-object<-context o)))
                                        (unless (functionp ctx)
                                          (setf (alist-get 'as (spiel-object<-context o)) acc))))
                                    possible)
                              acc nil)))))
         finally (when (and acc (not (string-empty-p acc))) (push acc result))
         finally (when described (push (car described) result))
         finally return (nreverse result))))))

(defun spiel-room-description (&optional room)
  "Return ROOM description. ROOM defaults to player's current room."
  (let ((room (spiel-ensure-entity (or room (spiel-object-room)))))
    (unless (spiel-room-p room) (signal 'wrong-type-argument `(room ,room)))
    (string-join
     (cons (spiel-object<-description room)
           (delq nil (mapcar (lambda (o) (spiel-object<-description o))
                             (cl-remove spiel-player (spiel-object-inventory room)))))
     "\n")))

(defun spiel-objects-p (object)
  "Return t if OBJECT is a list of `spiel-object' structs."
  (and (consp object) (cl-every #'spiel-object-p object)))

(defun spiel--pattern-to-query (pattern)
  "Return qeury from PATTERN."
  (mapconcat (lambda (el) (cond
                           ((spiel-named-p el) (spiel-named<-as el))
                           ((spiel-objects-p el) (spiel-named<-as (car el)))
                           (t el)))
             (if (spiel-object-p pattern) (list pattern) pattern) " "))

(defun spiel--disambiguation-prompt (objs)
  "Set up disambiguation prompt for OBJS."
  (setf spiel-entities (cl-remove 'disambiguation spiel-entities :key #'spiel-entity<-id))
  (spiel-create-entity 'question
                       :id 'disambiguation
                       :asker 'prompt
                       :text (format "Which one?")
                       :options
                       (cl-loop for obj in objs collect
                                (cons (concat (car (spiel-object<-adjectives obj))
                                              " "
                                              (car (spiel-object<-names obj)))
                                      obj)))
  (spiel-ask 'disambiguation))

(defun spiel--disambiguate (things &optional filter callback ask)
  "Eval CALLBACK on THINGS.
FILTER is a function which takes an object.
If FILTER returns non-nil for that object it is considered as a candidate.
If ASK is non-nil, prompt user to disambiguate and return t."
  (let ((cb (or callback #'identity)))
    (cond
     ((or (stringp things) (spiel-object-p things) (null things))
      (funcall cb things))
     ((and things (cl-every #'stringp things)) (string-join things " "))
     ((and (not ask) (or (spiel-objects-p things) (spiel-objects-p (car things))))
      (unless (spiel-object-p (car things)) (setq things (car things)))
      (if (= (length things) 1)
          (funcall cb (car things))
        (if-let ((filtered (cl-remove-if-not (or filter #'always) things)))
            (spiel--disambiguate filtered nil callback (> (length filtered) 1))
          (funcall cb (list (spiel-object<-as things))))))
     (ask
      (setf (spiel-object<-actions (spiel-ensure-entity 'prompt)) cb)
      (spiel--disambiguation-prompt things) t))))

(defun spiel--describe-inventory (obj)
  "Describe OBJ's inventory."
  (let ((actorp (spiel-actor-p obj)))
    (if-let ((name (spiel-entity-name obj))
             (inventory (spiel-object-inventory obj)))
        (format "%s %s:\n  - %s" name
                (if actorp "is holding" "contains")
                (mapconcat (lambda (it)
                             (concat
                              (when-let ((adj (car (spiel-object<-adjectives it))))
                                (concat adj " "))
                              (car (spiel-object<-names it))))
                           inventory "\n  -"))
      (format (if actorp "%s is not holding anything."
                "There is nothing inside the %s")
              name))))

(defun spiel-object-in-room-p (object &optional room)
  "Return t if OBJECT is in ROOM, otherwise nil."
  (equal (spiel-object-room (spiel-ensure-entity object))
         (spiel-ensure-entity (or room (spiel-object-room)))))

(defun spiel--default-actions (pattern)
  "Call default function for PATTERN."
  (pcase pattern
    (`(,(and (pred spiel-question-p)) . ,_) (spiel--answer pattern))
    ((and `(,verb . ,rest) (guard (spiel-verb-p verb)))
     (or (spiel--do rest verb)
         (spiel--do (spiel--pattern-to-query pattern))))
    (_ (format "%s can't %S" (spiel-entity-name spiel-player)
               (string-join pattern " ")))))

(defun spiel--do (pattern &optional entity)
  "Do PATTERN with ENTITY."
  (if-let ((entity)
           (e (spiel-ensure-entity entity)))
      (when-let (((spiel-named-p e))
                 (actions (spiel-named<-actions e)))
        (funcall actions pattern))
    (spiel--default-actions pattern)))

(defun spiel--answer (pattern)
  "Answer question PATTERN."
  (let* ((q (spiel-ensure-entity (car pattern)))
         (asker (spiel-question<-asker q))
         (options (spiel-question<-options q)))
    (if-let (((eq asker 'prompt))
             (response (string-join (cadr pattern) " ")))
        (progn
          (setq response
                (if-let ((n (ignore-errors (read response)))
                         ((numberp n))
                         (found (nth (1- n) options)))
                    (cdr found)
                  (alist-get response options nil nil
                             (lambda (v k) (string-match-p k v))))
                spiel-pending-question nil)
          (if response
              (spiel--do response asker)
            (spiel-ask q)))
      (spiel--do pattern asker))))

(defun spiel--input-region ()
  "Return list of form (START END) for input region."
  (with-current-buffer spiel-buffer
    (let ((start nil))
      (save-excursion
        (goto-char (point-max))
        (goto-char (line-beginning-position))
        (while (get-text-property (point) 'read-only) (forward-char))
        (setq start (point))
        (while (not (or (get-text-property (point) 'read-only) (eobp)))
          (forward-char))
        (list start (point))))))

(defun spiel-send-input ()
  "Send the input from input-buffer."
  (interactive)
  (let* ((region (spiel--input-region))
         (raw (apply #'buffer-substring-no-properties region))
         (input (string-trim (replace-regexp-in-string " +"  " " raw))))
    (unless (or (and spiel-pending-question
                     (not (eq (spiel-question<-asker spiel-pending-question) 'prompt)))
                (> (length input) 0))
      (user-error "No input"))
    (delete-region (car region) (cadr region))
    (let ((display (or (unless spiel-pending-question
                         (alist-get input spiel-command-aliases nil nil #'equal))
                       input)))
      (insert (propertize (if (string-empty-p input) "..." display) 'face
                          (if spiel-pending-question 'spiel-question 'spiel-command)
                          'read-only t)))
    (catch 'turn-over
      (when-let ((result (spiel--do (spiel--pattern input)))
                 ((stringp result)))
        (spiel-print "\n" result "\n\n"))
      (spiel-insert-prompt (and spiel-pending-question (not (spiel--prompt-pending-p)))))))

(defun spiel-quit ()
  "Quit game."
  (spiel-print "\nQuitting game...\n")
  (sit-for 0.5)
  (kill-buffer spiel-buffer)
  (setq spiel-entities nil)
  (ignore-errors (throw 'turn-over t)))

;; ;;;###autoload
;; (defun spiel-load (file)
;;   "Load a game FILE."
;;   (interactive (list (let* ((choices (or spiel-collection (user-error "No games registered")))
;;                             (choice (completing-read "Load game: " choices nil t)))
;;                        (alist-get choice spiel-collection nil nil #'equal))))
;;   (load file nil 'no-message))

(provide 'spiel)
;;; spiel.el ends here

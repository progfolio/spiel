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
  '(("^r$" "reset")
    ("^q$" "quit")
    ("^i$" "describe inventory")
    ("^n$" "go north")
    ("^s$" "go south")
    ("^e$" "go east")
    ("^w$" "go west")
    ("\\(?:\\(?:^\\|[[:space:]]+\\)l\\(?:$\\|[[:space:]]+\\)\\)" "look ")
    ("^o " "open ")
    ("@\\(.*\\)" "look at \\1" 'fixed-case)
    ("^la " "look at ")
    ("^li " "look in ")
    ("^x " "examine "))
  "Alist of form: (REGEXP . ARGS).
Args are applied to `replace-regexp-in-string' sans REGEXP and target string."
  :type 'alist)

(defconst spiel--unlimited-capacity most-positive-fixnum)
(defconst spiel--vowels '("a" "e" "i" "o" "u" "A" "E" "I" "O" "U"))
(defconst spiel--articles '("the" "a" "an"))

(defvar-local spiel-buffer "*game*")
(defvar-local spiel-entities nil "List of game entities.")
(defvar-local spiel-player nil "Game player object.")
(defvar-local spiel-pending-question nil)
(defvar-local spiel-print-cursor-timer nil "Timer to debounce post-printing cursor display.")
(defvar-local spiel-input-history nil)
(defvar-local spiel-last-parsed nil)
(defvar-local spiel-last-input nil)

;;@COMPAT: copied from Emacs 29 `string-equal-ignore-case'.
(defsubst spiel--string-equal (string1 string2)
  "Compare STRING1 and STRING2 case-insensitively."
  (declare (side-effect-free t))
  (eq t (compare-strings string1 0 nil string2 0 nil t)))

(defun spiel-capitalize (string)
  "Return copy of STRING with first letter capitalized."
  (setf (aref string 0) (upcase (aref string 0)))
  string)

(defun spiel-entity-name (entity)
  "Return ENTITY's name."
  (car (spiel-named<-names (spiel-ensure-entity entity))))

(defmacro spiel-context-get (entity key)
  "Return ENTITY's context slot value matching KEY."
  `(let ((o (spiel-ensure-entity ,entity)))
     (alist-get ,key (spiel-entity<-context o) nil nil #'equal)))

(defun spiel-linking-verb (strings)
  "Return linking verb for STRINGS."
  (if (consp strings)
      (if (= (length strings) 1) "is" "are")
    (if (or (member (downcase strings) '("you" "they" "we")))
        "are" (if (equal strings "I") "am" "is"))))

(defun spiel-indefinite-article (string)
  "Return indefinite article for STRING."
  (if (member (substring string 0 1) spiel--vowels) "an" "a"))

(defun spiel-object-noun-phrase (object)
  "Return OBJECT's first adjective and first name."
  (let* ((o (spiel-ensure-entity object))
         (adj (car (spiel-object<-adjectives o))))
    (concat adj (when adj " ") (spiel-entity-name o))))

(defun spiel-determined-object-phrase (object &optional singular)
  "Return a indefinite article and noun phrase for OBJECT.
If SINGULAR is non-nil, use the singular form."
  (let ((phrase (spiel-object-noun-phrase object)))
    (concat (if singular "the" (spiel-indefinite-article phrase)) " " phrase)))

(defvar spiel-singular-enumeration nil)
(defun spiel-enumeration (&rest objects)
  "Return string listing OBJECTS."
  (setq objects (mapcar #'spiel-ensure-entity objects))
  (let* ((count (length objects))
         (first (pop objects))
         (last (car (last objects))))
    (concat (spiel-determined-object-phrase first spiel-singular-enumeration)
            (if (= count 2) " " (when (> count 2) ", "))
            (mapconcat #'spiel-object-noun-phrase (butlast objects) ", ")
            (when (> count 2) ", ")
            (when last (concat "and " (spiel-determined-object-phrase last))))))

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
      ((or `(,(or "at" "in") ,(or "room" "here" "around"))
           `(,(or "around" "here" "room"))
           (and room (pred spiel-room-p))
           'nil)
       (spiel-room-description))
      (`("at" . ,rest) (spiel--look (car rest)))
      (`(,(or "in" "into" "inside") ,objs)
       (spiel--disambiguate
        objs #'spiel-object-in-room-p
        (lambda (obj)
          (when (spiel-object-p obj)
            (or
             (spiel--do "look in" obj)
             (cond
              ((spiel-context-get obj 'closed) "It's closed.")
              ((eq obj spiel-player) (format "%s takes a deep breath and looks inward..." name))
              ((spiel-actor-p obj) (format "%s does not have x-ray vision." name))
              (t (spiel--describe-inventory obj))))))))
      ((or (and `(,objs) (guard (spiel-objects-p objs))) (and objs (pred spiel-objects-p)))
       (spiel--disambiguate objs #'spiel-object-in-room-p #'spiel--look))
      ((or (and obj (pred spiel-object-p) (pred spiel-object-in-room-p))
           (and `(,obj) (guard (and (spiel-object-p obj) (spiel-object-in-room-p obj)))))
       (or (spiel--do "look" obj)
           (concat
            (or (spiel-object<-details obj)
                (spiel-object<-description obj))
            (when-let (((not (spiel-context-get obj 'closed)))
                       (inventory (spiel--describe-inventory obj))
                       ;;@FIX: shouldn't be this ad-hoc
                       ((not (equal inventory "It's Empty."))))
              (concat "\n" inventory)))
           ;;@FIX: Replace with non-developer message.
           (format "@TODO: give %s details or description" (spiel-object<-id obj)))))))

(defvar spiel-go-hook nil "Hook run after player changes destination via the \"go\" verb.")
(defun spiel--go (pattern)
  "Go PATTERN."
  (pcase pattern
    ('nil "Where?")
    ;;@MAYBE: implement "back" to go to last location?
    (_ (when-let ((destination (spiel--do pattern (spiel-object-room))))
         (progn
           (spiel-object-put 'in destination spiel-player)
           (run-hooks 'spiel-go-hook)
           (spiel-print "\n" (spiel-room-description) "\n\n")
           (spiel-insert-prompt)
           (throw 'turn-over t))))))

(defun spiel-flagged-p (object &rest flags)
  "Return t if all FLAGS are non-nil in OBJECT's context."
  (cl-every (lambda (flag) (spiel-context-get object flag)) flags))

(defun spiel-movable-p (object)
  "Return t if OBJECT can be moved, otherwise nil."
  (let* ((location (spiel-object<-location (spiel-ensure-entity object)))
         (container (cdr location)))
    (not (or (spiel-context-get object 'immobile)
             (and (eq (car location) 'in)
                  (spiel-flagged-p container 'immobile 'closed))))))

(defun spiel--put (pattern)
  "Put PATTERN."
  (let ((name (spiel-entity-name spiel-player)))
    (pcase pattern
      ('nil "Put what?")
      ((and `(,obj) (guard (spiel-object-p obj))) "Where?")
      ((or (and `(,obj "on") (guard (spiel-object-p obj)))
           (and `("on" ,obj) (guard (spiel-object-p obj))))
       (cond
        ((equal (spiel-object<-location obj) `(on . ,(spiel-object<-id spiel-player)))
         (format "%s is already wearing %s." name (spiel-entity-name obj)))
        ((not (spiel-object-has-p spiel-player obj))
         (format "%s %s have %s." name
                 (if (member name '("You" "They")) "don't" "doesn't")
                 (spiel-object<-as obj)))
        ((not (spiel-flagged-p obj 'wearable))
         (format "Can't wear %s." (spiel-object<-as obj)))
        ((spiel-object-has-p spiel-player obj)
         (or (spiel--do (list "on" spiel-player) obj)
             (progn
               (setf (spiel-object<-location obj) (cons 'on (spiel-object<-id spiel-player)))
               (format "%s put on the %s." name (spiel-entity-name obj)))))))
      ((and `(,source ,(and (or "in" "on") where) ,destination)
            (guard (cl-every #'spiel-object-p (list source destination))))
       (let ((source-name (spiel-entity-name source))
             (destination-name (spiel-entity-name destination))
             (location (intern where)))
         (cond
          ((or (equal source destination)
               (member destination (append (spiel-object-inventory 'in source)
                                           (spiel-object-inventory 'on source))))
           nil)
          ((spiel-context-get destination 'closed)
           (format "%s is closed."
                   (spiel-capitalize (spiel-determined-object-phrase destination 'singular))))
          ((not (alist-get location (spiel-object<-capacity destination))) nil)
          ((spiel-object-has-p destination source)
           (format "%s %s %s."
                   (capitalize source-name)
                   (if (equal where "on") "is already on" "already has")
                   (downcase destination-name)))
          ;; ((not (spiel-object-has-p spiel-player source))
          ;;  (format "%s doesn't have %s." name source-name))
          (t ;;@TODO: catch error
           (spiel-object-put (intern where) destination source))))))))

(defun spiel--take (pattern)
  "Take PATTERN."
  (let ((name (spiel-entity-name spiel-player)))
    (pcase pattern
      ('nil "Take what?")
      ((and `("all" ,(and (or "in" "on" "from") location) ,obj) (guard (spiel-object-p obj)))
       (let* ((load (spiel-object-inventory 'on obj))
              (location (if (not (equal location "from"))
                            location
                          (if load 'on 'in))))
         (cl-loop
          for o in (spiel-object-inventory location obj)
          collect (spiel--do (list (car spiel-last-parsed) o))
          into responses
          finally return (string-join responses "\n"))))
      ((and (or `(,obj) obj) (guard (spiel-object-p obj)))
       (or (spiel--do "take" obj)
           (cond
            ((spiel-actor-p obj) "Nonsesne.")
            ((spiel-object-has-p spiel-player obj)
             (format "%s already has the %s" name (spiel-object-noun-phrase obj)))
            ((spiel-movable-p obj) (spiel-object-put 'in spiel-player obj)))))
      ((and `(,objs) (guard (spiel-objects-p objs)))
       ;;@FIX: shouldn't hardcode filter here.
       (spiel--disambiguate objs #'spiel-object-in-room-p (lambda (o) (spiel--take o)))))))

(defun spiel--open (pattern)
  "Open PATTERN."
  (pcase pattern
    ((and `(,obj) (guard (spiel-object-p obj)))
     (or (spiel--do "open" obj)
         (cond
          ((not (assoc 'closed (spiel-object<-context obj)))
           (format "Can't open %s." (spiel-entity-name obj)))
          ((not (spiel-context-get obj 'closed)) "It's already open.")
          (t (setf (spiel-context-get obj 'closed) nil)
             (format "Opened %s." (spiel-named<-as obj))))))))

(defvar-local spiel-verbs
    (list
     (spiel-verb :names '("look" "glance" "gaze" "stare" "see" "peer" "peek" "watch" "examine" "describe" "study" "inspect" "scan" "scrutinize")
                 :actions #'spiel--look)
     (spiel-verb :names '("put" "place" "set" "position")
                 :actions #'spiel--put
                 :disambiguator (lambda (o) (spiel-object-has-p spiel-player o)))
     (spiel-verb :names '("take" "get" "grab" "acquire")
                 :actions #'spiel--take)
     (spiel-verb :names '("say" "tell" "shout" "whisper" "tell"))
     (spiel-verb :names '("go" "move" "walk" "run" "crawl")
                 :actions #'spiel--go)
     (spiel-verb :names '("open") :actions #'spiel--open)
     (spiel-verb :names '("attack" "hit"))
     (spiel-verb :names '("inventory"))
     (spiel-verb :names '("pull"))
     (spiel-verb :names '("use"))
     (spiel-verb :names '("clear") :actions (lambda (_) (spiel-clear)))
     (spiel-verb :names '("quit") :actions (lambda (_) (spiel-quit)))
     (spiel-verb :names '("reset") :actions (lambda (_) (spiel-reset)))
     (spiel-verb :names '("give"))))

(defun spiel-create-entity (type &rest args)
  "Return entity of TYPE with ARGS."
  (unless (memq type '(question verb actor object room item))
    (signal 'wrong-type-argument `((or question verb actor object room item) ,type)))
  (cl-loop for key in '(:names :adjectives) do
           (when-let ((declared (plist-get args key))
                      ((not (listp declared))))
             (setq args (plist-put args key (list declared)))))
  (let* ((id (or (plist-get args :id)
                 (when-let ((name (car (plist-get args :names))))
                   (intern (replace-regexp-in-string " +" "-" (downcase name))))
                 (error "No ID for %s %S" type args)))
         (found (cl-find id spiel-entities :key #'spiel-entity<-id)))
    (if (member this-command '(eval-last-sexp eval-buffer eval-defun))
        (setq spiel-entities (cl-remove id spiel-entities :key #'spiel-entity<-id))
      (when found (error "Duplicate entity id %S %S" id found)))
    (setq args (plist-put args :id id))
    (car (push (apply (intern (format "spiel-%s" (symbol-name type))) args) spiel-entities))))

(defun spiel-ensure-entity (id-or-entity)
  "Return ENTITY from ID-OR-ENTITY."
  (if (spiel-entity-p id-or-entity) id-or-entity
    (unless (and id-or-entity (symbolp id-or-entity))
      (signal 'wrong-type-argument `(symbolp ,id-or-entity)))
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

(defun spiel-pending-prompt-p ()
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
    (while (setq last container
                 container (ignore-errors (spiel-ensure-entity (cdr (spiel-object<-location obj)))))
      (setq obj container))
    last))

(defun spiel-object-inventory (location object &optional _recursive)
  "Return list of objects in OBJECT's LOCATION.
If RECURSIVE is non-nil, return inventory of all returned objects."
  (unless (memq location '(in on)) (signal 'wrong-type-argument `((or in on) ,location)))
  (cl-loop with id = (if (symbolp object) object (spiel-object<-id object))
           with target = (cons location id)
           for obj in (cl-remove-if-not #'spiel-object-p spiel-entities)
           when (equal (spiel-object<-location obj) target)
           collect obj))

(defun spiel-object-has-p (subject predicate)
  "Return t if SUBJECT object has PREDICATE object."
  (eq (cdr (spiel-object<-location (spiel-ensure-entity predicate)))
      (spiel-object<-id (spiel-ensure-entity subject))))

(defun spiel-ensure-number (object)
  "Convert OBJECT to number."
  (cond ((eq object t) most-positive-fixnum)
        ((not (numberp object)) 0)
        (t object)))

(defun spiel-object-capacity (location object)
  "Return OBJECT's numeric capcity for LOCATION."
  (spiel-ensure-number (alist-get location (spiel-object<-capacity object))))

(defun spiel-object-put (location object &rest objects)
  "Move OBJECTS to OBJECT's inventory.
LOCATION is either the symbol `in` or `on`."
  (unless (memq location '(in on))
    (signal 'wrong-type-argument `((or in on) ,location)))
  (cl-loop
   with obj
   with moved
   with destination = (spiel-ensure-entity object)
   with capacity = (cl-reduce #'- (spiel-object-inventory location destination)
                              :initial-value (spiel-object-capacity location destination)
                              :key (lambda (o) (or (spiel-object<-size o) 0)))
   while (and (setq obj (pop objects))
              (setq obj (spiel-ensure-entity obj))
              (>= (cl-decf capacity (spiel-ensure-number (spiel-object<-size obj))) 0))
   do
   (setf (spiel-object<-location obj) (cons location (spiel-object<-id destination)))
   (push obj moved)
   finally return
   (concat (when moved
             (if (eq destination spiel-player)
                 (format "Took %s."
                         (let ((spiel-singular-enumeration t))
                           (apply #'spiel-enumeration (nreverse moved))))
               (format "Put %s %s %s."
                       (let ((spiel-singular-enumeration t))
                         (apply #'spiel-enumeration (nreverse moved)))
                       location
                       (spiel-object-noun-phrase object))))
           (when (and moved (or objects obj)) "\n")
           (when-let ((remaining (append objects obj)))
             (if (eq destination spiel-player)
                 "Inventory full."
               (format "Could not fit %s."
                       (replace-regexp-in-string
                        "^an? " "" (apply #'spiel-enumeration
                                          (if (spiel-object-p remaining)
                                              (list remaining)
                                            remaining)))))))))

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

(defun spiel-objects-matching (string slot)
  "Return list of objects where STRING is member of SLOT."
  (cl-loop for o in (cl-remove-if-not #'spiel-object-p spiel-entities)
           when (cl-find string (funcall slot o) :test #'spiel--string-equal)
           collect o))

(defun spiel--tokenize (string)
  "Return ojbects pattern from STRING."
  (cl-loop
   with (verb described result)
   with tokens = (mapcar #'string-trim (string-split string " " 'omit-nulls))
   with escapep = (when (string-prefix-p "/" (car tokens))
                    (setf (car tokens) (substring (car tokens) 1)))
   initially (when (and spiel-pending-question (not escapep))
               (cl-return (list spiel-pending-question tokens)))
   for token in tokens do
   (cond
    ((member token spiel--articles) nil)
    ((null verb) (when-let ((v (spiel--verb (downcase token))))
                   (setf (spiel-named<-as v) token
                         verb (push v result))))
    (t (if-let ((named (spiel-objects-matching token #'spiel-object<-names))
                (possible
                 (mapc (lambda (o) (setf (spiel-named<-as o) token))
                       (if described (cl-intersection described named) named))))
           (setq result (cons (if (cdr possible) possible (car possible)) result)
                 described nil)
         (if-let ((possible (spiel-objects-matching token #'spiel-object<-adjectives)))
             (setq described possible)
           (push token result)))))
   finally return (nreverse (append (when described (list described)) result))))

(defun spiel-room-description (&optional room)
  "Return ROOM description. ROOM defaults to player's current room."
  (let ((room (spiel-ensure-entity (or room (spiel-object-room)))))
    (unless (spiel-room-p room) (signal 'wrong-type-argument `(room ,room)))
    (string-join
     (delq nil
           (mapcar #'spiel-object<-description
                   (cons room (cl-sort (cl-remove spiel-player (spiel-object-inventory 'in room))
                                       #'< :key #'spiel-object<-order))))
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
             (if (or (spiel-object-p pattern) (stringp pattern))
                 (list pattern) pattern)
             " "))

(defun spiel--disambiguation-prompt (objs)
  "Set up disambiguation prompt for OBJS."
  (setf spiel-entities (cl-remove 'disambiguation spiel-entities :key #'spiel-entity<-id))
  (spiel-create-entity 'question
                       :id 'disambiguation
                       :asker 'prompt
                       :text (format "Which one?")
                       :options
                       (nreverse
                       (cl-loop for obj in objs collect
                                (cons (spiel-object-noun-phrase obj) obj))))
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
  (let* ((actorp (spiel-actor-p obj))
         (name (spiel-entity-name obj))
         (inventory (spiel-object-inventory 'in obj))
         (load (spiel-object-inventory 'on obj)))
    (if (or inventory load)
        (concat
         (when load
           (let ((list (apply #'spiel-enumeration load)))
             (if actorp (format "%s wearing %s." (spiel-linking-verb name) list)
               (format "%s are on top of it." (spiel-capitalize list)))))
         (when (and inventory load) "\n")
         (when inventory
           (concat (if actorp name "It")
                   (if actorp (format " %s holding " (spiel-linking-verb name)) " contains ")
                   (apply #'spiel-enumeration inventory) ".")))
      (concat (when (not actorp) "It's ") "Empty."))))

(defun spiel-object-in-room-p (object &optional room)
  "Return t if OBJECT is in ROOM, otherwise nil."
  (equal (spiel-object-room (spiel-ensure-entity object))
         (spiel-ensure-entity (or room (spiel-object-room)))))

(defun spiel--default-actions (pattern)
  "Call default function for PATTERN."
  (pcase pattern
    (`(,(and (pred spiel-question-p)) . ,_) (spiel--answer pattern))
    ((and `(,verb . ,rest) (guard (spiel-verb-p verb)))
     (or
      (spiel--do rest verb)
      (spiel--do (spiel--pattern-to-query pattern))))
    (_ (format "Can't %s." spiel-last-input))))

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

(defun spiel--substitute-aliases (string)
  "Return copy of STRING with `spiel-command-aliases' replaced."
  (cl-loop for (regexp replacement . args) in spiel-command-aliases do
           (setq string (apply #'replace-regexp-in-string
                               `(,regexp ,replacement ,string ,@args))))
  string)

(defun spiel-replace-input (string)
  "Replace input region with STRING."
  (apply #'delete-region (spiel--input-region))
  (insert string))

(defun spiel-print-input (input)
  "Print INPUT."
  (spiel-replace-input
   (propertize (if (string-empty-p input) "..."
                 (replace-regexp-in-string " +" " " input))
               'face (if spiel-pending-question 'spiel-question 'spiel-command)
               'read-only t)))

(defun spiel-input ()
  "Return trimmed input string with aliases substituted."
  (let* ((region (spiel--input-region))
         (raw (apply #'buffer-substring-no-properties region))
         (trimmed (string-trim (replace-regexp-in-string " +"  " " raw)))
         (escapedp (string-prefix-p "/" trimmed)))
    (concat
     (when escapedp "/")
     (spiel--substitute-aliases (if escapedp (substring trimmed 1) trimmed)))))

(defun spiel-dialogue-p ()
  "Return t when an actor is asking a quesiton, otherwise nil."
  (and spiel-pending-question (not (spiel-pending-prompt-p))))

(defun spiel-send-input ()
  "Send the input from input-buffer."
  (interactive)
  (let ((input (setq spiel-last-input (spiel-input))))
    (unless (or (spiel-dialogue-p) (> (length input) 0)) (user-error "No input"))
    (spiel-print-input input)
    (unless (string-empty-p input) (push input spiel-input-history))
    (catch 'turn-over
      (when-let ((result (spiel--do (or (setq spiel-last-parsed (spiel--tokenize input))
                                        input)))
                 ((stringp result)))
        (spiel-print "\n" result "\n\n"))
      (spiel-insert-prompt (spiel-dialogue-p)))))

(defun spiel-quit ()
  "Quit game."
  (spiel-print "\nQuitting game...\n")
  (sit-for 0.5)
  (kill-buffer spiel-buffer)
  (setq spiel-entities nil)
  (ignore-errors (throw 'turn-over t)))

(defun spiel-clear ()
  "Clear game window."
  (interactive)
  (with-current-buffer spiel-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (forward-line -1)
      (beginning-of-line)
      (delete-region (point) (point-min)))
    "Screen cleared."))
;; ;;;###autoload
;; (defun spiel-load (file)
;;   "Load a game FILE."
;;   (interactive (list (let* ((choices (or spiel-collection (user-error "No games registered")))
;;                             (choice (completing-read "Load game: " choices nil t)))
;;                        (alist-get choice spiel-collection nil nil #'equal))))
;;   (load file nil 'no-message))

(provide 'spiel)
;;; spiel.el ends here

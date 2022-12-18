;;; spiel.el --- Story Programming Interface for Emacs Lisp: text adventure framework                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords:

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
(require 'eieio)

(defgroup spiel nil
  "Story Programming Interface for Emacs Lisp: A text adventure framework."
  :group 'spiel)

(defcustom spiel-want-typing t
  "Whether or not to display messages with an animation."
  :type 'boolean)

(defcustom spiel--verbs '(("look" "inspect")
                        ("go" "walk" "move" "run")
                        ("take" "get" "grab")
                        ("use" "give"))
  "List of verb lists. Each element in the sublists are synonymous actions."
  :type 'alist)

(defcustom spiel--global-commands '(("inventory" . spiel--print-inventory)
                                  ("look" . spiel--print-room-description)
                                  ("status" . spiel--print-status)
                                  ("reset" . spiel--reset)
                                  ("quit" . spiel-quit)
                                  ("help" . spiel-help))
  "Alist of global player commands."
  :type 'alist)

(defvar spiel--input-buffer "*spiel input*" "Name of input buffer.")
(defvar spiel--output-buffer "*spiel output*" "Name of output buffer.")
(defvar spiel--old-window-config nil "Window config priror to launching game.")
(defvar spiel-player nil "The main character struct.")
(defvar spiel--rooms nil "Alist of rooms in memory.")
(defvar spiel--room  nil "Name of current room.")

(defclass spiel-player ()
  ((status :initarg :status
           :initform nil
           :type list
           :custom list
           :documentation "The player's status.")
   (inventory :initarg :inventory
              :initform nil
              :type list
              :custom list
              :documentation "The player's inventory.")
   (health :initarg :health
           :initform 3
           :type integer
           :custom integer
           :documentation "The player's health."))
  "A basic player class.")

(cl-defmethod spiel--print-status ((player spiel-player))
  "Print PLAYER's current status."
  (spiel--print (format "%s" (car (oref player status))) "\n"))

(defun spiel-help ()
  "Print game help."
  (spiel--print
   "Try typing one of the following verbs at the prompt..."
   "\n"
   (string-join (mapcar (lambda (v) (propertize (car v) 'face '(:weight bold))) spiel--verbs) "\n")
   "\n"
   "...followed by a NOUN in the room.\n"
   "You can also type any of the following global commands by themselves:\n"
   (string-join (mapcar (lambda (v) (propertize (car v) 'face '(:weight bold))) spiel--global-commands) "\n")))

(defun spiel-quit ()
  "Quit game."
  (interactive)
  (spiel--print "Quitting game...")
  (sit-for 0.5)
  (with-current-buffer (get-buffer-create spiel--input-buffer)
    (kill-buffer))
  (with-current-buffer (get-buffer-create spiel--output-buffer)
    (kill-buffer))
  (when spiel--old-window-config
    (set-window-configuration spiel--old-window-config)))

(cl-defmethod spiel--print-inventory ((player spiel-player))
  "Print PLAYER's inventory."
  (spiel--print
   (if-let ((inventory (oref player inventory)))
       (format "Your inventory contains: %s" (string-join (mapcar #'cdr inventory) ", "))
     "Your inventory is empty.")
   "\n"))

(defun spiel--print-room-description ()
  "Print description of current room."
  (let ((description (plist-get spiel--room :description)))
    (spiel--print (if (functionp description) (funcall description) description) "\n")))

(cl-defmethod spiel--get ((player spiel-player) item description)
  "Add ITEM with DESCRIPTION to PLAYER's inventory."
  (if-let ((found (alist-get item (oref player inventory))))
      (spiel--print (format "You already have the %s." item))
    (setf (alist-get item (oref player inventory)) description))
  (spiel--print (format "%s added to inventory." description)))

(cl-defmethod spiel--use ((player spiel-player) item)
  "Use ITEM in PLAYER's inventory."
  (setf (alist-get item (oref player inventory) nil 'remove) nil))

(cl-defmethod spiel--has-p ((player spiel-player) item)
  "Return t if PLAYER has ITEM."
  (and (alist-get item (oref player inventory)) t))

(defun spiel--load-room (path)
  "Load room at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (setf (alist-get (plist-get data :id) spiel--rooms) data)
      data)))

(defun spiel--load-rooms (dir)
  "Load DIRs rooms into memory."
  (mapc #'spiel--load-room (directory-files dir 'full "eld$")))

(defun spiel--go (id)
  "Go to room with ID."
  (if-let ((destination (alist-get id spiel--rooms)))
      (progn
        (setf spiel--room destination)
        (spiel--set-title)
        (spiel--print-room-description))
    (spiel--print (format "ERROR loading room: %S" id))))

(defun spiel--input-buffer ()
  "Display the input buffer."
  (pop-to-buffer (get-buffer-create spiel--input-buffer)
                 '(display-buffer-at-bottom
                   display-buffer-in-atom-window
                   (window-height . 5)
                   (dedicated . t)))
  (with-silent-modifications
    (erase-buffer)
    (spiel-input-mode)))

(defmacro spiel--room-var (symbol)
  "Return room :vars SYMBOL's associated value."
  `(alist-get ,symbol (plist-get spiel--room :vars)))

(defmacro spiel--set-room-item (item &rest body)
  "Add ITEM with BODY to the current room."
  (declare (indent 1))
  `(setf (alist-get ,item (plist-get spiel--room :items) nil nil #'equal)
         '(,@body)))

(defun spiel--set-title ()
  "Set ouptut buffer title."
  (with-current-buffer (get-buffer-create spiel--output-buffer)
    (setq-local header-line-format
                (propertize (plist-get spiel--room :title)
                            'face '(:height 2.0)))))

(defun spiel--output-buffer ()
  "Display the output buffer."
  (pop-to-buffer (get-buffer-create spiel--output-buffer)
                 '(display-buffer-same-window
                   display-buffer-in-atom-window
                   (display-buffer-reuse-window)
                   (dedicated . t)))
  (with-silent-modifications
    (erase-buffer)
    (setq-local mode-line-format nil)
    (spiel--set-title)
    (spiel--print-room-description))
  (read-only-mode)
  (visual-line-mode))

(defun spiel--type (&rest strings)
  "Type STRINGS in output buffer."
  (with-current-buffer (get-buffer-create spiel--output-buffer)
    (with-silent-modifications
      (cl-loop for string in strings
               for tokens = (split-string string "" 'omit-nulls)
               do (cl-loop for token in tokens
                           do
                           (goto-char (point-max))
                           (insert token)
                           (set-window-point (get-buffer-window (current-buffer)) (point-max))
                           (sit-for 0.0015))))))

(defun spiel--print (&rest args)
  "Print ARGS in the output buffer."
  (with-current-buffer (get-buffer-create spiel--output-buffer)
    (with-silent-modifications
      (goto-char (point-max))
      (apply (if spiel-want-typing #'spiel--type #'insert) args))
    (set-window-point (get-buffer-window (current-buffer)) (point-max)))
  nil)

(defun spiel--print-command (command)
  "Print COMMAND."
  (spiel--print
   (propertize (format "\n> %s\n" command) 'face '(:height 1.0 :foreground "grey"))))

(defun spiel--parse-command (command &optional noprint)
  "Parse COMMAND.
If NOPRINT is non-nil, do not print command."
  (if-let ((tokens (split-string command " "))
           (verbs (cl-some (lambda (token)
                             (cl-some (lambda (verbs) (and (member token verbs) verbs))
                                      (append (mapcar (lambda (it) (list (car it)))
                                                      spiel--global-commands)
                                              spiel--verbs)))
                           tokens))
           (verb (car verbs)))
      (progn
        (unless noprint (spiel--print-command command))
        (with-current-buffer (get-buffer-create spiel--output-buffer)
          (setq tokens (cl-remove-if (lambda (token) (member token verbs)) tokens))
          (if-let ((global (and (not tokens)
                                ;;@TODO: assoc-string or assoc-default?
                                (alist-get verb spiel--global-commands nil nil #'equal))))
              (funcall global)
            (catch 'acted
              (cl-loop with items = (plist-get spiel--room :items)
                       for token in tokens
                       for item = (alist-get token items nil nil #'equal)
                       for action = (alist-get verb item nil nil #'equal)
                       for result = (and action
                                         (if (functionp (car action))
                                             (funcall (car action) tokens)
                                           (eval `(progn ,@action) t)))
                       when action do
                       (spiel--print (if (stringp result) result "") "\n")
                       (throw 'acted t)
                       finally do (spiel--print "Nothing to do.\n"))))))
    (spiel--print-command (format "Unrecognized command %S" command))))

(defun spiel-send-input ()
  "Send the input of the input-buffer."
  (interactive)
  (unless (equal (buffer-name) spiel--input-buffer)
    (user-error "Not in input buffer"))
  (let ((input (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (1+ (point-min)) (point-max))
    (spiel--parse-command (string-trim input))))

(defvar spiel-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'spiel-send-input)
    map))

(define-derived-mode spiel-input-mode fundamental-mode
  "spiel-input" "Major mode for inputting commands."
  (insert (propertize " " 'face '(:weight bold :height 2.0 :foreground "green")
                      'display ">"
                      'intangible t
                      'read-only t
                      'rear-nonsticky t)))

(provide 'spiel)
;;; spiel.el ends here

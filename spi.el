;;; spi.el --- Story Programming Interface for Emacs Lisp: text adventure framework                     -*- lexical-binding: t; -*-

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
  :group 'spiel
  :prefix "spi-")

(defcustom spi-want-typing t
  "Whether or not to display messages with an animation."
  :type 'boolean)

(defcustom spi--verbs '(("look" "inspect")
                        ("go" "walk" "move" "run")
                        ("take" "get" "grab")
                        ("use" "give"))
  "List of verb lists. Each element in the sublists are synonymous actions."
  :type 'alist)

(defcustom spi--global-commands '(("inventory" . spi--print-inventory)
                                  ("look" . spi--print-room-description)
                                  ("status" . spi--print-status)
                                  ("reset" . spi--reset)
                                  ("quit" . spi-quit)
                                  ("help" . spi-help))
  "Alist of global player commands."
  :type 'alist)

(defvar spi--input-buffer "*spiel input*" "Name of input buffer.")
(defvar spi--output-buffer "*spiel output*" "Name of output buffer.")
(defvar spi--old-window-config nil "Window config priror to launching game.")
(defvar spi-player nil "The main character struct.")
(defvar spi--rooms nil "Alist of rooms in memory.")
(defvar spi--room  nil "Name of current room.")

(defclass spi-player ()
  ((status :initarg :status
           :initform nil
           :type list
           :custom list
           :documentation "The player's status."))
  ((inventory :initarg :inventory
              :initform nil
              :type list
              :custom list
              :documentation "The player's inventory."))
  ((health :initarg :health
           :initform 3
           :type integer
           :custom integer
           :documentation "The player's health."))
  "A basic player class.")

(cl-defmethod spi--print-status ((player spi-player))
  "Print PLAYER's current status."
  (spi--print (format "%s" (car (oref player status))) "\n"))

(defun spi-help ()
  "Print game help."
  (spi--print
   "Try typing one of the following verbs at the prompt..."
   "\n"
   (string-join (mapcar (lambda (v) (propertize (car v) 'face '(:weight bold))) spi--verbs) "\n")
   "\n"
   "...followed by a NOUN in the room.\n"
   "You can also type any of the following global commands by themselves:\n"
   (string-join (mapcar (lambda (v) (propertize (car v) 'face '(:weight bold))) spi--global-commands) "\n")))

(defun spi-quit ()
  "Quit game."
  (interactive)
  (spi--print "Quitting game...")
  (sit-for 0.5)
  (with-current-buffer (get-buffer-create spi--input-buffer)
    (kill-buffer))
  (with-current-buffer (get-buffer-create spi--output-buffer)
    (kill-buffer))
  (when spi--old-window-config
    (set-window-configuration spi--old-window-config)))

(cl-defmethod spi--print-inventory ((player spi-player))
  "Print PLAYER's inventory."
  (spi--print
   (if-let ((inventory (oref player 'inventory)))
       (format "Your inventory contains: %s" (string-join (mapcar #'cdr inventory) ", "))
     "Your inventory is empty.")
   "\n"))

(defun spi--print-room-description ()
  "Print description of current room."
  (let ((description (plist-get spi--room :description)))
    (spi--print (if (functionp description) (funcall description) description) "\n")))

(cl-defmethod spi--get ((player spi-player) item description)
  "Add ITEM with DESCRIPTION to PLAYER's inventory."
  (if-let ((found (alist-get item (oref player 'inventory))))
      (spi--print (format "You already have the %s." item))
    (setf (alist-get item (oref player 'inventory)) description))
  (spi--print (format "%s added to inventory." description)))

(cl-defmethod spi--use ((player spi-player) item)
  "Use ITEM in PLAYER's inventory."
  (setf (alist-get item (oref player 'inventory) nil 'remove) nil))

(cl-defmethod spi--has-p ((player spi-player) item)
  "Return t if PLAYER has ITEM."
  (and (alist-get item (oref player 'inventory)) t))

(defun spi--load-room (path)
  "Load room at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (setf (alist-get (plist-get data :id) spi--rooms) data)
      data)))

(defun spi--load-rooms (dir)
  "Load DIRs rooms into memory."
  (mapc #'spi--load-room (directory-files dir 'full "eld$")))

(defun spi--go (id)
  "Go to room with ID."
  (if-let ((destination (alist-get id spi--rooms)))
      (progn
        (setf spi--room destination)
        (spi--set-title)
        (spi--print-room-description))
    (spi--print (format "ERROR loading room: %S" id))))

(defun spi--input-buffer ()
  "Display the input buffer."
  (pop-to-buffer (get-buffer-create spi--input-buffer)
                 '(display-buffer-at-bottom
                   display-buffer-in-atom-window
                   (window-height . 5)
                   (dedicated . t)))
  (with-silent-modifications
    (erase-buffer)
    (spi-input-mode)))

(defmacro spi--room-var (symbol)
  "Return room :vars SYMBOL's associated value."
  `(alist-get ,symbol (plist-get spi--room :vars)))

(defmacro spi--set-room-item (item &rest body)
  "Add ITEM with BODY to the current room."
  (declare (indent 1))
  `(setf (alist-get ,item (plist-get spi--room :items) nil nil #'equal)
         '(,@body)))

(defun spi--set-title ()
  "Set ouptut buffer title."
  (with-current-buffer (get-buffer-create spi--output-buffer)
    (setq-local header-line-format
                (propertize (plist-get spi--room :title)
                            'face '(:height 2.0)))))

(defun spi--output-buffer ()
  "Display the output buffer."
  (pop-to-buffer (get-buffer-create spi--output-buffer)
                 '(display-buffer-same-window
                   display-buffer-in-atom-window
                   (display-buffer-reuse-window)
                   (dedicated . t)))
  (with-silent-modifications
    (erase-buffer)
    (setq-local mode-line-format nil)
    (spi--set-title)
    (spi--print-room-description))
  (read-only-mode)
  (visual-line-mode))

(defun spi--type (&rest strings)
  "Type STRINGS in output buffer."
  (with-current-buffer (get-buffer-create spi--output-buffer)
    (with-silent-modifications
      (cl-loop for string in strings
               for tokens = (split-string string "" 'omit-nulls)
               do (cl-loop for token in tokens
                           do
                           (goto-char (point-max))
                           (insert token)
                           (set-window-point (get-buffer-window (current-buffer)) (point-max))
                           (sit-for 0.0015))))))

(defun spi--print (&rest args)
  "Print ARGS in the output buffer."
  (with-current-buffer (get-buffer-create spi--output-buffer)
    (with-silent-modifications
      (goto-char (point-max))
      (apply (if spi-want-typing #'spi--type #'insert) args))
    (set-window-point (get-buffer-window (current-buffer)) (point-max)))
  nil)

(defun spi--print-command (command)
  "Print COMMAND."
  (spi--print
   (propertize (format "\n> %s\n" command) 'face '(:height 1.0 :foreground "grey"))))

(defun spi--parse-command (command &optional noprint)
  "Parse COMMAND.
If NOPRINT is non-nil, do not print command."
  (if-let ((tokens (split-string command " "))
           (verbs (cl-some (lambda (token)
                             (cl-some (lambda (verbs) (and (member token verbs) verbs))
                                      (append (mapcar (lambda (it) (list (car it)))
                                                      spi--global-commands)
                                              spi--verbs)))
                           tokens))
           (verb (car verbs)))
      (progn
        (unless noprint (spi--print-command command))
        (with-current-buffer (get-buffer-create spi--output-buffer)
          (setq tokens (cl-remove-if (lambda (token) (member token verbs)) tokens))
          (if-let ((global (and (not tokens)
                                ;;@TODO: assoc-string or assoc-default?
                                (alist-get verb spi--global-commands nil nil #'equal))))
              (funcall global)
            (catch 'acted
              (cl-loop with items = (plist-get spi--room :items)
                       for token in tokens
                       for item = (alist-get token items nil nil #'equal)
                       for action = (alist-get verb item nil nil #'equal)
                       for result = (and action
                                         (if (functionp (car action))
                                             (funcall (car action) tokens)
                                           (eval `(progn ,@action) t)))
                       when action do
                       (spi--print (if (stringp result) result "") "\n")
                       (throw 'acted t)
                       finally do (spi--print "Nothing to do.\n"))))))
    (spi--print-command (format "Unrecognized command %S" command))))

(defun spi-send-input ()
  "Send the input of the input-buffer."
  (interactive)
  (unless (equal (buffer-name) spi--input-buffer)
    (user-error "Not in input buffer"))
  (let ((input (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (1+ (point-min)) (point-max))
    (spi--parse-command (string-trim input))))

(defvar spi-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'spi-send-input)
    map))

(define-derived-mode spi-input-mode fundamental-mode
  "spi-input" "Major mode for inputting commands."
  (insert (propertize " " 'face '(:weight bold :height 2.0 :foreground "green")
                      'display ">"
                      'intangible t
                      'read-only t
                      'rear-nonsticky t)))

(provide 'spi)
;;; spi.el ends here

;;@TODO: remove. testing only. should be user config.
(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'spi-input-mode 'insert))

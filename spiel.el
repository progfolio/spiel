;;; spiel.el --- Story Programming Interface for Emacs Lisp                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/spiel
;; Created: December 17, 2022
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0
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
;; This is a framework for creating text adventure games.

;;; Code:
(require 'eieio)

(defgroup spiel nil
  "Story Programming Interface for Emacs Lisp: A text adventure framework."
  :group 'spiel)

(defcustom spiel-want-typing t
  "Whether or not to display messages with an animation."
  :type 'boolean)

(defcustom spiel-verbs '(("look" "inspect")
                         ("go" "walk" "move" "run")
                         ("take" "get" "grab")
                         ("use" "give"))
  "List of verb lists. Each element in the sublists are synonymous actions."
  :type 'alist)

(defcustom spiel-global-commands '(("inventory" . spiel--print-inventory)
                                   ("look" . spiel--print-room-description)
                                   ("status" . spiel--print-status)
                                   ("reset" . spiel--reset)
                                   ("quit" . spiel-quit)
                                   ("help" . spiel-help))
  "Alist of global player commands."
  :type 'alist)

(defcustom spiel-inventory-message-format "Your inventory contains: %s"
  "Format string with one %s specifier used for printing inventory."
  :type 'string)

(defcustom spiel-inventory-empty-message "Your inventory is empty."
  "Message displayed when inventory is empty."
  :type 'string)

(defface spiel-command
  '((t (:weight bold :foreground "grey")))
  "Face for printed user commands.")

(defface spiel-title
  '((t (:weight bold :foreground "blue" :height 2.0)))
  "Face for room titles.")

(defvar spiel--game nil "The current game struct.")
(defvar spiel--player nil "The current game player struct.")

(cl-defstruct (spiel-player< (:constructor spiel-player<--create)
                             (:type list)
                             (:copier nil)
                             (:named))
  "Player struct."
  name
  inventory
  status
  (health 3))

(cl-defun spiel-player<-create (&key name inventory status health)
  "Create a new player struct.
NAME, INVENTORY, STATUS, HEALTH, keys initialize player."
  (setq spiel--player (spiel-player<--create :name name
                                             :inventory inventory
                                             :status status
                                             :health health)))

(cl-defstruct (spiel-game< (:constructor spiel-game<--create)
                           (:type list)
                           (:copier nil)
                           (:named))
  "Game struct."
  title rooms room
  (player (spiel-player<-create))
  (output-buffer "*spiel-output*")
  (input-buffer "*spiel-input*")
  (reset)
  (window-config (current-window-configuration)))

(cl-defun spiel-game<-create (title room-dir
                                    &key
                                    player room rooms output-buffer input-buffer
                                    reset window-config)
  "Create a new game struct.
TITLE and ROOM-DIR are required.
PLAYER, ROOM, ROOMS, OUTPUT-BUFFER, INPUT-BUFFER, RESET, WINDOW-CONFIG,
initialize struct."
  (prog1
      (setq spiel--game
            (spiel-game<--create
             :title title
             :player player
             :room room
             :rooms rooms
             :output-buffer output-buffer
             :input-buffer input-buffer
             :reset reset
             :window-config window-config))
    (spiel--load-rooms room-dir)))

(defun spiel--reset ()
  "Reset GAME."
  (funcall (spiel-game<-reset spiel--game)))

(defun spiel--print (&rest args)
  "Print ARGS in the GAME's output buffer."
  (with-current-buffer (spiel-game<-output-buffer spiel--game)
    (with-silent-modifications
      (goto-char (point-max))
      (apply (if spiel-want-typing #'spiel--type #'insert) args)
      (set-window-point (get-buffer-window (current-buffer)) (point-max)))
    nil))

(defun spiel--print-status ()
  "Print player's current status."
  (spiel--print (format "%s" (car (spiel-player<-status spiel--player))) "\n"))

(defun spiel--bold-list (commands)
  "Return emobldened list of COMMANDS."
  (string-join (mapcar (lambda (s) (propertize (car s) 'face '(:weight bold)))
                       commands)
               "\n"))

(defun spiel-help ()
  "Print GAME help."
  (spiel--print
   "Try typing one of the following verbs at the prompt..."
   "\n"
   (spiel--bold-list spiel-verbs)
   "\n"
   "...followed by a NOUN in the room.\n"
   "You can also type any of the following global commands by themselves:\n"
   (spiel--bold-list spiel-global-commands)))

(defun spiel-quit ()
  "Quit game."
  (spiel--print "Quitting game...")
  (sit-for 0.5)
  (with-current-buffer (spiel-game<-input-buffer spiel--game)
    (kill-buffer))
  (with-current-buffer (spiel-game<-output-buffer spiel--game)
    (kill-buffer))
  (when-let ((config (spiel-game<-window-config spiel--game)))
    (set-window-configuration config)))

(defun spiel--print-inventory ()
  "Print player's inventory."
  (spiel--print (if-let ((inventory (spiel-player<-inventory spiel--player)))
                    (format spiel-inventory-message-format
                            (string-join (mapcar #'cdr inventory) ", "))
                  spiel-inventory-empty-message)
                "\n"))

(defun spiel--print-room-description ()
  "Print current room description."
  (let ((description (plist-get (spiel-game<-room spiel--game) :description)))
    (spiel--print (if (functionp description) (funcall description) description) "\n")))

(defun spiel--get (item description)
  "Add ITEM with DESCRIPTION to player's inventory."
  (if-let ((found (alist-get item (spiel-player<-inventory spiel--player))))
      (spiel--print (format "You already have the %s." item))
    (setf (alist-get item (spiel-player<-inventory spiel--player)) description))
  (spiel--print (format "%s added to inventory." description)))

(defun spiel--use (item)
  "Use ITEM in player's inventory."
  (setf (alist-get item (spiel-player<-inventory spiel--player) nil 'remove) nil))

(defun spiel--has-p (item)
  "Return t if ITEM is in player's inventory."
  (and (alist-get item (spiel-player<-inventory spiel--player)) t))

(defun spiel--load-room (path)
  "Load room at PATH into game."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (setf (alist-get (plist-get data :id) (spiel-game<-rooms spiel--game)) data))))

(defun spiel--load-rooms (dir)
  "Load DIR's rooms into game's memory."
  (cl-loop for file in (directory-files dir 'full "eld$") do (spiel--load-room file)))

(defun spiel--go (id)
  "Go to game room with ID."
  (if-let ((destination (alist-get id (spiel-game<-rooms spiel--game))))
      (progn
        (setf (spiel-game<-room spiel--game) destination)
        (spiel--set-title)
        (spiel--print-room-description)
        (spiel--print (format "ERROR loading room: %S" id)))))

(defun spiel--input-buffer ()
  "Display game's input buffer."
  (pop-to-buffer (get-buffer-create (spiel-game<-input-buffer spiel--game))
                 '((display-buffer-at-bottom)
                   (window-height . 5)))
  (with-silent-modifications
    (erase-buffer)
    (spiel-input-mode)))

(defmacro spiel--room-var (symbol)
  "Return room :vars SYMBOL's associated value."
  `(alist-get ,symbol (plist-get (spiel-game<-room spiel--game) :vars)))

(defmacro spiel--set-room-item (item &rest body)
  "Add ITEM with BODY to the current room."
  (declare (indent 1))
  `(setf (alist-get ,item (plist-get (spiel-game<-room spiel--game) :items) nil nil #'equal)
         '(,@body)))

(defun spiel--set-title ()
  "Set ouptut buffer title."
  (with-current-buffer (spiel-game<-output-buffer spiel--game)
    (setq-local header-line-format
                (propertize (plist-get (spiel-game<-room spiel--game) :title)
                            'face 'spiel-title))))

(defun spiel--output-buffer ()
  "Display game's output buffer."
  (pop-to-buffer (get-buffer-create (spiel-game<-output-buffer spiel--game))
                 '((display-buffer-same-window)))
  (with-silent-modifications
    (erase-buffer)
    (setq-local mode-line-format nil)
    (spiel--set-title)
    (spiel--print-room-description))
  (read-only-mode)
  (visual-line-mode))

(defun spiel--type (&rest strings)
  "Type STRINGS in game's output buffer."
  (with-current-buffer (spiel-game<-output-buffer spiel--game)
    (with-silent-modifications
      (cl-loop for string in strings
               for tokens = (split-string string "" 'omit-nulls)
               do (cl-loop
                   for token in tokens
                   do
                   (goto-char (point-max))
                   (insert token)
                   (set-window-point (get-buffer-window (current-buffer)) (point-max))
                   (sit-for 0.0015))))))

(defun spiel--print-command (command)
  "Print COMMAND in output buffer."
  (spiel--print (propertize (format "\n> %s\n" command) 'face 'spiel-command)))

(defun spiel--parse-command (command &optional noprint)
  "Parse COMMAND.
If NOPRINT is non-nil, do not print command."
  (if-let ((tokens (split-string command " "))
           (verbs (cl-some (lambda (token)
                             (cl-some (lambda (verbs) (and (member token verbs) verbs))
                                      (append (mapcar (lambda (it) (list (car it)))
                                                      spiel-global-commands)
                                              spiel-verbs)))
                           tokens))
           (verb (car verbs)))
      (progn
        (unless noprint (spiel--print-command command))
        (with-current-buffer (spiel-game<-output-buffer spiel--game)
          (setq tokens (cl-remove-if (lambda (token) (member token verbs)) tokens))
          (if-let ((global (and (not tokens)
                                ;;@TODO: assoc-string or assoc-default?
                                (alist-get verb spiel-global-commands nil nil #'equal))))
              (funcall global spiel--game)
            (catch 'acted
              (cl-loop with items = (plist-get (spiel-game<-room spiel--game) :items)
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
  "Send the input from input-buffer."
  (interactive)
  (unless (equal (buffer-name) (spiel-game<-input-buffer spiel--game))
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

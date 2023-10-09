;;; spiel-time.el --- Time related utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

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
(require 'cl-lib)
(defvar-local spiel-time nil)

(defun spiel-timecode-to-seconds (timecode)
  "Convert TIMECODE to seconds."
  (let ((tokens (mapcar #'string-to-number (nreverse (split-string timecode ":" 'omit-nulls)))))
    (cl-reduce #'+ (cl-mapcar #'* tokens '(1 60 3600 86400)))))

(defun spiel-add-time (seconds)
  "Add SECONDS to `spiel-time'."
  (setq spiel-time (time-add spiel-time seconds)))

(defun spiel-subtract-time (seconds)
  "Subtract SECONDS from `spiel-time'."
  (setq spiel-time (time-subtract spiel-time seconds)))

(setq-default spiel-verbs
              (cons (spiel-verb :names '("wait") :actions
                                (lambda (_) (spiel-add-time 60) "Time passes..."))
                    spiel-verbs))

(provide 'spiel-time)
;;; spiel-time.el ends here

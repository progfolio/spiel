;;; spiel-structs.el --- Spiel structures -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Nicholas Vollmer

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
(require 'cl-macs)

(cl-defstruct (spiel-entity (:constructor spiel-entity)
                            (:type list) (:copier nil) (:named)
                            (:conc-name spiel-entity<-))
  id context)
;;@TODO: document flags wearable immobile invisible

(cl-defstruct (spiel-question (:include spiel-entity)
                              (:constructor spiel-question)
                              (:type list) (:copier nil) (:named)
                              (:conc-name spiel-question<-))
  asker
  text
  options
  responses)

(cl-defstruct (spiel-named (:include spiel-entity)
                           (:constructor spiel-named)
                           (:type list) (:copier nil) (:named)
                           (:conc-name spiel-named<-))
  names
  actions
  as)

(cl-defstruct (spiel-verb (:include spiel-named)
                          (:constructor spiel-verb)
                          (:type list) (:copier nil) (:named)
                          (:conc-name spiel-verb<-))
  (disambiguator #'spiel-object-in-room-p))

(cl-defstruct (spiel-object (:include spiel-named)
                            (:constructor spiel-object)
                            (:type list) (:copier nil) (:named)
                            (:conc-name spiel-object<-))
  description
  details
  adjectives
  location ;;(in . entity-id) (on . entity-id)
  capacity ;;((in . (or nil t numberp)))
  (size 1)
  (order 0))

(cl-defstruct (spiel-actor (:include spiel-object)
                           (:constructor spiel-actor)
                           (:type list) (:copier nil) (:named)
                           (:conc-name spiel-actor<-))
  avatar)

(cl-defstruct (spiel-room (:include spiel-object
                                    (capacity '((in . t)))
                                    (context '((immobile . t))))
                          (:constructor spiel-room)
                          (:type list) (:copier nil) (:named)
                          (:conc-name spiel-room<-)))

(cl-defstruct (spiel-item (:include spiel-object)
                          (:constructor spiel-item)
                          (:type list) (:copier nil) (:named)
                          (:conc-name spiel-item<-)))

(provide 'spiel-structs)
;;; spiel-structs.el ends here

;;; org-graph-view.el --- View an Org file as a graph (like a mind map)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: outlines
;; Package-Requires: ((emacs "25.2") (org "9.0") (dash "2.13.0"))

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

;;;; Requirements

(require 'cl-lib)
(require 'org)

(require 'dash)
(require 'graph)

;;;; Variables


;;;; Customization


;;;; Commands

(defun org-graph-view ()
  (interactive)
  (cl-labels ((rec (tree)
                   (-let (((_element properties . children) tree))
                     (cons (or (heading-string properties) "Root")
                           (mapcar #'rec children))))
              (heading-string (properties)
                              (-let* (((&plist :raw-value heading) properties)
                                      (color (color properties))
                                      (face (if color
                                                (list :foreground color)
                                              'default)))
                                (when heading
                                  (propertize heading
                                              'face face
                                              'properties (plist-put properties :buffer (current-buffer))))))
              (color (properties)
                     (-let (((&plist :begin) properties))
                       (org-entry-get begin "color"))))
    (org-with-wide-buffer
     (when (org-at-heading-p)
       (org-narrow-to-subtree))
     (let* ((keymap (aprog1 (make-sparse-keymap)
                      (define-key it [mouse-1] #'org-graph-view-jump)))
            (graph (cdr (rec (org-element-parse-buffer 'headline))))
            (drawn (graph-draw-tree graph)))
       (with-current-buffer (get-buffer-create "*org-graph-view*")
         (erase-buffer)
         (save-excursion
           (insert drawn))
         (use-local-map keymap)
         (pop-to-buffer (current-buffer)))))))

(defun org-graph-view-jump (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window _pos-or-area (_x . _y) _timestamp
                    _object _text-pos . _) position)
          ((&plist :buffer :begin)
           (get-text-property (point) 'properties)))
    (with-current-buffer buffer
      (goto-char begin)
      (org-reveal)
      (pop-to-buffer buffer))))

;;;; Functions


;;;; Footer

(provide 'org-graph-view)

;;; org-graph-view.el ends here

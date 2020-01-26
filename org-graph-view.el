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

;; An early proof-of-concept.

;; Uses <https://github.com/storax/graph.el>, which is included in
;; this repo since it's not in MELPA.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(require 'dash)
(require 'graph)

;;;; Variables

(defvar org-graph-view-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'org-graph-view-jump)
    (define-key map [mouse-2] #'org-graph-view-zoom-in)
    (define-key map [mouse-3] #'org-graph-view-zoom-out)
    map)
  "Keymap.")

;;;; Customization


;;;; Commands

(defun org-graph-view ()
  (interactive)
  (cl-labels ((rec (tree)
                   (-let (((_element properties . children) tree))
		     (if properties
			 (cons (heading-string properties)
			       (mapcar #'rec children))
		       (mapcar #'rec children))))
              (heading-string (properties)
                              (-let* (((&plist :raw-value heading) properties)
                                      (face (face properties)))
                                (when heading
                                  (propertize heading
                                              'face face
                                              'properties (plist-put properties :buffer (current-buffer))))))
              (face (properties)
                    (-let (((&plist :begin :level) properties))
                      (or (when-let* ((color (org-entry-get begin "color")))
                            (list :foreground color))
                          (when level
                            (nth level org-level-faces))))))
    (org-with-wide-buffer
     (when (org-at-heading-p)
       (org-narrow-to-subtree))
     (let* ((graph-line-wid 1)
	    ;; (graph-draw-shape-side-border-fn #'ignore)
	    (graph (rec (org-element-parse-buffer 'headline)))
            (drawn (graph-draw-tree graph))
            (inhibit-read-only t))
       (with-current-buffer (org-graph-view-buffer)
         (erase-buffer)
         (save-excursion
           (insert drawn))
         (pop-to-buffer (current-buffer)))))))

(defun org-graph-view-jump (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window _pos-or-area (_x . _y) _timestamp
                    _object text-pos . _) position)
          ((&plist :buffer :begin)
           (get-text-property text-pos 'properties)))
    (when buffer
      (pop-to-buffer buffer)
      (goto-char begin)
      (org-reveal)
      (org-show-entry)
      (goto-char begin))))

(defun org-graph-view-zoom-in (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window _pos-or-area (_x . _y) _timestamp
                    _object text-pos . _) position)
          ((&plist :buffer :begin)
           (get-text-property text-pos 'properties)))
    (when buffer
      (pop-to-buffer buffer)
      (goto-char begin)
      (org-graph-view))))

(defun org-graph-view-zoom-out (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window _pos-or-area (_x . _y) _timestamp
                    _object text-pos . _) position)
          ((&plist :buffer :begin)
           (get-text-property text-pos 'properties)))
    (when buffer
      (pop-to-buffer buffer)
      (goto-char begin)
      (or (org-up-heading-safe)
          (goto-char (point-min)))
      (org-graph-view))))

;;;; Functions

(defun org-graph-view-buffer ()
  (or (get-buffer "*org-graph-view*")
      (with-current-buffer (get-buffer-create "*org-graph-view*")
        (toggle-truncate-lines 1)
        (read-only-mode 1)
        (use-local-map org-graph-view-map)
        (current-buffer))))

;;;; Footer

(provide 'org-graph-view)

;;; org-graph-view.el ends here

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
                            (nth (1- level) org-level-faces))))))
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

(cl-defun org-graph-view-graphviz (&key (layout "twopi"))
  (interactive)
  (let* ((node-id 0)
         (nodes (make-hash-table :test #'equal)))
    (cl-labels ((format-tree (tree &optional path)
                             (--map (format-node it path)
                                    (cddr tree)))
                (format-node (node &optional path)
                             (-let* (((_element _properties . children) node)
                                     (path (append path (list node))))
                               (list (--map (concat (node-id node) " -> " (node-id it) ";\n")
                                            children)
                                     (--map (format-node it path)
                                            children))))
                (node-color (node)
                            (-let (((_element (&plist :level) . children) node))
                              (--> (face-attribute (nth (1- level) org-level-faces) :foreground)
                                   (color-name-to-rgb it)
                                   (-let (((r g b) it))
                                     (color-rgb-to-hex r g b 2)))))
                (node-id (node)
                         (-let* (((element (properties &as &plist :begin) . children) node))
                           (or (car (gethash begin nodes))
                               (let* ((node-id (format "node%s" begin))
                                      (value (cons node-id node)))
                                 (puthash begin value nodes)
                                 node-id))))
                (node-properties (node)
                                 (cl-loop with (_element properties . _children) = node
                                          for (name property) in (list '(label :raw-value)
                                                                       '(style "filled")
                                                                       (list 'fillcolor #'node-color))

                                          collect (cl-typecase property
                                                    (keyword (cons name (plist-get properties property)))
                                                    (function (cons name (funcall property node)))
                                                    (string (cons name property)))))
                (monitor-wh ()
                            (-let* (((&alist 'mm-size (w h)) (car (display-monitor-attributes-list))))
                              (list (mm-in w) (mm-in h))))
                (mm-in (mm)
                       (* mm 0.04))
                (insert-val (key value)
                            (insert (format "%s=\"%s\"" key value) "\n")))
      (org-with-wide-buffer
       ;; (when (org-at-heading-p)
       ;;   (org-narrow-to-subtree))
       (-let* ((graph-line-wid 1)
               (root-pos (save-excursion
                           (when (org-before-first-heading-p)
                             (outline-next-heading))
                           (org-back-to-heading)
                           (point)))
               (graphviz (format-tree (org-element-parse-buffer 'headline)))
               (root-node-name (car (gethash root-pos nodes)))
               (inhibit-read-only t)
               ((&alist 'geometry (_ _ monitor-width-pix monitor-height-pix)
                        'mm-size (monitor-width-mm monitor-height-mm))
                (car (display-monitor-attributes-list)))
               (monitor-width-in (mm-in monitor-width-mm))
               (monitor-height-in (mm-in monitor-height-mm))
               (monitor-width-res (/ monitor-width-pix monitor-width-in))
               (monitor-height-res (/ monitor-height-pix monitor-height-in))
               (window-width-in (/ (window-text-width nil t) monitor-width-res))
               (window-height-in (/ (window-text-height nil t) monitor-height-res)))
         (with-temp-buffer
           (save-excursion
             (insert "digraph orggraphview {\n")
             (insert-val "layout" layout)
             (insert (format "size=\"%.1d,%.1d\";\n" window-width-in window-height-in))
             (insert "margin=\"0\";\n")
             (insert "ratio=\"fill\";\n")
             (insert "nodesep=\"0\";\n")
             (insert "mindist=\"0\";\n")
             (mapc #'insert (-flatten graphviz))
             (maphash (lambda (key value)
                        (insert (format "%s [%s];\n" (car value)
                                        (s-join ","
                                                (--map (format "%s=\"%s\"" (car it) (cdr it))
                                                       (node-properties (cdr value)))))))
                      nodes)
             (insert (format "root=\"%s\"" root-node-name))
             (insert "}"))
           (org-graph-view--view-graph)))))))

(defun org-graph-view--view-graph ()
  "Render and show graph in current buffer."
  (message "%s" (buffer-string))
  (if (zerop (call-process-region (point-min) (point-max) "circo" 'delete t nil
                                  "-Tsvg"))
      (let ((image (svg-image (libxml-parse-xml-region (point-min) (point-max)))))
        (with-current-buffer (org-graph-view-buffer)
          (erase-buffer)
          (insert-image image)
          (pop-to-buffer (current-buffer))))
    (error "Oops: %s" (buffer-string)))
  )

(defun org-graph-view--graphviz (tree)
  (let* ((node-id 0))
    (cl-labels ((rec (tree)
                     (-let (((_element properties . children) tree))
                       (if properties
                           (cons (heading-string properties)
                                 (mapcar #'rec children))
                         (mapcar #'rec children))))
                (new-node-id ()
                             ;; Return new node ID.
                             (format "node:%s" (cl-incf node-id)))
                ))
    ))

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

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

(cl-defun org-graph-view-graphviz (layout)
  (interactive (pcase current-prefix-arg
                 ('nil '("twopi"))
                 (_ (list (completing-read "Layout: " '("twopi" "circo" "dot"))))))
  (let* ((nodes (make-hash-table :test #'equal)))
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
                                                                       (list 'color (lambda (&rest _)
                                                                                      (face-attribute 'default :foreground)))
                                                                       (list 'fillcolor #'node-color)
                                                                       (list 'href #'node-href))

                                          collect (cl-typecase property
                                                    (keyword (cons name (plist-get properties property)))
                                                    (function (cons name (funcall property node)))
                                                    (string (cons name property))
                                                    (symbol (cons name (symbol-value property))))))
                (node-href (node)
                           (-let* (((element (properties &as &plist :begin) . children) node))
                             (format "%s" begin)))
                (monitor-wh ()
                            (-let* (((&alist 'mm-size (w h)) (car (display-monitor-attributes-list))))
                              (list (mm-in w) (mm-in h))))
                (mm-in (mm)
                       (* mm 0.04))
                (insert-vals (&rest pairs)
                             (cl-loop for (key value) on pairs by #'cddr
                                      do (insert (format "%s=\"%s\"" key value) "\n")))
                (format-val-list (&rest pairs)
                                 (s-wrap (s-join "," (cl-loop for (key value) on pairs by #'cddr
                                                              collect (format "%s=\"%s\"" key value)))
                                         "[" "]")))
      (org-with-wide-buffer
       (-let* ((graph-line-wid 1)
               (root-pos (save-excursion
                           (when (org-before-first-heading-p)
                             (outline-next-heading))
                           (org-back-to-heading)
                           (point)))
               (source-buffer (current-buffer))
               (graphviz (format-tree (org-element-parse-buffer 'headline)))
               (background-color (face-attribute 'default :background))
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
               (window-height-in (/ (window-text-height nil t) monitor-height-res))
               image-map)
         (with-temp-buffer
           (save-excursion
             (insert "digraph orggraphview {\n")
             (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
             (insert-vals "layout" layout
                          "bgcolor" background-color

                          "size" (format "%.1d,%.1d" window-width-in window-height-in)
                          "margin" "0"
                          "ratio" "fill"
                          "nodesep" "0"
                          "mindist" "0")
             (mapc #'insert (-flatten graphviz))
             (maphash (lambda (key value)
                        (insert (format "%s [%s];\n" (car value)
                                        (s-join ","
                                                (--map (format "%s=\"%s\"" (car it) (cdr it))
                                                       (node-properties (cdr value)))))))
                      nodes)
             (insert (format "root=\"%s\"" root-node-name))
             (insert "}"))
           (message "%s" (buffer-string))
           (let ((s (buffer-string)))
             (with-temp-buffer
               (insert s)
               (setq image-map (org-graph-view--cmapx))))
           (org-graph-view--view-graph :map image-map :source-buffer source-buffer)))))))

(defmacro org-graph-view--graphviz (type &rest body)
  "TYPE BODY."
  (declare (indent defun)
           (debug (stringp body)))
  `(if (zerop (call-process-region (point-min) (point-max) "circo" 'delete t nil
                                   (concat "-T" ,type)))
       (progn
         ,@body)
     (error "Oops: %s" (buffer-string))))

(cl-defun org-graph-view--view-graph (&key map source-buffer)
  "Render and show graph in current buffer."
  (if-let* ((svg-image (org-graph-view--svg)))
      (with-current-buffer (org-graph-view-buffer)
        (setf (image-property svg-image :map) map)
        (setf (image-property svg-image :source-buffer) source-buffer)
        (erase-buffer)
        (insert-image svg-image)
        (pop-to-buffer (current-buffer)))
    (error "Oops: %s" (buffer-string))))

(defun org-graph-view--svg ()
  "GRAPH."
  (org-graph-view--graphviz "svg"
    (let* ((image (svg-image (libxml-parse-xml-region (point-min) (point-max))))
           (svg-xml (buffer-string)))
      (with-current-buffer (get-buffer-create "*SVG XML*")
        (erase-buffer)
        (insert svg-xml))
      image)))

(defun org-graph-view--cmapx ()
  "GRAPH."
  (org-graph-view--graphviz "cmapx"
    (cl-labels ((convert-map (map)
                             (-let (((_map _props . areas) map))
                               (mapcar #'convert-area areas)))
                (convert-area (area)
                              (-let (((_area (&alist 'shape 'title 'href 'coords)) area))
                                (pcase-exhaustive shape
                                  ("poly" (list (cons 'poly (convert-coords coords)) href (list :help-echo title))))))
                (convert-coords (coords)
                                (->> coords (s-split ",") (-map #'string-to-number) (apply #'vector))))
      (let* ((cmapx (libxml-parse-xml-region (point-min) (point-max))))
        (with-current-buffer (get-buffer-create "*CMAPX*")
          (erase-buffer)
          (convert-map cmapx)
          (prin1 (convert-map cmapx) (current-buffer)))))))

(defun org-graph-view-jump (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window pos-or-area (_x . _y) _timestamp
                    _object text-pos . (_ (_image . (&plist :source-buffer))))
           position)
          (begin (string-to-number pos-or-area)))
    (when source-buffer
      (pop-to-buffer source-buffer)
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

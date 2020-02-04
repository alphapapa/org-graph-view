;;; org-graph-view.el --- View an Org file as a graph (like a mind map)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: outlines
;; Package-Requires: ((emacs "25.2") (org "9.0") (dash "2.13.0") (s "1.0"))

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
(require 'svg)

(require 'dash)

;;;; Variables

(defvar org-graph-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'org-graph-view-jump)
    (define-key map [mouse-2] #'org-graph-view-zoom-in)
    (define-key map [mouse-3] #'org-graph-view-zoom-out)
    map)
  "Keymap.")

(defvar org-graph-view-graph-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'org-graph-view-jump-from-graph)
    (define-key map [mouse-2] #'org-graph-view-zoom-in-from-graph)
    (define-key map [mouse-3] #'org-graph-view-zoom-out-from-graph)
    map)
  "Keymap.")

(define-minor-mode org-graph-view-mode
  "Drag mouse buttons to manipulate `org-graph-view'."
  :keymap 'org-graph-view-mode-map)

;;;; Customization


;;;; Commands

;; (defun org-graph-view ()
;;   (interactive)
;;   (cl-labels ((rec (tree)
;;                    (-let (((_element properties . children) tree))
;; 		     (if properties
;; 			 (cons (heading-string properties)
;; 			       (mapcar #'rec children))
;; 		       (mapcar #'rec children))))
;;               (heading-string (properties)
;;                               (-let* (((&plist :raw-value heading) properties)
;;                                       (face (face properties)))
;;                                 (when heading
;;                                   (propertize heading
;;                                               'face face
;;                                               'properties (plist-put properties :buffer (current-buffer))))))
;;               (face (properties)
;;                     (-let (((&plist :begin :level) properties))
;;                       (or (when-let* ((color (org-entry-get begin "color")))
;;                             (list :foreground color))
;;                           (when level
;;                             (nth (1- level) org-level-faces))))))
;;     (org-with-wide-buffer
;;      (when (org-at-heading-p)
;;        (org-narrow-to-subtree))
;;      (let* ((graph-line-wid 1)
;; 	    ;; (graph-draw-shape-side-border-fn #'ignore)
;; 	    (graph (rec (org-element-parse-buffer 'headline)))
;;             (drawn (graph-draw-tree graph))
;;             (inhibit-read-only t))
;;        (with-current-buffer (org-graph-view-buffer)
;;          (erase-buffer)
;;          (save-excursion
;;            (insert drawn))
;;          (pop-to-buffer (current-buffer)))))))

;;;###autoload
(cl-defun org-graph-view-graphviz (layout)
  (interactive (pcase current-prefix-arg
                 ('nil '("twopi"))
                 (_ (list (completing-read "Layout: " '("twopi" "circo" "dot"))))))
  (let* ((nodes (make-hash-table :test #'equal))
         (source-buffer (current-buffer))
         (root-pos (save-excursion
                     (when (org-before-first-heading-p)
                       (outline-next-heading))
                     (org-back-to-heading)
                     (point))))
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
                            (-let* (((_element (&plist :level) . _children) node))
                              (--> (face-attribute (nth (1- level) org-level-faces) :foreground nil 'default)
                                   (color-name-to-rgb it)
                                   (-let (((r g b) it))
                                     (color-rgb-to-hex r g b 2)))))
                (node-id (node)
                         (-let* (((_element (properties &as &plist :begin) . _children) node))
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
                           (-let* (((_element (properties &as &plist :begin) . _children) node))
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
      ;; Pop to view buffer so we know its dimensions.
      (pop-to-buffer (org-graph-view-buffer))
      (-let* ((graph (format-tree (with-current-buffer source-buffer
                                    (org-element-parse-buffer 'headline))))
              (background-color (face-attribute 'default :background))
              (root-node-name (car (gethash root-pos nodes)))
              (inhibit-read-only t)
              ((&alist 'geometry (_ _ monitor-width-px monitor-height-px)
                       'mm-size (monitor-width-mm monitor-height-mm))
               ;; TODO: Ensure we get the monitor the frame is on.
               (car (display-monitor-attributes-list)))
              (monitor-width-in (mm-in monitor-width-mm))
              (monitor-height-in (mm-in monitor-height-mm))
              (monitor-width-res (/ monitor-width-px monitor-width-in))
              (monitor-height-res (/ monitor-height-px monitor-height-in))
              (window-width-in (/ (window-text-width nil t) monitor-width-res))
              (window-height-in (/ (window-text-height nil t) monitor-height-res))
              (graphviz (with-temp-buffer
                          (save-excursion
                            (insert "digraph orggraphview {\n")
                            (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
                            (insert "node" (format-val-list "fontname" (face-attribute 'default :family)) ";\n")

                            (insert-vals "layout" layout
                                         "bgcolor" background-color
                                         "size" (format "%.1d,%.1d" window-width-in window-height-in)
                                         "margin" "0"
                                         "ratio" "fill"
                                         "nodesep" "0"
                                         "mindist" "0")
                            (mapc #'insert (-flatten graph))
                            (maphash (lambda (_key value)
                                       (insert (format "%s [%s];\n" (car value)
                                                       (s-join ","
                                                               (--map (format "%s=\"%s\"" (car it) (cdr it))
                                                                      (node-properties (cdr value)))))))
                                     nodes)
                            (insert (format "root=\"%s\"" root-node-name))
                            (insert "}"))
                          (buffer-string)))
              (image-map (org-graph-view--cmapx graphviz))
              (svg-image (org-graph-view--svg graphviz :map image-map :source-buffer source-buffer)))
        (erase-buffer)
        (insert-image svg-image)))))

(defmacro org-graph-view--graphviz (type &rest body)
  "TYPE BODY."
  (declare (indent defun)
           (debug (stringp body)))
  `(if (zerop (call-process-region (point-min) (point-max) "circo" 'delete t nil
                                   (concat "-T" ,type)))
       (progn
         ,@body)
     (error "Oops: %s" (buffer-string))))

;; (cl-defun org-graph-view--view-graph (&key map source-buffer)
;;   "Render and show graph in current buffer."
;;   (if-let* ((svg-image (org-graph-view--svg)))
;;       (with-current-buffer (org-graph-view-buffer)
;;         (setf (image-property svg-image :map) map)
;;         (setf (image-property svg-image :source-buffer) source-buffer)
;;         (erase-buffer)
;;         (insert-image svg-image)
;;         (pop-to-buffer (current-buffer)))
;;     (error "Oops: %s" (buffer-string))))

(cl-defun org-graph-view--svg (graph &key map source-buffer)
  "GRAPH."
  (with-temp-buffer
    (insert graph)
    (org-graph-view--graphviz "svg"
      (let* ((image (svg-image (libxml-parse-xml-region (point-min) (point-max))))
             ;; (svg-xml (buffer-string))
             )
        ;; (with-current-buffer (get-buffer-create "*SVG XML*")
        ;;   (erase-buffer)
        ;;   (insert svg-xml))
        (setf (image-property image :map) map)
        (setf (image-property image :source-buffer) source-buffer)
        image))))

(defun org-graph-view--cmapx (graph)
  "GRAPH."
  (with-temp-buffer
    (insert graph)
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
          ;; (with-current-buffer (get-buffer-create "*CMAPX*")
          ;;   (erase-buffer)
          ;;   (prin1 (convert-map cmapx) (current-buffer)))
          (convert-map cmapx))))))

(defun org-graph-view-jump (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window pos-or-area (_x . _y) _timestamp
                   _object _text-pos . _)
           position))
    (with-selected-window window
      (goto-char pos-or-area)
      (org-reveal)
      (org-show-entry)
      (goto-char pos-or-area)
      (call-interactively #'org-graph-view-graphviz))))

(defun org-graph-view-zoom-in (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window pos-or-area (_x . _y) _timestamp
                   _object _text-pos . _)
           position))
    (with-selected-window window
      (goto-char pos-or-area)
      (org-reveal)
      (org-show-entry)
      (goto-char pos-or-area)
      (org-narrow-to-subtree)
      (call-interactively #'org-graph-view-graphviz))))

(defun org-graph-view-zoom-out (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window pos-or-area (_x . _y) _timestamp
                   _object _text-pos . _)
           position))
    (with-selected-window window
      (widen)
      (goto-char pos-or-area)
      (when (org-up-heading-safe)
	(org-narrow-to-subtree))
      (call-interactively #'org-graph-view-graphviz))))

(defun org-graph-view-jump-from-graph (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window pos-or-area (_x . _y) _timestamp
                    _object _text-pos . (_ (_image . (&plist :source-buffer))))
           position)
          (begin (cl-typecase pos-or-area
                   (string (string-to-number pos-or-area))
                   (number pos-or-area))))
    (when source-buffer
      (pop-to-buffer source-buffer)
      (goto-char begin)
      (org-reveal)
      (org-show-entry)
      (goto-char begin)
      (call-interactively #'org-graph-view-graphviz))))

(defun org-graph-view-zoom-in-from-graph (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window pos-or-area (_x . _y) _timestamp
                    _object _text-pos . (_ (_image . (&plist :source-buffer))))
           position)
          (begin (cl-typecase pos-or-area
                   (string (string-to-number pos-or-area))
                   (number pos-or-area))))
    (when source-buffer
      (pop-to-buffer source-buffer)
      (widen)
      (goto-char begin)
      (org-narrow-to-subtree)
      (call-interactively #'org-graph-view-graphviz))))

(defun org-graph-view-zoom-out-from-graph (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window pos-or-area (_x . _y) _timestamp
                    _object _text-pos . (_ (_image . (&plist :source-buffer))))
           position)
          (begin (cl-typecase pos-or-area
                   (string (string-to-number pos-or-area))
                   (number pos-or-area))))
    (when source-buffer
      (pop-to-buffer source-buffer)
      (widen)
      (goto-char begin)
      (cond ((org-up-heading-safe)
             (org-narrow-to-subtree))
            (t (goto-char (point-min))))
      (call-interactively #'org-graph-view-graphviz))))

;;;; Functions

(defun org-graph-view-buffer ()
  "Return initialized \"*org-graph-view*\" buffer."
  (or (get-buffer "*org-graph-view*")
      (with-current-buffer (get-buffer-create "*org-graph-view*")
        (buffer-disable-undo)
        (setq cursor-type nil)
        (toggle-truncate-lines 1)
        (read-only-mode 1)
        (use-local-map org-graph-view-graph-map)
        (current-buffer))))

;;;; Footer

(provide 'org-graph-view)

;;; org-graph-view.el ends here

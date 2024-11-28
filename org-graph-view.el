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

;; A usable work-in-progress.  Feedback is welcome.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'map)
(require 'org)
(require 'subr-x)
(require 'svg)
(require 'warnings) ;; FIXME: Remove when not needed.

(require 'dash)
(require 's)

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

;;;; Customization

;;;;; Options

(defgroup org-graph-view nil
  "Settings for `org-graph-view'."
  ;; FIXME: Add url.
  :group 'org)

(defcustom org-graph-view-depth 3
  "Render nodes this many levels deep."
  :type 'integer)

(defcustom org-graph-view-overlap "voronoi"
  "How to handle overlapping.  See Graphviz documentation.
It seems unclear which is the best default, because each option
renders one or another layout slightly better than other
options."
  :type '(choice (const :tag "Voronoi" "voronoi")
		 (const :tag "Scale" "scale")
		 (const :tag "Allow overlap" "true")
		 (const :tag "VPSC" "vpsc")))

(defcustom org-graph-view-shape-default "oval"
  "Shape for non-to-do nodes.  See Graphviz documentation."
  :type 'string)

(defcustom org-graph-view-shape-todo "box"
  "Shape for to-do nodes.  See Graphviz documentation."
  :type 'string)

(defcustom org-graph-view-shape-done "plaintext"
  "Shape for done nodes.  See Graphviz documentation."
  :type 'string)

(defcustom org-graph-view-layout "twopi"
  "Default layout.  See Graphviz documentation."
  :type '(choice ((const :description "Pretty good layout.  Offers perspective/root node."
                         "twopi")
                  (const :description "Good all-around layout.  No perspective/root node."
                         "neato")
                  (const :description "Spacious layout, smaller labels with larger graphs.  Offers perspective/root node."
                         "circo")
                  (const :description "Very cozy layout, larger labels.  Some randomness.  No perspective/root node."
                         "fdp")
                  (const :description "Top-down, linear layout.  Not very efficient in terms of screen space."
                         "dot")
                  (const :description "Similar to fdp." "sfdp"))))

;;;;; Faces

(defface org-graph-view-selected
  `((t (;; :foreground ,(face-background 'default)
	:inherit highlight :inverse-video t)))
  "Face for selected nodes.")

;;;; Macros

;; (cl-defmacro debug-warn (&rest args)
;;   "Display a debug warning showing the runtime value of ARGS.
;; The warning automatically includes the name of the containing
;; function, and it is only displayed if `warning-minimum-log-level'
;; is `:debug' at runtime (which avoids formatting messages that
;; won't be shown).

;; Each of ARGS may be a string, which is displayed as-is, or a
;; symbol, the value of which is displayed prefixed by its name, or
;; a Lisp form, which is displayed prefixed by its first symbol.

;; Before the actual ARGS arguments, you can write keyword
;; arguments, i.e. alternating keywords and values.  The following
;; keywords are supported:

;; :buffer BUFFER   Name of buffer to pass to `display-warning'.
;; :level  LEVEL    Level passed to `display-warning', which see.
;;                  Default is :debug."
;;   (pcase-let* ((fn-name (with-current-buffer
;;                             (or byte-compile-current-buffer (current-buffer))
;;                           ;; This is a hack, but a nifty one.
;;                           (save-excursion
;;                             (beginning-of-defun)
;;                             (cl-second (read (current-buffer))))))
;;                (plist-args (cl-loop while (keywordp (car args))
;;                                     collect (pop args)
;;                                     collect (pop args)))
;;                ((map (:buffer buffer) (:level level)) plist-args)
;;                (level (or level :debug))
;;                (string (cl-loop for arg in args
;;                                 concat (pcase arg
;;                                          ((pred stringp) "%S ")
;;                                          ((pred symbolp)
;;                                           (concat (upcase (symbol-name arg)) ":%S "))
;;                                          ((pred listp)
;;                                           (concat "(" (upcase (symbol-name (car arg)))
;;                                                   (pcase (length arg)
;;                                                     (1 ")")
;;                                                     (_ "...)"))
;;                                                   ":%S "))))))
;;     `(when (eq :debug warning-minimum-log-level)
;;        (display-warning ',fn-name (format ,string ,@args) ,level ,buffer))))

;;;; Commands

;;;###autoload
(define-minor-mode org-graph-view-mode
  ;; FIXME: Not quite working.
  "Drag mouse buttons to manipulate `org-graph-view'."
  :keymap 'org-graph-view-mode-map)

;;;###autoload
(cl-defun org-graph-view (layout)
  (interactive
   (pcase current-prefix-arg
     ('nil (list org-graph-view-layout))
     (_ (list (completing-read "Layout: " (cadadr (get 'org-graph-view-layout 'custom-type)))))))
  (cl-labels ((window-dimensions-in (&optional (window (selected-window)))
                ;; Return WINDOW (width-in height-in) in inches.
                (with-selected-window window
                  (-let* (((&alist 'geometry (_ _ monitor-width-px monitor-height-px)
                                   'mm-size (monitor-width-mm monitor-height-mm))
                           ;; TODO: Ensure we get the monitor the frame is on.
                           (car (display-monitor-attributes-list)))
                          (monitor-width-in (mm-in monitor-width-mm))
                          (monitor-height-in (mm-in monitor-height-mm))
                          (monitor-width-res (/ monitor-width-px monitor-width-in))
                          (monitor-height-res (/ monitor-height-px monitor-height-in))
                          (window-width-in (/  (window-text-width nil t) monitor-width-res))
                          (window-height-in (/ (window-text-height nil t) monitor-height-res)))
                    (list window-width-in window-height-in
                          monitor-width-res monitor-height-res))))
              (mm-in (mm) (* mm 0.04)))
    (-let* ((graph-buffer (org-graph-view-buffer))
            ((width-in height-in width-res height-res)
             (save-window-excursion
               (pop-to-buffer graph-buffer)
               (window-dimensions-in)))
            (root-node-pos (save-excursion
                             (when (org-before-first-heading-p)
                               (outline-next-heading))
                             (org-back-to-heading t)
                             (point)))
            ((graph nodes) (org-graph-view--buffer-graph (current-buffer)))
            (graphviz (org-graph-view--format-graph graph nodes root-node-pos :layout layout
                                                    :width-in width-in :height-in height-in
                                                    ;; Average the two resolutions.
                                                    :dpi (/ (+ width-res height-res) 2)))
            (_ (with-temp-file "/tmp/org-graph-view.dot"
                 (insert graphviz)))
            (image-map (org-graph-view--graph-map graphviz))
            (svg-image (org-graph-view--svg graphviz :map image-map :source-buffer (current-buffer)))
            (inhibit-read-only t))
      (with-temp-file "/tmp/org-graph-view.svg"
        (insert (image-property svg-image :data)))
      (save-excursion
        (with-current-buffer graph-buffer
          (erase-buffer)
          (insert-image svg-image)
          (pop-to-buffer graph-buffer)
          (image-minor-mode))))))

(defun org-graph-view-jump (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window pos-or-area (_x . _y) _timestamp
                   _object _text-pos . _)
           position))
    (with-selected-window window
      (goto-char pos-or-area)
      (when (org-before-first-heading-p)
        (outline-next-heading))
      (org-reveal)
      (org-show-entry)
      (org-show-children)
      (goto-char pos-or-area)
      (call-interactively #'org-graph-view))))

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
      (call-interactively #'org-graph-view))))

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
      (call-interactively #'org-graph-view))))

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
      (setf (window-point) begin)
      (when (org-before-first-heading-p)
        (outline-next-heading))
      (org-reveal)
      (org-show-entry)
      (org-show-children)
      (save-excursion
        (call-interactively #'org-graph-view)))))

(defun org-graph-view-zoom-in-from-graph (event)
  (interactive "e")
  (-let* (((_type position _count) event)
          ((_window pos-or-area (_x . _y) _timestamp
                    _object _text-pos . (_ (_image . (&plist :source-buffer))))
           position)
          (begin (cl-typecase pos-or-area
                   (string (string-to-number pos-or-area))
                   (number pos-or-area))))
    (when (and source-buffer begin)
      (pop-to-buffer source-buffer)
      (widen)
      (goto-char begin)
      (when (org-at-heading-p)
        (org-narrow-to-subtree))
      (call-interactively #'org-graph-view))))

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
      (call-interactively #'org-graph-view))))

;;;; Functions

(cl-defun org-graph-view--buffer-graph (buffer)
  "Return (graph nodes) for BUFFER."
  (let ((nodes (make-hash-table :test #'equal)))
    (cl-labels ((format-tree (tree depth &optional path)
                  (--map (format-node it depth path)
                         (cddr tree)))
                (format-node (node depth &optional path)
		  (when (> depth 0)
		    (-let* (((_element _properties . children) node)
			    (path (append path (list node)))
                            (links
                             (cl-loop for link in (links-in node)
                                      collect (pcase-let (((map :from :to) link))
                                                (list (format "%s -> node%s [headclip=false;tailclip=false;constraint=false; style=dotted; arrowsize=0.25];\n"
                                                              from (pos-of to))
                                                      (format "node%s [label=\"%s\"]"
                                                              (pos-of to)
                                                              ;; HACK.
                                                              (org-with-point-at (pos-of to)
                                                                (s-word-wrap
                                                                 25 (s-replace "\"" "\\\""
                                                                               (org-entry-get nil "ITEM")))))))))
                            (nodes
                             (if children
                                 (list (--map (concat (node-id node) " -> " (node-id it) ";\n")
                                              children)
                                       (--map (format-node it (1- depth) path)
                                              children))
                               (list (concat (node-id node) ";\n")))))
                      (append nodes links))))
                (node-id (node)
                  (-let* (((_element (properties &as &plist :begin) . _children) node))
                    (unless begin
                      (setf begin (org-element-property :begin node)))
                    (or (car (gethash begin nodes))
                        (let* ((node-id (format "node%s" begin))
                               (value (cons node-id node)))
                          (puthash begin value nodes)
                          node-id))))
                (links-in (node)
                  "Return links in NODE."
                  (pcase-let* ((`(,_element ,(map :begin) . ,_children) node))
                    (unless begin
                      (setf begin (org-element-property :begin node)))
                    (org-with-point-at (goto-char begin)
                      (let ((end (org-entry-end-position)))
                        (cl-loop while (re-search-forward org-link-bracket-re end t)
                                 do (goto-char (match-beginning 0))
                                 for link = (save-match-data
                                              (pcase-let ((`(,_link ,(map :type :path))
                                                           (org-element-link-parser)))
                                                (pcase type
                                                  ("id" (list :from (node-id node) :to path)))))
                                 when link collect link
                                 do (goto-char (match-end 0)))))))
                (pos-of (id)
                  "Return buffer position of node having ID."
                  (org-with-wide-buffer
                   (cdr (org-id-find-id-in-file id (buffer-file-name))))))
      (with-current-buffer buffer
        (list (format-tree (org-element-parse-buffer 'headline)
			   (+ org-graph-view-depth (1- (or (org-current-level) 1))))
	      nodes)))))

(cl-defun org-graph-view--format-graph (graph nodes root-node-pos
                                              &key layout width-in height-in dpi)
  "Return Graphviz string for GRAPH and NODES viewed from ROOT-NODE-POS."
  (cl-labels ((node-properties
	        (node) (cl-loop with (_element properties . children) = node
			        for (name property) in
			        (list (list 'label #'node-label)
				      (list 'fillcolor #'node-color)
				      (list 'href #'node-href)
				      (list 'shape #'node-shape)
				      (list 'style #'node-style)
				      (list 'color #'node-pencolor)
				      (list 'fontcolor #'node-fontcolor)
				      (list 'penwidth #'node-penwidth))
			        collect (cl-typecase property
					  (keyword (cons name (->> (plist-get properties property)
								   (org-link-display-format)
								   (s-replace "\"" "\\\"")
								   (s-word-wrap 25))))
					  (function (cons name (funcall property node)))
					  (string (cons name property))
					  (symbol (cons name (symbol-value property))))))
	      (node-label
	        (node) (-let* (((_element (&plist :raw-value) . _children) node))
			 (s-word-wrap 25 (s-replace "\"" "\\\"" (org-link-display-format raw-value)))))
              (node-href
	        (node) (-let* (((_element (properties &as &plist :begin) . _children) node))
                         (unless begin
                           (setf begin (org-element-property :begin node)))
			 (format "%s" begin)))
	      (node-shape
	        (node) (-let* (((_element (&plist :todo-type) . children) node))
			 (pcase-exhaustive children
			   ('nil (pcase todo-type
				   ('nil org-graph-view-shape-done)

				   (_ org-graph-view-shape-todo)))
			   (_ org-graph-view-shape-default))))
              (node-style
	        (node) (-let* (((_element (&plist :begin) . children) node)
                               (begin (or begin (org-element-property :begin node)))
			       (base-style (pcase children
					     ('nil "solid")
					     (_ "filled")))
			       (selection-style (when (equal root-node-pos begin)
						  "bold")))
			 (string-join (delq nil
					    (list base-style selection-style))
				      ",")))
              (node-color
	        (node) (-let* (((_element (&plist :level :todo-type) . _children) node))
			 (pcase todo-type
			   ('nil (level-color level))
			   ('todo (level-color level))
			   ('done (level-color level)))))
              (node-fontcolor
	        (node) (-let* (((_element (&plist :level :begin) . children) node))
                         (unless begin
                           (setf begin (org-element-property :begin node)))
			 (pcase children
			   ('nil (level-color level))
			   (_ (if (equal begin root-node-pos)
				  (if (face-attribute 'org-graph-view-selected :inverse-video)
				      (face-attribute 'org-graph-view-selected :background nil 'default)
				    (face-attribute 'org-graph-view-selected :foreground nil 'default))
			        (face-attribute 'default :background))))))
              (level-color
	        (level) (color-name-to-hex
			 (face-attribute (or (nth (1- level) org-level-faces) 'default)
					 :foreground nil 'default)))
              (color-name-to-hex
	        (color) (-let (((r g b) (color-name-to-rgb color)))
			  (color-rgb-to-hex r g b 2)) )
              (node-pencolor (node)
                (-let* (((_element (&plist :level :todo-type :begin) . _children) node))
                  (unless begin
                    (setf begin (org-element-property :begin node)))
                  (pcase todo-type
                    ('nil (if (equal root-node-pos begin)
			      (face-attribute 'org-graph-view-selected :background nil 'default)
			    (level-color level)))
                    ('todo (color-name-to-hex (face-attribute 'org-todo :foreground nil 'default)))
                    ('done (level-color level)))))
              (node-penwidth (node)
                (-let* (((_element (&plist :todo-type) . _children) node))
                  (pcase todo-type
                    ('nil "1")
                    ('todo "2")
                    ('done "0.5"))))
              (insert-vals (&rest pairs)
                (cl-loop for (key value) on pairs by #'cddr
                         do (insert (format "%s=\"%s\"" key value) "\n")))
              (format-val-list (&rest pairs)
                (s-wrap (s-join "," (cl-loop for (key value) on pairs by #'cddr
                                             collect (format "%s=\"%s\"" key value)))
                        "[" "]")))
    (let ((root-node-name (car (gethash root-node-pos nodes))))
      (with-temp-buffer
        (save-excursion
          (insert "digraph orggraphview {\n")
          (insert "graph" (format-val-list "layout" layout
                                           "bgcolor" (face-attribute 'default :background)
                                           "size" (format "%.1din,%.1din!" width-in height-in)
                                           "viewport" (format "%.1d,%.1d"
                                                              (* 72 width-in) (* 72 height-in))
                                           ;; NOTE: The dpi setting is important, because
                                           ;; without it, sometimes cmap areas don't align
                                           ;; with the rendered elements.
                                           "dpi" (format "%s" dpi)
		                           "overlap" org-graph-view-overlap
                                           "margin" "0"
                                           "ratio" "fill"
                                           "nodesep" "0"
                                           "mindist" "0"
                                           "splines" "true")
                  ";\n")
          (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
          (insert "node" (format-val-list "fontname" (face-attribute 'default :family)
					  "nodesep" "1"
					  "mindist" "1")
		  ";\n")
          (mapc #'insert (-flatten graph))
          (maphash (lambda (_key value)
                     (insert (format "%s [%s];\n" (car value)
                                     (s-join ","
                                             (--map (format "%s=\"%s\"" (car it) (cdr it))
                                                    (node-properties (cdr value)))))))
                   nodes)
          (insert (format "root=\"%s\"" root-node-name))
          (insert "}"))
        ;; (debug-warn (buffer-string))
        (buffer-string)))))

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

;;;; Graphviz

(defmacro org-graph-view--graphviz (type &rest body)
  "Run Graphviz for TYPE on current buffer, then run BODY in it.
Current buffer should contain a Graphviz graph.  Graphviz is
called and replaces the buffer content with the rendered output."
  (declare (indent defun) (debug (stringp body)))
  `(if (zerop (call-process-region (point-min) (point-max) "dot" 'delete '(t nil) nil
                                   (concat "-T" ,type)))
       (progn
         ,@body)
     (error "Oops: %s" (buffer-string))))

(cl-defun org-graph-view--svg (graph &key map source-buffer)
  "Return SVG image for Graphviz GRAPH.
MAP is an Emacs-ready image map to apply to the image's
properties.  SOURCE-BUFFER is the Org buffer the graph displays,
which is applied as a property to the image so map-clicking
commands can find the buffer."
  (with-temp-buffer
    (insert graph)
    (org-graph-view--graphviz "svg"
      ;; (debug-warn (buffer-string))
      (save-excursion
	;; HACK: Remove "pt" units from SVG width and height.  See
	;; <https://gitlab.com/graphviz/graphviz/-/issues/867>.
	;; Although it doesn't seem to fix the problem, so some
	;; combinations of window and graph sizes still render parts (or
	;; most) of the SVG off-screen.  *sigh*
	(goto-char (point-min))
	;; (when (re-search-forward (rx "<svg width=\"" (group (1+ (not (any "\"")))) "\" "
	;;                              "height=\"" (group (1+ (not (any "\"")))) "\"")
	;;                          nil t)
	;;   (replace-match  (substring (match-string 1) nil -2)
        ;;                   t t nil 1)
	;;   (replace-match (substring (match-string 2) nil -2)
        ;;                  t t nil 2))
        )
      (save-excursion
        ;; HACK: Set SVG viewBox width/height to match the SVG
        ;; width/height.  This is the only way I've found to fix the
        ;; problem of the graph rendering outside the viewport and/or
        ;; the Emacs window.  I don't know why Graphviz is setting the
        ;; viewBox smaller than the image, but this workaround seems
        ;; like it will have to do for now.
        (goto-char (point-min))
        (if (re-search-forward (rx "<svg width=\"" (group (1+ (not (any "\"")))) "\" "
	                           "height=\"" (group (1+ (not (any "\"")))) "\"")
	                       nil t)
            (let ((width (match-string 1))
                  (height (match-string 2)))
              (if (re-search-forward (rx "viewBox=\""
                                         (group (1+ (or digit "."))) " "
                                         (group (1+ (or digit "."))) " "
                                         (group (1+ (or digit "."))) " "
                                         (group (1+ (or digit ".")))
                                         "\"")
	                             nil t)
                  (progn
                    (replace-match width t t nil 3)
                    (replace-match height t t nil 4))
                (display-warning 'org-graph-view "Unable to match SVG viewBox for fixing.")))
	  ;; (replace-match  (substring (match-string 1) nil -2)
          ;;                 t t nil 1)
	  ;; (replace-match (substring (match-string 2) nil -2)
          ;;                t t nil 2)
          (display-warning 'org-graph-view "Unable to match SVG width/height to fix viewBox."))
        )
      (let* ((image (apply #'create-image (buffer-string) 'svg t nil)))
        (setf (image-property image :map) map)
        (setf (image-property image :source-buffer) source-buffer)
        image))))

(defun org-graph-view--graph-map (graph)
  "Return image map for Graphviz GRAPH."
  (with-temp-buffer
    (insert graph)
    (org-graph-view--graphviz "cmapx"
      ;; (debug-warn (buffer-string))
      (mapcar (lambda (area)
                (pcase-let* ((`(area ,(map shape href coords)) area)
                             (coords-list (mapcar #'string-to-number
                                                  (split-string coords ","))))
                  (list (pcase-exhaustive shape
        	          ("circle" (pcase-let ((`(,x ,y ,r) coords-list))
                                      (cons 'circle (cons (cons x y) r))))
        	          ("poly" (cons 'poly (vconcat coords-list)))
        	          ("rect" (pcase-let ((`(,x0 ,y0 ,x1 ,y1) coords-list))
                                    (cons 'rect
                                          (cons (cons x0 y0) (cons x1 y1))))))
        	        href (list 'help-echo href))))
              (cddr (libxml-parse-xml-region (point-min) (point-max))))
      ;; (cl-labels ((convert-map
      ;;   	    (map) (-let (((_map _props . areas) map))
      ;;   		    (mapcar #'convert-area areas)))
      ;;             (convert-area
      ;;   	    (area) (-let (((_area (&alist 'shape 'title 'href 'coords)) area))
      ;;   		     (list (pcase-exhaustive shape
      ;;   			     ("circle" (cons 'circle (convert-circle coords)))
      ;;   			     ("poly" (cons 'poly (convert-poly coords)))
      ;;   			     ("rect" (cons 'rect (convert-rect coords))))
      ;;   			   href (list :help-echo title))))
      ;;             (convert-circle
      ;;   	    (coords) (-let (((x y r) (->> coords (s-split ",") (-map #'string-to-number))))
      ;;   		       (cons (cons x y) r)))
      ;;             (convert-poly
      ;;   	    (coords) (->> coords (s-split ",") (-map #'string-to-number) (apply #'vector)))
      ;;             (convert-rect
      ;;   	    (coords) (-let (((x0 y0 x1 y1)
      ;;   			     (->> coords (s-split ",") (-map #'string-to-number))))
      ;;   		       (cons (cons x0 y0) (cons x1 y1)))))
      ;;   (let* ((cmapx (libxml-parse-xml-region (point-min) (point-max))))
      ;;     (convert-map cmapx)))
      )))

;;;; Footer

(provide 'org-graph-view)

;;; org-graph-view.el ends here

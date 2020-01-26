;;; graph.el --- Draw directed graphs in ascii

;; Copyright (C) 2017 by David ZUBER

;; Author: David ZUBER <zuber.david@gmx.de>
;; URL: https://github.com/storax/graph.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.2") (s "1.11.0") (dash "2.13.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A graph layout library for Emacs Lisp.
;; The algorithm is a port from <https://github.com/drcode/vijual>,
;; a Clojure library by Conrad Barski, who deserves most of the credits.

;;; Code:
(require 's)
(require 'seq)
(require 'dash)

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl))

(cl-defstruct graph-shape x y width height type text dir on-top data)

(cl-defstruct graph-treen
  id x y width height text wrapped-text
  line-right line-left line-y line-ypos
  leaf parent parent-line-y children data)

(cl-defstruct graph-btreen ind width text left right)

(defun graph--letbinding-gen (type binding var)
  "Helper for creating bindings for structures of TYPE.

BINDING is a list of slots or cons of (slot symbol).
VAR is the variable to bind."
  (--map (if (consp it)
             (list (car it) `(cl-struct-slot-value ',type ',(cdr it) ,var))
           (list it `(cl-struct-slot-value ',type ',it ,var)))
         binding))

(defmacro let-shape (binding shape &rest body)
  "Bind the slots in BINDING to the values in SHAPE and execute BODY.

For example, this code:

  (let-shape (x y) shape
     (message \"%s\ %s\" x y))

expands to:

  (let ((x (graph-shape-x shape))
        (y (graph-shape-y shape)))
    (message \"%s %s\" x y))"
  (declare (indent defun))
  `(let ((shape ,shape))
     (let ,(graph--letbinding-gen 'graph-shape binding 'shape)
       ,@body)))

(defmacro let-treen (binding treen &rest body)
  "Bind the slots in BINDING to the values in TREEN and execute BODY.

For example, this code:

  (let-treen (x y) treenode
     (message \"%s\ %s\" x y))

expands to:

  (let ((x (graph-treen-x treenode))
        (y (graph-treen-y treenode)))
    (message \"%s %s\" x y))"
  (declare (indent defun))
  `(let ((treen ,treen))
     (let ,(graph--letbinding-gen 'graph-treen binding 'treen)
       ,@body)))

(defmacro let-btreen (binding btreen &rest body)
  "Bind the slots in BINDING to the values in BTREEN and execute BODY.

For example, this code:

  (let-btreen (ind width) btreenode
     (message \"%s\ %s\" ind width))

expands to:

  (let ((ind (graph-btreen-ind treenode))
        (width (graph-btreen-width treenode)))
    (message \"%s %s\" x y))"
  (declare (indent defun))
  `(let ((btreen ,btreen))
     (let ,(graph--letbinding-gen 'graph-btreen binding 'btreen)
       ,@body)))

(defun graph-half (x)
  "Devide X by two."
  (/ x 2.0))

(defun graph-fill (c &optional n)
  "Return a string with the character C repeated N times."
  (let ((n (max 0 (or n 1))))
    (make-string n c)))

(defun graph-integer-shapes (shapes)
  "Take a sequence of SHAPES and floor all the dimensions for ASCII rendering.

See `graph-shape'."
  (--map
   (let-shape (x y width height type) it
     (let ((nu-x (floor x))
           (nu-y (floor y))
           (newitem (copy-graph-shape it)))
       (setf (graph-shape-x newitem) nu-x
             (graph-shape-y newitem) nu-y
             (graph-shape-width newitem) (- (floor (+ x width)) nu-x)
             (graph-shape-height newitem) (- (floor (+ y height)) nu-y))
       newitem))
   shapes))

(defun graph-rect-relation (ypos y height)
  "Calculate whether a rectangle intersects a YPOS.

Y is the y position of the rectangle.
HEIGHT is the height of the rectangle.

Returns 'on, 'in or 'nil."
  (let ((bottom (- (+ y height) 1)))
    (cond ((or (= ypos y) (= ypos bottom)) 'on)
          ((and (> ypos y) (< ypos bottom)) 'in))))

(defun graph-shapes-height (shapes)
  "Return the maxium y value that is covered by the given SHAPES."
  (apply 'max (--map (let-shape (y height) it (+ y height)) shapes)))

(defun graph-shapes-width (shapes)
  "Return the maximum x value that is covered by the given SHAPES."
  (apply 'max (--map (let-shape (x width) it (+ x width)) shapes)))

(defun graph-iter-height (shapes)
  "Iterate over all y values for the height of the given SHAPES.

Starts at 0."
  (number-sequence 0 (- (graph-shapes-height shapes) 1)))

(defun graph-filter-shapes-at-ypos (shapes ypos)
  "Return the SHAPES that are visible at YPOS."
  (--filter (let-shape (y height) it (graph-rect-relation ypos y height)) shapes))

(defun graph-x-sort-shapes (shapes)
  "Sort the given SHAPES.

Shapes that start at a lower x value come first.
If x value is the same the narrower shape takes precedence.
If they are the same width the higher shape takes precedence."
  (sort (copy-sequence shapes)
        (lambda (a b)
          (let-shape ((ax . x) (aw . width) (ah . height)) a
            (let-shape ((bx . x) (bw . width) (bh . height)) b
              (or (< ax bx)
                  (and (= ax bx) (< aw bw))
                  (and (= ax bx) (= aw bw) (> ah bh))))))))

(defun graph-draw-shapes (shapes)
  "Render a list of SHAPES into text.

The two current shape types are 'rect, which represent nodes and
lines between nodes (simply rectangles of minimal width).
The arrow heads are represented as type 'arrow."
  (let ((drawn ""))
    (dolist (ypos (graph-iter-height shapes))
      (let* ((xcur 0)
             (sorted-shapes (graph-x-sort-shapes (graph-filter-shapes-at-ypos shapes ypos)))
             (rest-shapes sorted-shapes))
        (while rest-shapes
          (let ((result (graph-draw-shapes-pos ypos xcur rest-shapes)))
            (setq xcur (cdr (assoc 'xcur result))
                  rest-shapes (cdr (assoc 'shapes result))
                  drawn (concat drawn (cdr (assoc 'drawn result)))))))
      (setq drawn (concat drawn "\n")))
       drawn))

(defun graph-new-xcur (oldx first-x drawn)
  "Return a new xcur value.

OLDX is the current x value.
FIRST-X is the x value of the first shape that is currently drawn.
DRAWN is the string that we got so far.
Returns the max of OLDX and (FIRST-X + (length DRAWN))."
  (max (+ first-x (length drawn)) oldx))

(defun graph-crop-already-drawn (xcur x s)
  "Return a string starting at xcur.

XCUR is the current x cursor possition.
X is the x position of the shape we are drawing.
If X is smaller than XCUR we have to drop that part
because we already drawn over that area.
Else we just return S."
  (if (< x xcur)
      (substring s (min (length s) (- xcur x)))
    s))

(defun graph-get-next-shapes-to-draw (overlapping shape more)
  "Return the next shapes to draw.

OVERLAPPING is a boolean if shape is currently overlapping
with the first of more.
SHAPE is the current shape that is drawn.
MORE is the rest of the shapes."
  (if overlapping
      (append (list (car more) shape) (cdr more))
    more))

(defun graph-draw-custom (drawn data)
  "Wrapper function for all shape drawing functions.

This is called with the drawn text plus the user data if any for
the shape that is drawn."
  drawn)

(defvar graph-draw-custom-fn 'graph-draw-custom
  "Wrapper function for all shape drawing functions.

This is called with the drawn text plus the user data if any for
the shape that is drawn.")

(defun graph-draw-arrow (dir data)
  "Draw an arrow pointing in the given DIR.

DIR is either 'right, 'left, 'up, 'down
or something else if there is no clear direction.
DATA is arbitrary user data associated witht the shape."
  (pcase dir
    ('right ">")
    ('left "<")
    ('up "^")
    ('down "V")
    (_ "*")))

(defvar graph-draw-arrow-fn 'graph-draw-arrow
  "Function for drawing an arrow.")

(defun graph-draw-cap (dir data)
  "Draw a cap in the given DIR.

DIR is either 'right, 'left, 'up, or 'down.
DATA is arbitrary user data associated witht the shape."
  (pcase dir
    ('right "-")
    ('left "-")
    ('up "|")
    ('down "|")))

(defvar graph-draw-cap-fn 'graph-draw-cap
  "Function for drawing a cap.")

(defun graph-draw-other-type-edge (type data)
  "Draw the border edge of misc types.

TYPE could be rect or nil.
DATA is arbitrary user data associated witht the shape."
  "+")

(defvar graph-draw-other-type-edge-fn 'graph-draw-other-type-edge
  "Function for drawing a border edge of misc types.")

(defun graph-draw-border-mid (width data)
  "Draw border middle of given WIDTH.

If WIDTH is 0 or negative, return an empty string.
DATA is arbitrary user data associated witht the shape."
  (graph-fill ?- width))

(defvar graph-draw-border-mid-fn 'graph-draw-border-mid
  "Function for drawing the border middle part of a shape.")

(defun graph-draw-wrapped (func &rest arguments)
  (funcall graph-draw-custom-fn (apply func arguments) (car (last arguments))))

(defun graph-draw-border (type dir width &optional data)
  "Draw the border of a shape.

Shape has the given TYPE, DIR and WIDTH.
DATA is arbitrary user data associated witht the shape."
  (concat
   (pcase type
     ('arrow (graph-draw-wrapped graph-draw-arrow-fn dir data))
     ('cap (graph-draw-wrapped graph-draw-cap-fn dir data))
     (_ (graph-draw-wrapped graph-draw-other-type-edge-fn type data)))
   (graph-draw-wrapped graph-draw-border-mid-fn (- width 2) data)
   (when (> width 1)
     (graph-draw-wrapped graph-draw-other-type-edge-fn type data))))

(defun graph-draw-shape-side-border (data)
  "Draw the side border of a shape.

DATA is arbitrary user data associated witht the shape."
  "|")

(defvar graph-draw-shape-side-border-fn 'graph-draw-shape-side-border
  "Function for drawing the side border of a shape.")

(defun graph-draw-shape-space (width data)
  "Draw WIDTH amount of space of a shape.

If WIDTH is negative or 0 return an empty string.
DATA is arbitrary user data associated witht the shape."
  (graph-fill ?\s width))

(defvar graph-draw-shape-space-fn 'graph-draw-shape-space
  "Function for drawing the empty space of a shape.")

(defun graph-draw-text (text data)
  "Draw the given TEXT of a shape.

DATA is arbitrary user data associated witht the shape."
  text)

(defvar graph-draw-text-fn 'graph-draw-text
  "Function for drawing text.")

(defun graph-draw-body-line (ypos y width text &optional data)
  "Draw the body of a shape for a given YPOS.

Y is the y position of the shape.
WIDTH is the width of the shape.
TEXT is the text of the shape.
DATA is arbitrary user data associated witht the shape."
  (concat
   (graph-draw-wrapped graph-draw-shape-side-border-fn data)
   (when (> width 1)
     (let* ((index (- ypos y 1))
            (s (if (>= index (length text))
                   ""
                 (graph-draw-wrapped graph-draw-text-fn (elt text index) data))))
       (concat s (graph-draw-wrapped graph-draw-shape-space-fn (- width (length s) 2) data)
               (graph-draw-wrapped graph-draw-shape-side-border-fn data))))))

(defun graph-draw-at-ypos (ypos shape)
  "Draw at YPOS the given SHAPE."
  (let-shape (dir type width height y text data) shape
    (if (equal 'on (graph-rect-relation ypos y height))
        (graph-draw-border type dir width data)
      (graph-draw-body-line ypos y width text data))))

(defun graph-get-overlapping-text (s x width on-top shapes)
  "Calculate the overlapping text.

S is the text.
X is the x position of the shape.
WIDTH is the width of the shape.
If non-nil, the shape is ON-TOP.
SHAPES are the other shapes that we have to check for overlapping."
  (or
   (dolist (shape shapes)
     (let-shape ((x2 . x) (width2 . width) (on-top2 . on-top)) shape
       (when (<= width2 (+ x width))
         (when (and (<= (+ x2 width2) (+ x width)) (or (not on-top) on-top2))
           (return (list (< (+ x2 width2) (+ x width)) (substring s 0 (min (length s) (- x2 x)))))))))
   (list nil s)))

(defun graph-draw-shapes-pos (ypos xcur shapes)
  "Return the text and the next x position to draw.

YPOS is the current line to render.
XCUR is the current x position to render.
SHAPES are the shapes torender.

Returns an alist with 'xcur, 'shapes, and 'drawn as keys."
  (let* (drawn
         (shape (car shapes))
         (more (cdr shapes)))
    (let-shape (x y width text dir on-top data) shape
      (when (<= xcur x) ; draw spaces until the start of the first shape
        (setq drawn (concat drawn (graph-fill ?\s (- x xcur)))))
      (let* ((s (graph-draw-at-ypos ypos shape))
             (overlapping-result (graph-get-overlapping-text s x width on-top more))
             (overlapping (car overlapping-result))
             (s (cadr overlapping-result)))
        (list (cons 'xcur (graph-new-xcur xcur x s))
              (cons 'shapes (graph-get-next-shapes-to-draw overlapping shape more))
              (cons 'drawn (concat drawn (graph-crop-already-drawn xcur x s))))))))

(defun graph-numbered (lst)
  "Enumerate the given LST."
  (-map-indexed 'cons lst))

(defun graph-label-text (text)
  "Return a text string representing the TEXT for the item.

It handles the special case of symbols, so that 'oak-tree ==> 'oak tree'."
  (if (string-or-null-p text)
      (or text "")
    (if (symbolp text)
        (s-replace "-" " " (symbol-name text))
      (format "%s" text))))

(defun graph-center (lines width height)
  "Center the given LINES.

WIDTH and HEIGHT are the dimensions of the shape."
  (let ((n (length lines))
        (lines (--map (concat (make-string (floor (graph-half (- width (length it)))) ?\s) it)
                       lines)))
    (if (< n height)
        (append (cl-loop repeat (graph-half (- height n)) collect "") lines)
      lines)))

(defun graph-positions (pred coll)
  "Return a sequence with the positions at which PRED is t for items in COLL."
  (cl-loop for x to (- (length coll) 1) when (funcall pred (elt coll x)) collect x))

(defun graph-wrap (text width)
  "Optimally wrap TEXT to fit within a given WIDTH, given a monospace font."
  (mapcar
   'concat
   (let (lines)
     (while (> (length text) 0)
       (let* ((mw (min width (length text)))
              (spc (graph-positions (lambda (x) (equal ?\s x)) (substring text 0 mw))))
         (if spc
             (if (= 0 (car (last spc)))
                 (setq text (substring text 1))
               (progn
                 (setq lines (nconc lines (cons (substring text 0 (car (last spc))) nil))
                       text (substring text (+ 1 (car (last spc)))))))
           (progn
             (setq lines (nconc lines (cons (substring text 0 mw) nil))
                   text (substring text mw))))))
     lines)))

(defun graph-horizontal (dir)
  "Return the given DIR if it's horizontal."
  (car (memq dir '(right left))))

(defun graph-vertical (dir)
  "Return the given DIR if it's horizontal."
  (car (memq dir '(up down))))

;;Scan Functions
;; A "scan" is a run-length-encoded list of heights that s used to calculate packing of tree and graphs.
;; An example scan would be [[0 5] [7 10]] which would mean
;; "The height is 5 for an x between 0 (inclusive) and 7 (exclusive).
;; The height is 10 for any x greater than or equal to 7."
(defun graph--scan-add (scan cury xend)
  "Add a pair to the given SCAN.

Scan is a list of (x y) pairs.
CURY is the potential y position to add.
XEND is the potential x position to add."
  (if scan
      (let* ((a (car scan))
             (ax (car a))
             (ay (cadr a))
             (d (cdr scan)))
        (if (<= ax xend)
            (graph--scan-add d ay xend)
          (cons (list xend cury) scan)))
    (list (list xend cury))))

(defun graph--scan-advance (scan cury x y xend)
  (if scan
      (let* ((a (car scan))
             (ax (car a))
             (ay (cadr a))
             (d (cdr scan)))
        (if (> x ax)
            (cons a (graph--scan-advance d ay x y xend))
          (cons (list x y) (graph--scan-add scan cury xend))))
    (list (list x y) (list xend cury))))

(defun graph-scan-add (scan x y wid)
  "Add a new height bar at x with a width of wid and a height of y."
  (let ((xend (+ x wid)))
    (graph--scan-advance scan nil x y xend)))

(defun graph-scan-lowest-y (scan x width)
  "Find in SCAN the lowest y that is available at X with the given WIDTH.

THe line should not intersec with the scan."
  (let (cury besty)
    (or (cl-loop
        while scan do
        (let ((ax (caar scan))
              (ay (cadar scan))
              (d (cdr scan)))
          (cond
           ((<= (+ x width) ax)
            (if besty
                (return (max besty cury))
              (return cury)))
           ((< x ax)
            (setq scan d
                  besty (if besty
                            (max besty ay cury)
                          (if cury
                              (max ay cury)
                            ay))
                  cury ay))
           (t (setq scan d
                    cury ay)))))
        (if besty
            (max besty cury)
          cury))))

;;This code is specific to ascii rendering

(defvar graph-ascii-wrap-threshold 10
  "During ascii rendering, text in boxes wrap after this many characters.")

(defun graph-wrap-fn (text &optional width height)
  "The default wrap functions to wrap the given TEXT to fit in WIDTH and HEIGHT."
  (if (not width)
      (--map (concat " " it) (graph-wrap text graph-ascii-wrap-threshold))
    (--map (concat " " it)
           (graph-center (graph-wrap text (max 1 (- width 4)))
                         (max 1 (- width 4))
                         (- height 3)))))

(defvar graph-node-padding 1
  "Padding between nodes.")

(defvar graph-row-padding 8
  "Padding between rows.")

(defvar graph-line-wid 1
  "Line width.")

(defvar graph-line-padding 1
  "Line padding.")

(defun graph-height-fn (text)
  "Height of a box given a TEXT."
  (+ (length (graph-wrap text graph-ascii-wrap-threshold)) 2))

(defun graph-width-fn (text)
  "Width of a box given a TEXT."
  (+ (min (length text) graph-ascii-wrap-threshold) 4))

;;Functions specific to tree drawing

(defun graph--tree-to-shapes (tree)
  "Convert a full layed-out TREE into a bunch of shapes to be sent to the rendering backend."
  (apply 'append
         (mapcar
          (lambda (node)
            (let-treen (x y width height text line-left line-right line-ypos leaf parent-line-y data) node
              (append
               (when parent-line-y
                 (list (make-graph-shape :type 'rect
                                         :x (+ x (graph-half width) (- (graph-half graph-line-wid)))
                                         :y parent-line-y
                                         :width graph-line-wid
                                         :height (+ 1 (- y parent-line-y)))))
               (list (make-graph-shape :type 'rect :text (graph-wrap-fn text width height)
                                       :x x :y y :width width :height height :data data))
               (unless leaf
                 (list (make-graph-shape :type 'rect
                                         :x line-left :y line-ypos
                                         :width (- line-right line-left)
                                         :height graph-line-wid)
                       (make-graph-shape :type 'rect
                                         :x (+ x (graph-half width) (- (graph-half graph-line-wid)))
                                         :y (- (+ y height) 1)
                                         :width graph-line-wid
                                         :height (+ (- line-ypos y height) 2)))))))
          tree)))

(defun graph-make-rows (tree)
  "Take a TREE and convert it into rows.

This is needed since items in the same row and their widths will affect the spacing and layout of items."
  (when tree
    (cons (mapcar (lambda (node)
                    (let-treen (text id parent children data) node
                      (make-graph-treen :text text :id id :parent parent :leaf (seq-empty-p children) :data data)))
                  tree)
          (graph-make-rows
           (seq-mapcat (lambda (node)
                         (let-treen (id children) node
                           (--map (let ((newc (copy-graph-treen it)))
                                    (setf (graph-treen-parent newc) id)
                              newc)
                                   children)))
                       tree)))))

(defun graph-wrap-text (rows)
  "Calculate the wrapped text of each tree item in ROWS and their height.

The height depends on how the text is broken into lines."
  (--map (--map (let ((text (graph-treen-text it))
                      (newitem (copy-graph-treen it)))
                  (setf (graph-treen-wrapped-text newitem) (graph-wrap-fn text)
                        (graph-treen-height newitem) (graph-height-fn text))
                  newitem)
                it) rows))

(defun graph-row-pos (row y)
  "Calculate preliminary x positions for nodes in ROW.

Y is unused.
This will be refined later by the calculations from `graph-space' and `graph-space-row'."
  (let ((x 0))
    (--map
     (let* ((text (graph-treen-text it))
            (w (graph-width-fn text))
            (newitem (copy-graph-treen it))
            (oldx x))
       (setq x (+ oldx w graph-node-padding))
       (setf (graph-treen-x newitem) oldx
             (graph-treen-width newitem) w
             (graph-treen-height newitem) (graph-height-fn text))
       newitem)
     row)))

(defun graph-parent-p (child parent)
  "Return t if CHILD's parent is PARENT."
  (let ((cp (graph-treen-parent child))
        (pid (graph-treen-id parent)))
    (and cp pid (equal cp pid))))

(defun graph-child-p (parent child)
  "Return t if PARENT has the given CHILD."
  (graph-parent-p child parent))

(defun graph-space-row (fun total-width target-row row remaining)
  "Calculate the x positions of tree nodes.

All calculations start from the longest row in the tree.
This function is then used to position nodes upwards
or downwards from the longest row.
Each row is positioned relative a 'target row',
which is the neighboring row nearest to the longest row.

FUN is either `graph-child-p' or `graph-parent-p'."
  (let ((curx 0))
    (mapcar
     (lambda (item)
       (let-treen (width id) item
         (let* ((newitem (copy-graph-treen item))
                (children (--filter (funcall fun it item) target-row))
                (child-pos (--map (+ (graph-treen-x it) (graph-half (graph-treen-width it)))
                                   children))
                (nu-remaining (- remaining width graph-node-padding))
                (nu-x (if child-pos
                          (let* ((child (car children))
                                 (siblings (--filter (funcall fun child it) row))
                                 (left-scoot (if (and (< 1 (length siblings)) (= (graph-treen-id (car siblings)) id))
                                                 (graph-half (- (apply '+ (--map (+ (graph-treen-width it) graph-node-padding)
                                                                                  siblings))
                                                                graph-node-padding (graph-treen-width child)))
                                               0))
                                 (k (- (graph-half (+ (car child-pos) (car (last child-pos)))) (graph-half width) left-scoot))
                                 (nu-right (+ k width graph-node-padding)))
                            (if (< k curx)
                                curx
                              (if (< (- total-width nu-right) nu-remaining)
                                  (- total-width nu-remaining graph-node-padding width)
                                k)))
                        curx)))
           (setq remaining nu-remaining
                 curx (+ nu-x width graph-node-padding))
           (setf (graph-treen-x newitem) nu-x)
           newitem)))
     row)))

(defun graph-space (fun total-width target-row rest)
  "Use space-row for a list of rows."
  (when rest
    (let* ((curx 0)
           (row (caar rest))
           (remaining (cadar rest))
           (more (cdr rest))
           (nu-row (graph-space-row fun total-width target-row row remaining)))
      (cons nu-row (graph-space fun total-width nu-row more)))))

(defun graph-bounds (node)
  "Calculate the boundary of the given NODE."
  (let-treen (x width) node
    (+ x (graph-half width))))

(defun graph-horz-lines (rows)
  "Calculate the left and right extents of the horizontal lines below nodes in ROWS that lead to their children."
  (cl-mapcar (lambda (cur next)
               (--map (let* ((id (graph-treen-id it))
                             (ranges (cons (graph-bounds it)
                                           (cl-loop for chi in next when
                                                    (= id (graph-treen-parent chi))
                                                    collect (graph-bounds chi))))
                             (newcur (copy-graph-treen it)))
                  (setf (graph-treen-line-left newcur) (- (apply 'min ranges) (graph-half graph-line-wid))
                        (graph-treen-line-right newcur) (+ (apply 'max ranges) (graph-half graph-line-wid)))
                  newcur)
                       cur))
             rows
             (append (cdr rows) (list (list)))))

(defun graph-level-lines (lined)
  "Stack horizontal lines of the LINED shapes vertically in an optimal fashion.

Nodes should be able to connect to their children in a tree with the least amount of inbetween space."
  (--map (let ((group-right 0)
               (line-y 0))
     (--map (let-treen (leaf line-left line-right x width) it
              (let ((newitem (copy-graph-treen it)))
                (cond (leaf it)
                      ((and group-right (> (+ group-right graph-line-wid) line-left))
                       (setq line-y (if (<= (+ x (graph-half width)) (+ group-right graph-line-padding))
                                        (- line-y 1)
                                      (+ line-y 1))))
                      (t (setq line-y 0)))
                (setq group-right (max line-right group-right))
                (setf (graph-treen-line-y newitem) line-y)
                newitem))
            it))
         lined))

(defun graph-lev-children (levlines)
  "Update children with the level of the horizontal line of their parents."
  (cl-mapcar
   (lambda (cur par)
     (--map
      (let* ((newitem (copy-graph-treen it))
             (parent (graph-treen-parent it))
             (k (car (--filter (= (graph-treen-id it) parent) par)))
             (liney (when k (graph-treen-line-ypos k))))
        (setf (graph-treen-parent-line-y newitem) liney)
        newitem)
      cur))
   levlines
   (cons nil levlines)))

(defun graph-place-boxes (scan acc row)
  "Place boxes as high as possible during tree packing."
  (cl-loop while t do
    (if row
        (let* ((item (car row))
               (newitem (copy-graph-treen item))
               (r (cdr row)))
          (let-treen (x width height) item
            (let ((y (graph-scan-lowest-y scan x width)))
              (setf (graph-treen-y newitem) y)
              (setq scan (graph-scan-add scan x (+ y height graph-line-padding) width)
                    acc (cons newitem acc)
                    row r))))
      (return (list (reverse acc) scan)))))

(defun graph-place-lines (scan acc row)
  "Place lines as hight as possible during tree packing."
  (cl-loop while t do
    (if row
        (let* ((item (car row))
               (line-left (graph-treen-line-left item))
               (line-right (graph-treen-line-right item))
               (leaf (graph-treen-leaf item))
               (r (cdr row))
               (line-width (- line-right line-left))
               (cury (graph-scan-lowest-y scan line-left line-width)))
          (if leaf
              (setq acc (cons item acc)
                    row r)
            (let ((newitem (copy-graph-treen item)))
              (setf (graph-treen-line-ypos newitem) cury)
              (setq scan (graph-scan-add scan line-left (+ cury graph-line-wid graph-line-padding) line-width)
                    acc (cons newitem acc)
                    row r))))
      (return (list (reverse acc) scan)))))

(defun graph--pack-tree (scan rows)
  (when rows
    (let* ((row (car rows))
           (more (cdr rows))
           (rowscan (graph-place-boxes scan nil row))
           (row (car rowscan))
           (scan (cadr rowscan))
           (sorted-row (cl-sort row '< :key 'graph-treen-line-y))
           (lps (graph-place-lines scan nil sorted-row))
           (lines-placed (car lps))
           (scan (cadr lps)))
      (cons lines-placed (graph--pack-tree scan more)))))

(defun graph-pack-tree (rows)
  "Get rid of extra empty space in the ROWS by moving up nodes and horizontal lines as much as possible."
  (graph--pack-tree '((0 0)) rows))

(defun graph--idtree (n tree)
  "Assign ids starting at N to the nodes in TREE."
  (let ((labeled
         (mapcar (lambda (nodes)
                   (let* ((nam (car nodes))
                          (data (if (consp nam) (cdr nam) nil))
                          (nam (if (consp nam) (car nam) nam))
                          (chi (cdr nodes))
                          (nchildren (graph--idtree (+ 1 n) chi))
                          (newn (car nchildren))
                          (children (cdr nchildren))
                          (node (make-graph-treen :text (graph-label-text nam)
                                                  :id n :children children :data data)))
                     (setq n newn)
                     node))
                 tree)))
    (cons n labeled)))

(defun graph-idtree (tree)
  "Assign an id number to each node in a TREE so that it can be flattened later on."
  (cdr (graph--idtree 0 tree)))

(defun graph-tree-row-wid (row)
  "Figrue out the width of a ROW in a tree."
  (+ (--reduce-from (+ acc (graph-width-fn (graph-treen-text it))) 0 row)
     (* (- (length row) 1)) graph-node-padding))

(defun graph-layout-tree (tree)
  "Take a TREE and elegantly arrange it."
  (let* ((rows (graph-make-rows tree))
         (wrapped (graph-wrap-text rows))
         (widths (-map 'graph-tree-row-wid wrapped))
         (total-width (apply 'max widths))
         (top 0)
         (left 0)
         (divider (car (graph-positions (apply-partially '= total-width) widths)))
         (pos (cl-mapcar 'list
                         (cl-mapcar 'graph-row-pos wrapped (number-sequence 0 (length wrapped)))
                         widths))
         (zipped-top (reverse (cl-subseq pos 0 divider)))
         (target-row (car (nth divider pos)))
         (zipped-bottom (cl-subseq pos (+ 1 divider)))
         (spaced-top (graph-space 'graph-parent-p total-width target-row zipped-top))
         (spaced-bottom (graph-space 'graph-child-p total-width target-row zipped-bottom))
         (spaced-pos (append (reverse spaced-top) (list target-row) spaced-bottom))
         (lined (graph-horz-lines spaced-pos))
         (leveled-lines (graph-level-lines lined))
         (packed (graph-pack-tree leveled-lines))
         (lev-chi (graph-lev-children packed)))
    (apply 'append lev-chi)))

;;Functions specific to graphs

(defun graph-get-side (col)
  "Calculate the lenght of a side of a graph.

All graphs currently start as square. Therefore the
length of a side is calculated with the square root."
  (ceiling (sqrt (length col))))

;;Functions specific to binary trees

(defun graph--line-info-btree (top tx bottom bx in-line x)
  "Arrange the given rows."
  (if (and top (or (not bottom) (< (+ tx (graph-btreen-ind (car top)))
                                   (+ bx (graph-btreen-ind (car bottom))))))
      (let ((n (car top)))
        (let-btreen (ind width left right) n
          (append (if left
                      (list (cons 'lbottom nil) (cons 'line (- (+ tx ind) bx 2))
                            (cons 'ltop nil) (cons 'space width) (cons 'nop nil))
                    (list (cons 'space (- (+ tx ind width) x)) (cons 'nop nil)))
                  (graph--line-info-btree (cdr top) (+ tx ind width) bottom bx right (+ tx ind width)))))
    (when bottom
      (let ((n (car bottom)))
        (let-btreen (ind width) n
          (append (if in-line
                      (list (cons 'rtop nil) (cons 'line (- (+ bx ind) tx 2))
                            (cons 'rbottom nil) (cons 'space width) (cons 'nop nil))
                    (list (cons 'space (- (+ bx ind width) x)) (cons 'nop nil)))
                  (graph--line-info-btree top tx (cdr bottom) (+ bx ind width) nil (+ bx ind width))))))))

(defun graph-line-info-btree (top bottom)
  "Take TOP and BOTTOM row of a binary tree and arrange lines between them."
  (graph--line-info-btree top 0 bottom 0 nil 0))

(defun graph-btree-row-wid (row)
  "Calculate the width of ROW in a binary tree."
  (apply '+ (--map (+ (graph-btreen-ind it) (graph-btreen-width it)) row)))

(defun graph-layout-btree (btree)
  "Layout BTREE.

Take a binary tree and convert it into rows.
A row holds all the nodes of a certain level of the tree.
The data for each item in a row is a vector, formatted as [indentation width text left-children? right-children?]"
  (when btree
    (let* ((cur (car btree))
           (lno (graph-layout-btree (cadr btree)))
           (lyes (graph-layout-btree (caddr btree)))
           (wno (apply 'max 0 (-map 'graph-btree-row-wid lno)))
           (wyes (apply 'max 0 (-map 'graph-btree-row-wid lyes)))
           (wid (+ (length (graph-label-text cur)) 4))
           (node-off (if lno
                         (+ (graph-btree-row-wid (car lno)) 2)
                       0))
           (yes-off (max (+ 1 wno) (+ node-off wid 2))))
      (cons (list (make-graph-btreen :ind node-off :width wid :text cur
                                     :left (not (seq-empty-p lno))
                                     :right (not (seq-empty-p lyes))))
            (let ((m (max (length lno) (length lyes))))
              (cl-mapcar (lambda (rno ryes)
                           (if ryes
                               (let ((node (car ryes))
                                     (rest (cdr ryes)))
                                 (let-btreen (ind width text left right) node
                                   (append rno (cons (make-graph-btreen :ind (- (+ ind yes-off) (graph-btree-row-wid rno))
                                                                        :width width :text text :left left :right right)
                                                     rest))))
                             rno))
                         (-take m (apply 'append (-pad nil lno lyes)))
                         (-take m (apply 'append (-pad nil lyes lno)))))))))

(defun graph-sp (&optional n)
  "Return 1 to N spaces in a string."
  (if n
      (graph-fill ?\s n)
    (graph-sp 1)))

(defmacro graph--concat (seq &rest body)
  "Concatenate the elements in SEQ after applying BODY to them."
  (declare (indent 1))
  `(mapconcat (lambda (it) ,@body) ,seq ""))

(defmacro graph--draw-row-line (row left right &rest middle)
  "Draw a line of the given ROW.

For each node use LEFT and RIGHT as borders
and execute MIDDLE for the middle part.
MIDDLE can acces 'ind, 'width, and 'text as variables of the current box."
  `(graph--concat ,row
     (let-btreen (ind width text) it
       (concat (graph-fill ?\s ind) ,left ,@middle ,right))))

(defun graph--draw-btree-nodes (row)
  "Draw the boxes of the given ROW in a binary tree."
  (concat (graph--draw-row-line row "+" "+" (graph-fill ?- (- width 2))) "\n"
          (graph--draw-row-line row "| " " |" (graph-label-text text)) "\n"
          (graph--draw-row-line row "+" "+" (graph-fill ?- (- width 2))) "\n"))

(defun graph--draw-btree-lines (top bottom)
  "Draw the lines between TOP and BOTTOM."
  (let ((li (graph-line-info-btree top bottom)))
                       (concat
                        (graph--concat li
                          (let ((type (car it))
                                (n (cdr it)))
                            (pcase type
                              ('space (graph-sp n))
                              ('lbottom (graph-sp))
                              ('line (graph-fill ?_ n))
                              ('ltop (graph-fill ?/))
                              ('rtop (graph-fill ?\\))
                              ('rbottom (graph-sp))
                              ('nop nil))))
                        "\n"
                        (graph--concat li
                          (let ((type (car it))
                                (n (cdr it)))
                            (pcase type
                              ('space (graph-sp n))
                              ('lbottom (graph-fill ?\/))
                              ('line (graph-sp n))
                              ('ltop (graph-sp))
                              ('rtop (graph-sp))
                              ('rbottom (graph-fill ?\\))
                              ('nop nil))))
                        "\n")))

(defun graph-render-btree (rows)
  "Render ROWS.

ROWS contian the indentation, so this is just about rendering the data to text."
  (let (drawn)
    (while rows
      (let ((row (car rows))
            (next (cadr rows)))
        (setq drawn (concat drawn (graph--draw-btree-nodes row)
                            (when next (graph--draw-btree-lines row next)))))
      (setq rows (cdr rows)))
    drawn))

;;Exported functions for interfacing with this library
(defun graph-tree-to-shapes (tree)
  "Return shapes ready to be rendered with `graph-draw-shapes' for the given TREE."
  (graph-integer-shapes (graph--tree-to-shapes (graph-layout-tree (graph-idtree tree)))))

(defun graph-draw-tree (tree)
  "Render a TREE and return the text."
  (graph-draw-shapes (graph-tree-to-shapes tree)))

(defun graph-draw-binary-tree (btree)
  "Draws a BTREE to the console.

Nodes are in the form [text left right] where 'left' and 'right'
are optional fields containing the children of this node."
  (graph-render-btree (graph-layout-btree btree)))

(provide 'graph)
;;; graph.el ends here

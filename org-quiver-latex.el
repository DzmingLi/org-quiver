;;; org-quiver-latex.el --- Native tikz-cd export -*- lexical-binding: t -*-

;;; Commentary:
;; SPDX-License-Identifier: MIT
;; Arrow mappings follow varkor/quiver's MIT-licensed exporter, pinned and
;; attributed in org-quiver-fletcher.el, which includes its license notice.
;; A conservative v0 exporter for diagrams between vertices.  The document
;; should load quiver.sty (which loads tikz-cd).  Unsupported styles fail
;; explicitly.  No Typst, Node or preview package is needed for LaTeX labels.

;;; Code:
(require 'org-quiver-data)
(require 'color)

(defun org-quiver-latex--colour (hsla)
  "Return an xcolor expression for HSLA, or nil for default black."
  (when hsla
    (unless (and (listp hsla) (<= 3 (length hsla) 4) (seq-every-p #'numberp hsla))
      (user-error "Invalid Quiver colour"))
    (unless (= (or (nth 3 hsla) 1) 1)
      (user-error "The tikz-cd converter does not support translucent colours"))
    (unless (= (nth 2 hsla) 0)
      (apply #'format "{rgb,1:red,%.6f;green,%.6f;blue,%.6f}"
             (color-hsl-to-rgb (/ (nth 0 hsla) 360.0)
                               (/ (nth 1 hsla) 100.0) (/ (nth 2 hsla) 100.0))))))

(defun org-quiver-latex--label (text colour)
  "Wrap math TEXT, with optional COLOUR, safely for a TikZ cell or quote."
  (let ((label (if (string-match-p "\n" text)
                   (concat "\\begin{array}{c}" (string-replace "\n" "\\\\" text) "\\end{array}")
                 text)))
    (when-let* ((colour (org-quiver-latex--colour colour)))
      (setq label (concat "\\textcolor" colour "{" label "}")))
    (concat "{" label "}")))

(defun org-quiver-latex--edge (edge positions)
  "Emit a tikz-cd arrow for EDGE referencing vertex POSITIONS."
  (let* ((source (nth 0 edge)) (target (nth 1 edge))
         (alignment (or (nth 3 edge) 0)) (options (nth 4 edge))
         (style (plist-get options :style))
         (body (or (plist-get (plist-get style :body) :name) "cell"))
         (tail (plist-get style :tail)) (head (plist-get style :head))
         (level (org-quiver--number (or (plist-get (plist-get style :body) :level)
                                       (plist-get options :level)) 1))
         (curve (org-quiver--number (plist-get options :curve) 0))
         (offset (org-quiver--number (plist-get options :offset) 0))
         (position (org-quiver--number (plist-get options :label_position) 50))
         args label-args)
    (unless (and (integerp source) (integerp target)
                 (<= 0 source) (< source (length positions))
                 (<= 0 target) (< target (length positions)))
      (user-error "The tikz-cd converter currently supports only arrows between vertices"))
    (unless (and (integerp alignment) (<= 0 alignment 3))
      (user-error "Invalid Quiver label alignment"))
    (unless (equal (or (plist-get style :name) "arrow") "arrow")
      (user-error "The tikz-cd converter does not yet support corner/adjunction marks"))
    (unless (member level '(1 2))
      (user-error "The tikz-cd converter supports single and double arrows"))
    (when (= level 2)
      (unless (and (equal body "cell")
                   (member (plist-get tail :name) '(nil "none"))
                   (member (plist-get head :name) '(nil "arrowhead" "none")))
        (user-error "Unsupported double-arrow decoration"))
      (push (if (equal (plist-get head :name) "none") "equals" "Rightarrow") args))
    (pcase body
      ("cell" nil)
      ((or "dashed" "dotted" "squiggly") (push body args))
      ("none" (push "no body" args))
      (_ (user-error "Unsupported tikz-cd arrow body: %s" body)))
    (pcase (plist-get tail :name)
      ((or 'nil "none") nil)
      ("maps to" (push "maps to" args))
      ("mono" (push "tail" args))
      ("arrowhead" (push "tail reversed" args))
      ("hook" (push (if (equal (plist-get tail :side) "top") "hook" "hook'") args))
      (_ (user-error "Unsupported tikz-cd arrow tail")))
    (pcase (plist-get head :name)
      ((or 'nil "arrowhead") nil)
      ("none" (unless (= level 2) (push "no head" args)))
      ("epi" (push "two heads" args))
      ("harpoon" (push (if (equal (plist-get head :side) "top") "harpoon" "harpoon'") args))
      (_ (user-error "Unsupported tikz-cd arrow head")))
    (when-let* ((colour (org-quiver-latex--colour (plist-get options :colour))))
      (push (concat "draw=" colour) args))
    (unless (zerop offset)
      (push (format "shift %s=%g" (if (> offset 0) "right" "left") (abs offset)) args))
    ;; quiver.sty's curve key uses signed height rather than bend angles.
    (unless (or (zerop curve) (= source target))
      (push (format "curve={height=%gpt}" (* curve 6)) args))
    (let* ((shorten (plist-get options :shorten))
           (old-length (org-quiver--number (plist-get options :length) 100))
           (start (if shorten (org-quiver--number (plist-get shorten :source) 0)
                    (/ (- 100 old-length) 2.0)))
           (end (if shorten (org-quiver--number (plist-get shorten :target) 0)
                  (/ (- 100 old-length) 2.0))))
      (unless (and (zerop start) (zerop end))
        (push (format "between={%g}{%g}" (/ start 100.0) (- 1 (/ end 100.0))) args)))
    (when (= source target)
      (let* ((radius (org-quiver--number (plist-get options :radius) 3))
             (angle (org-quiver--number (plist-get options :angle) 0))
             (clockwise (if (>= radius 0) 1 -1))
             (base (- 180 (* 90 clockwise) angle))
             (spread (+ 30 (* 2.5 (1- (abs radius))))))
        (push "loop" args)
        (push (format "in=%g" (mod (- base (* spread clockwise)) 360)) args)
        (push (format "out=%g" (mod (+ base (* spread clockwise)) 360)) args)
        (push (format "distance=%gmm" (+ 5 (* 2.5 (1- (abs radius))))) args)))
    (push (concat "from=" (aref positions source)) args)
    (push (concat "to=" (aref positions target)) args)
    (unless (string-empty-p (nth 2 edge))
      (pcase alignment
        (1 (push "description" label-args))
        (3 (push "marking" label-args) (push "allow upside down" label-args)))
      (unless (= position 50) (push (format "pos=%g" (/ position 100.0)) label-args))
      (setq args (append args
                         (list (concat "\"" (org-quiver-latex--label (nth 2 edge) (nth 5 edge)) "\""
                                       (when (= alignment 2) "'")
                                       (when label-args
                                         (concat "{" (string-join (nreverse label-args) ", ") "}")))))))
    (concat "\\arrow[" (string-join (nreverse args) ", ") "]")))

(defun org-quiver-latex (input)
  "Convert Quiver INPUT to a native LaTeX display containing tikz-cd.
Load quiver.sty in the document preamble.  LaTeX labels need no helper;
Typst labels require `org-quiver-convert-labels-function' except simple
single-letter/integer labels shared by both syntaxes."
  (let* ((data (org-quiver-data input 'latex))
         (count (plist-get data :count)) (cells (plist-get data :cells))
         (vertices (seq-take cells count))
         (positions (make-vector count nil))
         (grid (make-hash-table :test #'equal))
         rows arrows)
    (dolist (vertex vertices)
      (unless (and (integerp (nth 0 vertex)) (integerp (nth 1 vertex)))
        (user-error "Invalid Quiver vertex position")))
    (let* ((xs (mapcar #'car vertices)) (ys (mapcar #'cadr vertices))
           (min-x (if xs (apply #'min xs) 0)) (max-x (if xs (apply #'max xs) 0))
           (min-y (if ys (apply #'min ys) 0)) (max-y (if ys (apply #'max ys) 0)))
      (when (> (* (1+ (- max-x min-x)) (1+ (- max-y min-y))) 10000)
        (user-error "Quiver matrix is too sparse to export as tikz-cd"))
      (cl-loop for vertex in vertices for index from 0 do
               (let ((key (cons (car vertex) (cadr vertex))))
                 (when (gethash key grid) (user-error "Overlapping Quiver vertices"))
                 (puthash key (org-quiver-latex--label (nth 2 vertex) (nth 3 vertex)) grid)
                 (aset positions index (format "%d-%d" (1+ (- (cadr vertex) min-y))
                                               (1+ (- (car vertex) min-x))))))
      (cl-loop for y from min-y to max-y do
               (push (string-join
                      (cl-loop for x from min-x to max-x collect (gethash (cons x y) grid "")) " & ") rows)))
    (dolist (edge (nthcdr count cells))
      (push (org-quiver-latex--edge edge positions) arrows))
    (concat "\n% " (plist-get data :url) "\n\\begingroup\n" (plist-get data :macros)
            "\n\\[\n\\begin{tikzcd}\n" (string-join (nreverse rows) " \\\\\n")
            "\n" (string-join (nreverse arrows) "\n") "\n\\end{tikzcd}\n\\]\n\\endgroup\n")))

(defun org-quiver-export-latex (path _description _backend _info)
  "Org export callback producing native LaTeX for PATH."
  (org-quiver-latex path))

(provide 'org-quiver-latex)
;;; org-quiver-latex.el ends here

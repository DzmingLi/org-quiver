;;; org-quiver-fletcher.el --- Quiver to Fletcher in Elisp -*- lexical-binding: t -*-

;; Port of QuiverExport.fletcher and the v0 data format from varkor/quiver,
;; commit 2f289ecbae9b7e5a473e04b924750c538ed5c4cf (Fletcher 0.5.8).
;; https://github.com/varkor/quiver/blob/2f289ecbae9b7e5a473e04b924750c538ed5c4cf/src/quiver.mjs
;;
;; MIT License
;; Copyright (c) 2018 varkor
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Typst labels pass through, other syntax uses the optional label translator.
;; Unsupported upstream features signal errors, not partial output.

;;; Code:
(require 'org-quiver-data)

(defun org-quiver--colour (colour)
  "Return Fletcher colour for HSLA COLOUR; nil means inherited black."
  (when colour
    (unless (and (listp colour) (<= 3 (length colour) 4)
                 (seq-every-p #'numberp colour))
      (user-error "Invalid Quiver colour"))
    (unless (and (= (nth 2 colour) 0) (= (or (nth 3 colour) 1) 1))
      ;; Preserve the upstream exporter's rounding and treatment of alpha.
      (format "color.hsl(%gdeg, %d, %d)" (nth 0 colour)
              (floor (+ 0.5 (* (nth 1 colour) 2.55)))
              (floor (+ 0.5 (* (nth 2 colour) 2.55)))))))

(defun org-quiver--label (label colour renderer)
  "Format LABEL with COLOUR, in RENDERER syntax."
  (setq label (or label ""))
  (unless (stringp label) (user-error "Expected a Quiver label string"))
  (when (and (not (equal renderer "typst")) (string-match-p "\\\\[[:alpha:]]" label))
    (user-error "Fletcher uses Typst labels; select Quiver's Typst renderer and translate LaTeX labels"))
  (if (string-empty-p label) ""
    (concat (when-let* ((colour (org-quiver--colour colour)))
              (format "text(%s)" colour))
            "[$" label "$]")))

(defun org-quiver--mark (name table)
  "Look up arrow NAME in TABLE; reject unsupported styles."
  (or (cdr (assoc name table))
      (user-error "Fletcher does not support arrow mark %s" name)))

(defun org-quiver--style (options)
  "Return (MARKS . EXTRA-ARGUMENTS) for Quiver edge OPTIONS."
  (let* ((style (plist-get options :style))
         (tail (plist-get style :tail))
         (body (plist-get style :body))
         (head (plist-get style :head))
         (name (or (plist-get style :name) "arrow"))
         (level (org-quiver--number (or (plist-get body :level)
                                        (plist-get options :level)) 1))
         extra)
    (unless (equal name "arrow")
      (user-error "Fletcher does not support adjunction or pullback/pushout corner marks"))
    (unless (and (integerp level) (<= 1 level 4))
      (user-error "Expected Quiver arrow multiplicity 1–4"))
    (cons
     (concat
      (if (equal (plist-get tail :name) "hook")
          (if (equal (plist-get tail :side) "top") "hook" "hook'")
        (org-quiver--mark (or (plist-get tail :name) "none")
                          '(("none" . "") ("maps to" . "|") ("mono" . ">") ("arrowhead" . "<"))))
      (if (equal (or (plist-get body :name) "cell") "cell")
          (if (> level 3)
              (progn (setq extra '("extrude: (-6,-2,2,6)" "mark-scale: 2")) "-")
            (if (= level 1) "-" (make-string (1- level) ?=)))
        (org-quiver--mark (plist-get body :name)
                          '(("dashed" . "--") ("dotted" . "..") ("squiggly" . "~")
                            ("barred" . "-|-") ("double barred" . "-||-")
                            ("bullet solid" . "-@-") ("bullet hollow" . "-O-") ("none" . " "))))
      (if (equal (plist-get head :name) "harpoon")
          (if (equal (plist-get head :side) "top") "harpoon" "harpoon'")
        (org-quiver--mark (or (plist-get head :name) "arrowhead")
                          '(("none" . "") ("arrowhead" . ">") ("epi" . ">>") ("multimap" . "O")))))
     extra)))

(defun org-quiver--edge (edge positions renderer)
  "Return Fletcher for EDGE using POSITIONS and RENDERER."
  (let* ((source (nth 0 edge)) (target (nth 1 edge))
         (alignment (or (nth 3 edge) 0)) (options (nth 4 edge))
         (label (org-quiver--label (nth 2 edge) (nth 5 edge) renderer))
         (curve (org-quiver--number (plist-get options :curve) 0))
         (offset (org-quiver--number (plist-get options :offset) 0))
         (shorten (plist-get options :shorten))
         (old-length (plist-get options :length))
         (marks (org-quiver--style options))
         args)
    (unless (and (integerp source) (integerp target)
                 (<= 0 source) (< source (length positions))
                 (<= 0 target) (< target (length positions)))
      (user-error "Fletcher does not support arrows between arrows, or the link has invalid endpoints"))
    (unless (and (integerp alignment) (<= 0 alignment 3))
      (user-error "Invalid Quiver label alignment"))
    (unless (string-empty-p label)
      (push label args)
      (when (= alignment 3)
        (push "label-fill: false" args) (push "label-angle: right" args))
      (push (concat "label-side: " (nth alignment '("left" "center" "right" "center"))) args)
      (let ((position (org-quiver--number (plist-get options :label_position) 50)))
        (unless (= position 50) (push (format "label-pos: %g" (/ position 100.0)) args))))
    (unless (and (zerop (org-quiver--number (plist-get shorten :source) 0))
                 (zerop (org-quiver--number (plist-get shorten :target) 0))
                 (or shorten (null old-length) (= old-length 100)))
      (user-error "Fletcher does not support shortened arrows"))
    (unless (zerop offset)
      (unless (and (/= source target) (zerop curve))
        (user-error "Fletcher does not support offset curves or loops"))
      (push (format "shift: %g" (/ (- offset) 20.0)) args))
    (unless (equal (car marks) "-")
      (push (format "\"%s\"" (car marks)) args)
      (dolist (arg (cdr marks)) (push arg args)))
    (when-let* ((colour (org-quiver--colour (plist-get options :colour))))
      (push (concat "stroke: " colour) args))
    (cond
     ((= source target)
      (let ((radius (org-quiver--number (plist-get options :radius) 3))
            (angle (org-quiver--number (plist-get options :angle) 0)))
        (push (format "bend: %gdeg" (* (cond ((> radius 0) 1) ((< radius 0) -1) (t 0))
                                         (+ 130 (* (1- (abs radius)) 5)))) args)
        (push (format "loop-angle: %gdeg" (- 90 angle)) args)))
     ((not (zerop curve)) (push (format "bend: %gdeg" (* (- curve) 18)) args)))
    (format "\tedge(%s, %s%s)\n" (aref positions source) (aref positions target)
            (if args (concat ", " (string-join (nreverse args) ", ")) ""))))

(defun org-quiver-fletcher (input)
  "Convert Quiver INPUT to complete Fletcher source in pure Elisp.
Supports the v0 data format and the upstream Fletcher feature set.
Typst labels pass through; other labels use the optional label translator.
The result includes its original URL and the pinned Fletcher import."
  (let* ((data (org-quiver-data input 'typst))
         (url (plist-get data :url))
         (renderer "typst")
         (count (plist-get data :count))
         (cells (plist-get data :cells))
         positions lines)
    (setq positions (make-vector count nil))
    (dotimes (index count)
      (let* ((vertex (nth index cells)) (x (nth 0 vertex)) (y (nth 1 vertex))
             (label (org-quiver--label (nth 2 vertex) (nth 3 vertex) renderer)))
        (unless (and (integerp x) (integerp y)) (user-error "Invalid Quiver vertex position"))
        (aset positions index (format "(%d, %d)" x y))
        (push (format "\tnode(%s%s)\n" (aref positions index)
                      (if (string-empty-p label) "" (concat ", " label))) lines)))
    (dolist (edge (nthcdr count cells))
      (push (org-quiver--edge edge positions renderer) lines))
    (concat "#import \"@preview/fletcher:0.5.8\": diagram, node, edge\n"
            (plist-get data :macros)
            "\n// " url "\n#diagram({\n" (apply #'concat (nreverse lines)) "})")))

(defun org-quiver-export-fletcher (path _description _backend _info)
  "Org export callback emitting Fletcher for PATH."
  (org-quiver-fletcher path))

(provide 'org-quiver-fletcher)
;;; org-quiver-fletcher.el ends here

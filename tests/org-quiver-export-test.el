;;; org-quiver-export-test.el --- Standalone native converter tests -*- lexical-binding: t -*-
(require 'ert)
(require 'org-quiver-fletcher)
(require 'org-quiver-latex)
(require 'ox-latex)

(defun org-quiver-test--encode (diagram &optional renderer)
  (concat "https://q.uiver.app/#r=" (or renderer "typst") "&q="
          (base64-encode-string (encode-coding-string (json-serialize diagram) 'utf-8) t)))

(ert-deftest org-quiver-native-exporters-keep-backend-syntax ()
  (let* ((url (org-quiver-test--encode [0 2 [0 0 "A"] [1 0 "B"] [0 1 "f"]]))
         (link (concat "[[quiver:" url "]]"))
         (latex (org-export-string-as link 'latex t)))
    (should (string-match-p (regexp-quote "\\begin{tikzcd}") latex))
    (should-not (string-match-p "#diagram\\|<svg\\|includegraphics" latex))
    (when (require 'ox-typst nil t)
      (let ((typst (org-export-string-as link 'typst t)))
        (should (string-match-p "#diagram(" typst))
        (should (string-match-p "@preview/fletcher:0.5.8" typst))
        (should-not (string-match-p (regexp-quote "\\begin{tikzcd}") typst))))))

(ert-deftest org-quiver-native-latex-preserves-labels-and-grid ()
  (let ((source (org-quiver-latex
                 (org-quiver-test--encode
                  [0 3 [-1 -2 "\\alpha"] [1 -2 "B"] [-1 -1 "C"]
                     [0 1 "\\phi" 2 (:curve 2 :offset -1 :label_position 20)]
                     [0 2 "g" 1 (:style (:head (:name "none")))]] "katex"))))
    (dolist (text '("{\\alpha} &  & {B} \\\\" "from=1-1, to=1-3"
                    "curve={height=12pt}" "shift left=1" "pos=0.2" "no head" "description"))
      (should (string-match-p (regexp-quote text) source)))))

(ert-deftest org-quiver-fletcher-exports-styles-and-colours ()
  (let ((source (org-quiver-fletcher
                 (org-quiver-test--encode
                  [0 2 [0 0 "alpha" [0 100 50]] [1 1 "B"]
                     [0 1 "f" 3 (:offset 2 :style (:tail (:name "maps to") :head (:name "epi")))] ]))))
    (dolist (text '("text(color.hsl(0deg, 255, 127))[$alpha$]" "label-fill: false"
                    "label-angle: right" "label-side: center" "shift: -0.1" "\"|->>\""))
      (should (string-match-p (regexp-quote text) source)))))

(ert-deftest org-quiver-mixed-label-syntax-has-an-independent-translator ()
  (let ((url (org-quiver-test--encode [0 1 [0 0 "alpha"]])))
    (let ((org-quiver-convert-labels-function nil))
      (should-error (org-quiver-latex url) :type 'user-error))
    (let ((org-quiver-convert-labels-function
           (lambda (labels from to macros)
             (should (equal labels '("alpha")))
             (should (eq from 'typst)) (should (eq to 'latex))
             (should (equal macros ""))
             '("\\alpha"))))
      (should (string-match-p (regexp-quote "{\\alpha}") (org-quiver-latex url))))))

(ert-deftest org-quiver-converters-reject-invalid-and-unsupported-data ()
  (dolist (converter '(org-quiver-fletcher org-quiver-latex))
    (dolist (diagram '([1 0] [0 2] [0 1 [0 0 "A"] [0 9 "f"]]
                       [0 2 [0 0 "A"] [1 0 "B"] [0 1 "" 0 (:style (:name "corner"))]]))
      (should-error (funcall converter (org-quiver-test--encode diagram)) :type 'user-error)))
  (should-error (org-quiver-fletcher
                 (org-quiver-test--encode [0 1 [0 0 "A"] [0 0 "" 0 (:shorten (:source 10))]]))
                :type 'user-error))

(provide 'org-quiver-export-test)

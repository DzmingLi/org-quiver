;;; org-quiver-test.el --- Standalone link regressions -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'org-quiver)
(require 'ox-latex)

(defconst org-quiver-test--payload "WzAsMixbMCwwLCJBIl0sWzEsMCwiQiJdLFswLDFdXQ==")
(defconst org-quiver-test--url
  (concat "https://q.uiver.app/#q=" org-quiver-test--payload))

(defvar org-quiver-test--opened nil)

(ert-deftest org-quiver-preserves-full-urls-and-accepts-bare-payloads ()
  (dolist (input (list org-quiver-test--payload org-quiver-test--url
                       (concat "quiver:" org-quiver-test--payload)
                       (concat " \n" org-quiver-test--url "\n")))
    (should (equal (org-quiver-url input) org-quiver-test--url)))
  (let ((url (concat org-quiver-test--url "&macro_url=https%3A%2F%2Fexample.org%2Fm.tex")))
    (should (equal (org-quiver-url url) url)))
  (let ((url (concat "https://q.uiver.app/#q="
                     (url-hexify-string org-quiver-test--payload))))
    (should (equal (org-quiver-url url) url)))
  (let ((org-quiver-base-url "http://localhost:8080/"))
    (should (equal (org-quiver-url org-quiver-test--payload)
                   (concat org-quiver-base-url "#q=" org-quiver-test--payload)))))

(ert-deftest org-quiver-rejects-missing-data-and-non-editor-urls ()
  (dolist (input '(nil "" " " "not a diagram" "https://q.uiver.app/"
                    "https://q.uiver.app/#q=" "https://q.uiver.app/#q=abc&q=def"
                    "https://q.uiver.app/#q=abc%20def"
                    "https://q.uiver.app/#q=abc&x=[oops]"
                    "https://example.org/#q=abc" "javascript:alert(1)"))
    (should-error (org-quiver-url input) :type 'user-error)))

(ert-deftest org-quiver-org-follow-uses-the-existing-browser ()
  (let ((browse-url-browser-function
         (lambda (url &rest _) (setq org-quiver-test--opened url)))
        (org-quiver-browse-function #'browse-url))
    (with-temp-buffer
      (org-mode)
      (org-quiver-insert org-quiver-test--url "Diagram")
      (goto-char (point-min))
      (org-open-at-point)
      (should (equal org-quiver-test--opened org-quiver-test--url)))))

(ert-deftest org-quiver-insert-opens-a-blank-typst-diagram-without-prompts ()
  (let ((org-quiver-default-renderer "typst")
        (org-quiver-browse-function (lambda (url) (setq org-quiver-test--opened url))))
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Unexpected prompt")))
                ((symbol-function 'gui-get-selection) (lambda (&rest _) (ert-fail "Clipboard access"))))
        (call-interactively #'org-quiver-insert))
      (should (equal org-quiver-test--opened "https://q.uiver.app/#r=typst&q=WzAsMF0="))
      (should (equal (buffer-string) (concat "[[quiver:" org-quiver-test--opened "]]"))))))

(ert-deftest org-quiver-insert-rejects-invalid-input-before-opening ()
  (with-temp-buffer
    (org-mode)
    (let ((org-quiver-browse-function (lambda (&rest _) (ert-fail "Browser opened"))))
      (should-error (org-quiver-insert "not a URL") :type 'user-error)
      (should (string-empty-p (buffer-string))))))

(ert-deftest org-quiver-export-dispatches-through-org ()
  (let ((org-quiver-latex-export-function
         (lambda (url description backend _info)
           (should (equal url org-quiver-test--url))
           (should (equal description "Diagram"))
           (should (eq backend 'latex))
           "\\begin{tikzcd} A \\end{tikzcd}")))
    (should (string-match-p
             (regexp-quote "\\begin{tikzcd} A \\end{tikzcd}")
             (org-export-string-as
              (concat "[[quiver:" org-quiver-test--payload "][Diagram]]")
              'latex t)))))

(ert-deftest org-quiver-export-supports-derived-backends-and-missing-renderers ()
  (let ((org-export-registered-backends org-export-registered-backends))
    (org-export-define-derived-backend 'org-quiver-test-latex 'latex)
    ;; Simulate a loaded Typst backend when running with only built-in Org.
    ;; Keep the registration local, and never replace an existing ox-typst.
    (unless (org-export-get-backend 'typst)
      (org-export-define-backend 'typst nil))
    (org-export-define-derived-backend 'org-quiver-test-typst 'typst)
    (let ((org-quiver-latex-export-function (lambda (&rest _) "tikz-cd"))
          (org-quiver-typst-export-function (lambda (&rest _) "Fletcher")))
      (should (equal (org-quiver-export org-quiver-test--payload nil
                                       'org-quiver-test-latex nil) "tikz-cd"))
      (should (equal (org-quiver-export org-quiver-test--payload nil
                                       'org-quiver-test-typst nil) "Fletcher"))))
  (let ((org-quiver-latex-export-function nil))
    (should-error (org-quiver-export org-quiver-test--payload nil 'latex nil)
                  :type 'user-error)))

(ert-deftest org-quiver-preview-is-optional-and-renderer-agnostic ()
  (with-temp-buffer
    (org-mode)
    (insert "[[quiver:WzAsMF0=]]")
    (let ((overlay (make-overlay (point-min) (point-max)))
          (org-quiver-preview-function nil))
      (should-not (org-quiver-preview overlay "not-even-decoded" nil))
      (let ((org-quiver-preview-function
             (lambda (ov url link)
               (should (eq ov overlay))
               (should (equal url "https://q.uiver.app/#q=WzAsMF0="))
               (should (eq link 'element))
               (overlay-put ov 'display "External LaTeX preview")
               t)))
        (should (org-quiver-preview overlay "WzAsMF0=" 'element))
        (should (equal (overlay-get overlay 'display) "External LaTeX preview"))))))

(ert-deftest org-quiver-default-insert-can-use-latex ()
  (with-temp-buffer
    (org-mode)
    (let ((org-quiver-default-renderer "katex")
          (org-quiver-browse-function #'ignore))
      (org-quiver-insert)
      (should (string-match-p "r=katex" (buffer-string))))))

(provide 'org-quiver-test)
;;; org-quiver-test.el ends here

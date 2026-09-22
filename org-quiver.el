;;; org-quiver.el --- Edit Quiver diagrams through Org links -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30.2") (org "9.8"))
;; Version: 0.1.0
;; Author: Dzming Li <i@dzming.li>
;; URL: https://github.com/DzmingLi/org-quiver
;; Keywords: outlines, tex, tools
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Keep diagram data in quiver: links.  `org-quiver-insert' inserts a blank
;; diagram and opens the browser.  Follow and edit the link through Org's
;; ordinary commands.  Export diagrams as tikz-cd or Fletcher.

;;; Code:

(require 'org)
(require 'browse-url)
(require 'subr-x)
(require 'seq)
(require 'url-util)

(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(autoload 'org-quiver-fletcher "org-quiver-fletcher")
(autoload 'org-quiver-export-fletcher "org-quiver-fletcher")
(autoload 'org-quiver-export-latex "org-quiver-latex")

(defgroup org-quiver nil
  "Quiver diagram links in Org."
  :group 'org-link)

(defcustom org-quiver-base-url "https://q.uiver.app/"
  "Editor URL, without a fragment, used to open bare payloads.
Full URLs from this editor and https://q.uiver.app/ are accepted."
  :type 'string)

(defcustom org-quiver-preview-function nil
  "Optional function that previews a Quiver link.
Called with (OVERLAY URL LINK) in the Org document buffer.  URL is the
complete Quiver URL; LINK is the Org element.  OVERLAY belongs to Org:
the provider sets its display and returns non-nil if it accepts the
request, including asynchronous work.  Return nil to leave plain text.
Async providers must ignore results after OVERLAY is deleted or changed.
Nil disables previews.  No renderer or preview package is loaded by default.
This option may be set buffer-locally to select a document's provider."
  :type '(choice (const :tag "No preview" nil) function))

(defcustom org-quiver-default-renderer "katex"
  "Formula syntax for labels in new blank Quiver diagrams.
\"katex\" selects LaTeX label syntax, for example \\alpha.
\"typst\" selects Typst label syntax, for example alpha.

Used only when `org-quiver-insert' creates a blank diagram without an
explicit URL or payload.  It sets the editor URL's r parameter; existing
links and explicitly supplied diagrams keep their own settings.

This selects the label syntax in the Quiver editor, not the Org export
backend.  LaTeX export uses tikz-cd and Typst export uses Fletcher,
regardless of this option.  Changing it does not translate existing labels."
  :type '(choice (const :tag "LaTeX labels (KaTeX)" "katex")
                 (const :tag "Typst labels" "typst")))

(defun org-quiver--payload-p (text)
  "Whether TEXT has the syntax of an opaque Quiver base64 payload.
Do not interpret or rewrite Quiver's versioned diagram representation."
  (string-match-p "\\`[A-Za-z0-9+/_-]+=\\{0,2\\}\\'"
                  text))

(defun org-quiver--path (input)
  "Validate INPUT and return an Org link path without the quiver: prefix.
Accept a complete Save URL, a bare payload or a quiver: target.  Full
URLs retain all editor options.  Validation checks transport syntax,
not the internal diagram schema."
  (unless (stringp input)
    (user-error "Expected a Quiver URL or payload"))
  (let ((path (string-remove-prefix "quiver:" (string-trim input))))
    (cond
     ((org-quiver--payload-p path) path)
     ((and (not (string-match-p (rx (or space "[" "]" "\\")) path))
           (string-match "\\`\\(https?://[^#]+\\)#\\(.*\\)\\'" path))
      (let* ((editor (match-string 1 path))
             (fragment (match-string 2 path))
             (fields (split-string fragment "&"))
             (data (seq-filter (lambda (field) (string-prefix-p "q=" field))
                               fields)))
        (unless (and (member editor (list org-quiver-base-url
                                         "https://q.uiver.app/"))
                     (= (length data) 1)
                     (org-quiver--payload-p
                      (url-unhex-string (substring (car data) 2))))
          (user-error "Expected a Quiver Save URL with one nonempty #q= payload"))
        path))
     (t (user-error "Expected a Quiver Save URL or base64 payload")))))

(defun org-quiver-url (input)
  "Return the complete editor URL for INPUT."
  (let ((path (org-quiver--path input)))
    (if (string-match-p "\\`https?://" path) path
      (concat org-quiver-base-url "#q=" path))))

(defun org-quiver-follow (path _argument)
  "Open Quiver PATH using Emacs browser configuration."
  (browse-url (org-quiver-url path)))

(defun org-quiver-preview (overlay path link)
  "Delegate OVERLAY, PATH and LINK to the optional preview provider."
  (when org-quiver-preview-function
    (funcall org-quiver-preview-function overlay (org-quiver-url path) link)))

;;;###autoload
(defun org-quiver-insert (&optional input description)
  "Insert a diagram link at point and open it in the browser.
Interactively create a blank diagram without prompting.  Lisp callers
may supply INPUT (a URL or payload) and DESCRIPTION.  Edit the resulting
link with ordinary Org commands; this command does not read the clipboard."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Insert Quiver links in an Org buffer"))
  (let ((path (org-quiver--path
               (or input (concat org-quiver-base-url "#r="
                                 org-quiver-default-renderer "&q=WzAsMF0=")))))
    (insert (org-link-make-string (concat "quiver:" path)
                                  description))
    (org-quiver-follow path nil)))

(defun org-quiver-export (path description backend info)
  "Render PATH and DESCRIPTION for BACKEND with export context INFO.
LaTeX and derived backends use tikz-cd; Typst and derived backends use
Fletcher.  Signal an error for unsupported backends."
  (require 'ox)
  (let ((url (org-quiver-url path)))
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (org-quiver-export-latex url description backend info))
     ((org-export-derived-backend-p backend 'typst)
      (org-quiver-export-fletcher url description backend info))
     (t (user-error "Quiver export does not support backend %s" backend)))))

(org-link-set-parameters "quiver"
                         :follow #'org-quiver-follow
                         :export #'org-quiver-export
                         :preview #'org-quiver-preview)

(provide 'org-quiver)
;;; org-quiver.el ends here

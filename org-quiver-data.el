;;; org-quiver-data.el --- Decode Quiver links -*- lexical-binding: t -*-

;;; Commentary:
;; Shared data reader; no browser, rendering process or preview dependency.
;; SPDX-License-Identifier: MIT

;;; Code:
(require 'org-quiver)
(require 'json)
(require 'cl-lib)

(defcustom org-quiver-convert-labels-function nil
  "Optional translator called as (LABELS FROM TO MACROS).
LABELS is a list of math strings, FROM and TO are `latex' or `typst',
and MACROS is the source editor's inline definitions string.
Return equally many translated strings, or signal an error.  Called in
the document buffer only when source and export label syntax differ.
Nil permits only empty labels and single ASCII letters or integers across
syntaxes; other mixed-syntax exports fail rather than misinterpret labels."
  :type '(choice (const :tag "No translator" nil) function)
  :group 'org-quiver)

(defun org-quiver--parameters (url)
  "Read fragment parameters from URL without interpreting + as a space."
  (mapcar (lambda (field)
            (let ((index (string-search "=" field)))
              (cons (if index (substring field 0 index) field)
                    (decode-coding-string
                     (url-unhex-string (if index (substring field (1+ index)) "")) 'utf-8))))
          (split-string (substring url (1+ (string-search "#" url))) "&" t)))

(defun org-quiver--number (value default)
  "Return numeric VALUE, or DEFAULT when omitted."
  (cond ((null value) default) ((numberp value) value)
        (t (user-error "Expected a numeric Quiver option"))))

(defun org-quiver-data (input &optional target)
  "Decode INPUT into a plist with :url, :params, :count and :cells.
Optionally translate labels to TARGET (`latex' or `typst') using
`org-quiver-convert-labels-function'.  The original URL is never changed."
  (let* ((url (org-quiver-url input))
         (params (org-quiver--parameters url))
         (payload (cdr (assoc "q" params)))
         (data (condition-case nil
                   (json-parse-string
                    (decode-coding-string
                     (base64-decode-string
                      (string-replace "_" "/" (string-replace "-" "+" payload))) 'utf-8)
                    :array-type 'list :object-type 'plist :null-object nil)
                 (error (user-error "Invalid Quiver base64/JSON data")))))
    (unless (and (listp data) (eql (car data) 0) (integerp (nth 1 data))
                 (<= 0 (nth 1 data) (length (nthcdr 2 data))))
      (user-error "Expected Quiver v0 data and a valid vertex count"))
    (when (assoc "macro_url" params)
      (user-error "Remote macro URLs are not fetched; use document headers"))
    (let* ((count (nth 1 data)) (cells (nthcdr 2 data))
           (from (if (equal (cdr (assoc "r" params)) "typst") 'typst 'latex))
           (macros (or (cdr (assoc "macros" params)) ""))
           (labels (mapcar (lambda (cell)
                             (unless (and (listp cell) (>= (length cell) 2)
                                          (or (null (nth 2 cell)) (stringp (nth 2 cell))))
                               (user-error "Invalid Quiver cell"))
                             (or (nth 2 cell) "")) cells)))
      (when (and target (not (eq from target)))
        (setq labels
              (if org-quiver-convert-labels-function
                  (funcall org-quiver-convert-labels-function labels from target macros)
                (if (and (string-empty-p macros)
                         (seq-every-p (lambda (s) (string-match-p "\\`\\(?:[A-Za-z]\\|[0-9]+\\)?\\'" s)) labels))
                    labels
                  (user-error "Quiver %s labels need a %s translator (org-quiver-convert-labels-function)"
                              from target))))
        (unless (and (listp labels) (= (length labels) (length cells))
                     (seq-every-p #'stringp labels))
          (error "Quiver label translator returned invalid labels")))
      (setq cells (cl-mapcar (lambda (cell label)
                              (append (seq-take cell 2) (list label) (nthcdr 3 cell))) cells labels))
      (list :url url :params params :count count :cells cells :syntax (or target from)
            :macros (if (or (null target) (eq from target)) macros "")))))

(provide 'org-quiver-data)
;;; org-quiver-data.el ends here

;;; org-headline-cmpl-consult.el --- Org headline completion with consult -*- lexical-binding: t -*-
;; This package is serves no purpose org-headline-cmpl
;;
;; Example configuration:
;; (setq org-headline-cmpl-completing-read-fn
;;       'org-headline-cmpl-consult-completing-read)

(require 'org-headline-cmpl)
(require 'consult)

(defcustom org-headline-cmpl-consult-sources
  '(org-headline-cmpl-consult--source-headline
    org-headline-cmpl-consult--source-todo
    org-headline-cmpl-consult--source-top-level)
  "Sources used by `org-headline-cmpl-consult-completing-read'."
  :type '(repeat symbol))

(defvar org-headline-cmpl-consult--hash-map (make-hash-table)
  "Local variable used to generate candidates for
`org-headline-cmpl-consult-sources'")

(defun org-headline-cmpl--query (filter)
  "Return a list of string candidates which fn FILTER is non-nil.
The FILTER fn accepts one headline components argument.
See `org-headline-cmpl--headline-components' for components structure."
  (seq-filter (lambda (key)
                (funcall filter
                         (gethash key
                                  org-headline-cmpl-consult--hash-map)))
              (hash-table-keys
               org-headline-cmpl-consult--hash-map)))

(defvar org-headline-cmpl-consult--source-headline
  `(:name     "Headline"
    :narrow   ?h
    :category org-headline-cmpl
    :state    ,#'org-headline-cmpl-consult--headline-state
    :default  t
    :items
    ,(lambda () (org-headline-cmpl--query (lambda (_) t))))
  "Headline candidate source for `org-headline-cmpl-consult-completing-read'.")

(defvar org-headline-cmpl-consult--source-todo
  `(:name     "Todos"
    :narrow   ?t
    :category org-headline-cmpl
    :state    ,#'org-headline-cmpl-consult--headline-state
    :default  nil
    :hidden t
    :items
    ,(lambda () (org-headline-cmpl--query
                 (lambda (cand) (alist-get :todo cand)))))
    "Todo headline candidate source for
`org-headline-cmpl-consult-completing-read'.")

(defvar org-headline-cmpl-consult--source-top-level
  `(:name     "Top level"
    :narrow   ?l
    :category org-headline-cmpl
    :state    ,#'org-headline-cmpl-consult--headline-state
    :default  nil
    :hidden t
    :items
    ,(lambda () (org-headline-cmpl--query
                 (lambda (cand) (eq (alist-get :level cand) 1)))))
    "Top level headline candidate source for
`org-headline-cmpl-consult-completing-read'.")

(defun org-headline-cmpl-consult--position (headline &optional find-file)
  "Return the position marker for HEADLINE
See `org-headline-cmpl--headline-components' for HEADLINE structure.
FIND-FILE is the file open function, defaulting to `find-file'."
  (when headline
    (org-headline-cmpl--file-link-to-marker
     headline
     nil
     (funcall (or find-file #'find-file) (alist-get :file headline)))))

(defun org-headline-cmpl-consult--after-jump ()
  "Hook to show headline subtree and narrow to subtree, if
`org-headline-cmpl-narrow-after-goto' is non-nil."
  (org-show-subtree)
  (when org-headline-cmpl-narrow-after-goto
    (org-narrow-to-subtree)))

(defun org-headline-cmpl-consult--restore-buffer (buffer-reset-info)
  "Restor buffer after preview."
  (when buffer-reset-info
    (with-current-buffer (alist-get :buffer buffer-reset-info)
      (widen)
      (when-let ((region (alist-get :region buffer-reset-info)))
        (apply #'narrow-to-region region))
      (goto-char (alist-get :position buffer-reset-info)))))

(defun org-headline-cmpl-consult--headline-state ()
  "Headline state function."
  (let ((reset-buffer (current-buffer))
        (buffers-to-kill)
        (buffers-reset-info))
    (lambda (state cand)
      (cond ((and (eq state 'preview)
                  cand)
             ;; FIX: Should handle when candidate is a non-match
             (let* ((headline (gethash cand org-headline-cmpl-consult--hash-map))
                    (not-opened (null (find-buffer-visiting (alist-get :file headline))))
                    (buffer (find-file-noselect (alist-get :file headline))))
               ;; Preview cand
               (switch-to-buffer buffer)
               (if not-opened
                   (push buffer buffers-to-kill)
                 (unless (or (seq-some (lambda (buffer-reset-info)
                                         (eq buffer
                                             (alist-get :buffer buffer-reset-info)))
                                       buffers-reset-info)
                             (member buffer buffers-to-kill))
                   (push `((:buffer . ,buffer)
                           (:region . ,(when (org-buffer-narrowed-p)
                                         (cons (point-min) (point-max))))
                           (:position . ,(point)))
                         buffers-reset-info)))
               (widen)
               (goto-char (marker-position
                           (org-headline-cmpl--file-link-to-marker headline)))
               (when org-headline-cmpl-narrow-after-goto
                 (org-narrow-to-subtree))
               (org-show-subtree)))
            ((and (eq state 'preview)
                  (null cand))
             (progn 
               (switch-to-buffer reset-buffer)
               (org-headline-cmpl-consult--restore-buffer
                (seq-find (lambda (buffer-reset-info)
                            (eq reset-buffer
                                (alist-get :buffer buffer-reset-info)))
                          buffers-reset-info))))
            ((eq state 'exit)
             (seq-do #'kill-buffer buffers-to-kill)
             (seq-do #'org-headline-cmpl-consult--restore-buffer buffers-reset-info)
             (switch-to-buffer reset-buffer))))))

;; FIX: I am to lazy to figure out why this is happening
(defun org-headline-cmpl-consult--annotate (table)
  "Create annotatio function for `org-headline-cmpl-consult-completing-read'
compleations. TABLE consist of the hash-table.
See `org-headline-cmpl--completion-table'"
  (let ((annotate-fn (org-headline-cmpl--annotate table)))
    (lambda (candidate)
      (funcall annotate-fn (apply #'concat (butlast (split-string candidate "") 2))))))

(defun org-headline-cmpl-consult-completing-read (prompt
                                                  &optional
                                                  hash-table
                                                  require-match)
  "Consult multi source replacment for `org-headline-cmpl-completing-read'
HASH-TABLE is used when the total set of precomputet headlines candidates is not
preferable. See `org-headline-cmpl-consult-completing-read' how HASH-TABLE is
constructed.
For docs on the rest of the arguments see `completing-read'"
  (setq org-headline-cmpl-consult--hash-map
        (or hash-table (org-headline-cmpl--completion-table)))
  (puthash "test" nil org-headline-cmpl-consult--hash-map)
  (let* (;(consult-after-jump-hook (append consult-after-jump-hook
         ;                                 '(org-headline-cmpl-consult--after-jump)))
         (match (consult--multi org-headline-cmpl-consult-sources
                                :require-match require-match
                                :prompt prompt
                                :annotate (org-headline-cmpl-consult--annotate
                                           org-headline-cmpl-consult--hash-map)
                                :sort nil)))
    (gethash (car match) org-headline-cmpl-consult--hash-map (car match))))

(provide 'org-headline-cmpl-consult)

;;; org-mono-consult.el --- Org headline completion with consult -*- lexical-binding: t -*-
;; This package is serves no purpose org-mono
;;
;; Example configuration:
;; (setq org-mono-completing-read-fn
;;       'org-mono-consult-completing-read)

(require 'org-mono)
(require 'consult)

(defcustom org-mono-consult-sources
  '(org-mono-consult--source-headline
    org-mono-consult--source-todo
    org-mono-consult--source-todo-in-progress
    org-mono-consult--source-top-level
    org-mono-consult--source-top-level-no-todos
    org-mono-consult--source-today
    org-mono-consult--source-week)
  "Sources used by `org-mono-consult-completing-read'."
  :type '(repeat symbol)
  :group 'org-mono)

(defun org-mono-consult-special-entries-default-fn ()
  (list
   (format-time-string "Journal %Y-%m-%d")))

(defcustom org-mono-consult-special-entries-fn
  #'org-mono-consult-special-entries-default-fn
  "Function to generate special entires
See `org-mono-consult-special-entries-default-fn' and
`org-mono-consult--source-special'."
  :type 'function
  :group 'org-mono)

(defvar org-mono-consult--hash-map (make-hash-table)
  "Local variable used to generate candidates for
`org-mono-consult-sources'")

(defun org-mono--query (filter)
  "Return a list of string candidates which fn FILTER is non-nil.
The FILTER fn accepts one headline components argument.
See `org-mono--headline-components' for components structure."
  (seq-filter (lambda (key)
                (funcall filter
                         (gethash key
                                  org-mono-consult--hash-map)))
              (hash-table-keys
               org-mono-consult--hash-map)))

(defvar org-mono-consult--source-headline
  `(:narrow   (?  . "All")
    :category org-mono
    :default  nil
    :hidden   t
    :shows-all t
    :items
    ,(lambda () (org-mono--query (lambda (_) t))))
  "Headline candidate source for `org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-todo
  `(:narrow   (?t . "Todo")
    :category org-mono
    :default  nil
    :hidden   t
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand) (alist-get :todo cand)))))
    "Todo headline candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-todo-in-progress
  `(:narrow   (?i . "In progress")
    :category org-mono
    :default  nil
    :hidden   t
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand)
                   (let ((todo (alist-get :todo cand)))
                     (and todo (not (equal todo "DONE"))))))))
  "Not in done todo headline candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-top-level
  `(:narrow   (?l . "Top level")
    :category org-mono
    :default  nil
    :hidden   nil
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand)
                   (eq (alist-get :level cand) 1)))))
  "Top level headline candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-top-level-no-todos
  `(:narrow   (?L . "Top level no todos")
    :category org-mono
    :default  nil
    :hidden   t
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand)
                   (and (eq (alist-get :level cand) 1)
                        (null (alist-get :todo cand)))))))
  "Top level headline candidate source with no todos for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-special
  `(:narrow          (?s . "Special")
    :category        org-mono
    :default         nil
    :only-full-table t
    :items ,(lambda () (funcall org-mono-consult-special-entries-fn)))
  "Special candidate sourcw source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-today
  `(:narrow          (?d . "Today")
    :category        org-mono
    :default         nil
    :hidden          t
    :items
    ,(lambda ()
       (let ((current-day (org-today)))
         (org-mono--query
          (lambda (cand)
            (let ((timestamp (alist-get :timestamp cand)))
              (when timestamp
                (equal
                 (time-to-days (org-2ft timestamp))
                 current-day))))))))
  "Today timestamp candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-week
  `(:narrow          (?w . "This week")
    :category        org-mono
    :default         nil
    :hidden          t
    :items
    ,(lambda ()
       (let ((current-week (org-days-to-iso-week (org-today))))
         (org-mono--query
          (lambda (cand)
            (let ((timestamp (alist-get :timestamp cand)))
              (when timestamp
                (equal
                 (org-days-to-iso-week
                  (time-to-days (org-2ft timestamp)))
                 current-week))))))))
  "Current week candidate source for
`org-mono-consult-completing-read'.")

(defun org-mono--store-overlay-repr ()
  (let* ((overlays-list (overlay-lists))
         (overlays (append (car overlays-list)
                           (cdr overlays-list))))
    (mapcar (lambda (overlay)
              `(,(overlay-start overlay)
                ,(overlay-end overlay)
                . ,(overlay-properties overlay)))
            overlays)))

(defun org-mono-consult--headline-state ()
  "Headline state function."
  (let ((reset-buffer (current-buffer))
        (buffers-to-kill)
        (preview-buffers (make-hash-table :test 'equal)))
    (lambda (state cand)
      (let ((headline (gethash (car cand) org-mono-consult--hash-map)))
        (cond ((and (eq state 'preview)
                    headline)
               (let* ((opened (find-buffer-visiting
                                         (alist-get :file headline)))
                      (buffer (find-file-noselect
                               (alist-get :file headline))))
                 ;; Preview cand
                 (when opened
                   (let* ((buffer-fname (buffer-file-name buffer)))
                     (setq buffer
                           (or (gethash buffer-fname preview-buffers)
                               (puthash buffer-fname
                                        (make-indirect-buffer
                                         buffer
                                         (generate-new-buffer-name
                                          (format "Preview:%s"
                                                  (buffer-name buffer)))
                                         t)
                                        preview-buffers)))))
                 (push buffer buffers-to-kill)
                 (switch-to-buffer buffer)
                 (widen)
                 (when-let (marker (org-mono--file-link-to-marker headline))
                   (goto-char (marker-position marker))
                   (org-mono--after-jump))))
              ((and (eq state 'preview)
                    (null headline))
                 (switch-to-buffer reset-buffer))
              ((eq state 'exit)
               (seq-do #'kill-buffer buffers-to-kill)
               (switch-to-buffer reset-buffer)))))))

;; FIX: I am to lazy to figure out why this is happening
;; this is definitely going to break someday
(defun org-mono-consult--annotate (table)
  "Create annotatio function for `org-mono-consult-completing-read'
compleations. TABLE consist of the hash-table.
See `org-mono--completion-table'"
  (let ((annotate-fn (org-mono--annotate table)))
    (lambda (candidate)
      (funcall annotate-fn
               (apply #'concat
                      (butlast (split-string candidate "") 2))))))

(defvar org-mono-consult--hist nil)

(defun org-mono-consult-completing-read (prompt
                                         &optional
                                         hash-table
                                         require-match)
  "Consult multi source replacment for `org-mono-completing-read'
HASH-TABLE is used when the total set of precomputet headlines candidates is not
preferable. See `org-mono-consult-completing-read' how HASH-TABLE is
constructed.
For docs on the rest of the arguments see `completing-read'"
  (setq org-mono-consult--hash-map
        (or hash-table (org-mono--completion-table)))
  (let* (;; If we have recieved a hash-table remove :only-full-table
         ;; sources
         (sources (if hash-table
                      (seq-filter
                       (lambda (source)
                         (not (plist-get source :only-full-table)))
                       org-mono-consult-sources)
                    org-mono-consult-sources))
         ;; If we have require-match and hash-table "un-hide"
         ;; all `:shows-all' temporarily
         (sources (if (and hash-table require-match)
                      (mapcar (lambda (source)
                                (if (plist-get (eval source) :shows-all)
                                      (plist-put
                                       (seq-copy (eval source))
                                       :hidden
                                       nil)
                                  source))
                              sources)
                    sources))
         (match (consult--multi sources
                                :require-match require-match
                                :prompt prompt
                                :annotate (org-mono-consult--annotate
                                           org-mono-consult--hash-map)
                                :state (org-mono-consult--headline-state)
                                :history 'org-mono-consult--hist
                                )))
    (gethash (car match) org-mono-consult--hash-map (car match))))

(provide 'org-mono-consult)

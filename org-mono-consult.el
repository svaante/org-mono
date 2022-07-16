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
  `(:name     "All headlines"
    :narrow   ?a
    :category org-mono
    :state    ,#'org-mono-consult--headline-state
    :default  t
    :items
    ,(lambda () (org-mono--query (lambda (_) t))))
  "Headline candidate source for `org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-todo
  `(:name     "Todos"
    :narrow   ?t
    :category org-mono
    :state    ,#'org-mono-consult--headline-state
    :default  nil
    :hidden   t
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand) (alist-get :todo cand)))))
    "Todo headline candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-todo-in-progress
  `(:name     "Todos in-progress"
    :narrow   ?i
    :category org-mono
    :state    ,#'org-mono-consult--headline-state
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
  `(:name     "Top level"
    :narrow   ?l
    :category org-mono
    :state    ,#'org-mono-consult--headline-state
    :default  nil
    :hidden   t
    :items
    ,(lambda () (org-mono--query
                 (lambda (cand)
                   (eq (alist-get :level cand) 1)))))
  "Top level headline candidate source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-top-level-no-todos
    `(:name     "Top level no todos"
      :narrow   ?L
      :category org-mono
      :state    ,#'org-mono-consult--headline-state
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
  `(:name            "Special"
    :narrow          ?s
    :category        org-mono
    :state           ,#'org-mono-consult--headline-state
    :default         t
    :only-full-table t
    :items ,(lambda () (funcall org-mono-consult-special-entries-fn)))
  "Special candidate sourcw source for
`org-mono-consult-completing-read'.")

(defvar org-mono-consult--source-today
  `(:name            "Today"
    :narrow          ?d
    :category        org-mono
    :state           ,#'org-mono-consult--headline-state
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
  `(:name            "Week"
    :narrow          ?w
    :category        org-mono
    :state           ,#'org-mono-consult--headline-state
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

(defun org-mono-consult--position (headline &optional find-file)
  "Return the position marker for HEADLINE
See `org-mono--headline-components' for HEADLINE structure.
FIND-FILE is the file open function, defaulting to `find-file'."
  (when headline
    (org-mono--file-link-to-marker
     headline
     nil
     (funcall (or find-file #'find-file) (alist-get :file headline)))))

(defun org-mono-consult--after-jump ()
  "Hook to show headline subtree and narrow to subtree, if
`org-mono-narrow-after-goto' is non-nil."
  (org-show-subtree)
  (when org-mono-narrow-after-goto
    (org-narrow-to-subtree)))

(defun org-mono-consult--restore-buffer (buffer-reset-info)
  "Restor buffer after preview."
  (when buffer-reset-info
    (with-current-buffer (alist-get :buffer buffer-reset-info)
      (widen)
      (when-let ((region (alist-get :region buffer-reset-info)))
        (apply #'narrow-to-region region))
      (goto-char (alist-get :position buffer-reset-info)))))

(defun org-mono-consult--headline-state ()
  "Headline state function."
  (let ((reset-buffer (current-buffer))
        (buffers-to-kill)
        (buffers-reset-info))
    (lambda (state cand)
      (let ((headline (gethash cand org-mono-consult--hash-map)))
        (cond ((and (eq state 'preview)
                    headline)
               ;; FIX: Should handle when candidate is a non-match
               (let* ((not-opened (null (find-buffer-visiting (alist-get :file headline))))
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
                             (org-mono--file-link-to-marker headline)))
                 (when org-mono-narrow-after-goto
                   (org-narrow-to-subtree))
                 (org-show-subtree)))
              ((and (eq state 'preview)
                    (null cand))
               (progn 
                 (switch-to-buffer reset-buffer)
                 (org-mono-consult--restore-buffer
                  (seq-find (lambda (buffer-reset-info)
                              (eq reset-buffer
                                  (alist-get :buffer buffer-reset-info)))
                            buffers-reset-info))))
              ((eq state 'exit)
               (seq-do #'kill-buffer buffers-to-kill)
               (seq-do #'org-mono-consult--restore-buffer buffers-reset-info)
               (switch-to-buffer reset-buffer)))))))

;; FIX: I am to lazy to figure out why this is happening
;; this is definitely going to break someday
(defun org-mono-consult--annotate (table)
  "Create annotatio function for `org-mono-consult-completing-read'
compleations. TABLE consist of the hash-table.
See `org-mono--completion-table'"
  (let ((annotate-fn (org-mono--annotate table)))
    (lambda (candidate)
      (funcall annotate-fn (apply #'concat (butlast (split-string candidate "") 2))))))

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
  (let* ((sources (if hash-table
                      (seq-filter (lambda (source)
                                    (not (plist-get source :only-full-table)))
                                  org-mono-consult-sources)
                    org-mono-consult-sources))
         (match (consult--multi sources
                                :require-match require-match
                                :prompt prompt
                                :annotate (org-mono-consult--annotate
                                           org-mono-consult--hash-map)
                                :sort nil)))
    (gethash (car match) org-mono-consult--hash-map (car match))))

(provide 'org-mono-consult)

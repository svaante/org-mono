;;; org-headline-cmpl.el --- Org headline chache and completion -*- lexical-binding: t -*-

(require 'seq)
(require 'org)
(require 'ol)
(require 'org-capture)
(require 'timer)

(defgroup org-headline-cmpl nil
  "TODO"
  :group 'extensions)

(defcustom org-headline-cmpl-files '()
  "Specifies where org-headline-cmpl should search for file to index.
Value could either be function that returns a list of files or a list of files."
  :type '(choice (list :value-type string :tag "List of files")
                 (function :tag "Function that return list of files"))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-capture-default
  '(entry
    (file "~/org/notes.org")
    "* %(org-headline-cmpl-dwim-headline)\n  %?")
  "The default capture template that is used with `org-headline-cmpl-dwim'.
Works as `org-capture-templates' but withouth key and description."
  :type 'list
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-capture-templates
  '(("t" "Todo" "** TODO %?"))
  "Capture templates that capture under specified region used by
`org-headline-cmpl-capture-under-headline'. Same format as
`org-capture-templates' except for no target as that is handled by
`org-headline-cmpl-capture-under-headline'."
  :type 'list
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-headline-level 'all
  "Specifies headline level for indexing."
  :type '(choice (const :tag "All levels" all)
                 (integer :tag "Level of headline, [1, n]"))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-headline-post-filter-fn #'identity
  "Specifies which headlines that goes into the cache.
Takes a component as input and should return a non-nil value if the headline
should go into the cache. See `org-headline-cmpl--headline-components' for
headline data format"
  :type 'function
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-all-org-files nil
  "Limit caching to only files specified by `org-headline-cmpl-files'.
If `org-headline-cmpl-mode' is enable in a non-specified buffer this var toggels
indexing of headlines in current buffer."
  :type '(choice (const :tag "All org files" t)
                 (const :tag "Only files specified by `org-headline-cmpl-files`" nil))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-narrow-after-goto nil
  "To narrow or not to narrow after jump to a headline."
  :type '(choice (const :tag "Narrow" t)
                 (const :tag "Do not narrow" nil))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-cache-delay 2
  "Delay before re-indexing current buffer.
Increasing the value of `org-headline-cmpl-cache-delay' should improve performance."
  :type 'integer
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-completion-at-point-suffix nil
  "Add file name suffix when doing `completion-at-point'."
  :type '(choice (const :tag "Suffix completions with file" file)
                 (const :tag "No suffix" nil))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-completion-candidate-max-length 40
  "Max candidate length. Increase this value if window width is a non-issue."
  :type 'integer
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-annotation-format
  `((:file org-column 30 file-name-nondirectory)
    (:level org-property-value 2 (lambda (level) (make-string level ?*)))
    (:todo org-todo 4 identity)
    (:back-links org-level-4 50 ,(lambda (back-links)
                                    (string-join
                                     (mapcar (lambda (back-link)
                                               (alist-get :headline
                                                          back-link))
                                             back-links)
                                     ", "))))
  "Annotation formating."
  :type '(alist :value-type (group face integer function))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-dwim-default-candidate-fn nil
  "Default candidate for `org-headline-cmpl-dwim'
If function return default candidate."
  :type '(choice (function :tag "Function that returns a string")
                 (const :tag "No default" nil))
  :group 'org-headline-cmpl)

(defcustom org-headline-cmpl-completing-read-fn #'org-headline-cmpl-completing-read
  "What function are used for completing headlines."
  :type 'function
  :group 'org-headline-cmpl)

;; Internal vars
(defvar org-headline-cmpl--cache
  (make-hash-table :test 'equal))

(defvar org-headline-cmpl--cache-timer
  nil)

;; Cache
(defun org-headline-cmpl--schedule-cache-timer (&rest _)
  "Indexing headlines on a timer."
  (unless (bound-and-true-p org-headline-cmpl--cache-timer)
    (setq org-headline-cmpl--cache-timer
          (run-with-timer
           org-headline-cmpl-cache-delay
           nil
           (lambda (buffer)
             (when (or (and org-headline-cmpl-all-org-files (with-current-buffer buffer
                                                              (bound-and-true-p org-mode)))
                       (member (buffer-file-name buffer) (org-headline-cmpl--get-files)))
               (org-headline-cmpl--cache-file (buffer-file-name buffer) t))
             (setq org-headline-cmpl--cache-timer nil))
           (current-buffer)))))

(defun org-headline-cmpl--full-cache (&optional files)
  "Cache all files in `org-headline-cmpl-files'."
  (let* ((files (or files (org-headline-cmpl--get-files)))
         (files-to-cache (seq-filter (lambda (filename)
                                       (when (or org-headline-cmpl-all-org-files
                                                 (member filename (org-headline-cmpl--get-files)))))
                                     files)))
    (clrhash org-headline-cmpl--cache)
    (seq-do #'org-headline-cmpl--cache-file files)
    (org-headline-cmpl--add-backlinks-to-cache)))

(defun org-headline-cmpl--cache-file (file &optional rebuild-backlinks)
  "Cache FILE if REBUILD-BACKLINKS is non-nil backlinks are added to the cache."
  (let* ((not-opened (null (find-buffer-visiting file)))
         (buffer (find-file-noselect file t))
         (components (org-headline-cmpl--headlines-components buffer))
         ;; FIXME: The filter function does not have access to :back-links at this time
         (filtered-components (seq-filter org-headline-cmpl-headline-post-filter-fn
                                          components)))
    (puthash file
             filtered-components
             org-headline-cmpl--cache)
    (when rebuild-backlinks
      (org-headline-cmpl--add-backlinks-to-cache))
    (when not-opened
      (kill-buffer buffer))))

(defun org-headline-cmpl--add-backlinks-to-cache ()
  "Add backlinks to cache. Expect cache to be populated before running."
  (let ((headlines (org-headline-cmpl--list-headlines 'list))
        (back-links-hmap (make-hash-table :test 'equal)))
    (seq-do (lambda (components)
              (let ((file (alist-get :file components))
                    (headline (alist-get :headline components)))
                (seq-do (lambda (file-links)
                          (puthash file-links
                                   (cons
                                    `((:file . ,file)
                                      (:headline . ,headline))
                                    (gethash file-links
                                             back-links-hmap))
                                   back-links-hmap))
                        (alist-get :file-links components))))
            headlines)
    (seq-do (lambda (components)
              (let ((key `((:file . ,(alist-get :file components))
                           (:headline . ,(alist-get :headline components)))))
                ;; Update inplace
                (setcdr
                 (assoc :back-links components)
                 (gethash key back-links-hmap))))
            headlines)))

;; Utils
(defconst org-headline-cmpl--link-re
  "\\[\\[\\(file:\\(.+\\.org\\)::\\)?\\*\\(.+\\)\\]\\[.*\\]\\]")

(defun org-headline-cmpl--get-files ()
  "Resolve file list specified by `org-headline-cmpl-files'"
  (mapcar 'expand-file-name org-headline-cmpl-files))

(defun org-headline-cmpl--file-link-to-marker (components &optional create buffer)
  "Create marker from COMPONENTS see `org-headline-cmpl--headline-components' for
COMPONENTS data structure. CREATE specifies if the headline should be created if
not found. Use BUFFER marker should be created in BUFFER."
  ;; TODO: should probobly handle non existent files
  (with-current-buffer (or buffer (find-file-noselect (alist-get :file components)))
    (org-with-wide-buffer
     (let ((org-link-search-must-match-exact-headline (or (not create) 'query-to-create)))
       (ignore-errors (org-link-search (alist-get :headline components))))
     (point-marker))))

(defun org-headline-cmpl--headline-re ()
  "Construct regexp for headline. Uses `org-headline-cmpl-headline-level'."
  (pcase org-headline-cmpl-headline-level
    ((and (pred integerp) n) (format "^\\*\\{1,%d\\}" n))
     ('all "^\\*+")))

(defun org-headline-cmpl--headline-components ()
  "Creates base for headline components at point."
  (ignore-error user-error
    (pcase-let ((`(,level ,_ ,todo ,prio ,headline ,tags)
                 (org-heading-components)))
      `((:level . ,level)
        (:todo  . ,todo)
        (:prio  . ,prio)
        (:headline  . ,headline)
        (:tags . ,tags)
        (:file-links . ,(org-headline-cmpl--heading-org-links))
        (:back-links . ,nil)
        (:file . ,(buffer-file-name (current-buffer)))))))

(defun org-headline-cmpl--next-headline-point ()
  "Get point of next headline."
  (save-match-data
    (save-mark-and-excursion
      (unless (eq (condition-case nil
                      (org-back-to-heading t)
                    (ignore-error 'before-first-heading))
                  'before-first-heading)
        ;; We are standing at the start of a headline
        (forward-char))
      (when (re-search-forward
             (org-headline-cmpl--headline-re)
             nil
             t)
        (match-beginning 0)))))

(defun org-headline-cmpl--heading-org-links ()
  "Return current headlines org links as a list of alists containing :file and
:headline."
  (let ((bound (org-headline-cmpl--next-headline-point)))
    (save-match-data
      (save-mark-and-excursion
        (let (links)
          (while (re-search-forward org-headline-cmpl--link-re
                                    bound
                                    t)
            (let* ((match-filename (when (match-string 2)
                                     (substring-no-properties
                                      (match-string 2))))
                   (match-headline (when (match-string 3)
                                     (substring-no-properties
                                      (match-string 3))))
                   (filename (or match-filename
                                 (buffer-file-name (current-buffer)))))
              (push `((:file . ,(expand-file-name filename))
                      (:headline . ,match-headline))
                    links)))
          (reverse links))))))

(defun org-headline-cmpl--headlines-components (buffer)
  "Return all headline components for BUFFER."
  ;; FIX: this is not optimal at all should use headline level instead of
  ;; org-map-entries
  (seq-filter
   (lambda (hc)
     (pcase org-headline-cmpl-headline-level
       ((and (pred integerp) n)
        (eq (alist-get :level hc) n))
       ('all t)))
   (with-current-buffer buffer
     (org-map-entries
      #'org-headline-cmpl--headline-components nil 'file))))

(defun org-headline-cmpl--list-headlines (format)
  "Get cache contents. Return as either list or hash-map where key is filename.
This is specified with FORMAT as 'list or 'hash."
  (when (seq-empty-p (hash-table-keys org-headline-cmpl--cache))
    (org-headline-cmpl--full-cache))
  (pcase format
    ('hash org-headline-cmpl--cache)
    ('list (apply #'append
                  (hash-table-values org-headline-cmpl--cache)))))

(defun org-headline-cmpl--completion-table-add
    (components table &optional capf try-nbr)
  "Add COMPONENTS, see `org-headline-cmpl--headline-components' into hash-table TABLE.
The key for COMPONENTS is different depending in if CAPF is t or TRY-NBR which
should only used by `org-headline-cmpl--completion-table-add' itself."
  (let* ((headline (alist-get :headline components))
         (base-key (if (and capf
                            (eq 'file org-headline-cmpl-completion-at-point-suffix))
                       (format "%s:%s"
                               headline
                               (file-name-nondirectory
                                (alist-get :file components)))
                     headline))
         (key (concat 
               (truncate-string-to-width base-key
                                         org-headline-cmpl-completion-candidate-max-length
                                         0 nil "...")
               (when try-nbr
                 (format "<%d>" try-nbr)))))
    (if (gethash key table nil)
        (org-headline-cmpl--completion-table-add components
                                                 table
                                                 capf
                                                 (1+ (or try-nbr 0)))
      (puthash key components table))))


(defun org-headline-cmpl--completion-table (&optional headlines)
  "Construct a completion candidate hash-map of string -> components.
HEADLINES specifies a subset of the cache as a list of components."
  (let ((headlines (or headlines (org-headline-cmpl--list-headlines 'list)))
        (table (make-hash-table :test 'equal)))
    (seq-do (lambda (components)
              (org-headline-cmpl--completion-table-add components
                                                       table))
            headlines)
    table))

(defun org-headline-cmpl--headlines-list-with-filter (fn &rest keys)
  "Return a list of cached components based the filter function FN.
FN takes components and should return a non-nil value if the headline is kept.
If KEYS are specified KEYS are alisted and then applied to FN."
  (seq-filter (lambda (headline)
                (if keys
                    (apply fn (mapcar (lambda (key)
                                        (alist-get key headline))
                                      keys))
                  (funcall fn headline)))
              (org-headline-cmpl--list-headlines 'list)))

(defun org-headline-cmpl--list-backlinks (headline-components)
  "List all back links for HEADLINE-COMPONENTS."
  (let ((source-file (alist-get :file headline-components))
        (source-headline (alist-get :headline headline-components)))
    (org-headline-cmpl--headlines-list-with-filter
     (lambda (file headline)
       (and (equal file source-file)
            (equal headline source-headline)))
     :file :headline)))

(defmacro org-headline-cmpl--with-headline (headline &rest body)
  "Macro for using executing BODY at point of HEADLINE."
  (declare (indent 1) (debug t))
  `(let* ((not-opened (null (find-buffer-visiting (alist-get :file ,headline))))
          (marker (org-headline-cmpl--file-link-to-marker headline))
          (buffer (marker-buffer marker))
          (position (marker-position marker)))
     (with-current-buffer buffer
       (save-excursion
         (save-restriction
           (widen)
           (goto-char position)
           (org-show-subtree)
           ,@body)))
     (when not-opened
       (kill-buffer buffer))))

;; Completions
(defun org-headline-cmpl--annotate (table)
  "Creates annotation function for `org-headline-cmpl-completing-read'.
Uses TABLE to calculate the max length for the candidates."
  (let* ((candidates (hash-table-keys table))
         (--candidates-max-length
          (seq-reduce (lambda (acc candidate)
                        (max acc (length candidate)))
                      candidates
                      0)))
    (lambda (candidate)
      (let ((components (gethash candidate table)))
        (if components
          (concat
           (make-string (- --candidates-max-length
                           (length candidate))
                        ?\s)
           "  "
           (string-join
            (mapcar (lambda (annotation-format)
                      (pcase-let* ((`(,key ,face ,width ,fn)
                                    annotation-format)
                                   (component (alist-get key
                                                         components))
                                   (str (funcall fn component)))
                        (truncate-string-to-width (propertize (or str "")
                                                              'face face)
                                                  width
                                                  0
                                                  ?\s)))
                    org-headline-cmpl-annotation-format)
            "  "))
          " (new caption)")))))


;; Shamelessly stolen from org-roam-complete-everywhere
(defun org-headline-cmpl-link-complete ()
  "Complete symbol at point as a link completion to an org headline in current buffer.
  This is a `completion-at-point' function."
  (when (and (thing-at-point 'word)
             (not (org-at-heading-p))
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((table (org-headline-cmpl--completion-table))
          (bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (org-headline-cmpl--completion-table)
            :exit-function
            (lambda (str _status)
              (let ((components (gethash str table)))
                (delete-char (- (length str)))
                (insert (format "[[file:%s::*%s][%s]]"
                                (alist-get :file components)
                                (alist-get :headline components)
                                (alist-get :headline components)))))
            ;; Proceed with the next completion function if the returned titles
            ;; do not match. This allows the default Org capfs or custom capfs
            ;; of lower priority to run.
            :exclusive 'no))))

(defun org-headline-cmpl-completing-read (prompt &optional hash-table require-match)
  "HASH-TABLE is used when the total set of precomputet headlines candidates is not
preferable. See `org-headline-cmpl--completion-table' how HASH-TABLE is
constructed.
For docs on the rest of the arguments see `completing-read'"
  (let* ((hash-table (or hash-table (org-headline-cmpl--completion-table)))
         (metadata `(metadata
                     (category . org-headline-cmpl)
                     (annotation-function . ,(org-headline-cmpl--annotate
                                              hash-table))))
         (collection (lambda (string predicate action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action hash-table string predicate)))))
    (let ((match (completing-read prompt collection nil require-match)))
      (gethash match hash-table match))))

;; Capture
(defvar org-headline--headline-injected-headline nil
  "Used to save candiate match and inject into org capture template.")

(defun org-headline-cmpl-dwim-headline ()
  "Returns the headline string for the last `org-headline-cmpl-dwim' non-match."
  org-headline--headline-injected-headline)

(defun org-headline-cmpl-create-file+function (headline)
  "Creates an org-capture-file+function from HEADLINE see
`org-capture-templates'."
  (lambda ()
    (let ((marker (org-headline-cmpl--file-link-to-marker headline)))
      (set-buffer (org-capture-target-buffer (buffer-file-name
                                              (marker-buffer marker))))
      (goto-char (marker-position marker))
      (let ((beg (point)))
        ;; Move to next of current match
        (org-forward-heading-same-level 1 t)
        ;; Move to end of buffer if this is the last headline in the file
        (when (= beg (point))
          (goto-char (point-max)))))))

(defun org-headline--dwim-capture (headline-str)
  "Captures a new headline under new `HEADLINE-STR` with template
`org-headline-cmpl-capture-default'."
  (unless org-headline-cmpl-capture-default
   (user-error "org-headline-cmpl-capture-default is nil, specify default org-headline-cmpl capture template"))
  (let ((org-capture-templates (list (append
                                      '("a" "")
                                      org-headline-cmpl-capture-default))))
    (setq org-headline--headline-injected-headline headline-str)
    (org-capture nil "a")))

;; Eldoc integration
(defun org-headline-cmpl--back-links-in-heading ()
  "Gets all backlinks under heading at point."
  (when (and (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (let ((headline (nth 4 (org-heading-components))))
      (alist-get
       :back-links 
       (seq-find (lambda (component)
                   (equal (alist-get :headline component)
                          headline))
                 (gethash buffer-file-name
                          org-headline-cmpl--cache))))))

(defun org-headline-cmpl-eldoc-back-links (callback)
  "This is an `eldoc-documentation-functions' to display backlinks at point.
Note this only work if current file is indexed in cache."
  (let ((back-links (org-headline-cmpl--back-links-in-heading)))
    (funcall
     callback
     (when back-links
       (org-fontify-like-in-org-mode
        (format "Back links: %s"
                (string-join
                 (mapcar (lambda (component)
                           (format "[[%s]]"
                                   (alist-get :headline component)))
                         back-links)
                 ", ")))))))

;; Commands
(defun org-headline-cmpl-capture-under-headline (headline)
  "Capture under HEADLINE."
  (interactive
   (list
    (funcall org-headline-cmpl-completing-read-fn "Entry headline: ")))
  (let ((org-capture-templates
         (mapcar
          (lambda (entry)
            (pcase-let ((`(,keys ,desc ,template . ,properties) entry))
              `(,keys
                ,desc
                plain
                (function ,(org-headline-cmpl-create-file+function headline))
                ,template
                . ,properties)))
          org-headline-cmpl-capture-templates)))
    (call-interactively #'org-capture)))

(defun org-headline-cmpl-goto-back-links (headline internal-link)
  "Goto backlinks for HEADLINE."
  (interactive
   (list
    (setq match
          (funcall org-headline-cmpl-completing-read-fn
           "Headline: "
           (org-headline-cmpl--completion-table
            (org-headline-cmpl--headlines-list-with-filter #'identity :back-links))
           t))
    (let* ((prompt (format "Back links for *%s*: " (alist-get :headline match)))
           (table (org-headline-cmpl--completion-table
                   (org-headline-cmpl--list-backlinks match))))
      (when table
        (funcall org-headline-cmpl-completing-read-fn prompt table t)))))
  (if internal-link
      (org-headline-cmpl-goto internal-link)
    (user-error "Unable to derive current headline or missing internal links")))

(defun org-headline-cmpl-delete-headline (headline)
  "Delete HEADLINE."
  (interactive
   (list
    (funcall org-headline-cmpl-completing-read-fn "Delete headline: ")))
  (when (yes-or-no-p
         (format "Do you really want to delete headline *%s* and its contents"
                 (alist-get :headline headline)))
    (org-headline-cmpl--with-headline headline
      (org-narrow-to-subtree)
      (delete-region (point-min) (point-max)))))

(defun org-headline-cmpl-goto (headline)
  "Goto HEADLINE."
  (interactive
   (list
    (funcall org-headline-cmpl-completing-read-fn "Goto headline: " nil t)))
  (let ((marker (org-headline-cmpl--file-link-to-marker headline)))
    (switch-to-buffer (marker-buffer marker))
    (widen)
    (goto-char (marker-position marker))
    (org-show-subtree)
    (when org-headline-cmpl-narrow-after-goto
      (org-narrow-to-subtree))))

(defun org-headline-cmpl-todo (headline)
  "Cycle todo for HEADLINE."
  (interactive
   (list
    (funcall org-headline-cmpl-completing-read-fn "Todo headline: " nil t)))
  (org-headline-cmpl--with-headline headline
    (org-todo)))

(defun org-headline-cmpl-dwim (headline)
  "Dwim HEADLINE. Goto if match otherwise use capture under HEADLINE's name
with `nameorg-headline-cmpl-capture-default'."
  (interactive
   (list
    (funcall org-headline-cmpl-completing-read-fn "DWIM headline: ")))
  (cond ((listp headline) (org-headline-cmpl-goto headline))
        ((stringp headline) (org-headline--dwim-capture headline))))'

;; Mode
(defvar org-headline-cmpl-mode nil)

;;;###autoload
(define-minor-mode org-headline-cmpl-mode
  "Mode to index org headlines and gather metadata for said headlines.
This mode also enables completion at point and eldoc documentation."
  :init-value nil
  :lighter " org-headline-cmpl"
  :group 'org-headline-cmpl
  (cond
   (org-headline-cmpl-mode
    (add-hook 'post-command-hook #'org-headline-cmpl--schedule-cache-timer t t)
    (add-hook 'after-change-functions #'org-headline-cmpl--schedule-cache-timer t t)
    (add-hook 'eldoc-documentation-functions #'org-headline-cmpl-eldoc-back-links nil t)
    (add-hook 'completion-at-point-functions #'org-headline-cmpl-link-complete 100 t))
   (t
    (remove-hook 'post-command-hook #'org-headline-cmpl--schedule-cache-timer t)
    (remove-hook 'after-change-functions #'org-headline-cmpl--schedule-cache-timer t)
    (remove-hook 'eldoc-documentation-functions #'org-headline-cmpl-eldoc-back-links t)
    (remove-hook 'completion-at-point-functions #'org-headline-cmpl-link-complete t)
    (when org-headline-cmpl--cache-timer
      (cancel-timer org-headline-cmpl--cache-timer)
      (setq org-headline-cmpl--cache-timer nil)))))

(defun turn-on-headline-cmpl-mode ()
  "If `org-headline-cmpl-mode' should be enabled for buffer.
Based on `org-headline-cmpl-all-org-files' or if buffer is specified by
`org-headline-cmpl-files'."
  (if org-headline-cmpl-all-org-files
      (when (bound-and-true-p org-mode)
        (org-headline-cmpl-mode))
    (when (member buffer-file-name (org-headline-cmpl--get-files))
      (org-headline-cmpl-mode))))

;;;###autoload
(define-globalized-minor-mode global-org-headline-cmpl-mode
  org-headline-cmpl-mode
  turn-on-headline-cmpl-mode
  :init-value nil
  (cond
   (global-org-headline-cmpl-mode
    (add-hook 'org-capture-after-finalize-hook #'org-headline-cmpl--full-cache t))
   (t
    (remove-hook 'org-capture-after-finalize-hook #'org-headline-cmpl--full-cache t))))

(provide 'org-headline-cmpl)


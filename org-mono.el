;;; org-mono.el --- Org headline chache and completion -*- lexical-binding: t -*-

(require 'seq)
(require 'org)
(require 'ol)
(require 'org-capture)
(require 'org-element)
(require 'timer)
(require 'cl-macs)

(defgroup org-mono nil
  "TODO"
  :group 'extensions)

(defcustom org-mono-files '()
  "Specifies where org-mono should search for file to index.
Value could either be function that returns a list of files or a list of files."
  :type '(choice (list :value-type string :tag "List of files")
                 (function :tag "Function that return list of files"))
  :group 'org-mono)

(defcustom org-mono-capture-dwim-template
  '(entry
    (file "~/org/notes.org")
    "* %(org-mono-dwim-headline)\n  %?")
  "The default capture template that is used with `org-mono-dwim'.
Works as `org-capture-templates' but withouth key and description."
  :type 'list
  :group 'org-mono)

(defcustom org-mono-capture-templates
  '(("t" "Todo" "%(org-mono-capture-parent-stars)* TODO %?"))
  "Capture templates that capture under specified region used by
`org-mono-capture-under-headline'. Same format as
`org-capture-templates' except for no target as that is handled by
`org-mono-capture-under-headline'."
  :type 'list
  :group 'org-mono)

(defcustom org-mono-headline-level 'all
  "Specifies headline level for indexing."
  :type '(choice (const :tag "All levels" all)
                 (integer :tag "Level of headline, [1, n]"))
  :group 'org-mono)

(defcustom org-mono-headline-post-filter-fn #'identity
  "Specifies which headlines that goes into the cache.
Takes a component as input and should return a non-nil value if the headline
should go into the cache. See `org-mono--headline-components' for
headline data format"
  :type 'function
  :group 'org-mono)

(defcustom org-mono-all-org-files nil
  "Limit caching to only files specified by `org-mono-files'.
If `org-mono-mode' is enable in a non-specified buffer this var toggels
indexing of headlines in current buffer."
  :type '(choice (const :tag "All org files" t)
                 (const :tag "Only files specified by `org-mono-files`" nil))
  :group 'org-mono)

(defcustom org-mono-narrow-after-goto nil
  "To narrow or not to narrow after jump to a headline."
  :type '(choice (const :tag "Narrow" t)
                 (const :tag "Do not narrow" nil))
  :group 'org-mono)

(defcustom org-mono-auto-save t
  "After org-mono applies changes to buffer, auto save before
closing."
  :type '(choice (const :tag "Auto save" t)
                 (const :tag "Prompt user for save" nil)))

(defcustom org-mono-cache-delay 2
  "Delay before re-indexing current buffer.
Increasing the value of `org-mono-cache-delay' should improve performance."
  :type 'integer
  :group 'org-mono)

(defcustom org-mono-completion-at-point-suffix nil
  "Add file name suffix when doing `completion-at-point'."
  :type '(choice (const :tag "Suffix completions with file" file)
                 (const :tag "No suffix" nil))
  :group 'org-mono)

(defcustom org-mono-completion-candidate-max-length 40
  "Max candidate length. Increase this value if window width is a non-issue."
  :type 'integer
  :group 'org-mono)

(defcustom org-mono-annotation-format
  `((:file org-column 10 file-name-nondirectory)
    (:level org-property-value 2 ,(lambda (level) (make-string level ?*)))
    (:timestamp org-date 16 identity)
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
  :group 'org-mono)

(defcustom org-mono-dwim-default-candidate-fn nil
  "Default candidate for `org-mono-dwim'
If function return default candidate."
  :type '(choice (function :tag "Function that returns a string")
                 (const :tag "No default" nil))
  :group 'org-mono)

(defcustom org-mono-completing-read-fn #'org-mono-completing-read
  "What function are used for completing headlines."
  :type 'function
  :group 'org-mono)

;; Internal vars
(defvar org-mono--cache
  (make-hash-table :test 'equal))

(defvar org-mono--cache-timer
  nil)

;; Cache
(defun org-mono--schedule-cache-timer (&rest _)
  "Indexing headlines on a timer."
  (unless (bound-and-true-p org-mono--cache-timer)
    (setq org-mono--cache-timer
          (run-with-timer
           org-mono-cache-delay
           nil
           (lambda (buffer)
             (when (or (and org-mono-all-org-files (with-current-buffer buffer
                                                              (bound-and-true-p org-mode)))
                       (member (buffer-file-name buffer) (org-mono--get-files)))
               (org-mono--cache-file (buffer-file-name buffer) t))
             (setq org-mono--cache-timer nil))
           (current-buffer)))))

(defun org-mono--full-cache (&optional files)
  "Cache all files in `org-mono-files'."
  (let* ((files (or files (org-mono--get-files)))
         (files-to-cache (seq-filter
                          (lambda (filename)
                            (or org-mono-all-org-files
                                (member filename (org-mono--get-files))))
                          files)))
    (clrhash org-mono--cache)
    (seq-do #'org-mono--cache-file files-to-cache)
    (org-mono--add-backlinks-to-cache)))

(defun org-mono--cache-file (file &optional rebuild-backlinks)
  "Cache FILE if REBUILD-BACKLINKS is non-nil backlinks are added to the cache."
  (let* ((not-opened (null (find-buffer-visiting file)))
         (buffer (find-file-noselect file t))
         (components (org-mono--headlines-components buffer))
         ;; FIXME: The filter function does not have access to :back-links at this time
         (filtered-components (seq-filter org-mono-headline-post-filter-fn
                                          components)))
    (puthash file
             filtered-components
             org-mono--cache)
    (when rebuild-backlinks
      (org-mono--add-backlinks-to-cache))
    (when not-opened
      (kill-buffer buffer))))

(defun org-mono--add-backlinks-to-cache ()
  "Add backlinks to cache. Expect cache to be populated before running."
  (let ((headlines (org-mono--list-headlines 'list))
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
(defconst org-mono--link-re
  "\\[\\[\\(file:\\(.+\\.org\\)::\\)?\\*\\(.+\\)\\]\\[.*\\]\\]")

(defun org-mono--get-files ()
  "Resolve file list or function specified by `org-mono-files'"
  (mapcar 'expand-file-name
          (pcase org-mono-files
            ((pred functionp) (funcall org-mono-files))
            (_ org-mono-files))))

(defun org-mono--file-link-to-marker (components &optional create buffer)
  "Create marker from COMPONENTS see `org-mono--headline-components' for
COMPONENTS data structure. CREATE specifies if the headline should be created if
not found. Use BUFFER marker should be created in BUFFER."
  ;; TODO: should probobly handle non existent files
  (with-current-buffer (or buffer (find-file-noselect (alist-get :file components)))
    (org-with-wide-buffer
     (let ((org-link-search-must-match-exact-headline (or (not create) 'query-to-create)))
       (ignore-errors (org-link-search (alist-get :headline components))))
     (point-marker))))

(defun org-mono--headline-re ()
  "Construct regexp for headline. Uses `org-mono-headline-level'."
  (pcase org-mono-headline-level
    ((and (pred integerp) n) (format "^\\*\\{1,%d\\}" n))
     ('all "^\\*+")))

(defun org-mono--headline-components ()
  "Creates base for headline components at point."
  (ignore-error user-error
    (pcase-let ((`(,level ,_ ,todo ,prio ,headline ,tags)
                 (org-heading-components))
                (timestamp
                 (org-mono--get-timestamp-at-headline))
                (parents (org-mono--get-parents-at-point)))
      `((:level . ,level)
        (:todo  . ,todo)
        (:prio  . ,prio)
        (:headline  . ,headline)
        (:tags . ,tags)
        (:file-links . ,(org-mono--heading-org-links))
        (:back-links . ,nil)
        (:timestamp . ,timestamp)
        (:parents . ,parents)
        (:file . ,(buffer-file-name (current-buffer)))))))

(defun org-mono--next-headline-point ()
  "Get point of next headline."
  (save-match-data
    (save-mark-and-excursion
      (unless (eq (condition-case nil
                      (org-back-to-heading t)
                    (error 'before-first-heading))
                  'before-first-heading)
        ;; We are standing at the start of a headline
        (forward-char))
      (when (re-search-forward
             (org-mono--headline-re)
             nil
             t)
        (match-beginning 0)))))

(defun org-mono--prev-headline-point ()
  "Get point of prev headline."
  (save-match-data
    (save-mark-and-excursion
      (when (re-search-backward
             (org-mono--headline-re)
             nil
             t)
        (match-beginning 0)))))

(defun org-mono--get-parents-at-point (&optional starting-point)
  (org-with-wide-buffer
   (save-mark-and-excursion
     (when starting-point
       (goto-char starting-point))
     (pcase-let ((`(,parent-level) (org-heading-components)))
       (when-let ((parent-point (org-mono--prev-headline-point)))
         (goto-char parent-point)
         (pcase-let ((`(,child-level ,_ ,_ ,_ ,headline) (org-heading-components)))
           (when (> parent-level child-level)
             (cons headline
                   (org-mono--get-parents-at-point parent-point)))))))))

(defun org-mono--heading-org-links ()
  "Return current headlines org links as a list of alists containing :file and
:headline."
  (let ((bound (org-mono--next-headline-point)))
    (save-match-data
      (save-mark-and-excursion
        (let (links)
          (while (re-search-forward org-mono--link-re
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

(defun org-mono--headlines-components (buffer)
  "Return all headline components for BUFFER."
  ;; FIX: this is not optimal at all should use headline level instead of
  ;; org-map-entries
  (seq-filter
   (lambda (hc)
     (pcase org-mono-headline-level
       ((and (pred integerp) n)
        (eq (alist-get :level hc) n))
       ('all t)))
   (with-current-buffer buffer
     ;; HACK to remove "Non-existent agenda file" when file does not exist.
     (cl-flet ((org-agenda-prepare-buffers (&rest _) nil))
       (org-map-entries
        #'org-mono--headline-components nil 'file)))))

(defun org-mono--list-headlines (format)
  "Get cache contents. Return as either list or hash-map where key is filename.
This is specified with FORMAT as 'list or 'hash."
  (when (seq-empty-p (hash-table-keys org-mono--cache))
    (org-mono--full-cache))
  (pcase format
    ('hash org-mono--cache)
    ('list (apply #'append
                  (hash-table-values org-mono--cache)))))

(defun org-mono--completion-table-add
    (components table &optional capf try-nbr)
  "Add COMPONENTS, see `org-mono--headline-components' into hash-table TABLE.
The key for COMPONENTS is different depending in if CAPF is t or TRY-NBR which
should only used by `org-mono--completion-table-add' itself."
  (let* ((headline (alist-get :headline components))
         (base-key (if (and capf
                            (eq 'file org-mono-completion-at-point-suffix))
                       (format "%s:%s"
                               headline
                               (file-name-nondirectory
                                (alist-get :file components)))
                     headline))
         (key (concat 
               (truncate-string-to-width base-key
                                         org-mono-completion-candidate-max-length
                                         0 nil "...")
               (when try-nbr
                 (format "<%d>" try-nbr)))))
    (if (gethash key table nil)
        (org-mono--completion-table-add components
                                                 table
                                                 capf
                                                 (1+ (or try-nbr 0)))
      (puthash key components table))))


(defun org-mono--completion-table (&optional headlines)
  "Construct a completion candidate hash-map of string -> components.
HEADLINES specifies a subset of the cache as a list of components."
  (let ((headlines (or headlines (org-mono--list-headlines 'list)))
        (table (make-hash-table :test 'equal)))
    (seq-do (lambda (components)
              (org-mono--completion-table-add components
                                                       table))
            headlines)
    table))

(defun org-mono--headlines-list-with-filter (fn &rest keys)
  "Return a list of cached components based the filter function FN.
FN takes components and should return a non-nil value if the headline is kept.
If KEYS are specified KEYS are alisted and then applied to FN."
  (seq-filter (lambda (headline)
                (if keys
                    (apply fn (mapcar (lambda (key)
                                        (alist-get key headline))
                                      keys))
                  (funcall fn headline)))
              (org-mono--list-headlines 'list)))

(defun org-mono--list-backlinks (headline-components)
  "List all back links for HEADLINE-COMPONENTS."
  (let ((back-links (mapcar (lambda (comp)
                              (cons (alist-get :file comp)
                                    (alist-get :headline comp)))
                            (alist-get :back-links headline-components))))
    (seq-filter (lambda (comp)
                  (member (cons (alist-get :file comp)
                                (alist-get :headline comp))
                          back-links))
                (org-mono--list-headlines 'list))))

;; FIX: this should not always run hooks
(defmacro org-mono--with-headline (headline &rest body)
  "Macro for using executing BODY at point of HEADLINE."
  (declare (indent 1) (debug t))
  `(let* ((not-opened (null (find-buffer-visiting (alist-get :file ,headline))))
          (marker (org-mono--file-link-to-marker ,headline))
          (buffer (marker-buffer marker))
          (position (marker-position marker)))
     (with-current-buffer buffer
       (save-excursion
         (save-restriction
           (widen)
           (goto-char position)
           (org-show-subtree)
           (prog1 ,@body
             (org-mono--schedule-cache-timer)
             (when not-opened
               (when (and org-mono-auto-save
                          (buffer-modified-p))
                 (save-buffer))
               (kill-buffer buffer))))))))

(defun org-mono--first-timestamp-mark (components)
  "Find mark for first timestamp under and within COMPONENTS."
  (org-mono--with-headline components
    (save-match-data
      (let* ((end (or (org-mono--next-headline-point) (point-max)))
             (timestamp-point (re-search-forward
                               org-element--timestamp-regexp
                               end
                               t)))
        (when timestamp-point
          (goto-char (match-beginning 0))
          (point-marker))))))

(defun org-mono--get-timestamp-at-headline ()
  "Get org timestamp string under headline at point."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (org-show-subtree)
        (let* ((end (or (org-mono--next-headline-point) (point-max)))
               (timestamp-point (re-search-forward
                                 org-element--timestamp-regexp
                                 end
                                 t)))
          (when timestamp-point
            (buffer-substring-no-properties
             (match-beginning 0)
             (match-end 0))))))))

;; FIX: this is a bit messy
(defun org-mono--headlines-with-children ()
  "Get all headlines with children"
  (let ((values (hash-table-values (org-mono--list-headlines 'hash))))
    (mapcan
     (lambda (components-in-file)
       (delete-dups
        (cdr
         (seq-reduce
          (lambda (cands-res components)
            (let* ((cands (car cands-res))
                   (res (cdr cands-res))
                   (level (alist-get :level components))
                   (parents (alist-get :parents components))
                   (cand-headlines (mapcar (lambda (x)
                                             (alist-get :headline x))
                                           cands)))
              (cons
               (cons components
                     ;; Drop the difference of current `cands' length and `level'
                     (seq-drop cands (max (- (1+ (length cands)) level) 0)))
               (append res
                       (when (equal cand-headlines parents)
                         cands)))))
          components-in-file
          nil))))
     values)))

(defun org-mono--children-of-headline (components)
  "Get all children for COMPONENTS headline."
  (let* ((values (gethash (alist-get :file components)
                          (org-mono--list-headlines 'hash)))
         (candidates (cdr (seq-drop-while (lambda (comp)
                                            (not (equal comp components)))
                                          values)))
         (level (alist-get :level components)))
    (seq-take-while (lambda (comp)
                      (< level (alist-get :level comp)))
                    candidates)))

;; Completions
(defun org-mono--annotate (table)
  "Creates annotation function for `org-mono-completing-read'.
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
                    org-mono-annotation-format)
            "  "))
          " (new headline)")))))


;; Shamelessly stolen from org-roam-complete-everywhere
(defun org-mono-link-complete ()
  "Complete symbol at point as a link completion to an org headline in current buffer.
  This is a `completion-at-point' function."
  (when (and (thing-at-point 'word)
             (not (org-at-heading-p))
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((table (org-mono--completion-table))
          (bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (org-mono--completion-table)
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

(defun org-mono-completing-read (prompt &optional hash-table require-match)
  "HASH-TABLE is used when the total set of precomputet headlines candidates is not
preferable. See `org-mono--completion-table' how HASH-TABLE is
constructed.
For docs on the rest of the arguments see `completing-read'"
  (let* ((hash-table (or hash-table (org-mono--completion-table)))
         (metadata `(metadata
                     (category . org-mono)
                     (annotation-function . ,(org-mono--annotate
                                              hash-table))))
         (collection (lambda (string predicate action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action hash-table string predicate)))))
    (let ((match (completing-read prompt collection nil require-match)))
      (gethash match hash-table match))))

;; Capture
(defvar org-mono--injected-headline-str nil
  "Used to save candidate match str and inject into org capture templates.")

(defvar org-mono--injected-headline nil
  "Used to save candidate match and inject into org capture templates.")

(defun org-mono-dwim-headline ()
  "Returns the headline string for the last `org-mono-dwim' non-match."
  org-mono--injected-headline-str)

(defun org-mono-capture-parent-stars ()
  (unless org-mono--injected-headline
    (user-error
     "Should only be used after `org-mono-capture-under-headline'"))
  (make-string
   (alist-get :level org-mono--injected-headline)
   ?*))

(defun org-mono-create-file+function (headline)
  "Creates an org-capture-file+function from HEADLINE see
`org-capture-templates'."
  (lambda ()
    (let* ((headline (if (stringp headline) ;; FIX: this should be nicer
                         `((:headline . ,headline)
                           (:file .     ,(car (org-mono--get-files))))
                       headline))
           (marker (org-mono--file-link-to-marker headline t)))
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
`org-mono-capture-dwim-template'."
  (unless org-mono-capture-dwim-template
   (user-error "org-mono-capture-dwim-template is nil, specify default org-mono capture template"))
  (let ((org-capture-templates (list (append
                                      '("a" "")
                                      org-mono-capture-dwim-template))))
    (setq org-mono--injected-headline-str headline-str)
    (org-capture nil "a")))

;; Eldoc integration
(defun org-mono--back-links-in-heading ()
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
                          org-mono--cache))))))

(defun org-mono-eldoc-back-links (callback)
  "This is an `eldoc-documentation-functions' to display backlinks at point.
Note this only work if current file is indexed in cache."
  (let ((back-links (org-mono--back-links-in-heading)))
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
(defun org-mono-archive (headline)
  "Org archive subtree for HEADLINE."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Archive headline: ")))
  (org-mono--with-headline headline
    (org-archive-subtree)))

(defun org-mono-time-stamp (headline)
  "Org time stamp HEADLINE under headline or update if time-stamp found."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Timestamp headline: ")))
  (org-mono--with-headline headline
    (let ((timestamp-marker
           (org-mono--first-timestamp-mark headline)))
      (if timestamp-marker
          (goto-char (marker-position timestamp-marker))
        (end-of-line)
        (newline))
      (org-time-stamp nil)
      (org-indent-line))))

(defun org-mono-capture-under-headline (headline)
  "Capture under HEADLINE."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Entry headline: ")))
  (let ((org-capture-templates
         (mapcar
          (lambda (entry)
            (pcase-let ((`(,keys ,desc ,template . ,properties) entry))
              `(,keys
                ,desc
                plain
                (function ,(org-mono-create-file+function headline))
                ,template
                . ,properties)))
          org-mono-capture-templates)))
    (setq org-mono--injected-headline headline)
    (call-interactively #'org-capture)))

(defun org-mono-goto-headline-child (headline child)
  "Goto CHILD of HEADLINE."
  (interactive
   (list
    (setq match
          (funcall org-mono-completing-read-fn
           "Headline with children: "
           (org-mono--completion-table
            (org-mono--headlines-with-children))
           t))
    (let* ((match-headline (alist-get :headline match))
           (prompt (format "Children for *%s*: " match-headline))
           (table (org-mono--completion-table
                   (org-mono--children-of-headline match))))
      (unless table
        (user-error "No children for *%s*" match-headline))
      (funcall org-mono-completing-read-fn prompt table t))))
  (if child
      (org-mono-goto child)
    (user-error "Unable to derive current headline or child")))

(defun org-mono-goto-back-links (headline internal-link)
  "Goto backlinks for HEADLINE."
  (interactive
   (list
    (setq match
          (funcall org-mono-completing-read-fn
           "Headline: "
           (org-mono--completion-table
            (org-mono--headlines-list-with-filter #'identity :back-links))
           t))
    (let* ((prompt (format "Back links for *%s*: " (alist-get :headline match)))
           (table (org-mono--completion-table
                   (org-mono--list-backlinks match))))
      (unless table
        (user-error "No back links for *%s*"  (alist-get :headline match)))
      (funcall org-mono-completing-read-fn prompt table t))))
  (if internal-link
      (org-mono-goto internal-link)
    (user-error "Unable to derive current headline or missing internal links")))

(defun org-mono-delete-headline (headline)
  "Delete HEADLINE."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Delete headline: ")))
  (when (yes-or-no-p
         (format "Do you really want to delete headline *%s* and its contents"
                 (alist-get :headline headline)))
    (org-mono--with-headline headline
      (org-narrow-to-subtree)
      (delete-region (point-min) (point-max)))))

(defun org-mono-goto (headline)
  "Goto HEADLINE."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Goto headline: " nil t)))
  (let ((marker (org-mono--file-link-to-marker headline)))
    (switch-to-buffer (marker-buffer marker))
    (widen)
    (goto-char (marker-position marker))
    (org-show-subtree)
    (when org-mono-narrow-after-goto
      (org-narrow-to-subtree))))

(defun org-mono-todo (headline)
  "Cycle todo for HEADLINE."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "Todo headline: " nil t)))
  (org-mono--with-headline headline
    (org-todo)))

(defun org-mono-dwim (headline)
  "Dwim HEADLINE. Goto if match otherwise use capture under HEADLINE's name
with `nameorg-mono-capture-default'."
  (interactive
   (list
    (funcall org-mono-completing-read-fn "DWIM headline: ")))
  (cond ((listp headline) (org-mono-goto headline))
        ((stringp headline) (org-headline--dwim-capture headline))))'

;; Mode
(defvar org-mono-mode nil)

;;;###autoload
(define-minor-mode org-mono-mode
  "Mode to index org headlines and gather metadata for said headlines.
This mode also enables completion at point and eldoc documentation."
  :init-value nil
  :lighter " org-mono"
  :group 'org-mono
  (cond
   (org-mono-mode
    (add-hook 'post-command-hook #'org-mono--schedule-cache-timer t t)
    (add-hook 'after-change-functions #'org-mono--schedule-cache-timer t t)
    (add-hook 'eldoc-documentation-functions #'org-mono-eldoc-back-links nil t)
    (add-hook 'completion-at-point-functions #'org-mono-link-complete 100 t))
   (t
    (remove-hook 'post-command-hook #'org-mono--schedule-cache-timer t)
    (remove-hook 'after-change-functions #'org-mono--schedule-cache-timer t)
    (remove-hook 'eldoc-documentation-functions #'org-mono-eldoc-back-links t)
    (remove-hook 'completion-at-point-functions #'org-mono-link-complete t)
    (when org-mono--cache-timer
      (cancel-timer org-mono--cache-timer)
      (setq org-mono--cache-timer nil)))))

(defun turn-on-headline-cmpl-mode ()
  "If `org-mono-mode' should be enabled for buffer.
Based on `org-mono-all-org-files' or if buffer is specified by
`org-mono-files'."
  (if (and org-mono-all-org-files
           buffer-file-name
           (equal (file-name-extension buffer-file-name)
                  "org"))
      (org-mono-mode)
    (when (member buffer-file-name (org-mono--get-files))
      (org-mono-mode))))

;;;###autoload
(define-globalized-minor-mode global-org-mono-mode
  org-mono-mode
  turn-on-headline-cmpl-mode
  :init-value nil
  (cond
   (global-org-mono-mode
    (add-hook 'org-capture-after-finalize-hook #'org-mono--full-cache t))
   (t
    (remove-hook 'org-capture-after-finalize-hook #'org-mono--full-cache t))))

(provide 'org-mono)


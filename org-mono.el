;;; org-mono.el --- Org mono file -*- lexical-binding: t -*-

(defgroup org-mono nil ""
  :group 'extensions)

(defcustom org-mono-cache-delay 2
  ""
  :type 'integer
  :group 'org-mono)

(defcustom org-mono-headline-level 1
  ""
  :type 'integer
  :group 'org-mono)

(defcustom org-mono-file "~/mono.org"
  ""
  :type 'string
  :group 'org-mono)

(defcustom org-mono-templates '(("t" "Todo" "** TODO %?\n" :unnarrowed t)
                                ("n" "Note" "%?" :unnarrowed t))
  ""
  :type 'list
  :group 'org-mono)

(defcustom org-mono-default-template "n"
  ""
  :type 'string
  :group 'org-mono)

(defcustom org-mono-capture-default-headline-function #'org-mono--capture-default-headline
  ""
  :type 'function
  :group 'org-mono)

(defun org-mono--capture-default-headline ()
  (format-time-string "Journal %Y-%m-%d"))

(defvar org-mono-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'org-mono-capture)
    (define-key map "l" #'org-mono-goto-internal-link-heading)
    (set-keymap-parent map embark-general-map)
    map))

(defvar org-mono-cache-timer nil "Org-mono cache timer object.")
(defvar org-mono-headline-cache nil)
(defvar org-mono-fuzzy-links-and-headlines-cache nil)

(defvar org-mono--headline nil "For internal use only")

(defun org-mono--cache ()
  (when (bound-and-true-p org-mono-mode)
    (setq org-mono-headline-cache (org-mono--list-headlines))
    (setq org-mono-fuzzy-links-and-headlines-cache (org-mono--fuzzy-links-and-headings)))
  (setq org-mono-cache-timer nil))

(defun org-mono--schedule-cache-timer ()
  (or org-mono-cache-timer
      (setq org-mono-cache-timer
            (run-with-timer
             org-mono-cache-delay nil
             #'org-mono--cache))))

(defun org-mono--buffer ()
  (let ((buffer (find-file-noselect org-mono-file)))
    (with-current-buffer buffer
      (unless (bound-and-true-p org-mono-mode)
        (org-mono-mode)))
    buffer))

(defun org-mono--list-headlines (&optional cache headline-regexp)
  (if (and cache org-mono-headline-cache)
      org-mono-headline-cache
    (setq
     org-mono-headline-cache
     (with-current-buffer (org-mono--buffer)
       (let ((headline-regexp (or headline-regexp org-outline-regexp)))
         (save-restriction
           (and (buffer-narrowed-p) (widen))
           (save-excursion
             (goto-char (point-min))
             (let (tbl)
               (while (re-search-forward headline-regexp nil t)
                 ;; Remove the leading asterisk from
                 ;; `org-link-heading-search-string' result.
                 (push (nth 4 (org-heading-components)) tbl))
               tbl))))))))

(defun org-mono--list-all-fuzzy-links (widen-buffer)
  (save-restriction
    (and widen-buffer (buffer-narrowed-p) (widen))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "fuzzy")
          (org-element-property :path link))))))

(defun org-mono--fuzzy-links-and-headings (&optional cache)
  (if (and cache org-mono-fuzzy-links-and-headlines-cache)
      org-mono-fuzzy-links-and-headlines-cache
    (setq
     org-mono-fuzzy-links-and-headlines-cache
     (with-current-buffer (org-mono--buffer)
       (save-restriction
         (widen)
         (apply 'append
                (org-map-entries
                 (lambda ()
                   (save-restriction
                     (org-narrow-to-subtree)
                     (let ((title (nth 4 (org-heading-components)))
                           (fuzzy-links (org-mono--list-all-fuzzy-links nil)))
                       (mapcar (lambda (link)
                                 (cons link title))
                               fuzzy-links)))))))))))

(defun org-mono--narrow-to-subtree-top-level ()
  (interactive)
  (save-excursion
    (condition-case err
        (outline-up-heading 9999)
      (error nil))
    (org-narrow-to-subtree)))

(defun org-mono-capture-template-new-or-edit (&optional headline)
  (add-hook 'org-capture-mode-hook
            'org-mono--narrow-to-subtree-top-level t t)
  ;; Store buffers initial state
  (org-capture-put-target-region-and-position)
  (let ((headline (or headline
                      org-mono--headline
                      (org-mono--consult-read-heading))))
    (beginning-of-buffer)
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote headline))
                           nil t)
        (let* ((_ (beginning-of-line))
               (beg (point)))
          (setq-local org-narrow-to-subtree-var beg)
          (org-forward-heading-same-level 1 t)
          (when (= beg (point)) (end-of-buffer)))
      (let ((beg (point-max)))
        (goto-char beg)
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (goto-char (point-max))))))

(defun org-mono-interal-links-for-heading (heading &optional cache)
  (seq-filter (lambda (link-heading)
                (string= heading
                         (car link-heading)))
              (org-mono--fuzzy-links-and-headings cache)))

(defun org-mono-current-heading ()
  (when (and (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (nth 4 (org-heading-components))))


(defun org-mono--fuzzy-link-to-marker (fuzzy-link &optional create)
  (with-current-buffer (find-file-noselect org-mono-file)
    (org-with-wide-buffer
     (let ((org-link-search-must-match-exact-headline (or (not create) 'query-to-create)))
       (ignore-errors (org-link-search fuzzy-link)))
     (point-marker))))

;; Completion at point

;; Shamelessly stolen from org-roam-complete-everywhere
(defun org-mono-headline-link-complete ()
  "Complete symbol at point as a link completion to an org headline in current buffer.
  This is a `completion-at-point' function"
  (when (and (thing-at-point 'word)
             (not (org-at-heading-p))
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (org-mono--list-headlines t)
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert "[[" str "]]"))
            ;; Proceed with the next completion function if the returned titles
            ;; do not match. This allows the default Org capfs or custom capfs
            ;; of lower priority to run.
            :exclusive 'no))))

;; Eldoc integration

(defun org-mono--internal-links-in-heading ()
  (when-let ((heading (org-mono-current-heading)))
    (let* ((heading (nth 4 (org-heading-components)))
           (headlines (org-mono-interal-links-for-heading heading t)))
      (mapcar #'cdr headlines))))

(defun org-mono-eldoc-internal-links (callback)
  (let ((internal-links (org-mono--internal-links-in-heading)))
    (funcall
     callback
     (when internal-links
       (org-fontify-like-in-org-mode
        (format "Internal links: %s"
                (string-join
                 (mapcar (lambda (heading)
                           (format "[[%s]]" heading))
                         (delete-dups internal-links))
                 ", ")))))))

;; Consult / completing-read

(defun consult-org-mono--headline-preview (cand _)
  (when cand
    (ignore-errors
      (consult--jump-nomark (org-mono--fuzzy-link-to-marker cand))
      (org-narrow-to-subtree)
      (org-show-subtree))))

(defun org-mono--annotate-headline (headline)
  (let* ((links-headlines (org-mono-interal-links-for-heading headline t))
         (links (mapcar #'cdr links-headlines))
         (links (string-join links ", ")))
    (concat (propertize " " 'display '(space :align-to center))
            links)))

(defun org-mono--consult-read-heading (&optional prompt default headlines)
  (let ((headlines (or headlines (org-mono--list-headlines))))
  (with-current-buffer (find-file-noselect org-mono-file)
    (save-excursion
      (save-restriction
        (consult--read
         (if default
             (cons default headlines)
           headlines)
         :prompt (or prompt "Heading: ")
         :require-match nil
         :category 'org-mono-headline
         :default default
         :annotate #'org-mono--annotate-headline
         :state #'consult-org-mono--headline-preview))))))

;; Commands

;; TODO
(defun org-mono-goto-internal-link-heading (heading)
  (interactive
   (list
    (when-let ((heading (org-mono-current-heading)))
      (let* ((prompt (format "Internal links for *%s*: " heading))
             (links-heading (org-mono-interal-links-for-heading heading t))
             (headings (mapcar #'cdr links-heading)))
        (when headings
          (completing-read prompt headings nil t))))))
  (if heading
      (org-link-open-from-string (format "[[%s]]" heading))
    (user-error "Unable to derive current heading or missing internal links")))

(defun org-mono-goto-heading (heading)
  (interactive
   (list
    (org-mono--consult-read-heading "Headline: ")))
  (switch-to-buffer (find-file-noselect org-mono-file))
  (goto-char (org-mono--fuzzy-link-to-marker heading))
  (org-narrow-to-subtree))

(defun org-mono-capture (headline)
  (interactive
   (list
    (org-mono--consult-read-heading
     "Capture headline: "
     (funcall org-mono-capture-default-headline-function))))
  (let ((org-mono--headline headline))
    (org-capture)))

;;;###autoload
(defun org-mono-dwim (headline)
  (interactive
   (list
    (org-mono--consult-read-heading
     "Headline: "
     (funcall org-mono-capture-default-headline-function))))
  (let ((org-mono--headline headline))
    (cond ((string-empty-p headline)
           (org-capture nil org-mono-default-empty-template))
          ((not (member headline (org-mono--list-headlines)))
           (org-capture nil org-mono-default-template))
          (t (org-mono-goto-heading headline)))))

;; Mode and init

(defun org-mono--init-templates ()
  (seq-do
   (lambda (template)
     (pcase-let ((`(,key ,desc . ,rest) template))
       (add-to-list 'org-capture-templates
                    (append
                     (list key
                           desc
                           'plain
                           '(file+function
                             org-mono-file
                             org-mono-capture-template-new-or-edit))
                     rest))))
   org-mono-templates))

;;;###autoload
(define-minor-mode org-mono-mode
  "" :lighter " org-mono"
  (if (string= buffer-file-name (expand-file-name org-mono-file))
      (cond
       (org-mono-mode
        (add-hook 'post-command-hook #'org-mono--schedule-cache-timer nil t)
        (add-hook 'eldoc-documentation-functions #'org-mono-eldoc-internal-links nil t)
        (add-hook 'completion-at-point-functions #'org-mono-headline-link-complete 100 t))
       (t
        (remove-hook 'post-command-hook #'org-mono--schedule-cache-timer t)
        (remove-hook 'eldoc-documentation-functions #'org-mono-eldoc-internal-links t)
        (remove-hook 'completion-at-point-functions #'org-mono-headline-link-complete t)
        (when org-mono-cache-timer
          (cancel-timer org-mono-cache-timer)
          (setq org-mono-cache-timer nil))))
    (setq org-mono-mode nil)
    (user-error "Org mono should only be active in file specified by org-mono-file")))

;;;###autoload
(defun org-mono-init ()
  (interactive)
  ;; Initialize org templates
  (org-mono--init-templates)
  (add-to-list 'embark-keymap-alist '(org-mono-headline . org-mono-action-map))
  (bury-buffer (org-mono--buffer)))


(provide 'org-mono)

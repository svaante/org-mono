;;; org-mono-test.el --- Test org-mono.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'org-mono)
(require 'ert)

(defun org-file-1 ()
  (let ((file (make-temp-file "test1" nil ".org")))
    (with-temp-file file
      (insert "* H1")
      (newline)
      (insert "[[*H2][desc fuzzy link]]")
      (newline)
      (insert "** H1.1")
      (newline)
      (insert "<2022-03-28 Mon>")
      (newline)
      (insert "*** H1.1.1")
      (newline)
      (insert "* TODO H2")
      (newline)
      (insert "* H3")
      (newline)
      (insert "[[file:test2.org::*h1][desc file link]]")
      (newline)
      (insert "[[*H2][desc fuzzy link]]")
      (newline))
    file))

(defun org-file-2 (link-file)
  (let ((file (make-temp-file "test2" nil ".org")))
    (with-temp-file file
      (insert "* h1")
      (newline)
      (insert "[[*h2][desc fuzzy link]]")
      (newline)
      (insert "** h1.1")
      (newline)
      (insert "* TODO h2")
      (newline)
      (insert "* h3")
      (newline)
      (insert (format "[[file:%s::*H2][desc file link]]" link-file))
      (newline)
      (insert "[[*h2][desc fuzzy link]]")
      (newline))
    file))

(defun get-component-from-name (components name)
  (car (seq-filter (lambda (hc)
                     (equal name
                            (alist-get :headline hc)))
                   components)))

(defun get-buffer-default-directory (buffer)
  (with-current-buffer buffer
    default-directory))

(ert-deftest basic-level-filter-test ()
  (let* ((file (org-file-1))
         (buffer (find-file-noselect file))
         (org-mono-headline-level 1)
         (headlines-components
          (org-mono--headlines-components buffer)))
    (should (equal
             (get-component-from-name headlines-components "H1")
             `((:level . 1)
               (:todo . nil)
               (:prio . nil)
               (:headline . "H1")
               (:tags . nil)
               (:file-links . (((:file . ,(buffer-file-name buffer))
                                (:headline . "H2"))))
               (:back-links . nil)
               (:timestamp . nil)
               (:parents . nil)
               (:file . ,(buffer-file-name buffer)))))
    (should (equal
             (get-component-from-name headlines-components "H1.1")
             nil))))

(ert-deftest full-cache-test ()
  (let* ((file1 (org-file-1))
         (buffer1 (find-file-noselect file1))
         (file2 (org-file-2 file1))
         (org-mono-files (list file1 file2)))
    (org-mono--full-cache)
    (should (find-buffer-visiting file1))
    (should (not (find-buffer-visiting file2)))
    (should (equal (hash-table-keys org-mono--cache)
                   `(,file1 ,file2)))
    (should (equal
             (mapcar (lambda (components)
                       (alist-get :headline components))
                     (gethash file1
                              org-mono--cache))
             '("H1" "H1.1" "H1.1.1" "H2" "H3")))
    (should (equal
             (mapcar (lambda (components)
                       (alist-get :headline components))
                     (gethash file2
                              org-mono--cache))
             '("h1" "h1.1" "h2" "h3")))
    (let ((backlinks (alist-get :back-links
                        (get-component-from-name (gethash file1
                                                          org-mono--cache)
                                                 "H2"))))
      (should (member
               `((:file . ,file2)
                 (:headline . ,"h3"))
               backlinks))
      (should (member
               `((:file . ,file1)
                 (:headline . ,"H1"))
               backlinks))
      (should (member
               `((:file . ,file1)
                 (:headline . ,"H3"))
               backlinks)))))

(ert-deftest file-links-to-marker-test ()
  (let* ((file (org-file-1))
         (buffer (find-file-noselect file))
         (headlines-components
          (org-mono--headlines-components buffer))
         (h3-components 
          (get-component-from-name headlines-components "H1"))
         (file-link (car
                     (seq-filter (lambda (file-link)
                                   (equal (buffer-file-name buffer)
                                          (alist-get :file file-link)))
                                 (alist-get :file-links h3-components))))
         (marker (org-mono--file-link-to-marker file-link)))
  (should (equal
           (marker-buffer marker)
           buffer))
  (with-current-buffer buffer
    (re-search-forward "\\* TODO H2")
    (should (equal
             (match-beginning 0)
             (marker-position marker))))))

(ert-deftest basic-components-generation-test ()
  (let* ((file (org-file-1))
         (buffer (find-file-noselect file))
         (headlines-components
          (org-mono--headlines-components buffer)))
    (should (equal
             (get-component-from-name headlines-components "H1")
             `((:level . 1)
               (:todo . nil)
               (:prio . nil)
               (:headline . "H1")
               (:tags . nil)
               (:file-links . (((:file . ,(buffer-file-name buffer))
                                (:headline . "H2"))))
               (:back-links . nil)
               (:timestamp . nil)
               (:parents . nil)
               (:file . ,(buffer-file-name buffer)))))
    (should (equal
             (get-component-from-name headlines-components "H1.1")
             `((:level . 2)
               (:todo . nil)
               (:prio . nil)
               (:headline . "H1.1")
               (:tags . nil)
               (:file-links . nil)
               (:back-links . nil)
               (:timestamp . "<2022-03-28 Mon>")
               (:parents . ("H1"))
               (:file . ,(buffer-file-name buffer)))))
    (should (equal
             (get-component-from-name headlines-components "H1.1.1")
             `((:level . 3)
               (:todo . nil)
               (:prio . nil)
               (:headline . "H1.1.1")
               (:tags . nil)
               (:file-links . nil)
               (:back-links . nil)
               (:timestamp . nil)
               (:parents . ("H1.1" "H1"))
               (:file . ,(buffer-file-name buffer)))))
    (should (equal
             (get-component-from-name headlines-components "H2")
             `((:level . 1)
               (:todo . "TODO")
               (:prio . nil)
               (:headline . "H2")
               (:tags . nil)
               (:file-links . nil)
               (:back-links . nil)
               (:timestamp . nil)
               (:parents . nil)
               (:file . ,(buffer-file-name buffer)))))
    (should (equal
             (get-component-from-name headlines-components "H3")
             `((:level . 1)
               (:todo . nil)
               (:prio . nil)
               (:headline . "H3")
               (:tags . nil)
               (:file-links . (((:file . ,(concat
                                           (get-buffer-default-directory buffer)
                                           "test2.org"))
                                (:headline . "h1"))
                               ((:file . ,(buffer-file-name buffer))
                                (:headline . "H2"))))
               (:back-links . nil)
               (:timestamp . nil)
               (:parents . nil)
               (:file . ,(buffer-file-name buffer)))))))

(ert-deftest timestamp-to-marker-test ()
  (let* ((file (org-file-1))
         (buffer (find-file-noselect file))
         (components `((:headline . "H1.1")
                       (:file . ,(buffer-file-name buffer))))
         (timestamp-mark (org-mono--first-timestamp-mark
                          components)))
    (should timestamp-mark)
    (should (equal
             buffer
             (marker-buffer timestamp-mark)))
    (should (equal
             39
             (marker-position timestamp-mark)))
    (org-mono-goto components)
    (should (equal
             "<2022-03-28 Mon>"
             (org-mono--get-timestamp-at-headline)))

    (org-mono-goto `((:headline . "H1")
                     (:file . ,(buffer-file-name buffer))))
    (should (equal
             nil
             (org-mono--get-timestamp-at-headline)))))


(ert-deftest headlines-with-children-test ()
  (let* ((file (org-file-1))
         (buffer (find-file-noselect file))
         (headlines-components
          (org-mono--headlines-components buffer))
         ;; Populate the cache
         (_ (clrhash org-mono--cache))
         (_ (puthash "somefile" headlines-components org-mono--cache))
         (headlines-with-children (org-mono--headlines-with-children)))
    (should (equal
             '("H1" "H1.1")
             (mapcar (lambda (comp)
                       (alist-get :headline comp))
                     headlines-with-children)))))

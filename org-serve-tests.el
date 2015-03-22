(require 'org-serve)

(defconst org-serve-test-org-directory (expand-file-name "./orgs"))

(defun org-serve-test-setup-files (file-data)
  (mkdir org-serve-test-org-directory t)
  (let ((default-directory org-serve-test-org-directory))
    (mapcar (lambda (file)
              (let ((name (expand-file-name (concat org-serve-test-org-directory "/" (car file))))
                    (contents (cdr file)))
                (when (file-exists-p name)
                  (message "deleting file %s" name)
                  (delete-file name))
                (save-excursion
                  (find-file name)
                  (delete-region (point-min) (point-max))
                  (insert contents)
                  (save-buffer)
                  (kill-buffer (buffer-name)))))
            file-data)))

(defun org-serve-test-default-file-setup ()
  (org-serve-test-setup-files '(("file1.org" . "#+PROPERTY: ID 6511b2f4-d008-11e4-8406-14109ff1106e
* file1 - Heading 1
  :PROPERTIES:
  :ID:       65131798-d008-11e4-98c2-14109ff1106e
  :END:

* file1 - Heading 2
  :PROPERTIES:
  :ID:       15e8df32-d007-11e4-830d-14109ff1106e
  :END:
")
                                ("file2.org" . "#+PROPERTY: ID 6515c880-d008-11e4-889f-14109ff1106e
* file2 - Heading 1
  :PROPERTIES:
  :ID:       6516c262-d008-11e4-80b3-14109ff1106e
  :END:

* file2 - Heading 2
  :PROPERTIES:
  :ID:       9fecb9b0-d007-11e4-9af5-14109ff1106e
  :END:
"))))


;; (ert-deftest org-serve:org-serve-post-noop ()
;;   (let* ((org-serve-data-dir (expand-file-name "./orgs"))
;;          (post-data '(((id . "6511b2f4-d008-11e4-8406-14109ff1106e")
;;                        (children . (((id . "65131798-d008-11e4-98c2-14109ff1106e"))
;;                                     ((id . "15e8df32-d007-11e4-830d-14109ff1106e")))))
;;                       ((id . "6515c880-d008-11e4-889f-14109ff1106e")
;;                        (children . (((id . "6516c262-d008-11e4-80b3-14109ff1106e"))
;;                                     ((id . "9fecb9b0-d007-11e4-9af5-14109ff1106e"))))))))
;;     (org-serve-test-default-file-setup)
;;     (should (string-equal "TODO" (org-serve-post post-data)))))


(ert-deftest org-serve:org-serve-post-diff-entries-single-insert ()
  (let* ((org-serve-data-dir (expand-file-name "./orgs"))
         (post-data '(((id . "6511b2f4-d008-11e4-8406-14109ff1106e")
                       (children . (((id . "65131798-d008-11e4-98c2-14109ff1106e"))
                                    ((id . "15e8df32-d007-11e4-830d-14109ff1106e"))
                                    ((id . "9dacf69a-8711-4026-9fe3-c0d56587fbc3"))))) ;; new one
                      ((id . "6515c880-d008-11e4-889f-14109ff1106e")
                       (children . (((id . "6516c262-d008-11e4-80b3-14109ff1106e"))
                                    ((id . "9fecb9b0-d007-11e4-9af5-14109ff1106e"))))))))
    (org-serve-test-default-file-setup)
    (should (equal '(((add . "9dacf69a-8711-4026-9fe3-c0d56587fbc3")
                      (after . "15e8df32-d007-11e4-830d-14109ff1106e")
                      (child-of . "6511b2f4-d008-11e4-8406-14109ff1106e")))
                   (org-serve-post-diff-entries (org-serve-data) post-data nil)))))

(ert-deftest org-serve:org-serve-post-diff-entries-multiple-inserts ()
  (let* ((org-serve-data-dir (expand-file-name "./orgs"))
         (post-data '(((id . "6511b2f4-d008-11e4-8406-14109ff1106e")
                       (children . (((id . "65131798-d008-11e4-98c2-14109ff1106e"))
                                    ((id . "15e8df32-d007-11e4-830d-14109ff1106e"))
                                    ((id . "9dacf69a-8711-4026-9fe3-c0d56587fbc3")))))
                      ((id . "6515c880-d008-11e4-889f-14109ff1106e")
                       (children . (((id . "a8fde3d9-d2c8-4c8c-9f29-fa5ac91f13e6"))
                                    ((id . "6516c262-d008-11e4-80b3-14109ff1106e"))
                                    ((id . "cdf2ae73-15aa-4201-9537-aa0fae2ef0bb"))
                                    ((id . "9fecb9b0-d007-11e4-9af5-14109ff1106e"))))))))
    (org-serve-test-default-file-setup)
    (should (equal '(((add . "cdf2ae73-15aa-4201-9537-aa0fae2ef0bb")
                      (after . "6516c262-d008-11e4-80b3-14109ff1106e")
                      (child-of . "6515c880-d008-11e4-889f-14109ff1106e"))
                     ((add . "a8fde3d9-d2c8-4c8c-9f29-fa5ac91f13e6")
                      (after . nil)
                      (child-of . "6515c880-d008-11e4-889f-14109ff1106e"))
                     ((add . "9dacf69a-8711-4026-9fe3-c0d56587fbc3")
                      (after . "15e8df32-d007-11e4-830d-14109ff1106e")
                      (child-of . "6511b2f4-d008-11e4-8406-14109ff1106e")))
                   (org-serve-post-diff-entries (org-serve-data) post-data nil)))))

(defmacro with-org-serve-test-defaults (body)
  `(let ((org-serve-data-dir (expand-file-name "./orgs")))
     (org-serve-test-default-file-setup)
     ,body))

(with-org-serve-test-defaults
 (org-serve

(ert-deftest org-serve:org-serve-apply-changes-single-insert ()
  (with-org-serve-test-defaults
   (let* ((before-data (org-serve-data))
          (changes '(((add . "9dacf69a-8711-4026-9fe3-c0d56587fbc3")
                      (after . "15e8df32-d007-11e4-830d-14109ff1106e")
                      (child-of . "6511b2f4-d008-11e4-8406-14109ff1106e"))))
          (post-data '(((id . "6511b2f4-d008-11e4-8406-14109ff1106e")
                        (children . (((id . "65131798-d008-11e4-98c2-14109ff1106e"))
                                     ((id . "15e8df32-d007-11e4-830d-14109ff1106e"))
                                     ((id . "9dacf69a-8711-4026-9fe3-c0d56587fbc3")
                                      (name . "blubb"))))) ;; new one
                       ((id . "6515c880-d008-11e4-889f-14109ff1106e")
                        (children . (((id . "6516c262-d008-11e4-80b3-14109ff1106e"))
                                     ((id . "9fecb9b0-d007-11e4-9af5-14109ff1106e")))))))
          (a (org-serve-apply-changes changes post-data))
          ;; (b (progn (org-serve-goto "15e8df32-d007-11e4-830d-14109ff1106e")
          ;;           (insert "MAGIC")))
          ;; (b (progn (org-serve-goto "6511b2f4-d008-11e4-8406-14109ff1106e")
          ;;           (insert "MAGIC")))
          (after-data (org-serve-data)))
     (message "should check after data."))))

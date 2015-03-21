;; commands:
;; - list
;; - post (need to include entire structure, ids only)

(require 'websocket)
(require 'uuidgen)
(require 'json)

(defconst org-serve-port 3034)
(defconst org-serve-data-dir "/tmp/orgs")
(defconst org-serve-org-suffix ".org")
(defconst org-serve-server
  (websocket-server org-serve-port
                    :on-open 'org-serve-handle-open
                    :on-message 'org-serve-handle-message
                    :on-close (lambda (&rest args) (message "[os] Closed connection."))
                    :on-error (lambda (websocket type error)
                                (message "[os] Error: [%s] type [%s] on [%s]." error type websocket))))

;; (websocket-server-close org-serve-server)

(defun org-serve-org-file-p (filename)
  (string-match (concat "^[^.].*" (regexp-quote org-serve-org-suffix) "$") filename))

(defun org-serve-generate-uuid ()
  (uuidgen-1))

(defun org-serve-find-top-level-files ()
  (remove-if-not 'org-serve-org-file-p
                 (directory-files org-serve-data-dir)))

(defun org-serve-find-top-level-entries ()
  (mapcar (lambda (filename) (substring filename 0 (- (length filename) (length org-serve-org-suffix))))
          (org-serve-find-top-level-files)))

(defun org-serve-full-file-name (file)
  (expand-file-name (concat org-serve-data-dir "/" file)))

(defun org-serve-ensure-file-id (filename)
  (save-excursion
    (let* ((file (find-file-noselect (org-serve-full-file-name filename)))
           (contents (with-current-buffer file (buffer-substring-no-properties (point-min) (point-max))))
           (file-id (with-temp-buffer
                      (insert contents)
                      (goto-char (point-min))
                      (re-search-forward "^\*" (point-max) t)
                      (beginning-of-line)
                      (delete-region (point) (point-max))
                      (goto-char (point-min))
                      (when (re-search-forward "#\\+PROPERTY:[[:space:]]+ID[[:space:]]+\\(.\\{8\\}-.\\{4\\}-.\\{4\\}-.\\{4\\}-.\\{12\\}\\)" (point-max) t)
                        (buffer-substring-no-properties (- (point) 36) (point))))))
      (if file-id file-id
        (let ((new-id (org-serve-generate-uuid)))
          (with-current-buffer file
            (goto-char (point-min))
            (insert (concat "#+PROPERTY: ID " new-id "\n")))
          new-id)))))

(defun org-serve-ensure-entry-id (headline)
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (let ((entry-id (org-entry-get (point) "ID" nil)))
      (if entry-id entry-id
        (let ((new-id (org-serve-generate-uuid)))
          (org-set-property "ID" new-id)
          new-id)))))

(defun org-serve-list-entry (headline)
  (let ((entry-id (org-serve-ensure-entry-id headline))
        (name (org-element-property :raw-value headline))
        (children (mapcar 'org-serve-list-entry
                          (remove-if-not (lambda (el) (eq 'headline (car el)))
                                         (org-element-contents headline)))))
    `((id . ,entry-id)
      (name . ,name)
      (children . ,children))))

(defun org-serve-list-file-entries (file)
  (with-current-buffer (find-file-noselect (org-serve-full-file-name file))
    (org-element-map
        (org-element-parse-buffer)
        'headline
      'org-serve-list-entry
      nil
      nil
      'headline)))

(defun org-serve-list-file (file)
  (let ((file-id (org-serve-ensure-file-id file))
        (name (substring file 0 (- (length file) (length org-serve-org-suffix))))
        (children (org-serve-list-file-entries file)))
    `((id . ,file-id)
      (name . ,name)
      (children . ,children))))

;; (org-serve-list-file "sample.org")

(defun org-serve-data ()
  (let* ((files (org-serve-find-top-level-files))
         (data (mapcar 'org-serve-list-file files)))
    data))

(defun org-serve-list (in-response-to)
  (let* ((message-id (org-serve-generate-uuid))
         (data (org-serve-data)))
    (json-encode
     `((id . ,message-id)
       (in-response-to . ,in-response-to)
       (data . ,data)))))

(defun org-serve-handle-open (websocket)
  (message "[os] Open connection [%s]" websocket))

(defun org-serve-error-unknown-command (in-response-to command)
  (json-encode `(id ,(org-serve-generate-uuid) in-response-to ,in-response-to error ,(format "Unknown command: %s" command))))

(defun org-serve-error-invalid-message (payload)
  (json-encode `(id ,(org-serve-generate-uuid) error ,(format "Invalid message: %s" payload))))

(defun org-serve-handle-command (payload)
  (let ((command (cdr (assoc 'command payload)))
        (id (cdr (assoc 'id payload)))
        (data (append (cdr (assoc 'data payload)) nil)))
    (cond ((string-equal "list" command)
           (org-serve-list id))
          ((string-equal "post" command)
           (org-serve-post data))
          ;; TODO: delete
          ;; TODO: sync
          (t
           (org-serve-error-unknown-command id command)))))

(defun org-serve-file-for-id (id)
  (let* ((all-files (org-serve-find-top-level-files))
         (files-with-ids (mapcar (lambda (f)
                                   (cons (org-serve-ensure-file-id f) f))
                                 all-files))
         (found-file (cdr (assoc id files-with-ids))))
    found-file))

(defun org-serve-vector-to-list (vector) (append vector nil))

(defun org-serve-post-diff-entries (old new child-of)
  (let ((rest-old old)
        (rest-new new)
        result
        insert-after)
    (while (or rest-old rest-new)
      (let* ((old-current (car rest-old))
             (old-id (cdr (assoc 'id old-current)))
             (new-current (car rest-new))
             (new-id (cdr (assoc 'id new-current))))
        (cond ((string-equal old-id new-id)
               (message "Found equal ids, nothing to do: [%s]" old-id)
               (setq insert-after old-id)
               (setq result (append (org-serve-post-diff-entries
                                     (org-serve-vector-to-list (cdr (assoc 'children old-current)))
                                     (org-serve-vector-to-list (cdr (assoc 'children new-current)))
                                     old-id)
                                    result))
               (setq rest-old (cdr rest-old)
                     rest-new (cdr rest-new)))
              ((not new-current)
               (message "No more entries on the new side. Maybe we should delete stuff in the future")
               (setq rest-old nil))
              (t
               (message "Found non-equal ids new-id [%s] old-id [%s]" new-id old-id)
               (cond
                ;; still at the beginning
                ((= (length old) (length rest-old))
                 (setq result (cons `((add . ,new-id)
                                      (after . ,nil)
                                      (child-of . ,child-of)) result))
                 (setq rest-old rest-old
                       rest-new (cdr rest-new)))

                ;; at the end of old ids
                ((not old-id)
                 (setq result (cons `((add . ,new-id)
                                      (after . ,(cdr (assoc 'id (car (last old)))))
                                      (child-of . ,child-of)) result))
                 (setq rest-old rest-old
                       rest-new (cdr rest-new)))

                ;; default to last proper id
                (t
                 (setq result (cons `((add . ,new-id)
                                      (after . ,insert-after)
                                      (child-of . ,child-of)) result))
                 (setq rest-old rest-old
                       rest-new (cdr rest-new)))
               )))))
    (remove-if-not 'identity result)))


(defun org-serve-post (post-data)
  ;; find existing structure, and add where appropriate
  ;; no op if all ids exist
  (let* ((stored-data (org-serve-data)))
    (message "diff-result: %s" (org-serve-post-diff-entries stored-data post-data nil))
    "helo"))

(defun org-serve-handle-message (websocket frame)
  (let* ((payload (condition-case condition
                      (json-read-from-string (websocket-frame-payload frame))
                    ('json-readtable-error nil))))
    (message "[os] Received message [%s]" payload)
    (websocket-send-text websocket (if payload
                                       (let ((response (org-serve-handle-command payload)))
                                         (message "[os] sending response [%s]" response)
                                         response)
                                     (org-serve-error-invalid-message payload)))))

;; (org-id-find-id-file "7f69e14a-cf9e-11e4-80d1-14109ff1106e")
;; (org-id-find-id-file "7f6acb32-cf9e-11e4-9beb-14109ff1106e")
;; (gethash "7f6acb32-cf9e-11e4-9beb-14109ff1106e" org-id-locations)

(provide 'org-serve)


(require 'websocket)
(require 'uuidgen)

(defconst org-serve-port 3034)
(defconst org-serve-data-dir nil)
(defconst org-serve-server
      (websocket-server org-serve-port
			:on-open 'org-serve-handle-open
			:on-message 'org-serve-handle-message
			:on-close (lambda (&rest args) (message "Closed connection."))
			:on-error (lambda (websocket type error)
				    (message "[os] Error: [%s] type [%s] on [%s]" error type websocket))
			))

(delete-process org-serve-server)
(setq org-serve-data-dir (expand-file-name "/tmp/data"))
(setq uuidgen-suppress-network-info-warnings t)

(defconst org-serve-org-suffix ".org")

(defun org-serve-is-org-file (filename)
  (string-match (concat ".+" (regexp-quote org-serve-org-suffix) "$") filename))

(defun org-serve-find-top-level-files ()
  (remove-if-not 'org-serve-is-org-file
		 (directory-files org-serve-data-dir)))

(defun org-serve-find-top-level-entries ()
  (mapcar (lambda (filename) (substring filename 0 (- (length filename) (length org-serve-org-suffix))))
	  (org-serve-find-top-level-files)))


(defun org-serve-list (in-response-to)
  (let ((id (uuidgen-4))
	(data (mapcar (lambda (entry) `(:id ,(uuidgen-4) :name ,entry))
		      (org-serve-find-top-level-entries))))
    (json-encode
     `(:id ,id :in-response-to ,in-response-to :data ,data))))

(defun org-serve-handle-open (websocket)
  (message "[os] Open connection [%s]" websocket))

(defun org-serve-error-unknown-command (in-response-to command)
  (json-encode `(:id ,(uuidgen-4) :in-response-to ,in-response-to :error ,(format "Unknown command: %s" command))))

(defun org-serve-error-invalid-message (payload)
  (json-encode `(:id ,(uuidgen-4) :error ,(format "Invalid message: %s" payload))))

(defun org-serve-handle-command (payload)
  (let ((command (cdr (assoc 'command payload)))
	(id (cdr (assoc 'id payload))))
    (cond ((string-equal "list" command)
	   (org-serve-list id))
	  (t
	   (org-serve-error-unknown-command id command)))))

(defun org-serve-handle-message (websocket frame)
  (let* ((payload (condition-case condition
		      (json-read-from-string (websocket-frame-payload frame))
		    ('json-readtable-error nil))))
    (message "[os] Received message [%s]" payload)
    (websocket-send-text websocket (if payload
				       (org-serve-handle-command payload)
				     (org-serve-error-invalid-message payload)))))



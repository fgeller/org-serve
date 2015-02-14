(require 'websocket)
(require 'uuidgen)

(defconst org-serve-port 3034)
(defconst org-serve-data-dir nil)
(defconst org-serve-org-suffix ".org")
(defconst org-serve-server
      (websocket-server org-serve-port
			:on-open 'org-serve-handle-open
			:on-message 'org-serve-handle-message
			:on-close (lambda (&rest args) (message "Closed connection."))
			:on-error (lambda (websocket type error)
				    (message "[os] Error: [%s] type [%s] on [%s]" error type websocket))
			))

(defun org-serve-is-org-file (filename)
  (string-match (concat "^[^.].*" (regexp-quote org-serve-org-suffix) "$") filename))

(defun org-serve-find-top-level-files ()
  (remove-if-not 'org-serve-is-org-file
		 (directory-files org-serve-data-dir)))

(defun org-serve-find-top-level-entries ()
  (mapcar (lambda (filename) (substring filename 0 (- (length filename) (length org-serve-org-suffix))))
	  (org-serve-find-top-level-files)))

(defun org-serve-ensure-file-id (filename)
  (save-excursion
    (let* ((file (find-file-noselect (expand-file-name (concat org-serve-data-dir "/" filename))))
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
	(let ((new-id (uuidgen-1)))
	  (with-current-buffer file
	    (goto-char (point-min))
	    (insert (concat "#+PROPERTY: ID " new-id "\n")))
	  new-id)))))

(defun org-serve-list (in-response-to)
  (let* ((message-id (uuidgen-4))
	 (files (org-serve-find-top-level-files))
	 (data (mapcar (lambda (file)
			 (let ((file-id (org-serve-ensure-file-id file))
			       (name (substring file 0 (- (length file) (length org-serve-org-suffix)))))
			   `(:id ,file-id :name ,name)))
		       files)))
    (json-encode
     `(:id ,message-id :in-response-to ,in-response-to :data ,data))))

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

(provide 'org-serve)


(require 'websocket)

(defconst org-serve-server nil)
(defconst org-serve-port 3034)
(defconst org-serve-data-dir nil)

(defun org-serve-handle-open (websocket)
  (message "[os] Open connection [%s]" websocket))

(defconst org-serve-sample-message
  (json-encode '(:id "e70a2403-5b27-42b7-b6e7-35b3b82afa2b"
		 :in-response-to "b2a3284c-744a-4874-a48d-6b3ba8dfcc4d"
		 :data ((:id "3d86371e-12d7-40a1-8729-29bdbc00d9e4" :name "Tasks" :children ((:id "597d534e-c16f-4dbb-9be9-0ee106149225" :name "Hack on org-web")))
			(:id "4e754dae-1749-4813-9bad-25fc3c8509f2" :name "Shopping")
			(:id "f2361112-c195-4730-ba25-d88dc716f51c" :name "Bookmarks")))))

(defun org-serve-handle-message (websocket frame)
  (let ((payload (websocket-frame-payload frame)))
    (message "[os] Received message [%s]" payload)
    (websocket-send-text websocket org-serve-sample-message)))

(delete-process org-serve-server)
(setq org-serve-server
      (websocket-server org-serve-port
			:on-open 'org-serve-handle-open
			:on-message 'org-serve-handle-message
			:on-close (lambda (&rest args) (message "Closed connection."))
			:on-error (lambda (websocket type error)
				    (message "[os] Error: [%s] type [%s] on [%s]" error type websocket))
			))

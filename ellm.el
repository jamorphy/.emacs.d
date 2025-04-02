;;; -*- lexical-binding: t -*-

(defun llm-new-chat ()
  "Create a new chat buffer with a prompt ready."
  (interactive)
  (let ((buf (generate-new-buffer "*llm-chat*")))
    (switch-to-buffer buf)
    (text-mode)
    (insert "\n<You>: ")
    (goto-char (line-beginning-position))))

(defun llm-stream (buffer prompt)
  "Send PROMPT to BUFFER and stream the response from the LLM server."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (save-excursion
        ;; If there's a prompt, append it after the last <You>:
        (when (not (string-empty-p prompt))
          (goto-char (point-max))
          (if (re-search-backward "^<You>: " nil t)
              (progn
                (end-of-line)
                (insert " " prompt))
            (goto-char (point-max))
            (insert (concat "\n<You>: " prompt)))))))
  (let ((buffer-content (with-current-buffer buffer (buffer-string))))
    (if (string-empty-p buffer-content)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n<Error>: Buffer is empty")))
      (let ((proc (condition-case err
                      (make-network-process :name "llm"
                                            :buffer buffer
                                            :host "localhost"
                                            :service 9999
                                            :family 'ipv4)
                    (error
                     (message "Failed to connect to LLM server: %s" (error-message-string err))
                     nil))))
        (if proc
            (progn
              (process-send-string proc buffer-content)
              (process-send-eof proc)
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "\n\n<Assistant>: ")))
              (set-process-filter proc
                                  (lambda (proc string)
                                    (when (buffer-live-p (process-buffer proc))
                                      (with-current-buffer (process-buffer proc)
                                        (let ((inhibit-read-only t))
                                          (save-excursion
                                            (goto-char (point-max))
                                            (insert string)))))))
              (set-process-sentinel proc
                                    (lambda (proc event)
                                      (when (buffer-live-p (process-buffer proc))
                                        (with-current-buffer (process-buffer proc)
                                          (let ((inhibit-read-only t))
                                            (save-excursion
                                              (goto-char (point-max))
                                              (insert "\n\n<You>: "))))
                                        ;;(message "LLM response for %s: %s" (buffer-name (process-buffer proc)) event)
                                        ))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\n\n<Error>: Could not connect to server at localhost:9999"))))))))

(defun llm-use-region-as-multiprompt ()
  "Send the selected region as a prompt to all *llm-chat* buffers."
  (interactive)
  (if (use-region-p)
      (let* ((prompt (buffer-substring-no-properties (region-beginning) (region-end)))
             (chat-buffers (seq-filter (lambda (buf)
                                         (string-match-p "\\*llm-chat\\*" (buffer-name buf)))
                                       (buffer-list))))
        (if chat-buffers
            (progn
              ;;(message "Sending prompt '%s' to %d chat buffers" prompt (length chat-buffers))
              (dolist (buf chat-buffers)
                (llm-stream buf prompt)))
          (message "No *llm-chat* buffers found")))
    (message "No region selected; please select text to use as a prompt")))

(defun llm-choose-chats-for-multiprompt ()
  "Send the selected region as a prompt to chosen *llm-chat* buffers."
  (interactive)
  (if (use-region-p)
      (let* ((prompt (buffer-substring-no-properties (region-beginning) (region-end)))
             (chat-buffers (seq-filter (lambda (buf)
                                         (string-match-p "\\*llm-chat\\*" (buffer-name buf)))
                                       (buffer-list))))
        (if chat-buffers
            (let* ((buffer-names (mapcar #'buffer-name chat-buffers))
                   (selected (completing-read-multiple "Select chat buffers: " buffer-names)))
              (dolist (buf-name selected)
                (let ((buf (get-buffer buf-name)))
                  (when buf
                    (llm-stream buf prompt)))))
          (message "No *llm-chat* buffers found")))
    (message "No region selected; please select text to use as a prompt")))

(global-set-key (kbd "C-c C-c") (lambda ()
                                  (interactive)
                                  (llm-stream (current-buffer) "")))

(global-set-key (kbd "C-c C-l") 'llm-new-chat)

;;; -*- lexical-binding: t -*-

(defun llm-fetch-models-to-kill-ring ()
  "Fetch available models from the LLM server and add the selected one to the kill ring."
  (interactive)
  (let ((buf (get-buffer-create "*llm-temp*")))
    (with-current-buffer buf
      (erase-buffer)
      (let ((proc (condition-case err
                      (make-network-process :name "llm-models"
                                            :buffer buf
                                            :host "localhost"
                                            :service 9999
                                            :family 'ipv4)
                    (error
                     (message "Failed to connect to LLM server: %s" (error-message-string err))
                     nil))))
        (if (not proc)
            (message "Could not connect to server at localhost:9999")
          (process-send-string proc "list-models\n")
          (process-send-eof proc)
          (set-process-filter proc
                              (lambda (proc string)
                                (when (buffer-live-p (process-buffer proc))
                                  (with-current-buffer (process-buffer proc)
                                    (insert string)))))
          (accept-process-output proc 5)  ;; Wait up to 5 seconds
          (while (process-live-p proc)
            (accept-process-output proc 1))
          (goto-char (point-min))
          (let ((models (split-string (buffer-string) "\n" t)))
            (kill-buffer buf)
            (if (not models)
                (message "No models received from server")
              (let ((selected-model (completing-read "Select model to copy: " models nil t)))
                (kill-new selected-model)
                (message "Model '%s' added to kill ring" selected-model)))))))))

;; Optional keybinding
(global-set-key (kbd "C-c C-m") 'llm-fetch-models-to-kill-ring)

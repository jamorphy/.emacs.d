;;; .emacs
;;; Commentary
;;; Code:

(when (version<  emacs-version "30.0")
  (error "Config not tested on v%s. Please use v30.0 or higher." emacs-version))

(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "Terminus (TTF)-16")
        (vertical-scroll-bars . nil)))

(setq backup-directory-alist '(("." . "~/.emacsbackups")))

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
(package-initialize)

(when (daemonp)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

(setenv "LIBRARY_PATH" "/opt/homebrew/lib")


(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ;; Prevent double-loading packages
  (package-initialize))

(defvar pkg-refreshed nil)

(defun pm/use (package)
  (when (not pkg-refreshed)
    (setq pkg-refreshed t)
    (package-refresh-contents))
  (when (not (package-installed-p package))
   (package-install package)))

;;;
;;; Defaults
;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(column-number-mode)
(global-display-line-numbers-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(fringe-mode '(10 . 0))
(global-hl-line-mode)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      scroll-conservatively 100
      display-time-day-and-date t
      package-enable-at-startup nil
      confirm-kill-emacs 'yes-or-no-p
      switch-to-buffer-obey-display-actions t)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              tab-width 4)

;;
;; Local packages
;;
(add-to-list 'load-path "~/.emacs.d/custom/")

(pm/use 'base16-theme)
(load-theme 'base16-zenburn t)

(setq native-comp-async-report-warnings-errors 'silent)

(setopt display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;
;;; OS Specific
;;;
(defvar cfg-loc "")
(defvar dump-loc "")
(set-frame-font "Terminus (TTF) 16" nil t)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-12")
  (setq cfg-loc "C:/Users/ja/AppData/Roaming/.emacs.d/init.el")
  (setq default-directory "C:/Workspace/"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq cfg-loc "~/.emacs.d/init.el")
  (setq dump-loc "~/Workspace/sync/dump.org"))
  
(when (eq system-type 'gnu/linux)
  (setq cfg-loc "~/.emacs.d/init.el")
  (set-face-attribute 'default nil :font "Hack-12"))

;;;
;;; Window and Layout Handling
;;;
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c f") 'switch-to-next-buffer)
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-`"))
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))


(defun save-layout-delete-other-windows ()
  "Save the current layout and focus on current window."
  (interactive)
  (window-configuration-to-register ?-)
  (delete-other-windows))

(defun goto-saved-layout ()
  (interactive)
  "Go to saved window layout"
  (jump-to-register ?-))

(global-set-key (kbd "C-;") 'switch-to-buffer)
(global-set-key (kbd "C-x 1") 'save-layout-delete-other-windows)
(global-set-key (kbd "C-c w") 'goto-saved-layout)
(global-unset-key (kbd "C-x `"))
(global-unset-key (kbd "C-z"))

;;;
;;; Editing
;;;
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(pm/use 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(pm/use 'move-text)
(move-text-default-bindings)

(pm/use 'expand-region)
(global-set-key (kbd "M-;") 'er/expand-region)

;;;
;;; Programming Modes
;;;
(setq python-indent-offset 4)
(pm/use 'yaml-mode)
(pm/use 'json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 'js-tabwidth)))

(setq c-set-style "k&r")
(setq c-basic-offset 4)

(pm/use 'magit)

;;;
;;; Projects
;;; TODO

;;;
;;; Compilation
;;;
(global-set-key (kbd "C-c r") (lambda () (interactive) (recompile)))
(global-set-key (kbd "C-c `") 'next-error)

;;;
;;; Misc
;;;
(defun cfg ()
  "Open the `.emacs` or `init.el`."
  (interactive)
  (find-file cfg-loc))
(defun dump ()
  "Open dump.org"
  (interactive)
  (find-file dump-loc))

;;;
;;; UI
;;;
(pm/use 'icomplete-vertical)
(fido-mode t)
(icomplete-vertical-mode t)

(pm/use 'which-key)
(which-key-mode)

(global-set-key (kbd "M-i") 'imenu)
(setq imenu-auto-rescan t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a12f585b1ff1b35f4000eab71f9f20a784b94f797296de13d467f9b3021b9a8b"
     "e338de851db9cb260207b8b7246761585e79a489c7750110e01c10e216af495f"
     "76c92281dc2f878bce2ab4b4466f76afda48c1ed95dfb3a97da31df39b21491d"
     "882d6a5981fd85d9f987d31623e25e69b8d5635a53ab442f1a51e7c252790320"
     "308ead333d5dfc7d64258a9d3d46f9e479e4b62bdf33a8ea8700423842cae32e"
     "6ed98f47da7556a8ce6280346e5d8e1e25bede71dc5186aa2654b93bec42d2a6"
     "1f82b7df8a4ce0e2ba3b0217d2c552b2fe5b5dd1244481cb65bef733f9818c63"
     "d8b8c09a745470f6c088dce5df19ade98894f4ced69ce32d53aded94d512826d"
     "623e9fe0532cc3a0bb31929b991a16f72fad7ad9148ba2dc81e395cd70afc744"
     "e8915dac580af7a4a57bd38a0638a5a7714b47714529b1a66bd19e7aa155818d"
     default))
 '(dape-key-prefix [24 1])
 '(eglot-ignored-server-capabilities
   '(:hoverProvider :completionProvider :signatureHelpProvider
                    :codeActionProvider :codeLensProvider
                    :documentFormattingProvider
                    :documentRangeFormattingProvider
                    :documentOnTypeFormattingProvider
                    :documentLinkProvider :foldingRangeProvider
                    :inlayHintProvider))
 '(package-selected-packages
   '(amx base16-theme bufferlo dap-mode expand-region go-mode gptel
         icomplete-vertical json-mode llm magit markdown-mode
         move-text multiple-cursors request twittering-mode
         vs-light-theme which-key whiteboard yaml-mode zig-mode))
 '(persp-modestring-short t)
 '(persp-show-modestring t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; LSP
;;
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(add-hook 'prog-mode-hook 'eglot-ensure)

;;
;; Org
;;
(load-file "~/.emacs.d/om.el")

(pm/use 'bufferlo)

;;
;; Debug
;;

(pm/use 'dape)
(setq dape-buffer-window-arrangement 'right)
(setq dape-key-prefix (kbd "C-x C-a"))

;;
;; LLMs
;;

(load-file "~/.emacs.d/secrets.el")

(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))



(pm/use 'llm)

(setq llm-warn-on-nonfree nil)

(defvar openai-provider (make-llm-openai :key openai-api-key :chat-model "gpt-4o-mini"))
(defvar groq-provider (make-llm-openai-compatible :url "https://api.groq.com/openai/v1" :key groq-api-key :chat-model "llama-3.1-70b-versatile"))
(defvar mistral-provider (make-llm-openai-compatible :url "https://api.mistral.ai/v1" :key mistral-api-key :chat-model "mistral-large-2407"))
(defvar deepseek-provider (make-llm-openai-compatible :url "https://api.deepseek.com" :key deepseek-api-key :chat-model "deepseek-coder"))

(defun llm-quick (provider)
  "Process the selected region, current line, or prompt with the given LLM PROVIDER and stream the response."
  (interactive (list my-llm-provider))  ; Default to my-llm-provider when called interactively
  (let (begin end input-text)
    (cond
     ;; If there's an active region, use it
     ((use-region-p)
      (setq begin (region-beginning)
            end (region-end)
            input-text (buffer-substring-no-properties begin end)))
     
     ;; If the current line is non-empty, use it
     ((not (string-empty-p (string-trim (thing-at-point 'line t))))
      (setq begin (line-beginning-position)
            end (line-end-position)
            input-text (string-trim (buffer-substring-no-properties begin end))))
     
     ;; If the current line is empty, prompt for input
     (t
      (setq input-text (read-string "Enter text to process: "))
      (setq begin (point) end (point))))

    (when input-text
      (when (use-region-p)
        (delete-region begin end))
      (save-excursion
        (goto-char end)
        (insert "\n\n")
        (llm-chat-streaming-to-point 
         provider
         (llm-make-chat-prompt input-text)
         (current-buffer)
         (point)
         (lambda () 
           (message "LLM response complete")))))))

(global-set-key (kbd "C-c m") (lambda () (interactive) (llm-quick groq-provider)))

(defvar-local llm-chat-buffer-provider nil
  "The LLM provider for this chat buffer.")

(defvar-local llm-chat-history nil
  "The chat history for this buffer.")

(defvar llm-available-providers
  '(("OpenAI" . openai-provider)
    ("Llama3.1 70b" . groq-provider)
    ("deepseek" . deepseek-provider)
    ("mistral" . mistral-provider))
  "Alist of available providers and their corresponding variable names.")

(define-derived-mode llm-chat-mode text-mode "LLM Chat"
  "Major mode for LLM chat buffers."
  (setq-local llm-chat-history (llm-make-chat-prompt ""))
  (setq-local llm-chat-buffer-provider nil))

(defun llm-chat-select-provider ()
  "Prompt the user to select a provider and return a cons of (provider-name . provider-object)."
  (let* ((provider-name (completing-read "Select provider: "
                                         (mapcar #'car llm-available-providers)
                                         nil t))
         (provider-var (cdr (assoc provider-name llm-available-providers))))
    (cons provider-name (symbol-value provider-var))))

(defun llm-new-chat ()
  "Create a new chat buffer and prompt for the provider."
  (interactive)
  (let* ((provider-info (llm-chat-select-provider))
         (provider-name (car provider-info))
         (provider (cdr provider-info))
         (chat-buffer (generate-new-buffer (format "*JaLLM chat: %s*" provider-name))))
    (with-current-buffer chat-buffer
      (llm-chat-mode)
      (setq-local llm-chat-buffer-provider provider)
      (insert (format "llm interface (%s), C-c n to send msg, winnable?\n\n" provider-name))
      (insert "USER: "))
    (switch-to-buffer chat-buffer)))

(defvar-local llm-chat-response-start nil
  "Marker for the start of the assistant's response in the chat buffer.")



(defun llm-chat-send-message ()
  "Send the message in the input area of the chat buffer, keeping the cursor at the end of the stream."
  (interactive)
  (let* ((provider llm-chat-buffer-provider)
         (last-user-prompt (save-excursion
                             (goto-char (point-max))
                             (search-backward "USER: " nil t)
                             (point)))
         (input (buffer-substring-no-properties 
                 (+ last-user-prompt (length "USER: "))
                 (point-max))))
    (when (string-empty-p (string-trim input))
      (user-error "Message is empty"))
    
    ;; Append user message to history
    (llm-chat-prompt-append-response llm-chat-history input)
    
    ;; Add two newlines after the user's message
    (goto-char (point-max))
    (insert "\n\n")
    
    ;; Prepare buffer for AI response
    (insert "ASSISTANT: ")
    (setq-local llm-chat-response-start (point-marker))
    
    ;; Function to update cursor position
    (defun llm-chat-update-cursor (&rest _)
      (goto-char (point-max))
      (recenter -1))
    
    ;; Add hook to update cursor after each change
    (add-hook 'after-change-functions #'llm-chat-update-cursor nil t)
    
    (llm-chat-streaming-to-point
     provider
     llm-chat-history
     (current-buffer)
     (point-max)
     (lambda ()
       (save-excursion
         (goto-char (point-max))
         ;; Add two newlines after the assistant's response
         (insert "\n\n")
         (insert "USER: ")
         (llm-chat-prompt-append-response 
          llm-chat-history 
          (buffer-substring-no-properties llm-chat-response-start (point-max)) 
          'assistant))
       ;; Move cursor to the next USER: prompt
       (goto-char (point-max))
       ;; Remove the hook after streaming is complete
       (remove-hook 'after-change-functions #'llm-chat-update-cursor t)))))
(define-key llm-chat-mode-map (kbd "C-c n") #'llm-chat-send-message)

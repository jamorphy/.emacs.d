;;; .emacs
;;; Commentary
;;; Code:

(when (version<  emacs-version "30.0")
  (error "Config not tested on v%s. Please use v30.0 or higher." emacs-version))

(setq default-frame-alist
      '((font . "Monaco-13")
        (fullscreen . maximized)
        (vertical-scroll-bars . nil)))

(setq backup-directory-alist '(("." . "~/.emacsbackups")))

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
(package-initialize)

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

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-12")
  (setq cfg-loc "C:/Users/ja/AppData/Roaming/.emacs.d/init.el")
  (setq default-directory "C:/Workspace/"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq cfg-loc "~/.emacs.d/init.el")
  (setq dump-loc "~/Workspace/sync/dump.org"))
;;  (set-face-attribute 'default nil :font "Monaco-14"))

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
   '("1f82b7df8a4ce0e2ba3b0217d2c552b2fe5b5dd1244481cb65bef733f9818c63"
     "d8b8c09a745470f6c088dce5df19ade98894f4ced69ce32d53aded94d512826d"
     "623e9fe0532cc3a0bb31929b991a16f72fad7ad9148ba2dc81e395cd70afc744"
     "e8915dac580af7a4a57bd38a0638a5a7714b47714529b1a66bd19e7aa155818d"
     default))
 '(eglot-ignored-server-capabilities
   '(:hoverProvider :completionProvider :codeActionProvider
                    :documentLinkProvider :foldingRangeProvider))
 '(package-selected-packages
   '(amx base16-theme bufferlo expand-region go-mode gptel
         icomplete-vertical json-mode magit markdown-mode move-text
         multiple-cursors request twittering-mode vs-light-theme
         which-key whiteboard yaml-mode))
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

;; Org
(load-file "~/.emacs.d/om.el")

(pm/use 'bufferlo)

;;
;; LLMs
;;

(load-file "~/.emacs.d/secrets.el")

(require 'ezllm)

(global-set-key (kbd "C-c m") 'ezllm-send)

(setq sysprompt "You are a code generator. Only output valid code based on the prompt or comment. Do not talk at all. Only output valid code. Do not provide any backticks that surround the code. Never ever output backticks like this ```. Other comments should left alone. Do not output backticks. Never output emojis.")

(ezllm-configure-provider :name 'llama-3.1-8b
                          :spec ezllm-openai
                          :endpoint "https://api.groq.com/openai/v1/chat/completions"
                          :model "llama-3.1-8b-instant"
                          :max-tokens 3000
                          :api-key groq-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'llama-3.1-70b
                          :spec ezllm-openai
                          :endpoint "https://api.groq.com/openai/v1/chat/completions"
                          :model "llama-3.1-70b-versatile"
                          :max-tokens 3000
                          :api-key groq-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'mixtral-8x7b
                          :spec ezllm-openai
                          :endpoint "https://api.groq.com/openai/v1/chat/completions"
                          :model "mixtral-8x7b-32768"
                          :max-tokens 2048
                          :api-key groq-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'sonnet-3.5
                          :spec ezllm-anthropic
                          :endpoint "https://api.anthropic.com/v1/messages"
                          :model "claude-3-5-sonnet-20240620"
                          :max-tokens 1024
                          :api-key claude-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'gpt-4-turbo
                          :spec ezllm-openai
                          :endpoint "https://api.openai.com/v1/chat/completions"
                          :model "gpt-4-turbo"
                          :max-tokens 2048
                          :api-key openai-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'gpt-4o-mini
                          :spec ezllm-openai
                          :endpoint "https://api.openai.com/v1/chat/completions"
                          :model "gpt-4o-mini"
                          :max-tokens 2048
                          :api-key openai-api-key
                          :system-prompt "you are a helpful assistant")

(ezllm-configure-provider :name 'mistral-large-2
                          :spec ezllm-openai
                          :endpoint "https://api.mistral.ai/v1/chat/completions"
                          :model "mistral-large-2407"
                          :max-tokens 3500
                          :api-key mistral-api-key
                          :system-prompt sysprompt)

(ezllm-configure-provider :name 'deepseek-coder
                          :spec ezllm-openai
                          :endpoint "https://api.deepseek.com/chat/completions"
                          :model "deepseek-coder"
                          :max-tokens 3000
                          :api-key deepseek-api-key
                          :system-prompt sysprompt)

(ezllm-set-provider 'llama-3.1-70b)

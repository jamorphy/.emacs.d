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

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))
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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-expression (read (current-buffer)))))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

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
(pm/use 'magit-section)

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

(global-set-key (kbd "C-;") 'ido-switch-buffer)


(pm/use 'which-key)
(which-key-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     "f5f070872db3e4d8b82dbb2f3b1c60beca86fc93327a38ebddd22070458a14bc"
     "a12f585b1ff1b35f4000eab71f9f20a784b94f797296de13d467f9b3021b9a8b"
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
 '(dape-key-prefix [24 1] t)
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

(add-to-list 'exec-path "/Users/j/.local/bin")

;;
;; LLMs
;;
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))

(setq straight-repository-branch "develop")

(load-file "~/.emacs.d/ellm.el")
(setq warning-minimum-level :error)

(pm/use 'rg)
(rg-enable-default-bindings)

;; Basic org-roam setup - just the essentials
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Workspace/notes")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))


(defvar afk-timer nil
  "Timer used to update the AFK buffer.")

(defvar afk-buffer nil
  "Buffer used to display AFK status.")

(defun afk ()
  "Display an AFK buffer with ASCII art and a timer."
  (interactive)
  ;; Cancel any existing timer
  (when afk-timer
    (cancel-timer afk-timer)
    (setq afk-timer nil))
  
  ;; Create new buffer
  (setq afk-buffer (get-buffer-create "*AFK*"))
  
  (with-current-buffer afk-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "
                                                                              
                                        db         88888888888  88      a8P   
                                       d88b        88           88    ,88'    
                                      d8'`8b       88           88  ,88\"      
                                     d8'  `8b      88aaaaa      88,d88'       
                                    d8YaaaaY8b     88\"\"\"\"\"      8888\"88,      
                                   d8\"\"\"\"\"\"\"\"8b    88           88P   Y8b     
                                  d8'        `8b   88           88     \"88,   
                                 d8'          `8b  88           88       Y8b  
                                                                              
                                                                              
")
      (insert "Time away: 0s\n"))
    
    ;; Set up buffer properties
    (read-only-mode 1)
    (use-local-map (make-sparse-keymap))
    (local-set-key "q" 'afk-quit))
  
  ;; Set up the timer to update every second
  (setq afk-timer 
        (run-at-time t 1 'afk-update-timer))
  
  ;; Display the buffer
  (switch-to-buffer afk-buffer))

(defun afk-quit ()
  "Quit the AFK mode and clean up."
  (interactive)
  (when afk-timer 
    (cancel-timer afk-timer)
    (setq afk-timer nil))
  (when (buffer-live-p afk-buffer)
    (kill-buffer afk-buffer)))

(defun afk-update-timer ()
  "Update the timer in the AFK buffer."
  (when (buffer-live-p afk-buffer)
    (with-current-buffer afk-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (if (re-search-forward "Time away: \\([0-9]+\\)s" nil t)
            (let ((seconds (1+ (string-to-number (match-string 1)))))
              (replace-match (format "Time away: %ds" seconds)))
          ;; If pattern not found, reset the buffer
          (erase-buffer)
          (insert "
                                                                              
                                        db         88888888888  88      a8P   
                                       d88b        88           88    ,88'    
                                      d8'`8b       88           88  ,88\"      
                                     d8'  `8b      88aaaaa      88,d88'       
                                    d8YaaaaY8b     88\"\"\"\"\"      8888\"88,      
                                   d8\"\"\"\"\"\"\"\"8b    88           88P   Y8b     
                                  d8'        `8b   88           88     \"88,   
                                 d8'          `8b  88           88       Y8b  
                                                                              
                                                                              
")
          (insert "Time away: 0s\n"))))))

(provide 'afk)

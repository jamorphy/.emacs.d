;;; .emacs
;;; Commentary
;;; Code:

(when (version<  emacs-version "26.3")
  (error "Config not tested on v%s. Please use v26.3 or higher." emacs-version))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq backup-directory-alist '(("." . "~/.emacsbackups")))

(package-initialize)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

(pm/use 'base16-theme)
(load-theme 'base16-zenburn t)

;; silence native comp warnings buffer
(setq native-comp-async-report-warnings-errors 'silent)

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
  (setq dump-loc "~/Workspace/sync/dump.org")
  (set-face-attribute 'default nil :font "Monaco-14"))

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
;;(global-unset-key (kbd "M-o M-o"))
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

(global-set-key (kbd "C-.") 'switch-to-buffer)
(global-set-key (kbd "C-x 1") 'save-layout-delete-other-windows)
(global-set-key (kbd "C-c w") 'goto-saved-layout)                

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

(global-unset-key (kbd "C-x `"))
(global-unset-key (kbd "C-z"))

;;;
;;; UI
;;;
(pm/use 'icomplete-vertical)
(fido-mode t)
(icomplete-vertical-mode t)

(pm/use 'which-key)
(which-key-mode)

(pm/use 'perspective)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c x"))
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(persp-mode)

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
 '(package-selected-packages
   '(amx base16-theme expand-region go-mode gptel icomplete-vertical
         json-mode magit markdown-mode move-text multiple-cursors
         perspective request twittering-mode vs-light-theme which-key
         whiteboard yaml-mode))
 '(persp-modestring-short t)
 '(persp-show-modestring t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(require 'org-annotation-helper)

;; LSP
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)


;; Org
;; TODO: move to another file
(setq org-html-preamble t)
(setq org-html-preamble
      (concat
       "<div style='display: flex; justify-content: center'>"
       "<ul id='banner'>"
       "<li> <a href='../../index.html'><< back</a></li>"
       "</ul>"
       "</div>"
       "<style>"
      "#banner { margin:0; list-style-type: none }"
      "#banner li {"
      "  float: left;"
      "}"
      "#banner li a, #banner li a:visited {"
        "display: block;"
        "padding: 10px;"
        "color: #07a;"
        "text-decoration: none;"
      "}"
      "</style><br>"))

(setq org-html-postamble t)
(setq org-html-postamble-format '(("en" "<p class='date'>Last updated: %C</p>"))
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      )

(setq org-html-html5-fancy t
      org-html-doctype "html5")

;; LLMs
(pm/use 'gptel)
(setq gptel-api-key "")
(gptel-make-gemini "Gemini" :key "" :models '("gemini-1.5-flash" "gemini-pro") :stream t)

(gptel-make-openai "Groq"               ;Any name you want
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key ""
  :models '("mixtral-8x7b-32768"
            "llama3-70b-8192"
            "llama3-8b-8192"))

;; Disable scroll bar on frame
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))


(defun my/set-default-font ()
  (set-face-attribute 'default nil :font "Monaco-14"))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/set-default-font))))


(defun launch-alt-frame ()
  "Launch a new frame with a 'whiteboard'-like appearance."
  (interactive)
  (let ((new-frame (make-frame)))
    (select-frame new-frame)
    ;; Set frame-specific face attributes to mimic the 'whiteboard' theme
    (set-face-attribute 'default new-frame :background "white" :foreground "black")
    (set-face-attribute 'fringe new-frame :background "white")
    (set-face-attribute 'mode-line new-frame :background "grey75" :foreground "black" :box nil)
    (set-face-attribute 'mode-line-inactive new-frame :background "grey90" :foreground "black" :box nil)
    ;; Line numbers
    (set-face-attribute 'line-number new-frame :background "white" :foreground "gray")
    (set-face-attribute 'line-number-current-line new-frame :background "white" :foreground "black")
    ;; Highlight current line
    (set-face-attribute 'hl-line new-frame :background "lightgrey")
    ;; Additional face settings can be added here to further mimic the 'whiteboard' theme
    ))

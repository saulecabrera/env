;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Config use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 140
                    :weight 'normal
                    :width 'normal)
;; Disable bold
(set-face-bold-p 'bold nil)

;; Line numbers
(global-display-line-numbers-mode 1)
;; Column number mode
(setq column-number-mode t)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
	doom-gruvbox-dark-variant "hard") ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; Vim
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Magit
(use-package magit
             :ensure t)

;; Git
(use-package git-gutter
  :init (global-git-gutter-mode t))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil
	      lsp-headerline-breadcrumb-enable nil)
  (evil-collection-define-key 'normal 'lsp-mode-map
    "gd" 'lsp-find-definition
    "gr" 'lsp-find-references))
														   
(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :config (setq lsp-ui-sideline-enable t
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-doc-show-with-cursor t
		lsp-ui-show-with-mouse t)
  :ensure t
  :init (add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Inconsolata" :height 140))))

;; Rust
(use-package rustic
  :ensure t
  :config (setq rustic-lsp-client 'lsp-mode)
         (setq rustic-analyzer-command '("/Users/saulecabrera/.nix-profile/bin/rust-analyzer")))

;; Leader configuration
(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config (evil-leader/set-key												      
	    ;; Magit
	    "m"  'magit-status
	    ;; Config file
	    "c"  'open-config-file
	    "e"  'open-env
	    ;; Windows
	    "wo"  'other-window
	    "wv"  'split-window-horizontally
	    "ws"  'split-window-vertically
	    "wl"  'windmove-right
	    "wh"  'windmove-left
	    "wj"  'windmove-down
	    "wk"  'windmove-up
	    ;; Projectile
	    "ff"  'projectile-find-file
	    "f/"  'projectile-ripgrep
	    "pp"  'projectile-switch-project
	    ;; Perspective
	    "wb"  'persp-list-buffers
	    "ww"  'persp-switch
	    "wn"  'persp-next
	    "wp"  'persp-prev
	    "wx"  'persp-kill
	    "wy"  'persp-scratch-buffer
	    ;; General
	    "fs"  'save-buffer)

  (evil-leader/set-leader "<SPC>"))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (evil-collection-define-key 'normal 'projectile-mode-map
    "-" 'dired-jump))

(use-package helm
  :init (helm-mode 1)
  :config (global-set-key (kbd "M-x") 'helm-M-x))

(use-package helm-projectile
  :init (helm-projectile-on))

;; ripgrep
(use-package ripgrep
  :ensure t)

(use-package helm-rg
  :ensure t
  :after helm)

;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package projectile-ripgrep
  :after projectile
  :config (evil-collection-ripgrep-setup)
  :ensure t)

;; Company mode
(use-package company
  :init (global-company-mode)
  :config (global-set-key (kbd "<tab>") #'company-indent-or-complete-common))

;; Diagnostics
(use-package flycheck
  :ensure t)

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode))

(use-package hl-todo
  :init (global-hl-todo-mode)
  :ensure t)

(use-package evil-easymotion
  :init
  (evilem-default-keybindings "SPC")
  :config
  (evilem-define (kbd "SPC gw") 'evil-forward-word-begin)
  :ensure t)

(use-package dired
  :ensure nil
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package web-mode
  :config
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package graphql-mode
  :ensure t)

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode))

(use-package persp-projectile
  :ensure t
  :init (require 'persp-projectile))
  
;; Set meta key to Cmd
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Fast access to the config file
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-env ()
  (interactive)
  (dired "~/Developer/env"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

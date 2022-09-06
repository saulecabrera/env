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
                    :family "Berkeley Mono"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Theme
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-hard t)
  :config (setq doom-gruvbox-dark-variant "hard"))

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
  :commands lsp)

(use-package lsp-ui
  :ensure t)

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
	    ;; LSP bindings
	    "gd" 'lsp-find-definition
	    "gr" 'lsp-find-references
	    "K"  'lsp-describe-thing-at-point
	    ;; Magit
	    "m"  'magit-status
	    ;; Config file
	    "c"  'open-config-file
	    "e"  'open-env
	    ;; Windows
	    "ww"  'other-window
	    "wv"  'split-window-vertically
	    "ws"  'split-window-horizontally
	    ;; Projectile
	    "ff"  'projectile-find-file
	    "fs"  'projectile-grep
	    "pp"  'projectile-switch-project)
  (evil-leader/set-leader "<SPC>"))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; Projectile
(use-package projectile
  :ensure t
  :config (projectile-mode +1))

(use-package helm
  :init (helm-mode 1))

(use-package helm-projectile
  :init (helm-projectile-on))

;; Company mode
(use-package company
  :init (global-company-mode)
  :config (global-set-key (kbd "<tab>") #'company-indent-or-complete-common))

;; Fast access to the config file
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-env ()
  (interactive)
  (dired "~/Developer/env"))
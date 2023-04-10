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

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq font-family "PragmataPro Mono Liga")
(setq font-size 140)

;; Font
(set-face-attribute 'default nil
                    :family font-family
                    :height font-size
                    :weight 'normal
                    :width 'normal)
;; Line numbers
(setq column-number-mode t)
(global-display-line-numbers-mode 1)

;; Bullets
(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package vterm
  :ensure t)

;; Evil org
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Multiple cursors
(use-package evil-mc
  :config
  (global-evil-mc-mode  1))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material-light))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
	doom-gruvbox-light-variant "soft"
	doom-gruvbox-dark-variant "hard")

  ;; (load-theme 'doom-tomorrow-day)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package yaml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

;; Vim
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Magit
(use-package magit
  :config
  (evil-collection-define-key 'normal 'magit-mode-map
    "C-<tab>" 'magit-section-cycle-diffs)
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package forge
  :ensure t
  :after magit
  :config (setq forge-owned-accounts '(("saulecabrera"))))
(setq auth-sources '("~/.authinfo"))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode))

;; ISLE
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))


;; Debugging C++
(use-package dap-mode
  :ensure t
  :after lsp-mode)

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
    "gr" 'lsp-find-references)

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :config (setq lsp-ui-sideline-enable t
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-doc-show-with-cursor t
		lsp-ui-show-with-mouse t)
  :ensure t
  :init (add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font font-family :height font-size))))

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp))

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
	    "gb"  'magit-blame
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
	    "ff"  'fzf-projectile
	    "f/"  'projectile-ripgrep
	    "fr"  'helm-buffers-list
	    "pp"  'projectile-switch-project
	    ;; Perspective
	    "wb"  'persp-list-buffers
	    "ww"  'persp-switch
	    "wn"  'persp-next
	    "wp"  'persp-prev
	    "wx"  'persp-kill
	    "wy"  'persp-scratch-buffer
	    ;; General
	    "fs"  'save-buffer
	    ;; Roam
	    "rc"  'org-roam-capture
	    "rt"  'org-roam-tag-add
	    "rr"  'org-roam-tag-remove
	    "rf"  'org-roam-node-find
	    ;; Org
	    "oa"  'org-agenda
	    ;; Enhanced search via swoop
	    "s"   'helm-swoop)

  (evil-leader/set-leader "<SPC>"))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
	fzf/executable "fzf"
	fzf/git-grep-args "-i --line-number %s"
	fzf/grep-command "rg --no-heading -nH"
	fzf/position-bottom t
	fzf/window-height 15))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Developer/" "~/src/github.com/"))
  (projectile-mode +1)
  (evil-collection-define-key 'normal 'projectile-mode-map
    "-" 'dired-jump))

(use-package helm
  :init (helm-mode 1)
  :config (global-set-key (kbd "M-x") 'helm-M-x))
(use-package helm-swoop
  :after helm)

(use-package helm-projectile
  :init (helm-projectile-on))

;; ripgrep
(use-package ripgrep
  :config
  (rg-enable-menu)
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
  (evilem-define (kbd "SPC gW") 'evil-backward-word-begin)
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

(use-package rust-playground
  :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode))

(use-package persp-projectile
  :ensure t
  :init (require 'persp-projectile))

(use-package org
  :ensure nil
  :config (setq org-agenda-files (list "~/Developer/org/")
		org-directory (concat (getenv "HOME") "/Developer/org/")
		org-hide-emphasis-markers t
		org-hide-leading-stars t)
  (let* ((variable-tuple
	  (cond ((x-list-fonts "PragmataPro Mono Liga") '(:font "PragmataPro Mono Liga"))
		(nil (warn "Cannot find font for org mode"))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:weight bold :inherit default :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 140))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 140))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 140))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 140))))
     '(org-done ((t (:inherit fixed-pitch))))
     '(org-todo ((t (:inherit fixed-pitch))))
     `(variable-pitch ((t (:family font-family :height 140))))
     `(fixed-pitch ((t ( :family font-family :height 140))))

     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-drawer ((t (:inherit fixed-pitch))) t)
     '(org-document-title ((t (:inherit fixed-pitch))) t)
     '(org-date ((t (:inherit fixed-pitch))) t)
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (fixed-pitch)))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
     '(line-number ((t (:inherit (shadow fixed-pitch)))))
     '(line-number-current-line ((t (:inherit (shadow fixed-pitch)))))
     ))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)) 

;; package meta key to Cmd
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

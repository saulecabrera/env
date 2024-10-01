(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; PACKAGES

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ===========================================================================

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(set-face-attribute 'default nil :font "PragmataPro Mono Liga" :height 120)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-gruvbox-dark-medium t))

(use-package yaml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Vim
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Magit
(use-package magit
  :config
  (evil-collection-define-key 'normal 'magit-mode-map
    "C-<tab>" 'magit-section-cycle-diffs)
  :ensure t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode))

;; ISLE
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))
;; WAT
(add-to-list 'auto-mode-alist '("\\.wat\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.wast\\'" . lisp-mode))


(use-package dumb-jump
  :after evil
  :ensure t)

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

  (add-hook 'c-mode-hook 'lsp))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :config (setq lsp-ui-sideline-enable t
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-doc-show-with-cursor t)
  :ensure t
  :init (add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "PragmataPro Mono Liga" :height 120))))

;; Rust
(use-package rustic
  :ensure t
  :config (setq rustic-lsp-client 'lsp-mode)
          (setq rustic-analyzer-command '("/Users/saulecabrera/.nix-profile/bin/rust-analyzer")))

;; Leader configuration
(use-package evil-leader
  :ensure t
  :after evil
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
	    "pp"  'projectile-switch-project
	    ;; Consult
	    "ff"  'consult-find
	    "f/"  'consult-ripgrep
	    "fg"  'consult-git-grep
	    "fm"  'consult-man
	    "fr"  'consult-project-buffer
	    "s"  'consult-line
	    ;; General
	    "fs"  'save-buffer
	    ;; Dumb jump
	    "jg"  'dumb-jump-go-other-window
	  )

  (evil-leader/set-leader "<SPC>"))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Developer/" "~/src/github.com/"))
  (projectile-mode +1)
  (evil-collection-define-key 'normal 'projectile-mode-map
    "-" 'dired-jump))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; "C-+"
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; Diagnostics
(use-package flycheck
  :ensure t)

(use-package evil-commentary
  :after evil
  :ensure t
  :init (evil-commentary-mode))

(use-package hl-todo
  :init (global-hl-todo-mode)
  :ensure t)

(use-package evil-easymotion
  :after evil
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

(use-package graphql-mode
  :ensure t)

(use-package company
  :config
  (setq company-minimum-prefix-length 1
      company-idle-delay 0.0))


(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  (tide-hl-identifier-mode +1))

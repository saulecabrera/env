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
(setq font-size 150)

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
  (setq org-bullets-bullet-list '("⁖" "⁖" "⁖" "⁖" "⁖"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package vterm
  :ensure t)

(use-package svelte-mode
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :after evil)

;; Evil org
(use-package evil-org
  :ensure t
  :after '(evil org)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Multiple cursors
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode  1))


;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil
	doom-gruvbox-light-variant "medium"
	doom-gruvbox-dark-variant "hard")

  (load-theme 'doom-gruvbox)

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
;; WAT
(add-to-list 'auto-mode-alist '("\\.wat\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.wast\\'" . lisp-mode))


(use-package dumb-jump
  :after evil
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


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
	    "rd"  'org-roam-dailies-capture-today
	    "rx"  'org-roam-dailies-find-date
	    ;; Org
	    "oa"  'org-agenda
	    ;; Vterm
	    "vt"  'vterm-toggle
	    "vc"  'vterm-toggle-cd
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

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package org
  :ensure nil
  :config (setq org-agenda-files (list "~/Developer/org/" "~/Developer/org/daily")
		org-directory (concat (getenv "HOME") "/Developer/org/")
		org-hide-emphasis-markers t
		org-hide-leading-stars t
		org-startup-indented t
		org-indent-mode-turns-on-hiding-stars t
		org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (let* ((variable-tuple
	  (cond ((x-list-fonts "PragmataPro") '(:font "PragmataPro"))
		(nil (warn "Cannot find font for org mode"))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:weight bold :inherit default :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     '(org-level-8 ((t (,@headline ,@variable-tuple))))
     '(org-level-7 ((t (,@headline ,@variable-tuple))))
     '(org-level-6 ((t (,@headline ,@variable-tuple))))
     '(org-level-5 ((t (,@headline ,@variable-tuple))))
     '(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     '(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
     '(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
     '(org-level-1 ((t (,@headline ,@variable-tuple :height 1.1))))
     '(org-done ((t (:inherit fixed-pitch))))
     '(org-todo ((t (:inherit fixed-pitch))))
     '(variable-pitch ((t (:family "PragmataPro" :height 150))))
     '(fixed-pitch ((t (:family "PragmataPro" :height 150))))

     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info ((t (:foreground "dark orange"))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-hide ((t (:inherit fixed-pitch))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
     
     '(line-number ((t (:inherit (shadow fixed-pitch)))))
     '(line-number-current-line ((t (:inherit (shadow fixed-pitch)))))
     )
    (add-hook 'org-mode-hook 'variable-pitch-mode)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
    (add-hook 'org-agenda-mode-hook (lambda () (display-line-numbers-mode 0) (setq truncate-lines 1)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook (lambda ()
			       "Beautify Org Checkbox Symbol"
			       (push '("[ ]" .  "☐") prettify-symbols-alist)
			       (push '("[X]" . "☑" ) prettify-symbols-alist)
			       (push '("[-]" . "❍" ) prettify-symbols-alist)
			       (prettify-symbols-mode))))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)) 

(use-package writeroom-mode
  :ensure t
  :init
  (add-hook 'org-mode-hook 'writeroom-mode)
  (add-hook 'org-agenda-mode-hook 'writeroom-mode)
  :config
  (setq writeroom-width 95)
  :after org)

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

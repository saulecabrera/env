;; Base

;; Windows
(windmove-default-keybindings)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq saul/font-family "PragmataPro Mono Liga"
      saul/font-size                    120
      saul/agenda-files                 (directory-files-recursively (expand-file-name "~/Developer/t/agenda/") "\\.org$")
      saul/org-directory                (expand-file-name "~/Developer/t/agenda")
      saul/notes                        (expand-file-name "~/Developer/t/notes")
      saul/inbox-file                   (concat saul/org-directory "/inbox.org")
      inhibit-startup-message           t      
      inhibit-startup-echo-area-message t      
      inhibit-startup-screen            t      
      initial-buffer-choice             t      
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore
      display-time-default-load-average nil   
      scroll-margin                     0     
      use-dialog-box                    nil
      text-scale-mode-step              1.1)

;; Font
(when (member saul/font-family (font-family-list))
  (set-face-attribute 'default nil :font saul/font-family :height saul/font-size)
  (set-face-attribute 'fixed-pitch nil :family saul/font-family :height saul/font-size))

(when (member saul/font-family (font-family-list))
  (set-face-attribute 'variable-pitch nil :family saul/font-family :height saul/font-size))

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Theme
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t
	doom-gruvbox-dark-variant "medium"
	doom-themes-visual-bell-config t)
  :config
  (load-theme 'doom-gruvbox t))

;; Undo
(use-package undo-fu
  :bind (("C-c u" . undo-fu-only-undo)
	 ("C-c U" . undo-fu-only-redo)))

;; Keybindings (evil + devil)
(use-package devil
  :config
  (global-devil-mode t)
  :bind (("C-," . global-devil-mode)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; Ensure that , is not mapped to anything else
  ;; and use it rather to control devil mode.
  (define-key evil-motion-state-map "," nil)
  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init))

(use-package evil-commentary
  :config (evil-commentary-mode))

;; Git
(use-package magit)

;; Diff highlight
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode))

;; ISLE
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))

;; LSP
(use-package eglot)

;; Eldoc box
(use-package eldoc-box
  :after (eglot))

;; Rust
(use-package rustic
  :after (eglot)
  :init
  (setq rustic-lsp-client 'eglot))

;; Which Key
(use-package which-key
  :config (which-key-mode))

;; Projects
(use-package projectile
  :bind (("C-x p p" . projectile-switch-project))
  :init (setq projectile-project-search-path '("~/Developer/" "~/src/github.com/"))
  :config (projectile-mode +1))

(use-package projectile-ripgrep
  :after (projectile))

;; Perspective
(use-package perspective
  :init (setq persp-suppress-no-prefix-key-warning t)
  :config (persp-mode t))

(use-package persp-projectile
  :after (perspective projectile))

;; Nix
(use-package nix-mode)

;; Vertico
(use-package vertico
  :config (vertico-mode 1)
  :init
  (setq vertico-cycle t
        vertico-resize nil))

;; Marginalia
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Orderless
(use-package orderless
  :init (setq completion-styles '(orderless basic)))

;; Consult
(use-package consult
  :init
  (setq consult-find-args "find .")
  :bind (("C-c f" . consult-find)
	 ("C-c /" . consult-ripgrep)
	 ("C-c b" . consult-buffer)
	 ("M-g f" . consult-flymake)
	 ("M-g s" . consult-line)))
;; Corfu
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-quit-no-match 'separator)
  :config (global-corfu-mode))

;; WAT
(use-package wat-mode)

;; fancy-compilation
(use-package fancy-compilation
  :config (fancy-compilation-mode))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("GPG_TTY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Org
(use-package org
  :init
  (setq org-todo-keywords  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))
	org-refile-targets '((nil :maxlevel . 9)
                             (saul/agenda-files :maxlevel . 9))
	org-directory saul/org-directory
	org-outline-path-complete-in-steps nil
	org-refile-use-outline-path t
	org-clock-persist 'history)
  :config
  (org-clock-persistence-insinuate)
  :bind (("C-c a" . #'org-agenda)
	 ("C-c c" . #'org-capture)))


(use-package org-agenda
  :after (org)
  :init
  (setq org-agenda-skip-timestamp-if-done t
	org-agenda-files saul/agenda-files
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-include-deadlines t
	org-agenda-block-separator nil
	org-agenda-compact-blocks t
	org-agenda-start-day nil ;; i.e. today
	org-agenda-span 1
	org-agenda-start-on-weekday nil
        org-default-notes-file saul/inbox-file))

(use-package org-modern
  :after (org org-agenda)
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(use-package org-super-agenda
  :after (org-agenda)
  :config (org-super-agenda-mode)
  :init
  (setq org-super-agenda-groups
	'((:log t)

         ;; Things to work for the day.
         (:name "Today"
                :time-grid t)

	 ;; Overdue stuff
         (:name "Overdue"
		:face 'error
		:deadline past)

	 ;; Needs attention 
         (:name "Reschedule"
		:face 'warning
		:and (:scheduled past))

	 ;; Deadline, today.
	 (:name "Due today"
		:face 'warning
		:and (:deadline today :scheduled nil))

	 ;; Next items
         (:name "Next"
		:and (:todo "NEXT" :scheduled future :not (:scheduled today) :not (:deadline today))
		:and (:todo "NEXT" :deadline future :not (:scheduled today) :not (:deadline today))))))


;; Shackle
(use-package shackle
  :init
  (setq shackle-rules '((compilation-mode  :select t :align bottom :size 0.9)
                      (magit-status-mode :select t :popup t :align t :size 0.9))
	shackle-default-rule '(:select t))
  :config
  (shackle-mode 1))

;; Denote
(use-package denote
  :init (setq denote-directory (expand-file-name saul/notes)))
  
;; Direnv integration
(use-package direnv
  :config (direnv-mode))

;; avy
(use-package avy
  :bind (("C-:" . avy-goto-char)))

;; expand-region
(use-package expand-region
  :bind (("C-$" . 'er/expand-region)))

;; Mixed pitch mode
(use-package mixed-pitch
  :hook
  ((org-mode . mixed-pitch-mode)
   (org-mode . variable-pitch-mode)))

;; Olivetti mode
(use-package olivetti
  :init (setq olivetti-body-width 85)
  :hook ((org-mode . olivetti-mode)))

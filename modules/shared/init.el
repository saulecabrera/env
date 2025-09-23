;; Theme
(require 'doom-themes)
(setq doom-themes-enable-bold t
    doom-themes-enable-italic t
	    doom-gruvbox-dark-variant "medium"
		doom-themes-visual-bell-config t)
(load-theme 'doom-gruvbox t)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
(setq font-family "PragmataPro Mono Liga")
(setq font-size 120)

(when (member "PragmataPro Mono Liga" (font-family-list))
  (set-face-attribute 'default nil :font font-family :height font-size)
  (set-face-attribute 'fixed-pitch nil :family font-family :height font-size))

(when (member "Liberation Mono" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Liberation Mono" :height 120))

(setq inhibit-startup-message           t       ;; No startup message
      inhibit-startup-echo-area-message t       ;; No startup message in echo area
      inhibit-startup-screen            t       ;; No default startup screen
      initial-buffer-choice             t       ;; *scratch* is default startup buffer
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore ;; No bell
      display-time-default-load-average nil     ;; Don't show me load time
      scroll-margin                     0       ;; Space between top/bottom
      use-dialog-box                    nil)    ;; Disable dialog
;; Decrease the font size, a bit.
(setq text-scale-mode-step 1.1)

(require 'undo-fu)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; Devil mode
(require 'devil)
(global-devil-mode t)
(global-set-key (kbd "C-,") 'global-devil-mode)
;; To ensure that `devil-mode` is correctly
;; loaded at the startup screen.
(advice-add 'display-startup-screen
              :after (lambda (&optional _) (devil-mode 1)))

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Git
(require 'magit)

;; Diff highlight
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-dired-mode)

;; ISLE
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))

;; LSP
(require 'eglot)
;; Eldoc box
(require 'eldoc-box)

(require 'rustic)
(setq rustic-lsp-client 'eglot)

;; Which Key
(require 'which-key)
(which-key-mode)

;; Projects
(require 'projectile)
(require 'projectile-ripgrep)
(setq projectile-project-search-path '("~/Developer/" "~/src/github.com/"))
(projectile-mode +1)
(global-set-key (kbd "C-x p p") 'projectile-switch-project)

;; Perspective
(setq persp-suppress-no-prefix-key-warning t)
(require 'perspective)
(require 'persp-projectile)
(persp-mode)


;; Nix
(require 'nix-mode)

;; Vertico
(require 'vertico)
(setq vertico-cycle t)
(setq vertico-resize nil)
(vertico-mode 1)

;; Marginalia
(require 'marginalia)
(marginalia-mode 1)

;; Orderless
(require 'orderless)
(setq completion-styles '(orderless basic))

;; Consult
(require 'consult)
(global-set-key (kbd "M-s f") 'consult-find)
(global-set-key (kbd "M-s /") 'consult-ripgrep)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-s e") 'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") 'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history)

;; Corfu
(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t
(global-corfu-mode)

;; WAT
(require 'wat-mode)


;; fancy-compilation
(require 'fancy-compilation)
(fancy-compilation-mode)

;; exec-path-from-shell
(require 'exec-path-from-shell)
(dolist (var '("GPG_TTY"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)


;; Org
(setq org-agenda-files
      (directory-files-recursively "~/Developer/t/agenda" org-agenda-file-regexp))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))

(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(require 'org)
(require 'org-agenda)
(require 'org-super-agenda)

(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil

      org-super-agenda-groups
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
		:and (:todo "NEXT" :deadline future :not (:scheduled today) :not (:deadline today)))))

(org-super-agenda-mode)

(with-eval-after-load 'org (global-org-modern-mode))
(with-eval-after-load 'org
    (setq org-directory "~/Developer/t/agenda"))
(with-eval-after-load 'org
  (setq org-default-notes-file (concat org-directory "/inbox.org")))

;; Shackle
(require 'shackle)
(setq shackle-rules '((compilation-mode  :noselect t :align bottom :size 0.9)
                      (magit-status-mode :select t :popup t :align t :size 0.9))
      shackle-default-rule '(:select t))
(shackle-mode 1)

(require 'denote)
(setq denote-directory (expand-file-name "~/Developer/t/notes/"))

;; Ace Window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-dispatch-always t)

;; Direnv integration
(require 'direnv)
(direnv-mode)

;; avy
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-$") 'er/expand-region)

;; Mixed pitch mode
(require 'mixed-pitch)
(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Olivetti mode
(setq olivetti-body-width 85)
(require 'olivetti)
(add-hook 'org-mode-hook 'olivetti-mode)

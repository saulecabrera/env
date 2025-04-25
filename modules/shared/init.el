;; Theme
(require 'doom-themes)
(setq doom-themes-enable-bold t
    doom-themes-enable-italic t
	    doom-gruvbox-dark-variant "hard"
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

(set-face-attribute 'default nil
    :family font-family
    :height font-size
    :weight 'normal
    :width 'normal)

;; Line numbers
(setq column-number-mode t)
(global-display-line-numbers-mode 1)

;; evil
;; must set this to nil before loading `evil`
(setq evil-want-keybinding nil)
(require 'evil)
(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(evil-mode 1)

(require 'evil-collection)
(evil-collection-init)

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ;; Magit
  "m"   'magit-status
  "gb"  'magit-blame
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


;; Git
(require 'magit)
(evil-collection-define-key 'normal 'magit-mode-map
    "C-<tab>" 'magit-section-cycle-diffs)

;; Diff highlight
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-dired-mode)

;; ISLE
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))

(require 'lsp-mode)
(setq lsp-headerline-breadcrumb-enable-diagnostics nil
	    lsp-headerline-breadcrumb-enable nil)

(evil-collection-define-key 'normal 'lsp-mode-map
  "gd" 'lsp-find-definition
  "gr" 'lsp-find-references)

(require 'lsp-ui)
(setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-show-with-mouse t)

;; Rust
(require 'rustic)
(setq rustic-lsp-client 'lsp-mode)

;; Which Key
(require 'which-key)
(which-key-mode)

;; Fuzzy
(require 'fzf)
(setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
	fzf/executable "fzf"
	fzf/git-grep-args "-i --line-number %s"
	fzf/grep-command "rg --no-heading -nH"
	fzf/position-bottom t
	fzf/window-height 15)


;; Projects
(require 'projectile)
(setq projectile-project-search-path '("~/Developer/" "~/src/github.com/"))
(projectile-mode +1)
(evil-collection-define-key 'normal 'projectile-mode-map
  "-" 'dired-jump)

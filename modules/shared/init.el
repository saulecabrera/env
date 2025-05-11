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
(require 'evil-leader)
(require 'evil-collection)
(require 'evil-easymotion)
(require 'evil-commentary)

(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(evil-mode 1)

(evil-collection-init)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ;; Buffer
  "q"  'kill-buffer-and-window
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
  ;; Project
  "ff"  'consult-fd
  "f/"  'consult-ripgrep
  "fr"  'consult-buffer
  "pp"  'projectile-switch-project
  "pc"  'projectile-compile-project
  "ps"  'project-shell
  "pd"  'flymake-show-project-diagnostics

  ;; Cargo
  ;; TODO: Load only when in Rust mode?
  "cf"  'rustic-cargo-fmt
  ;; Perspective
  "wb"  'persp-list-buffers
  "ww"  'persp-switch
  "wn"  'persp-next
  "wp"  'persp-prev
  "wx"  'persp-kill
  "wy"  'persp-scratch-buffer
  ;; Avy
  "jc"  'avy-goto-char
  "jl"  'avy-goto-line
  ;; Enhanced search via swoop
  "s"   'consult-line)

(evil-global-set-key 'normal "fs" 'save-buffer)

(evilem-default-keybindings "SPC")

(evil-commentary-mode)

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

;; LSP
(require 'eglot)

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
(evil-collection-define-key 'normal 'projectile-mode-map
  "-" 'dired-jump)
(evil-collection-ripgrep-setup)

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

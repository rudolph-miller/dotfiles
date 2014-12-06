(require 'cask "~/.cask/cask.el")
(cask-initialize)

(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "cyan")))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; emacs directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(auto-install-compatibility-setup)

;; package management
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
    (package-refresh-contents))
  (unless (package-installed-p package)
    (package-install package)))

;; install evil
(package-install-with-refresh 'evil)
(package-install-with-refresh 'color-theme)

;; enable evil
(require 'evil)
(evil-mode 1)
(setq evil-shift-width 1)

(require 'color-theme)

(global-set-key "\C-h" 'backward-delete-char)

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'previous-line)
(define-key evil-insert-state-map (kbd "C-j") 'next-line)

(define-key evil-normal-state-map ")" 'paredit-forward-up)
(define-key evil-normal-state-map "(" 'paredit-backward-up)

;; clone slime in .emacs.d/
;; `git clone https://github.com/slime/slime`
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime/"))
(require 'auto-install)
(require 'slime)
(setq inferior-lisp-program "ros run -Q")
(slime-setup '(slime-repl slime-fancy slime-banner))
(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/dev//HyperSpec/"))
      common-lisp-hyperspec-symbol-table
      (expand-file-name "~/dev/HyperSpec/Data/Map_Sym.txt"))
(add-hook 'lisp-mode-hook
          (lambda ()
            (slime-mode t)
            (show-paren-mode)))
(add-hook 'lisp-mode-hook
          (lambda ()
            (global-set-key "\C-ch" 'slime-hyperspec-lookup)))

(setq mac-pass-command-to-system nil)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; Apropos
(push '("*slime-apropos*") popwin:special-display-config)
;; Macroexpand
(push '("*slime-macroexpansion*") popwin:special-display-config)
;; Help
(push '("*slime-description*") popwin:special-display-config)
;; Compilation
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;; Cross-reference
(push '("*slime-xref*") popwin:special-display-config)
;; Debugger
(push '(sldb-mode :stick t) popwin:special-display-config)
;; REPL
(push '(slime-repl-mode) popwin:special-display-config)
;; Connections
(push '(slime-connection-list-mode) popwin:special-display-config)

;; ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'lisp-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interacton-mode-hook 'enable-paredit-mode)

(require 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)

(require 'magit)

(add-to-list 'auto-mode-alist
	     '("\\.tmpl\\'" . html-mode))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

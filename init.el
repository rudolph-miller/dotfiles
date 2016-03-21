;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cask


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(load-theme 'manoj-dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((((class color) (min-colors 89)) (:foreground "#767676" :background "#d7ff00"))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#a1db00" :bold nil)))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-language-environment "Japanese")
(icomplete-mode 1)
(global-auto-revert-mode 1)
(display-time-mode 1)
(column-number-mode 1)
(display-battery-mode 1)
(setq mac-pass-command-to-system nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-q") 'helm-mini)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(evil-mode 1)
(setq evil-shift-width 2)

(setq require-final-newline t)

(global-set-key "\C-h" 'backward-delete-char)
(define-key key-translation-map [?\C-h] [?\C-?])

(define-key evil-normal-state-map ")" 'sp-up-sexp)
(define-key evil-normal-state-map "(" 'sp-down-sexp)

(defun set-pretty-patterns (patterns)
  (loop for (glyph . pairs) in patterns do
        (loop for (regexp . major-modes) in pairs do
              (loop for major-mode in major-modes do
                    (let ((major-mode (intern (concat (symbol-name major-mode) "-mode")))
                          (n (if (string-match "\\\\([^?]" regexp) 1 0)))
                      (font-lock-add-keywords major-mode
                                              `((,regexp (0 (prog1 ()
                                                              (compose-region (match-beginning ,n)
                                                                              (match-end ,n)
                                                                              ,glyph)))))))))))

;;(set-pretty-patterns
;; '((?λ ("\\<lambda\\>" lisp lisp-interaction emacs-lisp scheme))
;;   (?λ ("\\<function\\>" js2))))

;;(add-hook 'slime-mode-hook
;;          (lambda ()
;;            (load-library "cl-indent")
;;            (setq lisp-indent-function 'common-lisp-indent-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime

(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime-repl-ansi-color/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/cl-annot/misc/"))

(require 'slime-annot)

(setq inferior-lisp-program "ros run -Q -s")
;; (setq inferior-lisp-program "ccl")
;; (setq inferior-lisp-program "clisp")

(slime-setup '(slime-repl slime-fancy slime-banner slime-repl-ansi-color slime-indentation))

(add-hook 'lisp-mode-hook
          (lambda ()
            (slime-mode t)
            (show-paren-mode t)
            (global-set-key "\C-a" 'slime-switch-to-output-buffer)
            (global-set-key "\C-ch" 'slime-hyperspec-lookup)
            (global-set-key (kbd "M-a") 'slime-edit-definition)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (slime-repl-ansi-on)
            (define-key slime-repl-mode-map "\C-c\M-r" 'slime-restart-inferior-lisp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popwin

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove trailing white spaces

(defvar current-remove-trailing-whitespace-state "")

(defun remove-trailing-whitespaces ()
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'remove-trailing-whitespaces)

(setq-default mode-line-format
              (cons '(:eval current-remove-trailing-whitespace-state) mode-line-format))

(defun toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'remove-trailing-whitespaces before-save-hook)
         (setq current-remove-trailing-whitespace-state
               (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
         (remove-hook 'before-save-hook 'remove-trailing-whitespaces))
        (t
          (setq current-remove-trailing-whitespace-state "")
          (add-hook 'before-save-hook 'remove-trailing-whitespaces)))
  (force-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cask

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
;; Auto Complete

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-mode

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

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

(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/dev//HyperSpec/"))
      common-lisp-hyperspec-symbol-table
      (expand-file-name "~/dev/HyperSpec/Data/Map_Sym.txt"))

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
;; Smartparens

(require 'smartparens-config)

(defun enable-smartparens-mode ()
  (unless smartparens-mode
    (smartparens-mode)))

(add-hook 'slime-mode-hook #'enable-smartparens-mode)
(add-hook 'lisp-mode-hook #'enable-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-smartparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML mode

(add-to-list 'auto-mode-alist
             '("\\.tmpl\\'" . html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2 mode

(add-to-list 'load-path (expand-file-name "~/.emacs.d/espresso-mode/"))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
          #'(lambda ()
              (require 'espresso)

              (setq espresso-indent-level 2
                    espresso-expr-indent-offset 2
                    indent-tabs-mode nil)

              (defun my-js-indent-line ()
                (interactive)
                (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
                       (offset (- (current-column) (current-indentation)))
                       (indentation (espresso--proper-indentation parse-status)))
                  (back-to-indentation)
                  (if (looking-at "case\\s-")
                      (indent-line-to (+ indentation 2))
                    (espresso-indent-line))
                  (when (> offset 0) (forward-char offset))))

              (set (make-local-variable 'indent-line-function) 'my-js-indent-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emmet mode

(require 'emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(eval-after-load "emmet-mode" '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(define-key emmet-mode-keymap (kbd "C-i") 'emmet-expand-line) ;; C-i で展開

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSX

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq web-mode-markup-indent-offset 2)

(flycheck-def-executable-var 'jsxhint-checker "jsxhint")
(flycheck-define-command-checker 'jsxhint-checker
  "A JSX syntax and style checker based on JSXHint.
   You must insatll jsxhint with `npm insatll -g jsxhint` first"

  :command `("jsxhint" "--config" ,(expand-file-name "~/.emacs.d/.jshintrc") source)
  :error-patterns '((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes '(web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml mode

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickrun

(push '("*quickrun*") popwin:special-display-config)

(global-set-key (kbd "C-c C-r") 'quickrun)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "92d35a9332f9cb30881828fe0d58131aec5950430dc07c4f609980d4ddc5aef5" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "42ccd5eadda3546a89026b94794df7f4addadf25417b96917cf9db2f892b25a4" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "726dd9a188747664fbbff1cd9ab3c29a3f690a7b861f6e6a1c64462b64b306de" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twittering mode

(setq twittering-use-master-password t)
(setq twittering-use-ssl t)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 300)
(setq twittering-url-show-status t)
(setq twittering-icon-storage-limit t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EWW

(setq eww-search-prefix "https://www.google.co.jp/search?q=")
(defvar eww-disable-colorize t)

(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))

(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)

(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)

(defun eww-disable-color ()
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))

(defun eww-enable-color ()
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

(setq browse-url-browser-function 'eww-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SML

(add-to-list 'auto-mode-alist '("\\.sml$" . sml-mode))
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.rust\\'" . rust-mode))
(eval-after-load 'flycheck
                 '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(quickrun-add-command "cargo"
                      '((:command . "cargo")
                        (:exec    . "%c run %s"))
                      :mode 'rust-mode)
(quickrun-set-default "rust" "cargo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(require 'magit)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(set-face-background 'magit-item-highlight "#202020")
(set-face-foreground 'magit-diff-add "#40ff40")
(set-face-foreground 'magit-diff-del "#ff4040")
(set-face-foreground 'magit-diff-file-header "#4040ff")
(set-face-foreground 'magit-diff-file-header "#4040ff")

(defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
      (interactive)
        (kill-buffer)
          (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
    (interactive)
      (if (member "-w" magit-diff-options)
              (magit-dont-ignore-whitespace)
                  (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
    (interactive)
      (add-to-list 'magit-diff-options "-w")
        (magit-refresh))

(defun magit-dont-ignore-whitespace ()
    (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
        (magit-refresh))

(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'magit-commit-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)
(evil-set-initial-state 'magit-log-mode 'motion)
(evil-set-initial-state 'magit-wassup-mode 'motion)
(evil-set-initial-state 'magit-mode 'motion)
(evil-set-initial-state 'magit-rebase-mode 'motion)

(evil-define-key 'motion git-rebase-mode-map
  "c" 'git-rebase-pick
  "r" 'git-rebase-reword
  "s" 'git-rebase-squash
  "e" 'git-rebase-edit
  "f" 'git-rebase-fixup
  "y" 'git-rebase-insert
  "d" 'git-rebase-kill-line
  "u" 'git-rebase-undo
  "x" 'git-rebase-exec
  (kbd "RET") 'git-rebase-show-commit
  "\M-n" 'git-rebase-move-line-down
  "\M-p" 'git-rebase-move-line-up)

(evil-define-key 'motion magit-commit-mode-map
  "\C-c\C-b" 'magit-show-commit-backward
  "\C-c\C-f" 'magit-show-commit-forward)

(evil-define-key 'motion magit-status-mode-map
  "\C-f" 'evil-scroll-page-down
  "\C-b" 'evil-scroll-page-up
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "C" 'magit-add-log
  "I" 'magit-ignore-item-locally
  "S" 'magit-stage-all
  "U" 'magit-unstage-all
  "W" 'magit-toggle-whitespace
  "X" 'magit-reset-working-tree
  "d" 'magit-discard-item
  "i" 'magit-ignore-item
  "s" 'magit-stage-item
  "u" 'magit-unstage-item
  "z" 'magit-key-mode-popup-stashing
  (kbd "RET") 'magit-toggle-section)

(evil-define-key 'motion magit-log-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "e" 'magit-log-show-more-entries)

(evil-define-key 'motion magit-wazzup-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "i" 'magit-ignore-item)

(evil-set-initial-state 'magit-branch-manager-mode 'motion)
(evil-define-key 'motion magit-branch-manager-mode-map
  "a" 'magit-add-remote
  "c" 'magit-rename-item
  "d" 'magit-discard-item
  "o" 'magit-create-branch
  "v" 'magit-show-branches
  "T" 'magit-change-what-branch-tracks)

(evil-define-key 'motion magit-mode-map
  "\M-1" 'magit-show-level-1-all
  "\M-2" 'magit-show-level-2-all
  "\M-3" 'magit-show-level-3-all
  "\M-4" 'magit-show-level-4-all
  "\M-H" 'magit-show-only-files-all
  "\M-S" 'magit-show-level-4-all
  "\M-h" 'magit-show-only-files
  "\M-s" 'magit-show-level-4
  "!" 'magit-key-mode-popup-running
  "$" 'magit-process
  "+" 'magit-diff-larger-hunks
  "-" 'magit-diff-smaller-hunks
  "=" 'magit-diff-default-hunks
  "/" 'evil-search-forward
  ":" 'evil-ex
  ";" 'magit-git-command
  "?" 'evil-search-backward
  "<" 'magit-key-mode-popup-stashing
  "A" 'magit-cherry-pick-item
  "B" 'magit-key-mode-popup-bisecting
  "D" 'magit-revert-item
  "E" 'magit-ediff
  "F" 'magit-key-mode-popup-pulling
  "G" 'evil-goto-line
  "H" 'magit-rebase-step
  "J" 'magit-key-mode-popup-apply-mailbox
  "K" 'magit-key-mode-popup-dispatch
  "L" 'magit-add-change-log-entry
  "M" 'magit-key-mode-popup-remoting
  "N" 'evil-search-previous
  "P" 'magit-key-mode-popup-pushing
  "Q" 'magit-quit-session
  "R" 'magit-refresh-all
  "S" 'magit-stage-all
  "U" 'magit-unstage-all
  "W" 'magit-diff-working-tree
  "X" 'magit-reset-working-tree
  "Y" 'magit-interactive-rebase
  "Z" 'magit-key-mode-popup-stashing
  "a" 'magit-apply-item
  "b" 'magit-key-mode-popup-branching
  "c" 'magit-key-mode-popup-committing
  "e" 'magit-diff
  "f" 'magit-key-mode-popup-fetching
  "g?" 'magit-describe-item
  "g$" 'evil-end-of-visual-line
  "g0" 'evil-beginning-of-visual-line
  "gE" 'evil-backward-WORD-end
  "g^" 'evil-first-n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown

(require 'livedown)

(global-set-key (kbd "M-m") 'livedown:preview)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang

(require 'go-autocomplete)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir

(require 'alchemist)
(setq alchemist-mix-command "/usr/local/bin/mix")
(push '("*alchemist-test-report*") popwin:special-display-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl

(setq cperl-indent-parens-as-block t)
(setq perl-indent-parens-as-block t)

(add-hook 'cperl-mode-hook
          (lambda ()
            (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
              (auto-complete-mode t)
              (make-variable-buffer-local 'ac-sources)
              (setq ac-sources '(ac-source-perl-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


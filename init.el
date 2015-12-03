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
;; Company-mode

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(setq flycheck-jshintrc "~/dotfiles/jshintrc")

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


(flycheck-def-executable-var 'jsxhint-checker "jsxhint")
(flycheck-define-command-checker 'jsxhint-checker
  "A JSX syntax and style checker based on JSXHint.
   You must insatll jsxhint with `npm insatll -g jsxhint` first"

  :command `("jsxhint" "--config" ,(expand-file-name "~/.emacs.d/.jshintrc") source)
  :error-patterns '((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes '(web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web mode

(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

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
;; Markdown

(require 'livedown)

(global-set-key (kbd "M-m") 'livedown:preview)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir

(require 'alchemist)
(setq alchemist-mix-command "/usr/local/bin/mix")
(push '("*alchemist-test-report*") popwin:special-display-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby

(add-hook 'ruby-mode-hook
          '(lambda ()
             (rubocop-mode t)
             (flycheck-define-checker ruby-rubocop
               "A Ruby syntax and style checker using the RuboCop tool."
               :command ("rubocop" "--format" "emacs"
                         (config-file "--config" flycheck-rubocoprc)
                         source)
               :error-patterns
               ((warning line-start
                         (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
                         line-end)
                (error line-start
                       (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
                       line-end)))
             (setq flycheck-checker 'ruby-rubocop)
             (ruby-electric-mode t)
             (ruby-block-mode t)
             (setq ruby-block-highlight-toggle t)
             (flycheck-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCaml

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crystal

(add-to-list 'auto-mode-alist '("Projectfile$" . crystal-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Ian's init.el as he learns Emacs
;;

;; ----------------------------------------------------------------------------
;; Packages
;; ----------------------------------------------------------------------------

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(require 'cl)         ;; Common Lisp Extensions
(require 'ffap)       ;; Find File improvements
(require 'uniquify)   ;; Unique buffer names
(require 'ansi-color) ;; Support ANSI color when running commands
(require 'recentf)    ;; Recent files
(require 'saveplace)

(require 'color-theme)
(require 'cperl-mode)
(require 'css-mode)
(require 'diff-git)
(require 'eshell-vc)
(require 'espresso)
(require 'highlight-parentheses)
(require 'highline)
(require 'idle-highlight)
(require 'ido)
(require 'magit)
(require 'markdown-mode)
(require 'nav)
(require 'whitespace)
(require 'yaml-mode)
(require 'zenburn)

;; ----------------------------------------------------------------------------
;; Emacs Options
;; ----------------------------------------------------------------------------

;; Save locations between files
(setq-default save-place t)

;; Command key is Meta on OS X
(setq mac-command-modifier 'meta)

;; No backup files, thanks.
(setq make-backup-files nil)

;; No tabs, 4 spaces per tab.
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Color theming
(color-theme-initialize)
(if window-system (color-theme-tango))

;; Improved buffer switching and stuff
(ido-mode t)

;; ----------------------------------------------------------------------------
;; Python
;; ----------------------------------------------------------------------------

;; Enable "electric pairs", courtesy jesselegg.com
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; "This automatically indents newlines and attempts to locate the
;; cursor at the appropriate, whitespace-sensitive location whenever
;; you press Return."
(add-hook 'python-mode-hook '(lambda () 
                               (define-key python-mode-map "\C-m"
                                 'newline-and-indent)))


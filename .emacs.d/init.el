;;
;; Ian's init.el as he learns Emacs
;;

;; ----------------------------------------------------------------------------
;; Packages
;; ----------------------------------------------------------------------------

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/packages"))

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
(require 'magit)
(require 'markdown-mode)
(require 'nav)
(require 'yaml-mode)
(require 'zenburn)


;; ----------------------------------------------------------------------------
;; Options
;; ----------------------------------------------------------------------------

;; Save locations between files
(setq-default save-place t)

;; Command key is Meta on OS X
(setq mac-command-modifier 'meta)

;; No backup files, thanks.
(setq make-backup-files nil)

;; No tabs, 2 spaces per tab.
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Color theming
(color-theme-initialize)
(if window-system (color-theme-zenburn))

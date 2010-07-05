;;
;; Ian's init.el as he learns Emacs
;;

(require 'cl)         ;; Common Lisp Extensions
(require 'ffap)       ;; Find File improvements
(require 'uniquify)   ;; Unique buffer names
(require 'ansi-color) ;; Support ANSI color when running commands
(require 'recentf)    ;; Recent files

;; Save locations between files
(require 'saveplace)
(setq-default save-place t)

;; Command key is Meta on OS X
(setq mac-command-modifier 'meta)

;; No backup files, thanks.
(setq make-backup-files nil)

;; No tabs, 2 spaces per tab.
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

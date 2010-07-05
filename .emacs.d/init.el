;;
;; Ian's init.el as he learns Emacs
;;

(require 'cl)         ;; Common Lisp Extensions
(require 'ffap)       ;; Find File improvements
(require 'uniquify)   ;; Unique buffer names
(require 'ansi-color) ;; Support ANSI color when running commands
(require 'recentf)    ;; Recent files

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(when
  (load
    (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Load non-ELPA packages.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/non-elpa"))

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

;; Color theming
(require 'color-theme)
(color-theme-initialize)
(if window-system (color-theme-desert))

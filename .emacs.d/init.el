;;
;; Ian's Emacs config
;;

(defconst gui
  (not (eq window-system 'nil))
  "Are we running window system?")

(defconst macgui
  (eq window-system "ns")
  "Are we running as a Max OS X app?")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/icicles"))

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(when gui
  (color-theme-blackboard))

(require 'icicles)

(setq
 inhibit-startup-messages t
 inhibit-startup-screen t
 make-backup-files nil 
 auto-save-default nil)

(setq-default
 indent-tabs-mode nil
 case-fold-search t)

(menu-bar-mode -1)

(when gui
  (setq mac-command-modifier 'meta)
  (tool-bar-mode -1)
  (scroll-bar-mode 'nil))

(when macgui
  (set-default-font "Inconsolata 14"))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

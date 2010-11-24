;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous small things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Use ack instead of grep - http://betterthangrep.com/
(load-library "ack")
(defalias 'grep 'ack)

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete-1.3.1/ac-dict")
(ac-config-default)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(when gui
  (color-theme-blackboard))

;; Edit remote files - http://www.gnu.org/software/emacs/manual/tramp.html
(require 'tramp)
(setq tramp-default-method "scp")

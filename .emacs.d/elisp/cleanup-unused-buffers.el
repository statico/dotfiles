;;
;; http://www.emacswiki.org/emacs/KillingBuffers
;;

;; midnight mode
(require 'midnight)

;;kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special 900)
(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps
      '("^.*$"))

;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*scratch*" "*Pymacs*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")

;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*EMMS Playlist\\*.*$")
              clean-buffer-list-kill-never-regexps-init))

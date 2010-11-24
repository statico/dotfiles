;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactively Do Things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode 1)

;; Display ido results vertically, rather than horizontally
;; (from timcharper, jpkotta via EmacsWiki)
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
              " [No match]" " [Matched]" " [Not readable]"
              " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)


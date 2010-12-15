;; http://www.emacswiki.org/emacs/SearchAtPoint

;; highlight symbol at point and jump to next automatically
(load-library "highlight-symbol")
(defun hl-symbol-and-jump ()
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (unless hi-lock-mode (hi-lock-mode 1))
    (if (member symbol highlight-symbol-list)
	(highlight-symbol-next)
      (highlight-symbol-at-point)
      (highlight-symbol-next))))
(defun hl-symbol-cleanup ()
  (interactive)
  (mapc 'hi-lock-unface-buffer highlight-symbol-list)
  (setq highlight-symbol-list ()))

(global-set-key (kbd "C-x *") 'hl-symbol-and-jump)
(global-set-key (kbd "C-*") 'hl-symbol-cleanup)

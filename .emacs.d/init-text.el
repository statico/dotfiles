;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings for editing text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (lambda () (set-fill-column 72)))
(add-hook 'text-mode-hook (lambda () (column-number-mode 1)))



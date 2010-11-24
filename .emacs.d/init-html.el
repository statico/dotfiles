;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML-editing settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 23)
  (load "~/.emacs.d/vendor/nxhtml-2.08/autostart.el")

  ;; No silly background colors for different modes.
  (setq mumamo-background-colors nil) 

  ;; Set as the Django default for HTML files.
  (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode)))

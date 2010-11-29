;;
;; Viper mode customizations
;;

;; I kinda know what I'm doing, right?
(setq viper-inhibit-startup-message t)
(setq viper-expert-level '5)

;; Emulate Vim the way I'm used to.
(setq-default viper-auto-indent t)
(setq-default viper-electric-mode t)
(setq-default viper-case-fold-search t)
(setq-default viper-re-search t)
(setq-default blink-matching-paren t)
(setq-default viper-search-wrap-around t)
(setq-default viper-search-scroll-threshold 10)
(setq-default viper-re-query-replace t)
(setq-default viper-ex-style-motion nil)
(setq-default viper-ex-style-editing nil)
(setq-default viper-want-emacs-keys-in-insert t)
(setq-default viper-want-ctl-h-help t)

;; Keybinding
(setq-default viper-toggle-key (kbd "C-q"))
(define-key viper-vi-global-user-map (kbd "C-e") 'switch-to-previous-buffer)
(define-key viper-vi-global-user-map (kbd "C-f") 'ido-find-file)
(define-key viper-vi-global-user-map (kbd "C-u") 'viper-scroll-down)
(define-key viper-vi-global-user-map (kbd ";") 'ido-switch-buffer)
(define-key viper-vi-global-user-map (kbd "-") 'shell)
(define-key viper-insert-global-user-map (kbd "C-e") 'end-of-line)

;; This might be changed later.
(setq-default viper-shift-width 2)

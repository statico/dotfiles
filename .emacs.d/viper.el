  ;; Raph makes nice fonts :)
  (set-default-font "Inconsolata 14")

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
(define-key viper-vi-global-user-map (kbd "C-;") 'dired-jump)
(define-key viper-vi-global-user-map (kbd ";") 'ido-switch-buffer)
(define-key viper-vi-global-user-map (kbd "-") 'shell)
(define-key viper-insert-global-user-map (kbd "C-e") 'end-of-line)

;; TODO: Make C-c cancel the region, like C-g.

;; I used backslash as my <Leader> key in Vim, and backslash-X keys
;; made good keybindings for toggling settings and macros.
(define-key viper-vi-global-user-map (kbd "\\") nil)
(define-key viper-vi-global-user-map (kbd "\\ a") 'auto-fill-mode)
(define-key viper-vi-global-user-map (kbd "\\ c") 'css-color-mode)
(define-key viper-vi-global-user-map (kbd "\\ j") 'speedbar)
(define-key viper-vi-global-user-map (kbd "\\ l") 'linum-mode)
(define-key viper-vi-global-user-map (kbd "\\ u") 'ethan-wspace-mode)
(define-key viper-vi-global-user-map (kbd "\\ v") 'global-centered-cursor-mode)

(add-hook 'hs-minor-mode-hook
          '(lambda ()
             (define-key viper-vi-local-user-map (kbd "SPC") 'hs-toggle-hiding)))

(add-hook 'sgml-mode-hook
          '(lambda ()
             (define-key viper-insert-local-user-map (kbd "C-/") 'sgml-close-tag)))

;; Is rebinding C-c a good idea?
(define-key viper-insert-global-user-map (kbd "C-c") 'vimpulse-exit-insert-state)

;; This might be changed later.
(setq-default viper-shift-width 2)

(setq scrolloff 20) ; don't touch or else...

;; Color theme.
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(when gui
  (zenburn)
  (color-theme-railscasts)
  (highlight-current-line-on nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(button ((t (:inherit zenburn-blue-1 :underline t :weight bold))))
 '(cursor ((t (:background "green"))))
 '(custom-link ((t (:inherit zenburn-blue :underline t))))
 '(fancy-widget-button ((t (:background "#3f3f3f" :box (:line-width 2 :style released-button)))))
 '(fancy-widget-field ((t (:background "#333" :box (:line-width 1 :color "#444")))))
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((t (:underline "cyan"))))
 '(font-lock-comment-delimiter ((t (:inherit zenburn-lowlight-1))))
 '(font-lock-comment-delimiter-face ((t (:inherit zenburn-lowlight-1))))
 '(font-lock-comment-face ((t (:foreground nil :inherit zenburn-lowlight-1 :slant italic))))
 '(fringe ((t (:foreground "#4f4f4f"))))
 '(highlight-current-line ((t (:background "#292929"))))
 '(highlight-current-line-face ((t (:inherit highlight-current-line))))
 '(lazy-highlight ((((class color) (min-colors 8)) (:background "grey25"))))
 '(linum ((t (:inherit font-lock-comment-delimiter))))
 '(mode-line ((t (:background "#545d57" :foreground "#c5ffd1" :box (:line-width 2 :color "#1e2320")))))
 '(mode-line ((t (:background "#454d48" :foreground "#acbc90" :box (:line-width 2 :color "#1e2320")))))
 '(mode-line-buffer-id ((t (:foreground "#A5BAF1" :weight bold))))
 '(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090" :box (:line-width 2 :color "#2e3330")))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-border-face-out ((t (:inherit font-lock-preprocessor-face :underline t))))
 '(primary-selection ((t (:inherit region))))
 '(region ((t (:background "#446"))))
 '(show-paren-match ((t (:background "#2f2f43" :foreground "#838fea"))))
 '(vertical-border ((nil (:foreground "#666"))))
 '(viper-minibuffer-emacs ((((class color)) (:background "darkseagreen2" :foreground "Black"))))
 '(viper-minibuffer-insert ((((class color)) nil)))
 '(viper-search ((((class color)) (:background "#330" :foreground "yellow"))))
 '(zenburn-highlight-subtle ((t nil))))

;;
;; Ian's Emacs config
;;
;; Inspiration:
;; - http://github.com/EnigmaCurry/emacs
;; - http://github.com/technomancy/emacs-starter-kit
;;

;; ----------------------------------------------------------------------------
;; Packages
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'cl)         ;; Common Lisp Extensions
(require 'ffap)       ;; Find File improvements
(require 'uniquify)   ;; Unique buffer names
(require 'ansi-color) ;; Support ANSI color when running commands
(require 'recentf)    ;; Recent files
(require 'saveplace)

;; vendor plugins
(require 'auto-complete-config)
(require 'color-theme)
(require 'cperl-mode)
(require 'css-mode)
(require 'diff-git)
(require 'eshell-vc)
(require 'espresso)
(require 'highlight-parentheses)
(require 'highline)
(require 'idle-highlight)
(require 'ido)
(require 'magit)
(require 'markdown-mode)
(require 'nav)
(require 'whitespace)
(require 'yaml-mode)
(require 'yasnippet-bundle)
(require 'zenburn)

;; ----------------------------------------------------------------------------
;; Emacs Options
;; ----------------------------------------------------------------------------

;; Show line & column number
(column-number-mode 1)

;; Highlight matching parens
(show-paren-mode 1)

;; Show the scratch buffer at startup, not the welcome screen
(setq inhibit-startup-screen 1)

;; Show empty lines at the bottom of a frame
(setq indicate-empty-lines 1)

;; Save locations between files
(setq-default save-place t)

;; Command key is Meta on OS X
(setq mac-command-modifier 'meta)

;; Stop beeping
(setq visible-bell t)

;; Hide the toolbar
(tool-bar-mode -1)

;; No backup files, thanks
(setq make-backup-files nil)
(setq auto-save-default nil)

;; No tabs, 4 spaces per tab
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Color theming
(color-theme-initialize)
(if window-system (color-theme-tango))

;; Improved buffer switching and stuff
(ido-mode t)

;; Auto-completion
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/ac-dict")
(ac-config-default)

;; Make it a little easier to move between window
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; Make C-x/c/v into cut-copy-paste when text is selected
(cua-mode t)

;; Configure YASnippet
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;; ----------------------------------------------------------------------------
;; Python
;; ----------------------------------------------------------------------------

(require 'init-python)

;; ----------------------------------------------------------------------------
;; Bindings
;; ----------------------------------------------------------------------------

;; Font-size adjustment from http://is.gd/iaAo
(defun custom/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun custom/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))
(global-set-key (kbd "C-M-+") 'custom/increase-font-size)
(global-set-key (kbd "C-M--") 'custom/decrease-font-size)

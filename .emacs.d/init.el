;;
;; Ian's Emacs config
;;
;; A lot of things stolen from EmacsWiki and threads like
;; http://news.ycombinator.com/item?id=1654164

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gui (not (eq window-system 'nil))
  "Are we running window system?")

(defconst macgui (string-equal window-system "ns")
  "Are we running as a Max OS X app?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries and snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add all subdirectories in the vendor dir to the load path.

(defun add-subfolders-to-load-path (parent-dir) ;; from bbatsov
  "Adds all first level `parent-dir' subdirs to the Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(add-subfolders-to-load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Load extra Emacs lisp snippets.

(defun load-snippet (name)
  "Loads the file in elisp-dir with the given name."
  (load (concat "~/.emacs.d/elisp/" name)))

(load-snippet "rename-file-and-buffer")
(load-snippet "swap-windows")
(load-snippet "whole-line-or-region")

;; Executables might be somewhere else
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/opt/local/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pile o' settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Be quiet at startup.
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)

;; No damn beeping.
(setq ring-bell-function 'ignore)

;; Don't save backup files everywhere.
(setq make-backup-files nil )
(setq auto-save-default nil)

;; Hide the menu, toolbar and scroll bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (not gui)
  (menu-bar-mode -1))

;; By default, use spaces, not tabs, and display 4 spaces per tab.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset tab-width
              js-indent-level tab-width
              css-indent-level tab-width)

;; Make search case-insensitive.
(setq-default case-fold-search t)

;; Make the region act like common text selection.
(transient-mark-mode 1)

;; Highlight parentheses.
(show-paren-mode 1)

;; All of my terminals are Unicode-aware.
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Display the line and column number in the modeline.
(setq line-number-mode t)
(setq column-number-mode t)
(line-number-mode t)
(column-number-mode t)

;; Make all "yes or no" prompts show "y or n" instead.
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow narrowing. (`C-x n w' gets you out of this.)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Allow upcasing-downcasing.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Color theme.
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(when gui
  (zenburn)
  (color-theme-railscasts))

;; <Enter> should be smart. (DWIM)
(global-set-key (kbd "RET") 'newline-and-indent)

;; IDO mode: Better buffer and file completion.
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode 1)
;; Get rid of the annoying .ido.last file
;; (http://stackoverflow.com/questions/1371076)
(setq
 ido-enable-last-directory-history nil
 ido-record-commands nil
 ido-max-work-directory-list 0
 ido-max-work-file-list 0)
(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-completion-map (kbd "C-u") 'kill-line)
             (define-key ido-completion-map (kbd "C-k") 'backward-kill-word)))
(global-set-key (kbd "C-;") 'ido-switch-buffer)

;; Display IDO results vertically, rather than horizontally
;; (from timcharper, jpkotta via EmacsWiki)
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
              " [No match]" " [Matched]" " [Not readable]"
              " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Switch-to-previous-buffer
(global-set-key (kbd "C-=") 'switch-to-previous-buffer)
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; C-z toggles between shell. (C-x C-z still suspends.)
(require 'shell)
(global-set-key (kbd "C-z") 'shell)
(define-key shell-mode-map (kbd "C-z") 'bury-buffer)

;; Make buffer names unique.
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")

;; Fonts I like
(defun set-font-face (face anti-alias)
  (interactive
   (list (read-string "Face? ")
         (y-or-n-p "Anti-alias? ")))
  (set-default-font face)
  (setq mac-allow-anti-aliasing anti-alias)
  (redraw-display)
  (message (format "Font set to '%s', anti-aliasing %s"
                   face
                   (if anti-alias "enabled" "disabled"))))
(defun set-font-face-inconsolata ()
  (interactive)
  (set-font-face "Inconsolata 14" t))
(defun set-font-face-monaco ()
  (interactive)
  (set-font-face "Monaco 9" nil))

;; Mac OS X customizations
(when macgui

  ;; Raph makes nice fonts :)
  (set-default-font "Inconsolata 14")

  ;; Make the command key the meta key.
  (setq mac-command-modifier 'meta)

  ;; Make Cmd-~ do the right thing.
  (global-set-key (kbd "M-`") 'ns-next-frame))

;; When starting the GUI, maximize the frame vertically.
(when gui
  (require 'frame-cmds)
  (maximize-frame-vertically))

;; Zoom the font size in and out in GUI.
(when gui
  (require 'zoom-frm)
  (global-set-key (kbd "M--") 'zoom-out)
  (global-set-key (kbd "M-=") 'zoom-in)
  (global-set-key (kbd "M-+") 'zoom-in)
  (global-set-key (kbd "M-0") 'zoom-frm-unzoom))

;; browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-x C-r") 'browse-kill-ring)

;; C-_ is undo, but what about redo?
(global-set-key (kbd "M-_") 'redo)

;; Keep the cursor kinda centered, like scrolloff in vim
(when gui
    (require 'centered-cursor-mode)
    (global-set-key (kbd "C-M-c") 'global-centered-cursor-mode))

;; Use ack instead of grep - http://betterthangrep.com/
(load-library "ack")
(defalias 'grep 'ack)
(setq ack-command "~/bin/ack --nocolor --nogroup")

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete-1.3.1/ac-dict")
(ac-config-default)
(setq ac-auto-start t)
(setq ac-auto-show-menu nil)
(setq ac-delay 0.1)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-show-menu t)
(setq ac-trigger-key nil)

;; Settings for editing text
(setq sentence-end-double-space nil)

;; HTML-editing settings
(when (>= emacs-major-version 23)
  (load "~/.emacs.d/vendor/nxhtml-2.08/autostart.el")

  ;; No silly background colors for different modes.
  (setq mumamo-background-colors nil)

  ;; Set as the Django default for HTML files.
  (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode)))

;; "Sparkup" or "Zen-coding" makes churning out HTML easier.
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; CSS settings
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-hook 'css-mode-hook 'css-color-mode)

;; dired settings
(require 'dired)
(define-key dired-mode-map (kbd "u") 'dired-up-directory)
(define-key dired-mode-map (kbd "U") 'dired-unmark)
(global-set-key (kbd "C-M-;") 'dired-jump)
(global-set-key (kbd "C-:") 'dired-jump)

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key markdown-mode-map (kbd "RET") 'markdown-enter-key)
(setq-default markdown-command "~/bin/markdown")
(setq-default markdown-italic-underscore t)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(define-key yaml-mode-map (kbd "RET") 'newline-and-indent)

;; Open init.el
(defun open-init-dot-el ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-0") 'open-init-dot-el)

;; Useful for editing config files
(global-set-key (kbd "C-M-9") '(lambda ()
  "Evaluates the current region (if a region is active) or the
  current buffer (if no region is active)."
  (interactive)
  (if (or (eq major-mode 'lisp-interaction-mode)
          (eq major-mode 'emacs-lisp-mode))
      (if (region-active-p)
          (progn
            (eval-region (region-beginning) (region-end))
            (message "Region eval'd"))
        (eval-buffer)
        (message "Buffer eval'd"))
      (message "Not in a Lisp mode"))))


;; The Cult of Steve Yegge ;)
;; http://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(defalias 'er 'eval-region)
(defalias 'rb 'rename-buffer)
(defalias 'qrr 'query-replace-regexp)

;; C-w kills a word or region depending on context. (DWIM)
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-c") 'kill-region)

;; Directory browser
(global-set-key (kbd "C-M-b") 'speedbar)

;; Make dired ignore certain files
(eval-after-load "dired"
  '(require 'dired-x))
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))
(setq dired-omit-files
      (concat dired-omit-files "\\|\.pyc$"))

;; Vim-like mark commands
(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x '") 'jump-to-register)

;; Vim-like "*" command
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-next)
(global-set-key (kbd "C-x *") 'highlight-symbol-prev)

;; Vim-like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; Window movement
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Modern IDE-like line highlighting (subtle)
(require 'highlight-current-line)
(setq highlight-current-line-globally t)
(highlight-current-line-on t)

;; Ethan whitespace mode
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; Vim-like line-joining with C-k
(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable code-folding
(add-hook 'python-mode-hook
          '(lambda ()
             (flyspell-prog-mode)))
             ;; (hs-minor-mode 1)
             ;; (hs-hide-all)))

;; Use M-RET to toggle hiding
(add-hook 'hs-minor-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "M-RET") 'hs-toggle-hiding)
             (define-key python-mode-map (kbd "C-M-RET") 'hs-show-all)))

;; Use our local installation of Pymacs and rope.
(setq pymacs-load-path (expand-file-name "~/.emacs.d/python"))
;; (setenv "PYTHONPATH"
;;         (concat (getenv "PYTHONPATH") ":"
;;                 (expand-file-name "~/.emacs.d/python")))

;; ;; Basic Pymacs setup instructions.
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; Load Rope/ropemacs only when needed.
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-confirm-saving nil)
  (setq ropemacs-guess-project t)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  )
(global-set-key (kbd "\C-x p y") 'load-ropemacs)

;; Python/Flymake/Pylint attempt.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-delay 0.1)
 '(ac-show-menu-immediately-on-auto-complete t)
 '(ac-trigger-key nil)
 '(debug-on-error nil)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(visual-line-fringe-indicators (quote (nil nil))))
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
 '(font-lock-comment-delimiter ((t (:inherit zenburn-lowlight-1))))
 '(font-lock-comment-delimiter-face ((t (:inherit zenburn-lowlight-1))))
 '(font-lock-comment-face ((t (:foreground nil :inherit zenburn-lowlight-1 :slant italic))))
 '(fringe ((t (:foreground "#4f4f4f"))))
 '(highlight-current-line ((t (:background "#292929"))))
 '(highlight-current-line-face ((t (:inherit highlight-current-line))))
 '(lazy-highlight ((((class color) (min-colors 8)) (:background "grey25"))))
 '(linum ((t (:inherit font-lock-comment-delimiter))))
 '(mode-line ((t (:background "#454d48" :foreground "#acbc90" :box (:line-width 2 :color "#1e2320") :height 0.9 :family "Arial"))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-host customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((local-elisp-path "~/.local.el"))
  (if (file-exists-p local-elisp-path)
      (load local-elisp-path)))

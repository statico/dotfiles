;;
;; Ian's Emacs config
;;
;; A lot of things stolen from EmacsWiki and threads like
;; http://news.ycombinator.com/item?id=1654164

;; TODO: smex, minimap, pysmell

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
(load-snippet "whole-line-or-region")
(load-snippet "sgml-delete-tagged-text")
(load-snippet "remove-alist-name")
(load-snippet "ian-python")
(load-snippet "ian-javascript")

;; Executables might be somewhere else
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; Info might be somewhere else
(add-to-list 'Info-default-directory-list (expand-file-name "~/.dotfiles/info"))
(add-hook 'Info-mode-hook               ; After Info-mode has started
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)
            ))

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

;; Don't keep splitting windows for things. It's annoying.
(setq pop-up-windows nil)

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

;; If a file has changed and I haven't modified it, don't bother me.
(global-auto-revert-mode 1)

;; From http://www.emacswiki.org/emacs/SmoothScrolling -
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Color theme.
(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/themes")
(color-theme-initialize)

(defun color-theme-dark ()
  "Use a white-on-black theme and disable line highlighting."
  (interactive)
  (color-theme-clarity)
  (highlight-current-line-on nil))

(defun color-theme-light ()
  (interactive)
  (color-theme-mac-classic)
  (highlight-current-line-on nil))

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
(defun set-font-face-georgia ()
  (interactive)
  (set-font-face "Georgia 16" t))
(defun set-font-face-terminus ()
  (interactive)
  (set-font-face "Terminus 10" t))
(defalias 'sffi 'set-font-face-inconsolata)
(defalias 'sffm 'set-font-face-monaco)
(defalias 'sffg 'set-font-face-georgia)

(when macgui
  (set-face-font 'variable-pitch "Lucida Grande"))

;; Settings for editing text
(setq sentence-end-double-space nil)

;; <Enter> should be smart. (DWIM)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Project Root is hotness.
(require 'project-root)
(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p g") 'project-root-grep)
(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p d") 'project-root-goto-root)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
(global-set-key (kbd "C-.") 'project-root-find-file)

;; Anything - starting to replace IDO below...
(require 'anything-match-plugin)
(require 'anything-config)
(load-snippet "custom-anything-sources")
(setq anything-samewindow t)
(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers
     anything-c-source-buffer-not-found
     anything-c-source-file-name-history
     anything-c-source-files-in-current-dir
     anything-c-source-project-files
     anything-c-source-ctags)
   " *my-anything*"))
(global-set-key (kbd "C-:") 'my-anything)
(global-set-key (kbd "M-X") 'anything-imenu)
(global-set-key (kbd "C-M-y") 'anything-show-kill-ring)

;; IDO mode: Better buffer and file completion.
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode 1)
;; Ignore some stuff
;; http://emacsblog.org/2008/05/19/giving-ido-mode-a-second-chance/
(defun my-ido-ignore-buffers (name)
 (with-current-buffer name
   (string-match "-template-indent-buffer$" name)))
(setq ido-ignore-buffers '(my-ido-ignore-buffers))
;; fuzzy matching is a must have, says rmm5t
(setq ido-enable-flex-matching t)
;; Get rid of the annoying .ido.last file
;; (http://stackoverflow.com/questions/1371076)
(setq
 ido-enable-last-directory-history nil
 ido-record-commands nil
 ido-max-work-directory-list 0
 ido-max-work-file-list 0)
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

;; Neat little Textmate Command-T emulation
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))
(global-set-key (kbd "M-C-;") 'ido-find-file-in-tag-files)

;; Switch-to-previous-buffer
(global-set-key (kbd "C-=") 'switch-to-previous-buffer)
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; C-z toggles between shell. (C-x C-z still suspends.)
(require 'shell)
(define-key shell-mode-map (kbd "C-z") 'switch-to-previous-buffer)
(global-set-key (kbd "C-z") (if gui 'shell 'suspend-emacs))

;; Make buffer names unique.
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")

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
;; (Don't forget that C-x C-- and C-x C-+ do this for a single buffer.)
(when gui
  (require 'zoom-frm)
  (global-set-key (kbd "M--") 'zoom-frm-out)
  (global-set-key (kbd "M-+") 'zoom-frm-in)
  (global-set-key (kbd "M-=") 'zoom-frm-unzoom))

;; browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-x C-r") 'browse-kill-ring)

;; Use ack instead of grep - http://betterthangrep.com/
(load-library "ack")
(defalias 'grep 'ack)
(setq ack-command "~/bin/ack --nocolor --nogroup")

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete-1.3.1/ac-dict")
(ac-config-default) ;; Use customize for further settings.
(define-key ac-complete-mode-map "\r" nil) ;; Interferes with CSS

;; HTML-editing settings
;;(when (>= emacs-major-version 23)
;;  (load "~/.emacs.d/vendor/nxhtml-2.08/autostart.el")
;;
;;  ;; No silly background colors for different modes.
;;  (setq mumamo-background-colors nil)
;;
;;  ;; Set as the Django default for HTML files.
;;  (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))
;;
;;  ;; Useful commands
;;  (global-set-key (kbd "C-c d") 'sgml-delete-tagged-text)
;;
;;  ;; Keep zencoding on when we edit HTML.
;;  (add-hook 'django-html-mumamo-mode-hook 'zencoding-mode)
;;  (add-hook 'html-mumamo-mode-hook 'zencoding-mode)
;;  (add-hook 'sgml-mumamo-mode-hook 'zencoding-mode)
;;  (add-hook 'sgml-mode-hook 'zencoding-mode))

;; CSS settings
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-hook 'sass-mode-hook 'css-color-mode)
(add-hook 'css-mode-hook 'css-color-mode)

;; dired settings
(require 'dired)
(define-key dired-mode-map (kbd "u") 'dired-up-directory)
(define-key dired-mode-map (kbd "U") 'dired-unmark)
(global-set-key (kbd "C-:") 'dired-jump)

;; speedbar settings
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil)
(defun speedbar-timer-fn () '(lambda ()))
(global-set-key (kbd "C-M-b") 'sr-speedbar-toggle)
(define-key speedbar-key-map (kbd "u") 'speedbar-up-directory)
(setq speedbar-mode-hook
      '(lambda ()
         (hl-line-mode)
         (text-scale-decrease 1)
         (buffer-face-set 'variable-pitch)))
(dolist (extension '(".coffee" ".jade" ".sass"))
  (speedbar-add-supported-extension extension))

;; Make dired ignore certain files
(eval-after-load "dired"
  '(require 'dired-x))
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))
(setq dired-omit-files
      (concat dired-omit-files "\\|\.pyc$"))

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
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(define-key yaml-mode-map (kbd "RET") 'newline-and-indent)

;; CoffeeScript
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
   '(lambda() (coffee-custom)))
;; coffee-mode is still kinda buggy...
(eval-when-compile (defvar javascript-mode-syntax-table))

;; SASS
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Open init.el
(defun open-init-dot-el ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-0") 'open-init-dot-el)
(defun open-local-el ()
  (interactive)
  (find-file (expand-file-name "~/.local.el")))
(global-set-key (kbd "C-M-)") 'open-local-el)

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
(global-set-key (kbd "M-j") 'query-replace-regexp)

;; Misc aliases
(defalias 'er 'eval-region)
(defalias 'ffap 'find-file-at-point)
(defalias 'ow 'overwrite-mode)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rb 'rename-buffer)
(defalias 'rfab 'rename-file-and-buffer)
(defalias 'slines 'sort-lines)
(defalias 'afm 'auto-fill-mode)
(defalias 'llm 'longlines-mode)

;; C-w kills a word or region depending on context. (DWIM)
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-c") 'kill-region)

;; I seem to be using overwrite-mode...
(global-set-key (kbd "C-M-S-o") 'overwrite-mode)

;; Vim-like mark commands
(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x '") 'jump-to-register)

;; Vim-like "*" command
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-next)
(global-set-key (kbd "C-x *") 'highlight-symbol-prev)
(defalias 'hsap 'highlight-symbol-at-point)
(defalias 'hrap 'highlight-symbol-remove-all)

;; Vim-like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; Vim-like shift-O
(defun open-line-above ()
  (interactive)
  (previous-line)
  (open-line))
(global-set-key (kbd "C-S-o") 'open-line-above)

;; Window movement and settings
(winner-mode t)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Window switching avoids special windows
;; From http://stackoverflow.com/questions/4941960
(defvar avoid-window-regexp "^[0-9]$")
(defun my-other-window ()
  "Similar to 'other-window, only try to avoid windows whose buffers match avoid-window-regexp"
  (interactive)
  (let* ((window-list (delq (selected-window) (window-list)))
         (filtered-window-list (remove-if
                                (lambda (w)
                                  (string-match-p avoid-window-regexp (buffer-name (window-buffer w))))
                                window-list)))
    (if filtered-window-list
        (select-window (car filtered-window-list))
      (and window-list
           (select-window (car window-list))))))
(global-set-key (kbd "M-o") 'my-other-window)

;; Toggle dedicated windows
(load-snippet "toggle-current-window-dedication")
(global-set-key (kbd "M-6") 'toggle-current-window-dedication)

;; Set dedicated terminals for long running process
(defun init-server-window ()
  "Sets up some defaults for a window with a long-running process."
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-scroll-to-bottom-on-input t))

;; Helper function to create new shells.
(defun make-shell (name)
  (interactive
   (list (read-string "Name of new shell buffer? ")))
  (shell name))
(global-set-key (kbd "M-5") 'make-shell)

;; Map the window manipulation keys to meta 0, 1, 2, o (from rmm5t)
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-4") 'delete-window) ; left-handed!
;; Replace dired's M-o
(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map (kbd "M-o") 'my-other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook
          (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'my-other-window))) ; was ibuffer-visit-buffer-1-window
;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

;; Easier buffer killing
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; Modern IDE-like line highlighting (subtle)
(require 'highlight-current-line)
(setq highlight-current-line-globally t)
(highlight-current-line-on t)

;; Ethan whitespace mode
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
(defalias 'ewcam 'ethan-wspace-clean-all-modes)
(add-hook 'javascript-mode-hook 'ewcam)
(add-hook 'js-mode-hook 'ewcam)

;; Vim-like line-joining with C-k
(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

;; Bind line-joins to an easier key
(global-set-key (kbd "C-M-j") 'delete-indentation)

;; Swap two windows
(load-snippet "swap-windows")
(defalias 'sw 'swap-windows)
(global-set-key (kbd "C-M-S-s") 'swap-windows)

;; Rotate windows
(load-snippet "rotate-windows")
(global-set-key (kbd "M-S-<left>") 'rotate-windows)

;; Manage splitting and tiling.
(require 'tiling)
(define-key global-map (kbd "M-\\") 'tiling-cycle) ; accepts prefix number

;; Disable Flymake for HTML/XML files.
(require 'flymake)
(require 'flymake-cursor)
(dolist (extension '("html?" "xml"))
  (let ((full-extension (concat "\\." extension "\\'")))
    (setq flymake-allowed-file-name-masks
          (remove-alist-name full-extension flymake-allowed-file-name-masks))))
(global-set-key (kbd "C-c f n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)

;; Git
(require 'magit)
(require 'mo-git-blame)
(global-set-key (kbd "M-8") 'magit-status)
(defalias 'git-blame 'mo-git-blame-current)

(defun set-git-path (path)
  "Sets the path to the git executable. Use in ~/.local.el"
  (interactive (list (read-string "Path to git executable? ")))
  (setq magit-git-executable path)
  (setq mo-git-blame-git-executable path))

;; ReBuilder for regexes
(setq reb-re-syntax 'string)

;; YASnippet
;; (require 'yasnippet)
;; (yas/initialize)
;; (setq yas/root-directory '("~/.emacs.d/snippets"
;;                            "~/.emacs.d/vendor/yasnippet/snippets"))
;; (mapc 'yas/load-directory yas/root-directory)

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
 '(blink-cursor-mode nil)
 '(ccm-recenter-at-end-of-file t)
 '(ccm-vpos-init 10)
 '(column-number-mode t)
 '(debug-on-error nil)
 '(global-centered-cursor-mode t)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines t)
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(visual-line-fringe-indicators (quote (nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-host customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((local-elisp-path "~/.local.el"))
  (if (file-exists-p local-elisp-path)
      (load local-elisp-path)))
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

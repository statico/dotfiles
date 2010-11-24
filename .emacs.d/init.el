;;
;; Ian's Emacs config
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gui (not (eq window-system 'nil))
  "Are we running window system?")

(defconst macgui (eq window-system "ns")
  "Are we running as a Max OS X app?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Be quiet at startup.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; Don't save backup files everywhere.
(setq make-backup-files nil )
(setq auto-save-default nil)

;; Hide the menu, tool and scroll bars.
(menu-bar-mode -1)
(when gui
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Use spaces, not tabs, and display 2 spaces per tab.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Maybe I'm too used to bash.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; <Enter> should be smart.
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load everything else
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

;; Load extra Emacs lisp snippets.

(defun load-snippet (name)
  "Loads the file in elisp-dir with the given name."
  (load (concat "~/.emacs.d/elisp/" name)))

(load-snippet "rename-file-and-buffer")

;; Load the rest of the emacs configuration.

(defun load-config (name)
  "Loads the init file in basedir with the given name."
  (load (concat "~/.emacs.d/init-" name)))

(load-config "autocomplete")
(load-config "color-theme")
(load-config "customize")
(load-config "html")
(load-config "ido")
(load-config "macosx")
(load-config "python")
(load-config "text")
(load-config "tramp")

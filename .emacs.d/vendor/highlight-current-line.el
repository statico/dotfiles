;;; highlight-current-line.el --- highlight line where the cursor is.

;; Copyright (c) 1997-2003 Christoph Conrad Time-stamp: <19.09.2003 20:10:05>

;; Author: Christoph Conrad <Christoph.Conrad@gmx>
;; Created: 10 Oct 1997
;; Version: 0.57
;; Keywords: faces

;; This file is not yet part of any Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Minor mode to highlight the line the cursor is in. You can change colors
;; of foreground (text) and background. The default behaviour is to set
;; only a background color, so that font-lock fontification colors remain
;; visible (syntax coloring). Enable a buffer using the command
;; `highlight-current-line-minor-mode' and customize via:
;;
;;   M-x customize-group highlight-current-line <RET>.
;;
;; You can select whether the whole line (from left to right window border)
;; is marked or only the really filled parts of the line (from left window
;; border to the last char in the line). The second behaviour is suitable
;; if it's important for you to see trailing spaces or tabs in a
;; line. Customize the variable `highlight-current-line-whole-line' (or use
;; the function `highlight-current-line-whole-line-on' retained for
;; compatibility with prior versions).
;;
;; You may enable the minor-mode automatically for (almost) all buffers by
;; customizing the variable `highlight-current-line-globally' (or using the
;; compatibility command `highlight-current-line-on').  Buffers whose
;; buffer-name match the regular expression in the customizable variable
;; `highlight-current-line-ignore-regexp' do not highlighted.  You can
;; extend or redefine this regexp. This works together with the default
;; ignore function `highlight-current-line-ignore-function'. You can
;; redefine this function to implement your own criterias.

;; (The functions `highlight-current-line-on',
;; `highlight-current-line-set-fg-color' and
;; `highlight-current-line-set-bg-color' are retained for backward
;; compatibility. There's a special color "none" defined to set no color.)


;;; People which made contributions or suggestions:

;; This list is ordered by time. Latest in time first.
;; - Peter S Galbraith   <psg@debian.org>
;; - Masatake Yamato     <jet@gyve.org>
;; - Hrvoje Niksic	 <hniksic@srce.hr>
;; - Jari Aalto		 <jari.aalto@ntc.nokia.com>
;; - Shawn Ostermann     <sdo@picard.cs.OhioU.Edu>
;; - Peter Ikier	 <p_ikier@infoac.rmi.de>
;;   Many thanks to him for the idea. He liked this behaviour in another
;;   editor ("Q").

;;; Installation:
;;
;; Put a copy of highlight-current-line.el/.elc into some path of
;; `load-path'. To show `load-path': <C-h v> load-path RET
;;
;; Load the file, e.g. add in ~/.emacs
;;
;;  (require 'highlight-current-line)
;;
;; Enable it on a buffer using `M-x highlight-current-line-minor-mode'
;; or globally by customizing `highlight-current-line-globally'.
;;
;; Previous versions of this code worked by adding other comamnds in
;; ~/.emacs instead of using the custom interface.  This is still
;; supported:
;;
;;  ;; If you want to mark only to the end of line:
;;  (highlight-current-line-whole-line-on nil)
;;  ;; switch highlighting on
;;  (highlight-current-line-on t)
;;  ;; Ignore no buffer
;;  (setq highlight-current-line-ignore-regexp nil) ; or set to ""
;;  ;; alternate way to ignore no buffers
;;  (fmakunbound 'highlight-current-line-ignore-function)
;;  ;; Ignore more buffers
;;  (setq highlight-current-line-ignore-regexp
;;       (concat "Dilberts-Buffer\\|"
;;	       highlight-current-line-ignore-regexp))

;;; Troubleshooting:

;; - Q: I do not see matching parens from paren.el any more!
;; - A: Check the colors from highlight-current-line or from show-paren-face
;;   and choose some combination which works together.

;;; ToDo:

;; - highlight paragraphs, functions etc... (suggestion by Daniel Lundin
;;   <daniel@emacs.org> 19 Dec 1999)
;; - provide overlay priorities
;;   (overlay-put highlight-current-line-overlay 'priority 60)
;; - better way to switch off 'ignore buffer'
;; - face fore/backgroundcolor depending on major-mode
;; - better way to detect xemacs

;; - some suggestions for default keys
;; - highlight-current-line as minor mode. Suggested by Shawn Ostermann.

;;; Change log:

;; 10 Sept 2003 - v0.57 <psg@debian.org>
;; - highlight-current-line-minor-mode created.
;; - highlight-current-line-globally defcustom added.

;; 7 Sept 2003 - v0.56
;; - defface for highlight-current-line-face with customization.
;;   Thanks to Peter S. Galbraith for the suggestion. Retained
;;   highlight-current-line-set-fg/bg-color for backward
;;   compatibility.

;; 7 Sept 2003 - v0.55
;; - v0.54 change works now correctly

;; 22 Mar 2003 - v0.54
;; - don't highlight lines which contain faces specified in
;;   highlight-current-line-high-faces. Elisp manual: "Currently, all
;;   overlays take priority over text properties." So, if a text
;;   property is a face, highlight-current-line always hides that face.

;; 12 Mar 2002 - v0.53
;; - updated email address

;; 05 Feb 2001
;; - highlight-current-line-ignore-regexp: better regexp for minibuffers

;; 15 Jul 2000 - v0.52:
;; - Masatake YAMATO: added emacsclient / gnudoit support. Invoking emacs
;;   to load a file from external, highlight-current-line couldn't
;;   initially show the line of the loaded file highlighted.

;; 19 Oct 1997 - v0.51:
;; - uses defcustom-library if available. Suggested by Jari Aalto and Hrvoje
;;   Niksic.
;; - logic error in if-condition of post-command-hook. All Buffers were
;;   ignored if highlight-current-line-ignore-function was unbound.

;; 18 Oct 1997 - v0.5:
;; - GNU General Public License
;; - ignore user-definable buffernames which are ignored for
;;   highlighting. Suggested by Jari Aalto.
;; - works with XEmacs, at least version 19.15. Mark whole line doesnt work
;;   yet. Suggested by Jari Aalto.
;; - highlight-current-line-set-fg/bg-color understand "none" as color
;; - overlay-put moved from post-command-hook to initialization-code
;; - version-variable: `highlight-current-line-version'. Always
;;   "major.minor". Suggested by Jari Aalto.

;; 11 Oct 1997 - v0.4:
;; - Possibility to highlight whole line (from left to right windowborder) or
;;   only from left window border to the last char in the line.
;;
;; 20 Aug 1997 - v0.3:
;; - First public released version.

;;; Code:

;; Initialization for XEmacs

;; XEmacs needs overlay emulation package.
;; Old XEmacs won't have the package and we must quit.
(eval-and-compile
  (if (boundp 'xemacs-logo)
      (if (not (load "overlay" 'noerr))
	  (error "\
highlight-current-line.el: ** This package requires overlays.  Abort"))))

;; Compatibility code - blob for those without the custom library:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can be set by user

(defgroup highlight-current-line nil
  "Highlight line where the cursor is."
  :load 'highlight-current-line
  :group 'faces) ;; or 'matching??

(defcustom highlight-current-line-ignore-regexp
  (concat
   "Faces\\|Colors\\| \\*Mini"
   ;; for example:
   ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
   )
  "*Regexps for buffers to ignore.
Used by `highlight-current-line-ignore-function'."
  :type  'regexp
  :group 'highlight-current-line)

(defcustom highlight-current-line-whole-line t
  "*If non-nil, mark up to `end-of-line'.  If nil, mark up to window-border.
Use `highlight-current-line-whole-line-on' to set this value."
  :type  'boolean
  :group 'highlight-current-line)

(defcustom highlight-current-line-high-faces '()
  "*Lines containing one of this faces are not highlighted."
  :type  'list
  :group 'highlight-current-line)

(defface highlight-current-line-face
  '((t (:background "wheat")))
    "Face used to highlight current line."
  :group 'highlight-current-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should not be set by user

(defconst highlight-current-line-version "0.57"
  "Version number." )

(defvar highlight-current-line-minor-mode nil
  "Non-nil if using highlight-current-line mode as a minor mode.
Use the command `highlight-current-line-minor-mode' to toggle or set this
variable.")
(make-variable-buffer-local 'highlight-current-line-minor-mode)

(defvar highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting.")

;; Set face-property of overlay
(overlay-put highlight-current-line-overlay
	     'face 'highlight-current-line-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal function for test
(defun highlight-current-line-reload ()
  "Reload library highlight-current-line for test purposes."
  (unload-feature 'highlight-current-line)
  (load-library "highlight-current-line"))

;; Decide whether to highlight the buffer.
(defun highlight-current-line-ignore-function  ()
  "Check current buffer name against `highlight-current-line-ignore-regexp'.
Inhibits global enabling of highlight-current-line on buffer whose name
match this regexp."
  (if (or (equal "" highlight-current-line-ignore-regexp)
	  (not highlight-current-line-ignore-regexp))
      nil
    (string-match highlight-current-line-ignore-regexp (buffer-name))))

(defvar highlight-current-line-globally)

;; Post-Command-Hook for highlighting
(defun highlight-current-line-hook ()
  "Post-Command-Hook for highlighting."
  (condition-case ()
      (if (or highlight-current-line-minor-mode
              (and highlight-current-line-globally
                   (or (not (fboundp 'highlight-current-line-ignore-function))
                       (not (highlight-current-line-ignore-function)))))
          (let ((current-point (point)))

            ;; Set overlay
            (let ((beg (progn (beginning-of-line) (point)))
                  (end (progn (if highlight-current-line-whole-line
                                  (forward-line 1)
                                (end-of-line))
                              (point))))
              (if (delete nil (mapcar
                                 (lambda( face )
                                   (text-property-any beg end 'face face))
                                 highlight-current-line-high-faces))
                  (delete-overlay highlight-current-line-overlay)
                (move-overlay highlight-current-line-overlay
                              beg end (current-buffer)))

              (goto-char current-point))))
    (error nil)))

(defconst highlight-current-line-no-color (if (boundp 'xemacs-logo)
                                              '[]
                                            nil)
  "'color' value that represents \"no color\".")

;; Compatibility code
(defun highlight-current-line-on (&optional on-off)
  "Switch highlighting of cursor-line on/off globally.
Key: \\[highlight-current-line-on]"
  (interactive (list (y-or-n-p "Highlight line with cursor? ")))
  (setq-default highlight-current-line-globally on-off)
  (highlight-current-line on-off nil))

;; Compatibility code - Set foregroundcolor of cursor-line.
(defun highlight-current-line-set-fg-color (color)
  "Set foregroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-fg-color]"
  (interactive "sForeground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-foreground 'highlight-current-line-face color))

;; Compatibility code - Set backgroundcolor of cursor-line.
(defun highlight-current-line-set-bg-color (color)
  "Set backgroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-bg-color]"
  (interactive "sBackground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-background 'highlight-current-line-face color))

;; Compatibility code - Enable/Disable whole line marking
(defun highlight-current-line-whole-line-on (&optional on-off)
  "Switch highlighting of whole line ON-OFF.
Key: \\[highlight-current-line-whole-line-on]"
  (interactive (list (y-or-n-p "Highlight whole line? ")))
  (setq highlight-current-line-whole-line on-off))

;; Enable/Disable Highlighting
(defun highlight-current-line (&optional on-off local)
  "Switch highlighting of cursor-line ON-OFF
If LOCAL is non-nil, do so locally for the current buffer only."
  (cond
   (on-off
    (if (or (= emacs-major-version 20)
            (string-match "XEmacs" emacs-version))
        (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'highlight-current-line-hook nil local)
    (if (boundp 'server-switch-hook)
        (add-hook 'server-switch-hook 'highlight-current-line-hook nil local))
    (if (boundp 'gnuserv-visit-hook)
        (add-hook 'gnuserv-visit-hook 'highlight-current-line-hook nil local)))
   (t
    (if  (boundp 'server-switch-hook)
        (remove-hook 'server-switch-hook 'highlight-current-line-hook local))
    (if (boundp 'gnuserv-visit-hook)
        (remove-hook 'gnuserv-visit-hook 'highlight-current-line-hook local))
    (remove-hook 'post-command-hook 'highlight-current-line-hook t)
    (delete-overlay highlight-current-line-overlay))))

;;;###autoload
(defun highlight-current-line-minor-mode (&optional arg)
  "Toggle highlight-current-line minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-current-line."
  (interactive "P")
  (setq highlight-current-line-minor-mode
        (if (null arg)
            (not highlight-current-line-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if highlight-current-line-minor-mode
      (highlight-current-line t t)
    (highlight-current-line nil t)))

(or (assq 'highlight-current-line-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  (list '(highlight-current-line-minor-mode " hcl")))))

(defcustom highlight-current-line-globally nil
  "*Whether to enable `highlight-current-line-minor-mode' automatically.
This affects only files visited after this variable is set.
Buffers will not be enabled if they match the regular expression in
`highlight-current-line-ignore-regexp'."
  :type  'boolean
  :require 'highlight-current-line
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (highlight-current-line t nil)
           (highlight-current-line nil nil)))
  :group 'highlight-current-line)

(provide 'highlight-current-line)

;;; highlight-current-line.el ends here

;;; nxhtml-base.el --- The very, very basic vars...
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-01-13 Wed
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Things that always must be loaded and that are often necessary when
;; byte compiling.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;(eval-when-compile (require 'web-vcs nil t))
(eval-when-compile (require 'flymake-js nil t))
(eval-when-compile (require 'flymake-css nil t))
(eval-when-compile (require 'flymake-java-1 nil t))

(defconst nxhtml-menu:version "2.08")
(setq message-log-max t)
(setq debug-on-error t)

(defconst nxhtml-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory for nXhtml.")

(define-minor-mode nxhtml-autoload-web
  "If on download elisp files from web when they are needed.
If t then during `require' nXhtml elisp files can be downloaded
from the nXhtml repository on the web.  This will currently
download the development sources, latest version.

Other files that are used by a command may also be downloaded.

Note that files are not updated automatically.  You have to use
`nxhtml-update-existing-files' for that."
  :global t
  ;;:lighter (propertize " nX" 'face 'font-lock-comment-face)
  :lighter " nX"
  :group 'nxhtml)

(defun nxhtml-autoload (fun src &optional docstring interactive type)
  "Generalized `autoload'. May setup autoload from the web.
If `nxhtml-autoload-web' is t then setup autoloading from the web.
Otherwise setup for normal local autoloading."
  (if nxhtml-autoload-web
      (progn
        ;; Do not require this until we really need it.
        (require 'web-autoload)
        (web-autoload fun src docstring interactive type))
    (let ((file src))
      (when (listp file)
        (setq file (file-name-nondirectory (nth 2 file))))
      (autoload fun file docstring interactive type))))

;; Fix-me: web autoload defcustoms.
;;
;; I have no good idea how to fix this. It looks like I have to
;; defadvice `custom-load-symbol'. I thought that should not be
;; necessary since it does (require load) on line 605 but the web
;; autoload does not start. Why? Hm, you never know since it is inside
;; a (condition-case nil ...).
;;
;; Ah, found it. The require is only done if custom loads contains a
;; symbol, not a string. So I changed this to a symbol instead in
;; nxhtml-loaddefs.el. Maybe `load' instead of `require' should be
;; advised?

;; What a hell is this below? Have things been rewritten in custom or
;; did I mix somethintg?
(defun nxhtml-custom-autoload (symbol load &optional noset)
  "Like `custom-autoload', but also run :set for defcustoms etc."
  ;; Fix-me: is-boundp is currently always t because of the order in
  ;; loaddefs. Hm, so this worked just by chance...
  (let* ((is-boundp (prog1 (boundp symbol)
                      (custom-autoload symbol load noset)))
         (standard (get symbol 'standard-value))
         (saved (get symbol 'saved-value))
         ;; Fix-me: property custom-set etc are not available
         (custom-set (get symbol 'custom-set))
         (custom-initialize (get symbol 'custom-initialize))
         (set (or custom-set 'custom-set-default))) ;; Fix-me: initialize
    (setq custom-set t) ;; Not available here
    (when (or custom-initialize
              (and saved
                   (not (equal (car saved) (symbol-value symbol)))
                   custom-set))
      (funcall set symbol (car saved))
      (custom-load-symbol symbol))))

(defun flymake-init-load-flymakemsg ()
  (require 'flymakemsg))

(define-minor-mode nxhtml-flymake-setup
  "Let nXhtml add some addtions to flymake.
This adds support for CSS and JavaScript files.

It also adds showing of errors in minibuffer when point is on
them.

If you turn this off you must restart Emacs for it to take
effect."
  :group 'nxhtml
  :group 'flymake
  (when nxhtml-flymake-setup
    (flymake-js-load)
    (flymake-css-load)
    (flymake-java-1-load)
    (add-hook 'flymake-mode-hook 'flymake-init-load-flymakemsg)))


(provide 'nxhtml-base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-base.el ends here

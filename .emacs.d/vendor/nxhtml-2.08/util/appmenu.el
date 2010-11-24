;;; appmenu.el --- A framework for [apps] popup menus.

;; Copyright (C) 2008 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman AT gmail DOT com>
;; Created: Thu Jan 05 14:00:26 2006
(defconst appmenu:version "0.63") ;; Version:
;; Last-Updated: 2010-01-04 Mon
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
;;  appmenu.el is a framework for creating cooperative context
;;  sensitive popup menus with commands from different major and minor
;;  modes.  For more information see `appmenu-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 0.61:
;; - Remove support for minor and major menus.
;; - Add support for text and overlay keymaps.
;; - Add customization options.
;;
;; Version 0.62:
;; - Fix problem with keymap at point.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'flyspell))
(eval-when-compile (require 'help-mode))
(eval-when-compile (require 'ourcomments-util nil t))
(eval-when-compile (require 'mumamo nil t))
;;(eval-when-compile (require 'mlinks nil t))

;;;###autoload
(defgroup appmenu nil
  "Customization group for `appmenu-mode'."
  :group 'convenience)

(defcustom appmenu-show-help nil
  "Non-nil means show AppMenu help on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

(defcustom appmenu-show-point-menu t
  "If non-nil show entries fetched from keymaps at point."
  :type 'boolean
  :group 'appmenu)

(defvar appmenu-alist nil
  "List of additional menu keymaps.
To change this list use `appmenu-add' and `appmenu-remove'.

The entries in this list are lists:

   \(ID PRIORITY TEST TITLE DEFINITION)

ID is a unique identity.

PRIORITY is a number or a variable whose value is a number
telling where to put this entry when showing the menu.

TEST should be a form to evaluate.  The entry is used if \(eval
TEST) returns non-nil.

DEFINITION should be either a keymap or a function that returns a
keymap.

The function must take no argument and return a keymap.  If the
function returns nil then the entry is not shown in the popup
menu.  Using this you can make context sensitive popup menus.

For an example of use see mlinks.el.")

(defun appmenu-sort-by-priority ()
  "Sort `appmenu-alist' entries by priority."
  (setq appmenu-alist
        (sort appmenu-alist
              (lambda (recA recB)
                (let ((priA (nth 1 recA))
                      (priB (nth 1 recB)))
                  (when (symbolp priA) (setq priA (symbol-value priA)))
                  (when (symbolp priB) (setq priB (symbol-value priB)))
                  (< priA priB))))))

;;;###autoload
(defun appmenu-add (id priority test title definition)
  "Add entry to `appmenu-alist'.
Add an entry to this list with ID, PRIORITY, TEST, TITLE and
DEFINITION as explained there."
  (assert (symbolp id))
  (unless priority (setq priority 100))
  (assert (numberp priority))
  (assert (stringp title))
  (let ((rec (list id priority test title definition)))
    (appmenu-remove id)
    (add-to-list 'appmenu-alist rec)))

(defun appmenu-remove (id)
  "Remove entry with id ID from `appmenu-alist'."
  (setq appmenu-alist (assq-delete-all id appmenu-alist)))

(defun appmenu-help ()
  "Show help for minor mode function `appmenu-mode'."
  (interactive)
  (describe-function 'appmenu-mode))

(defun appmenu-keymap-len (map)
  "Return length of keymap MAP."
  (let ((ml 0))
    (map-keymap (lambda (e f) (setq ml (1+ ml))) map)
    ml))

(defvar appmenu-mouse-only
  '((flyspell-correct-word appmenu-flyspell-correct-word-before-point)))

(defun appmenu-flyspell-correct-word-before-point ()
  "Pop up a menu of possible corrections for misspelled word before point.
Special version for AppMenu."
  (interactive)
  (flyspell-correct-word-before-point))

(defcustom appmenu-at-any-point '(ispell-word)
  "Commands that may work at any point in a buffer.
Some important but not too often used commands that may be useful
for most points in a buffer."
  :group 'appmenu)

(defvar appmenu-map-fun) ;; dyn var, silence compiler

(defun appmenu-make-menu-for-point (this-point)
  "Construct a menu based on point THIS-POINT.
This includes some known commands for point and keymap at
point."
  (let ((point-map (get-char-property this-point 'keymap))
        (funs appmenu-at-any-point)
        (map (make-sparse-keymap "At point"))
        (num 0)
        last-prefix
        this-prefix)
    ;; Known for any point
    (when point-map
      (let ((appmenu-map-fun
             (lambda (key fun)
               (if (keymapp fun)
                   (map-keymap appmenu-map-fun fun)
                 (when (and (symbolp fun)
                            (fboundp fun))
                   (let ((mouse-only (assq fun appmenu-mouse-only)))
                     (when mouse-only
                       (setq fun (cadr mouse-only)))
                     (add-to-list 'funs fun)))))))
        (map-keymap appmenu-map-fun point-map)))
    (dolist (fun funs)
      (let ((desc (when fun (documentation fun))))
        (when desc
          (setq desc (car (split-string desc "[\n]")))
          ;;(lwarn t :warning "pk: %s, %s" fun desc)
          (setq this-prefix
                (car (split-string (symbol-name fun) "[-]")))
          (when (and last-prefix
                     (not (string= last-prefix this-prefix)))
            (define-key map
              (vector (intern (format "appmenu-point-div-%s" num)))
              (list 'menu-item "--")))
          (setq last-prefix this-prefix)
          (setq num (1+ num))
          (define-key map
            (vector (intern (format "appmenu-point-%s" num)))
            (list 'menu-item desc fun)))))
    (when (> num 0) map)))

(defvar appmenu-level) ;; dyn var
(defvar appmenu-funs) ;; dyn var
(defvar appmenu-events) ;; dyn var
(defvar appmenu-this-point) ;; dyn var

(defun appmenu-keymap-map-fun (ev def)
  (if (keymapp def)
        (progn
          (add-to-list 'appmenu-funs (list appmenu-level ev))
          (setq appmenu-events (cons ev appmenu-events))
          (setq appmenu-level (1+ appmenu-level))

          (map-keymap 'appmenu-keymap-map-fun def)

          (setq appmenu-events (cdr appmenu-events))
          (setq appmenu-level (1- appmenu-level)))
      (when (and (symbolp def)
                 (fboundp def))
        (let* ((mouse-only (assq def appmenu-mouse-only))
               (fun (if mouse-only (cadr mouse-only) def))
               (doc (when fun
                      (if (not (eq fun 'push-button))
                          (documentation fun)
                        (concat
                         "Button: "
                         (with-current-buffer (marker-buffer appmenu-this-point)
                           (or (get-char-property appmenu-this-point 'help-echo)
                               (let ((action-fun (get-char-property appmenu-this-point 'action)))
                                 (if action-fun
                                     (documentation action-fun)
                                   "No action, ignored"))
                               "No documentation available")))))))
          (add-to-list 'appmenu-funs (list appmenu-level (cons ev appmenu-events) def doc))))))

;;(appmenu-as-help (point))
(defun appmenu-as-help (this-point)
  "Show keybindings specific done current point in buffer.
This shows the binding in the help buffer.

Tip: This may be helpful if you are using `css-color-mode'."
  (interactive (list (copy-marker (point))))
  ;; Split this for debugging
  (let ((menu-here
         (with-current-buffer (or (and (markerp this-point)
                                       (marker-buffer this-point))
                                  (current-buffer))
           (unless (markerp this-point) (setq this-point (copy-marker this-point)))
           (get-char-property this-point 'keymap))))
    ;;(describe-variable 'menu-here)
    (appmenu-as-help-1 menu-here this-point)))

(defun appmenu-as-help-1 (menu-here this-point)
  (let ((appmenu-level 0)
        (appmenu-funs nil)
        (appmenu-events nil)
        (appmenu-this-point this-point))
    (when menu-here
      (map-keymap 'appmenu-keymap-map-fun menu-here))
    ;;(describe-variable 'appmenu-funs)
    ;; Fix-me: collect info first in case we are in help-buffer!
    (with-output-to-temp-buffer (help-buffer)
      (help-setup-xref (list #'appmenu-as-help this-point) (interactive-p))
      (with-current-buffer (help-buffer)
        (let ((fmt " %s%15s     %-30s\n"))
          (insert (propertize
                   ;;"AppMenu: Keys found at point in buffer\n\n"
                   (format "Appmenu: Key bindings specific to point %s in buffer %S\n\n"
                           (+ 0 this-point)
                           (when (markerp this-point)
                             (buffer-name (marker-buffer this-point))))
                   'face 'font-lock-comment-face))
          (if (not menu-here)
              (insert "\n\nThere are no point specific key bindings there now.")
            (insert (propertize (format fmt "" "Key" "Function") 'face 'font-lock-function-name-face))
            (insert (propertize (format fmt "" "---" "--------") 'face 'font-lock-function-name-face))
            (dolist (rec appmenu-funs)
              (let* ((lev (nth 0 rec))
                     (ev  (nth 1 rec))
                     (fun (nth 2 rec))
                     (doc (nth 3 rec))
                     (d1  (when doc (car (split-string doc "[\n]")))))
                (if fun
                    (insert (format fmt
                                    "" ;;(concat "*" (make-string (* 4 lev) ?\ ))
                                    (key-description (reverse ev))
                                    d1)
                            (if nil (format "(%s)" fun) ""))
                  ;;(insert (format "something else=%S\n" rec))
                  )))))))))


(defun appmenu-map ()
  "Return menu keymap to use for popup menu."
  (let* ((map (make-sparse-keymap
               "AppMenu"
               ))
         (map-len (appmenu-keymap-len map))
         (map-init-len map-len)
         (num-minor 0)
         (id 0)
         (point-menu (when appmenu-show-point-menu
                       (appmenu-make-menu-for-point (point)))))
    ;; AppMenu itself
    (when appmenu-show-help
      (define-key map [appmenu-customize]
        (list 'menu-item "Customize AppMenu"
              (lambda () (interactive) (customize-group 'appmenu))
              :help "Customize AppMenu"
              :visible 'appmenu-show-help))
      (define-key map [appmenu-help]
        (list 'menu-item "Help for AppMenu" 'appmenu-help
              :help "Help for how to use AppMenu"
              :visible 'appmenu-show-help))
      (define-key map [appmenu-separator-1]
        (list 'menu-item "--")))
    (setq map-len (appmenu-keymap-len map))
    (appmenu-sort-by-priority)
    (dolist (rec appmenu-alist)
      (let* ((test   (nth 2 rec))
             (title  (nth 3 rec))
             (mapdef (nth 4 rec))
             (usedef (if (symbolp mapdef)
                         (funcall mapdef)
                       mapdef)))
        (when (and usedef
                   (eval test))
          (setq id (1+ id))
          (define-key map
            (vector (intern (format "appmenu-%s" id)))
            (list 'menu-item title usedef)))
        ))
    (when point-menu
      (setq map-len (appmenu-keymap-len map))
      (when (> map-len map-init-len)
        (define-key map [appmenu-at-point-div]
          (list 'menu-item "--")))
      (define-key map [appmenu-at-point]
        (list 'menu-item "Bound To Point"
              point-menu)))
    (setq map-len (appmenu-keymap-len map))
    (when (> map-len map-init-len)
      map)))

;; (defun appmenu-get-submenu (menu-command)
;;   (let (subtitle submenumap)
;;     (if (eq 'menu-item (car menu-command))
;;         (progn (setq subtitle   (cadr  menu-command))
;;                (setq submenumap (caddr menu-command)))
;;       (setq subtitle   (car menu-command))
;;       (setq submenumap (cdr menu-command)))
;;     (unless (keymapp submenumap) (error "Submenu not a keymap=%s" submenumap))
;;     (cons subtitle submenumap)))

(defun appmenu-popup ()
  "Pops up the AppMenu menu."
  (interactive)
  (let* ((mod (event-modifiers last-input-event))
         (is-mouse (or (memq 'click mod)
                       (memq 'down  mod)
                       (memq 'drag  mod))))
    (when is-mouse
      (goto-char (posn-point (event-start last-input-event)))
      (sit-for 0.01))
    (let ((menu (appmenu-map)))
      (if menu
          (popup-menu-at-point menu)
        (message "Appmenu is empty")))))

(defvar appmenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [apps]         'appmenu-popup)
    (define-key map [mouse-3]      'appmenu-popup)
    (define-key map [(control apps)] 'appmenu-as-help)
    map))


;;(setq appmenu-auto-help 4)
(defcustom appmenu-auto-help 2
  "Automatically show help on keymap at current point.
This shows up after the number of seconds in this variable.
If it it nil this feature is off.

This feature is only on in `appmenu-mode'."
  :type '(choice (number :tag "Number of seconds to wait")
                 (const :tag "Turned off" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-hook 'post-command-hook 'appmenu-auto-help-post-command nil t)
           (remove-hook 'post-command-hook 'appmenu-auto-help-post-command t)))
  :group 'appmenu)

(defcustom appmenu-auto-match-keymaps
  '(css-color)
  "Keymaps listed here can be avoided."
  :type '(set (const unknown)
              (const mlink)
              (const css-color))
  :group 'appmenu)

(defvar appmenu-auto-help-timer nil)

(defun appmenu-dump-keymap (km)
  (let ((fun (lambda (ev def)
               (message "ev=%S def=%S" ev def)
               (when (keymapp def)
                 (map-keymap fun def)))))
    (map-keymap fun km)))

(defun appmenu-on-keymap (where)
  (setq where (or where (point)))
  (let* ((rec (get-char-property-and-overlay where 'keymap))
         (kmp (car rec))
         (ovl (cdr rec)))
    (when kmp
      (or (memq 'unknown appmenu-auto-match-keymaps)
          (and (memq 'css-color appmenu-auto-match-keymaps)
               (get-text-property where 'css-color-type))
          (and (memq 'mlinks appmenu-auto-match-keymaps)
               (boundp 'mlinks-point-hilighter-overlay)
               (eq ovl mlinks-point-hilighter-overlay))
          ))))

(defsubst appmenu-auto-help-add-wcfg (at-point wcfg)
  (mumamo-with-buffer-prepared-for-jit-lock
   (add-text-properties at-point (1+ at-point)
                        (list 'point-left 'appmenu-auto-help-maybe-remove
                              'appmenu-auto-help-wcfg wcfg))))

(defsubst appmenu-auto-help-remove-wcfg (at-point)
  (mumamo-with-buffer-prepared-for-jit-lock
   (remove-list-of-text-properties at-point (1+ at-point)
                                   '(appmenu-auto-help-wcfg point-left))))

(defun appmenu-auto-help-maybe-remove (at-point new-point)
  "Run in 'point-left property.
Restores window configuration."
  (let ((old-wcfg (get-text-property at-point 'appmenu-auto-help-wcfg)))
    (appmenu-auto-help-remove-wcfg at-point)
    (if (appmenu-on-keymap new-point)
        (appmenu-auto-help-add-wcfg new-point old-wcfg)
      (if old-wcfg
          (set-window-configuration old-wcfg)
        (help-xref-go-back (help-buffer))))))

(defun appmenu-as-help-in-timer (win buf)
  (condition-case err
      (when (and (eq (selected-window) win)
                 (eq (current-buffer) buf)
                 appmenu-auto-help
                 (appmenu-on-keymap (point)))
        (let* ((old-help-win (get-buffer-window (help-buffer)))
               (wcfg (unless old-help-win
                      (current-window-configuration))))
          (unless old-help-win
            (display-buffer (help-buffer)))
          (appmenu-auto-help-add-wcfg (point) wcfg)
          (appmenu-as-help (copy-marker (point)))))
    (error (message "appmenu-as-help-in-timer: %s" (error-message-string err)))))

(defun appmenu-auto-help-cancel-timer ()
  (when (timerp appmenu-auto-help-timer)
    (cancel-timer appmenu-auto-help-timer))
  (setq appmenu-auto-help-timer nil))

(defun appmenu-auto-help-post-command ()
  (when (fboundp 'appmenu-as-help)
    (condition-case err
        (appmenu-auto-help-post-command-1)
      (error (message "css-color-post-command: %s" (error-message-string err))))))

;; #fff  #c9ff33
(defun appmenu-auto-help-post-command-1 ()
  (appmenu-auto-help-cancel-timer)
  (and appmenu-auto-help
       (appmenu-on-keymap (point))
       (not (get-text-property (point) 'appmenu-auto-help-wcfg))
       (setq appmenu-auto-help-timer
             (run-with-idle-timer appmenu-auto-help nil 'appmenu-as-help-in-timer
                                  (selected-window)
                                  (current-buffer)))))


;;;###autoload
(define-minor-mode appmenu-mode
  "Use a context sensitive popup menu.
AppMenu (appmenu.el) is a framework for creating cooperative
context sensitive popup menus with commands from different major
and minor modes. Using this different modes may cooperate about
the use of popup menus.

There is also the command `appmenu-as-help' that shows the key
bindings at current point in the help buffer.

The popup menu and the help buffer version are on these keys:

\\{appmenu-mode-map}

The variable `appmenu-alist' is where the popup menu entries
comes from.

If there is a `keymap' property at point then relevant bindings
from this is also shown in the popup menu.

You can write functions that use whatever information you want in
Emacs to construct these entries. Since this information is only
collected when the popup menu is shown you do not have to care as
much about computation time as for entries in the menu bar."
  :global t
  :keymap appmenu-mode-map
  :group 'appmenu
  (if appmenu-mode
      (add-hook 'post-command-hook 'appmenu-auto-help-post-command)
    (remove-hook 'post-command-hook 'appmenu-auto-help-post-command)))

(when (and appmenu-mode
           (not (boundp 'define-globa-minor-mode-bug)))
  (appmenu-mode 1))

(provide 'appmenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appmenu.el ends here

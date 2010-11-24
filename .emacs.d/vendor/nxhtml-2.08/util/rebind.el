;;; rebind.el --- Rebind keys
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-20T12:04:37+0100 Sun
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
;; See `rebind-keys-mode' for information.
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
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile (require 'new-key-seq-widget nil t))
(eval-when-compile (require 'ourcomments-widgets nil t))


(defun rebind-toggle-first-modifier (orig-key-seq mod)
  (let* ((first (elt orig-key-seq 0))
         (new-key-seq (copy-sequence orig-key-seq)))
    (setq first (if (memq mod first)
                    (delq mod first)
                  (cons mod first)))
    (aset new-key-seq 0 first)
    new-key-seq))
;; (rebind-toggle-first-modifier (key-description-to-vector "C-c a") 'shift)
;; (rebind-toggle-first-modifier (key-description-to-vector "C-S-c a") 'shift)

(defvar widget-commandp-prompt-value-history nil)

;;;###autoload
(defgroup rebind nil
  "Customizaton group for `rebind-keys-mode'."
  :group 'convenience
  :group 'emulations
  :group 'editing-basics
  :group 'emacsw32)

;; (customize-option-other-window 'rebind-keys)
;; (Fetched key bindings from http://www.davidco.com/tips_tools/tip45.html)
(defcustom rebind-keys
  '(
    ("MS Windows - often used key bindings" t
      (
       (
        [(control ?a)]
        "C-a on w32 normally means 'select all'. In Emacs it is `beginning-of-line'."
        t
        shift
        ourcomments-mark-whole-buffer-or-field)
      (
       [(control ?o)]
       "C-o on w32 normally means 'open file'. In Emacs it is `open-line'."
       nil
       shift
       find-file)
      (
       [(control ?f)]
       "C-f is commonly search on w32. In Emacs it is `forward-char'."
       nil
       shift
       isearch-forward)
      (
       [(control ?s)]
       "C-s is normally 'save file' on w32. In Emacs it is `isearch-forward'."
       nil
       nil
       save-buffer)
      (
       [(control ?w)]
       "C-w is often something like kill-buffer on w32. In Emacs it is `kill-region'."
       t
       shift
       kill-buffer)
      (
       [(control ?p)]
       "C-p is nearly always print on w32. In Emacs it is `previous-line'."
       t
       shift
       hfyview-buffer)
      (
       [(home)]
       "HOME normally stays in a field. By default it does not do that in Emacs."
       t
       nil
       ourcomments-move-beginning-of-line)
      (
       [(control ?+)]
       "C-+ often increases font size (in web browsers for example)."
       t
       shift
       text-scale-adjust)
      (
       [(control ?-)]
       "C-- often decreases font size (in web browsers for example)."
       t
       shift
       text-scale-adjust)
      (
       [(control ?0)]
       "C-0 often resets font size (in web browsers for example)."
       t
       shift
       text-scale-adjust)
       )))
  "Normal Emacs keys that are remapped to follow some other standard.
The purpose of this variable is to make it easy to switch between
Emacs key bindings and other standards.

The new bindings are made in the global minor mode
`rebind-keys-mode' and will only have effect when this mode is
on.

*Note:* You can only move functions bound in the global key map
        this way.
*Note:* To get CUA keys you should turn on option `cua-mode'.
*Note:* To get vi key bindings call function `viper-mode'.
*Note:* `text-scale-adjust' already have default key bindings."
  :type '(repeat
          (list
           (string :tag "For what")
           (boolean :tag "Group on/off")
           (repeat
            (list
             (key-sequence :tag "Emacs key binding")
             (string :tag "Why rebind")
             (boolean :tag "Rebinding on/off")
             (choice :tag "Move original by"
                     (const :tag "Don't put it on any new binding" nil)
                     (choice :tag "Add key binding modifier"
                             (const meta)
                             (const control)
                             (const shift))
                     (key-sequence :tag "New binding for original function"))
             (command :tag "New command on above key"))
            )))
  :set (lambda (sym val)
	 (set-default sym val)
	 (when (featurep 'rebind)
	   (rebind-update-keymap)))
  :group 'rebind)

(defvar rebind-keys-mode-map nil)

(defvar rebind--emul-keymap-alist nil)

;;(rebind-update-keymap)
(defun rebind-update-keymap ()
  (let ((m (make-sparse-keymap)))
    (dolist (group rebind-keys)
      (when (nth 1 group)
        (dolist (v (nth 2 group))
          (let* ((orig-key   (nth 0 v))
                 (comment    (nth 1 v))
                 (enabled    (nth 2 v))
                 (new-choice (nth 3 v))
                 (new-fun    (nth 4 v))
                 (orig-fun (lookup-key global-map orig-key))
                 new-key)
            (when enabled
              (when new-choice
                (if (memq new-choice '(meta control shift))
                    (setq new-key (rebind-toggle-first-modifier orig-key new-choice))
                  (setq new-key new-choice))
                (define-key m new-key orig-fun))
              (define-key m orig-key new-fun))))
        (setq rebind-keys-mode-map m))))
  (setq rebind--emul-keymap-alist (list (cons 'rebind-keys-mode rebind-keys-mode-map))))

;;;###autoload
(define-minor-mode rebind-keys-mode
  "Rebind keys as defined in `rebind-keys'.
The key bindings will override almost all other key bindings
since it is put on emulation level, like for example ``cua-mode'
and `viper-mode'.

This is for using for example C-a to mark the whole buffer \(or a
field). There are some predifined keybindings for this."
  :keymap rebind-keys-mode-map
  :global t
  :group 'rebind
  (if rebind-keys-mode
      (progn
        (rebind-update-keymap)
        ;;(rebind-keys-post-command)
        (add-hook 'post-command-hook 'rebind-keys-post-command t))
    (remove-hook 'post-command-hook 'rebind-keys-post-command)
    (setq emulation-mode-map-alists (delq 'rebind--emul-keymap-alist emulation-mode-map-alists))))

(defun rebind-keys-post-command ()
  "Make sure we are first in the list when turned on.
This is reasonable since we are using this mode to really get the
key bindings we want!"
  (unless (eq 'rebind--emul-keymap-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists (delq 'rebind--emul-keymap-alist emulation-mode-map-alists))
    (when rebind-keys-mode
      (add-to-list 'emulation-mode-map-alists 'rebind--emul-keymap-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions for the keymap



(provide 'rebind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rebind.el ends here

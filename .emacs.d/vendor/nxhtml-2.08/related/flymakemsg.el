;;; flymakemsg.el --- Show flymake compile errors in echo area
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-21 Sat
;; Version: 0.1
;; Last-Updated: 2009-11-21 Sat
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `backquote', `bytecomp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Show flymake error messages in minibuffer when point is on a
;; flymake error overlay.
;;
;; To use it just load this file. Put this in .emacs:
;;
;;   (require 'flymakemsg)
;;
;; This file run `defadvice' on some functions in `flymake-mode'.
;; This code started from an idea in a paste.
;;
;; Note: This code breaks Emacs conventions since it does the
;; defadvising when you just loads this file.
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

(eval-when-compile (require 'flymake))

(defun flymakemsg-show-err-at-point ()
  "If point is on a flymake error, show it in echo area.
Protected to run in timers and hooks."
  (condition-case err
      (flymakemsg-show-err-at-point-1)
    (error (message "%s" err))))

(defvar flymakemsg-last-errovl nil)

(defun flymakemsg-show-err-at-point-1 ()
  "If point is on a flymake error, show it in echo area."
  (interactive)
  (when flymake-mode
    (let ((flyovl (flymakemsg-get-errovl (point))))
      (unless (eq flyovl flymakemsg-last-errovl)
        (setq flymakemsg-last-errovl flyovl)
        (when flyovl
          (message "%s" (propertize
                                    (overlay-get flyovl 'help-echo)
                                    'face 'flymake-errline)))))))

(defun flymakemsg-get-errovl (POS)
  "Get flymake error overlay at POS."
  (catch 'errovl
    (dolist (ovl (overlays-at POS))
      (when (eq 'flymake-errline (overlay-get ovl 'face))
        (throw 'errovl ovl)))))

(defadvice flymake-mode (after
                         flymakemsg-ad-flymake-mode
                         activate compile)
  "Turn on showing of flymake errors then point is on them.
This shows the error in the echo area."
  (if flymake-mode
      nil ;;(add-hook 'post-command-hook 'flymakemsg-post-command t t)
    (remove-hook 'post-command-hook 'flymakemsg-post-command t)))

(defadvice flymake-log (after
                        flymakemsg-ad-flymake-log
                        activate compile)
  "Display error on current line if any."
  ;;(message "flymake-log defadvice called")
  (if (not flymake-err-info)
      (remove-hook 'post-command-hook 'flymakemsg-post-command t)
    (add-hook 'post-command-hook 'flymakemsg-post-command t t)
    ;; Wait, because there is another message first.
    (flymakemsg-start-msg-timer 3.0)))

(defun flymakemsg-post-command ()
  ;; Wait to not disturb to much.
  (flymakemsg-start-msg-timer 0.2))

(defvar flymakemsg-msg-timer nil)

(defun flymakemsg-cancel-msg-timer ()
  (when (timerp flymakemsg-msg-timer)
    (cancel-timer flymakemsg-msg-timer)))

(defun flymakemsg-start-msg-timer (delay)
  (flymakemsg-cancel-msg-timer)
  (run-with-idle-timer delay nil 'flymakemsg-show-err-at-point))

;;; I have no idea why it was done the way below. It was in the paste.
;;; It seems very unnecessary but I keep it for now.
;;
;; (defun fly-pyflake-determine-message (err)
;;   "pyflake is flakey if it has compile problems, this adjusts the
;; message to display, so there is one ;)"
;;   (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
;; 	((null (flymake-ler-file err))
;; 	 ;; normal message do your thing
;; 	 (flymake-ler-text err))
;; 	(t ;; could not compile err
;; 	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

;; (let ((line-no (line-number-at-pos)))
;;   (dolist (elem flymake-err-info)
;;     (if (eq (car elem) line-no)
;;         (let ((err (car (second elem))))
;;           (message "%s" (fly-pyflake-determine-message err))))))


(provide 'flymakemsg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymakemsg.el ends here

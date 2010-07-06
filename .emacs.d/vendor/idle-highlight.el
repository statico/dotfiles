;;; idle-highlight.el --- highlight the word the point is on

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Version: 1.0
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Based on some snippets by fledermaus from the #emacs channel.

;; M-x idle-highlight sets an idle timer that highlights all
;; occurences in the buffer of the word under the point.

;; Enabling it in a hook is recommended. But you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-coding-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (idle-highlight))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

(require 'thingatpt)

(defvar idle-highlight-last-word nil
  "Last word to be idle-highlighted.")

(defvar idle-highlight-timer nil
  "Timer to activate re-highlighting.")

(defun idle-highlight-word-at-point ()
  "Highlight the word under the point."
  (let* ((target-symbol (symbol-at-point))
         (target (symbol-name target-symbol)))
    (when idle-highlight-last-word
      (unhighlight-regexp (concat "\\<"
                                  (regexp-quote idle-highlight-last-word)
                                  "\\>")))
    (when (and idle-highlight-timer target target-symbol
               ;; TODO: no need to highlight keywords like if
               (not (in-string-p)) (not (equal target "end")))
      (highlight-regexp (concat "\\<" (regexp-quote target) "\\>") 'region)
      (setq idle-highlight-last-word target))))

;;;###autoload
(defun idle-highlight (&optional arg)
  "Toggle idle-highlighting."
  (interactive "P")
  (if (and (boundp 'idle-highlight-timer)
           idle-highlight-timer)
      (progn
        (cancel-timer idle-highlight-timer)
        (setq idle-highlight-timer nil))
    (set (make-local-variable 'idle-highlight-last-word) nil)
    (set (make-local-variable 'idle-highlight-timer)
         (run-with-idle-timer 0.5 :repeat 'idle-highlight-word-at-point))))

(provide 'idle-highlight)
;;; idle-highlight.el ends here

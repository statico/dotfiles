;;; ourcomments-widgets.el --- widgets for custom etc
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-10-13 Tue
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
;;
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

(eval-when-compile (require 'mumamo nil t))

;;;###autoload (autoload 'command "ourcomments-widgets")
(define-widget 'command 'restricted-sexp
  "A command function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'commandp))
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'commandp
  :prompt-history 'widget-command-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(commandp)
  :validate (lambda (widget)
              (unless (commandp (widget-value widget))
                (widget-put widget :error (format "Invalid command: %S"
                                                  (widget-value widget)))
                widget))
  :value 'ignore
  :tag "Command")


;;;###autoload
(defun major-or-multi-majorp (value)
  "Return t if VALUE is a major or multi major mode function."
  (or (and (fboundp 'mumamo-multi-major-modep)
           (fboundp (mumamo-multi-major-modep value)))
      (major-modep value)))

;; Fix-me: This might in the future be defined in Emacs.
;;;###autoload
(defun major-modep (value)
  "Return t if VALUE is a major mode function."
  (let ((sym-name (symbol-name value)))
    ;; Do some reasonable test to find out if it is a major mode.
    ;; Load autoloaded mode functions.
    ;;
    ;; Fix-me: Maybe test for minor modes? How was that done?
    (when (and (fboundp value)
               (commandp value)
               (not (memq value '(flyspell-mode
                                  isearch-mode
                                  savehist-mode
                                  )))
               (< 5 (length sym-name))
               (string= "-mode" (substring sym-name (- (length sym-name) 5)))
               (if (and (listp (symbol-function value))
                        (eq 'autoload (car (symbol-function value))))
                   (progn
                     (message "loading ")
                     (load (cadr (symbol-function value)) t t))
                 t)
               (or (memq value
                         ;; Fix-me: Complement this table of known major modes:
                         '(fundamental-mode
                           xml-mode
                           nxml-mode
                           nxhtml-mode
                           css-mode
                           javascript-mode
                           espresso-mode
                           php-mode
                           ))
                   (and (intern-soft (concat sym-name "-hook"))
                        ;; This fits `define-derived-mode'
                        (get (intern-soft (concat sym-name "-hook")) 'variable-documentation))
                   (progn (message "Not a major mode: %s" value)
                          ;;(sit-for 4)
                          nil)
                   ))
      t)))

;;;###autoload (autoload 'major-mode-function "ourcomments-widgets")
(define-widget 'major-mode-function 'function
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'major-or-multi-majorp))
  :prompt-match 'major-or-multi-majorp
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(major-or-multi-majorp)
  :validate (lambda (widget)
              (unless (major-or-multi-majorp (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Major mode function")



(provide 'ourcomments-widgets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ourcomments-widgets.el ends here

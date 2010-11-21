;;; markchars.el --- Mark chars fitting certain characteristics
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-03-22 Mon
;; Version:
;; Last-Updated: 2010-03-25 Thu
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Required feature `markchars' was not provided.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Mark special chars, by default non-ascii, non-IDN chars. See
;; `markchars-mode'.
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

(require 'idn)

;;;###autoload
(defgroup markchars nil
  "Customization group for `markchars-mode'."
  :group 'convenience)

(defface markchars-light
  '((t (:underline "light blue")))
  "Light face for `markchars-mode' char marking."
  :group 'markchars)

(defface markchars-heavy
  '((t (:underline "magenta")))
  "Heavy face for `markchars-mode' char marking."
  :group 'markchars)

(defcustom markchars-face 'markchars-heavy
  "Pointer to face used for marking chars."
  :type 'face
  :group 'markchars)

;; (markchars-nonidn-fun (point-max))
;; åäö
;; character: å (229, #o345, #xe5)
;; (idn-is-recommended 229) => t
;; 152F ;	00B7 0034 ;	SL	# ( ᔯ → ·4 ) CANADIAN SYLLABICS YWE → MIDDLE DOT, DIGIT FOUR	# {source:835} ᐧ4 {[source:696]}

(defun markchars-nonidn-fun (bound)
  "Font lock matcher for non-IDN, non-ascii chars."
  (let* ((beg (catch 'beg
               (while (< (point) bound)
                 (let ((char (char-after)))
                   (unless (or (< char 256)
                               (idn-is-recommended char))
                     (throw 'beg (point)))
                   (forward-char)))))
         (end (when beg
                (catch 'end
                  (while (< (point) bound)
                    (let ((char (char-after (point))))
                      (when (or (< char 256)
                                (idn-is-recommended char))
                        (throw 'end (point)))
                      (forward-char)))))))
    (when beg
      (setq end (or end bound))
      (set-match-data (list (copy-marker beg) (copy-marker end)))
      t)))

(defcustom markchars-keywords (or (when (fboundp 'idn-is-recommended) 'markchars-nonidn-fun)
                                  "[[:nonascii:]]+")
  "Regexp or function for font lock to use for characters to mark.
By default it matches non-IDN, non-ascii chars."
  :type '(choice (const :tag "Non-ascii chars" "[[:nonascii:]]+")
                 (const :tag "Non IDN chars (Unicode.org tr39 suggestions)" markchars-nonidn-fun))
  :group 'markchars)

(defvar markchars-used-keywords nil
  "Keywords currently used for font lock.")
(put 'markchars-used-keywords 'permanent-local t)

(defun markchars-set-keywords ()
  "Set `markchars-used-keywords' from options."
  (set (make-local-variable 'markchars-used-keywords)
       (list
        (list markchars-keywords
              (list 0 '(put-text-property (match-beginning 0) (match-end 0)
                                          'face markchars-face))))))

;;;###autoload
(define-minor-mode markchars-mode
  "Mark special characters.
Which characters to mark are defined by `markchars-keywords'.

The default is to mark non-IDN, non-ascii chars with a magenta
underline.

For information about IDN chars see `idn-is-recommended'.

If you change anything in the customization group `markchars' you
must restart this minor mode for the changes to take effect."
  :group 'markchars
  :lighter " ø"
  (if markchars-mode
      (progn
        (markchars-set-keywords)
        (font-lock-add-keywords nil markchars-used-keywords))
    (font-lock-remove-keywords nil markchars-used-keywords))
  ;; Fix-me: Something like mumamo-mark-for-refontification should be in Emacs.
  (if (fboundp 'mumamo-mark-for-refontification)
      (save-restriction
        (widen)
        (mumamo-mark-for-refontification (point-min) (point-max)))
    (font-lock-fontify-buffer)))

;;;###autoload
(define-globalized-minor-mode markchars-global-mode markchars-mode
  (lambda () (markchars-mode 1))
  :group 'markchars)

(provide 'markchars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markchars.el ends here

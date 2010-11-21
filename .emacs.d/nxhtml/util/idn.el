;;; idn.el --- Recommended Identifier Profiles for IDN
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-03-24 Wed
;; Version: 0.1
;; Last-Updated: 2010-03-26 Fri
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `nxhtml-base'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Functions for handling IDN chars defined by
;; `http://www.unicode.org/reports/tr39/'.
;;
;; See `idn-is-recommended'.
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

;; Fix-me: You have to change this if you are not using nXhtml:
(require 'nxhtml-base)
(defvar uts39-datadir (expand-file-name "etc/uts39/" nxhtml-install-dir))

(defun idn-init (bv)
  (save-match-data
    (let* ((idnchars-file (expand-file-name "idnchars.txt" uts39-datadir))
           (idnchars-old (find-buffer-visiting idnchars-file))
           (idnchars-buf (or idnchars-old
                             (if (not (file-exists-p idnchars-file))
                                 (message "Can't find file %S" idnchars-file)
                               (find-file-noselect idnchars-file))))
           here
           (range-patt (rx bol
                           (group (repeat 4 (any xdigit)))
                           (optional ".."
                                     (group (repeat 4 (any xdigit))))))
           (num-idn 0))
      (when idnchars-buf
        (with-current-buffer idnchars-buf
          (setq here (point))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward range-patt nil t)
              (let* ((str-beg (match-string 0))
                     (str-end (match-string 2))
                     (beg (string-to-number str-beg 16))
                     (end (or (when str-end (string-to-number str-end 16))
                              beg)))
                ;;(message "str-beg=%S str-end=%S" str-beg str-end)
                (dotimes (ii (1+ (- end beg)))
                  (let ((num (+ ii beg)))
                    ;;(message "setting idn-char %s #%4x" num num)
                    (setq num-idn (1+ num-idn))
                    (aset bv num t))))))
          (goto-char here))
        (unless idnchars-old (kill-buffer idnchars-buf))
        (message "Found %d IDN chars" num-idn)
        t))))

(defconst idn-char-vector
  (let ((bv (make-bool-vector (* 256 256) nil)))
    (when (idn-init bv)
      ;; (string-to-number "002D" 16)
      ;; Make a quick sanity check:
      (unless (and (not (aref bv 44))
                   (aref bv 45))
        (message "idn-char-vector: Bad idn data in file idnchars.txt"))
      bv))
  "Boolean vector with recommended IDN chars.")


;;(idn-is-recommended 0)
;;(idn-is-recommended 65535)
(defsubst idn-is-recommended (char)
  "Return t if character CHAR is a recommended IDN char.
See URL `http://www.unicode.org/reports/tr39/'.

Data is initialized from the file idnchars.txt in the directory
`uts39-datadir'.  This file is fetched from the above URL."
  (aref idn-char-vector char))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below are some help functions that can be commented out.

;;(global-set-key [f9] 'idn-char-at-point)
(defun idn-char-at-point (pos)
  "Tell if char at POS is an recommended IDN char.
Default POS is current point."
  (interactive "d")
  (let* ((this-char (char-after pos))
         (recommended (idn-is-recommended this-char)))
    (message "IDN char at point: %s (#%000x)" recommended this-char)))

(defun idn-list-chars ()
  "Show all IDN chars.
For more info see `idn-is-recommended'.

Note: This may crash Emacs currently, at least on w32."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'idn-list-chars) (interactive-p))
    (with-current-buffer (help-buffer)
      (insert
       "Recommended Identifier Characters for IDN:\n\n")
      (let ((col 0)
            (cnt 0))
        (dotimes (nn (length idn-char-vector))
          (when (aref idn-char-vector nn)
            (setq cnt (1+ cnt))
            (setq col (mod (1+ col) 20))
            (when (= col 0) (insert "\n "))
            (insert " " (char-to-string nn))))
        (insert "\n\n"
                (format "There were %d IDN chars defined in `idn-char-vector'." cnt))
        ))))

(provide 'idn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idn.el ends here

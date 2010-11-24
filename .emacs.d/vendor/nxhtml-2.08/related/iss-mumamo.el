;;; iss-mumamo.el --- Defines multi major mode for Inno Setup files
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-09
;; Version: 0.3
;; Last-Updated: 2009-12-12 Sat
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `comint', `compile', `iss-mode', `ring', `tool-bar'.
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

(require 'iss-mode)
(require 'mumamo)

(defun mumamo-chunk-iss-code (pos min max)
  "Find [code]..., return range and `pascal-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX.

Note that if this section is not the last"
  (mumamo-quick-static-chunk pos min max "[code]" "{*** End of CODE **}" t 'pascal-mode t))

;;;###autoload
(define-mumamo-multi-major-mode iss-mumamo-mode
  "Turn on multiple major modes Inno Setup .iss files.
The main major mode will be `iss-mode'.
The [code] section, if any, will be in `pascal-mode'."
    ("Inno ISS Family" iss-mode
     (mumamo-chunk-iss-code
      )))

(add-to-list 'auto-mode-alist '("\\.iss\\'" . iss-mumamo-mode))

(provide 'iss-mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iss-mumamo.el ends here

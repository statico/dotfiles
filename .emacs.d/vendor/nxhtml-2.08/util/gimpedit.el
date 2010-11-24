;;; gimpedit.el --- Edit files with GIMP
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Wed May 23 14:59:50 2007
(defconst gimpedit:version "0.31") ;;Version:
;; Last-Updated: 2009-11-03 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `setup-helper', `w32-reg-iface', `w32-regdat'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Simple interface to start editing with GIMP.
;;
;; If you want to edit files from within Emacs see the doc string of
;; `gimpedit-edit-buffer'.
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

(eval-and-compile (require 'w32-regdat nil t))

;; (message "%S" (gimpedit-get-remote-command))
(defun gimpedit-get-remote-command ()
  (if (featurep 'w32-regdat)
      (save-match-data
        (let ((cmd (w32-regdat-gimp-win-remote-cmd))
              cmd-list)
          (while (< 0 (length cmd))
            (cond
             ((or (string-match (rx string-start
                                    ?\"
                                    (submatch
                                     (0+ (not (any ?\"))))
                                    ?\"
                                    (0+ space))
                                cmd)
                  (string-match (rx string-start
                                    (submatch
                                     (0+ (not (any space))))
                                    (0+ space))
                                cmd))
              (setq cmd-list (cons (match-string-no-properties 1 cmd) cmd-list))
              (setq cmd (substring cmd (match-end 0))))))
          (cadr cmd-list)))
    (if (memq system-type '(windows-nt))
        (let (prog)
          (catch 'found-prog
            (dolist (num '(2 3 4 5 6 7 8 9))
              (setq prog (concat (getenv "ProgramFiles")
                                 "\\GIMP-2.0\\bin\\gimp-2."
                                 (number-to-string num)
                                 ".exe"))
              (when (file-exists-p prog)
                (throw 'found-prog prog)))))
      "gimp")))

;;;###autoload
(defgroup gimpedit nil
  "Customization group for GIMP."
  :group 'external
  :group 'nxhtml)

(defcustom gimpedit-remote-command (gimpedit-get-remote-command)
  "Program name to use when calling GIMP remotely.
This could be be the full path to the program used when opening
files with GIMP or a just the program file name if it is in the
executables path.

Example:

  The value is fetched from the registry on MS Windows if
  possible or is else given the default value:

    \"C:\\Program Files\\GIMP-2.0\\bin\\gimp-2.6.exe\"

  On other system it has the default value

    \"gimp\"."
  :type '(choice (file :tag "Full file name" :must-match t)
                 (string :tag "File name (must be in path)"))
  :group 'gimpedit)

;;;###autoload
(defun gimpedit-edit-file (image-file &optional extra-args)
  "Edit IMAGE-FILE with GIMP.
See also `gimpedit-edit-file'."
  (interactive (list (or (get-char-property (point) 'image-file)
                         (read-file-name "Image to edit in GIMP: "))))
  (setq image-file (expand-file-name image-file))
  (apply 'call-process gimpedit-remote-command
         nil
         0
         nil
         (reverse (cons image-file (reverse extra-args))))
  (let ((msg " Asked GIMP to open %s - you may have to switch to GIMP"))
    (put-text-property 0 (length msg) 'face 'highlight msg)
    (message msg (file-name-nondirectory image-file))))

;;;###autoload
(defun gimpedit-edit-buffer ()
  "Edit image file in current buffer with GIMP.
See also `gimpedit-edit-file'.

You may also be interested in gimpedit-mode with which you can edit
gimp files from within Emacs using GIMP's scripting
possibilities. See

  URL `http://www.emacswiki.org/emacs/GimpMode'"
  (interactive)
  (unless (buffer-file-name)
    (error
     "Can't edit in GIMP because this buffer does not have a file name."))
  (gimpedit-edit-file (buffer-file-name)))

;;;###autoload
(defun gimpedit-can-edit (file-name)
  (and file-name
       (member (downcase (file-name-extension file-name))
               '("png" "gif" "jpg" "jpeg"))))

;; (defcustom gimpedit-point-key-bindings '(([(control ?c) ?&] gimpedit-edit-file))
;;   "Key bindings suggested for image links etc."
;;   :type '(repeat (list key-sequence function))
;;   :group 'gimpedit)

;; (defun gimpedit-add-point-bindings (map)
;;   "Add `gimpedit-point-key-bindings' to point keymap MAP.
;; Set it up like this:

;;   (eval-after-load 'gimpedit
;;     '(gimpedit-add-point-bindings MY-MAP))

;; There must also be a character property `image-file' at point for this
;; to work."
;;   (dolist (binding gimpedit-point-key-bindings)
;;     (let ((key (nth 0 binding))
;;           (fun (nth 1 binding)))
;;       (define-key map key fun))))

(provide 'gimpedit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gimpedit.el ends here

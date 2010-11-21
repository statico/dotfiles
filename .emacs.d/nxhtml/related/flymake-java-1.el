;;; flymake-java-1.el --- Flymake for single java files
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-12-02 Wed
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

(eval-and-compile (require 'flymake))

(defun flymake-init-maybe-find-buildfile-dir (source-file-name buildfile-name)
  "Find buildfile, store its dir in buffer data and return its dir, if found."
  (let* ((buildfile-dir
          (flymake-find-buildfile buildfile-name
                                  (file-name-directory source-file-name))))
    (if buildfile-dir
        (setq flymake-base-dir buildfile-dir)
      (flymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
      nil)))

(defun flymake-complex-make-init-impl-1 (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
	 (source-file-name   buffer-file-name)
	 (buildfile-dir      (flymake-init-maybe-find-buildfile-dir source-file-name build-file-name)))
    (if buildfile-dir
	(let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
	  (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
							    use-relative-base-dir use-relative-source
							    get-cmdline-f))))
    args))

(defun flymake-complex-java-init ()
  (or (flymake-complex-make-init-impl-1 'flymake-create-temp-with-folder-structure nil nil "Makefile" 'flymake-get-make-cmdline)
      (flymake-complex-make-init-impl-1 'flymake-create-temp-with-folder-structure nil nil "build.xml" 'flymake-get-make-cmdline)
      (flymake-java-1-init)))

(defcustom flymake-java-1-javac "c:/Sun/SDK/jdk/bin/javac.exe"
  "Path to javac."
  :group 'flymake)

(defun flymake-java-1-init ()
  (if (not (executable-find flymake-java-1-javac))
      (message "Can't find javac. Please customize flymake-java-1-javac")
    (list flymake-java-1-javac
          (list (flymake-init-create-temp-buffer-copy
                 'flymake-create-temp-with-folder-structure)))))

;; (defun flymake-java-1-turn-on ()
;;   (interactive)
;;   (if (not (executable-find flymake-java-1-javac))
;;       (message "Can't find javac. Please customize flymake-java-1-javac")
;;     (let ((flymake-allowed-file-name-masks
;;            '(("\\.java\\'" flymake-java-1-init flymake-simple-cleanup))))
;;       (when flymake-mode (flymake-mode -1))
;;       (flymake-mode 1))))

;;;###autoload
(defun flymake-java-1-load ()
  (let ((jrec (assoc "\\.java\\'" flymake-allowed-file-name-masks)))
    (setq flymake-allowed-file-name-masks
          (delete jrec flymake-allowed-file-name-masks))
    (setq flymake-allowed-file-name-masks
          (cons
           '("\\.java\\'" flymake-complex-java-init flymake-simple-java-cleanup)
           flymake-allowed-file-name-masks))))

(provide 'flymake-java-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-java-1.el ends here

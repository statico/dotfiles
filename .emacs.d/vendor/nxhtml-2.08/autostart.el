;;; autostart.el --- Load nxhtml
;;
;; Author: By: Lennart Borgman
;; Created: Fri Dec 15 2006
;; Version:
;; Last-Updated: 2009-04-30 Thu
;; Keywords:
;; Compatibility:
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

;; Fix-me: Split out the definitions from this file so it can be
;; loaded during byte compilation.

;;(eval-when-compile (require 'web-vcs nil t))
;;(eval-when-compile (require 'nxhtml-web-vcs nil t))

(message "Nxml/Nxhtml Autostart.el loading ...")

(defconst nxhtml-autostart-trace nil)
(defsubst nxhtml-autostart-trace (format-string &rest args)
  (when nxhtml-autostart-trace
    (apply 'message format-string args)))

(defconst nxhtml-load-time-start (float-time))

;; Add this dir to load-path
(add-to-list 'load-path
             (file-name-directory (or load-file-name
                                      (when (boundp 'bytecomp-filename) bytecomp-filename)
                                      buffer-file-name)))

(require 'nxhtml-base)
(eval-and-compile (when (fboundp 'nxml-mode)
                     (load (expand-file-name "etc/schema/schema-path-patch"
                                             nxhtml-install-dir))))

;; (defun nxhtml-custom-load-and-get-value (symbol)
;;   (custom-load-symbol symbol)
;;   (symbol-value symbol))

(defun nxhtml-list-loaded-features (use-message)
  (interactive (list t))
  (let ((buf (when use-message ;(called-interactively-p)
               (get-buffer-create "*nXhtml loaded features*"))))
    (if buf
        (with-current-buffer buf (erase-buffer))
      (message "")
      (message "=== Loaded at nxhtml/autostart.el end:"))
    (dolist (feature '(
                       as-external
                       html-chklnk
                       html-imenu
                       html-move
                       html-pagetoc
                       html-quote
                       html-site
                       html-toc
                       html-upl
                       html-wtoc
                       inlimg
                       mumamo
                       nxhtml-bug
                       nxhtml-menu
                       nxhtml-mode
                       nxhtml-mumamo
                       nxhtml-strval
                       nxhtml
                       nxhtml-js
                       nxml-where
                       outline-magic
                       rngalt
                       tidy-xhtml
                       xhtml-help
                       ))
      (when (featurep feature)
        (if buf
            (with-current-buffer buf
              (insert (format "(feature '%s)=%s\n" feature (featurep feature))))
          (message "(feature '%s)=%s" feature (featurep feature)))))
    (if buf
        (display-buffer buf)
      (message ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code that will run on loading this file

(if (< emacs-major-version 23)
    (unless (featurep 'autostart22)
      (load (expand-file-name "autostart22" nxhtml-install-dir)))
  ;; Check that the nxml-mode included with Emacs is used. There
  ;; has been some problems on Debian with this.
  (let ((nxml-mode-file (locate-library "nxml-mode"))
        (help-file      (locate-library "help")))
    (unless (string= (expand-file-name ".." help-file)
                     (expand-file-name "../.." nxml-mode-file))
      (error "Wrong nxml-mode=%s used, please use the one that comes with Emacs" nxml-mode-file))))

(let* ((util-dir (file-name-as-directory (expand-file-name "util" nxhtml-install-dir)))
       (related-dir (file-name-as-directory (expand-file-name "related" nxhtml-install-dir)))
       (nxhtml-dir (file-name-as-directory (expand-file-name "nxhtml" nxhtml-install-dir)))
       ;;(company-dir (file-name-as-directory (expand-file-name "util/nxhtml-company-mode" nxhtml-install-dir)))
       (tests-dir (file-name-as-directory (expand-file-name "tests" nxhtml-install-dir))))
  (add-to-list 'load-path nxhtml-dir)
  (add-to-list 'load-path related-dir)
  (add-to-list 'load-path util-dir)
  (add-to-list 'load-path nxhtml-install-dir)
  ;;(add-to-list 'load-path company-dir)
  (add-to-list 'load-path tests-dir)

  (nxhtml-autostart-trace "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

  ;; Autoloading etc
  ;; (unless (featurep 'web-vcs)
  ;;   (load (expand-file-name "web-vcs" nxhtml-install-dir) (not nxhtml-autoload-web)))

  ;; (when (catch 'miss
  ;;         (dolist (file nxhtml-basic-files)
  ;;           (let ((dl-file (expand-file-name file nxhtml-install-dir)))
  ;;             (unless (file-exists-p dl-file)
  ;;               (throw 'miss t))))
  ;;         nil)
  ;;   (nxhtml-setup-auto-download nxhtml-install-dir))

  (unless (featurep 'web-autoload)
    (load (expand-file-name "web-autoload" nxhtml-install-dir) (not nxhtml-autoload-web)))

  (when nxhtml-autoload-web
    (ad-activate 'require t))

  ;; Fix-me: Why must as-external be loaded? Why doesn't it work in batch?
  ;;(unless noninteractive (require 'as-external))

  (unless (featurep 'nxhtml-loaddefs)
    (load (expand-file-name "nxhtml-loaddefs" nxhtml-install-dir) nxhtml-autoload-web))
  (nxhtml-autostart-trace "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

  ;; Turn on `nxhtml-menu-mode' unconditionally
  (nxhtml-autostart-trace "Turn on `nxhtml-menu-mode' unconditionally")
  (nxhtml-menu-mode 1)
  (nxhtml-autostart-trace "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

  ;; Patch the rnc include paths
  (when (fboundp 'rncpp-patch-xhtml-loader) (rncpp-patch-xhtml-loader))
  (nxhtml-autostart-trace "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

  ;; Load nXhtml
  (unless (featurep 'nxhtml-autoload)
    (load (expand-file-name "nxhtml/nxhtml-autoload" nxhtml-install-dir))))
(nxhtml-autostart-trace "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))


(unless (featurep 'nxhtml-autostart)
  ;; Provide the feature here to avoid loading looping on error.
  (provide 'nxhtml-autostart)

  ;; Tell what have been loaded of nXhtml:
  (when nxhtml-autostart-trace (nxhtml-list-loaded-features nil))

  ;; How long time did it all take?
  (message "Nxml/Nxhtml Autostart.el loaded in %.1f seconds" (- (float-time) nxhtml-load-time-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autostart.el ends here

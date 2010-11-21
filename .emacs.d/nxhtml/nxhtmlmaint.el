;;; nxhtmlmaint.el --- Some maintenance helpers
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-27T15:29:35+0200 Sat
;; Version: 0.6
;; Last-Updated: 2010-01-18 Mon
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
;; This module contains maintenance functions:
;;
;; `nxhtmlmaint-get-all-autoloads' (nxhtmlmaint-get-all-autoloads)
;;
;; `nxhtmlmaint-start-byte-compilation'
;; `nxhtmlmaint-byte-uncompile-all'
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

(eval-when-compile (require 'advice))
(eval-when-compile (require 'nxhtml-base))
(eval-when-compile (require 'nxhtml-web-vcs nil t))
(eval-when-compile (require 'web-vcs nil t))
(eval-when-compile (require 'ourcomments-util))

(defvar nxhtmlmaint-dir
  ;;(file-name-directory (if load-file-name load-file-name buffer-file-name))
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Maintenance directory for nXhtml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Autoload helpers

(defun nxhtmlmaint-autoloads-file ()
  "Return autoload file name for nXhtml."
  (file-truename (expand-file-name "nxhtml-loaddefs.el" nxhtmlmaint-dir)))

(defun nxhtmlmaint-util-dir ()
  "Return nXhtml util directory."
  (file-truename (file-name-as-directory
                  (expand-file-name "util" nxhtmlmaint-dir))))

(defvar nxhtmlmaint-autoload-default-directory (nxhtmlmaint-util-dir))

(defvar generated-autoload-file)

(defun nxhtmlmaint-initialize-autoloads-file ()
  "Initialize nXhtml autoload file."
  (with-current-buffer (find-file-noselect generated-autoload-file)
    (when (= 0 (buffer-size))
      (insert ";; Autoloads for nXthml
;;
;; This file should be updated by `nxhtmlmaint-get-file-autoloads',
;; `nxhtmlmaint-get-dir-autoloads' or `nxhtmlmaint-get-all-autoloads'.
\(eval-when-compile (require 'nxhtml-base))
\(eval-when-compile (require 'web-vcs))")
    (basic-save-buffer))))

(defun nxmtmlmaint-advice-autoload (on)
  "Activate advices if ON, otherwise turn them off."
  (if on
      (progn
        (ad-activate 'autoload-file-load-name)
        (ad-activate 'make-autoload))
    (ad-deactivate 'autoload-file-load-name)
    (ad-deactivate 'make-autoload)))

(defun nxhtmlmaint-get-file-autoloads (file)
  "Get autoloads for file FILE.
Update nXhtml autoload file with them."
  (interactive (list (buffer-file-name)))
  (let* ((generated-autoload-file (nxhtmlmaint-autoloads-file))
         (emacs-lisp-mode-hook nil)
         (default-directory (nxhtmlmaint-util-dir)))
    (nxhtmlmaint-initialize-autoloads-file)
    ;; Get the autoloads using advice
    (nxmtmlmaint-advice-autoload t)
    (update-file-autoloads file nil)
    (nxmtmlmaint-advice-autoload nil)
    ;; Display
    (display-buffer (find-file-noselect generated-autoload-file))))

(defun nxhtmlmaint-get-dir-autoloads (dir)
  "Get autoloads for directory DIR.
Update nXhtml autoload file with them."
  (interactive (list (or (when (buffer-file-name)
                           (file-name-directory (buffer-file-name)))
                         default-directory)))
  (let* ((generated-autoload-file (nxhtmlmaint-autoloads-file))
         (emacs-lisp-mode-hook nil)
         (auto-buf (find-file-noselect generated-autoload-file)))
    (nxhtmlmaint-initialize-autoloads-file)
    ;; Get the autoloads using advice
    (nxmtmlmaint-advice-autoload t)
    ;; Fix-me: Loop instead, some files must be avoided.
    (update-directory-autoloads dir)
    (nxmtmlmaint-advice-autoload nil)
    ;; Display
    (display-buffer (find-file-noselect generated-autoload-file))))

(defun nxhtmlmaint-get-tree-autoloads (root)
  "Get autoloads for directory tree ROOT.
Update nXhtml autoload file with them."
  (interactive (list (or (when (buffer-file-name)
                           (file-name-directory (buffer-file-name)))
                         default-directory)))
  (message "Getting autoloads in %s" root)
  (nxhtmlmaint-get-dir-autoloads root)
  (let* ((files (directory-files root))
         (sub-dirs (mapcar (lambda (file)
                             (when (and (not (member file '("." "..")))
                                        (not (member file '("nxml-mode-20041004" "old")))
                                        (not (member file '("nxhtml-company-mode")))
                                        (not (member file '("in")))
                                        (file-directory-p (expand-file-name file root)))
                               file))
                           files)))
    (setq sub-dirs (delq nil sub-dirs))
    ;;(message "sub-dirs=%s" sub-dirs)
    (dolist (dir sub-dirs)
      (let ((full-dir (expand-file-name dir root)))
        (unless (or (string= full-dir nxhtmlmaint-dir)
                    (string= dir "alts"))
          (nxhtmlmaint-get-tree-autoloads full-dir))))))

;;(nxhtmlmaint-get-all-autoloads)
(defun nxhtmlmaint-get-all-autoloads ()
  "Get all autoloads for nXhtml.
Update nXhtml autoload file with them."
  ;;(interactive)
  (if nxhtml-autoload-web
      (message "Skipping rebuilding autoloads, not possible when autoloading from web")
    (let ((auto-buf (find-file-noselect (nxhtmlmaint-autoloads-file))))
      (with-current-buffer auto-buf
        (erase-buffer)
        (basic-save-buffer))
      (nxhtmlmaint-get-tree-autoloads nxhtmlmaint-dir)
      ;; `nxhtml-mode' and `nxhtml-validation-header-mode' should only be
      ;; autoloaded if nxml-mode if available.
      (with-current-buffer auto-buf
        (message "Fixing nxml autoloads")
        (let ((frmt (if (= emacs-major-version 22)
                        "^(autoload (quote %s) "
                      "^(autoload '%s ")))
          (dolist (nxmode '(nxhtml-mode nxhtml-validation-header-mode))
            (goto-char (point-min))
            (when (re-search-forward (format frmt nxmode) nil t)
              (forward-line 0)
              (insert "(when (fboundp 'nxml-mode)\n")
              (forward-sexp)
              (insert ")"))))
        ;; Fix defcustom autoloads
        (goto-char (point-min))
        (let ((cus-auto "(\\(custom-autoload\\) +'.* +\\(\".*?\"\\)"))
          (while (re-search-forward cus-auto nil t)
            ;;(backward-char (1- (length cus-auto)))
            ;;(insert "nxhtml-")
            (let ((lib (match-string 2)))
              ;; Change to symbol to fix autoloading. This works because
              ;; custom-load-symbol does require on symbols.
              (setq lib (concat "'" (substring lib 1 -1)))
              (replace-match "nxhtml-custom-autoload" t t nil 1)
              (replace-match lib t t nil 2))))
        ;; Fix autoload calls
        (goto-char (point-min))
        (let ((auto "(autoload "))
          (while (search-forward auto nil t)
            (backward-char (1- (length auto)))
            (insert "nxhtml-")))
        ;; Fix autoload source
        (goto-char (point-min))
        (let* ((patt-src "^;;; Generated autoloads from \\(.*\\)$")
               (patt-auto "^(nxhtml-autoload '[^ ]+ \\(\"[^\"]+\"\\)")
               (patt-cust "^(nxhtml-custom-autoload '[^ ]+ \\(\"[^\"]+\"\\)")
               (patt (concat "\\(?:" patt-src "\\)\\|\\(?:" patt-auto "\\)\\|\\(?:" patt-cust "\\)"))
               curr-src)
          (while (re-search-forward patt nil t)
            (cond
             ( (match-string 1)
               (setq curr-src (match-string-no-properties 1))
               ;; Remove .el
               (setq curr-src (substring curr-src 0 -3))
               ;; Setup up for web autoload
               (let* ((src-name (file-name-nondirectory curr-src))
                      (feature (make-symbol src-name))
                      )
                 (end-of-line)
                 (insert "\n"
                         "(web-autoload-require '"
                         (symbol-name feature)
                         " 'lp"
                         " '(nxhtml-download-root-url nil)"
                         " \"" curr-src "\""
                         " nxhtml-install-dir"
                         " 'nxhtml-byte-compile-file"
                         ")\n"))
               )
             ( (match-string 3)
               ;; (custom-autoload 'sym "lib" nil) is will give a
               ;; (require 'lib) so everything is ok here.
               nil)
             ( (or (match-string 2)
                   (match-string 3)
                   )
               (let* ((subexp (if (match-string 2) 2 3))
                      (file (match-string-no-properties subexp)))
                 (replace-match (concat "`(lp '(nxhtml-download-root-url nil)"
                                        " \"" curr-src "\""
                                        " nxhtml-install-dir)")
                                nil ;; fixedcase
                                nil ;; literal
                                nil ;; string
                                subexp   ;; subexp
                                ))
               )
             (t (error "No match???")))))
        ;; Save
        (basic-save-buffer)))))


(defun nxhtmlmaint-autoload-file-load-name (file)
  "Return relative file name for FILE to autoload file directory."
  (let ((name (if (and nxhtmlmaint-autoload-default-directory
                       (file-name-absolute-p file))
                  (file-relative-name
                   file nxhtmlmaint-autoload-default-directory)
                (file-name-nondirectory file))))
    (if (string-match "\\.elc?\\(\\.\\|\\'\\)" name)
        (substring name 0 (match-beginning 0))
      name)))

(defadvice autoload-file-load-name (around
                                    nxhtmlmaint-advice-autoload-file-load-name
                                    ;;activate
                                    compile)
  "Advice to return relative file name."
  (setq ad-return-value (nxhtmlmaint-autoload-file-load-name (ad-get-arg 0))))

(defun nxhtmlmaint-make-autoload (form file)
  "Make autoload for multi major modes."
  ;;(message "form=%S" form)
  (if (or (not (listp form))
          (not (eq 'define-mumamo-multi-major-mode (car form))))
      ad-return-value
    (if ad-return-value
        ad-return-value
      ;; Fix-me: Maybe expand??
      (let ((name (nth 1 form))
            (doc  (nth 2 form)))
        `(autoload ',name ,file ,doc t)
        ))))

(defadvice make-autoload (after
                          nxhtmlmaint-advice-make-autoload
                          ;;activate
                          compile)
  "Make autoload for multi major modes."
  (setq ad-return-value
        (nxhtmlmaint-make-autoload (ad-get-arg 0)
                                   (ad-get-arg 1))))

;; (defun nxhtmlmaint-generate-library-autoloads (library)
;;   "Insert at point autoloads for Emacs library LIBRARY.
;;   Works like `generate-file-autoloads', but for a library."
;;   (interactive
;;    (list (completing-read "Generate autoloads for library: "
;;                           'locate-file-completion
;;                           (cons load-path (get-load-suffixes)))))
;;   (let ((file (locate-library library)))
;;     ;; Fix-me: wasn't this defined???
;;     (generate-file-autoloads file)))

;;;###autoload
(defun nxhtmlmaint-start-byte-compilation ()
  "Start byte compilation of nXhtml in new Emacs instance.
Byte compiling in general makes elisp code run 5-10 times faster
which is quite noticeable when you use nXhtml.

This will also update the file nxhtml-loaddefs.el.

You must restart Emacs to use the byte compiled files.

If for some reason the byte compiled files does not work you can
remove then with `nxhtmlmaint-byte-uncompile-all'."
  (interactive)
  ;; Fix-me: This message and redisplay seems only necessary sometimes.
  (message "Preparing byte compilation of nXhtml ...") (redisplay t)
  (let* ((this-file    (expand-file-name "nxhtmlmaint.el" nxhtmlmaint-dir))
         (auto-file    (expand-file-name "autostart.el" nxhtmlmaint-dir))
         (web-vcs-file (expand-file-name "nxhtml-web-vcs.el" nxhtmlmaint-dir))
         (this-emacs (locate-file invocation-name
                                  (list invocation-directory)
                                  exec-suffixes))
         (process-args `(,this-emacs nil 0 nil "-Q")))
    (nxhtmlmaint-byte-uncompile-all)
    (if (or noninteractive
            (not window-system))
        (nxhtmlmaint-byte-compile-all)
      ;;(when noninteractive (setq process-args (append process-args '("-batch"))))
      (setq process-args (append process-args
                                 (list "-l" auto-file
                                       "-l" web-vcs-file
                                       "-l" this-file
                                       "-f" "nxhtmlmaint-byte-compile-all")))
      (message "process-args=%S" process-args)
      (message "Starting new Emacs instance for byte compiling ...")
      (apply 'call-process process-args))))

;;(nxhtmlmaint-byte-compile-all)
(defun nxhtmlmaint-byte-compile-all ()
  "Byte recompile all files in nXhtml that needs it."
  (message "nxhtmlmaint-byte-compile-all: nxhtmlmaint-dir=%S, exists=%s" nxhtmlmaint-dir (file-directory-p nxhtmlmaint-dir))
  (let* ((load-path load-path)
         (nxhtml-dir (file-name-as-directory
                      (expand-file-name "nxhtml"
                                        nxhtmlmaint-dir)))
         (util-dir (file-name-as-directory
                    (expand-file-name "util"
                                      nxhtmlmaint-dir)))
         ;; (nxhtml-company-dir (file-name-as-directory
         ;;                      (expand-file-name "nxhtml-company-mode"
         ;;                                        util-dir)))
         (related-dir (file-name-as-directory
                       (expand-file-name "related"
                                         nxhtmlmaint-dir)))
         (tests-dir (file-name-as-directory
                     (expand-file-name "tests"
                                       nxhtmlmaint-dir)))
         (emacsw32-dir (file-name-as-directory
                        (expand-file-name "../lisp"
                                          nxhtmlmaint-dir)))
         (default-dir nxhtml-dir)
         )
    (message "nxhtmlmaint-byte-compile-all: nxhtml-dir=%S, exists=%s" nxhtml-dir (file-directory-p nxhtml-dir))
    (message "nxhtmlmaint-byte-compile-all: util-dir=%S, exists=%s" util-dir (file-directory-p util-dir))
    (message "nxhtmlmaint-byte-compile-all: related-dir=%S, exists=%s" related-dir (file-directory-p related-dir))
    (message "nxhtmlmaint-byte-compile-all: tests-dir=%S, exists=%s" tests-dir (file-directory-p tests-dir))
    (add-to-list 'load-path nxhtml-dir)
    (add-to-list 'load-path util-dir)
    ;;(add-to-list 'load-path nxhtml-company-dir)
    (add-to-list 'load-path related-dir)
    (add-to-list 'load-path tests-dir)
    (when (file-directory-p emacsw32-dir)
      (add-to-list 'load-path emacsw32-dir))
    (require 'cl) ;; This is run in a new Emacs. Fix-me: This might not be true any more.
    (message "load-path=%s" load-path)
    (let ((dummy-debug-on-error t))
      (nxhtmlmaint-byte-compile-dir nxhtmlmaint-dir nil nil nil))
    (web-vcs-message-with-face 'web-vcs-gold "Byte compiling nXhtml is ready, restart Emacs to use the compiled files")))

;;;###autoload
(defun nxhtmlmaint-byte-recompile ()
  "Recompile or compile all nXhtml files in current Emacs."
  (interactive)
  (nxhtmlmaint-byte-compile-dir nxhtmlmaint-dir nil nil t)
  (web-vcs-message-with-face 'web-vcs-gold "Byte recompiling nXhtml ready"))

;;;###autoload
(defun nxhtmlmaint-byte-uncompile-all ()
  "Delete byte compiled files in nXhtml.
This will also update the file nxhtml-loaddefs.el.

See `nxhtmlmaint-start-byte-compilation' for byte compiling."
  (interactive)
  (nxhtmlmaint-get-all-autoloads)
  (let ((dummy-debug-on-error t))
    (nxhtmlmaint-byte-compile-dir nxhtmlmaint-dir t t nil))
  (message "Byte uncompiling is ready, restart Emacs to use the elisp files"))

(defconst nxhtmlmaint-nonbyte-compile-dirs
  '("." ".." "alts" "nxml-mode-20041004" "old" "tests" "nxhtml-company-mode"))

;; Fix-me: simplify this now that nxml is not included
(defun nxhtmlmaint-byte-compile-dir (dir force del-elc load)
  "Byte compile or uncompile directory tree DIR.
If FORCE is non-nil byte recompile the elisp file even if the
compiled file is newer.

If DEL-ELC is nil then byte compile files.  If DEL-ELC is non-nil
then instead delete the compiled files."
  ;;(directory-files (file-name-directory buffer-file-name) t "\.el\\'")
  (dolist (el-src (directory-files dir t "\.el\\'"))
    (let ((elc-dst (concat el-src "c")))
      (if del-elc
          (when (file-exists-p elc-dst)
            (delete-file elc-dst)
            (message "Deleted %s" elc-dst))
        (setq debug-on-error t)
        (when (or force (file-newer-than-file-p el-src elc-dst))
          ;;(message "fn=%s" (file-name-nondirectory el-src))
          (when t ;;(string= "nxhtml-menu.el" (file-name-nondirectory el-src))
            ;;(message "(nxhtml-byte-compile-file %s)" el-src)
            (unless (nxhtml-byte-compile-file el-src load)
              (message "Couldn't compile %s" el-src)))))))
  (dolist (f (directory-files dir t))
    (when (file-directory-p f)
      ;; Fix-me: Avoid some dirs
      (let ((name (file-name-nondirectory f)))
        (unless (member name nxhtmlmaint-nonbyte-compile-dirs)
          (nxhtmlmaint-byte-compile-dir f force del-elc load))))))

(provide 'nxhtmlmaint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmlmaint.el ends here

;;; nxhtml-web-vcs.el --- nXhtml things for web-vcs.el
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-01-13 Wed
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

(eval-when-compile (require 'cl))
(eval-when-compile (require 'nxhtml-base nil t))
;;(eval-when-compile (require 'nxhtmlmaint nil t))
(eval-when-compile (require 'web-vcs nil t))

(defvar nxhtml-web-vcs-file (or load-file-name
                                (when (boundp 'bytecomp-filename) bytecomp-filename)
                                buffer-file-name)
  "This file.")

(defun nxhtml-require-base ()
  (require 'nxhtml-base nil t)
  (unless (featurep 'nxhtml-base)
    ;; At startup, need to load it by hand.
    (let ((load-path load-path))
      (add-to-list 'load-path (file-name-directory nxhtml-web-vcs-file))
      (require 'nxhtml-base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repository URL


;;(nxhtml-default-download-directory)
(defun nxhtml-default-download-directory ()
  (let* ((ur (expand-file-name "" "~"))
         (ur-len (length ur))
         (full (if (and (boundp 'nxhtml-install-dir)
                        nxhtml-install-dir)
                   nxhtml-install-dir
                 (file-name-as-directory
                  (expand-file-name ""
                                    (web-vcs-default-download-directory)))))
         (full-len (length full)))
    (if (and (> full-len ur-len)
             (string= ur (substring full 0 ur-len)))
        (concat "~" (substring full ur-len))
      full)))


(defun nxhtml-web-vcs-read-dl-dir (prompt)
  "Return current nXhtml install dir or read dir."
  (or (and (boundp 'nxhtml-install-dir)
           nxhtml-install-dir)
      (let* ((pr (concat
                  "A directory named 'nxhtml' will be created below the root you give."
                  "\n"
                  prompt))
             (root (read-directory-name pr (nxhtml-default-download-directory))))
        (when root
          (expand-file-name "nxhtml" root)))))

;;(call-interactively 'nxhtml-setup-install)
;; (read-key "Prompt: ")
;; (y-or-n-p "Prompt")
;;;###autoload
(defun nxhtml-setup-install (way)
  "Setup and start nXhtml installation.

This is for installation and updating directly from the nXhtml
development sources.

There are two different ways to install:

  (1) Download all at once: `nxhtml-setup-download-all'
  (2) Automatically download part by part: `nxhtml-setup-auto-download'

You can convert between those ways by calling this function again.
You can also do this by setting the option `nxhtml-autoload-web' yourself.

When you have nXhtml installed you can update it:

  (3) Update new files in nXhtml: `nxhtml-update-existing-files'

To learn more about nXhtml visit its home page at URL
`http://www.emacswiki.com/NxhtmlMode/'.

If you want to test auto download \(but not use it further) there
is a special function for that, you answer T here:

   (T) Test automatic download part by part: `nxhtml-setup-test-auto-download'

======
*Note*
If you want to download a zip file with latest released version instead then
please see URL `http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html'."
  (interactive (let ((curr-cfg (current-window-configuration)))
                 (describe-function 'nxhtml-setup-install)
                 (select-window (get-buffer-window (help-buffer)))
                 (delete-other-windows)
                 (list
                  (let* ((key nil)
                         (has-nxhtml (and (boundp 'nxhtml-install-dir) nxhtml-install-dir))
                         (current-way (if has-nxhtml
                                          (if (and (boundp 'nxhtml-autoload-web)
                                                   nxhtml-autoload-web)
                                              "Your current setup is to download part by part from the web."
                                            "Your current setup it to download all of nXhtml at once.")
                                        "(You have not currently installed nXhtml.)"))
                         (prompt (concat "Setup nXhtml install."
                                         "\n" current-way
                                         "\n"
                                         "\n(1) Download whole at once, or (2) part by part as needed"
                                         (if has-nxhtml "\n(3) Update your existing nXhtml" "")
                                         "\n(T) For temporary testing downloading part by part"
                                         "\n"
                                         "\n(? for help, q to quit): "))
                         (allowed-keys (if has-nxhtml
                                           '(?1 ?2 ?3 ?T ?q 7)
                                         '(?1 ?2 ?T ?q 7)))
                         (please nil))
                    (while (not (member key allowed-keys))
                      (if (not (member key '(??)))
                          (when key
                            (unless please
                              (setq prompt (concat "Please answer with one of the alternatives.\n\n"
                                                   prompt))
                              (setq please t)))
                        (describe-function 'nxhtml-setup-install)
                        (select-window (get-buffer-window (help-buffer)))
                        (delete-other-windows))
                      (setq key (web-vcs-read-key prompt))
                      ;;(message "key = %S" key) (sit-for 1)
                      )
                    (case key
                      (7 (set-window-configuration curr-cfg)
                         nil)
                      (?1 'whole)
                      (?2 'part-by-part)
                      (?3 'update-existing)
                      (?T 'test-part-by-part)
                      )))))
  (message "")
  (case way
    (whole             (call-interactively 'nxhtml-setup-download-all))
    (part-by-part      (call-interactively 'nxhtml-setup-auto-download))
    (update-existing   (call-interactively 'nxhtml-update-existing-files))
    (test-part-by-part (call-interactively 'nxhtml-setup-test-auto-download))
    ((eq nil way) nil)
    (t (error "Unknown way = %S" way))))

(defvar nxhtml-basic-files '(
                             "nxhtml-base.el"
                             "nxhtml-loaddefs.el"
                             "web-autoload.el"
                             "etc/schema/schema-path-patch.el"
                             "nxhtml/nxhtml-autoload.el"
                             "autostart.el"
                             ))

;;;###autoload
(defun nxhtml-setup-auto-download (dl-dir)
  "Set up to autoload nXhtml files from the web.

This function will download some initial files and then setup to
download the rest when you need them.

Files will be downloaded under the directory root you specify in
DL-DIR.

Note that files will not be upgraded automatically.  The auto
downloading is just for files you are missing. (This may change a
bit in the future.) If you want to upgrade those files that you
have downloaded you can just call `nxhtml-update-existing-files'.

You can easily switch between this mode of downloading or
downloading the whole of nXhtml by once.  To switch just call the
command `nxhtml-setup-install'.

See also the command `nxhtml-setup-download-all'.

Note: If your nXhtml is to old you can't use this function
      directly.  You have to upgrade first, se the function
      above. Version 2.07 or above is good for this."
  (interactive (progn
                 (describe-function 'nxhtml-setup-auto-download)
                 (select-window (get-buffer-window (help-buffer)))
                 (delete-other-windows)
                 (nxhtml-check-convert-to-part-by-part)
                 (list
                  (progn
                    (when (and (boundp 'nxhtml-autoload-web)
                               (not nxhtml-autoload-web))
                      (unless (yes-or-no-p "Convert to updating nXhtml part by part? ")
                        (throw 'command-level nil)))
                    (nxhtml-web-vcs-read-dl-dir "Download nXhtml part by part to directory: ")))))
  (catch 'command-level
    (if (not dl-dir)
        (unless (with-no-warnings (called-interactively-p))
          (error "dl-dir should be a directory"))
      (nxhtml-check-convert-to-part-by-part)
      (when (and (boundp 'nxhtml-install-dir)
                 nxhtml-install-dir)
        (unless (string= (file-truename dl-dir)
                         (file-truename nxhtml-install-dir))
          (error "Download dir must be same as nxhtml-install-dir=%S" nxhtml-install-dir)))
      (let* (;; Need some files:
             (web-vcs-el-src (concat (file-name-sans-extension web-vcs-el-this) ".el"))
             (web-vcs-el (expand-file-name (file-name-nondirectory web-vcs-el-src)
                                           dl-dir))
             (vcs 'lp)
             (base-url (nxhtml-download-root-url nil))
             (byte-comp (if (boundp 'web-autoload-autocompile)
                            web-autoload-autocompile
                          t))
             (has-nxhtml (and (boundp 'nxhtml-install-dir)
                              nxhtml-install-dir))
             (web-vcs-folder-cache nil))
        (setq nxhtml-install-dir dl-dir)
        (let ((root (file-name-directory dl-dir)))
          (unless (file-exists-p root)
            (unless (yes-or-no-p (format "Directory %S does not exist, create it? " root))
              (error "Aborted by user"))))
        (make-directory dl-dir t)
        (setq message-log-max t)
        (view-echo-area-messages)
        (message "")
        (message "")
        (web-vcs-message-with-face 'web-vcs-green "==== Starting nXhtml part by part state ====")
        (message "has-nxhtml=%s" has-nxhtml)
        ;; Fix-me: First copy this file and web-vcs.el to its destination:
        (unless (string= (file-truename dl-dir)
                         (file-truename (file-name-directory nxhtml-web-vcs-file)))
          (dolist (f (list web-vcs-el-src nxhtml-web-vcs-file))
            (copy-file f (expand-file-name (file-name-nondirectory f) dl-dir)
                       'ok-overwrite)))
        (when byte-comp (web-vcs-byte-compile-newer-file web-vcs-el t))
        ;; Get basic file list:
        (catch 'web-autoload-comp-restart
          ;;(let ((file-mask (regexp-opt nxhtml-basic-files)))
          ;;  (web-vcs-get-missing-matching-files vcs base-url dl-dir file-mask))
          (dolist (f nxhtml-basic-files)
            (web-vcs-get-missing-matching-files vcs base-url dl-dir f))
          ;; Autostart.el has not run yet, add download dir to load-path.
          (let ((load-path (cons (file-name-directory web-vcs-el) load-path)))
            (when byte-comp
              (dolist (file nxhtml-basic-files)
                (let ((el-file (expand-file-name file dl-dir)))
                  (web-vcs-byte-compile-newer-file el-file nil)))))
          (let ((autostart-file (expand-file-name "autostart" dl-dir)))
            ;;(ad-deactivate 'require)
            (web-vcs-set&save-option 'nxhtml-autoload-web t)
            (web-vcs-log nil nil "* nXhtml: Download Part by Part as Needed\n")
            (load autostart-file)
            (unless (ad-is-active 'require) (ad-activate 'require))
            (web-vcs-log-save)
            (web-vcs-message-with-face 'web-vcs-green "==== Basic files for nXhtml part by part are now installed ====")
            (web-vcs-display-messages t)
            (unless has-nxhtml (nxhtml-add-loading-to-custom-file autostart-file t))))))))

;;(call-interactively 'nxhtml-download)
;;;###autoload
(defun nxhtml-setup-download-all (dl-dir)
  "Download or update all of nXhtml.

You can download all if nXhtml with this command.

To update existing files use `nxhtml-update-existing-files'.

If you want to download only those files you are actually using
then call `nxhtml-setup-auto-download' instead.

See the command `nxhtml-setup-install' for a convenient way to
call these commands.

For more information about auto download of nXhtml files see
`nxhtml-setup-auto-download'."
  (interactive (progn
                 (describe-function 'nxhtml-setup-auto-download)
                 (select-window (get-buffer-window (help-buffer)))
                 (delete-other-windows)
                 ;;(nxhtml-check-convert-to-part-by-part)
                 (list
                  (nxhtml-web-vcs-read-dl-dir "Download whole nXhtml to directory: "))))

  (let ((root (file-name-directory dl-dir)))
    (unless (file-exists-p root)
      (unless (yes-or-no-p (format "Directory %S does not exist, create it? " root))
        (error "Aborted by user"))))
  (make-directory dl-dir t)
  (let ((msg (concat "Downloading nXhtml through Launchpad web interface will take rather long\n"
                     "time (5-15 minutes) so you may want to do it in a separate Emacs session.\n\n"
                     "Do you want to download using this Emacs session? "
                     )))
    (if (not (y-or-n-p msg))
        (message "Aborted")
      (setq message-log-max t)
      (let ((do-byte (y-or-n-p "Do you want to byte compile the files after downloading? ")))
        (nxhtml-download-1 dl-dir nil do-byte)))))


(defun nxhtml-download-1 (dl-dir revision do-byte)
  "Download nXhtml to directory DL-DIR.
If REVISION is nil download latest revision, otherwise the
specified one.

If DO-BYTE is non-nil byte compile nXhtml after download."
  (let* ((has-nxhtml (and (boundp 'nxhtml-install-dir)
                          nxhtml-install-dir))
         (base-url nxhtml-web-vcs-base-url)
         (files-url (concat base-url "files/"))
         ;;(revs-url  (concat base-url "changes/"))
         (rev-part (if revision (number-to-string revision) "head%3A/"))
         (full-root-url (concat files-url rev-part))
         (web-vcs-folder-cache nil)
         (web-autoload-paranoid nil))
    ;;(nxhtml-require-base)
    (when (web-vcs-get-files-from-root 'lp full-root-url dl-dir)
      (web-vcs-display-messages t)
      (web-vcs-log nil nil "* nXhtml: Download All\n")
      (web-vcs-set&save-option 'nxhtml-autoload-web nil)
      (message "")
      (web-vcs-message-with-face 'web-vcs-green "==== Starting downloading whole nXhtml ====")
      (let ((autostart-file (expand-file-name "autostart" dl-dir)))
        (load autostart-file)
        (web-vcs-log-save)
        (web-vcs-message-with-face 'web-vcs-green "==== All files for nXhtml are now installed ====")
        (nxhtmlmaint-byte-recompile)
        (unless has-nxhtml (nxhtml-add-loading-to-custom-file autostart-file nil))))))

(defun nxhtml-check-convert-to-part-by-part ()
  (when (and (boundp 'nxhtml-install-dir)
             nxhtml-install-dir)
    (unless (and (boundp 'nxhtml-autoload-web)
                 nxhtml-autoload-web)
      (if (not (boundp 'nxhtml-menu:version))
          (error "nxhtml-install-dir set but no version found")
        (unless (string-match "[\.0-9]+" nxhtml-menu:version)
          (error "Can't find current version nxhtml-menu:version=%S" nxhtml-menu:version))
        (let* ((ver-str (match-string 0 nxhtml-menu:version))
               (ver-num (string-to-number ver-str)))
          (when (< ver-num 2.07)
            (web-vcs-message-with-face 'web-vcs-red "Too old nXhtml for download part by part.")
            (throw 'command-level nil)))))))


;;(directory-files default-directory nil "\\el$")
;;(directory-files default-directory nil "[^#~]$")
;;;###autoload
(defun nxhtml-update-existing-files ()
  "Update existing nXhtml files from the development sources.
Only files you already have will be updated.

Note that this works both if you have setup nXhtml to auto
download files as you need them or if you have downloaded all of
nXhtml at once.

For more information about installing and updating nXhtml see the
command `nxhtml-setup-install'."
  ;; Fix-me: download new files too if you are not auto downloading.
  (interactive)
  (when (y-or-n-p "Do you want to update your nXhtml files? ")
    (message "")
    (web-vcs-display-messages t)
    (web-vcs-message-with-face 'web-vcs-yellow "*\nStarting updating your nXhtml files.\n*\n")
    (message nil)
    (web-vcs-clear-folder-cache)
    (let ((vcs 'lp)
          (base-url (nxhtml-download-root-url nil))
          (dl-dir nxhtml-install-dir)
          web-vcs-folder-cache)
      (setq dl-dir (file-name-as-directory dl-dir))
      (web-vcs-update-existing-files vcs base-url dl-dir dl-dir)
      (web-vcs-clear-folder-cache))
    (display-buffer (get-buffer-create "*Compile-Log*"))
    (nxhtmlmaint-byte-recompile)
    (web-vcs-log-save)
    (web-vcs-message-with-face 'web-vcs-yellow "*\nFinished updating your nXhtml files.\n*\n")
    (message nil)))


;;(nxhtml-maybe-download-files (expand-file-name "nxhtml/doc/img/" nxhtml-install-dir) nil)
;;;###autoload
(defun nxhtml-get-missing-files (sub-dir file-name-list)
  (let (file-mask
        (root-url (nxhtml-download-root-url nil))
        files-regexp
        (full-dir (expand-file-name sub-dir nxhtml-install-dir))
        miss-names)
    (if file-name-list
        (progn
          (dolist (f file-name-list)
            (let ((full-f (expand-file-name f full-dir)))
              (unless (file-exists-p full-f)
                (setq miss-names (cons f miss-names)))))
          (setq files-regexp (regexp-opt miss-names)))
      (setq files-regexp ".*"))
    ;;(unless (file-exists-p full-dir) (make-directory full-dir t))
    (setq file-mask
	  (concat (file-relative-name (file-name-as-directory full-dir)
				      nxhtml-install-dir)
		  files-regexp))
    (let ((web-vcs-folder-cache nil))
      (web-vcs-get-missing-matching-files 'lp root-url nxhtml-install-dir
                                          file-mask))))

;; Fix-me: Does not work, Emacs Bug
;; Maybe use wget? http://gnuwin32.sourceforge.net/packages/wget.htm
;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=5103
;; (nxhtml-get-release-revision)
(defun nxhtml-get-release-revision ()
  "Get revision number for last release."
  (let* ((all-rev-url "http://code.launchpad.net/%7Enxhtml/nxhtml/main")
         (url-buf (url-retrieve-synchronously all-rev-url))
         (vcs-rec (or (assq 'lp web-vcs-links-regexp)
                      (error "Does not know web-vcs 'lp")))
         (rel-ver-regexp (nth 6 vcs-rec))
         )
    (message "%S" url-buf)
    (with-current-buffer url-buf
      (when (re-search-forward rel-ver-regexp nil t)
        (match-string 1)))))

;;;###autoload
(defun nxhtml-byte-compile-file (file &optional load)
  (let ((extra-load-path (when nxhtml-install-dir
                           (mapcar (lambda (p)
                                     (file-name-as-directory
                                      (expand-file-name p nxhtml-install-dir)))
                                   '("tests" "related" "nxhtml" "util" ".")))))
    ;; (message "nxhtml-byte-compile-file:extra-load-path=%s" extra-load-path)
    (web-vcs-byte-compile-file file load extra-load-path)))

;; fix-me: change web-vcs-byte-compile-file instead
;;;###autoload
(defun nxhtml-byte-recompile-file (file &optional load)
  "Byte recompile FILE file if necessary.
For more information see `nxhtml-byte-compile-file'.
Loading is done if recompiled and LOAD is t."
  (interactive (list (buffer-file-name)
                     t))
  (let ((elc-file (byte-compile-dest-file file)))
    (if (file-newer-than-file-p file elc-file)
        (nxhtml-byte-compile-file file load)
      (message "Byte compilation of this file is up to date."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add to custom file


(defvar nxhtml-handheld-wincfg nil)
(defun nxhtml-handheld-restore-wincg ()
  (when nxhtml-handheld-wincfg
    (set-window-configuration nxhtml-handheld-wincfg)
    (setq nxhtml-handheld-wincfg nil)))

;;(nxhtml-handheld-add-loading-to-custom-file "TEST-ME")
(defun nxhtml-handheld-add-loading-to-custom-file (file-to-load)
  (setq nxhtml-handheld-wincfg (current-window-configuration))
  (delete-other-windows)
  (let ((info-buf (get-buffer-create "Information about how to add nXhtml to (custom-file)"))
        (load-str (format "(load %S)" file-to-load)))
    (with-current-buffer info-buf
      (add-hook 'kill-buffer-hook 'nxhtml-handheld-restore-wincg nil t)
      (insert "Insert the following line to (custom-file), ie the file in the other window:\n\n")
      (let ((here (point)))
        (insert "  "
                (propertize load-str 'face 'secondary-selection)
                "\n")
        (copy-region-as-kill here (point))
        (insert "\nThe line above is in the clipboard so you can just paste it where you want it.\n")
        (insert "When ready kill this buffer.")
        (goto-char here))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    (set-window-buffer (selected-window) info-buf)
    (find-file-other-window (custom-file))))

;; (nxhtml-add-loading-to-custom-file "test-file")
(defun nxhtml-add-loading-to-custom-file (file-to-load part-by-part)
  (message "")
  (require 'cus-edit)
  (if (not (condition-case nil (custom-file) (error nil)))
      (progn
        (message "\n\n")
        (web-vcs-message-with-face
         'web-vcs-red
         (concat "Since you have started this Emacs session without running your init files"
                 "\nthey are unknown and the installation can not add the statement below."
                 "\nTo finish the setup of nXhtml you must add"
                 "\n\n  (load %S)"
                 "\n\nto your custom-file if you have not done it yet."
                 "\nYou must also customize the variable `nxhtml-autoload-web' to tell that"
                 (if part-by-part
                     "\nyou want to download nXhml files as you need them."
                   "\nyou do not want to allow automatic downloading of nXhtml files."
                   )
                 "\n")
         file-to-load)
        (message "")
        (web-vcs-display-messages t))
    (let ((prompt (concat "Basic setup of nXhtml is done, but it must be loaded from (custom-file)."
                          "\nShould I add loading of nXhtml to (custom-file) for you? ")))
      (if (yes-or-no-p prompt)
          (nxhtml-add-loading-to-custom-file-auto file-to-load)
        (if (yes-or-no-p "Should I guide you through how to do it? ")
            (nxhtml-handheld-add-loading-to-custom-file file-to-load)
          (web-vcs-message-with-face 'web-vcs-green
                                     "OK. You need to add (load %S) to your init file" file-to-load))))))

;; Fix-me: really do this? Is it safe enough?
(defun nxhtml-add-loading-to-custom-file-auto (file-to-load)
  (unless (file-name-absolute-p file-to-load)
    (error "nxhtml-add-loading-to-custom-file: Not abs file name: %S" file-to-load))
  (let ((old-buf (find-buffer-visiting (custom-file)))
        (full-to-load (expand-file-name file-to-load)))
    (with-current-buffer (or old-buf (find-file-noselect (custom-file)))
      (save-restriction
        (widen)
        (catch 'done
          (while (progn
                   (while (progn (skip-chars-forward " \t\n\^l")
                                 (looking-at ";"))
                     (forward-line 1))
                   (not (eobp)))
            (let ((start (point))
                  (form (read (current-buffer))))
              (when (eq (nth 0 form) 'load)
                (let* ((form-file (nth 1 form))
                       (full-form-file (expand-file-name form-file)))
                  (when (string= full-form-file full-to-load)
                    (throw 'done nil))
                  (when (and (string= (file-name-nondirectory full-form-file)
                                      (file-name-nondirectory full-to-load))
                             (not (string= full-form-file full-to-load)))
                    (if (yes-or-no-p "Replace current nXhtml loading in (custom-file)? ")
                        (progn
                          (goto-char start) ;; at form start now
                          (forward-char (length "(load "))
                          (skip-chars-forward " \t\n\^l") ;; at start of string
                          (setq start (point))
                          (setq form (read (current-buffer)))
                          (delete-region start (point))
                          (insert (format "%S" full-to-load))
                          (basic-save-buffer))
                      (web-vcs-message-with-face 'web-vcs-red "Can't continue then")
                      (web-vcs-display-messages t)
                      (throw 'command-level nil)))))))
          ;; At end of file
          (insert (format "\n(load  %S)\n" file-to-load))
          (basic-save-buffer))
        (unless old-buf (kill-buffer old-buf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Start Testing function
(defun emacs-Q-no-nxhtml (&rest args)
  (let* ((old-env-load-path (getenv "EMACSLOADPATH"))
         sub-env-load-path
         (elp-list (or (when old-env-load-path
                         ;;(split-string old-env-load-path ";"))
                         (split-string old-env-load-path path-separator))
                       load-path))
         (sub-elp-list nil)
         ret
         (this-emacs-exe (locate-file invocation-name
                                      (list invocation-directory)
                                      exec-suffixes)))
    (dolist (p elp-list)
      (when (file-exists-p p)
        (unless (string= nxhtml-install-dir p)
          (let* ((dir (file-name-directory p))
                 (last (file-name-nondirectory p))
                 (last-dir (file-name-nondirectory
                            (directory-file-name dir))))
            (unless (and (string= "nxhtml" last-dir)
                         (member last '("util" "test" "nxhtml" "related" "alt")))
              (setq sub-elp-list (cons p sub-elp-list)))))))
    ;;(setq sub-env-load-path (mapconcat 'identity (reverse sub-elp-list) ";"))
    (setq sub-env-load-path (mapconcat 'identity (reverse sub-elp-list) path-separator))
    (setenv "EMACSLOADPATH" sub-env-load-path)
    (setq ret (apply 'call-process this-emacs-exe nil 0 nil "-Q" args))
    (setenv "EMACSLOADPATH" old-env-load-path)
    ret))

;; (call-interactively-p 'nxhtml-setup-test-auto-download)
;; (nxhtml-setup-test-auto-download "c:/test2/")
(defun nxhtml-setup-test-auto-download (test-dir)
  "Test autoload in a new emacs, started with 'emacs -Q'.
You can choose where to download the files and just delete them
when you have tested enough."
  (interactive (list (read-directory-name "Directory for test of auto download of nXhtml: ")))
  (let ((this-dir (file-name-directory web-vcs-el-this))
        (this-name (file-name-nondirectory web-vcs-el-this))
        that-file)
    (when (and (file-exists-p test-dir)
               (not (y-or-n-p (format "Directory %S exists, really test there? " test-dir))))
      (error "Aborted"))
    (unless (file-exists-p test-dir) (make-directory test-dir))
    (setq that-file (expand-file-name this-name test-dir))
    (when (file-exists-p that-file) (delete-file that-file))
    (copy-file web-vcs-el-this that-file)
    (emacs-Q-no-nxhtml "-l" that-file "-f" "nxhtml-setup-test-auto-download-do-it-here")))

(defun nxhtml-setup-test-auto-download-do-it-here ()
  "Helper for `nxhtml-setup-test-auto-down-load'."
  (let ((this-dir (file-name-directory web-vcs-el-this)))
    (nxhtml-setup-auto-download this-dir)))

(defun web-vcs-check-if-modified ()
  (let (
        (t1 (format-time-string "%Y-%m-%dT%T%z" (date-to-time "2010-01-01 18:20")))
        (t2 (format-time-string "%Y-%m-%dT%T%z" (date-to-time "Mon, 28 Dec 2009 08:57:44 GMT")))
        (url-request-extra-headers
         (list
          (cons "If-Modified-Since"
                (format-time-string
                 ;;"%Y-%m-%dT%T%z"
                 "%a, %e %b %Y %H:%M:%S GMT"
                 (nth 5 (file-attributes "c:/test/temp.el" )))
                )))
        xb)
    (setq xb (url-retrieve-synchronously "http://www.emacswiki.org/emacs/download/anything.el"))
    (switch-to-buffer xb)
    ))
;; (emacs-Q-no-nxhtml "web-vcs.el" "-l" "c:/test/d27/web-autostart.el")
;; (emacs-Q-no-nxhtml "web-vcs.el" "-l" "c:/test/d27/autostart.el")
;; (emacs-Q-no-nxhtml "web-vcs.el" "-f" "eval-buffer" "-f" "nxhtml-temp-setup-auto-download")
;; (emacs-Q-no-nxhtml "-l" "c:/test/d27/web-vcs" "-l" "c:/test/d27/nxhtml-web-vcs" "-f" "nxhtml-temp-setup-auto-download")
;; (emacs-Q-no-nxhtml "-l" "c:/test/d27/nxhtml-web-vcs" "-f" "nxhtml-temp-setup-auto-download")
;; (emacs-Q-no-nxhtml "--geometry=200x50+100+100" "-l" "c:/test/d27/web-vcs" "-f" "web-vcs-nxhtml")
(defun nxhtml-temp-setup-auto-download ()
  ;;(when (fboundp 'w32-send-sys-command) (w32-send-sys-command #xf030) (sit-for 2))
  (set-frame-size (selected-frame)
                  (/ 1024 (frame-char-width))
                  (/ 512 (frame-char-height))
                  )
  (tool-bar-mode -1)
  (set-frame-position (selected-frame) 100 50)
  (when (y-or-n-p "Do nXhtml? ")
    (view-echo-area-messages)
    (setq truncate-lines t)
    (split-window-horizontally)
    (let ((load-path (cons default-directory load-path)))
      (require 'web-vcs))
    ;(nxhtml-setup-auto-download "c:/test/d27")
    (call-interactively 'nxhtml-setup-auto-download)
    ))
;;;;;; End Testing function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'nxhtml-web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-web-vcs.el ends here

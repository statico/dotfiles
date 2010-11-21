;;; web-vcs.el --- Download file trees from VCS web pages
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-26 Thu
(defconst web-vcs:version "0.61") ;; Version:
;; Last-Updated: 2009-12-11 Fri
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
;; Update file trees within Emacs from VCS systems using information
;; on their web pages.
;;
;; Available download commands are currently:
;;
;;    `web-vcs-nxhtml'
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
(eval-and-compile  (require 'cus-edit))
(eval-and-compile  (require 'mm-decode))
(eval-when-compile (require 'url-http))

(require 'advice)
(require 'web-autoload nil t)
;; (require 'url-util)
;; (require 'url)
;;(require 'url-parse)

(defvar web-vcs-comp-dir nil)

(defgroup web-vcs nil
  "Customization group for web-vcs."
  :group 'nxhtml)

(defcustom web-vcs-links-regexp
  `(
    (lp ;; Id
     ;; Comment:
     "http://www.launchpad.com/ uses this 2009-11-29 with Loggerhead 1.10 (generic?)"
     ;; Files URL regexp:
     ;;
     ;; Extend this format to catch date/time too.
     ;;
     ;; ((patt (rx ...))
     ;;  ;; use subexp numbers
     ;;  (url 1)
     ;;  (time 2)
     ;;  (rev 3))

     ((time 1)
      (url 2)
      (patt ,(rx "<td class=\"date\">"
                 (submatch (regexp "[^<]*"))
                 "</td>"
                 (0+ space)
                 "<td class=\"timedate2\">"
                 (regexp ".+")
                 "</td>"
                 (*? (regexp ".\\|\n"))
                 "href=\""
                 (submatch (regexp ".*/download/[^\"]*"))
                 "\"")))

     ;; ,(rx "href=\""
     ;;      (submatch (regexp ".*/download/[^\"]*"))
     ;;      "\"")

     ;; Dirs URL regexp:
     ,(rx "href=\""
          (submatch (regexp ".*%3A/[^\"]*/"))
          "\"")
     ;; File name URL part regexp:
     "\\([^\/]*\\)$"
     ;; Page revision regexp:
     ,(rx "for revision"
          (+ whitespace)
          "<span>"
          (submatch (+ digit))
          "</span>")
     ;; Release revision regexp:
     ,(rx "/"
          (submatch (+ digit))
          "\"" (+ (not (any ">"))) ">"
          (optional "Release ")
          (+ digit) "." (+ digit) "<")
     )
    )
  "Regexp patterns for matching links on a VCS web page.
The patterns are grouped by VCS web system type.

*Note: It is always sub match 1 from these patterns that are
       used."
  :type '(repeat
          (list
           (symbol :tag "VCS web system type specifier")
           (string :tag "Description")
           (regexp :tag "Files URL regexp")
           (regexp :tag "Dirs URL regexp")
           (regexp :tag "File name URL part regexp")
           (regexp :tag "Page revision regexp")
           (regexp :tag "Release revision regexp")
           ))
  :group 'web-vcs)

(defface web-vcs-mode-line
  '((t (:foreground "black" :background "OrangeRed")))
  "Mode line face during download."
  :group 'web-vcs)

(defface web-vcs-mode-line-inactive
  '((t (:foreground "black" :background "Orange")))
  "Mode line face during download."
  :group 'web-vcs)

(defface web-vcs-gold
  '((t (:foreground "black" :background "gold")))
  "Face for web-vcs messages."
  :group 'web-vcs)

(defface web-vcs-red
  '((t (:foreground "black" :background "#f86")))
  "Face for web-vcs messages."
  :group 'web-vcs)

(defface web-vcs-green
  '((t (:foreground "black" :background "#8f6")))
  "Face for web-vcs messages."
  :group 'web-vcs)

(defface web-vcs-yellow
  '((t (:foreground "black" :background "yellow")))
  "Face for web-vcs messages."
  :group 'web-vcs)

(defface web-vcs-pink
  '((t (:foreground "black" :background "pink")))
  "Face for web-vcs messages."
  :group 'web-vcs)

(defcustom web-vcs-default-download-directory
  '~/.emacs.d/
  "Default download directory."
  :type '(choice (const :tag "~/.emacs.d/" '~/.emacs.d/)
                 (const :tag "Fist site-lisp in `load-path'" 'site-lisp-dir)
                 (const :tag "Directory where `site-run-file' lives" 'site-run-dir)
                 (string :tag "Specify directory"))
  :group 'web-vcs)

;;(web-vcs-default-download-directory)
;;;###autoload
(defun web-vcs-default-download-directory ()
  "Try to find a suitable place.
Considers site-start.el, site-
"
  (let ((site-run-dir (when site-run-file
			(file-name-directory (locate-library site-run-file))))
        (site-lisp-dir (catch 'first-site-lisp
                         (dolist (d load-path)
                           (let ((dir (file-name-nondirectory (directory-file-name d))))
                             (when (string= dir "site-lisp")
                               (throw 'first-site-lisp (file-name-as-directory d)))))))
        )
    (message "site-run-dir=%S site-lisp-dir=%S" site-run-dir site-lisp-dir)
    (case web-vcs-default-download-directory
      ('~/.emacs.d/ "~/.emacs.d/")
      ('site-lisp-dir site-lisp-dir)
      ('site-run-dir site-run-dir)
      (t web-vcs-default-download-directory))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging

(defcustom web-vcs-log-file "~/.emacs.d/web-vcs-log.org"
  "Log file for web-vcs."
  :type 'file
  :group 'web-vcs)

;;;###autoload
(defun web-vcs-log-edit ()
  "Open log file."
  (interactive)
  (find-file web-vcs-log-file))

(defvar web-vcs-log-save-timer nil)

(defun web-vcs-log-save-when-idle ()
  (when (timerp web-vcs-log-save-timer) (cancel-timer web-vcs-log-save-timer))
  (run-with-idle-timer 0 nil 'web-vcs-log-save))

(defun web-vcs-log-save ()
  (let ((log-buf (find-buffer-visiting web-vcs-log-file)))
    (when (and log-buf (buffer-modified-p log-buf))
      (with-current-buffer log-buf
          (basic-save-buffer)))
    log-buf))

(defun web-vcs-log-close ()
  (let ((log-buf (web-vcs-log-save)))
    (when log-buf
      (kill-buffer log-buf))))

;; Fix-me: Add some package descriptor to log
(defun web-vcs-log (url dl-file msg)
  (unless (file-exists-p web-vcs-log-file)
    (let ((dir (file-name-directory web-vcs-log-file)))
      (unless (file-directory-p dir)
        (make-directory dir))))
  (with-current-buffer (find-file-noselect web-vcs-log-file)
    (setq buffer-save-without-query t)
    (web-vcs-log-save-when-idle)
    (save-restriction
      (widen)
      (let ((today-entries (format-time-string "* %Y-%m-%d"))
            (now (format-time-string "%H:%M:%S GMT" nil t)))
        (goto-char (point-max))
        (unless (re-search-backward (concat "^" today-entries) nil t)
          (goto-char (point-max))
          (insert "\n" today-entries "\n"))
        (goto-char (point-max))
        (when url
          (insert "** Downloading file " now "\n"
                  (format "   file [[file:%s][%s]]\n   from %s\n" dl-file dl-file url)
                  ))
        (cond
         ((stringp msg)
          (goto-char (point-max))
          (insert msg "\n"))
         (msg (basic-save-buffer)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding and downloading files

;;;###autoload
(defun web-vcs-get-files-from-root (web-vcs url dl-dir)
  "Download a file tree from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Show URL first and offer to visit the page.  That page will give
you information about version control system \(VCS) system used
etc."
  (unless (web-vcs-contains-moved-files dl-dir)
    (when (if (not (y-or-n-p (concat "Download files from \"" url "\".\n"
                                     "You can see on that page which files will be downloaded.\n\n"
                                     "Visit that page before downloading? ")))
              t
            (browse-url url)
            (if (y-or-n-p "Start downloading? ")
                t
              (message "Aborted")
              nil))
      (message "")
      (web-vcs-get-files-on-page web-vcs url t (file-name-as-directory dl-dir) nil)
      t)))

(defun web-vcs-get-files-on-page (web-vcs url recursive dl-dir test)
  "Download files listed by WEB-VCS on web page URL.
WEB-VCS is a specifier in `web-vcs-links-regexp'.

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

Before downloading check if the downloaded revision already is
the same as the one on the web page.  This is stored in the file
web-vcs-revision.txt.  After downloading update this file.

If TEST is non-nil then do not download, just list the files."
  (unless (string= dl-dir (file-name-as-directory (expand-file-name dl-dir)))
    (error "Download dir dl-dir=%S must be a full directory path" dl-dir))
  (catch 'command-level
    (when (web-vcs-contains-moved-files dl-dir)
      (throw 'command-level nil))
    (let ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                       (error "Does not know web-cvs %S" web-vcs)))
          (start-time (current-time)))
      (unless (file-directory-p dl-dir)
        (if (yes-or-no-p (format "Directory %S does not exist, create it? "
                                 (file-name-as-directory
                                  (expand-file-name dl-dir))))
            (make-directory dl-dir t)
          (message "Can't download then")
          (throw 'command-level nil)))
      ;; (let ((old-win (selected-window)))
      ;;   (unless (eq (get-buffer "*Messages*") (window-buffer old-win))
      ;;     (switch-to-buffer-other-window "*Messages*"))
      ;;   (goto-char (point-max))
      ;;   (insert "\n")
      ;;   (insert (propertize (format "\n\nWeb-Vcs Download: %S\n" url) 'face 'web-vcs-gold))
      ;;   (insert "\n")
      ;;   (redisplay t)
      ;;   (set-window-point (selected-window) (point-max))
      ;;   (select-window old-win))
      (web-vcs-message-with-face 'web-vcs-gold "\n\nWeb-Vcs Download: %S\n" url)
      (web-vcs-display-messages nil)
      (let* ((rev-file (expand-file-name "web-vcs-revision.txt" dl-dir))
             (rev-buf (find-file-noselect rev-file))
             ;; Fix-me: Per web vcs speficier.
             (old-rev-range (with-current-buffer rev-buf
                              (widen)
                              (goto-char (point-min))
                              (when (re-search-forward (format "%s:\\(.*\\)\n" web-vcs) nil t)
                                ;;(buffer-substring-no-properties (point-min) (line-end-position))
                                ;;(match-string 1)
                                (cons (match-beginning 1) (match-end 1))
                                )))
             (old-revision (when old-rev-range
                             (with-current-buffer rev-buf
                               (buffer-substring-no-properties (car old-rev-range)
                                                               (cdr old-rev-range)))))
             (dl-revision (web-vcs-get-revision-on-page vcs-rec url))
             ret
             moved)
        (when (and old-revision (string= old-revision dl-revision))
          (when (y-or-n-p (format "You already have revision %s.  Quit? " dl-revision))
            (message "Aborted")
            (kill-buffer rev-buf)
            (throw 'command-level nil)))
        ;; We do not have a revision number once we start download.
        (with-current-buffer rev-buf
          (when old-rev-range
            (delete-region (car old-rev-range) (cdr old-rev-range))
            (basic-save-buffer)))
        (setq ret (web-vcs-get-files-on-page-1
                   vcs-rec url
                   dl-dir
                   ""
                   nil
                   (if recursive 0 nil)
                   dl-revision test))
        (setq moved       (nth 1 ret))
        ;; Now we have a revision number again.
        (with-current-buffer rev-buf
          (when (= 0 (buffer-size))
            (insert "WEB VCS Revisions\n\n"))
          (goto-char (point-max))
          (unless (eolp) (insert "\n"))
          (insert (format "%s:%s\n" web-vcs dl-revision))
          (basic-save-buffer)
          (kill-buffer))
        (message "-----------------")
        (web-vcs-message-with-face 'web-vcs-gold "Web-Vcs Download Ready: %S" url)
        (web-vcs-message-with-face 'web-vcs-gold "  Time elapsed: %S"
                                   (web-vcs-nice-elapsed start-time (current-time)))
        (when (> moved 0)
          (web-vcs-message-with-face 'web-vcs-yellow
                                     "  %i files updated (old versions renamed to *.moved)"
                                     moved))))))

(defun web-vcs-get-files-on-page-1 (vcs-rec url dl-root dl-relative file-mask recursive dl-revision test)
  "Download files listed by VCS-REC on web page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'.

If FILE-MASK is non nil then it is used to match a file path.
Only matching files will be downloaded.  FILE-MASK can have two
forms, a regular expression or a function.

If FILE-MASK is a regular expression then each part of the path
may be a regular expresion \(not containing /).

If FILE-MASK is a function then this function is called in each
directory under DL-ROOT.  The function is called with the
directory as a parameter and should return a cons. The first
element of the cons should be a regular expression matching file
names in that directory that should be downloaded.  The cdr
should be t if subdirectories should be visited.

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

The revision on the page URL should match DL-REVISION if this is non-nil.

If TEST is non-nil then do not download, just list the files"
  ;;(web-vcs-message-with-face 'font-lock-comment-face "web-vcs-get-files-on-page-1 %S %S %S %S" url dl-root dl-relative file-mask)
  (let* ((files-matcher      (nth 2 vcs-rec))
         (dirs-href-regexp   (nth 3 vcs-rec))
         (revision-regexp    (nth 5 vcs-rec))
         (dl-dir (file-name-as-directory (expand-file-name dl-relative dl-root)))
         (lst-dl-relative (web-vcs-file-name-as-list dl-relative))
         (lst-file-mask   (when (stringp file-mask) (web-vcs-file-name-as-list file-mask)))
         ;;(url-buf (url-retrieve-synchronously url))
         this-page-revision
         files
         suburls
         (moved 0)
         (temp-file-base (expand-file-name "web-vcs-temp-list.tmp" dl-dir))
         temp-list-file
         temp-list-buf
         folder-res
         http-sts)
    ;; Fix-me: It looks like there is maybe a bug in url-copy-file so
    ;; that it runs synchronously. Try to workaround the problem by
    ;; making a new file temp file name.
    (web-vcs-display-messages nil)
    (unless (file-directory-p dl-dir) (make-directory dl-dir t))
    ;;(message "TRACE: dl-dir=%S" dl-dir)
    (setq temp-list-file (make-temp-name temp-file-base))
    (setq temp-list-buf (web-vcs-ass-folder-cache url))
    (unless temp-list-buf
      ;;(setq temp-list-buf (generate-new-buffer "web-wcs-folder"))
      ;;(web-vcs-url-copy-file-and-check url temp-list-file nil)
      (setq folder-res (web-vcs-url-retrieve-synch url))
      ;; (with-current-buffer temp-list-buf
      ;;   (insert-file-contents temp-list-file))
      (unless (memq (cdr folder-res) '(200 201))
        (web-vcs-message-with-face 'web-vcs-red "Could not get %S" url)
          (web-vcs-display-messages t)
        (throw 'command-level nil)))
    ;;(with-current-buffer temp-list-buf
    (with-current-buffer (car folder-res)
      ;;(delete-file temp-list-file)
      ;;(find-file-noselect temp-list-file)
      (when dl-revision
        (setq this-page-revision (web-vcs-get-revision-from-url-buf vcs-rec (current-buffer) url)))
      (when dl-revision
        (unless (string= dl-revision this-page-revision)
          (web-vcs-message-with-face 'web-vcs-red "Revision on %S is %S, but should be %S"
                                     url this-page-revision dl-revision)
          (web-vcs-display-messages t)
          (throw 'command-level nil)))
      ;; Find files
      (goto-char (point-min))
      (let ((files-href-regexp (nth 1 (assq 'patt files-matcher)))
            (url-num           (nth 1 (assq 'url  files-matcher)))
            (time-num          (nth 1 (assq 'time files-matcher))))
        (while (re-search-forward files-href-regexp nil t)
          (let ((file (match-string url-num))
                (time (match-string time-num)))
            (add-to-list 'files (list file time)))))
      ;; Find subdirs
      (when recursive
        (goto-char (point-min))
        (while (re-search-forward dirs-href-regexp nil t)
          (let ((suburl (match-string 1))
                (lenurl (length url)))
            (when (and (> (length suburl) lenurl)
                       (string= (substring suburl 0 lenurl) url))
              (add-to-list 'suburls suburl)))))
      (kill-buffer))
    ;; Download files
    ;;(message "TRACE: files=%S" files)
    (web-vcs-download-files vcs-rec files dl-dir dl-root file-mask)
    ;; Download subdirs
    (when suburls
      (dolist (suburl (reverse suburls))
        (let* ((dl-sub-dir (substring suburl (length url)))
               (full-dl-sub-dir (file-name-as-directory
                                 (expand-file-name dl-sub-dir dl-dir)))
               (rel-dl-sub-dir (file-relative-name full-dl-sub-dir dl-root)))
          ;;(message "web-vcs-get-revision-from-url-buf dir: %S %S" file-mask rel-dl-sub-dir)
          (when (or (not file-mask)
                    (not (stringp file-mask))
                    (web-vcs-match-folderwise file-mask rel-dl-sub-dir))
            ;;(message "matched dir %S" rel-dl-sub-dir)
            (unless (web-vcs-contains-file dl-dir full-dl-sub-dir)
              (error "Subdir %S not in %S" dl-sub-dir dl-dir))
            (let* ((ret (web-vcs-get-files-on-page-1 vcs-rec
                                                     suburl
                                                     dl-root
                                                     rel-dl-sub-dir
                                                     file-mask
                                                     (1+ recursive)
                                                     this-page-revision
                                                     test)))
              (setq moved (+ moved (nth 1 ret))))))))
    (list this-page-revision moved)))

(defun web-vcs-get-missing-matching-files (web-vcs url dl-dir file-mask)
  "Download missing files from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Before downloading offer to visit the page from which the
downloading will be made.
"
  (let ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                     (error "Does not know web-cvs %S" web-vcs))))
    (web-vcs-get-files-on-page-1 vcs-rec url dl-dir "" file-mask 0 nil nil)))


;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/" t "c:/test/temp13/" t)

(defvar web-vcs-folder-cache nil) ;; dyn var
(defun web-vcs-add-folder-cache (url buf)
  (add-to-list 'web-vcs-folder-cache (list url buf)))
(defun web-vcs-ass-folder-cache (url)
  (assoc url web-vcs-folder-cache))
(defun web-vcs-clear-folder-cache ()
  (while web-vcs-folder-cache
    (let ((ub (car web-vcs-folder-cache)))
      (setq web-vcs-folder-cache (cdr web-vcs-folder-cache))
      (kill-buffer (nth 1 ub)))))

(defun web-vcs-url-copy-file-and-check (url dl-file dest-file)
  "Copy URL to DL-FILE.
Log what happened. Use DEST-FILE in the log, not DL-FILE which is
a temporary file."
  (let ((http-sts nil)
        (file-nonempty nil)
        (fail-reason nil))
    (when dest-file (web-vcs-log url dest-file nil))
    (web-vcs-display-messages nil)
    ;;(message "before url-copy-file %S" dl-file)
    (setq http-sts (web-vcs-url-copy-file url dl-file nil t)) ;; don't overwrite, keep time
    ;;(message "after  url-copy-file %S" dl-file)
    (if (and (file-exists-p dl-file)
             (setq file-nonempty (< 0 (nth 7 (file-attributes dl-file)))) ;; file size 0
             (memq http-sts '(200 201)))
        (when dest-file
          (web-vcs-log nil nil "   Done.\n"))
      (setq fail-reason
            (cond
             (http-sts (format "HTTP %s" http-sts))
             (file-nonempty "File looks bad")
             (t "Server did not respond")))
      (unless dest-file (web-vcs-log url dl-file "TEMP FILE"))
      (web-vcs-log nil nil (format "   *Failed:* %s\n" fail-reason))
      ;; Requires user attention and intervention
      (web-vcs-message-with-face 'web-vcs-red "Download failed: %s, %S" fail-reason url)
      (web-vcs-display-messages t)
      (message "\n")
      (web-vcs-message-with-face 'web-vcs-yellow "Please retry what you did before!\n")
      (throw 'command-level nil))))

(defvar web-autoload-temp-file-prefix "TEMPORARY-WEB-AUTO-LOAD-")
(defvar web-autoload-active-file-sub-url) ;; Dyn var, active during file download check
(defun web-autoload-acvtive ()
  (and (boundp 'web-autoload-active-file-sub-url)
       web-autoload-active-file-sub-url))

(defun web-vcs-download-files (vcs-rec files dl-dir dl-root file-mask)
  (dolist (file (reverse files))
    (let* ((url-file          (nth 0 file))
           (url-file-time-str (nth 1 file))
           ;; date-to-time assumes GMT so this is ok:
           (url-file-time     (when url-file-time-str (date-to-time url-file-time-str)))
           (url-file-name-regexp  (nth 4 vcs-rec))
           (url-file-rel-name (progn
                                (when (string-match url-file-name-regexp url-file)
                                  (match-string 1 url-file))))
           (dl-file-name (expand-file-name url-file-rel-name dl-dir))
           (dl-file-time (nth 5 (file-attributes dl-file-name)))
           (file-rel-name (file-relative-name dl-file-name dl-root))
           (file-name (file-name-nondirectory dl-file-name))
           (temp-file (expand-file-name (concat web-autoload-temp-file-prefix file-name) dl-dir))
           temp-buf)
      (cond
       ((and file-mask (not (web-vcs-match-folderwise file-mask file-rel-name))))
       ((and dl-file-time
             url-file-time
             (progn
               ;;(message "dl-file-time =%s" (when dl-file-time (current-time-string dl-file-time)))
               ;;(message "url-file-time=%s" (when url-file-time (current-time-string url-file-time)))
               ;;(message "url-file-tstr=%s" (when url-file-time url-file-time-str))
               t)
             (time-less-p url-file-time
                          (time-add dl-file-time (seconds-to-time 1))))
        (web-vcs-message-with-face 'web-vcs-green "Local file %s is newer or same age" file-rel-name))
       ;;(test (progn (message "TEST url-file=%S" url-file) (message "TEST url-file-rel-name=%S" url-file-rel-name) (message "TEST dl-file-name=%S" dl-file-name) ))
       (t
        ;; Avoid trouble with temp file
        (while (setq temp-buf (find-buffer-visiting temp-file))
          (set-buffer-modified-p nil) (kill-buffer temp-buf))
        (when (file-exists-p temp-file) (delete-file temp-file))
        ;;(web-vcs-message-with-face 'font-lock-comment-face "Starting url-copy-file %S %S t t" url-file temp-file)
        (web-vcs-url-copy-file-and-check url-file temp-file dl-file-name)
        ;;(web-vcs-message-with-face 'font-lock-comment-face "Finished url-copy-file %S %S t t" url-file temp-file)
        (let* ((time-after-url-copy (current-time))
               (old-buf-open (find-buffer-visiting dl-file-name)))
          (when (and old-buf-open (buffer-modified-p old-buf-open))
            (save-excursion
              (switch-to-buffer old-buf-open)
              (when (y-or-n-p (format "Buffer %S is modified, save to make a backup? " dl-file-name))
                (save-buffer))))
          (if (and dl-file-time (web-vcs-equal-files dl-file-name temp-file))
              (progn
                (delete-file temp-file)
                (when url-file-time (set-file-times dl-file-name url-file-time))
                (web-vcs-message-with-face 'web-vcs-green "File %S was ok" dl-file-name))
            (when dl-file-time
              (let ((backup (concat dl-file-name ".moved")))
                (rename-file dl-file-name backup t)))
            ;; Be paranoid and let user check here. I actually
            ;; believe that is a very good thing here.
            (web-vcs-be-paranoid temp-file dl-file-name file-rel-name)
            (rename-file temp-file dl-file-name)
            (when url-file-time (set-file-times dl-file-name url-file-time))
            ;; (let ((buf (find-buffer-visiting dl-file-name)))
            ;;   (when buf
            ;;     (with-current-buffer buf
            ;;       (message "before revert-buffer")
            ;;       (revert-buffer nil t t)
            ;;       (message "after revert-buffer")
            ;;       )))
            (if dl-file-time
                (web-vcs-message-with-face 'web-vcs-yellow "Updated %S" dl-file-name)
              (web-vcs-message-with-face 'web-vcs-green "Downloaded %S" dl-file-name))
            (when old-buf-open
              (with-current-buffer old-buf-open
                (set-buffer-modified-p nil)
                (revert-buffer nil t t)))
            (with-current-buffer (find-file-noselect dl-file-name)
              (setq header-line-format
                    (propertize (format-time-string "This file was downloaded %Y-%m-%d %H:%M")
                                'face 'web-vcs-green))))
          (web-vcs-display-messages nil)
          ;; This is both for user and remote server load.  Do not remove this.
          (redisplay t) (sit-for (- 1.0 (float-time (time-subtract (current-time) time-after-url-copy))))
          ;; (unless old-buf-open
          ;;   (when old-buf
          ;;     (kill-buffer old-buf)))
          )))
      (redisplay t))))

(defun web-vcs-get-revision-on-page (vcs-rec url)
  "Get revision number using VCS-REC on page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'."
  ;; url-insert-file-contents
  (let ((url-buf (url-retrieve-synchronously url)))
    (web-vcs-get-revision-from-url-buf vcs-rec url-buf url)))

(defun web-vcs-get-revision-from-url-buf (vcs-rec url-buf url)
  "Get revision number using VCS-REC.
VCS-REC should be an entry in the list `web-vcs-links-regexp'.
The buffer URL-BUF should contain the content on page URL."
  (let ((revision-regexp    (nth 5 vcs-rec)))
    ;; Get revision number
    (with-current-buffer url-buf
      (goto-char (point-min))
      (if (not (re-search-forward revision-regexp nil t))
          (progn
            (web-vcs-message-with-face 'web-vcs-red "Can't find revision number on %S" url)
            (web-vcs-display-messages t)
            (throw 'command-level nil))
        (match-string 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Download


;; fix-me: To emulation-mode-map
;; Fix-me: put this on better keys
(defvar web-vcs-paranoid-state-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c)(control ?c)] 'exit-recursive-edit)
    (define-key map [(control ?c)(control ?n)] 'web-autoload-continue-no-stop)
    (define-key map [(control ?c)(control ?r)] 'web-vcs-investigate-elisp-file)
    (define-key map [(control ?c)(control ?q)] 'web-vcs-quit-auto-download)
    map))

(defun web-vcs-quit-auto-download ()
  "Quit download process.
This stops the current web autoload processing."
  (interactive)
  ;; Fix-me.
  (when (y-or-n-p "Stop web autoload processing? You can resume it later. ")
    (web-vcs-message-with-face 'web-vcs-red
                               "Stopped autoloading in process. It will be resumed when necessary again.")
    (web-vcs-log nil nil "User stopped autoloading")
    (throw 'top-level 'web-autoload-stop)))

(define-minor-mode web-vcs-paranoid-state-mode
  "Mode used temporarily during user check of downloaded file.
Do not turn on this yourself."
  :lighter (concat " " (propertize "Download file check" 'face 'font-lock-warning-face))
  :global t
  :group 'web-vcs
  (or (not web-vcs-paranoid-state-mode)
      (web-autoload-acvtive)
      (error "This mode can't be used when not downloading")))

(defcustom web-autoload-paranoid t
  "Be paranoid and break to check each file after download."
  :type 'boolean
  :group 'web-vcs)

(defun web-autoload-continue-no-stop ()
  "Continue web auto download.
This is used after inspecting downloaded elisp files.  Set
`web-autoload-paranoid' to nil before contiuning to avoid further
breaks to check downloaded files."
  (interactive)
  (setq web-autoload-paranoid nil)
  (web-autoload-continue))

(defun web-autoload-continue ()
  "Continue web auto download.
This is used after inspecting downloaded elisp files."
  (interactive)
  (if (< 0 (recursion-depth))
      (exit-recursive-edit)
    (web-autoload-byte-compile-queue)))

(defun web-vcs-be-paranoid (temp-file file-dl-name file-sub-url)
  "Be paranoid and check FILE-DL-NAME."
  (when (or (not (boundp 'web-autoload-paranoid))
            web-autoload-paranoid)
    (save-window-excursion
      (let* ((comp-buf (get-buffer "*Compilation*"))
             (comp-win (and comp-buf
                            (get-buffer-window comp-buf)))
             (msg-win (web-vcs-display-messages nil))
             temp-buf
             (kf-desc (lambda (fun)
                        (let* ((key (where-is-internal fun nil t))
                               (k-desc (when key (key-description key)))
                               (fmt-kf "\n    %s (or %s)")
                               (fmt-f  "\n    %s"))
                          (if key
                              (format fmt-kf k-desc fun)
                            (format fmt-f fun)
                            )))))
        (if comp-win
            (progn
              (select-window comp-win)
              (find-file file-dl-name))
          (select-window msg-win)
          (find-file-other-window temp-file))
        (setq temp-buf (current-buffer))
        (web-vcs-log-save)
        (message "-")
        (message "")
        (with-selected-window msg-win
          (goto-char (point-max)))
        (let ((proceed nil)
              (web-autoload-active-file-sub-url file-sub-url)) ;; Dyn var, active during file download check
          (web-vcs-paranoid-state-mode 1)
          (web-vcs-message-with-face
           'secondary-selection
           (concat "Please check the downloaded file and then continue by doing"
                   (funcall kf-desc 'exit-recursive-edit)
                   (if (fboundp 'web-autoload-continue-no-stop)
                       (concat
                        "\n\nOr, for no more breaks to check files do"
                        (funcall kf-desc 'web-autoload-continue-no-stop))
                     "")
                   "\n\nTo stop the web autoloading process for now do"
                   (funcall kf-desc 'web-autoload-quit-download)
                   "\n\nTo see the log file you can do"
                   (funcall kf-desc 'web-vcs-log-edit)
                   "\n"))
          (message "")
          (while (not proceed)
            (condition-case err
                (when (eq 'web-autoload-stop
                          (catch 'top-level
                            ;; Fix-me: review file before rename!
                            (setq header-line-format
                                  (propertize
                                   (format "Review for downloading. Continue: C-c C-c%s. Destination: %S"
                                           (if (string= "el" (file-name-extension file-dl-name))
                                               ", Check: C-c C-r"
                                             "")
                                           file-dl-name)
                                   'face 'web-vcs-red))
                            (unwind-protect
                                (progn
                                  (recursive-edit))
                              (web-vcs-paranoid-state-mode -1))
                            (with-current-buffer temp-buf
                              (set-buffer-modified-p nil)
                              (kill-buffer temp-buf))
                            (setq proceed t)))
                  (throw 'top-level t))
              (error (message "%s" (error-message-string err))))))
        (web-vcs-display-messages t)
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Download Compile Queue
;;
;; Downloaded elisp files are placed in a compile queue. They are not
;; compiled until all required elisp files are downloaded (and
;; optionally compiled).
;;
;; This mechanism works through
;; - reading (eval-when-compile ...) etc in the files
;; - a defadviced require that is the driver of the process

(defvar web-autoload-compile-queue nil)

(defvar web-autoload-byte-compile-queue-active nil) ;; Dyn var

(defun web-autoload-byte-compile-file (file load comp-fun)
  (if nil ;;(file-exists-p file)
      (byte-compile-file file load)
    (let ((added-entry (list file load comp-fun nil)))
      (if (member added-entry web-autoload-compile-queue)
          (setq added-entry nil)
        (web-vcs-message-with-face 'web-vcs-gold "Add to compile queue (%S %s)" file load)
        (setq web-autoload-compile-queue (cons added-entry
                                               web-autoload-compile-queue)))
      (when added-entry
        (if web-autoload-byte-compile-queue-active
            (throw 'web-autoload-comp-restart t)
          (web-autoload-byte-compile-queue))))))

;;(web-autoload-byte-compile-queue)
(defun web-autoload-byte-compile-queue ()
  (let ((top-entry)
        (web-autoload-byte-compile-queue-active t))
    (while (and web-autoload-compile-queue
                (not (equal top-entry
                            (car web-autoload-compile-queue))))
      (setq top-entry (car web-autoload-compile-queue))
      (catch 'web-autoload-comp-restart
        (web-autoload-byte-compile-first)
        (setq web-autoload-compile-queue (cdr web-autoload-compile-queue))))))

(defun web-autoload-byte-compile-first ()
  "Compile first file on compile queue and maybe load it.
Compile the car of `web-autoload-compile-queue' and load if this
entry says so."
  (let* ((compiled-it nil)
         (first-entry (car web-autoload-compile-queue))
         (el-file  (nth 0 first-entry))
         (load     (nth 1 first-entry))
         (comp-fun (nth 2 first-entry))
         (req-done (nth 3 first-entry))
         (elc-file (byte-compile-dest-file el-file))
         (need-compile (or (not (file-exists-p elc-file))
                           (file-newer-than-file-p el-file elc-file))))
    (if (not need-compile)
        nil ;;(when load (load elc-file))
      (unless req-done
        (web-autoload-do-eval-requires el-file)
        (setcar (nthcdr 3 first-entry) t))
      (when (catch 'web-autoload-comp-restart
              (condition-case err
                  (progn
                    (web-vcs-message-with-face 'font-lock-comment-face "Start byte compiling %S" el-file)
                    (web-vcs-message-with-face 'web-vcs-pink "Compiling QUEUE: %S" web-autoload-compile-queue)
                    (let ((web-autoload-skip-require-advice t)) (funcall comp-fun el-file load))
                    (web-vcs-message-with-face 'font-lock-comment-face "Ready byte compiling %S" el-file)
                    ;; Return nil to tell there are no known problems
                    (if (file-exists-p elc-file)
                        nil
                      (web-vcs-message-with-face
                       'web-vcs-red "Error: byte compiling did not produce %S" elc-file)
                      (web-vcs-display-messages nil)
                      ;; Clean up before restart
                      (web-autoload-try-cleanup-after-failed-compile first-entry)
                      t))
                (error
                 (web-vcs-message-with-face
                  'web-vcs-red "Error in byte compiling %S: %s" el-file (error-message-string err))
                 (web-vcs-display-messages nil)
                 ;; Clean up before restart
                 (web-autoload-try-cleanup-after-failed-compile first-entry)
                 t ;; error
                 )))
        (throw 'web-autoload-comp-restart t)
        ))))

(defun web-autoload-do-eval-requires (el-file)
  "Do eval-when-compile and eval-and-compile."
  ;;(message "web-autoload-do-eval-requires %S" el-file)
  (let ((old-buf (find-buffer-visiting el-file)))
    (with-current-buffer (or old-buf (find-file-noselect el-file))
      (let ((here (point))
            (web-autoload-require-skip-noerror-entries t))
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;;(message "web-autoload-do-eval-requires cb=%s" (current-buffer))
          (while (progn
                   (while (progn (skip-chars-forward " \t\n\^l")
                                 (looking-at ";"))
                     (forward-line 1))
                   (not (eobp)))
            (let ((form (read (current-buffer))))
              (when (memq (car form) '(eval-when-compile eval-and-compile))
                (web-vcs-message-with-face 'web-vcs-gold "  eval %S" form)
                (eval form))
              )))
        (if old-buf (kill-buffer) (goto-char here))))))


;; Fix-me: protect against deep nesting
(defun web-autoload-do-require (feature filename noerror)
  (let* ((feat-name (symbol-name feature))
         (lib (or filename feat-name)))
    (if (load lib noerror t)
        (progn
          (unless (featurep feature)
            (error "web-autoload: Required feature `%s' was not provided" feature))
          feature)
      nil
      )))

(defvar web-autoload-require-skip-noerror-entries nil)

(defadvice require (around
                    web-autoload-ad-require)
  (let ((feature  (ad-get-arg 0))
        (filename (ad-get-arg 1))
        (noerror  (ad-get-arg 2)))
    (if (featurep feature)
        feature
      (if (or filename
              (and noerror
                   (or (not (boundp 'web-autoload-skip-require-advice))
                       web-autoload-skip-require-advice)))
          (progn
            (message "Doing nearly original require %s, because skipping" (ad-get-arg 0))
            ;; Can't ad-do-it because defadviced functions in load
            ;;(web-autoload-do-require feature filename noerror)
            ;;
            ;; Fix-me: Implement lazy loading here? Could it be done with while-no-input?
            ;;
            ;;(when (assq feature web-autoload-require-list) )
            ad-do-it)
        (unless (and noerror
                     web-autoload-require-skip-noerror-entries)
          (let* ((auto-rec (assq feature web-autoload-require-list))
                 (web-vcs      (nth 1 auto-rec))
                 (base-url     (nth 2 auto-rec))
                 (relative-url (nth 3 auto-rec))
                 (base-dir     (nth 4 auto-rec))
                 (comp-fun     (nth 5 auto-rec)))
            (if (not auto-rec)
                ad-do-it
              (let* ((full-el      (concat (expand-file-name relative-url base-dir) ".el"))
                     (full-elc     (byte-compile-dest-file full-el))
                     (our-buffer   (current-buffer)) ;; Need to come back here
                     (our-wcfg     (current-window-configuration))
                     (mode-line-old          (web-vcs-redefine-face 'mode-line 'web-vcs-mode-line))
                     (mode-line-inactive-old (web-vcs-redefine-face 'mode-line-inactive 'web-vcs-mode-line-inactive))
                     (header-line-format-old (with-current-buffer "*Messages*"
                                               (prog1
                                                   header-line-format
                                                 (setq header-line-format
                                                       (propertize "Downloading needed files..."
                                                                   'face 'web-vcs-mode-line
                                                                   ;;'face '(:height 1.5) ;; does not work
                                                                   ))))))
                ;; Fix-me: can't update while accessing the menus
                ;;(message "trying (redisplay t) ;; mode line")
                ;;(sit-for 1) (redisplay t) ;; mode line
                (unwind-protect
                    (progn
                      (web-vcs-message-with-face 'web-vcs-gold "Doing the really adviced require for %s" feature)
                      ;; Check if already downloaded first
                      (unless (file-exists-p full-el)
                        (setq base-url (eval base-url))
                        ;; Download and try again
                        (setq relative-url (concat relative-url ".el"))
                        (web-vcs-message-with-face 'web-vcs-green "Need to download feature '%s" feature)
                        (catch 'web-autoload-comp-restart
                          (web-vcs-get-missing-matching-files web-vcs base-url base-dir relative-url)))
                      (set-buffer our-buffer) ;; Before we load..
                      (when web-autoload-autocompile
                        (unless (file-exists-p full-elc)
                          ;; Byte compile the downloaded file
                          (web-autoload-byte-compile-file full-el t comp-fun)))
                      (web-vcs-message-with-face 'web-vcs-gold "Doing finally require for %s" feature)
                      (set-buffer our-buffer) ;; ... and after we load
                      (set-window-configuration our-wcfg))
                  (with-current-buffer "*Messages*" (setq header-line-format header-line-format-old))
                  (web-vcs-redefine-face 'mode-line mode-line-old)
                  (web-vcs-redefine-face 'mode-line-inactive mode-line-inactive-old)))
              ad-do-it)))))))

;; (setq x (web-vcs-redefine-face 'mode-line (setq z (face-all-attributes 'web-vcs-mode-line (selected-frame)))))
;; (setq x (web-vcs-redefine-face 'mode-line 'web-vcs-mode-line))
;; (setq y (web-vcs-redefine-face 'mode-line x))
;; (describe-face 'web-vcs-mode-line)
(defun web-vcs-redefine-face (face as-new)
  "Redefine FACE to use the attributes in AS-NEW.
AS-NEW may be either a face or a list returned by `face-all-attributes'.
Return an alist with old attributes."
  (let ((ret (face-all-attributes face (selected-frame)))
        (new-face-att (if (facep as-new)
                          (face-all-attributes as-new (selected-frame))
                        as-new))
        new-at-prop-list
        )
    (dolist (at new-face-att)
      (let ((sym (car at))
            (val (cdr at)))
        (unless (eq val 'unspecified)
          (setq new-at-prop-list (cons sym
                                       (cons val
                                             new-at-prop-list)))
          ;;(message "new=%S" new-at-prop-list)
          )))
    (apply 'set-face-attribute face (selected-frame) new-at-prop-list)
    ret
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Autoload Define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

;;(web-vcs-file-name-as-list "/a/b/c.el")
;;(web-vcs-file-name-as-list "a/b/c.el")
;;(web-vcs-file-name-as-list "c:/a/b/c.el")
;;(web-vcs-file-name-as-list ".*/a/c/")
;;(web-vcs-file-name-as-list "[^/]*/a/c/") ;; Just avoid this.
(defun web-vcs-file-name-as-list (filename)
  "Split file name FILENAME into a list with file names."
  ;; We can't use the primitives since they converts \ to / and
  ;; therefore damages the reg exps.  Just use our knowledge of the
  ;; internal file name representation instead.
  (split-string filename "/"))
;; (let ((lst-name nil)
;;       (head filename)
;;       (old-head ""))
;;   (while (and (not (string= old-head head))
;;               (> (length head) 0))
;;     (let* ((file-head (directory-file-name head))
;;            (tail (file-name-nondirectory (directory-file-name head))))
;;       (setq old-head head)
;;       (setq head (file-name-directory file-head))
;;       ;; For an abs path the final tail is "", use root instead:
;;       (when (= 0 (length tail))
;;         (setq tail head))
;;       (setq lst-name (cons tail lst-name))))
;;   lst-name))

;;(web-vcs-match-folderwise ".*/util/mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/util/mu.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut/mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top/util")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top")
;;(web-vcs-match-folderwise "top/ut../mum.el" "top")
(defun web-vcs-match-folderwise (regex file)
  "Split REGEXP as a file path and match against FILE parts."
  ;;(message "folderwise %S %S" regex file)
  (let ((lst-regex (web-vcs-file-name-as-list regex))
        (lst-file  (web-vcs-file-name-as-list file)))
    (when (>= (length lst-regex) (length lst-file))
      (catch 'match
        (while lst-file
          (let ((head-file  (car lst-file))
                (head-regex (car lst-regex)))
            (unless (or (= 0 (length head-file)) ;; Last /, if present, gives ""
                        (string-match-p (concat "^" head-regex "$") head-file))
              (throw 'match nil)))
          (setq lst-file  (cdr lst-file))
          (setq lst-regex (cdr lst-regex)))
        t))))

(defun web-vcs-contains-file (dir file)
  "Return t if DIR contain FILE."
  (assert (string= dir (file-name-as-directory (expand-file-name dir))) t)
  (assert (or (string= file (file-name-as-directory (expand-file-name file)))
              (string= file (expand-file-name file))) t)
  (let ((dir-len (length dir)))
    (assert (string= "/" (substring dir (1- dir-len))))
    (when (> (length file) dir-len)
      (string= dir (substring file 0 dir-len)))))

(defun web-vcs-nice-elapsed (start-time end-time)
  "Format elapsed time between START-TIME and END-TIME nicely.
Those times should have the same format as time returned by
`current-time'."
  (format-seconds "%h h %m m %z%s s" (float-time (time-subtract end-time start-time))))

;; (web-vcs-equal-files "web-vcs.el" "temp.tmp")
;; (web-vcs-equal-files "../.nosearch" "temp.tmp")
(defun web-vcs-equal-files (file-a file-b)
  "Return t if files FILE-A and FILE-B are equal."
  (let* ((cmd (if (eq system-type 'windows-nt)
                  (list "fc" nil nil nil
                        "/B" "/OFF"
                        (convert-standard-filename file-a)
                        (convert-standard-filename file-b))
                (list diff-command nil nil nil
                      "--binary" "-q" file-a file-b)))
         (ret (apply 'call-process cmd)))
    ;;(message "ret=%s, cmd=%S" ret cmd) (sit-for 2)
    (cond
     ((= 1 ret)
      nil)
     ((= 0 ret)
      t)
     (t
      (error "%S returned %d" cmd ret)))))

(defun web-vcs-display-messages (select)
  "Display *Messages* buffer. Select its window if SELECT."
  (let ((msg-win (display-buffer "*Messages*")))
    (with-selected-window msg-win (goto-char (point-max)))
    (when select (select-window msg-win))
    msg-win))

;; (web-vcs-message-with-face 'secondary-selection "I am saying: %s and %s" "Hi" "Farwell!")
;;;###autoload
(defun web-vcs-message-with-face (face format-string &rest args)
  "Display a colored message at the bottom of the string.
FACE is the face to use for the message.
FORMAT-STRING and ARGS are the same as for `message'.

Also put FACE on the message in *Messages* buffer."
  (with-current-buffer "*Messages*"
    (save-restriction
      (widen)
      (let* ((start (let ((here (point)))
                      (goto-char (point-max))
                      (prog1
                          (copy-marker
                           (if (bolp) (point-max)
                             (1+ (point-max))))
                        (goto-char here))))
             (msg-with-face (propertize (apply 'format format-string args)
                                        'face face)))
        ;; This is for the echo area:
        (message "%s" msg-with-face)
        ;; This is for the buffer:
        (when (< 0 (length msg-with-face))
          (goto-char (1- (point-max)))
          ;;(backward-char)
          ;;(unless (eolp) (goto-char (line-end-position)))
          (put-text-property start (point)
                             'face face))))))

(defun web-vcs-num-moved (root)
  "Return nof files matching *.moved inside directory ROOT."
  (let* ((file-regexp ".*\\.moved$")
         (files (directory-files root t file-regexp))
         (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (web-vcs-rdir-get-files subdir file-regexp) nil))))
    (length files)))

;; Copy of rdir-get-files in ourcomment-util.el
(defun web-vcs-rdir-get-files (root file-regexp)
  (let ((files (directory-files root t file-regexp))
        (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (web-vcs-rdir-get-files subdir file-regexp) nil))))
    files))

(defun web-vcs-contains-moved-files (dl-dir)
  "Return t if there are *.moved files in DL-DIR."
  (let ((num-moved (web-vcs-num-moved dl-dir)))
    (when (> num-moved 0)
      (web-vcs-message-with-face 'font-lock-warning-face
                                 (concat "There are %d *.moved files (probably from prev download)\n"
                                         "in %S.\nPlease delete them first.")
                                 num-moved dl-dir)
      t)))


(defun web-vcs-set&save-option (symbol value)
  (customize-set-variable symbol value)
  (customize-set-value symbol value)
  (when (condition-case nil (custom-file) (error nil))
    (customize-mark-to-save symbol)
    (custom-save-all)
    (message "web-vcs: Saved option %s with value %s" symbol value)))

(defvar web-vcs-el-this (or load-file-name
                            (when (boundp 'bytecomp-filename) bytecomp-filename)
                            buffer-file-name))


(require 'bytecomp)
(defun web-vcs-byte-compile-newer-file (el-file load)
  (let ((elc-file (byte-compile-dest-file el-file)))
    (when (or (not (file-exists-p elc-file))
              (file-newer-than-file-p el-file elc-file))
      (byte-compile-file el-file load))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling

;;;###autoload
(defun web-vcs-byte-compile-file (file &optional load extra-load-path comp-dir)
  "Byte compile FILE in a new Emacs sub process.
EXTRA-LOAD-PATH is added to the front of `load-path' during
compilation.

FILE is set to `buffer-file-name' when called interactively.
If LOAD"
  (interactive (list (buffer-file-name)
                     t))
  (when (with-no-warnings (called-interactively-p))
    (unless (eq major-mode 'emacs-lisp-mode)
      (error "Must be in emacs-lisp-mode")))
  (let* ((old-env-load-path (getenv "EMACSLOADPATH"))
         (sub-env-load-path (or old-env-load-path
                                ;;(mapconcat 'identity load-path ";")))
                                (mapconcat 'identity load-path path-separator)))
         ;; Fix-me: name of compile log buffer. When should it be
         ;; deleted? How do I bind it to byte-compile-file? Or do I?
         (file-buf (find-buffer-visiting file))
         (out-buf (get-buffer-create "*Compile-Log*"))
         (elc-file (byte-compile-dest-file file))
         (this-emacs-exe (locate-file invocation-name
                                      (list invocation-directory)
                                      exec-suffixes))
         (default-directory (or comp-dir default-directory))
         (debug-on-error t)
         start)
    ;; (when (and file-buf
    ;;            (buffer-modified-p file-buf))
    ;;   (switch-to-buffer file-buf)
    ;;   (error "Buffer must be saved first: %S" file-buf))
    (dolist (full-p extra-load-path)
      ;;(setq sub-env-load-path (concat full-p ";" sub-env-load-path)))
      (setq sub-env-load-path (concat full-p path-separator sub-env-load-path)))
    (unless (get-buffer-window out-buf (selected-frame))
      (if (string= file (buffer-file-name))
          (display-buffer out-buf)
        (unless (eq (current-buffer) out-buf)
          (switch-to-buffer out-buf))))
    (with-selected-window (get-buffer-window out-buf)
      (with-current-buffer out-buf
        (unless (local-variable-p 'web-vcs-comp-dir)
          (set (make-local-variable 'web-vcs-comp-dir) (or comp-dir default-directory)))
        (setq default-directory web-vcs-comp-dir)
        (widen)
        (goto-char (point-max))
        (when (or (= 0 (buffer-size))
                  (not (derived-mode-p 'compilation-mode)))
          (insert (propertize "\nWeb VCS compilation output" 'font-lock-face 'font-lock-comment-face))
          (compilation-mode)
          (setq font-lock-verbose nil)
          (font-lock-add-keywords nil
                                  '(("\\<Compile\\>" . 'compilation-info))))
        (let ((inhibit-read-only t)
              (rel-file (file-relative-name file)))
          (insert "\n\n")
          (insert "** Compile " rel-file "\n"))
        (setq start (point))
        (when (file-exists-p elc-file) (delete-file elc-file))
        (if (or (not window-system)
                (< emacs-major-version 23))
            (byte-compile-file file)
          ;;(message "web-vcs-byte-compile-file:sub-env-load-path=%s" sub-env-load-path)
          (unless (file-exists-p this-emacs-exe)
            (error "Can't find this-emacs-exe=%s" this-emacs-exe))
          (unless (stringp sub-env-load-path) (error "I did it again, sub-env-load-path=%S" sub-env-load-path))
          (setenv "EMACSLOADPATH" sub-env-load-path)
          ;; Fix-me: status
          (let* ((inhibit-read-only t)
                 (ret (apply 'call-process this-emacs-exe nil out-buf t
                            "-Q" "--batch"
                            "--eval" "(setq debug-on-error t)"
                            "--eval" "(remove-hook 'find-file-hook 'vc-find-file-hook)"
                            "--file" file
                            "-f" "emacs-lisp-byte-compile"
                             nil)))
            ;;(insert (format "call-process returned: %s\n" ret))
            )
          (setenv "EMACSLOADPATH" old-env-load-path))
        (goto-char start)
        (while (re-search-forward "^\\([a-zA-Z0-9/\._-]+\\):[0-9]+:[0-9]+:" nil t)
          (let ((rel-file (file-relative-name file))
                (inhibit-read-only t))
            (replace-match rel-file nil nil nil 1)))
        (goto-char (point-max))))
    (when (file-exists-p elc-file)
      (when (and load window-system) (load elc-file))
      t)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporary helpers, possibly included in Emacs

;; (setq x (web-vcs-url-retrieve-synch "http://emacswiki.org/"))
(defun web-vcs-url-retrieve-synch (url)
  "Retrieve URL, return cons with buffer and http status."
  (let* ((url-show-status nil) ;; just annoying showing status here
         (buffer (url-retrieve-synchronously url))
         (handle nil)
         (http-status nil))
    (if (not buffer)
	(error "Retrieving url %s gave no buffer" url))
    (with-current-buffer buffer
      (if (= 0 (buffer-size))
          (progn
            (kill-buffer)
            nil)
        (require 'url-http)
        (setq http-status (url-http-parse-response))
        (if (memq http-status '(200 201))
            (progn
              (goto-char (point-min))
              (unless (search-forward "\n\n" nil t)
                (error "Could not find header end in buffer for %s" url))
              (delete-region (point-min) (point))
              (set-buffer-modified-p nil)
              (goto-char (point-min)))
          (kill-buffer buffer)
          (setq buffer nil))))
    (cons buffer http-status)))

;; Modified just to return http status
(defun web-vcs-url-copy-file (url newname &optional ok-if-already-exists
                                  keep-time preserve-uid-gid)
  "Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
Fifth arg PRESERVE-UID-GID is ignored.
A prefix arg makes KEEP-TIME non-nil."
  (if (and (file-exists-p newname)
	   (not ok-if-already-exists))
      (error "Opening output file: File already exists, %s" newname))
  (let ((buffer (url-retrieve-synchronously url))
	(handle nil)
        (ret nil))
    (if (not buffer)
	(error "Retrieving url %s gave no buffer" url))
    (with-current-buffer buffer
      (if (= 0 (buffer-size))
          (progn
            (kill-buffer)
            nil)
        (require 'url-http)
        (setq ret (url-http-parse-response))
        (setq handle (mm-dissect-buffer t))
        (mm-save-part-to-file handle newname)
        (kill-buffer buffer)
        (mm-destroy-parts handle)))
    ret))

(defun web-vcs-read-and-accept-key (prompt accepted &optional reject-message help-function)
  (let ((key nil)
        rejected)
    (while (not (member key accepted))
      (if (and help-function
               (or (member key help-event-list)
                   (eq key ??)))
          (funcall help-function)
        (unless rejected
          (setq rejected t)
          (setq prompt (concat (or reject-message "Please answer with one of the alternatives.")
                               "\n\n"
                               prompt))
          (setq key (web-vcs-read-key prompt)))))
    key))

(defconst web-vcs-read-key-empty-map (make-sparse-keymap))

(defvar web-vcs-read-key-delay 0.01) ;Fast enough for 100Hz repeat rate, hopefully.

(defun web-vcs-read-key (&optional prompt)
  "Read a key from the keyboard.
Contrary to `read-event' this will not return a raw event but instead will
obey the input decoding and translations usually done by `read-key-sequence'.
So escape sequences and keyboard encoding are taken into account.
When there's an ambiguity because the key looks like the prefix of
some sort of escape sequence, the ambiguity is resolved via `web-vcs-read-key-delay'."
  (let ((overriding-terminal-local-map web-vcs-read-key-empty-map)
	(overriding-local-map nil)
	(old-global-map (current-global-map))
        (timer (run-with-idle-timer
                ;; Wait long enough that Emacs has the time to receive and
                ;; process all the raw events associated with the single-key.
                ;; But don't wait too long, or the user may find the delay
                ;; annoying (or keep hitting more keys which may then get
                ;; lost or misinterpreted).
                ;; This is only relevant for keys which Emacs perceives as
                ;; "prefixes", such as C-x (because of the C-x 8 map in
                ;; key-translate-table and the C-x @ map in function-key-map)
                ;; or ESC (because of terminal escape sequences in
                ;; input-decode-map).
                web-vcs-read-key-delay t
                (lambda ()
                  (let ((keys (this-command-keys-vector)))
                    (unless (zerop (length keys))
                      ;; `keys' is non-empty, so the user has hit at least
                      ;; one key; there's no point waiting any longer, even
                      ;; though read-key-sequence thinks we should wait
                      ;; for more input to decide how to interpret the
                      ;; current input.
                      (throw 'read-key keys)))))))
    (unwind-protect
        (progn
	  (use-global-map web-vcs-read-key-empty-map)
          (message (concat (apply 'propertize prompt (member 'face minibuffer-prompt-properties))
                           (propertize " " 'face 'cursor)))
	  (aref	(catch 'read-key (read-key-sequence-vector nil nil t)) 0))
      (cancel-timer timer)
      (use-global-map old-global-map))))

;; End temp helpers
;;;;;;;;;;;;;;;;;;;;;;;;

;;(web-vcs-existing-files-matcher default-directory)
(defun web-vcs-existing-files-matcher (dir)
  (let ((files-and-dirs (directory-files dir nil "[^#~]$"))
        files
        (default-directory dir))
    (dolist (df files-and-dirs)
      (unless (file-directory-p df)
        (setq files (cons df files))))
    (cons (regexp-opt files) t)))

(defun web-vcs-update-existing-files (vcs base-url dl-dir this-dir)
  (let ((files-and-dirs (directory-files this-dir nil "\\(?:\\.elc\\|\\.moved\\|[^#~]\\)$"))
        files
        dirs
        (this-rel (file-relative-name this-dir dl-dir))
        file-mask)
    (when (string= "./" this-rel) (setq this-rel ""))
    (dolist (df files-and-dirs)
      (if (and (file-directory-p df)
               (not (member df '("." ".."))))
          (setq dirs (cons df dirs))
        (setq files (cons df files))))
    ;;(web-vcs-message-with-face 'hi-blue "this-rel=%S  %S %S" this-rel  dl-dir this-dir)
    (setq file-mask (concat this-rel (regexp-opt files)))
    ;;(web-vcs-message-with-face 'hi-blue "r=%S" file-mask)
    (web-vcs-get-missing-matching-files vcs base-url dl-dir file-mask)
    (dolist (d dirs)
      (web-vcs-update-existing-files vcs base-url dl-dir
                                       (file-name-as-directory
                                        (expand-file-name d this-dir))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some small bits for security and just overview.

(defun web-vcs-fontify-as-ps-print()
  (save-restriction
    (widen)
    (let ((start (point-min))
          (end   (point-max)))
      (cond ((and (boundp 'jit-lock-mode) (symbol-value 'jit-lock-mode))
             (jit-lock-fontify-now start end))
            ((and (boundp 'lazy-lock-mode) (symbol-value 'lazy-lock-mode))
             (lazy-lock-fontify-region start end))))))


;;(web-vcs-get-fun-details 'describe-function)
;;(web-vcs-get-fun-details 'require)
;;(describe-function 'describe-function)
(defun web-vcs-get-fun-details (function)
  (unless (symbolp function) (error "Not a symbol: %s" function))
  (unless (functionp function) (error "Not a function: %s" function))
  ;; Do as in `describe-function':
  (let* ((advised (and (symbolp function) (featurep 'advice)
		       (ad-get-advice-info function)))
	 ;; If the function is advised, use the symbol that has the
	 ;; real definition, if that symbol is already set up.
	 (real-function
	  (or (and advised
		   (let ((origname (cdr (assq 'origname advised))))
		     (and (fboundp origname) origname)))
	      function))
	 ;; Get the real definition.
	 (def (if (symbolp real-function)
                      (symbol-function real-function)
                    function))
         errtype file-name (beg "") string)
    ;; Just keep this as it is to more easily compare with `describe-function-1'.
    (setq string
	  (cond ((or (stringp def)
		     (vectorp def))
		 "a keyboard macro")
		((subrp def)
		 (if (eq 'unevalled (cdr (subr-arity def)))
		     (concat beg "special form")
		   (concat beg "built-in function")))
		((byte-code-function-p def)
		 (concat beg "compiled Lisp function"))
		((symbolp def)
		 (while (and (fboundp def)
			     (symbolp (symbol-function def)))
		   (setq def (symbol-function def)))
		 ;; Handle (defalias 'foo 'bar), where bar is undefined.
		 (or (fboundp def) (setq errtype 'alias))
		 (format "an alias for `%s'" def))
		((eq (car-safe def) 'lambda)
		 (concat beg "Lisp function"))
		((eq (car-safe def) 'macro)
		 "a Lisp macro")
		((eq (car-safe def) 'autoload)
 		 ;;(setq file-name-auto (nth 1 def))
 		 ;;(setq file-name-auto (find-lisp-object-file-name function def))
                 ;;(setq file-auto-noext (file-name-sans-extension file-name-auto))
		 (format "%s autoloaded %s"
			 (if (commandp def) "an interactive" "an")
			 (if (eq (nth 4 def) 'keymap) "keymap"
			   (if (nth 4 def) "Lisp macro" "Lisp function"))))
                ((keymapp def)
                 (let ((is-full nil)
                       (elts (cdr-safe def)))
                   (while elts
                     (if (char-table-p (car-safe elts))
                         (setq is-full t
                               elts nil))
                     (setq elts (cdr-safe elts)))
                   (if is-full
                       "a full keymap"
                     "a sparse keymap")))
		(t "")))
    (setq file-name (find-lisp-object-file-name function def))
    (list errtype advised file-name string)
    ))

;;(web-vcs-investigate-read "c:/emacsw32/nxhtml/nxhtml/nxhtml-autoload.el" "*Messages*")
(defun web-vcs-investigate-read (elisp out-buf)
  "Check forms in buffer by reading it."
  (let* ((here (point))
        unsafe-eval re-fun re-var
        elisp-el-file
        (is-same-file (lambda (file)
                        (when file
                          (setq file (concat (file-name-sans-extension file) ".el"))
                          (string= (file-truename file) elisp-el-file)))))
    (with-current-buffer elisp
      (setq elisp-el-file (when (buffer-file-name)
                            (file-truename (buffer-file-name))))
      (save-restriction
        (widen)
        (web-vcs-fontify-as-ps-print)
        (goto-char (point-min))
        (while (progn
                 (while (progn (skip-chars-forward " \t\n\^l")
                               (looking-at ";"))
                   (forward-line 1))
                 (not (eobp)))
          (let* ((pos (point))
                 (form (read (current-buffer)))
                 (def (nth 0 form))
                 (sym (and (listp form)
                           (symbolp (nth 1 form))
                           (nth 1 form)))
                 (form-fun (and sym
                                (functionp sym)
                                (symbol-function sym)))
                 (form-var (boundp sym))
                 (safe-forms '( defun defmacro
                                define-minor-mode define-globalized-minor-mode
                                defvar defconst
                                defcustom
                                defface defgroup
                                ;; fix-me: check if these do re-fun too:
                                define-derived-mode
                                define-global-minor-mode
                                define-globalized-minor-mode

                                make-local-variable make-variable-buffer-local
                                provide
                                require
                                message))
                 (safe-eval (or (memq def safe-forms)
                                (and (memq def '( eval-when-compile eval-and-compile))
                                     (or (not (consp (nth 1 form)))
                                         (memq (car (nth 1 form)) safe-forms)))))
                 )
            (cond
             ((not safe-eval)
              (setq unsafe-eval
                    (cons (list form (copy-marker pos) (buffer-substring pos (point)))
                          unsafe-eval)))
             ((and form-fun
                   (memq def '( defun defmacro define-minor-mode define-globalized-minor-mode)))
              (setq re-fun (cons (cons sym pos) re-fun)))
             ((and form-var
                   (memq def '( defvar defconst defcustom))
                   (or (not (eq sym 'defvar))
                       (< 2 (length form))))
              (setq re-var (cons sym re-var)))))))
      (goto-char here))
    (with-current-buffer out-buf
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bobp) (insert "\n\n"))
        (insert (propertize "Found these possible problems when reading the file:\n"
                            'face '(:height 1.5)))
        (or unsafe-eval
            re-fun
            (insert "\n"
                    "Found no problems (but there may still be)"
                    "\n"))

        ;; Fix-me: Link
        (when unsafe-eval
          (insert (propertize
                   (format "\n* Forms that are executed when loading the file (found %s):\n\n"
                          (length unsafe-eval))
                   'face '(:height 1.2)))
          (dolist (u unsafe-eval)
            (insert-text-button "Go to form below"
                                'action
                                `(lambda (button)
                                   (let* ((marker ,(nth 1 u))
                                          (buf (marker-buffer marker)))
                                     (switch-to-buffer-other-window buf)
                                     (unless (and (< marker (point-max))
                                                  (> marker (point-min)))
                                       (widen))
                                     (goto-char marker))))
            (insert "\n")
            (insert (nth 2 u) "\n\n"))
          (insert "\n"))
        (when re-fun
          (insert (propertize
                   (format "\n* The file will possibly redefine these functions that are currently defined (%s):\n"
                          (length re-fun))
                   'face '(:height 1.2)))
          (setq re-fun (sort re-fun (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
          (let ((row 0)
                (re-fun-with-info (mapcar (lambda (fun)
                                            (cons fun (web-vcs-get-fun-details (car fun))))
                                            re-fun))
                re-fun-other-files
                (n-same 0)
                (n-web-auto 0))
            ;; Check same file
            (dolist (info re-fun-with-info)
              (let* ((file-name (nth 3 info))
                     (fun (car (nth 0 info)))
                     (web-auto (get fun 'web-autoload)))
                (cond ((funcall is-same-file file-name)
                       (setq n-same (1+ n-same)))
                      (web-auto
                       (setq n-web-auto (1+ n-web-auto))
                       (setq re-fun-other-files (cons info re-fun-other-files)))
                      (t
                       (setq re-fun-other-files (cons info re-fun-other-files))))))

            (when (< 0 n-same)
              (insert "\n  "
                      (propertize (format "%s functions alreay defined by this file (which seems ok)" n-same)
                                  'face 'web-vcs-green)
                      "\n"))

            (dolist (info re-fun-other-files)
              (let* ((fun-rec   (nth 0 info))
                     (errtype   (nth 1 info))
                     (advised   (nth 2 info))
                     (file-name (nth 3 info))
                     (string    (nth 4 info))
                     (fun     (car fun-rec))
                     (fun-pos (cdr fun-rec))
                     (fun-web-auto (get fun 'web-autoload))
                     )
                (when (= 0 (% row 5)) (insert "\n"))
                (setq row (1+ row))
                (insert "  `")
                (insert-text-button (format "%s" fun)
                                    'action
                                    `(lambda (button)
                                       (describe-function ',fun)))
                (insert "'")
                (insert " (" string)
                (when fun-web-auto
                  (insert " autoloaded from web, ")
                  (insert-text-button "info"
                                      'action
                                      `(lambda (button)
                                         ;; Fix-me: maybe a bit more informative ... ;-)
                                         (message "%S" ',fun-web-auto))))
                (insert ")")
                (when advised (insert ", " (propertize "adviced" 'face 'font-lock-warning-face)))
                (insert ", "
                        (cond
                         ((funcall is-same-file file-name)
                          (propertize "defined in this file" 'face 'web-vcs-green)
                          )
                         (fun-web-auto
                          (if (not (web-autoload-acvtive))
                              (propertize "web download not active" 'face 'web-vcs-yellow)
                            ;; See if file matches
                            (let ((active-sub-url web-autoload-active-file-sub-url)
                                  (fun-sub-url (nth 2 fun-web-auto)))
                              (setq active-sub-url (file-name-sans-extension active-sub-url))
                              (if (string-match-p fun-sub-url active-sub-url)
                                  (propertize "web download, matches" 'face 'web-vcs-yellow)
                                (propertize "web download, doesn't matches" 'face 'web-vcs-red)
                                ))))
                         (t
                          (propertize "defined in other file" 'face 'web-vcs-red))))
                (unless (funcall is-same-file file-name)
                  (insert " (")
                  (insert-text-button "go to new definition"
                                      'action
                                      `(lambda (button)
                                         (interactive)
                                         (let ((m-pos ,(with-current-buffer elisp
                                                         (copy-marker fun-pos))))
                                           (switch-to-buffer-other-window (marker-buffer m-pos))
                                           (goto-char m-pos))))
                  (insert ")"))
                (insert "\n")
                ))))))))

;; I am quite tired of doing this over and over again. Why is this not
;; in Emacs?
(defvar web-vcs-button-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'forward-button)
    (define-key map [(shift tab)] 'backward-button)
    map))
(define-minor-mode web-vcs-button-mode
  "Just to bind `forward-button' etc"
  :lighter nil)

(defvar web-vcs-eval-output-start nil)

;;(web-vcs-investigate-file)
;;;###autoload
(defun web-vcs-investigate-elisp-file (file-or-buffer)
  (interactive (list
                (if (derived-mode-p 'emacs-lisp-mode)
                    (current-buffer)
                  (read-file-name "Elisp file to check: "))))
  (let* ((elisp (if (bufferp file-or-buffer)
                    file-or-buffer
                  (find-file-noselect file-or-buffer)))
         (elisp-file (with-current-buffer elisp (buffer-file-name)))
         (out-buf (get-buffer-create "Web VCS Sec Inv")))
    (if (not (with-current-buffer elisp (derived-mode-p 'emacs-lisp-mode)))
        (progn
          (unless (eq (current-buffer) elisp)
            (display-buffer elisp))
          (message "Buffer %s is not in emacs-lisp-mode" (buffer-name elisp)))
      (switch-to-buffer-other-window out-buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq buffer-read-only t)
        (web-vcs-button-mode 1)
        (insert "A quick look for problems in ")
        (if elisp-file
            (progn
              (insert "file\n    ")
              (insert-text-button elisp-file
                                  'action
                                  `(lambda (button)
                                     (interactive)
                                     (find-file-other-window ,elisp-file))))
          (insert "buffer ")
          (insert-text-button (buffer-name elisp)
                              'action
                              `(lambda (button)
                                 (interactive)
                                 (switch-to-buffer-other-window ,elisp))))
        (insert "\n")
        (let ((here (point)))
          (insert
           "\n"
           (propertize
            (concat "Note that this is just a quick look at the file."
                    " You have to investigate the file more carefully yourself"
                    " (or be sure someone else has done it for you)."
                    " The following are checked for here:"
                    "\n")
            'face font-lock-comment-face))
          (fill-region here (point)))
        (insert
         (propertize
          (concat
           "- Top level forms that might be executed when loading the file.\n"
           "- Redefinition of functions.\n")
          'face font-lock-comment-face))
        (web-vcs-investigate-read elisp out-buf)
        (when elisp-file
          (insert "\n\n\n")
          (let ((here (point)))
            (insert "If you want to see what will actually be added to `load-history'"
                    " and which functions will be defined you can\n")
            (insert-text-button "click here to try to eval the file"
                                'action `(lambda (button) (interactive)
                                           (if (y-or-n-p "Load the file in a batch Emacs session? ")
                                               (web-vcs-investigate-eval ,elisp-file ,out-buf)
                                             (message "Aborted"))))
            (insert ".\n\nThis will load the file in a batch Emacs"
                    " which runs the same init files as you have run now"
                    (cond
                     ((not init-file-user) " (with -Q, ie no init files will run)")
                     ((not site-run-file) " (with -q, ie .emacs will not furn)")
                     (t " (your normal setup files will be run)"
                      ))
                    " and send back that information."
                    " The variable `load-path' is set to match the downloading"
                    " to make the loading possible before your setup is ready."
                    "\n\nYour current Emacs will not be affected by the loading,"
                    " but please be aware that this does not mean your computer can not be."
                    " So please look at the file first.")
            (fill-region here (point))
            (setq web-vcs-eval-output-start (point))
            ))
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

(make-variable-buffer-local 'web-vcs-eval-output-start)

;;(web-vcs-investigate-eval "c:/emacsw32/nxhtml/nxhtml/nxhtml-autoload.el" "*Messages*")
;;(web-vcs-investigate-eval "c:/emacsw32/nxhtml/autostart.el" "*Messages*")
(defun web-vcs-investigate-eval (elisp-file out-buf)
  "Get compile loads when evaling buffer.
For security reasons do this in a fresh Emacs and return the
resulting load-history entry."
  (let* ((emacs-exe (locate-file invocation-name
                                 (list invocation-directory)
                                 exec-suffixes))
         ;; see custom-load-symbol
         (get-lhe '(let ((lhe (or (assoc buffer-file-name load-history)
                                  (assoc (concat (file-name-sans-extension buffer-file-name) ".elc")
                                         load-history))))
                     (prin1 "STARTHERE\n")
                     (prin1 lhe)))
         (elisp-file-name (file-name-sans-extension (file-name-nondirectory elisp-file)))
         (elisp-el-file (file-truename (concat (file-name-sans-extension elisp-file) ".el")))
         (temp-prefix web-autoload-temp-file-prefix)
         (temp-prefix-len (length temp-prefix))
         (is-downloading (and (boundp 'web-autoload-paranoid)
                              web-autoload-paranoid))
         (is-temp-file (and is-downloading
                            (< (length temp-prefix) (length elisp-file-name))
                            (string= temp-prefix
                                     (substring elisp-file-name 0 temp-prefix-len))))
         (elisp-feature-name (if is-temp-file
                                 (substring elisp-file-name temp-prefix-len)
                               elisp-file-name))
         (is-same-file (lambda (file)
                         (when file ;; self protecting
                           (setq file (concat (file-name-sans-extension file) ".el"))
                           (string= (file-truename file) elisp-el-file))))
         (active-sub-url (when (web-autoload-acvtive)
                           (file-name-sans-extension web-autoload-active-file-sub-url)))
         whole-result
         batch-error
         result)
    (with-current-buffer out-buf
      (let ((here (point))
            (inhibit-read-only t))
        (save-restriction
          (widen)
          (goto-char (point-max))
          (delete-region web-vcs-eval-output-start (point)))
        (goto-char here)))
    ;; Fix-me: do not use temp buffer so we can check errors
    (with-temp-buffer
      (let ((old-loadpath (getenv "EMACSLOADPATH"))
            ;;(new-loadpath (mapconcat 'identity load-path ";"))
            (new-loadpath (mapconcat 'identity load-path path-separator))
            ret-val)
        (setenv new-loadpath)
        (message "Loading file in batch Emacs...")
        (setq ret-val
              (call-process emacs-exe nil
                            (current-buffer)
                            t "--batch"
                            ;; fix-me: "-Q" - should be run in the users current environment.
                            ;; init-file-user nil => -Q
                            ;; site-run-file nil => -q
                            (cond
                             ((not init-file-user) "-Q")
                             ((not site-run-file) "-q")
                             (t "--debug-init")) ;; have to have something here...
                            "-l" elisp-file
                            elisp-file
                            "-eval" (format "%S" get-lhe)))
        (message "Loading file in batch Emacs... done, returned %S" ret-val)
        (setenv old-loadpath))
      ;; Fix-me: how do you check the exit status on different platforms?
      (setq whole-result (buffer-substring-no-properties (point-min) (point-max)))
      (condition-case err
          (progn
            (goto-char (point-min))
            (search-forward "STARTHERE")
            (search-forward "(")
            (backward-char)
            (setq result (read (current-buffer))))
        (error (message "")
               ;; Process should probably have failed if we are here,
               ;; but anyway... ;-)
               (setq batch-error
                     (concat "Sorry, batch Emacs failed. It returned this message:\n\n"
                             whole-result
                             (if is-downloading
                                 (concat
                                  "\n--------\n"
                                  "The error may depend on that not all needed files are yet downloaded.\n")
                               "\n")))
               )))
    (with-current-buffer out-buf
      (let ((here (point))
            (inhibit-read-only t))
        (save-restriction
          (widen)
          (goto-char (point-max))
          (if batch-error
              (progn
                (insert "\n\n")
                (insert (propertize batch-error 'face 'web-vcs-red)))
          (insert (propertize "\n\nThis file added the following to `load-history':\n\n"
                              'face '(:height 1.5)))
          (insert "   (\"" (car result) "\"\n")
          (dolist (e (cdr result))
            (insert (format "    %S" e))
            (cond ((stringp e)) ;; Should not happen...
                  ;; Variables
                  ((symbolp e)
                   (insert "  - ")
                   (insert (if (not (boundp e))
                               (propertize "New" 'face 'web-vcs-yellow)
                             (let ((e-file (symbol-file e)))
                               (if (funcall is-same-file e-file)
                                   (propertize "Same file now" 'face 'web-vcs-green)
                                 (let* ((fun-web-auto (get e 'web-autoload))
                                        (fun-sub-url (nth 2 fun-web-auto)))
                                   (if (and fun-sub-url
                                            (string= fun-sub-url active-sub-url))
                                       (propertize "Web download, matches current download"
                                                   'face 'web-vcs-yellow)
                                     (propertize (format "Loaded from %S now" e-file)
                                                 'face 'web-vcs-red))))))))
                  ;; provide
                  ((eq (car e) 'provide)
                   (insert "  - ")
                   (let* ((feat (car e))
                          (feat-name (symbol-name feat)))
                     (insert (cond
                              ((not (featurep feat))
                               (if (or (string= elisp-feature-name
                                                (symbol-name (cdr e))))
                                   (propertize "Web download, matches file name" 'face 'web-vcs-green)
                                 (propertize "Does not match file name" 'face 'web-vcs-red)))
                              (t
                               ;; symbol-file will be where it is loaded
                               ;; so check load-path instead.
                               (let ((file (locate-library feat-name)))
                                 (if (funcall is-same-file file)
                                     (propertize "Probably loaded from same file now" 'face 'web-vcs-yellow)
                                   (propertize (format "Probably loaded from %S now" file)
                                               'face 'web-vcs-yellow))))))))
                  ;; require
                  ((eq (car e) 'require)
                   (if (featurep (cdr e))
                       (insert "  - " (propertize "Loaded now" 'face 'web-vcs-green))
                     (insert "  - " (propertize "Not loaded now" 'face 'web-vcs-yellow))))
                  ;; Functions
                  ((memq (car e) '( defun macro))
                   (insert "  - ")
                   (let ((fun (cdr e)))
                     (insert (if (functionp fun)
                                 (let ((e-file (symbol-file e)))
                                   (if (funcall is-same-file e-file)
                                       (propertize "Same file now" 'face 'web-vcs-green)
                                     (let* ((fun-web-auto (get fun 'web-autoload))
                                            (fun-sub-url (nth 2 fun-web-auto)))
                                       ;; Fix-me: check for temp download file.
                                       (if (string= fun-sub-url active-sub-url)
                                           (propertize "Web download, matches current download"
                                                       'face 'web-vcs-yellow)
                                         (propertize (format "Loaded from %S now" e-file)
                                                     'face 'web-vcs-yellow)))))
                               ;; Note that web autoloaded functions are already defined.
                               (propertize "New" 'face 'web-vcs-yellow))))))
            (insert "\n"))
          (insert "    )\n")
          (goto-char here))))
      (set-buffer-modified-p nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific for nXhtml

(defvar nxhtml-web-vcs-base-url "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/")

;; Fix-me: make gen for 'lp etc
(defun nxhtml-download-root-url (revision)
  (let* ((base-url nxhtml-web-vcs-base-url)
         (files-url (concat base-url "files/"))
         (rev-part (if revision (number-to-string revision) "head%3A/")))
    (concat files-url rev-part)))

(defun web-vcs-nxhtml ()
  "Install nXhtml.
Download and install nXhtml."
  (interactive)
  (catch 'command-level
    (setq debug-on-error t)
    (let* ((this-dir (file-name-directory web-vcs-el-this))
           (root-url (nxhtml-download-root-url nil))
           ;;(files '("nxhtml-web-vcs.el" "nxhtml-base.el"))
           (files '("nxhtml-web-vcs.el"))
           (files2 (mapcar (lambda (file)
                             (cons file (expand-file-name file this-dir)))
                           files))
           need-dl)
      (dolist (file files2)
        (unless (file-exists-p (cdr file))
          (setq need-dl t)))
      (when need-dl
        (let ((prompt
               (concat "Welcome to install nXhtml."
                       "\nFirst the nXhtml specific web install file must be downloaded."
                       "\nYou will get a chance to review it before it is used."
                       "\n\nDo you want to continue? ")))
          (unless (y-or-n-p prompt)
            (message "Aborted")
            (throw 'command-level nil))))
      (message nil)
      (unless (get-buffer-window "*Messages*")
        (web-vcs-display-messages t)
        (delete-other-windows))
      (dolist (file files2)
        (unless (file-exists-p (cdr file))
          (web-vcs-get-missing-matching-files 'lp root-url this-dir (car file))))
      (load (cdr (car files2))))
    (call-interactively 'nxhtml-setup-install)))


(provide 'web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs.el ends here

;;; ediff-url.el --- Diffing buffer against downloaded url
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sat Nov 24 2007
;; Version: 0.56
;; Last-Updated: 2010-03-18 Thu
;; URL: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/ediff-url.el
;;
;; Features that might be required by this library:
;;
  ;; `mail-prsvr', `mm-util', `timer', `url-parse', `url-util',
  ;; `url-vars'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains a simple function, `ediff-url', to help you
;; update a single file from the web.
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

(require 'url-util)
(eval-when-compile (require 'cl))

(defvar ediff-url-read-url-history nil)

(defun ediff-url-redir-launchpad (url)
  "Check if bazaar list page on Launchpad.
If URL is a description page for a file uploaded to EmacsWiki
suggest to use the download URL instead."
  (let* ((bazaar-url "http://bazaar.launchpad.net/")
         (bazaar-len (length bazaar-url)))
    (if (and (< bazaar-len (length url))
             (string= bazaar-url (substring url 0 bazaar-len)))
        (let* ((url-show-status nil) ;; just annoying showing status here
               (buffer (url-retrieve-synchronously url))
               (handle nil)
               (http-status nil)
               ;; Fix-me: better more flexible pattern?
               (dl-patt "<a href=\"\\(.*?\\)\">download file</a>")
               dl-url)
          (unless buffer
            (message "Got empty buffer for %s" url)
            (throw 'command-level nil))
          (with-current-buffer buffer
            (if (= 0 (buffer-size))
                (progn
                  (message "Got empty page for %s" url)
                  (throw 'command-level nil))
              (require 'url-http)
              (setq http-status (url-http-parse-response))
              (if (memq http-status '(200 201))
                  (progn
                    (goto-char (point-min))
                    (unless (search-forward "\n\n" nil t)
                      (error "Could not find header end in buffer for %s" url))
                    (unless (re-search-forward dl-patt nil t)
                      (error "Could not find download link"))
                    (setq dl-url (match-string 1))
                    (set-buffer-modified-p nil)
                    (kill-buffer buffer)
                    dl-url)
                (kill-buffer buffer)
                (setq buffer nil)
                (setq http-status
                      (concat (number-to-string http-status)
                              (case http-status
                                (401 " (unauthorized)")
                                (403 " (forbidden)")
                                (404 " (not found)")
                                (408 " (request timeout)")
                                (410 " (gone)")
                                (500 " (internal server error)")
                                (503 " (service unavailable)")
                                (504 " (gateway timeout)")
                                (530 " (user access denied)")
                                )))
                (message "Got status %s for %s" http-status url)
                (throw 'command-level nil)))))
      url)))

(defun ediff-url-redir-emacswiki-description-page (url)
  "Check if description page on EmacsWiki.
If URL is a description page for a file uploaded to EmacsWiki
suggest to use the download URL instead."
  ;;(let* ((desc-url "http://www.emacswiki.org/emacs/")
  (let* ((emacswiki-url "http://www.emacswiki.org/")
         (emacswiki-len (length emacswiki-url)))
    (if (and (< emacswiki-len (length url))
             (string= emacswiki-url (substring url 0 emacswiki-len))
             (not (string-match-p "/download/" url)))
        (let ((prompt
               (concat "This seem to be the description page on EmacsWiki,"
                       "\n\tdo you want the download url instead? ")))
          (when (y-or-n-p prompt)
            ;;(let ((start (+ 6 (string-match "/wiki/" url))))
            (let ((start (+ 7 (string-match "/emacs/" url))))
              (concat (substring url 0 start)
                                "download/"
                                (substring url start)))))
      ;; Not on the wiki, just return the url:
      url)))

(defcustom ediff-url-redirects '(ediff-url-redir-emacswiki-description-page
                                 ediff-url-redir-launchpad
                                 )
  "List of functions checking url given to `ediff-url'.
Each function should take an URL as argument and return this URL
or a new URL."
  :type '(repeat function)
  :group 'ediff)

;;;###autoload
(defun ediff-url (url)
  "Compare current buffer to a web URL using `ediff-buffers'.
Check URL using `ediff-url-redirects' before fetching the file.

This is for checking downloaded file.  A the file may have a comment
telling the download URL of thise form in the header:

   ;; URL: http://the-server.net/the-path/the-file.el

If not the user is asked for the URL."
  (interactive (let ((url-init (url-get-url-at-point)))
                 (unless url-init
                   (when (eq major-mode 'emacs-lisp-mode)
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward "URL:[ \t]*" nil t)
                         (setq url-init (url-get-url-at-point))))))
                 (list (read-from-minibuffer "Url for download file: "
                                             (cons (or url-init "") 1) ;nil
                                             nil nil
                                             'ediff-url-read-url-history
                                             ;;url-init
                                             ))))
  (catch 'command-level ;; Fix-me: remove and let go to top later
    (unless (> (length url) 0)
      (message "No URL given, aborted by user")
      (throw 'command-level nil))
    ;; Check if URL seems reasonable
    (dolist (fun ediff-url-redirects)
      (setq url (funcall fun url)))
    ;; Fetch URL and run ediff
    (let* ((url-buf-name (concat "URL=" url))
           (url-buf (get-buffer url-buf-name)))
      (when url-buf
        (unless (y-or-n-p "Use previously downloaded url? ")
          (kill-buffer url-buf)
          (setq url-buf nil)))
      (unless url-buf
        (setq url-buf (get-buffer-create url-buf-name))
        (let ((current-major major-mode))
          (with-current-buffer url-buf
            (url-insert-file-contents url)
            ;; Assume same modes:
            (funcall current-major))))
      (ediff-buffers url-buf (current-buffer)))))

(provide 'ediff-url)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff-url.el ends here

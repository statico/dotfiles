;;
;;  Copyright 2011 Yusuke KAWAKAMI, Akihiro ARISAWA
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;
;; evernote-mode home page is at: http://code.google.com/p/emacs-evernote-mode/
;; Author: Yusuke KAWAKAMI, Akihiro ARISAWA
;; Version: 0.41
;; Keywords: tools, emacs, evernote, bookmark

;; This emacs lisp offers the interactive functions to open, edit, and update notes of Evernote.
;; The minor mode Evernote-mode is applied to the buffer editing a note of Evernote.
;;
;; Please copy this file into emacs lisp library directory or place it in
;; a directory (for example "~/lisp") and write $HOME/.emacs like this.
;;
;;      (add-to-list 'load-path "~/lisp")
;;      (require 'evernote-mode)
;;      (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;;      (global-set-key "\C-cec" 'evernote-create-note)
;;      (global-set-key "\C-ceo" 'evernote-open-note)
;;      (global-set-key "\C-ces" 'evernote-search-notes)
;;      (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;      (global-set-key "\C-cew" 'evernote-write-note)
;;      (global-set-key "\C-cep" 'evernote-post-region)
;;      (global-set-key "\C-ceb" 'evernote-browser)
;;
;; There is one hooks, evernotes-mode-hook.
;; The usage of the hook is shown as follows.
;;
;; (setq evernote-mode-hook
;;   '(lambda ()
;;      (...)))

;;; Code

(require 'tree-widget)

(defun enh-bookmark-supported ()
  (or (> emacs-major-version 23)
      (and (= emacs-major-version 23)
           (>= emacs-minor-version 1))))

(when (enh-bookmark-supported)
  (declare-function bookmark-default-handler "bookmark" (bmk-record))
  (declare-function bookmark-get-bookmark-record "bookmark" (bookmark))
  (declare-function bookmark-make-record-default
                    "bookmark" (&optional no-file no-context posn))
  (declare-function bookmark-name-from-full-record
                    "bookmark" (full-record)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro enh-command-with-auth (&rest body)
  `(let (error-code
         (try-func (lambda () ,@body)))
     (setq error-code
           (catch 'error
             (progn
               (funcall try-func)
               t)))
     (cond
      ((eq error-code t)
       t)
      ((or (eq error-code enh-command-error-not-authed)
           (eq error-code enh-command-error-invalid-auth)
           (eq error-code enh-command-error-auth-expired))
       (let ((error-code
              (catch 'error
                (progn
                  (evernote-login)
                  t))))
         (if (eq error-code t)
             (progn
               (let (error-code)
                 (setq error-code
                       (catch 'error
                         (progn
                           (funcall try-func)
                           t)))
                 (unless (eq error-code t)
                   (message enh-command-last-error-message))))
           (message enh-command-last-error-message))))
      (t
       (message enh-command-last-error-message)))))


(defmacro enh-base-create-note-interactive (ask-notebook)
  "Common interactive procecure of creating a note"
  `(progn
     (if (called-interactively-p) (enh-clear-onmem-cache))
     (enh-command-with-auth
      (switch-to-buffer (enh-base-create-note-common "" ,ask-notebook t t nil)))))


(defmacro enh-base-write-note-interactive (ask-notebook)
  "Common interactive procecure of writing a note"
  `(progn
     (if (called-interactively-p) (enh-clear-onmem-cache))
     (enh-command-with-auth
      (enh-base-create-note-common (buffer-name) ,ask-notebook ,nil t t))))


(defmacro enh-base-post-region-interactive (begin end arg ask-notebook)
  "Common interactive procedure of posting a note"
  `(progn
     (if (called-interactively-p) (enh-clear-onmem-cache))
     (enh-command-with-auth
      (save-excursion
        (save-restriction
          (narrow-to-region ,begin ,end)
          (if (and (enutil-neq ,arg nil) (enutil-neq ,arg 1))
              (pop-to-buffer (enh-base-create-note-common (buffer-name) ,ask-notebook t t t))
            (enh-base-create-note-common (buffer-name) ,ask-notebook nil nil t)))))))

(defmacro enutil-neq (&rest exprs)
  `(not (eq ,@exprs)))


(defmacro enutil-nequal (&rest exprs)
  `(not (equal ,@exprs)))


(defmacro enutil-push (elem list)
  `(setq ,list (cons ,elem ,list)))


(defmacro enutil-pop (list)
  `(prog1
       (car ,list)
     (setq ,list (cdr ,list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-username nil
  "*An username of your evernote")

(defvar evernote-enml-formatter-command nil
  "*Formatter for xhtml")

(defvar evernote-ruby-command "ruby"
  "*Path of the ruby command")

(defvar evernote-password-cache nil
  "*Non-nil means that password cache is enabled.
It is recommended to encrypt the file with EasyPG.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-browsing-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar evernote-browsing-page-list nil
  "Buffer list of evernote browsing mode.")

(defvar evernote-browsing-current-page nil
  "Current buffer of evernote browsing mode.")


(defvar evernote-browsing-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    map)
  "Keymap used in evernote browsing mode.")
(define-key evernote-browsing-mode-map "o"    'widget-button-press)
(define-key evernote-browsing-mode-map "n"    'evernote-browsing-open-next-note)
(define-key evernote-browsing-mode-map "p"    'evernote-browsing-open-previous-note)
(define-key evernote-browsing-mode-map "N"    'evernote-browsing-list-notebooks)
(define-key evernote-browsing-mode-map "t"    'evernote-browsing-list-tags)
(define-key evernote-browsing-mode-map "S"    'evernote-browsing-list-searches)
(define-key evernote-browsing-mode-map "s"    'evernote-browsing-search-notes)
(define-key evernote-browsing-mode-map "b"    'evernote-browsing-prev-page)
(define-key evernote-browsing-mode-map "f"    'evernote-browsing-next-page)
(define-key evernote-browsing-mode-map "d"    'evernote-browsing-delete-page)
(define-key evernote-browsing-mode-map "l"    'evernote-browsing-reflesh)
;(define-key evernote-browsing-mode-map "e"    'evernote-browsing-change-edit-mode)
;(define-key evernote-browsing-mode-map "r"    'evernote-browsing-rename-note)
;(define-key evernote-browsing-mode-map "d"    'evernote-browsing-delete-note)


(defun evernote-browsing-mode ()
  "Major mode for browsing notes."
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (setq evernote-browsing-mode t)
  (use-local-map evernote-browsing-mode-map)
  (setq truncate-lines t
        major-mode 'evernote-browsing-mode
        mode-name "Evernote-Browsing")
  (goto-char (point-min)))


(defun evernote-browser ()
  "Open an evernote browser"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-browsing-update-page-list)
  (if evernote-browsing-current-page
      (enutil-move-cursor-to-window evernote-browsing-current-page)
    (evernote-browsing-list-tags)))


(defun evernote-browsing-open-next-note ()
  (interactive)
  (next-line)
  (when (eq enh-browsing-page-type 'note-list)
    (condition-case nil
        (widget-button-press (point))
      (error nil))))


(defun evernote-browsing-open-previous-note ()
  (interactive)
  (previous-line)
  (when (eq enh-browsing-page-type 'note-list)
    (condition-case nil
        (widget-button-press (point))
      (error nil))))


(defun evernote-browsing-list-notebooks ()
  "List tags"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-browsing-update-page-list)
  (let ((page (enh-browsing-get-page-of-type 'notebook-list)))
    (if page
        (progn
          (setq evernote-browsing-current-page page)
          (switch-to-buffer page))
      (enh-browsing-push-page
       (enh-browsing-create-page 'notebook-list "All Notebooks")))))


(defun evernote-browsing-list-tags ()
  "List tags"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-browsing-update-page-list)
  (let ((page (enh-browsing-get-page-of-type 'tag-list)))
    (if page
        (progn
          (setq evernote-browsing-current-page page)
          (switch-to-buffer page))
      (enh-browsing-push-page
       (enh-browsing-create-page 'tag-list "All Tags")))))


(defun evernote-browsing-list-searches ()
  "List saved searches"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-browsing-update-page-list)
  (let ((page (enh-browsing-get-page-of-type 'search-list)))
    (if page
        (progn
          (setq evernote-browsing-current-page page)
          (switch-to-buffer page))
      (enh-browsing-push-page
       (enh-browsing-create-page 'search-list "All Saved Searches")))))


(defun evernote-browsing-search-notes ()
  "Search notes"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (let (note-attrs (query (read-string "Query:")))
    (enh-command-with-auth
     (setq note-attrs
           (enh-command-get-note-attrs-from-query query)))
    (enh-browsing-update-page-list)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (format "Query Result of: %s" query)
                               note-attrs))))


(defun evernote-browsing-prev-page ()
  "Move to the prev page"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when (eq major-mode 'evernote-browsing-mode)
    (enh-browsing-update-page-list)
    (let ((prev-page (enh-browsing-get-prev-page)))
      (if prev-page
          (progn
            (setq evernote-browsing-current-page prev-page)
            (switch-to-buffer prev-page))
        (message "[No more previous page]")))))


(defun evernote-browsing-next-page ()
  "Move to the next page"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when (eq major-mode 'evernote-browsing-mode)
    (enh-browsing-update-page-list)
    (let ((next-page (enh-browsing-get-next-page)))
      (if next-page
          (progn
            (setq evernote-browsing-current-page next-page)
            (switch-to-buffer next-page))
        (message "[No more next page]")))))


(defun evernote-browsing-delete-page ()
  "Delete current page"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when (eq major-mode 'evernote-browsing-mode)
    (kill-buffer (current-buffer))
    (enh-browsing-update-page-list)
    (switch-to-buffer evernote-browsing-current-page)))


(defun evernote-browsing-reflesh ()
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when (eq major-mode 'evernote-browsing-mode)
    (funcall enh-browsing-page-setup-func)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-mode nil
  "Non-nil if Evernote mode is enabled.")
(make-variable-buffer-local 'evernote-mode)

(defvar evernote-note-guid nil
  "Note guid of the buffer")
(make-variable-buffer-local 'evernote-note-guid)

(defvar evernote-note-modified-name nil
  "Modified name of the note before saving")
(make-variable-buffer-local 'evernote-note-modified-name)

(defvar evernote-note-modified-notebook-guid nil
  "Modified noteobok guid of the note before saving")
(make-variable-buffer-local 'evernote-note-modified-notebook-guid)

(defvar evernote-note-modified-edit-mode nil
  "Modified edit-mode of the note before saving")
(make-variable-buffer-local 'evernote-note-modified-edit-mode)

(defvar evernote-note-is-modified-tag-names nil
  "Modified tag-names of the note before saving")
(make-variable-buffer-local 'evernote-note-is-modified-tag-names)

(defvar evernote-note-modified-tag-names nil
  "Modified tag-names of the note before saving")
(make-variable-buffer-local 'evernote-note-modified-tag-names)

(defvar evernote-note-xhtml-mode-content nil
  "Note contents as a string of XHTML")
(make-variable-buffer-local 'evernote-note-xhtml-mode-content)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-mode-display-menu t
  "display the evernote menu on the menubar if this variable is not nil")

(defvar evernote-mode-map (make-sparse-keymap)
  "Keymap used in evernote mode.")
(define-key evernote-mode-map "\C-x\C-s" 'evernote-save-note)
(define-key evernote-mode-map "\C-cen"   'evernote-change-notebook)
(define-key evernote-mode-map "\C-cet"   'evernote-edit-tags)
(define-key evernote-mode-map "\C-cee"   'evernote-change-edit-mode)
(define-key evernote-mode-map "\C-cer"   'evernote-rename-note)
(define-key evernote-mode-map "\C-ced"   'evernote-delete-note)
(define-key evernote-mode-map "\C-x\C-q" 'evernote-toggle-read-only)


(defun enh-menu-is-visible-on-ordinary-mode ()
  (not evernote-browsing-mode))

(defun enh-menu-is-visible-on-evernote-mode ()
  (and evernote-mode (not evernote-browsing-mode)))

(defun enh-menu-is-visible-on-evernote-browsing-mode ()
  evernote-browsing-mode)

(let ((menu-bar-map (make-sparse-keymap "Evernote")))
  (define-key-after global-map [menu-bar evernote]
    `(menu-item "Evernote" ,menu-bar-map
                :visible evernote-mode-display-menu)
    'tools)
  (define-key menu-bar-map [browser]
    '(menu-item "Evernote Browser" evernote-browser
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [seperator-0]
    '(menu-item "--" nil
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [toggle-read-only]
    '(menu-item "Toggle Read Only" evernote-toggle-read-only
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [delete-note]
    '(menu-item "Delete Note" evernote-delete-note
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [rename-note]
    '(menu-item "Rename Note" evernote-rename-note
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [change-notebook]
    '(menu-item "Change Notebook" evernote-change-notebook
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [change-edit-mode]
    '(menu-item "Change Edit Mode" evernote-change-edit-mode
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [edit-tag]
    '(menu-item "Edit Tag" evernote-edit-tags
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [save-note]
    '(menu-item "Save Note" evernote-save-note
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [seperator-1]
    '(menu-item "--" nil
                :visible (enh-menu-is-visible-on-evernote-mode)))
  (define-key menu-bar-map [edit-notebook]
    '(menu-item "Edit Notebook" evernote-edit-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [create-notebook]
    '(menu-item "Create Notebook" evernote-create-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [seperator-2]
    '(menu-item "--" nil
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [edit-search]
    '(menu-item "Edit Saved Search" evernote-edit-search
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [create-search]
    '(menu-item "Create Saved Search" evernote-create-search
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [do-saved-search]
    '(menu-item "Do Saved Search" evernote-do-saved-search
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [search-note]
    '(menu-item "Search Note" evernote-search-notes
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [seperator-3]
    '(menu-item "--" nil
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [post-region-in-notebook]
    '(menu-item "Post Region (w/ notebook param)" evernote-post-region-in-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [post-region]
    '(menu-item "Post Region" evernote-post-region
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [write-note-in-notebook]
    '(menu-item "Write Note (w/ notebook param)" evernote-write-note-in-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [write-note]
    '(menu-item "Write Note" evernote-write-note
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [create-note-in-notebook]
    '(menu-item "Create Note (w/ notebook param)" evernote-create-note-in-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [create-note]
    '(menu-item "Create Note" evernote-create-note
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [open-note-in-notebook]
    '(menu-item "Open Note (w/ notebook param)" evernote-open-note-in-notebook
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [open-note]
    '(menu-item "Open Note" evernote-open-note
                :visible (enh-menu-is-visible-on-ordinary-mode)))
  (define-key menu-bar-map [browsing-prev-page]
    '(menu-item "Prev Page" evernote-browsing-prev-page
                :visible (enh-menu-is-visible-on-evernote-browsing-mode)))
  (define-key menu-bar-map [browsing-next-page]
    '(menu-item "Next Page" evernote-browsing-next-page
                :visible (enh-menu-is-visible-on-evernote-browsing-mode)))
  (define-key menu-bar-map [browsing-search-notes]
    '(menu-item "Searche Notes" evernote-browsing-search-notes
                :visible (enh-menu-is-visible-on-evernote-browsing-mode)))
  (define-key menu-bar-map [browsing-list-searches]
    '(menu-item "List Saved Searches" evernote-browsing-list-searches
                :visible (enh-menu-is-visible-on-evernote-browsing-mode)))
  (define-key menu-bar-map [browsing-list-tags]
    '(menu-item "List Tags" evernote-browsing-list-tags
                :visible (enh-menu-is-visible-on-evernote-browsing-mode)))
  (define-key menu-bar-map [browsing-list-notebooks]
    '(menu-item "List Notebooks" evernote-browsing-list-notebooks
                :visible (enh-menu-is-visible-on-evernote-browsing-mode))))


(defun evernote-mode (&optional guid)
  "Toggle Evernote mode, a minor mode for using evernote functions."
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (or (assq 'evernote-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(evernote-mode " Evernote") minor-mode-alist)))
  (or (assq 'evernote-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'evernote-mode evernote-mode-map) minor-mode-map-alist)))
  (let ((modified (buffer-modified-p)))
    (set-buffer-file-coding-system 'utf-8)
    (set-buffer-modified-p modified))
  (setq evernote-mode (not evernote-mode))
  (if evernote-mode
      (progn
        (when guid (setq evernote-note-guid guid))
        (enh-base-update-mode-line
         evernote-note-modified-notebook-guid
         evernote-note-is-modified-tag-names
         evernote-note-modified-tag-names
         evernote-note-modified-edit-mode)
        (add-hook 'after-save-hook
                  'evernote-mode-after-save-hook
                  nil t)
        (add-hook 'change-major-mode-hook
                  'evernote-mode-change-major-mode-hook
                  nil t)
        (when (enh-bookmark-supported)
          (enh-bookmark-prepare))
        (run-hooks 'evernote-mode-hook))
    (progn
      (setq evernote-note-guid nil)
      (setq vc-mode nil)
      (remove-hook 'after-save-hook
                   'evernote-mode-after-save-hook)
      (remove-hook 'change-major-mode-hook
                   'evernote-mode-change-major-mode-hook))))


(defun evernote-login ()
  "Login"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (unwind-protect
      (let* ((cache (enh-password-cache-load))
             (usernames (mapcar #'car cache))
             (username (or evernote-username
                           (read-string "Evernote user name:"
                                        (car usernames) 'usernames)))
             (cache-passwd (enutil-aget username cache)))
        (unless (and cache-passwd
                     (eq (catch 'error 
                           (progn 
                             (enh-command-login username cache-passwd)
                             t))
                         t))
          (let* ((passwd (read-passwd "Passwd:")))
            (enh-command-login username passwd)
            (setq evernote-username username)
            (enh-password-cache-save (enutil-aset username cache passwd)))))
    (enh-password-cache-close)))


(defun evernote-open-note (&optional ask-notebook)
  "Open a note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-command-with-auth
   (let* ((notebook-guid (and ask-notebook
                              (enutil-aget 'guid (enh-read-notebook))))
          (tag-guids (enh-read-tag-guids
                      "Tags used for search (comma separated form. default search all tags):"))
          (note-attrs
           (enh-command-get-note-attrs-from-notebook-and-tag-guids notebook-guid tag-guids)))
     (enh-base-open-note-common (enh-base-read-note-attr note-attrs))
     (enh-browsing-update-page-list)
     (enh-browsing-push-page
      (enh-browsing-create-page 'note-list
                                (if tag-guids
                                    (format "Notes with tag: %s"
                                            (enh-tag-guids-to-comma-separated-names tag-guids))
                                  "All notes")
                                note-attrs)
      t))))


(defun evernote-open-note-in-notebook ()
  "Open a note in the specified notebook"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (evernote-open-note t))


(defun evernote-search-notes ()
  "Search notes with query and open a note among them"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (let ((query (read-string "Query:")))
    (enh-command-with-auth
     (let ((note-attrs
            (enh-command-get-note-attrs-from-query
             query)))
       (enh-base-open-note-common (enh-base-read-note-attr note-attrs))
       (enh-browsing-update-page-list)
       (enh-browsing-push-page
        (enh-browsing-create-page 'note-list
                                  (format "Query Result of: %s" query)
                                  note-attrs)
        t)))))


(defun evernote-do-saved-search ()
  "Do a saved search and open a note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-command-with-auth
   (let* ((search-attr (enh-read-saved-search))
          (note-attrs
           (enh-command-get-note-attrs-from-query
            (enutil-aget 'query search-attr))))
     (enh-base-open-note-common (enh-base-read-note-attr note-attrs))
     (enh-browsing-update-page-list)
     (enh-browsing-push-page
      (enh-browsing-create-page 'note-list
                                (format "Query Result of Saved Search: %s"
                                        (enutil-aget 'name search-attr))
                                note-attrs)
      t))))


(defun evernote-create-note ()
  "Create a note"
  (interactive)
  (enh-base-create-note-interactive nil))


(defun evernote-create-note-in-notebook ()
  "Create a note in the specified notebook"
  (interactive)
  (enh-base-create-note-interactive t))


(defun evernote-write-note ()
  "Write buffer to a note"
  (interactive)
  (enh-base-write-note-interactive nil))


(defun evernote-write-note-in-notebook ()
  "Write buffer to a note in the specified notebook"
  (interactive)
  (enh-base-write-note-interactive t))


(defun evernote-post-region (begin end arg)
  "Post the region as a note"
  (interactive "r\np")
  (enh-base-post-region-interactive begin end arg nil))


(defun evernote-post-region-in-notebook (begin end arg)
  "Post the region as a note in the specified notebook"
  (interactive "r\np")
  (enh-base-post-region-interactive begin end arg t))


(defun evernote-save-note ()
  "Save a note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (cond
   ((not evernote-mode)
    nil) ; do nothing
   ((not (buffer-modified-p))
    (message "(No changes need to be saved)"))
   (buffer-read-only
    (ding)
    (message "Unset read-only before you save"))
   (t
    (enh-command-with-auth
     (enh-base-update-note-common
      (current-buffer)   ; contents
      evernote-note-guid ; guid
      (if evernote-note-modified-name ; name
          evernote-note-modified-name
        nil)
      evernote-note-modified-notebook-guid ; notebook-guid
      evernote-note-is-modified-tag-names ; is-tag-updated
      (if evernote-note-is-modified-tag-names ; tag-names
          evernote-note-modified-tag-names
        nil)
      (if evernote-note-modified-edit-mode ; edit-mode
          evernote-note-modified-edit-mode
        nil))
     (if (or evernote-note-modified-name
             evernote-note-modified-notebook-guid
             evernote-note-is-modified-tag-names
             evernote-note-modified-edit-mode)
         (enh-browsing-reflesh-page 'note-list))
     (setq evernote-note-modified-name nil
           evernote-note-modified-notebook-guid nil
           evernote-note-is-modified-tag-names nil
           evernote-note-modified-tag-names nil
           evernote-note-modified-edit-mode nil)
     (set-buffer-modified-p nil)))))


(defun evernote-change-notebook ()
  "Change notebook to which this note belongs"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when evernote-mode
    (let* (current-notebook-guid current-notebook-name next-notebook-guid)
      (setq current-notebook-guid
            (or evernote-note-modified-notebook-guid
                (enutil-aget 'notebookGuid (enh-get-note-attr evernote-note-guid))))
      (setq current-notebook-name
            (enutil-aget 'name (enh-get-notebook-attr current-notebook-guid)))
      (setq next-notebook-guid
            (enutil-aget 'guid (enh-read-notebook current-notebook-name)))
      (when (not (string= current-notebook-guid next-notebook-guid))
        (setq evernote-note-modified-notebook-guid next-notebook-guid)
        (enh-base-update-mode-line evernote-note-modified-notebook-guid
                                   evernote-note-is-modified-tag-names
                                   evernote-note-modified-tag-names
                                   evernote-note-modified-edit-mode)
        (set-buffer-modified-p t)))))


(defun evernote-edit-tags ()
  "Add or remove tags from/to the note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when evernote-mode
    (setq evernote-note-modified-tag-names
          (enh-read-tag-names
           "Change attached Tags (comma separated form):"
           evernote-note-guid))
    (setq evernote-note-is-modified-tag-names t) ; this must be after enh-read-tag-names
    (enh-base-update-mode-line evernote-note-modified-notebook-guid
                               evernote-note-is-modified-tag-names
                               evernote-note-modified-tag-names
                               evernote-note-modified-edit-mode)
    (set-buffer-modified-p t)))


(defun evernote-change-edit-mode ()
  "Change edit mode of the note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when evernote-mode
    (let* ((current-edit-mode
            (or evernote-note-modified-edit-mode
                (enutil-aget 'editMode (enh-get-note-attr evernote-note-guid))))
           (next-edit-mode (enh-read-edit-mode current-edit-mode))
           (need-change nil))
      (when (not (string= current-edit-mode next-edit-mode))
        (cond
         ;; XHTML mode, Confirm the buffer is saved.
         ((and (string= current-edit-mode "XHTML")
               (buffer-modified-p))
          (ding)
          (message "Save the buffer before you change edit mode"))
         ;; XHTML mode, Formatted xml.
         ((and (string= current-edit-mode "XHTML")
               buffer-read-only)
          (when (y-or-n-p "Changing text mode will remove all format information. Continue? ")
            (setq evernote-note-xhtml-mode-content nil)
            (setq buffer-read-only nil)
            (setq need-change t)))
         ;; XHTML mode, raw xml.
         ((and (string= current-edit-mode "XHTML")
               (not buffer-read-only))
          (setq evernote-note-xhtml-mode-content nil)
          (setq need-change t))
         ;; HTML mode.
         ((string= current-edit-mode "TEXT")
          (setq buffer-read-only nil)
          (setq need-change t))))
      (when need-change
        (setq evernote-note-modified-edit-mode next-edit-mode)
        (enh-base-update-mode-line
         evernote-note-modified-notebook-guid
         evernote-note-is-modified-tag-names
         evernote-note-modified-tag-names
         evernote-note-modified-edit-mode)
        (set-buffer-modified-p t)))))


(defun evernote-rename-note ()
  "Rename a note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when evernote-mode
    (setq evernote-note-modified-name
          (read-string "New note name:"
                       (enutil-aget 'title (enh-get-note-attr evernote-note-guid))))
    (rename-buffer evernote-note-modified-name t)
    (enh-base-change-major-mode-from-note-name evernote-note-modified-name)
    (set-buffer-modified-p t)))


(defun evernote-delete-note ()
  "Delete a note"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (if (and evernote-mode
           (y-or-n-p "Do you really want to remove this note? "))
      (enh-command-with-auth
       (enh-command-delete-note evernote-note-guid)
       (kill-buffer (current-buffer)))))


(defun evernote-create-notebook ()
  "Create a notebook"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (let ((name (read-string "Notebook Name:")))
    (enh-command-with-auth
     (enh-command-create-notebook name nil))
    (enh-browsing-reflesh-page 'notebook-list)))


(defun evernote-edit-notebook ()
  "Create a notebook"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-command-with-auth
   (let* ((notebook-alist (enh-get-notebook-name-attr-alist))
          (notebook-attr
           (enutil-aget
            (completing-read
             "Notebook:"
             notebook-alist
             nil t)
            notebook-alist)))
     (enh-command-update-notebook
      (enutil-aget 'guid notebook-attr)
      (read-string "New notebook name:"
                   (enutil-aget 'name notebook-attr))
      (yes-or-no-p "Use as the default notebook:"))))
  (clrhash enh-notebook-info)
  (enh-browsing-reflesh-page 'notebook-list))


(defun evernote-create-search ()
  "Create a saved search"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (let ((name (read-string "Saved Search Name:"))
        (query (read-string "Query:")))
    (enh-command-with-auth
     (enh-command-create-search name query))
    (enh-browsing-reflesh-page 'search-list)))


(defun evernote-edit-search ()
  "Create a saved search"
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (enh-command-with-auth
   (let* ((search-alist (enh-get-search-name-attr-alist))
          (search-attr
           (enutil-aget
            (completing-read
             "Saved search:"
             search-alist
             nil t)
            search-alist)))
     (enh-command-update-search
      (enutil-aget 'guid search-attr)
      (read-string "New Saved search name:"
                   (enutil-aget 'name search-attr))
      (read-string "New Query:"
                   (enutil-aget 'query search-attr)))))
  (clrhash enh-search-info)
  (enh-browsing-reflesh-page 'search-list))


(defun evernote-toggle-read-only ()
  (interactive)
  (if (called-interactively-p) (enh-clear-onmem-cache))
  (when evernote-mode
    (if (string= (or evernote-note-modified-edit-mode
                     (enutil-aget 'editMode (enh-get-note-attr evernote-note-guid)))
                 "XHTML")
        (if buffer-read-only
            (progn
              (setq buffer-read-only nil)
              (let ((orig-buffer-modified-p (buffer-modified-p)))
                (erase-buffer)
                (insert evernote-note-xhtml-mode-content)
                (goto-char (point-min))
                (set-buffer-modified-p orig-buffer-modified-p)))
          (if (buffer-modified-p)
              (progn
                (ding)
                (message "Save the buffer before you toggle read only"))
            (setq evernote-note-xhtml-mode-content
                  (buffer-substring (point-min) (point-max)))
            (erase-buffer)
            (enh-format-enml evernote-note-xhtml-mode-content (current-buffer))
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
      (setq buffer-read-only (not buffer-read-only)))
    (force-mode-line-update)))


(defvar evernote-mode-info-for-changing-major-mode nil
  "Temporal values used when changing the major mode")


(defun evernote-mode-after-save-hook ()
  "After save hook for evernote mode. This invalid evernote-mode"
  (if evernote-mode
      (evernote-mode)))


(defun evernote-mode-change-major-mode-hook ()
  "Change major mode hook for evernote mode. This records the note info to the global variable to restore them after changing the major mode"
  (if evernote-mode
      (setq evernote-mode-info-for-changing-major-mode
            (list
             (cons 'guid  evernote-note-guid)
             (cons 'modified-name evernote-note-modified-name)
             (cons 'modified-notebook-guid evernote-note-modified-notebook-guid)
             (cons 'is-modified-tag-names evernote-note-is-modified-tag-names)
             (cons 'modified-tag-names evernote-note-modified-tag-names)
             (cons 'modified-edit-mode evernote-note-modified-edit-mode)
             (cons 'note-xhtml-mode-content evernote-note-xhtml-mode-content)))))


(defun evernote-mode-after-change-major-mode-hook ()
  "After change major mode hook for evernote mode. This restore the note info after changing the major mode"
  (if evernote-mode-info-for-changing-major-mode
      (progn
        (setq evernote-note-modified-name
              (enutil-aget 'modified-name evernote-mode-info-for-changing-major-mode))
        (setq evernote-note-modified-notebook-guid
              (enutil-aget 'modified-notebook-guid evernote-mode-info-for-changing-major-mode))
        (setq evernote-note-is-modified-tag-names
              (enutil-aget 'is-modified-tag-names evernote-mode-info-for-changing-major-mode))
        (setq evernote-note-modified-tag-names
              (enutil-aget 'modified-tag-names evernote-mode-info-for-changing-major-mode))
        (setq evernote-note-modified-edit-mode
              (enutil-aget 'modified-edit-mode evernote-mode-info-for-changing-major-mode))
        (evernote-mode ; this must be after setting evernote-note-modified-xxx
         (enutil-aget 'guid evernote-mode-info-for-changing-major-mode))
        (setq evernote-note-xhtml-mode-content
              (enutil-aget 'note-xhtml-mode-content evernote-mode-info-for-changing-major-mode))
        (setq evernote-mode-info-for-changing-major-mode nil))))


(add-hook 'after-change-major-mode-hook
          'evernote-mode-after-change-major-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for inputing the note name in the minibuffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-read-note-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    map))
(define-key evernote-read-note-map [tab] 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-i" 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-m" 'evernote-read-note-finish)
(define-key evernote-read-note-map  " "   'self-insert-command)


(defun evernote-read-note-completion ()
  "Complete note name and display completion list"
  (interactive)
  (let (word result start)
    (setq word (enutil-get-minibuffer-string))
    (setq result (try-completion word enh-base-displayed-name-attr-alist))
    (cond
     ((eq result t)
      (enutil-minibuffer-tmp-message "[Sole Completion]"))
     ((eq result nil)
      (ding)
      (enutil-minibuffer-tmp-message "[No Match]"))
     ((string= result word)
      (enh-base-display-note-completion-buf
       enh-base-displayed-name-formatted-name-alist
       word))
     (t (enutil-set-minibuffer-string result)
        (end-of-buffer)
        (if (eq t
                (try-completion result
                                enh-base-displayed-name-attr-alist))
            nil
          (enutil-minibuffer-tmp-message "[Complete, but not unique]"))))))


(defun evernote-read-note-finish ()
  "Finish input note name"
  (interactive)
  (if (assoc
       (enutil-get-minibuffer-string)
       enh-base-displayed-name-attr-alist)
      (progn
        (let ((completion-buf (get-buffer "*Evernote-Completions*")))
          (if completion-buf
              (kill-buffer completion-buf)))
        (exit-minibuffer))
    (enutil-minibuffer-tmp-message "[No Match]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-search-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map global-map)
    map)
  "Keymap used in evernote search mode.")
(define-key evernote-search-mode-map "\C-m" 'evernote-select-note-in-search-mode)

(defvar evernote-search-mode-formatted-name-displayed-name-alist nil
  "Alist from formatted names to names used only in evernote-search-mode buffer")
(make-variable-buffer-local 'evernote-search-mode-formatted-name-displayed-name-alist)


(defun evernote-search-mode ()
  "Major mode for selecting a note."
  (interactive)
  (use-local-map evernote-search-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'evernote-search-mode
        mode-name "Evernote-Search")
  (goto-char (point-min)))


(defun evernote-select-note-in-search-mode ()
  "Select a note name on this buffer and input it into the mini buffer"
  (interactive)
  (if (active-minibuffer-window)
      (save-excursion
        (let (displayed-name)
          (setq displayed-name
                (enutil-aget
                 (enutil-get-current-line-string)
                 evernote-search-mode-formatted-name-displayed-name-alist))
          (if displayed-name
              (progn
                (kill-buffer (current-buffer))
                (enutil-set-minibuffer-string displayed-name)
                (exit-minibuffer)))))
    (kill-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anything-c-evernote-title-set-candidates ()
  (let ((buffer (get-buffer enh-command-output-buffer-name))
        candidates)
    (when buffer ; create candidates only when logined.
      (enh-command-with-auth
       (setq candidates
             (enh-command-get-note-attrs-from-query
               ; Put '*' to match the start of a word.
               ; Note: A wildcard is only permitted at the end of the term,
               ;       not at the beginning or middle for scalability reasons on the service
              (format "intitle:\"%s\"*" anything-input))))
      candidates)))


(defun anything-c-evernote-title-candidate-transformer (candidates)
  (mapcar (lambda (cand)
            (cons (enutil-aget 'title cand) cand))
          candidates))


(defun anything-c-evernote-title-action (candidate)
  (enh-base-open-note-common candidate))


(defvar anything-c-source-evernote-title
  '((name . "Evernote Title")
    (candidates . anything-c-evernote-title-set-candidates)
    (candidate-transformer . anything-c-evernote-title-candidate-transformer)
    (action . (("Open" . anything-c-evernote-title-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))


(defun anything-evernote-title ()
  "Preconfigured `anything' for searching notes with the note names."
  (interactive)
  (anything-other-buffer 'anything-c-source-evernote-title "*anything evernote title*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for evernote-mode (enh-base-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst enh-base-enml-template
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\">\n"
   "<en-note>\n"
   "</en-note>\n"))


(defun enh-base-open-note-common (note-attr)
  "Common procedure of opening a note"
  (let* ((note-guid (enutil-aget 'guid note-attr))
         (note-name (enutil-aget 'title note-attr))
         (note-edit-mode (enutil-aget 'editMode note-attr))
         (note-content-file (enutil-aget 'contentFile note-attr))
         (opened-buf (enh-base-find-opened-buffer note-guid)))
    (unless note-content-file
      (setq note-attr (enh-get-note-attr note-guid))
      (setq note-content-file (enutil-aget 'contentFile note-attr)))
    (if opened-buf
        (enutil-move-cursor-to-window opened-buf t)
      (let ((buf (generate-new-buffer note-name)))
        (set-buffer buf)
        (insert-file-contents note-content-file)
        (when (string= note-edit-mode "XHTML")
          (setq evernote-note-xhtml-mode-content (buffer-string))
          (erase-buffer)
          (enh-format-enml evernote-note-xhtml-mode-content (current-buffer))
          (setq buffer-read-only t))
        (evernote-mode note-guid)
        ; this must be after (evernote-mode) that setup the change major mode hooks.
        (enh-base-change-major-mode-from-note-name note-name)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (pop-to-buffer buf)))))


(defun enh-base-create-note-common (default-note-name
                                     ask-notebook
                                     create-new-buffer
                                     change-to-evernote-mode
                                     &optional
                                     use-current-buffer-content)
  "Common procedure of creating a note"
  (let ((notebook-guid (and ask-notebook
                            (enutil-aget 'guid (enh-read-notebook))))
        (tag-names (enh-read-tag-names))
        (name (read-string "Note name:" default-note-name))
        (edit-mode (enh-read-edit-mode "TEXT"))
        content
        note-attr)
    (if use-current-buffer-content
        ;; create a note from a buffer.
        (progn
          (setq note-attr
                (enh-command-create-note (current-buffer)
                                         name
                                         notebook-guid
                                         tag-names
                                         edit-mode))
          (setq content (buffer-substring (point-min) (point-max))))
      ;; create a note from scratch.
      (if (string= edit-mode "TEXT") ;; edit-mode = TEXT
          (with-temp-buffer
            (setq note-attr
                  (enh-command-create-note (current-buffer)
                                           name
                                           notebook-guid
                                           tag-names
                                           edit-mode)))
        (with-temp-buffer ;; edit-mode = XHTML
          (insert enh-base-enml-template)
          (setq note-attr
                (enh-command-create-note (current-buffer)
                                         name
                                         notebook-guid
                                         tag-names
                                         edit-mode))
          (setq content (buffer-substring (point-min) (point-max))))))
    (enh-update-note-and-new-tag-attrs note-attr)
    (let ((buf nil))
      (save-excursion
        (if create-new-buffer
            (progn
              (setq buf (generate-new-buffer name))
              (set-buffer buf)
              (when content (insert content)))
          (when change-to-evernote-mode
            (rename-buffer name t)))
        (when change-to-evernote-mode
          (set-visited-file-name nil) ; set-visited-file-name must be before (evernote-mode) because it changes the mode line.
          (enh-base-change-major-mode-from-note-name name)
          (if (not evernote-mode)
              (evernote-mode (enutil-aget 'guid note-attr))
            (setq evernote-note-guid (enutil-aget 'guid note-attr))
            (enh-base-update-mode-line))
          (set-buffer-modified-p nil)))
      buf)))


(defun enh-base-update-note-common (inbuf guid &optional name notebook-guid is-tag-updated tag-names edit-mode)
  "Common procedure of opening a note"
  (let ((attr (enh-get-note-attr guid)))
    (setq attr
          (enh-command-update-note inbuf guid name notebook-guid is-tag-updated tag-names edit-mode))
    (enh-update-note-and-new-tag-attrs attr)))


(defun enh-base-read-note-attr (note-attrs &optional display-completion)
  "Prompts a note name and returns a note attribute"
  (let ((name-num-hash (make-hash-table :test #'equal))
        enh-base-displayed-name-attr-alist ; used in evernote-search-mode
        enh-base-displayed-name-formatted-name-alist) ; used in evernote-search-mode
    (mapc
     (lambda (attr)
       (let (name displayed-name)
         (setq name (enutil-aget 'title attr))
         (setq displayed-name
               (enh-base-get-displayed-note-name name name-num-hash))
         (setq enh-base-displayed-name-attr-alist
               (cons (cons displayed-name attr)
                     enh-base-displayed-name-attr-alist))
         (setq enh-base-displayed-name-formatted-name-alist
               (cons (cons displayed-name
                           (format "%-30s   %-20s   %-15s   %s"
                                   (enutil-aget 'updated attr)
                                   (enutil-aget 'name
                                                (enh-get-notebook-attr (enutil-aget 'notebookGuid attr)))
                                   (enh-tag-guids-to-comma-separated-names
                                    (enutil-aget 'tagGuids attr)
                                    15)
                                   displayed-name))
                     enh-base-displayed-name-formatted-name-alist))))
     note-attrs)
    (setq enh-base-displayed-name-attr-alist
          (nreverse enh-base-displayed-name-attr-alist))
    (setq enh-base-displayed-name-formatted-name-alist
          (nreverse enh-base-displayed-name-formatted-name-alist))
    (if display-completion
        (enh-base-display-note-completion-buf
         enh-base-displayed-name-formatted-name-alist))
    (enutil-aget (read-from-minibuffer "Note:"
                                       nil evernote-read-note-map)
                 enh-base-displayed-name-attr-alist)))


(defun enh-base-get-displayed-note-name (name name-hash)
  "Get displayed note name from the read note name"
  (let ((num (gethash name name-num-hash))
        result)
    (if num
        (progn
          (setq num (+ num 1))
          (setq result (format "%s(%d)" name num))
          (puthash name num name-num-hash))
      (setq result (substring name 0))
      (puthash name 1 name-num-hash))
    result))


(defun enh-base-display-note-completion-buf (displayed-name-formatted-name-alist &optional word)
  (let (formatted-name-displayed-name-alist completion-buf)
    (setq formatted-name-displayed-name-alist
          (mapcar (lambda (displayed-name)
                    (cons
                     (enutil-aget
                      displayed-name
                      enh-base-displayed-name-formatted-name-alist)
                     displayed-name))
                  (all-completions
                   (if word
                       word
                     "")
                   enh-base-displayed-name-formatted-name-alist)))
    (save-excursion
      (setq completion-buf (get-buffer-create "*Evernote-Completions*"))
      (set-buffer completion-buf)
      (enh-base-display-note-completion-list
       formatted-name-displayed-name-alist)
      (setq evernote-search-mode-formatted-name-displayed-name-alist
            formatted-name-displayed-name-alist)
      (evernote-search-mode))
    (display-buffer completion-buf)))


(defun enh-base-display-note-completion-list (formatted-name-displayed-name-alist)
  "Display formatted note names on this buffer"
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (format "total %d\n%-30s   %-20s   %-15s   %s\n\n"
                  (length formatted-name-displayed-name-alist)
                  "Last Modified"
                  "Notebook"
                  "Tags"
                  "Title"))
  (mapc (lambda (elem)
          (insert (car elem) "\n"))
        formatted-name-displayed-name-alist)
  (setq buffer-read-only t))


(defun enh-base-change-major-mode-from-note-name (note-name)
  (let ((buffer-file-name note-name))
    (normal-mode)))


(defun enh-base-find-opened-buffer (guid)
  "Find a buffer associated with guid"
  (let ((found_buf nil))
    (save-excursion
      (mapc (lambda (buf)
              (set-buffer buf)
              (if (string= evernote-note-guid guid)
                  (setq found_buf buf)))
            (buffer-list)))
    found_buf))


(defun enh-base-update-mode-line (&optional notebook-guid is-set-tag-names tag-names edit-mode)
  "Update mode line"
  (let ((note-attr (enh-get-note-attr evernote-note-guid)))
    (setq vc-mode
          (concat "[Notebook:"
                  (if notebook-guid
                      (enutil-aget 'name (enh-get-notebook-attr notebook-guid))
                    (enutil-aget 'name (enh-get-notebook-attr
                                        (enutil-aget 'notebookGuid note-attr))))
                  "] "
                  "[Tag:"
                  (if is-set-tag-names
                      (mapconcat #'identity tag-names ",")
                    (enh-tag-guids-to-comma-separated-names
                     (enutil-aget 'tagGuids note-attr)))
                  "] "
                  "[Edit mode:"
                  (if edit-mode
                      edit-mode
                    (enutil-aget 'editMode note-attr))
                  "]"))
    (force-mode-line-update)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for evernote-browsing-mode (enh-browsing-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-browsing-mode nil
  "Non-nil if Evernote Browsing mode is enabled.")
(make-variable-buffer-local 'evernote-browsing-mode)

(defvar enh-browsing-page-type nil)
(make-variable-buffer-local 'enh-browsing-page-type)

(defvar enh-browsing-page-description nil)
(make-variable-buffer-local 'enh-browsing-page-description)

(defvar enh-browsing-page-data nil)
(make-variable-buffer-local 'enh-browsing-page-data)

(defvar enh-browsing-page-widget-title nil)
(make-variable-buffer-local 'enh-browsing-page-widget-title)

(defvar enh-browsing-page-widget-root nil)
(make-variable-buffer-local 'enh-browsing-page-widget-root)

(defvar enh-browsing-page-setup-func nil)
(make-variable-buffer-local 'enh-browsing-page-setup-func)


(defun enh-browsing-open-notebook (widget &rest ignored)
  "Open a saved search in browsing mode"
  (enh-clear-onmem-cache)
  (let* ((guid (widget-value widget)) note-attrs)
    (enh-command-with-auth
     (setq note-attrs
           (enh-command-get-note-attrs-from-notebook-and-tag-guids guid nil)))
    (enh-browsing-update-page-list)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (format "Notes in Notebook: %s"
                                       (enutil-aget 'name
                                                    (enh-get-notebook-attr guid)))
                               note-attrs))))


(defun enh-browsing-open-tag (widget &rest ignored)
  "Open a tag in browsing mode"
  (enh-clear-onmem-cache)
  (let ((guid (widget-value widget)) note-attrs)
    (enh-command-with-auth
     (setq note-attrs
           (enh-command-get-note-attrs-from-notebook-and-tag-guids
            nil
            (if guid
                (list guid)
              nil))))
    (enh-browsing-update-page-list)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (if guid
                                   (format "Notes with tag %s"
                                           (enutil-aget 'name
                                                        (enh-get-tag-attr guid)))
                                 "All notes")
                               note-attrs))))


(defun enh-browsing-open-search (widget &rest ignored)
  "Open a saved search in browsing mode"
  (enh-clear-onmem-cache)
  (let* ((guid (widget-value widget)) note-attrs)
    (enh-command-with-auth
     (setq note-attrs
           (enh-command-get-note-attrs-from-query
            (enutil-aget 'query
                         (enh-get-search-attr guid)))))
    (enh-browsing-update-page-list)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (format "Query Result of Saved Search: %s"
                                       (enutil-aget 'name
                                                    (enh-get-search-attr guid)))
                               note-attrs))))


(defun enh-browsing-open-note (widget &rest ignored)
  "Open a note in browsing mode"
  (enh-clear-onmem-cache)
  (enh-command-with-auth
   (let* ((guid (widget-value widget))
          (note-attr (enh-get-note-attr guid))
          (cur-buf (current-buffer)))
     (enh-base-open-note-common note-attr)
     (let ((command-keys (this-command-keys)))
       (if (and (stringp command-keys)
                (or (string= "o" command-keys)
                    (string= "n" command-keys)
                    (string= "p" command-keys)))
           (enutil-move-cursor-to-window cur-buf t))))))


(defun enh-browsing-push-page (page &optional noswitch)
  "Push a new page to the browsing mode"
  (enutil-push page evernote-browsing-page-list)
  (if noswitch
      (save-excursion
        (set-buffer page)
        (funcall enh-browsing-page-setup-func))
    (setq evernote-browsing-current-page page)
    (switch-to-buffer page)
    (enh-command-with-auth
     (funcall enh-browsing-page-setup-func))))


(defun enh-browsing-create-page (type description &optional note-attrs)
  "Create a page structure of the attr-list"
  (let ((buf (generate-new-buffer (format "*ENB %s* " description))))
    (save-excursion
      (set-buffer buf)
      (setq enh-browsing-page-type type
            enh-browsing-page-description description
            enh-browsing-page-data note-attrs)
      (cond
       ((eq type 'notebook-list)
        (setq enh-browsing-page-setup-func
              'enh-browsing-setup-notebook-list-page))
       ((eq type 'tag-list)
        (setq enh-browsing-page-setup-func
              'enh-browsing-setup-tag-list-page))
       ((eq type 'search-list)
        (setq enh-browsing-page-setup-func
              'enh-browsing-setup-search-list-page))
       ((eq type 'note-list)
        (setq enh-browsing-page-setup-func
              'enh-browsing-setup-note-list-page)))
      (evernote-browsing-mode))
    buf))


(defun enh-browsing-setup-notebook-list-page ()
  "Insert notebook list into the browsing buffer"
  (when enh-browsing-page-widget-root
    (widget-delete enh-browsing-page-widget-root)
    (setq enh-browsing-page-widget-root nil))
  (let ((notebook-list nil))
    (maphash
     (lambda (guid attr)
       (let ((attr (enh-get-notebook-attr guid)))
         (enutil-push `(push-button
                        :tag ,(format "%-30s   %s"
                                      (enutil-aget 'name attr)
                                      (enutil-aget 'defaultNotebook attr))
                        :format "%[%t%]\n"
                        :notify enh-browsing-open-notebook
                        ,guid)
                      notebook-list)))
     (enh-get-notebook-attrs))
    (setq enh-browsing-page-widget-root
          (apply 'widget-create
                 `(group
                   (item ,(format "%s\n\ntotal %d\n%-30s   %s\n"
                                  enh-browsing-page-description
                                  (hash-table-count (enh-get-notebook-attrs))
                                  "Name"
                                  "Default"))
                   ,@(nreverse notebook-list)))))
  (widget-setup)
  (goto-char (point-min)))


(defun enh-browsing-setup-tag-list-page ()
  "Create a page structure of the attr-list"
  (when enh-browsing-page-widget-root
    (widget-delete enh-browsing-page-widget-title)
    (widget-delete enh-browsing-page-widget-root)
    (setq enh-browsing-page-widget-title nil)
    (setq enh-browsing-page-widget-root nil))
  (let ((guid-children-hash (make-hash-table :test #'equal)))
    (maphash
     (lambda (guid attr)
       (let* ((parent (enutil-aget 'parentGuid attr))
              (children (gethash parent guid-children-hash)))
         (if children
             (puthash parent (cons guid children) guid-children-hash)
           (puthash parent (list guid) guid-children-hash))))
     (enh-get-tag-attrs))
    (setq enh-browsing-page-widget-title
          (widget-create 'item (format "Tag List\n\ntotal %d\n" (hash-table-count (enh-get-tag-attrs)))))
    (setq enh-browsing-page-widget-root
          (apply 'widget-create
                 (enh-browsing-get-tag-tree nil))))
  (widget-setup)
  (goto-char (point-min)))


(defun enh-browsing-get-tag-tree (guid) ; root (eq guid nil)
  (let* ((children (gethash guid guid-children-hash))
         (attr (enh-get-tag-attr guid))
         (name (if attr (enutil-aget 'name attr) "All tags")))
    (if children
        `(tree-widget :node (push-button :tag ,name
                                         :format "%[%t%]\n"
                                         :notify enh-browsing-open-tag
                                         ,guid)
                      :args ,(mapcar
                              'enh-browsing-get-tag-tree
                              (nreverse children))
                      :open ,(if attr nil t))
      `(push-button :tag ,name
                    :format "%[%t%]\n"
                    :notify enh-browsing-open-tag
                    ,guid))))


(defun enh-browsing-setup-search-list-page ()
  "Insert saved search list into the browsing buffer"
  (when enh-browsing-page-widget-root
    (widget-delete enh-browsing-page-widget-root)
    (setq enh-browsing-page-widget-root nil))
  (let ((search-list nil))
    (maphash
     (lambda (guid attr)
       (let ((attr (enh-get-search-attr guid)))
         (enutil-push `(push-button
                        :tag ,(format "%-30s   %s"
                                      (enutil-aget 'name attr)
                                      (enutil-aget 'query attr))
                        :format "%[%t%]\n"
                        :notify enh-browsing-open-search
                        ,guid)
                      search-list)))
     (enh-get-search-attrs))
    (setq enh-browsing-page-widget-root
          (apply 'widget-create
                 `(group
                   (item ,(format "%s\n\ntotal %d\n%-30s   %s\n"
                                  enh-browsing-page-description
                                  (hash-table-count (enh-get-search-attrs))
                                  "Name"
                                  "Query"))
                   ,@(nreverse search-list)))))
  (widget-setup)
  (goto-char (point-min)))


(defun enh-browsing-setup-note-list-page ()
  "Insert note list into the browsing buffer"
  (when enh-browsing-page-widget-root
    (widget-delete enh-browsing-page-widget-root)
    (setq enh-browsing-page-widget-root nil))
  (let ((note-attrs enh-browsing-page-data)
        (note-list nil))
    (mapc
     (lambda (attr)
       (enutil-push `(push-button
                      :tag ,(format "%-30s   %-20s   %-15s   %4s   %s"
                                    (enutil-aget 'updated attr)
                                    (enutil-aget 'name
                                                 (enh-get-notebook-attr (enutil-aget 'notebookGuid attr)))
                                    (enh-tag-guids-to-comma-separated-names
                                     (enutil-aget 'tagGuids attr)
                                     15)
                                    (enutil-aget 'editMode attr)
                                    (enutil-aget 'title attr))
                      :format "%[%t%]\n"
                      :notify enh-browsing-open-note
                      ,(enutil-aget 'guid attr))
                    note-list))
     note-attrs)
    (setq enh-browsing-page-widget-root
          (apply 'widget-create
                 `(group
                   (item ,(format "%s\n\ntotal %d\n%-30s   %-20s   %-15s   %4s   %s\n"
                                  enh-browsing-page-description
                                  (length note-attrs)
                                  "Last Modified"
                                  "Notebook"
                                  "Tags"
                                  "Mode"
                                  "Name"))
                   ,@(nreverse note-list)))))
  (widget-setup)
  (goto-char (point-min)))


(defun enh-browsing-update-page-list ()
  (setq evernote-browsing-page-list
        (delete nil
                (mapcar (lambda (page)
                          (if (buffer-live-p page) page nil))
                        evernote-browsing-page-list)))
  (unless (memq evernote-browsing-current-page evernote-browsing-page-list)
    (setq evernote-browsing-current-page (car evernote-browsing-page-list))))


(defun enh-browsing-get-prev-page ()
  (cadr
   (memq evernote-browsing-current-page
         evernote-browsing-page-list)))


(defun enh-browsing-get-next-page ()
  (catch 'next-page
    (let (next-page)
      (mapc
       (lambda (page)
         (when (eq page evernote-browsing-current-page)
           (throw 'next-page next-page))
         (setq next-page page))
       evernote-browsing-page-list))))


(defun enh-browsing-get-page-of-type (type)
  "Get a page of the type in the browsing mode"
  (let ((result nil))
    (save-excursion
      (mapc
       (lambda (page)
         (set-buffer page)
         (when (eq enh-browsing-page-type type)
           (setq result page)))
       evernote-browsing-page-list))
    result))


(defun enh-browsing-reflesh-page (type)
  "Reflesh current page"
  (enh-browsing-update-page-list)
  (save-excursion
    (mapcar (lambda (page)
              (set-buffer page)
              (if (eq enh-browsing-page-type type)
                  (funcall enh-browsing-page-setup-func)))
            evernote-browsing-page-list)))

;
;
;(defun evernote-test-list-note ()
;  "Test"
;  (interactive)
;  (mapc
;   (lambda (tag)
;     (insert (format "guid=%s name=%s\n"
;                     (enutil-aget 'guid tag)
;                     (enutil-aget 'name tag))))
;   (enutil-aget 'tags (enh-command-list-tag))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for executing the external command (enh-command-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar enh-enclient-command
  (concat 
   (with-output-to-string
     (call-process evernote-ruby-command nil standard-output nil
                   "-rrbconfig" "-e" "print Config::CONFIG['bindir']"))
   "/enclient.rb")
  "Name of the enclient.rb command")
(defconst enh-command-process-name "Evernote-Client")
(defconst enh-command-output-buffer-name "*Evernote-Client-Output*")

;;
;; Const variables.
;;

(defconst enh-command-error-ok                  0)
(defconst enh-command-error-not-found         100)
(defconst enh-command-error-unexpected        101)
(defconst enh-command-error-not-authed        102)
(defconst enh-command-error-unknown             1)
(defconst enh-command-error-bad-data-format     2)
(defconst enh-command-error-permission-denied   3)
(defconst enh-command-error-internal-error      4)
(defconst enh-command-error-data-required       5)
(defconst enh-command-error-limit-reached       6)
(defconst enh-command-error-quota-reached       7)
(defconst enh-command-error-invalid-auth        8)
(defconst enh-command-error-auth-expired        9)
(defconst enh-command-error-data-conflict      10)
(defconst enh-command-error-enml-validation    11)
(defconst enh-command-error-shared-unavailable 12)

(defvar enh-command-last-result-code enh-command-error-ok)
(defvar enh-command-last-error-message nil)
(defvar enh-command-next-command-id 0)


(defun enh-command-login (user passwd)
  "Issue login command"
  (enh-command-issue
   (format ":class => %s, :user => %s, :passwd => %s"
           (enutil-to-ruby-string "AuthCommand")
           (enutil-to-ruby-string user)
           (enutil-to-ruby-string passwd))))


(defun enh-command-get-notebook-attrs ()
  "Issue listnotebooks command"
  (let ((reply (enh-command-issue
                (format ":class => %s"
                        (enutil-to-ruby-string "ListNotebookCommand")))))
    (enutil-aget 'notebooks reply)))


(defun enh-command-get-tag-attrs ()
  "Issue listtags command"
  (let ((reply (enh-command-issue
                (format ":class => %s"
                        (enutil-to-ruby-string "ListTagCommand")))))
    (enutil-aget 'tags reply)))


(defun enh-command-get-note-attr (guid)
  "Issue getnote command from the tag name list."
  (let ((reply (enh-command-issue
                (format ":class => %s, :guid => %s"
                        (enutil-to-ruby-string "GetNoteCommand")
                        (enutil-to-ruby-string guid)))))
    (enutil-aget 'note reply)))


(defun enh-command-get-note-attrs-from-notebook-and-tag-guids (notebook-guid tag-guids)
  "Issue listnotes command from the notebook guid and the tag guids."
  (message "notebook %s" notebook-guid)
  (let ((reply (enh-command-issue
                (format ":class => %s, :notebook_guid => %s, :tag_guids => %s"
                        (enutil-to-ruby-string "ListNoteCommand")
                        (enutil-to-ruby-string notebook-guid)
                        (enutil-to-ruby-string-list tag-guids nil)))))
    (enutil-aget 'notes reply)))


(defun enh-command-get-note-attrs-from-query (query)
  "Issue listnotes command from the query."
  (let ((reply (enh-command-issue
                (format ":class => %s, :query => %s"
                        (enutil-to-ruby-string "SearchNoteCommand")
                        (enutil-to-ruby-string query)))))
    (enutil-aget 'notes reply)))


(defun enh-command-get-note-content (guid edit-mode)
  "Issue getnotecontent command specified by the guid and the edit mode."
  (let ((reply (enh-command-issue
                (format ":class => %s, :guid => %s, :edit_mode => %s"
                        (enutil-to-ruby-string "GetContentCommand")
                        (enutil-to-ruby-string guid)
                        (enutil-to-ruby-string edit-mode)))))
    (enutil-aget 'content reply)))


(defun enh-command-create-note (inbuf name notebook-guid tag-names edit-mode)
  "Issue createnote command specified by the guid, tags and the edit-mode."
  (let ((reply (enh-command-issue
                (format ":class => %s, :notebook_guid => %s, :title => %s, :tag_names => %s, :edit_mode => %s, :content => %s"
                        (enutil-to-ruby-string "CreateNoteCommand")
                        (enutil-to-ruby-string notebook-guid)
                        (enutil-to-ruby-string name)
                        (enutil-to-ruby-string-list tag-names nil)
                        (enutil-to-ruby-string edit-mode)
                        (enutil-to-ruby-string (enutil-buffer-string inbuf))))))
    (enutil-aget 'note reply)))


(defun enh-command-update-note (inbuf guid name notebook-guid is-tag-updated tag-names edit-mode)
  "Issue updatenote command specified by the guid and the parameters for updating."
  (let ((reply (enh-command-issue
                (format ":class => %s, :guid => %s, :notebook_guid => %s, :title => %s, :tag_names => %s, :edit_mode => %s, :content => %s"
                        (enutil-to-ruby-string "UpdateNoteCommand")
                        (enutil-to-ruby-string guid)
                        (enutil-to-ruby-string notebook-guid)
                        (enutil-to-ruby-string name)
                        (enutil-to-ruby-string-list tag-names is-tag-updated)
                        (enutil-to-ruby-string edit-mode)
                        (enutil-to-ruby-string (enutil-buffer-string inbuf))))))
    (enutil-aget 'note reply)))


(defun enh-command-delete-note (guid)
  "Issue deletenote command specified by the guid, tags and the edit mode."
  (enh-command-issue
   (format ":class => %s, :guid => %s"
           (enutil-to-ruby-string "DeleteNoteCommand")
           (enutil-to-ruby-string guid))))


(defun enh-command-create-notebook (name default-notebook)
  "Issue createnotebook command"
  (enh-command-issue
   (format ":class => %s, :name => %s, :default_notebook => %s"
           (enutil-to-ruby-string "CreateNotebookCommand")
           (enutil-to-ruby-string name)
           (if default-notebook "true" "false"))))


(defun enh-command-update-notebook (guid name default-notebook)
  "Issue updatenotebook command"
  (enh-command-issue
   (format ":class => %s, :guid => %s, :name => %s, :default_notebook => %s"
           (enutil-to-ruby-string "UpdateNotebookCommand")
           (enutil-to-ruby-string guid)
           (enutil-to-ruby-string name)
           (if default-notebook "true" "false"))))


(defun enh-command-get-search-attrs ()
  "Issue listsearch command"
  (let ((reply (enh-command-issue
                (format ":class => %s"
                        (enutil-to-ruby-string "ListSearchCommand")))))
    (enutil-aget 'searches reply)))


(defun enh-command-create-search (name query)
  "Issue createsearch command"
  (enh-command-issue
   (format ":class => %s, :name => %s, :query => %s"
           (enutil-to-ruby-string "CreateSearchCommand")
           (enutil-to-ruby-string name)
           (enutil-to-ruby-string query))))


(defun enh-command-update-search (guid name query)
  "Issue updatesearch command"
  (enh-command-issue
   (format ":class => %s, :guid => %s, :name => %s, :query => %s"
           (enutil-to-ruby-string "UpdateSearchCommand")
           (enutil-to-ruby-string guid)
           (enutil-to-ruby-string name)
           (enutil-to-ruby-string query))))


(defun enh-command-issue (command)
  (enh-command-setup-process)
  (let ((proc (get-process enh-command-process-name))
        (reply nil)
        (buffer (get-buffer enh-command-output-buffer-name)))
    (message "Waiting for the result...")
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      ;(delete-region (point-min) (point-max))
      (setq enh-command-next-command-id
            (+ 1 enh-command-next-command-id))
      (process-send-string proc
                           (format "{%s, :command_id => %d}"
                                   command enh-command-next-command-id))
      (process-send-string proc "\x00\n")
      (while (or (not reply)
                 (enutil-neq (enutil-aget 'command_id reply)
                             enh-command-next-command-id))
        (accept-process-output proc)
        (setq reply (enutil-get-first-sexp-in-buffer))))
    (message "")
    (if (eq (enutil-aget 'class reply) 'ErrorReply)
        (progn
          (setq enh-command-last-result-code (enutil-aget 'result_code reply))
          (setq enh-command-last-error-message (enutil-aget 'message reply))
          (throw 'error enh-command-last-result-code))
      (setq enh-command-last-result-code enh-command-error-ok)
      reply)))


(defun enh-command-sentinel (process event)
  (error "enclient.rb %s%s" event
         (with-current-buffer enh-command-output-buffer-name
           (buffer-string))))

(defun enh-command-setup-process ()
  (let ((proc (get-process enh-command-process-name))
        (process-connection-type nil)) ; use pipe instead of pty
    (when (or (not proc)
              (not (eq (process-status proc) 'run)))
      (setq proc (start-process enh-command-process-name
                                enh-command-output-buffer-name
                                evernote-ruby-command "-S" enh-enclient-command))
      (set-process-sentinel proc 'enh-command-sentinel)
      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-query-on-exit-flag proc nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for all modes in this file (enh-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar enh-notebook-info (make-hash-table :test #'equal)
  "Notebook info associated with the guid")


(defvar enh-tag-info (make-hash-table :test #'equal)
  "Tag info associated with the guid")


(defvar enh-search-info (make-hash-table :test #'equal)
  "Saved search info associated with the guid")


(defvar enh-note-attr nil
  "Note attr associated with most recently accessed guid")


(defvar enh-password-cache-file "~/.evernote-mode/password.gpg"
  "Filename of saving password cache.")


(defvar enh-password-cache-buffer " *evernote-password-cache*")


(defun enh-get-notebook-attrs ()
  (when (eq (hash-table-count enh-notebook-info) 0)
    (mapc
     (lambda (attr)
       (puthash (enutil-aget 'guid attr) attr enh-notebook-info))
     (enh-command-get-notebook-attrs)))
  enh-notebook-info)


(defun enh-get-tag-attrs ()
  (when (eq (hash-table-count enh-tag-info) 0)
    (mapc
     (lambda (attr)
       (puthash (enutil-aget 'guid attr) attr enh-tag-info))
     (enh-command-get-tag-attrs)))
  enh-tag-info)


(defun enh-get-search-attrs ()
  (when (eq (hash-table-count enh-search-info) 0)
    (mapc
     (lambda (attr)
       (puthash (enutil-aget 'guid attr) attr enh-search-info))
     (enh-command-get-search-attrs)))
  enh-search-info)


(defun enh-get-note-attr (guid)
  "Get the note attr from the guid"
  (if (and enh-note-attr
           (string= guid (enutil-aget 'guid enh-note-attr)))
      enh-note-attr
    (setq enh-note-attr (enh-command-get-note-attr guid))))


(defun enh-get-notebook-attr (guid)
  "Get the notebook attr from the guid"
  (gethash guid (enh-get-notebook-attrs)))


(defun enh-get-tag-attr (guid)
  "Get the tag attr from the guid"
  (gethash guid (enh-get-tag-attrs)))


(defun enh-get-search-attr (guid)
  "Get the search attr from the guid"
  (gethash guid (enh-get-search-attrs)))


(defun enh-read-tag-guids (&optional prompt)
  (enh-tag-names-to-guids
   (enutil-completing-read-multiple
    (if prompt
        prompt
      "Tags (comma separated form):")
    (enh-get-tag-name-alist)
    nil
    t)))


(defun enh-read-tag-names (&optional prompt note-guid)
  (enutil-completing-read-multiple
   (if prompt
       prompt
     "Tags (comma separated form):")
   (enh-get-tag-name-alist)
   nil
   nil
   (if note-guid
       (enh-tag-guids-to-comma-separated-names
        (enutil-aget 'tagGuids
                     (enh-get-note-attr note-guid)))
     nil)))


(defun enh-format-enml (content outbuf)
  (if evernote-enml-formatter-command
      (let ((infile (concat (make-temp-file "evernote-enml") ".html"))
            (command (car evernote-enml-formatter-command))
            (args (cdr evernote-enml-formatter-command)))
        (setq args (append args (list infile)))
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) infile)
          (message "") ; remove the message notifying writing to tmp file.
          (let ((coding-system-for-read 'utf-8)
                (coding-system-for-write 'utf-8))
            (apply 'call-process
                   command
                   infile
                   outbuf
                   nil
                   args))))
    (save-excursion ; insert the content as is.
      (set-buffer outbuf)
      (insert content))))


(defun enh-clear-onmem-cache ()
  (clrhash enh-notebook-info)
  (clrhash enh-tag-info)
  (clrhash enh-search-info)
  (setq enh-note-attr nil))


(defun enh-read-notebook (&optional default)
  (let ((notebook-name-attr-alist (enh-get-notebook-name-attr-alist)))
    (enutil-aget (completing-read
                  "Notebook:"
                  notebook-name-attr-alist
                  nil
                  t
                  default)
                 notebook-name-attr-alist)))


(defun enh-read-saved-search (&optional prompt)
  (let ((search-name-query-alist (enh-get-search-name-attr-alist)))
    (enutil-aget (completing-read
                  (if prompt
                      prompt
                    "Saved search:")
                  search-name-query-alist
                  nil t)
                 search-name-query-alist)))


(defun enh-get-notebook-name-attr-alist ()
  "Get the notebook alist for completion from command output"
  (let (result)
    (maphash
     (lambda (guid attr)
       (setq result
             (cons
              (cons (enutil-aget 'name attr)
                    attr)
              result)))
     (enh-get-notebook-attrs))
    result))


(defun enh-get-tag-name-alist ()
  "Get the tag alist for completion from command output"
  (let (result)
    (maphash
     (lambda (guid attr)
       (setq result
             (cons
              (list (enutil-aget 'name attr))
              result)))
     (enh-get-tag-attrs))
    result))


(defun enh-get-search-name-attr-alist ()
  "Get the alist for completion from command output"
  (let (result)
    (maphash
     (lambda (guid attr)
       (setq result
             (cons
              (cons (enutil-aget 'name attr)
                    attr)
              result)))
     (enh-get-search-attrs))
    result))


(defun enh-update-note-and-new-tag-attrs (note-attr)
  (let ((tag-guids (enutil-aget 'tagGuids note-attr))
        (tag-attrs (enh-get-tag-attrs)))
    (when (catch 'result
            (mapc
             (lambda (guid)
               (unless (gethash guid tag-attrs)
                 (throw 'result t)))
             tag-guids)
            nil)
      (enh-clear-onmem-cache)
      (enh-browsing-reflesh-page 'tag-list))))


(defun enh-tag-guids-to-comma-separated-names (tag-guids &optional maxlen)
  (let (line)
    (setq line
          (mapconcat
           (lambda (guid)
             (enutil-aget 'name (enh-get-tag-attr guid)))
           tag-guids
           ","))
    (if maxlen
        (truncate-string-to-width line maxlen)
      line)))


(defun enh-tag-names-to-comma-separated-oct-names (tag-names)
  (mapconcat #'identity (mapcar 'enutil-string-to-oct tag-names) ","))


;;(defun enh-tag-guids-to-names (tag-guids)
;;  (mapcar
;;   (lambda (tag-guid)
;;     (enutil-aget 'name (enh-get-tag-attr tag-guid)))
;;   tag-guids))


(defun enh-notebook-name-to-guid (notebook-name)
  (let ((notebook-attrs (enh-get-notebook-attrs)))
    (catch 'guid
      (maphash
       (lambda (guid attr)
         (let ((name (enutil-aget 'name attr)))
           (if (equal notebook-name name)
               (throw 'guid guid))))
       notebook-attrs))))


(defun enh-tag-names-to-guids (tag-names)
  (let ((tag-attrs (enh-get-tag-attrs)))
    (mapcar
     (lambda (name)
       (catch 'guid
         (maphash
          (lambda (guid attr)
            (let ((tag-name (enutil-aget 'name attr)))
              (if (equal tag-name name)
                  (throw 'guid guid))))
          tag-attrs)))
     tag-names)))


(defun enh-read-edit-mode (default)
  (let ((edit-mode
         (completing-read "Edit mode (type \"TEXT\" or \"XHTML\"):"
                          '(("TEXT") ("XHTML"))
                          nil
                          t
                          default)))
    (if (and edit-mode (not (string= edit-mode "")))
        edit-mode
      default)))


(defun enh-bookmark-make-record ()
  "Make a emacs bookmark entry for a evernote buffer."
  `(,(buffer-name)
    ,@(bookmark-make-record-default 'no-file)
    ;; if bookmark-bmenu-toggle-filenames is t and a bookmark record doesn't
    ;; have filename field, , Emacs23.2 raises an error.
    ;; Here is the workaround suggested by ARISAWA Akihiro.
    (filename . ,(format "%s (evernote:%s)" (buffer-name) evernote-note-guid))
    (handler . enh-bookmark-jump)))


(defun enh-bookmark-jump (bookmark) ;; Note: don't rename this function for the bookmark file compatibility
  "Default bookmark handler for evernote buffers."
  (enh-command-with-auth
   (let ((filename (bookmark-get-filename bookmark)))
     (if (and filename (string-match "(evernote:\\(.*\\))$" filename))
         (progn
           (let* ((guid (substring filename (match-beginning 1) (match-end 1)))
                  (attr (enh-command-get-note-attr guid)))
             (enh-base-open-note-common attr)
             (let ((buf (current-buffer)))
               (bookmark-default-handler
                `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark))))))
       (message (format "Invalid bookmark %s" (bookmark-name-from-full-record bookmark)))))))


(defun enh-bookmark-prepare ()
  (interactive)
  (set (make-local-variable 'bookmark-make-record-function)
       'enh-bookmark-make-record))


(defun enh-password-cache-load ()
  "Load the password cache from the file"
  (when (and evernote-password-cache
             (file-exists-p enh-password-cache-file))
    (with-current-buffer (get-buffer-create enh-password-cache-buffer)
      (insert-file-contents enh-password-cache-file)
      (read (current-buffer)))))


(defun enh-password-cache-save (user-password)
  "Save the password cache to the file"
  (when evernote-password-cache
    (with-current-buffer (get-buffer-create enh-password-cache-buffer)
      (write-region (prin1-to-string user-password) nil
                    enh-password-cache-file))))


(defun enh-password-cache-close ()
  "Close the password cache buffer"
  (when (get-buffer enh-password-cache-buffer)
    (kill-buffer enh-password-cache-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General util functions (enutil-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun enutil-completing-read-multiple
  (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read multiple strings with completion. and return nil if no input is given"
  (let (results)
    (setq results
          (completing-read-multiple prompt
                                    table
                                    predicate
                                    require-match
                                    initial-input
                                    hist
                                    def
                                    inherit-input-method))
    (delete "" results)))


(defun enutil-aget (key alist)
  (let ((result-cons (assoc key alist)))
    (when result-cons
      (cdr result-cons))))


(defun enutil-aset (key alist value)
  (let ((result-cons (assoc key alist)))
    (if result-cons
        (setcdr result-cons value)
      (setq alist (cons (cons key value) alist)))
    alist))


(defun enutil-get-current-line-string ()
  (save-excursion
    (buffer-substring
     (progn
       (beginning-of-line)
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun enutil-erase-buffer-forcibly ()
  (let ((buffer-read-only nil))
    (erase-buffer)))


(defun enutil-get-first-line (str)
  "Get first line of the string"
  (let ((begin (string-match "^.*$" str)))
    (substring str begin (match-end 0))))


(defun enutil-get-minibuffer-string ()
  (save-excursion
    (enutil-set-buffer-to-minibuffer)
    (buffer-substring
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun enutil-set-minibuffer-string (str)
  (save-excursion
    (enutil-set-buffer-to-minibuffer)
    (delete-region
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))
    (insert str)))


(defun enutil-set-buffer-to-minibuffer ()
  (set-buffer (window-buffer (active-minibuffer-window))))


(defun enutil-minibuffer-tmp-message (msg)
  (save-excursion
    (goto-char (point-max))
    (save-excursion (insert " " msg))
    (sit-for 1)
    (delete-region (point) (point-max))))


(defun enutil-move-cursor-to-window (buf &optional pop)
  "Move cursor to the window associated with the bufer"
  (let ((buf-window (get-buffer-window buf)))
    (if buf-window
        (select-window buf-window)
      (if pop
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))


(defun enutil-get-first-sexp-in-buffer ()
  (condition-case nil
      (car (read-from-string
            (buffer-substring
             (point-min)
             (point-max))))
    (error nil)))


(defun enutil-hash-mapcar (func hash)
  (let (result)
    (maphash
     (lambda (key value)
       (cons
        (funcall func key value)
        result)))
    (nreverse result)))


(defun enutil-to-ruby-string (str)
  (if str
      (progn
        (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
        (setq str (replace-regexp-in-string "'" "\\\\'" str))
        (concat  "'" str "'"))
    "nil"))


(defun enutil-to-ruby-string-list (str-list return-empty-array)
  (if str-list
      (concat
       "["
       (mapconcat #'enutil-to-ruby-string str-list ",")
       "]")
    (if return-empty-array
        "[]"
      "nil")))


(defun enutil-buffer-string (buf)
  (save-excursion
    (set-buffer buf)
    (buffer-string)))


(provide 'evernote-mode)

;;(setq debug-on-error t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

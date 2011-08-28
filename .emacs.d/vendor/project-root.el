;;; project-root.el --- Define a project root and take actions based upon it.

;; Copyright (C) 2008-2010 Philip Jackson, Alexander Solovyov, Vladimir Sidorenko

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Author: Alexander Solovyov <piranha@piranha.org.ua>
;; Author: Vladimir Sidorenko <yoyavova@gmail.com>
;; Version: 0.8-pre

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; project-root.el allows the user to create rules that will identify
;; the root path of a project and then run an action based on the
;; details of the project.
;;
;; Example usage might be might be that you want a certain indentation
;; level/type for a particular project.
;;
;; once project-root-fetch has been run `project-details' will either
;; be nil if nothing was found or the project name and path in a cons
;; pair.

;; An example configuration:

;; (setq project-roots
;;       `(("Generic Perl Project"
;;          :root-contains-files ("t" "lib")
;;          :filename-regex ,(regexify-ext-list '(pl pm))
;;          :on-hit (lambda (p) (message (car p))))
;;         ("Django project"
;;          :root-contains-files ("manage.py")
;;          :filename-regex ,(regexify-ext-list '(py html css js))
;;          :exclude-paths ("media" "contrib"))))
;;
;; I bind the following:
;;
;; (global-set-key (kbd "C-c p f") 'project-root-find-file)
;; (global-set-key (kbd "C-c p g") 'project-root-grep)
;; (global-set-key (kbd "C-c p a") 'project-root-ack)
;; (global-set-key (kbd "C-c p d") 'project-root-goto-root)
;; (global-set-key (kbd "C-c p p") 'project-root-run-default-command)
;; (global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
;;
;; (global-set-key (kbd "C-c p M-x")
;;                 'project-root-execute-extended-command)
;;
;; (global-set-key
;;  (kbd "C-c p v")
;;  (lambda ()
;;    (interactive)
;;    (with-project-root
;;        (let ((root (cdr project-details)))
;;          (cond
;;            ((file-exists-p ".svn")
;;             (svn-status root))
;;            ((file-exists-p ".git")
;;             (git-status root))
;;            (t
;;             (vc-directory root nil)))))))
;;
;; This defines one project called "Generic Perl Projects" by running
;; the tests path-matches and root-contains-files. Once these tests
;; have been satisfied and a project found then (the optional) :on-hit
;; will be run.

;;; The tests:

;; :path-matches maps to `project-root-path-matches' and
;; :root-contains-files maps to `project-root-upward-find-files'. You
;; can use any amount of tests.

;;; Configuration:

;; :filename-regex should contain regular expression, which is passed
;;  to `find` to actually find files for your project.
;; :exclude-paths can contain paths to omit when searching for files.

;;; Bookmarks:

;; If you fancy it you can add a :bookmarks property (with a list of
;; strings) and when you run `project-root-browse-seen-projects' you
;; will see the bookmarks listed under the project name, linking
;; relatively to the project root. Also, the bookmarks will present
;; themselves as anything candidates if you configure as instructed
;; below.

;;; The default command:

;; If you give a project a :default-command property you can execute
;; it by running `project-root-run-default-command'. Nothing fancy but
;; very handy.

;;; installation:

;; Put this file into your `load-path' and evaulate (require
;; 'project-root).

;;; Using yourself:

;; If you wrap a call in `with-project-root' then everything in its
;; body will execute under project root:
;;
;; (with-project-root
;;  (shell-command-to-string "pwd"))

;;; anything.el intergration

;; If you want to add the bookmarks for the current project to the
;; anything source list then use:
;;
;; (add-to-list 'anything-sources
;;              project-root-anything-config-bookmarks)
;;
;; If you want to add the bookmarks for each of the files in the
;; current project to the anything source list then use:
;;
;; (add-to-list 'anything-sources
;;              project-root-anything-config-files)

(require 'find-cmd)
(require 'cl)

(eval-when-compile
  (defvar anything-project-root)
  (require 'outline)
  (require 'dired))

(defun project-root-find-prune (paths &optional no-default-directory)
  (mapconcat '(lambda (path)
                (if no-default-directory
                    (concat " -path \"" path "\" -prune ")
                  (concat " -path \"" default-directory path "\" -prune ")))
             paths "-o"))

(defvar project-root-extra-find-args
  (project-root-find-prune '("*/.hg" "*/.git" "*/.svn") t)
;  (find-to-string '(prune (name ".svn" ".git" ".hg")))
  "Extra find args that will be AND'd to the defaults (which are
in `project-root-file-find-process')")

(defvar project-root-seen-projects nil
  "All of the projects that we have met so far in this session.")

(defvar project-root-file-cache nil
  "Cache for `completing-read'")

(make-variable-buffer-local
 (defvar project-details nil
  "The name and path of the current project root."))

(defvar project-root-test-dispatch
  '((:root-contains-files . project-root-upward-find-files)
    (:path-matches . project-root-path-matches))
  "Map a property name to root test function.")

(defvar project-roots nil
  "An alist describing the projects and how to find them.")

(defvar project-root-max-search-depth 20
  "Don't go any further than this many levels when searching down
a filesystem tree")

(defvar project-root-find-options
  ""
  "Extra options to pass to `find' when using project-root-find-file.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

(defvar project-root-storage-file "~/.emacs.d/.project-roots"
  "File, where seen projects info is saved.")

(defvar project-root-project-name-func 'project-root-project-name-from-dir
  "Function to generate cute name for project.")

(defun project-root-run-default-command ()
  "Run the command in :default-command, if there is one."
  (interactive)
  (with-project-root
      (let ((command (project-root-data
                      :default-command project-details)))
        (when command
          (funcall command)))))

(defun project-root-project-name (project)
  (funcall project-root-project-name-func project))

(defun project-root-path-matches (re)
  "Apply RE to the current buffer name returning the first
match."
  (let ((filename (cond
                    ((string= major-mode "dired-mode")
                     (dired-get-filename nil t))
                    (buffer-file-name
                     buffer-file-name))))
    (when (and filename (not (null (string-match re filename))))
      (match-string 1 filename))))

(defun project-root-get-root (project)
  "Fetch the root path of the project according to the tests
described in PROJECT."
  (let ((root (plist-get project :root))
        (new-root))
    (catch 'not-a-project
      (mapc
       (lambda (test)
         (when (plist-get project (car test))
           ;; grab a potentially different root
           (setq new-root
                 (funcall (cdr test) (plist-get project (car test))))
           (cond
             ((null new-root)
              (throw 'not-a-project nil))
             ;; check root is so far consistent
             ((and (not (null root))
                   (not (string= root new-root)))
              (throw 'not-a-project nil))
             (t
              (setq root new-root)))))
       project-root-test-dispatch)
      (when root
        (file-name-as-directory root)))))

(defun project-root-data (key &optional project)
  "Grab the value (if any) for key in PROJECT. If PROJECT is
ommited then attempt to get the value for the current
project."
  (let ((project (or project project-details)))
    (plist-get (cdr (assoc (car project) project-roots)) key)))

(defun project-root-bookmarks (&optional project)
  "Grab the bookmarks (if any) for PROJECT."
  (project-root-data :bookmarks project))

(defun project-root-project-name-from-dir (project)
  "Generate cute name for project from its directory name."
  (upcase-initials (car (last (split-string (cdr project) "/" t)))))

(defun project-root-gen-org-url (project)
  ;; The first link to the project root itself
  (concat
   (format "** [[file:%s][%s]] (%s)"
           (cdr project)
           (project-root-project-name project)
           (cdr project))
   (mapconcat
    (lambda (b)
      (let ((mark (concat (cdr project) b)))
        (format "*** [[file:%s][%s]] (%s)" mark b mark)))
    (project-root-bookmarks project)
    "\n")
   "\n"))

(define-derived-mode project-root-list-mode org-mode "Project-List"
  (setq buffer-read-only t))

(dolist (keyfunc
         `(("q" kill-this-buffer)
           ("s" isearch-forward)
           ("r" isearch-backward)
           (,(kbd "RET")
            (lambda () (interactive) (beginning-of-line)
              (org-next-link) (org-open-at-point t)))
           (,(kbd "C-d") (lambda () (interactive)
                           (setq buffer-read-only nil)
                           (delete-region
                            (line-beginning-position)
                            (line-beginning-position 2))
                           (setq buffer-read-only t)))))

  (define-key project-root-list-mode-map (car keyfunc) (cadr keyfunc)))

(defun project-root-browse-seen-projects ()
  "Browse the projects that have been seen so far this session."
  (interactive)
  (let ((current-project project-details)
        (point-to nil))
    (if (not project-root-seen-projects)
        (project-root-load-roots))

    (switch-to-buffer (get-buffer-create "*Seen Project List*"))
    (erase-buffer)
    (insert "* Seen projects\n")
    (mapc (lambda (p)
            (when (file-exists-p (cdr p))
              (when (equal p current-project)
                (setq point-to (point)))
              (insert (project-root-gen-org-url p))))
          project-root-seen-projects)

    (project-root-list-mode)
    ;; show everything at second level
    (goto-char (point-min))
    (show-children)
    ;; expand bookmarks for current project only
    (when point-to
      (goto-char (+ point-to 3))
      (show-children))))

(defun project-root-save-roots ()
  "Saves seen projects info to file. Note that
 this is not done automatically"
  (interactive)
  (with-temp-buffer
    (print project-root-seen-projects (current-buffer))
    (write-file project-root-storage-file)))

(defun project-root-load-roots ()
  "Loads seen projects info from file"
  (interactive)
  (if (file-exists-p project-root-storage-file)
      (with-temp-buffer
        (insert-file-contents project-root-storage-file)
        (setq project-root-seen-projects (read (buffer-string))))))


;; TODO: refactor me
(defun project-root-fetch (&optional dont-run-on-hit)
  "Attempt to fetch the root project for the current file. Tests
will be used as defined in `project-roots'."
  (interactive)
  (let ((project
         (catch 'root-found
           (unless (mapc
                    (lambda (project)
                      (let ((name (car project))
                            (run (project-root-data :on-hit project))
                            (root (project-root-get-root (cdr project))))
                        (when root
                          (when (and root (not dont-run-on-hit) run)
                            (funcall run (cons name root)))
                          (throw 'root-found (cons name root)))))
                    project-roots)
             nil))))
    ;; set the actual var used by apps and add to the global project
    ;; list
    (when project
      (project-root-set-project project))))

(defun project-root-set-project (p)
  (if (not project-root-seen-projects)
      (project-root-load-roots))
  (when (not (member p project-root-seen-projects))
    (add-to-list 'project-root-seen-projects project)
    (project-root-save-roots))
  (setq project-details project))

(defun project-root-every (pred seq)
  "Return non-nil if pred of each element, of seq is non-nil."
  (catch 'got-nil
    (mapc (lambda (x)
            (unless (funcall pred x)
              (throw 'got-nil nil)))
          seq)))

(defun project-root-upward-find-files (filenames &optional startdir)
  "Return the first directory upwards from STARTDIR that contains
all elements of FILENAMES. If STATDIR is nil then use
current-directory."
  (let ((default-directory (expand-file-name (or startdir ".")))
        (depth 0))
    (catch 'pr-finish
      (while t
        ;; don't go too far down the tree
        (when (> (setq depth (1+ depth)) project-root-max-search-depth)
          (throw 'pr-finish nil))
        (cond
          ((project-root-every 'file-exists-p filenames)
           (throw 'pr-finish default-directory))
          ;; if we hit root
          ((string= (expand-file-name default-directory) "/")
           (throw 'pr-finish nil)))
        ;; try again up a directory
        (setq default-directory
              (expand-file-name ".." default-directory))))))

(defun project-root-p (&optional p)
  "Check to see if P or `project-details' is valid"
  (let ((p (or p project-details)))
    (and p (file-exists-p (cdr p)))))

(defun regexify-ext-list (extensions)
  "Turn a list of extensions to a regexp."
  (concat ".*\\.\\(" (mapconcat '(lambda (x) (format "%s" x))
                                extensions "\\|") "\\)"))

(defmacro with-project-root (&rest body)
  "Run BODY with default-directory set to the project root. Error
if not found. If `project-root' isn't defined then try and find
one."
  (declare (indent 2))
  `(progn
     (unless project-details (project-root-fetch))
     (if (project-root-p)
         (let ((default-directory (cdr project-details))
               (filename-regex (or (project-root-data :filename-regex) ".*"))
               (exclude-paths (project-root-data :exclude-paths)))
           ,@body)
       (error "No project root found"))))

(defun project-root-goto-root ()
  "Open up the project root in dired."
  (interactive)
  (with-project-root (find-file default-directory)))

(defun project-root-grep ()
  "Run the grep command from the current project root."
  (interactive)
  (with-project-root (call-interactively 'grep)))

(defun project-root-ack ()
  "Run the ack command from the current project root (if ack is
available)."
  (interactive)
  (with-project-root
    (if (fboundp 'ack)
        (call-interactively 'ack)
        (error "`ack' not bound"))))

(defun project-root-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (project-root-filename file)
                                     (expand-file-name file))))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string
                           (project-root-find-cmd))))))

(setq .project-root-find-executable nil)
(defun project-root-find-executable ()
  (if .project-root-find-executable
      .project-root-find-executable
    (setq .project-root-find-executable (executable-find "gfind"))
      (if (not .project-root-find-executable)
          (setq .project-root-find-executable (executable-find "find")))
      .project-root-find-executable))

(defun project-root-find-cmd (&rest pattern)
  (let ((pattern (car pattern)))
    ;; TODO: use find-cmd here
    (concat (project-root-find-executable) " " default-directory
            (project-root-find-prune exclude-paths)
            project-root-extra-find-args
            ", -type f -regex \"" filename-regex "\" "
            (if pattern (concat " -name '*" pattern "*' "))
            project-root-find-options)))

(defun project-root-filename (file)
  (let ((name (replace-regexp-in-string default-directory ""
                                        (expand-file-name file))))
    (mapconcat 'identity (reverse (split-string name "/")) "\\")))

(defun project-root-find-file ()
  "Find a file from a list of those that exist in the current
project."
  (interactive)
  (with-project-root
      (let* ((project-files (project-root-files))
             (file (if (functionp 'ido-completing-read)
                       (ido-completing-read "Find file in project: "
                                            (mapcar 'car project-files))
                     (completing-read "Find file in project: "
                                      (mapcar 'car project-files)))))
        (find-file (cdr (assoc file project-files))))))

(defun project-root-execute-extended-command ()
  "Run `execute-extended-command' after having set
`default-directory' to the root of the current project."
  (interactive)
  (with-project-root (execute-extended-command current-prefix-arg)))

(defun project-root-file-in-project (filename &optional p)
  "Check to see if FILENAME is in the project P. If P is omitted
then the current project-details are used."
  (let ((p (or p (progn
                   (project-root-fetch)
                   project-details))))
    (and
     p
     (file-exists-p filename)
     (not (null (string-match
                 (regexp-quote (abbreviate-file-name (cdr p)))
                  (abbreviate-file-name filename)))))))

(defun project-root-buffer-in-project (buffer &optional p)
  "Check to see if buffer is in project"
  (let ((filename (buffer-file-name buffer)))
    (and filename (project-root-file-in-project filename p))))

(defun ido-ignore-not-in-project (name)
  "Function to use with ido-ignore-buffers.
 Ignores files that are not in current project."
  (not (project-root-buffer-in-project (get-buffer name))))

(defun project-root-switch-buffer (arg)
  "ido-switch-buffer replacement. Ignore buffers that are not in current project,
   fallback to original ido-switch-buffer if no current project.
   Can be used with universal-argument to run orifinal function even in project."
  (interactive "P")
  (if (and (null arg) (or project-details (project-root-fetch)))
      (with-project-root
          (let ((ido-ignore-buffers
                 (append '(ido-ignore-not-in-project) ido-ignore-files)))
            (ido-switch-buffer)
            ))
    (ido-switch-buffer)))

(defun project-root-projects-names ()
  "Generates a list of pairs - project name and path."
  (mapcar (lambda (project)
            (cons (project-root-project-name project) (cdr project)))
          project-root-seen-projects))

(defun project-root-open-project ()
  "Open project with ido-mode."
  (interactive)
  (let* ((project-names (project-root-projects-names))
         (project (ido-completing-read "Select project: " (mapcar 'car project-names))))
    (find-file (cdr (assoc project project-names)))))


;;; anything.el config

(defun project-root-anything-colourfy-hits (hits)
  ;; delete the project-root part
  (let ((highs (project-root-data :anything-highlight
                                  anything-project-root)))
    (mapcar
     '(lambda (hit)
       (let ((new (replace-regexp-in-string
                   (regexp-quote (cdr anything-project-root))
                   ""
                   hit)))
         (when highs
           (mapc '(lambda (s)
                   ;; propertize either the first group or the whole
                   ;; string
                   (when (string-match (car s) new)
                     (put-text-property (or (match-beginning 1) 0)
                                        (or (match-end 1) (length new))
                                        'face (cdr s)
                                        new)))
                 highs))
         (cons new hit)))
     hits)))

(defvar project-root-anything-config-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-file-find-process anything-pattern)))
    (candidate-transformer . project-root-anything-colourfy-hits)
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

(defvar project-root-anything-config-bookmarks
  '((name . "Project Bookmarks")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-default-directory (cdr project-details)
                    anything-project-root project-details)))
    (candidates . (lambda ()
                    (mapcar
                     '(lambda (b)
                       (expand-file-name b anything-default-directory))
                     (project-root-bookmarks anything-project-root))))
    (type . file)))

(defun project-root-file-find-process (pattern)
  "Return a process which represents a find of all files matching
`project-root-extra-find-args' and the hard-coded arguments in
this function."
  (when anything-project-root
      (start-process-shell-command
       "project-root-find"
       nil
       "find"
       (cdr anything-project-root)
       (find-to-string
        `(and ,project-root-extra-find-args
              (name ,(concat "*" pattern "*"))
              (type "f"))))))

(provide 'project-root)

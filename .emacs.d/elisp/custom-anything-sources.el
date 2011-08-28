;;
;; Some parts from http://www.emacswiki.org/emacs/AnythingSources
;;

(defvar local-projects-dir "/Users/ian/Projects")

;;
;; On my laptop, projects are based out of ~/Projects, so if I have a
;; file open in one of those directories I'd like to search the whole
;; project directory.
;;
(defun get-local-project-basedir (path)
  "Given a path to something in ~/Projects, returns the project base."
  (let ((parts (split-string path "/")))
    ;; I know, I know.. should recursive, right? I'm lazy. Sorry.
    (setq base nil)
    (while (and (> (safe-length parts) 1) (equal base nil))
      (if (string-equal (mapconcat 'identity (butlast parts) "/")
                        local-projects-dir)
          (setq base (mapconcat 'identity parts "/")))
      (setq parts (butlast parts)))
    base))

(defun my-get-source-directory (path)
  (if (file-directory-p local-projects-dir)
      (get-local-project-basedir path)
    (file-name-directory path)))

(defvar my-anything-c-source-file-search
  '((name . "File Search")
    (init . (lambda ()
              (setq anything-default-directory
                    default-directory)))
    (candidates . (lambda ()
                    (let ((args
                           (format "'%s' \\( -path \\*/.svn \\) -prune -o -iregex '.*%s.*' -print"
                                   (my-get-source-directory anything-default-directory)
                                   anything-pattern)))
                      (start-process-shell-command "file-search-process" nil
                                                   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")

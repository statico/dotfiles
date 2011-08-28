;;
;; Some parts from http://www.emacswiki.org/emacs/AnythingSources
;;

(defun anything-project-root-find-files (pattern)
  (when anything-project-root
    (start-process-shell-command "project-root-find"
                                 nil
                                 "find"
                                 anything-project-root
                                 (find-to-string
                                  `(and (prune (name "*.svn" "*.git"))
                                        (name ,(concat "*" pattern "*"))
                                        (type "f"))))))

(defvar anything-c-source-project-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details (project-root-fetch))
              (setq anything-project-root (cdr project-details))))
    (candidates . (lambda ()
                    (anything-project-root-find-files anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

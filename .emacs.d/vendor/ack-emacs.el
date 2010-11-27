(require 'compile)
(require 'thingatpt)

(defvar ack-command "c:/utils/ack.pl" "The command run by the ack function.")

(defvar ack-mode-font-lock-keywords
  '(("^\\(Compilation\\|Ack\\) started.*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t))))

(defvar ack-use-search-in-buffer-name t
  "If non-nil, use the search string in the ack buffer's name.")

(define-compilation-mode ack-mode "Ack"
  "Specialization of compilation-mode for use with ack."
  nil)

(defun ack (dir pattern args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive (list (read-file-name "Run ack in directory: " nil "" t)
                     (read-string "Search for: " (thing-at-point 'symbol))
                     (read-string "Ack arguments: " nil nil "-i" nil)
                                  ))
  ; Get dir into an the right state, incase a file name was used
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "ack needs a directory: %s" dir))

  (let (compile-command
        (compilation-error-regexp-alist grep-regexp-alist)
        (compilation-directory default-directory)
        (ack-full-buffer-name (concat "*ack-" pattern "*")))
;;    (save-some-buffers (not compilation-ask-about-save) nil)
    ;; lambda defined here since compilation-start expects to call a function to get the buffer name
    (compilation-start (concat ack-command " " args " " pattern " \"" dir "\"") 'ack-mode
                       (when ack-use-search-in-buffer-name
                         (function (lambda (ignore)
                                     ack-full-buffer-name)))
                       (regexp-quote pattern))))

(provide 'ack-emacs)


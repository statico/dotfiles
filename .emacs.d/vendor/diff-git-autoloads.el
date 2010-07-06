;;; diff-git-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (diff-git-default-bindings diff-git-diff-unstaged
;;;;;;  diff-git-diff-staged diff-git-buffer-stage diff-git-hunk-stage)
;;;;;;  "diff-git" "diff-git.el" (19505 36504))
;;; Generated autoloads from diff-git.el

(autoload (quote diff-git-hunk-stage) "diff-git" "\
Stage the current hunk in the index using 'git apply --cached'.

\(fn)" t nil)

(autoload (quote diff-git-buffer-stage) "diff-git" "\
Stage the all the hunks in the current `diff-mode' buffer using 'git apply --cached'.

\(fn)" t nil)

(autoload (quote diff-git-diff-staged) "diff-git" "\
Show the diff of the index and HEAD.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-staged*.

\(fn &optional BUF)" t nil)

(autoload (quote diff-git-diff-unstaged) "diff-git" "\
Show the diff of the working tree and the index.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-unstaged*.

\(fn &optional BUF)" t nil)

(autoload (quote diff-git-default-bindings) "diff-git" "\
Add bindings to the `diff-mode' keymap.

\(fn)" nil nil)

(eval-after-load (quote diff-mode) (quote (diff-git-default-bindings)))

(eval-after-load (quote vc-mode) (quote (diff-git-default-bindings)))

;;;***

;;;### (autoloads nil nil ("diff-git-pkg.el") (19505 36504 629718))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; diff-git-autoloads.el ends here

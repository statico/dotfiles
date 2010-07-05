;;; diff-git.el --- Git integration with diff-mode

;; Copyright (C) 2009 by Alan Falloon

;; Author: Alan Falloon <alan.falloon@gmail.com>
;; Keywords: tools convenience patch diff vc git
;; Version: 0.1.1
;; URL: http://github.com/alanfalloon/diff-git.el/raw/master/diff-git.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds commands for working with the Git index from
;; within `diff-mode'.
;;
;; When splitting up commits and other fine juggling of staged and
;; unstaged changes, `diff-mode' offers a number of advantages:
;; specifically `diff-reverse-direction' and `diff-split-hunk' are
;; extremely useful for teasing apart changes.
;;
;; The major commands are: `diff-git-hunk-stage' which takes the hunk
;; under the point and stages it in the index; `diff-git-diff-staged'
;; which makes a new diff buffer showing the staged changes in the
;; file; and `diff-git-diff-unstaged' which shows the unstaged
;; changes.
;;
;; The staged and unstaged diff buffers are refreshed when
;; `diff-git-hunk-stage' is run, or when the file they are tracking is
;; saved.
;;
;; This extension was written and tested on GNU Emacs 23.0.91.1. To
;; use this extension you will require magit 0.7 and vc-git (included
;; with Emacs)

;;; History:

;;; Code:

(eval-and-compile
  (defvar diff-mode-map)
  (defvar diff-mode-shared-map)
  (defvar vc-prefix-map)
  (autoload 'diff-mode-map                   "diff-mode")
  (autoload 'diff-mode-shared-map            "diff-mode")
  (autoload 'diff-beginning-of-hunk          "diff-mode")
  (autoload 'diff-end-of-hunk                "diff-mode")
  (autoload 'diff-beginning-of-file-and-junk "diff-mode")
  (autoload 'diff-hunk-next                  "diff-mode")
  (autoload 'vc-deduce-fileset               "vc")
  (autoload 'vc-prefix-map                   "vc-hooks")
  (autoload 'vc-setup-buffer                 "vc-dispatcher")
  (autoload 'magit-run-git-with-input        "magit"))

(defvar diff-git-status-changed-hook nil
  "List of functions to be called after the git status is changed.
It is only triggered by diff-git commands that affect the status in some way.")

(defvar diff-git-update-function nil
  "The function to call to update the current buffer.")
(make-variable-buffer-local 'diff-git-update-function)

(defvar diff-git-update-buffers-list nil
  "Buffers that need updating when the Git repository changes.")

;;;###autoload
(defun diff-git-hunk-stage ()
  "Stage the current hunk in the index using 'git apply --cached'."
  (interactive)
  (let ((diff
         (let* ((hunk-beg (save-excursion (diff-beginning-of-hunk 'try-harder) (point)))
                (hunk-end (save-excursion (diff-end-of-hunk) (point)))
                (file-beg (save-excursion (diff-beginning-of-file-and-junk) (point)))
                (file-end (save-excursion (goto-char file-beg) (diff-hunk-next) (point))))
           (concat
            (buffer-substring-no-properties file-beg file-end)
            (buffer-substring-no-properties hunk-beg hunk-end))))
        (tmp (get-buffer-create "*magit-tmp*")))
    (with-current-buffer tmp
      (erase-buffer)
      (insert diff))
    (prog1 (magit-run-git-with-input tmp "apply" "--unidiff-zero" "--cached" "-")
      (run-hooks 'diff-git-status-changed-hook))))

;;;###autoload
(defun diff-git-buffer-stage ()
  "Stage the all the hunks in the current `diff-mode' buffer using 'git apply --cached'."
  (interactive)
  (prog1 (magit-run-git-with-input (current-buffer) "apply" "--unidiff-zero" "--cached" "-")
         (run-hooks 'diff-git-status-changed-hook)))

;;;###autoload
(defun diff-git-diff-staged (&optional buf)
  "Show the diff of the index and HEAD.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-staged*."
  (interactive)
  (prog1
      (diff-git-do-diff-command (or buf "*vc-diff-staged*") (not buf)
                                "--no-color" "--exit-code" "--cached" "--")
    (setq diff-git-update-function 'diff-git-diff-staged)))

;;;###autoload
(defun diff-git-diff-unstaged (&optional buf)
  "Show the diff of the working tree and the index.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-unstaged*."
  (interactive)
  (prog1
      (diff-git-do-diff-command (or buf "*vc-diff-unstaged*") (not buf)
                                "--no-color" "--exit-code" "--")
    (setq diff-git-update-function 'diff-git-diff-unstaged)))

(defun diff-git-do-diff-command (buffer pop &rest flags)
  "Run a git diff command in a `diff-mode' buffer.
BUFFER is the buffer that will hold the diff output.
POP determines if we should pop to the beffer after the command.
Optional argument FLAGS is the options to pass to git-diff."
  (let ((files (cadr (vc-deduce-fileset)))
        (oldpt (with-current-buffer
                (get-buffer-create buffer)
                (save-excursion
                 (ignore-errors
                  (diff-beginning-of-hunk 'try-harder))
                 (point)))))
    (vc-setup-buffer buffer)
    (apply 'vc-git-command buffer 1 files "diff" flags)
    (set-buffer buffer)
    (if (zerop (buffer-size))
        (progn (message "%s empty diff" buffer) nil)
      (diff-mode)
      (setq buffer-read-only t)
      (goto-char oldpt)
      (add-hook 'diff-git-status-changed-hook 'diff-git-update-buffers)
      (add-hook 'kill-buffer-hook 'diff-git-prune-update-buffers-list)
      (add-to-list 'diff-git-update-buffers-list (current-buffer))
      (dolist (file files)
        (let ((fbuf (get-file-buffer file)))
          (when fbuf
            (with-current-buffer fbuf
              (add-hook 'after-save-hook 'diff-git-update-buffers nil t)))))
      (when pop (pop-to-buffer (current-buffer)))
      t)))

(defun diff-git-update-buffers ()
  "Update all the buffers in `diff-git-update-buffers-list'."
  (dolist (buf diff-git-update-buffers-list)
    (with-current-buffer buf (diff-git-update-current-buffer))))

(defun diff-git-update-current-buffer ()
  "Update the current buffer using local `diff-git-update-function'."
  (interactive)
  (funcall diff-git-update-function (current-buffer)))

(defun diff-git-prune-update-buffers-list ()
  "Remove the current buffer from `diff-git-update-buffers-list'."
  (setq diff-git-update-buffers-list
        (delq (current-buffer) diff-git-update-buffers-list)))

;;;###autoload
(defun diff-git-default-bindings ()
  "Add bindings to the `diff-mode' keymap."
  (define-key vc-prefix-map "["        'diff-git-diff-unstaged)
  (define-key vc-prefix-map "]"        'diff-git-diff-staged)
  (define-key diff-mode-shared-map "g" 'diff-git-update-current-buffer)
  (define-key diff-mode-map "\C-c\M-v" 'diff-git-buffer-stage)
  (define-key diff-mode-map "\C-c\C-v" 'diff-git-hunk-stage))

;;;###autoload
(eval-after-load 'diff-mode '(diff-git-default-bindings))
;;;###autoload
(eval-after-load 'vc-mode '(diff-git-default-bindings))

(provide 'diff-git)

;; Local Variables:
;; coding: utf-8
;; End:
;;; diff-git.el ends here

;;; icicles-cmd1.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd1.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sat Jun 26 07:14:05 2010 (-0700)
;;           By: dradams
;;     Update #: 21206
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd1.el
;; Keywords: extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `doremi', `easymenu',
;;   `el-swank-fuzzy', `ffap', `ffap-', `frame-cmds', `frame-fns',
;;   `fuzzy-match', `hexrgb', `icicles-face', `icicles-fn',
;;   `icicles-mcmd', `icicles-opt', `icicles-var', `kmacro',
;;   `levenshtein', `misc-fns', `mwheel', `pp', `pp+', `ring',
;;   `ring+', `strings', `thingatpt', `thingatpt+', `wid-edit',
;;   `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  Library `icicles-cmd2.el' is a continuation of
;;  this file (a single file for all top-level commands would be too
;;  large to upload to Emacs Wiki).
;;
;;  For commands to be used mainly in the minibuffer or buffer
;;  *Completions*, see `icicles-mcmd.el'.
;;
;;  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  If you use the byte-compiled version of this library,
;;  `icicles-cmd1.elc', in Emacs 23, then it must be byte-compiled
;;  using Emacs 23.  Otherwise, Icicles key completion (and perhaps
;;  other things?) will not work correctly.
;;
;;  Macros defined here:
;;
;;    `icicle-define-bookmark-command',
;;    `icicle-define-bookmark-command-1',
;;    `icicle-define-bookmark-other-window-command'.
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+)`clear-option', (+)`icicle-add-buffer-candidate',
;;    (+)`icicle-add-buffer-config',
;;    `icicle-add-entry-to-saved-completion-set',
;;    `icicle-bbdb-complete-name', (+)`icicle-bookmark',
;;    (+)`icicle-bookmark-all-tags',
;;    (+)`icicle-bookmark-all-tags-other-window',
;;    (+)`icicle-bookmark-all-tags-regexp',
;;    (+)`icicle-bookmark-all-tags-regexp-other-window',
;;    (+)`icicle-bookmark-bookmark-list',
;;    `icicle-bookmark-bookmark-list-narrow',
;;    (+)`icicle-bookmark-cmd', (+)`icicle-bookmark-desktop',
;;    `icicle-bookmark-desktop-narrow', (+)`icicle-bookmark-dired',
;;    (+)`icicle-bookmark-dired-other-window',
;;    `icicle-bookmark-dired-narrow',
;;    (+)`icicle-bookmarked-buffer-list',
;;    (+)`icicle-bookmarked-file-list', (+)`icicle-bookmark-file',
;;    (+)`icicle-bookmark-file-other-window',
;;    `icicle-bookmark-file-narrow', (+)`icicle-bookmark-gnus',
;;    (+)`icicle-bookmark-gnus-other-window',
;;    `icicle-bookmark-gnus-narrow',
;;    (+)`icicle-bookmark-info-other-window',
;;    `icicle-bookmark-info-narrow', `icicle-bookmark-jump',
;;    `icicle-bookmark-jump-other-window', (+)`icicle-bookmark-list',
;;    (+)`icicle-bookmark-local-file',
;;    (+)`icicle-bookmark-local-file-other-window',
;;    `icicle-bookmark-local-file-narrow', (+)`icicle-bookmark-man',
;;    (+)`icicle-bookmark-man-other-window',
;;    `icicle-bookmark-man-narrow', (+)`icicle-bookmark-non-file',
;;    (+)`icicle-bookmark-non-file-other-window',
;;    `icicle-bookmark-non-file-narrow',
;;    (+)`icicle-bookmark-other-window', (+)`icicle-bookmark-region',
;;    (+)`icicle-bookmark-region-other-window',
;;    `icicle-bookmark-region-narrow',
;;    (+)`icicle-bookmark-remote-file',
;;    (+)`icicle-bookmark-remote-file-other-window',
;;    `icicle-bookmark-remote-file-narrow', `icicle-bookmark-set',
;;    (+)`icicle-bookmark-some-tags',
;;    (+)`icicle-bookmark-some-tags-other-window',
;;    (+)`icicle-bookmark-some-tags-regexp',
;;    (+)`icicle-bookmark-some-tags-regexp-other-window',
;;    (+)`icicle-bookmark-specific-buffers',
;;    `icicle-bookmark-specific-buffers-narrow',
;;    (+)`icicle-bookmark-specific-buffers-other-window',
;;    (+)`icicle-bookmark-specific-files',
;;    `icicle-bookmark-specific-files-narrow',
;;    (+)`icicle-bookmark-specific-files-other-window',
;;    (+)`icicle-bookmark-this-buffer',
;;    `icicle-bookmark-this-buffer-narrow',
;;    (+)`icicle-bookmark-this-buffer-other-window',
;;    (+)`icicle-bookmark-w3m-other-window',
;;    `icicle-bookmark-w3m-narrow', (+)`icicle-buffer',
;;    (+)`icicle-buffer-config', (+)`icicle-buffer-list',
;;    (+)`icicle-buffer-other-window', `icicle-cd-for-abs-files',
;;    `icicle-cd-for-loc-files', (+)`icicle-clear-history',
;;    (+)`icicle-clear-current-history', (+)`icicle-color-theme',
;;    `icicle-comint-dynamic-complete',
;;    `icicle-comint-dynamic-complete-filename',
;;    `icicle-comint-replace-by-expanded-filename',
;;    (+)`icicle-command-abbrev', (+)`icicle-command-abbrev-command',
;;    (+)`icicle-completing-yank', `icicle-customize-apropos',
;;    `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-customize-apropos-options-of-type',
;;    (+)`icicle-customize-face',
;;    (+)`icicle-customize-face-other-window',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    (+)`icicle-delete-file', (+)`icicle-delete-window',
;;    (+)`icicle-delete-windows', (+)`icicle-directory-list',
;;    (+)`icicle-dired', `icicle-dired-chosen-files',
;;    `icicle-dired-chosen-files-other-window',
;;    (+)`icicle-dired-other-window', `icicle-dired-project',
;;    `icicle-dired-project-other-window',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    `icicle-dired-save-marked',
;;    `icicle-dired-save-marked-as-project',
;;    `icicle-dired-save-marked-more',
;;    `icicle-dired-save-marked-persistently',
;;    `icicle-dired-save-marked-to-variable',
;;    `icicle-doremi-increment-variable+',
;;    `icicle-ess-complete-filename',
;;    `icicle-ess-complete-object-name',
;;    `icicle-ess-internal-complete-object-name',
;;    `icicle-ess-R-complete-object-name',
;;    (+)`icicle-execute-extended-command',
;;    (+)`icicle-execute-named-keyboard-macro', (+)`icicle-face-list',
;;    (+)`icicle-file', (+)`icicle-file-list',
;;    (+)`icicle-file-other-window', (+)`icicle-find-file',
;;    (+)`icicle-find-file-absolute',
;;    (+)`icicle-find-file-absolute-other-window',
;;    (+)`icicle-find-file-in-tags-table',
;;    (+)`icicle-find-file-in-tags-table-other-window',
;;    (+)`icicle-find-file-other-window',
;;    (+)`icicle-find-file-read-only',
;;    (+)`icicle-find-file-read-only-other-window',
;;    (+)`icicle-find-first-tag',
;;    (+)`icicle-find-first-tag-other-window', (+)`icicle-find-tag',
;;    `icicle-grep-saved-file-candidates',
;;    `icicle-gud-gdb-complete-command', (+)`icicle-increment-option',
;;    (+)`icicle-increment-variable', (+)`icicle-insert-buffer',
;;    (+)`icicle-kill-buffer', (+)`icicle-kmacro',
;;    `icicle-lisp-complete-symbol', (+)`icicle-locate-file',
;;    (+)`icicle-locate-file-no-symlinks',
;;    (+)`icicle-locate-file-no-symlinks-other-window',
;;    (+)`icicle-locate-file-other-window',
;;    (+)`icicle-other-window-or-frame', `icicle-pop-tag-mark',
;;    `icicle-pp-eval-expression', (+)`icicle-recent-file',
;;    (+)`icicle-recent-file-other-window',
;;    `icicle-recompute-shell-command-candidates',
;;    (+)`icicle-remove-buffer-candidate',
;;    (+)`icicle-remove-buffer-config',
;;    `icicle-remove-entry-from-saved-completion-set',
;;    (+)`icicle-remove-file-from-recentf-list',
;;    (+)`icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command',
;;    (+)`icicle-reset-option-to-nil',
;;    (+)`icicle-select-bookmarked-region', (+)`icicle-select-frame',
;;    `icicle-select-frame-by-name', (+)`icicle-select-window',
;;    `icicle-select-window-by-name', `icicle-send-bug-report',
;;    (+)`icicle-set-option-to-t', `icicle-shell-command-on-file',
;;    `icicle-shell-dynamic-complete-command',
;;    `icicle-shell-dynamic-complete-environment-variable',
;;    `icicle-shell-dynamic-complete-filename',
;;    (+)`icicle-toggle-option', (+)`icicle-yank-maybe-completing',
;;    (+)`toggle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `custom-variable-p', `icicle-binary-option-p',
;;    `icicle-bookmark-cleanup', `icicle-bookmark-cleanup-on-quit',
;;    `icicle-bookmark-delete-action', `icicle-bookmark-help-string',
;;    `icicle-bookmark-jump-1', `icicle-clear-history-1',
;;    `icicle-clear-history-entry',
;;    `icicle-comint-dynamic-complete-as-filename',
;;    `icicle-comint-dynamic-simple-complete',
;;    `icicle-comint-replace-orig-completion-fns',
;;    `icicle-command-abbrev-action',
;;    `icicle-command-abbrev-matching-commands',
;;    `icicle-command-abbrev-record', `icicle-command-abbrev-regexp',
;;    `icicle-customize-faces', `icicle-dabbrev--abbrev-at-point',
;;    `icicle-default-buffer-names',
;;    `icicle-delete-file-or-directory',
;;    `icicle-execute-extended-command-1', `icicle-explore',
;;    `icicle-find-first-tag-action',
;;    `icicle-find-first-tag-other-window-action',
;;    `icicle-find-tag-action', `icicle-find-tag-define-candidates',
;;    `icicle-find-tag-define-candidates-1',
;;    `icicle-find-tag-final-act', `icicle-find-tag-help',
;;    `icicle-find-tag-quit-or-error', `icicle-insert-for-yank',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', (+)`icicle-locate-file-1',
;;    `icicle-locate-file-action',
;;    `icicle-locate-file-other-window-action',
;;    `icicle-make-file+date-candidate', `icicle-make-frame-alist',
;;    `icicle-make-window-alist',
;;    `icicle-bookmark-propertize-candidate',
;;    `icicle-pp-display-expression',
;;    `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-from-recentf-candidate-action',
;;    `icicle-remove-saved-set-action',
;;    `icicle-shell-dynamic-complete-as-command',
;;    `icicle-shell-dynamic-complete-as-environment-variable'.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `bbdb-com.el' have
;;              been REDEFINED HERE:
;;              (BBDB is available here: http://bbdb.sourceforge.net/.)
;;
;;  `bbdb-complete-name' - Use Icicles minibuffer completion when there
;;                         are multiple candidates.
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;  `customize-face', `customize-face-other-window' - Multi-commands.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in
;;  `icicles-doc2.el'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Icicles Top-Level Commands, Part 1")
 
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

(eval-when-compile (require 'cl)) ;; pushnew
                                  ;; plus, for Emacs < 21: dolist, push
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode
(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--minibuffer-origin,
  ;; dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-completion-addsuffix, comint-completion-autolist, comint-completion-fignore,
  ;; comint-completion-recexact, comint-directory, comint-dynamic-complete-filename,
  ;; comint-dynamic-complete-functions, comint-line-beginning-position,
  ;; comint-match-partial-filename, comint-quote-filename
(eval-when-compile (require 'shell)) ;; shell-backward-command, shell-completion-execonly,
  ;; shell-dynamic-complete-command, shell-dynamic-complete-environment-variable,
  ;; shell-dynamic-complete-filename, shell-match-partial-variable
(eval-when-compile (require 'etags))
  ;; file-of-tag, find-tag, find-tag-default, find-tag-default-function,
  ;; find-tag-marker-ring, find-tag-other-window, goto-tag-location-function, snarf-tag-function,
  ;; tag-find-file-of-tag-noselect, tags-case-fold-search,
  ;; tags-lazy-completion-table, tags-table-files, visit-tags-table-buffer

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows
(eval-when-compile (require 'bbdb nil t) (require 'bbdb-com nil t)) ;; (no error if not found):
  ;; bbdb-auto-fill-function, bbdb-complete-name, bbdb-complete-name-allow-cycling,
  ;; bbdb-complete-name-cleanup, bbdb-complete-name-hooks, bbdb-completion-display-record,
  ;; bbdb-completion-predicate, bbdb-completion-type, bbdb-display-records-1,
  ;; bbdb-dwim-net-address, bbdb-expand-mail-aliases, bbdb-extract-address-components-func,
  ;; bbdb-gag-messages, bbdb-hashtable, bbdb-mapc, bbdb-pop-up-bbdb-buffer, bbdb-record-aka,
  ;; bbdb-record-name, bbdb-record-net, bbdb-search-intertwingle, bbdb-string-trim
(require 'cus-edit)
  ;; customize-apropos, customize-apropos-faces, customize-apropos-groups,
  ;; customize-apropos-options, custom-buffer-create, custom-buffer-order-groups, customize-face,
  ;; customize-face-other-window, custom-sort-items
(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on (my version)

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-define-command, icicle-define-file-command, icicle-define-add-to-alist-command
(require 'icicles-mcmd) ;; icicle-yank
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-candidates-alist,
  ;; icicle-completion-candidates, icicle-current-input, icicle-extra-candidates,
  ;; icicle-get-alist-candidate-function, icicle-incremental-completion-p, icicle-kmacro-alist,
  ;; icicle-must-match-regexp, icicle-must-not-match-regexp, icicle-must-pass-predicate,
  ;; icicle-re-no-dot, icicle-saved-completion-candidates
(require 'icicles-opt)
  ;; icicle-add-proxy-candidates-flag, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-buffer-ignore-space-prefix-flag, icicle-buffer-match-regexp,
  ;; icicle-buffer-no-match-regexp, icicle-buffer-predicate, icicle-buffer-require-match-flag,
  ;; icicle-buffer-sort, icicle-color-themes, icicle-saved-completion-sets,
  ;; icicle-sort-comparer, icicle-transform-function
(require 'icicles-fn)
  ;; icicle-assoc-delete-all, icicle-highlight-lighter, icicle-read-from-minibuf-nil-default



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 21)
  (defvar eval-expression-debug-on-error))

(when (< emacs-major-version 22)
  (defvar icicle-kmacro-alist)          ; In `icicles-var.el'
  (defvar kmacro-ring)                  ; In `kmacro.el'
  (defvar read-file-name-completion-ignore-case) ;  In `minibuffer.el'
  (defvar recentf-list)                 ; In `recentf.el'
  (defvar tags-case-fold-search))       ; In `etags.el'

(when (< emacs-major-version 23)
  (defvar read-buffer-completion-ignore-case))

(defvar bbdb-complete-name-allow-cycling) ; In `bbdb-com.el'
(defvar bbdb-extract-address-components-func) ; In `bbdb-com.el'
(defvar bbdb-expand-mail-aliases)       ; In `bbdb-com.el'
(defvar bbdb-complete-name-hooks)       ; In `bbdb-com.el'
(defvar bbdb-completion-display-record) ; In `bbdb.el'
(defvar bbdb-completion-type)           ; In `bbdb.el'
(defvar bbdb-hashtable)                 ; In `bbdb.el'
(defvar bmkp-prompt-for-tags-flag)      ; In `bookmark+.el'
(defvar color-theme)                    ; In `color-theme.el'
(defvar color-theme-initialized)        ; In `color-theme.el'
(defvar ess-current-process-name)       ; In `ess-inf.el'
(defvar ess-mode-syntax-table)          ; In `ess-cust.el'
(defvar ess-use-R-completion)           ; In `ess-cust.el'
(defvar filesets-data)                  ; In `filesets.el'
(defvar find-tag-default-function)      ; In `etags.el'
(defvar find-tag-marker-ring)           ; In `etags.el'
(defvar goto-tag-location-function)     ; In `etags.el'
(defvar icicle-clear-history-hist)      ; In `icicle-clear-history-1',`icicle-clear-current-history'
(defvar icicle-window-alist)            ; In `icicle-select-window'
(defvar shell-completion-execonly)      ; In `shell.el'
(defvar snarf-tag-function)             ; In `etags.el'
(defvar w3m-current-title)              ; In `w3m.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Icicles Top-Level Commands, Part 1")
;;; Icicles Top-Level Commands, Part 1 .   .   .   .   .   .   .   .   .


;; REPLACE ORIGINAL `pp-eval-expression' defined in `pp.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; This is essentially the same as `pp-eval-expression' defined in `pp+.el'.
;;
;; 1. Read with completion, using `icicle-read-expression-map'.
;; 2. Progress message added.
;; 3. Added optional arg and insertion behavior.
;; 4. Respect `icicle-pp-eval-expression-print-length', `icicle-pp-eval-expression-print-level',
;;    and `eval-expression-debug-on-error'.
;; 5. Adjusted to work in different Emacs releases.
;;
;;;###autoload
(defun icicle-pp-eval-expression (expression ; Bound to `M-:' in Icicle mode.
                                  &optional insert-value)
  "Evaluate Emacs-Lisp sexp EXPRESSION, and pretty-print its value.
Add the value to the front of the variable `values'.
With a prefix arg, insert the value into the current buffer at point.
 With a negative prefix arg, if the value is a string, then insert it
 into the buffer without double-quotes (`\"').
With no prefix arg:
 If the value fits on one line (frame width) show it in the echo area.
 Otherwise, show the value in buffer `*Pp Eval Output*'.

This command respects user options
`icicle-pp-eval-expression-print-length',
`icicle-pp-eval-expression-print-level', and
`eval-expression-debug-on-error'.

Emacs-Lisp mode completion and indentation bindings are in effect.

By default, Icicle mode remaps all key sequences that are normally
bound to `eval-expression' or `pp-eval-expression' to
`icicle-pp-eval-expression'.  If you do not want this remapping, then
customize option `icicle-top-level-key-bindings'."
  (interactive
   (list (read-from-minibuffer "Eval: " nil icicle-read-expression-map t 'read-expression-history)
         current-prefix-arg))
  (message "Evaluating...")
  (if (or (not (boundp 'eval-expression-debug-on-error))
          (null eval-expression-debug-on-error))
      (setq values  (cons (eval expression) values))
    (let ((old-value  (make-symbol "t"))
          new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evaled code changes it.
      (let ((debug-on-error  old-value))
        (setq values     (cons (eval expression) values)
              new-value  debug-on-error))
      ;; If evaled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
        (setq debug-on-error  new-value))))
  (let ((print-length  icicle-pp-eval-expression-print-length)
        (print-level   icicle-pp-eval-expression-print-level))
    (cond (insert-value
           (message "Evaluating...done. Value inserted.")
           (setq insert-value  (prefix-numeric-value insert-value))
           (if (or (not (stringp (car values))) (wholenump insert-value))
               (pp (car values) (current-buffer))
             (princ (car values) (current-buffer))))
          (t (icicle-pp-display-expression (car values) "*Pp Eval Output*")))))


;; REPLACE ORIGINAL in `pp.el':
;; 1. Use no `emacs-lisp-mode-hook' or `change-major-mode-hook'.
;; 2. Call `font-lock-fontify-buffer'.
;;
(defun icicle-pp-display-expression (expression out-buffer-name)
  "Prettify and show EXPRESSION in a way appropriate to its length.
If a temporary buffer is needed for representation, it is named
OUT-BUFFER-NAME."
  (let* ((old-show-function  temp-buffer-show-function)
         ;; Use this function to display the buffer.
         ;; This function either decides not to display it at all
         ;; or displays it in the usual way.
         (temp-buffer-show-function
          #'(lambda (buf)
              (with-current-buffer buf
                (goto-char (point-min))
                (end-of-line 1)
                (if (or (< (1+ (point)) (point-max))
                        (>= (- (point) (point-min)) (frame-width)))
                    (let ((temp-buffer-show-function  old-show-function)
                          (old-selected               (selected-window))
                          (window                     (display-buffer buf)))
                      (goto-char (point-min)) ; expected by some hooks ...
                      (make-frame-visible (window-frame window))
                      (unwind-protect
                           (progn (select-window window)
                                  (run-hooks 'temp-buffer-show-hook))
                        (select-window old-selected)
                        (message "Evaluating...done. See buffer `%s'."
                                 out-buffer-name)))
                  (message "%s" (buffer-substring (point-min) (point))))))))
    (with-output-to-temp-buffer out-buffer-name
      (pp expression)
      (with-current-buffer standard-output
        (setq buffer-read-only  nil)
        (let ((emacs-lisp-mode-hook    nil)
              (change-major-mode-hook  nil))
          (emacs-lisp-mode))
        (set (make-local-variable 'font-lock-verbose) nil)
        (font-lock-fontify-buffer)))))

;;;###autoload
(defun icicle-shell-command-on-file (file)
  "Read a shell command and invoke it, passing FILE as an argument."
  (dired-run-shell-command
   (dired-shell-stuff-it (icicle-read-shell-command (format "! on `%s': " file)) (list file) nil)))

;;;###autoload
(defun icicle-recompute-shell-command-candidates (&optional savep)
  "Update option `icicle-shell-command-candidates-cache'.
Recompute the available shell commands using your search path.
Return the new option value.
With a prefix argument, the updated option is saved persistently.

If the value of option `icicle-guess-commands-in-path' is not `load',
then turning on Icicle mode (again) resets the cache value to ().
If the value of `icicle-guess-commands-in-path' is `first-use', then
the cache is updated when you next use it, but it is not saved."
  (interactive "P")
  (setq icicle-shell-command-candidates-cache  (icicle-compute-shell-command-candidates))
  (when savep (funcall icicle-customize-save-variable-function
                       'icicle-shell-command-candidates-cache
                       icicle-shell-command-candidates-cache))
  icicle-shell-command-candidates-cache)


;; REPLACE ORIGINAL `comint-dynamic-complete' defined in `comint.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses Icicles completion when there are multiple candidates.
;;
;;;###autoload
(defun icicle-comint-dynamic-complete () ; Bound to `TAB' in Comint (and Shell) mode.
  "Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions', but with
Icicles functions substituted, to perform completion until a function
returns non-nil, at which point completion is assumed to have
occurred.

Uses Icicles completion."
  (interactive)
  ;; Need a symbol for `run-hook-with-args-until-success', so bind one.
  (let ((hook  (icicle-comint-replace-orig-completion-fns)))
    (run-hook-with-args-until-success 'hook)))

(defun icicle-comint-replace-orig-completion-fns ()
  "Return `comint-dynamic-complete-functions', but with Icicles functions."
  (let ((old  comint-dynamic-complete-functions)
        (new  ())
        pair)
    (dolist (fn  old)
      (if (setq pair  (assoc fn icicle-comint-dynamic-complete-replacements))
          (push (eval (cadr pair)) new)
        (push fn new)))
    (nreverse new)))

;;;###autoload
(defun icicle-comint-dynamic-complete-filename ()
  "Dynamically complete the filename at point.
Completes if after a filename.  See `comint-match-partial-filename' and
`icicle-comint-dynamic-complete-as-filename'.
This function is similar to `comint-replace-by-expanded-filename', except that
it won't change parts of the filename already entered in the buffer; it just
adds completion characters to the end of the filename.  A completions listing
may be shown in a help buffer if completion is ambiguous.

Completion is dependent on the value of `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore', and the timing of
completions listing is dependent on the value of `comint-completion-autolist'.

Returns t if successful.

Uses Icicles completion."
  (interactive)
  (when (comint-match-partial-filename)
    (unless (window-minibuffer-p (selected-window)) (message "Completing file name..."))
    (icicle-comint-dynamic-complete-as-filename)))

(defun icicle-comint-dynamic-complete-as-filename ()
  "Dynamically complete at point as a filename.
See `icicle-comint-dynamic-complete-filename'.
Returns t if successful."
  (let* ((completion-ignore-case         (if (boundp 'read-file-name-completion-ignore-case)
                                             read-file-name-completion-ignore-case
                                           (memq system-type '(ms-dos windows-nt cygwin))))
         (completion-ignored-extensions  comint-completion-fignore)
         (minibuffer-p                   (window-minibuffer-p (selected-window)))
         (success                        t)
         (dirsuffix                      (cond ((not comint-completion-addsuffix)         "")
                                               ((not (consp comint-completion-addsuffix)) "/")
                                               (t  (car comint-completion-addsuffix))))
         (filesuffix                     (cond ((not comint-completion-addsuffix)         "")
                                               ((not (consp comint-completion-addsuffix)) " ")
                                               (t  (cdr comint-completion-addsuffix))))
         (filename                       (comint-match-partial-filename))
         (filename-beg                   (if filename (match-beginning 0) (point)))
         (filename-end                   (if filename (match-end 0) (point)))
         (filename                       (or filename ""))
         (filedir                        (file-name-directory filename))
         (filenondir                     (file-name-nondirectory filename))
         (directory                      (if filedir (comint-directory filedir) default-directory))
         (completion                     (file-name-completion filenondir directory)))
    (cond ((null completion)
           (if minibuffer-p
               (minibuffer-message (format " [No completions of `%s']" filename))
             (message "No completions of `%s'" filename))
           (setq success  nil))
          ((eq completion t)            ; Already completed: "the-file".
           (insert filesuffix)
           (unless minibuffer-p (message "Sole completion")))
          ((string-equal completion "") ; A directory: "dir/" - complete it.
           (condition-case nil
               (let* ((icicle-show-Completions-initially-flag      t)
                      (icicle-incremental-completion-p             'display)
                      (icicle-top-level-when-sole-completion-flag  t)
                      (choice
                       (save-excursion
                         (save-window-excursion (read-file-name "Complete: " directory nil t)))))
                 (when (and choice (not (string= choice directory)))
                   (insert (comint-quote-filename
                            (file-name-nondirectory (directory-file-name choice))))
                   (insert (if (file-directory-p choice) dirsuffix filesuffix))))
             (error nil)))
          (t                            ; COMPLETION is the common prefix string.
           (let ((file            (concat (file-name-as-directory directory) completion))
                 (use-dialog-box  nil)) ; Inhibit use of open-file dialog box if called from menu.
             ;; Insert completion.  The completion string might have a different case from
             ;; what's in the prompt, if `read-file-name-completion-ignore-case' is non-nil.
             (delete-region filename-beg filename-end)
             (if filedir (insert (comint-quote-filename filedir)))
             (insert (comint-quote-filename (directory-file-name completion)))
             (cond ((symbolp (file-name-completion completion directory))
                    ;; We inserted a unique completion.  Add suffix.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p (message "Completed")))
                   ((and comint-completion-recexact comint-completion-addsuffix
                         (string-equal filenondir completion)
                         (file-exists-p file))
                    ;; It's not unique, but user wants shortest match.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p (message "Completed shortest")))
                   ((or comint-completion-autolist (string-equal filenondir completion))
                    (condition-case nil ; It's not unique.  Let user choose a completion.
                        (let* ((icicle-show-Completions-initially-flag      t)
                               (icicle-incremental-completion-p             'display)
                               (icicle-top-level-when-sole-completion-flag  t)
                               (choice
                                (save-excursion
                                  (save-window-excursion
                                    (read-file-name
                                     "Complete: " directory completion nil completion
                                     (and (> emacs-major-version 21)
                                          #'(lambda (f) (string-match completion f))))))))
                          (when choice
                            (delete-backward-char (length completion))
                            (insert (comint-quote-filename
                                     (file-name-nondirectory (directory-file-name choice))))
                            (insert (if (file-directory-p choice) dirsuffix filesuffix))))
                      (error nil)))
                   (t (unless minibuffer-p (message "Partially completed")))))))
    success))

;;;###autoload
(defun icicle-shell-dynamic-complete-command ()
  "Dynamically complete the command at point.
Similar to `icicle-comint-dynamic-complete-filename', but this
searches `exec-path' (minus the trailing Emacs library path) for
completion candidates.  Note that this may not be the same as the
shell's idea of the path.

Completion is dependent on the value of `shell-completion-execonly',
plus those that effect file completion.
See `icicle-shell-dynamic-complete-as-command'.

Returns t if successful.

Uses Icicles completion."
  (interactive)
  (let ((filename  (comint-match-partial-filename)))
    (if (and filename
             (save-match-data (not (string-match "[~/]" filename)))
             (eq (match-beginning 0) (save-excursion (shell-backward-command 1) (point))))
        (prog2 (unless (window-minibuffer-p (selected-window))
                 (message "Completing command name..."))
            (icicle-shell-dynamic-complete-as-command)))))

(defun icicle-shell-dynamic-complete-as-command ()
  "Dynamically complete text at point as a command.
See `icicle-shell-dynamic-complete-filename'.
Return t if successful."
  (let* ((filename       (or (comint-match-partial-filename) ""))
         (filenondir     (file-name-nondirectory filename))
         (path-dirs      (cdr (reverse exec-path)))
         (cwd            (file-name-as-directory (expand-file-name default-directory)))
         (ignored-extensions
          (and comint-completion-fignore
               (mapconcat #'(lambda (x) (concat (regexp-quote x) "$"))
                          comint-completion-fignore "\\|")))
         (dir            "")
         (comps-in-dir   ())
         (file           "")
         (abs-file-name  "")
         (completions    ()))
    (while path-dirs                    ; Go thru each dir in the search path, finding completions.
      (setq dir           (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
            comps-in-dir  (and (file-accessible-directory-p dir)
                               (file-name-all-completions filenondir dir)))
      (while comps-in-dir               ; Go thru each completion, to see whether it should be used.
        (setq file           (car comps-in-dir)
              abs-file-name  (concat dir file))
        (when (and (not (member file completions))
                   (not (and ignored-extensions (string-match ignored-extensions file)))
                   (or (string-equal dir cwd) (not (file-directory-p abs-file-name)))
                   (or (null shell-completion-execonly) (file-executable-p abs-file-name)))
          (setq completions  (cons file completions)))
        (setq comps-in-dir  (cdr comps-in-dir)))
      (setq path-dirs  (cdr path-dirs)))
    (let ((success  (let ((comint-completion-addsuffix  nil)
                          (icicle-candidate-help-fn
                           #'(lambda (cand)
                               (shell-command (concat "apropos " (shell-quote-argument cand))
                                              "*Help*"))))
                      (icicle-comint-dynamic-simple-complete filenondir completions))))
      (when (and (memq success '(sole shortest)) comint-completion-addsuffix
                 (not (file-directory-p (comint-match-partial-filename))))
        (insert " "))
      success)))

;;;###autoload
(defun icicle-comint-replace-by-expanded-filename ()
  "`comint-replace-by-expanded-filename', but uses Icicles completion.
Dynamically complete, expand, and canonicalize the filename at point."
  (interactive)
  (let ((filename  (comint-match-partial-filename)))
    (when filename
      (replace-match (expand-file-name filename) t t)
      (icicle-comint-dynamic-complete-filename))))

(defun icicle-comint-dynamic-simple-complete (stub candidates)
  "Dynamically complete STUB from CANDIDATES list.
Inserts completion characters at point by completing STUB from the
strings in CANDIDATES.  Uses Icicles completion if completion is
ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the completion matches.
Returns `partial' if completed as far as possible with the completion matches.
Returns `listed' if a completion listing was shown.

See also `icicle-comint-dynamic-complete-filename'."
  (let* ((completion-ignore-case  (memq system-type '(ms-dos windows-nt cygwin)))
         (minibuffer-p            (window-minibuffer-p (selected-window)))
         (suffix                  (cond ((not comint-completion-addsuffix)         "")
                                        ((not (consp comint-completion-addsuffix)) " ")
                                        (t  (cdr comint-completion-addsuffix))))
         (candidates              (mapcar #'list candidates))
         (completions             (all-completions stub candidates)))
    (cond ((null completions)
           (if minibuffer-p
               (minibuffer-message (format " [No completions of `%s']" stub))
             (message "No completions of `%s'" stub))
           nil)
          ((= 1 (length completions))
           (let ((completion  (car completions)))
             (if (string-equal completion stub)
                 (unless minibuffer-p (message "Sole completion"))
               (insert (substring completion (length stub)))
               (unless minibuffer-p (message "Completed")))
             (insert suffix)
             'sole))
          (t                            ; There's no unique completion.
           (let ((completion  (try-completion stub candidates)))
             ;; Insert the longest substring.
             (insert (substring completion (length stub)))
             (cond ((and comint-completion-recexact comint-completion-addsuffix
                         (string-equal stub completion)
                         (member completion completions))
                    (insert suffix)     ; User wants shortest match.
                    (unless minibuffer-p (message "Completed shortest"))
                    'shortest)
                   ((or comint-completion-autolist (string-equal stub completion))
                    (condition-case nil ;  Let user choose a completion.
                        (let* ((icicle-show-Completions-initially-flag      t)
                               (icicle-incremental-completion-p             'display)
                               (icicle-top-level-when-sole-completion-flag  t)
                               (choice (save-excursion
                                         (completing-read "Complete: " (mapcar #'list completions)
                                                          nil t nil nil completion))))
                          (when choice
                            (delete-backward-char (length completion))
                            (insert choice suffix)))
                      (error nil))
                    'listed)
                   (t
                    (unless minibuffer-p (message "Partially completed"))
                    'partial)))))))

;;;###autoload
(defun icicle-shell-dynamic-complete-filename ()
  "Dynamically complete the filename at point.
Completes only if point is at a suitable position for a filename
argument."
  (interactive)
  (let ((opoint  (point))
        (beg     (comint-line-beginning-position)))
    (when (save-excursion
            (goto-char (if (re-search-backward "[;|&]" beg t) (match-end 0) beg))
            (re-search-forward "[^ \t][ \t]" opoint t))
      (icicle-comint-dynamic-complete-as-filename))))

;;;###autoload
(defun icicle-shell-dynamic-complete-environment-variable ()
  "`shell-dynamic-complete-environment-variable' but uses Icicles completion."
  (interactive)
  (require 'shell)
  (let ((variable  (shell-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
        (prog2 (unless (window-minibuffer-p (selected-window))
                 (message "Completing variable name..."))
            (icicle-shell-dynamic-complete-as-environment-variable)))))

(defun icicle-shell-dynamic-complete-as-environment-variable ()
  "`shell-dynamic-complete-as-environment-variable' but uses Icicles completion."
  (require 'shell)
  (let* ((var                          (or (shell-match-partial-variable) ""))
         (variable                     (substring var (or (string-match "[^$({]\\|$" var) 0)))
         (variables                    (mapcar #'(lambda (x) (substring x 0 (string-match "=" x)))
                                               process-environment))
         (addsuffix                    comint-completion-addsuffix)
         (comint-completion-addsuffix  nil)
         (success                      (icicle-comint-dynamic-simple-complete variable variables)))
    (if (memq success '(sole shortest))
        (let* ((var           (shell-match-partial-variable))
               (variable      (substring var (string-match "[^$({]" var)))
               (protection    (cond ((string-match "{" var) "}")
                                    ((string-match "(" var) ")")
                                    (t "")))
               (suffix        (cond ((null addsuffix) "")
                                    ((file-directory-p
                                      (comint-directory (getenv variable))) "/")
                                    (t " "))))
          (insert protection  suffix)))
    success))

;;;###autoload
(defun icicle-ess-complete-object-name (&optional listcomp)
  "`ess-complete-object-name', but uses Icicles completion.
Complete `ess-language' object preceding point.
This is `icicle-ess-R-complete-object-name' if `ess-use-R-completion',
and `icicle-ess-internal-complete-object-name' otherwise."
  (interactive "P")
  (if ess-use-R-completion
      (icicle-ess-R-complete-object-name)
    (icicle-ess-internal-complete-object-name listcomp)))

;;;###autoload
(defun icicle-ess-internal-complete-object-name (&optional listcomp)
  "`ess-internal-complete-object-name', but uses Icicles completion.
Complete `ess-language' object preceding point."
  (interactive "P")
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix  nil)
             (end                          (point))
             (buffer-syntax                (syntax-table))
             (beg                          (unwind-protect
                                                (save-excursion
                                                  (set-syntax-table ess-mode-syntax-table)
                                                  (backward-sexp 1)
                                                  (point))
                                             (set-syntax-table buffer-syntax)))
             (full-prefix                  (buffer-substring beg end))
             (pattern                      full-prefix)
             (listname                  ; See if we're indexing a list with `$'
              (and (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$" full-prefix)
                   (setq pattern  (if (not (match-beginning 2))
                                      ""
                                    (substring full-prefix (match-beginning 2) (match-end 2))))
                   (substring full-prefix (match-beginning 1) (match-end 1))))
             (classname                 ; Are we trying to get a slot via `@' ?
              (and (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$" full-prefix)
                   (setq pattern  (if (not (match-beginning 2))
                                      ""
                                    (substring full-prefix (match-beginning 2) (match-end 2))))
                   (progn (ess-write-to-dribble-buffer (format "(ess-C-O-Name : slots..) : patt=%s"
                                                               pattern))
                          (substring full-prefix (match-beginning 1) (match-end 1)))))
             (components
              (if listname
                  (ess-object-names listname)
                (if classname
                    (ess-slot-names classname)
                  ;; Default case: It hangs here when options (error=recoves):
                  (ess-get-object-list ess-current-process-name)))))
        ;; Return non-nil to prevent history expansions
        (or (icicle-comint-dynamic-simple-complete  pattern components) 'none))))

(defun icicle-ess-complete-filename ()
  "`ess-complete-filename', but uses Icicles completion.
Do file completion only within strings, or when `!' call is used."
  (if (comint-within-quotes
       (1- (process-mark (get-buffer-process (current-buffer)))) (point))
      (progn (if (featurep 'xemacs)
                 (icicle-comint-dynamic-complete-filename) ; Work around XEmacs bug.  GNU Emacs and
               (icicle-comint-replace-by-expanded-filename)) ; a working XEmacs return t in a string
             t)))

;;;###autoload
(defun icicle-ess-R-complete-object-name ()
  "`ess-R-complete-object-name', but uses Icicles completion.
Completion in R."
  (interactive)
  (ess-make-buffer-current)
  (let* ((comint-completion-addsuffix  nil)
         (beg-of-line                  (save-excursion (comint-bol nil) (point)))
         (end-of-line                  (point-at-eol))
         (line-buffer                  (buffer-substring beg-of-line end-of-line))
         (NS                           (if (ess-current-R-at-least '2.7.0)
                                           "utils:::"
                                         "rcompgen:::"))
         (token-string                  ; Setup, including computation of the token
          (progn
            (ess-command (format (concat NS ".assignLinebuffer('%s')\n") line-buffer))
            (ess-command (format (concat NS ".assignEnd(%d)\n") (- (point) beg-of-line)))
            (car (ess-get-words-from-vector (concat NS ".guessTokenFromLine()\n")))))
         (possible-completions          ; Compute and retrieve possible completions
          (progn
            (ess-command (concat NS ".completeToken()\n"))
            (ess-get-words-from-vector (concat NS ".retrieveCompletions()\n")))))
    (or (icicle-comint-dynamic-simple-complete token-string possible-completions) 'none)))

;;;###autoload
(defun icicle-gud-gdb-complete-command (&optional command a b)
  "`gud-gdb-complete-command', but uses Icicles completion.
Perform completion on the GDB command preceding point."
  (interactive)
  (if command
      (setq command  (concat "p " command)) ; Used by gud-watch in mini-buffer.
    (let ((end  (point)))               ; Used in GUD buffer.
      (setq command  (buffer-substring (comint-line-beginning-position) end))))
  (let* ((command-word
          ;; Find the word break.  This match will always succeed.
          (and (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
               (substring command (match-beginning 2))))
         (complete-list
          (gud-gdb-run-command-fetch-lines (concat "complete " command)
                                           (current-buffer)
                                           ;; From string-match above.
                                           (match-beginning 2))))
    ;; Protect against old versions of GDB.
    (and complete-list
         (string-match "^Undefined command: \"complete\"" (car complete-list))
         (error "This version of GDB doesn't support the `complete' command"))
    ;; Sort the list like readline.
    (setq complete-list  (sort complete-list (function string-lessp)))
    ;; Remove duplicates.
    (let ((first   complete-list)
          (second  (cdr complete-list)))
      (while second
        (if (string-equal (car first) (car second))
            (setcdr first (setq second  (cdr second)))
          (setq first   second
                second  (cdr second)))))
    ;; Add a trailing single quote if there is a unique completion
    ;; and it contains an odd number of unquoted single quotes.
    (and (= (length complete-list) 1)
         (let ((str    (car complete-list))
               (pos    0)
               (count  0))
           (while (string-match "\\([^'\\]\\|\\\\'\\)*'" str pos)
             (setq count  (1+ count)
                   pos    (match-end 0)))
           (and (= (mod count 2) 1)
                (setq complete-list  (list (concat str "'"))))))
    ;; Let comint handle the rest.
    (icicle-comint-dynamic-simple-complete command-word complete-list)))


;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; You can complete from an empty abbrev also.
;; Uses Icicles completion when there are multiple candidates.
;;
(when (and (fboundp 'dabbrev-completion) (not (fboundp 'old-dabbrev-completion)))
(defalias 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *ALL* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers  (and arg t)) ; Must be t
         (dabbrev-check-all-buffers    (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev                       (icicle-dabbrev--abbrev-at-point))
         (ignore-case-p                (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                                case-fold-search
                                              dabbrev-case-fold-search)
                                            (or (not dabbrev-upcase-means-case-search)
                                                (string= abbrev (downcase abbrev)))))
         (my-obarray                   dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init  (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation  abbrev)
        (let ((completion-list         (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case  ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray  (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string  completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (icicle-upcase abbrev))
                   (intern (icicle-upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (icicle-upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray            my-obarray
                dabbrev--last-completion-buffer  (current-buffer)
                ;; Find the expanded common string.
                init                             (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init  abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys)
;;                                         (not icicle-key-descriptions-use-<>-flag)))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (icicle-highlight-lighter)
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag  t)
                  (icicle-incremental-completion-p         'display)
                  (minibuffer-completion-table             my-obarray)
                  (choice
                   (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))

(defun icicle-dabbrev--abbrev-at-point ()
  "Like `dabbrev--abbrev-at-point', but returns \"\" if there is no match.
Vanilla `dabbrev--abbrev-at-point' raises an error if no match."
  (let ((abv ""))
    (setq dabbrev--last-abbrev-location  (point)) ; Record the end of the abbreviation.
    (unless (bobp)
      (save-excursion                   ; Return abbrev at point
        ;; If we aren't right after an abbreviation, move point back to just after one.
        ;; This is so the user can get successive words by typing the punctuation followed by M-/.
        (save-match-data
          (when (and (save-excursion
                       (forward-char -1)
                       (not (looking-at
                             (concat "\\(" (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_") "\\)+"))))
                     (re-search-backward (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_") nil t))
            (forward-char 1)))
        (dabbrev--goto-start-of-abbrev) ; Now find the beginning of that one.
        (setq abv  (buffer-substring-no-properties dabbrev--last-abbrev-location (point)))))
    abv))


;; REPLACE ORIGINAL `bbdb-complete-name' defined in `bbdb-com.el',
;; saving it for restoration when you toggle `icicle-mode'.
;; Note: BBDB, the Insidious Big Brother Database, is available here:
;;       http://bbdb.sourceforge.net/.
;;
;; Uses Icicles completion when there are multiple candidates.
;;
;; Free vars here: `bbdb-*' are bound in `bbdb-com.el'.
;;;###autoload
(defun icicle-bbdb-complete-name (&optional start-pos)
  "Complete the user full-name or net-address before point.
Completes only up to the preceding newline, colon, or comma, or the
value of START-POS.

If what has been typed is unique, insert an entry of the form \"User
Name <net-addr>\" (but see `bbdb-dwim-net-address-allow-redundancy').
If it is a valid completion but not unique, you can choose from the
list of completions using Icicles completion.

If your input is completed and `bbdb-complete-name-allow-cycling' is
true, then you can repeat to cycle through the nets for the matching
record.

When called with a prefix arg, display a list of all nets.  You can
control completion behaviour using `bbdb-completion-type'."
  (interactive)
  (unless (and (require 'bbdb nil t) (require 'bbdb-com nil t))
    (error "`icicle-bbdb-complete-name' requires BBDB"))
  (let* ((end                  (point))
         (beg                  (or start-pos (save-excursion (re-search-backward
                                                              "\\(\\`\\|[\n:,]\\)[ \t]*")
                                                             (goto-char (match-end 0)) (point))))
         (orig                 (buffer-substring beg end))
         (typed                (downcase orig))
         (pattern              (bbdb-string-trim typed))
         ;; DADAMS -
         ;; Replaced `(bbdb-hashtable)' by its expansion (bbdb-with-db-buffer ... bbdb-hashtable),
         ;; to avoid the silly macro altogether and simplify user byte-compiling a little.
         (ht                   (bbdb-with-db-buffer (bbdb-records nil t) bbdb-hashtable))
         ;; Make a list of possible completion strings (all-the-completions), and a flag to
         ;; indicate if there's a single matching record or not (only-one-p).
         (only-one-p           t)
         (all-the-completions  ())
         (pred
          #'(lambda (sym)
              (and (bbdb-completion-predicate sym)
                   (progn
                     (when (and only-one-p all-the-completions
                                (or
                                 ;; Not sure about this. More than one record attached to the symbol?
                                 ;; Does that happen?
                                 (> (length (symbol-value sym)) 1)
                                 ;; This is the doozy. Multiple syms which all match the same record.
                                 (delete t (mapcar #'(lambda (x)
                                                       (equal (symbol-value x) (symbol-value sym)))
                                                   all-the-completions))))
                       (setq only-one-p  nil))
                     (if (memq sym all-the-completions)
                         nil
                       (setq all-the-completions  (cons sym all-the-completions)))))))
         (completion           (progn (all-completions pattern ht pred)
                                      (try-completion pattern ht)))
         (exact-match          (eq completion t)))
    (cond
      ;; No matches found OR you're trying completion on an already-completed record.
      ;; In the latter case, we might have to cycle through the nets for that record.
      ((or (null completion)
           (and bbdb-complete-name-allow-cycling
                exact-match             ; Which is a net of the record
                (member orig (bbdb-record-net (car (symbol-value (intern-soft pattern ht)))))))
       (bbdb-complete-name-cleanup)     ; Clean up the completion buffer, if it exists
       (unless (catch 'bbdb-cycling-exit ; Check for cycling
                 ;; Jump straight out if we're not cycling
                 (unless bbdb-complete-name-allow-cycling (throw 'bbdb-cycling-exit nil))
                 ;; Find the record we're working on.
                 (let* ((addr  (funcall bbdb-extract-address-components-func orig))
                        (rec  (and (listp addr)
                                   ;; For now, we ignore the case where this returns more than
                                   ;; one record.  Ideally, the last expansion would be stored
                                   ;; in a buffer-local variable, perhaps.
                                   (car (bbdb-search-intertwingle (caar addr)
                                                                  (car (cdar addr)))))))
                   (unless rec (throw 'bbdb-cycling-exit nil))
                   (if current-prefix-arg
                       ;; Use completion buffer
                       (let ((standard-output  (get-buffer-create "*Completions*")))
                         ;; A previously existing buffer has to be cleaned first
                         (with-current-buffer standard-output
                           (setq buffer-read-only  nil)
                           (erase-buffer))
                         (display-completion-list
                          (mapcar #'(lambda (n) (bbdb-dwim-net-address rec n))
                                  (bbdb-record-net rec)))
                         (delete-region beg end)
                         (switch-to-buffer standard-output))
                     ;; Use next address
                     (let* ((addrs      (bbdb-record-net rec))
                            (this-addr  (or (cadr (member (car (cdar addr)) addrs))
                                            (nth 0 addrs))))
                       (if (= (length addrs) 1)
                           (throw 'bbdb-cycling-exit t) ; No alternatives. don't signal an error.
                         ;; Replace with new mail address
                         (delete-region beg end)
                         (insert (bbdb-dwim-net-address rec this-addr))
                         (run-hooks 'bbdb-complete-name-hooks)
                         (throw 'bbdb-cycling-exit t))))))
         ;; FALL THROUGH.  Check mail aliases
         (when (and (or (not bbdb-expand-mail-aliases) (not (expand-abbrev)))
                    bbdb-complete-name-hooks)
           (message "No completion for `%s'" pattern) (icicle-ding)))) ; no matches

      ;; Match for a single record. If cycling is enabled then we don't
      ;; care too much about the exact-match part.
      ((and only-one-p (or exact-match bbdb-complete-name-allow-cycling))
       (let* ((sym   (if exact-match (intern-soft pattern ht) (car all-the-completions)))
              (recs  (symbol-value sym))
              the-net match-recs lst primary matched)
         (while recs
           (when (bbdb-record-net (car recs))
             ;; Did we match on name?
             (let ((b-r-name  (or (bbdb-record-name (car recs)) "")))
               (if (string= pattern (substring (downcase b-r-name) 0
                                               (min (length b-r-name) (length pattern))))
                   (setq match-recs  (cons (car recs) match-recs)
                         matched     t)))
             ;; Did we match on aka?
             (unless matched
               (setq lst  (bbdb-record-aka (car recs)))
               (while lst
                 (if (string= pattern (substring (downcase (car lst)) 0
                                                 (min (length (downcase (car lst)))
                                                      (length pattern))))
                     (setq match-recs  (append match-recs (list (car recs)))
                           matched     t
                           lst         ())
                   (setq lst  (cdr lst)))))
             ;; Name didn't match name so check net matching
             (unless matched
               (setq lst      (bbdb-record-net (car recs))
                     primary  t)        ; primary wins over secondary...
               (while lst
                 (if (string= pattern (substring (downcase (car lst)) 0
                                                 (min (length (downcase (car lst)))
                                                      (length pattern))))
                     (setq the-net     (car lst)
                           lst         ()
                           match-recs  (if primary
                                           (cons (car recs) match-recs)
                                         (append match-recs (list (car recs))))))
                 (setq lst      (cdr lst)
                       primary  nil))))
           (setq recs     (cdr recs)    ; Next rec for loop.
                 matched  nil))
         (unless match-recs (error "Only exact matching record has net field"))
         ;; Replace the text with the expansion
         (delete-region beg end)
         (insert (bbdb-dwim-net-address (car match-recs) the-net))
         ;; If we're past fill-column, wrap at the previous comma.
         (when (and (bbdb-auto-fill-function) (>= (current-column) fill-column))
           (let ((p  (point))
                 bol)
             (save-excursion
               (beginning-of-line)
               (setq bol  (point))
               (goto-char p)
               (when (search-backward "," bol t) (forward-char 1) (insert "\n   ")))))
         ;; Update the *BBDB* buffer if desired.
         (when bbdb-completion-display-record
           (let ((bbdb-gag-messages  t))
             (bbdb-pop-up-bbdb-buffer)
             (bbdb-display-records-1 match-recs t)))
         (bbdb-complete-name-cleanup)
         ;; Call the exact-completion hook
         (run-hooks 'bbdb-complete-name-hooks)))

      ;; Partial match.  Note: we can't use the trimmed version of the pattern here or
      ;; we'll recurse infinitely on e.g. common first names.
      ((and (stringp completion) (not (string= typed completion)))
       (delete-region beg end)
       (insert completion)
       (setq end  (point))
       (let ((last                              "")
             (bbdb-complete-name-allow-cycling  nil))
         (while (and (stringp completion) (not (string= completion last))
                     (setq last        completion
                           pattern     (downcase orig)
                           completion  (progn (all-completions pattern ht pred)
                                              (try-completion pattern ht))))
           (when (stringp completion) (delete-region beg end) (insert completion)))
         (bbdb-complete-name beg)))     ; RECURSE <================

      ;; Exact match, but more than one record
      (t
       (unless (eq (selected-window) (minibuffer-window)) (message "Making completion list..."))
       (let (dwim-completions uniq nets net name akas)
         ;; Collect all the dwim-addresses for each completion, but only once for each record.
         ;; Add if the net is part of the completions.
         (bbdb-mapc #'(lambda (sym)
                        (bbdb-mapc
                         #'(lambda (rec)
                             (unless (member rec uniq)
                               (setq uniq  (cons rec uniq)
                                     nets  (bbdb-record-net rec)
                                     name  (downcase (or (bbdb-record-name rec) ""))
                                     akas  (mapcar 'downcase (bbdb-record-aka rec)))
                               (while nets
                                 (setq net  (car nets))
                                 (when (cond
                                         ((and (member bbdb-completion-type ; Primary
                                                       '(primary primary-or-name))
                                               (member (intern-soft (downcase net) ht)
                                                       all-the-completions))
                                          (setq nets  ())
                                          t)
                                         ((and name (member bbdb-completion-type ; Name
                                                            '(nil name primary-or-name))
                                               (let ((cname  (symbol-name sym)))
                                                 (or (string= cname name)
                                                     (member cname akas))))
                                          (setq name  nil)
                                          t)
                                         ((and (member bbdb-completion-type '(nil net)) ; Net
                                               (member (intern-soft (downcase net) ht)
                                                       all-the-completions)))
                                         ;; (name-or-)primary
                                         ((and (member bbdb-completion-type
                                                       '(name-or-primary))
                                               (let ((cname  (symbol-name sym)))
                                                 (or (string= cname name)
                                                     (member cname akas))))
                                          (setq nets  ())
                                          t))
                                   (setq dwim-completions
                                         (cons (bbdb-dwim-net-address rec net)
                                               dwim-completions))
                                   (when exact-match (setq nets  ())))
                                 (setq nets  (cdr nets)))))
                         (symbol-value sym)))
                    all-the-completions)
         (cond ((and dwim-completions (null (cdr dwim-completions))) ; Insert the unique match.
                (delete-region beg end) (insert (car dwim-completions)) (message ""))
               (t                       ; More than one match.  Use Icicles minibuffer completion.
                (condition-case nil
                    (let* ((icicle-show-Completions-initially-flag      t)
                           (icicle-incremental-completion-p             'display)
                           (icicle-top-level-when-sole-completion-flag  t)
                           (completion-ignore-case                      t)
                           (choice
                            (save-excursion
                              (completing-read "Complete: " (mapcar #'list dwim-completions)
                                               nil t pattern nil pattern))))
                      (when choice
                        (delete-region beg end)
                        (insert choice)))
                  (error nil))
                (unless (eq (selected-window) (minibuffer-window))
                  (message "Making completion list...done")))))))))


;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Selects *Completions* window even if on another frame.
;;
(unless (fboundp 'old-lisp-complete-symbol)
(defalias 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol (&optional predicate) ; `M-TAB' (`C-M-i', `ESC-TAB'), globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If there is more than one completion, use the minibuffer to complete.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.

If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
  (let* ((end            (point))
         (buffer-syntax  (syntax-table))
         (beg            (unwind-protect
                              (save-excursion
                                (set-syntax-table emacs-lisp-mode-syntax-table)
                                (backward-sexp 1)
                                (while (= (char-syntax (following-char)) ?\') (forward-char 1))
                                (point))
                           (set-syntax-table buffer-syntax)))
         (pattern       (buffer-substring beg end))
         (new           (try-completion pattern obarray)))
    (unless (stringp new) (setq new  pattern))
    (delete-region beg end)
    (insert new)
    (setq end  (+ beg (length new)))
    (if (and (not (string= new "")) (not (string= (downcase new) (downcase pattern)))
             (< (length (all-completions new obarray)) 2))
        (message "Completed (no other completions)")
      ;; Use minibuffer to choose a completion.
      (let* ((enable-recursive-minibuffers                (active-minibuffer-window))
             (icicle-top-level-when-sole-completion-flag  t)
             (orig-window                                 (selected-window)) ; For alt actions.
             (alt-fn                                      nil)
             (icicle-show-Completions-initially-flag      t)
             (icicle-candidate-alt-action-fn
              (or icicle-candidate-alt-action-fn
                  (setq alt-fn  (icicle-alt-act-fn-for-type "symbol"))))
             (icicle-all-candidates-list-alt-action-fn ; M-|'
              (or icicle-all-candidates-list-alt-action-fn alt-fn
                  (icicle-alt-act-fn-for-type "symbol")))
             (predicate
              (or predicate
                  (save-excursion
                    (goto-char beg)
                    (if (not (eq (char-before) ?\( ))
                        #'(lambda (sym) ;why not just nil ?   -sm
                            (or (boundp sym) (fboundp sym) (symbol-plist sym)))
                      ;; If first element of parent list is not an open paren, assume that this is a
                      ;; funcall position: use `fboundp'.  If not, then maybe this is a variable in
                      ;; a `let' binding, so no predicate: use nil.
                      (and (not (condition-case nil
                                    (progn (up-list -2) (forward-char 1) (eq (char-after) ?\( ))
                                  (error nil)))
                           'fboundp))))))
        (setq new  (save-excursion (completing-read "Complete Lisp symbol: "
                                                    obarray predicate t new)))))
    (delete-region beg end)
    (insert new)))

;;;###autoload
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'Icicles))

;;;###autoload
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (format (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Icicles bug: \
&body=Describe bug below, using a precise recipe that starts with `emacs -Q' or `emacs -q'.  \
Each Icicles file has a header `Update #' that you can use to identify it.\
%%0A%%0AEmacs version: %s.")
                      (emacs-version))))


;; REPLACE ORIGINAL `customize-face' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(unless (fboundp 'old-customize-face)
(defalias 'old-customize-face (symbol-function 'customize-face)))

;;;###autoload
(defun icicle-customize-face (face)
  "Customize face FACE.
Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Move to next prefix-completion candidate and act
`C-up'    - Move to previous prefix-completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`M-!'     - Act on *all* candidates (or all that are saved):
            Customize all in the same buffer.
`C-!'     - Act on *all* candidates (or all that are saved):
            Customize each in a separate buffer.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
or `C-g' to quit.

With a prefix argument, you can enter multiple faces at the same time
with `RET' (in Emacs 22 or later).  This gives you the completion
behavior of `customize-face' in vanilla Emacs.  The advantage is that
the default value is the list of all faces under the cursor.  The
disadvantage is that face candidates are not WYSIWYG in buffer
*Completions*.

This is an Icicles command - see command `icicle-mode'."
  (interactive
   (list (let* ((icicle-list-use-nth-parts             '(1))
                (icicle-candidate-action-fn
                 #'(lambda (x)
                     (old-customize-face (intern (icicle-transform-multi-completion x)))
                     (select-window (minibuffer-window))
                     (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn  'icicle-customize-faces)
                (orig-window                           (selected-window)) ; For alt actions.
                (alt-fn                                nil)
                (icicle-candidate-alt-action-fn
                 (or icicle-candidate-alt-action-fn
                     (setq alt-fn  (icicle-alt-act-fn-for-type "face"))))
                (icicle-all-candidates-list-alt-action-fn ; M-|'
                 (or icicle-all-candidates-list-alt-action-fn
                     alt-fn (icicle-alt-act-fn-for-type "face"))))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face face))


;; REPLACE ORIGINAL `customize-face-other-window' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Multi-command version.
;;
(unless (fboundp 'old-customize-face-other-window)
(defalias 'old-customize-face-other-window (symbol-function 'customize-face-other-window)))

;;;###autoload
(defun icicle-customize-face-other-window (face)
  "Customize face FACE in another window.
Same as `icicle-customize-face' except it uses a different window."
  (interactive
   (list (let* ((icicle-list-use-nth-parts             '(1))
                (icicle-candidate-action-fn
                 #'(lambda (x)
                     (old-customize-face-other-window (intern (icicle-transform-multi-completion x)))
                     (select-window (minibuffer-window))
                     (select-frame-set-input-focus (selected-frame))))
                (icicle-all-candidates-list-action-fn  'icicle-customize-faces)
                (orig-window                           (selected-window)) ; For alt actions.
                (alt-fn                                nil)
                (icicle-candidate-alt-action-fn
                 (or icicle-candidate-alt-action-fn
                     (setq alt-fn  (icicle-alt-act-fn-for-type "face"))))
                (icicle-all-candidates-list-alt-action-fn ; M-|'
                 (or icicle-all-candidates-list-alt-action-fn
                     alt-fn (icicle-alt-act-fn-for-type "face"))))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face-other-window face))

(defun icicle-customize-faces (faces)
  "Open Customize buffer on all faces in list FACES."
  (let ((icicle-list-nth-parts-join-string  ": ")
        (icicle-list-join-string            ": ")
        (icicle-list-end-string             "")
        (icicle-list-use-nth-parts          '(1)))
    (custom-buffer-create
     (custom-sort-items
      (mapcar #'(lambda (f) (list (intern (icicle-transform-multi-completion f)) 'custom-face))
              faces)
      t custom-buffer-order-groups)
     "*Customize Apropos*")))


;; REPLACE ORIGINAL `customize-apropos' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(unless (fboundp 'old-customize-apropos)
(defalias 'old-customize-apropos (symbol-function 'customize-apropos)))

;;;###autoload
(defun icicle-customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which
  are not user-settable, as well as faces and groups.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg  current-prefix-arg))
     (list (completing-read "Customize (regexp): " obarray
                            #'(lambda (symbol)
                                (or (get symbol 'custom-group)
                                    (custom-facep symbol)
                                    (and (boundp symbol)
                                         (or (get symbol 'saved-value)
                                             (custom-variable-p symbol)
                                             (if (null pref-arg)
                                                 (user-variable-p symbol)
                                               (get symbol 'variable-documentation))))))
                            nil nil 'regexp-history)
           pref-arg)))
  (let ((found  nil))
    (mapatoms #'(lambda (symbol)
                  (when (string-match regexp (symbol-name symbol))
                    (when (and (not (memq all '(faces options))) ; groups or t
                               (get symbol 'custom-group))
                      (push (list symbol 'custom-group) found))
                    (when (and (not (memq all '(options groups))) ; faces or t
                               (custom-facep symbol))
                      (push (list symbol 'custom-face) found))
                    (when (and (not (memq all '(groups faces))) ; options or t
                               (boundp symbol)
                               (or (get symbol 'saved-value)
                                   (custom-variable-p symbol)
                                   (if (memq all '(nil options))
                                       (user-variable-p symbol)
                                     (get symbol 'variable-documentation))))
                      (push (list symbol 'custom-variable) found)))))
    (if (not found)
        (error "No matches")
      (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups)
                            "*Customize Apropos*"))))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value) (get variable 'custom-autoload))))


;; REPLACE ORIGINAL `customize-apropos-faces' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(unless (fboundp 'old-customize-apropos-faces)
(defalias 'old-customize-apropos-faces (symbol-function 'customize-apropos-faces)))

;;;###autoload
(defun icicle-customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize faces (regexp): " obarray 'custom-facep nil nil
                          'regexp-history)))
  (customize-apropos regexp 'faces))


;; REPLACE ORIGINAL `customize-apropos-groups' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(unless (fboundp 'old-customize-apropos-groups)
(defalias 'old-customize-apropos-groups (symbol-function 'customize-apropos-groups)))

;;;###autoload
(defun icicle-customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize groups (regexp): " obarray
                          #'(lambda (symbol) (get symbol 'custom-group)) nil nil 'regexp-history)))
  (customize-apropos regexp 'groups))


;; REPLACE ORIGINAL `customize-apropos-options' defined in `cus-edit.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(unless (fboundp 'old-customize-apropos-options)
(defalias 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix argument, include options which are not user-settable.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg  current-prefix-arg))
     (list (completing-read "Customize options (regexp): " obarray
                            #'(lambda (symbol)
                                (and (boundp symbol)
                                     (or (get symbol 'saved-value)
                                         (custom-variable-p symbol)
                                         (if (null pref-arg)
                                             (user-variable-p symbol)
                                           (get symbol 'variable-documentation)))))
                            nil nil 'regexp-history)
           pref-arg)))
  (customize-apropos regexp (or arg 'options)))


;; REPLACE ORIGINAL `customize-apropos-options-of-type' defined in `cus-edit+.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the regexp.
;;
(when (and (fboundp 'customize-apropos-options-of-type)
           (not (fboundp 'old-customize-apropos-options-of-type)))
(defalias 'old-customize-apropos-options-of-type
    (symbol-function 'customize-apropos-options-of-type)))

;;;###autoload
(defun icicle-customize-apropos-options-of-type (type regexp)
  "Customize all loaded customizable options of type TYPE that match REGEXP.
With no prefix arg, each option is defined with `defcustom' type TYPE.
With a prefix arg, either each option is defined with `defcustom' type
 TYPE or its current value is compatible with TYPE.

If TYPE is nil (the default value) then all `defcustom' variables are
potential candidates.

Use `S-TAB', `next', and `prior', to match regexp input - this lets
you see which options will be available in the customize buffer."
  (interactive
   (let ((typ       (car (condition-case err
                             (read-from-string
                              (let ((types ()))
                                (mapatoms
                                 #'(lambda (cand)
                                     (when (custom-variable-p cand)
                                       (push (list (format "%s"
                                                           (format "%S" (get cand 'custom-type))))
                                             types))))
                                (completing-read "Customize all options of type: "
                                                 (icicle-remove-duplicates types)
                                                 nil nil nil nil "nil")))
                           (end-of-file (error "No such custom type")))))
         (pref-arg  current-prefix-arg))
     (list typ
           (completing-read
            "Customize options matching (regexp): " obarray
            #'(lambda (symb)
                (and (boundp symb)
                     (or (not (fboundp 'indirect-variable)) (eq (indirect-variable symb) symb))
                     (or (get symb 'saved-value) (custom-variable-p symb))
                     (or (not typ)      ; `typ' = nil means use all types.
                         (if pref-arg
                             (icicle-var-is-of-type-p symb (list typ))
                           (equal (get symb 'custom-type) typ)))))
            nil nil 'regexp-history))))
  (custom-buffer-create (custom-sort-items
                         (mapcar #'(lambda (s) (list (intern s) 'custom-variable))
                                 icicle-completion-candidates)
                         t "*Customize Apropos*")))


;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;; saving it for restoration when you toggle `icicle-mode'.
;;
;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;
(unless (fboundp 'old-repeat-complex-command)
(defalias 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg) ; Bound to `C-x ESC ESC', `C-x M-:' in Icicle mode.
  "Edit and re-evaluate the last complex command, or ARGth from last.
A complex command is one that used the minibuffer.
ARG is the prefix argument numeric value.

You can edit the past command you choose before executing it.  The
Lisp form of the command is used.  If the command you enter differs
from the previous complex command, then it is added to the front of
the command history.

Icicles completion is available for choosing a past command.  You can
still use the vanilla Emacs bindings `\\<minibuffer-local-map>\\[next-history-element]' and \
`\\[previous-history-element]' to cycle inputs,
and `\\[repeat-matching-complex-command]' to match regexp input, but Icicles input cycling (`up',
`down',`next', `prior') and apropos completion (`S-TAB') are superior
and more convenient."
  (interactive "p")
  (let ((elt  (nth (1- arg) command-history))
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level                   nil)
                      (minibuffer-history-position   arg)
                      (minibuffer-history-sexp-flag  (1+ (minibuffer-depth))))
                  (unwind-protect
                       (let ((icicle-transform-function  'icicle-remove-duplicates))
                         (read (completing-read
                                "Redo: " (mapcar #'(lambda (entry) (list (prin1-to-string entry)))
                                                 command-history)
                                nil nil (prin1-to-string elt) (cons 'command-history arg)
                                (prin1-to-string elt))))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history  (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history  (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;;###autoload
(defun icicle-add-entry-to-saved-completion-set (set-name entry type)
  "Add ENTRY to saved completion-candidates set SET-NAME.
ENTRY is normally a single candidate (a string).
 With a prefix arg, however, and if option
 `icicle-filesets-as-saved-completion-sets-flag' is non-nil, then
 ENTRY is the name of an Emacs fileset (Emacs 22 or later).
TYPE is the type of entry to add: `Fileset' or `Candidate'."
  (interactive
   (let ((typ (if (and current-prefix-arg icicle-filesets-as-saved-completion-sets-flag
                       (prog1 (or (require 'filesets nil t)
                                  (error "Feature `filesets' not provided"))
                         (filesets-init))
                       filesets-data)
                  'Fileset
                'Candidate)))
     (list
      (save-selected-window
        (completing-read "Saved completion set: " icicle-saved-completion-sets nil t nil
                         'icicle-completion-set-history))
      (if (eq typ 'Fileset)
          (list ':fileset               ; Just save the fileset name, not the data.
                (car (assoc (completing-read "Fileset to add: " filesets-data nil t)
                            filesets-data)))
        (completing-read "Candidate to add: " (mapcar #'list icicle-saved-completion-candidates)))
      typ)))
  (let ((file-name  (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf  (find-file-noselect file-name 'nowarn 'raw))
          candidates newcands entry-type)
      (unwind-protect
           (condition-case icicle-add-entry-to-saved-completion-set
               (when (listp (setq newcands  (setq candidates  (read list-buf))))
                 (message "Set `%s' read from file `%s'" set-name file-name))
             (error (error "Bad cache file.  %s"
                           (error-message-string icicle-add-entry-to-saved-completion-set))))
        (kill-buffer list-buf))
      (unless (consp newcands) (error "Bad data in cache file `%s'" file-name))
      (pushnew entry newcands :test #'equal)
      (setq entry  (if (eq type 'Fileset) (caar entry) entry))
      (if (= (length candidates) (length newcands))
          (message "%s `%s' is already in saved set `%s', file `%s'" type entry set-name file-name)
        (with-temp-message (format "Writing entry to cache file `%s'..." file-name)
          (with-temp-file file-name (prin1 newcands (current-buffer))))
        (message "%s `%s' added to saved set `%s', file `%s'" type entry set-name file-name)))))

;;;###autoload
(defun icicle-remove-entry-from-saved-completion-set (set-name)
  "Remove an entry from saved completion-candidates set SET-NAME.
SET-NAME can be an Icicles saved completions set (cache file) or the
name of an Emacs fileset.

The entry to remove can be a single completion candidate (a string) or
an Emacs fileset.  You can thus remove a file name from a fileset or
remove a fileset from an Icicles saved completion set.  (You can also
remove a file name from a saved completion set.)  If a saved set has
both a file and a fileset of the same name, then both are removed.

To use filesets here, use Emacs 22 or later, load library `filesets',
use `(filesets-init)', and ensure that option
`icicle-filesets-as-saved-completion-sets-flag' is non-nil."
  (interactive
   (list (completing-read "Saved completion set: "
                          (if (and icicle-filesets-as-saved-completion-sets-flag
                                   (featurep 'filesets) filesets-data)
                              (append filesets-data icicle-saved-completion-sets)
                            icicle-saved-completion-sets)
                          nil t nil 'icicle-completion-set-history)))
  (let* ((file-name                              (cdr (assoc set-name icicle-saved-completion-sets)))
         (candidates                             (icicle-get-candidates-from-saved-set
                                                  set-name 'dont-expand))
         (icicle-whole-candidate-as-text-prop-p  t)
         (entry
          (funcall icicle-get-alist-candidate-function
                   (completing-read
                    "Candidate to remove: "
                    (mapcar #'(lambda (e)
                                (cond ((icicle-saved-fileset-p e) ; Swap `:fileset' with fileset name
                                       `(,(cadr e) ,(car e) ,@(cddr e)))
                                      ((consp e) e)
                                      (t (list e)))) ; Listify naked string.
                            candidates)
                    nil t))))
    (when (and (consp entry) (eq (cadr entry) ':fileset)) ; Swap back again: `:fileset' and name.
      (setq entry  `(,(cadr entry) ,(car entry) ,@(cddr entry))))
    (when (and (consp entry) (null (cdr entry))) (setq entry  (car entry))) ; Use just the string.
    ;; Delete any such candidate, then remove text properties used for completion.
    (setq candidates  (mapcar #'icicle-unpropertize (delete entry candidates)))
    (cond (file-name
           (with-temp-message           ; Remove from cache file.
               (format "Writing remaining candidates to cache file `%s'..." file-name)
             (with-temp-file file-name (prin1 candidates (current-buffer)))))
          ((icicle-saved-fileset-p (list ':fileset set-name)) ; Remove from fileset.
           (unless (require 'filesets nil t) (error "Feature `filesets' not provided"))
           (filesets-init)
           (let ((fst  (and filesets-data (assoc set-name filesets-data)))) ; The fileset itself.
             (unless fst (error "No such fileset: `%s'" set-name))
             (let ((fst-files  (filesets-entry-get-files fst)))
               (if (car (filesets-member entry fst-files :test 'filesets-files-equalp))
                   (if fst-files        ; Similar to code in `filesets-remove-buffer'.
                       (let ((new-fst  (list (cons ':files (delete entry fst-files)))))
                         (setcdr fst new-fst)
                         (filesets-set-config set-name 'filesets-data filesets-data))
                     (message "Cannot remove `%s' from fileset `%s'" entry set-name))
                 (message "`%s' not in fileset `%s'" entry set-name))))))
    (icicle-msg-maybe-in-minibuffer
     (concat (format "`%s' removed from %s `%s'"
                     (if (icicle-saved-fileset-p entry) (cadr entry) entry)
                     (if (icicle-saved-fileset-p entry) "fileset" "saved set")
                     set-name)
             (and file-name (format ", file `%s'" file-name))))))

;;;###autoload
(icicle-define-command icicle-remove-saved-completion-set ; Command name
  "Remove an entry from `icicle-saved-completion-sets'.
Save the updated option.
You are prompted to also delete the associated cache file.
You can add entries to `icicle-saved-completion-sets' using command
`icicle-add/update-saved-completion-set'." ; Doc string
  icicle-remove-saved-set-action
  "Remove set of completion candidates named: " ; `completing-read' args
  icicle-saved-completion-sets nil t nil 'icicle-completion-set-history nil nil
  ((icicle-whole-candidate-as-text-prop-p  t) ; Additional bindings
   (icicle-use-candidates-only-once-flag   t))
  nil nil (icicle-remove-Completions-window)) ; First code, undo code, last code

(defun icicle-remove-saved-set-action (set-name)
  "Remove saved set SET-NAME from `icicle-saved-completion-sets'."
  (let ((enable-recursive-minibuffers  t)
        (sets                          icicle-saved-completion-sets)
        set cache)
    (save-selected-window
      (select-window (minibuffer-window))
      (while (setq set    (assoc set-name sets)
                   cache  (cdr set))
        (when (file-exists-p cache)
          (if (y-or-n-p (format "Delete cache file `%s'? " cache))
              (when (condition-case err
                        (progn (delete-file cache) t)
                      (error (progn (message (error-message-string err)) nil)))
                (message "DELETED `%s'" cache) (sit-for 1))
            (message "OK, file NOT deleted") (sit-for 1)))
        (setq sets  (delete set sets)))))
  (setq icicle-saved-completion-sets
        (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
  (funcall icicle-customize-save-variable-function
           'icicle-saved-completion-sets
           icicle-saved-completion-sets)
  (message "Candidate set `%s' removed" set-name))

;;;###autoload
(defun icicle-dired-save-marked (&optional arg) ; Bound to `C-M->' in Dired.
  "Save the marked file names in Dired as a set of completion candidates.
Saves file names in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a non-zero numeric prefix arg (`C-u N'), save candidates in a
 variable for which you are prompted.
With a zero prefix arg (`C-0'), save candidates in a fileset (Emacs 22
 or later).  Use this only for file-name candidates, obviously.
 To subsequently use a fileset for candidate retrieval, option
 `icicle-filesets-as-saved-completion-sets-flag' must be non-nil.

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "Command `icicle-dired-save-marked' must be called from a Dired buffer"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg))

;;;###autoload
(defun icicle-dired-save-marked-more (&optional arg) ; Bound to `C->' in Dired.
  "Add the marked file names in Dired to the saved candidates set.
Add candidates to `icicle-saved-completion-candidates', by default.
A prefix argument acts the same as for `icicle-candidate-set-save'.

The existing saved candidates are still saved.  The current candidates
are added to those already saved.

You can retrieve the saved set of candidates with `C-M-<'.
You can use the saved set of candidates for operations such as"
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error "`icicle-dired-save-marked-more' must be called from a Dired buffer"))
  (icicle-candidate-set-save-1 (dired-get-marked-files) arg t))

;;;###autoload
(defun icicle-dired-save-marked-to-variable () ; Bound to `C-M-}' in Dired.
  "Save the marked file names in Dired as a candidate set to a variable.
You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive)
  (icicle-candidate-set-save-1 (dired-get-marked-files) 99))

(defalias 'icicle-dired-save-marked-as-project ; Bound to `C-}' in Dired.
    'icicle-dired-save-marked-persistently)
;;;###autoload
(defun icicle-dired-save-marked-persistently (filesetp)
  "Save the marked file names in Dired as a persistent set.
With no prefix arg, save in a cache file.
With a prefix arg, save in an Emacs fileset (Emacs 22 or later).

You can retrieve the saved set of file-name candidates during
completion using `\\<minibuffer-local-completion-map>\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from a Dired buffer."
  (interactive "P")
  (icicle-candidate-set-save-1 (dired-get-marked-files) (if filesetp 0 '(1))))


(put 'icicle-dired-saved-file-candidates 'icicle-Completions-window-max-height 200)
(defalias 'icicle-dired-chosen-files 'icicle-dired-saved-file-candidates)
;;;###autoload
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on a set of files and directories of your choice.
If you have saved a set of file names using \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]', then it is used.
If not, you are prompted to choose the files.
With a prefix argument, you are prompted for the default directory to use.
Otherwise, the current value of `default-directory' is used.
Names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the Dired
directory (default directory)."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
  (unless icicle-saved-completion-candidates
    (error (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
  (let* ((default-directory           (if prompt-for-dir-p
                                          (read-file-name "Directory: " nil default-directory nil)
                                        default-directory))
         (icicle-list-use-nth-parts   '(1))
         (file-names                  (icicle-remove-if
                                       #'(lambda (fil) (or (null fil) (not (file-exists-p fil))))
                                       (or (and icicle-saved-completion-candidates
                                                (mapcar #'icicle-transform-multi-completion
                                                        icicle-saved-completion-candidates))
                                           (icicle-file-list)))))
    (dired (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

(defalias 'icicle-dired-chosen-files-other-window 'icicle-dired-saved-file-candidates-other-window)
;;;###autoload
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p) ; Bound `C-M-<' in Dired.
  "Open Dired in other window on set of files & directories of your choice.
If you have saved a set of file names using \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]', then it is used.
If not, you are prompted to choose the files.
With a prefix arg, you are prompted for the default directory to use.
Otherwise, the current value of `default-directory' is used.
Names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the Dired
directory (default directory)."
  (interactive "P")
  ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
  (let* ((default-directory           (if prompt-for-dir-p
                                          (read-file-name "Directory: " nil default-directory nil)
                                        default-directory))
         (icicle-list-use-nth-parts   '(1))
         (file-names                  (icicle-remove-if
                                       #'(lambda (fil) (or (null fil) (not (file-exists-p fil))))
                                       (or (and icicle-saved-completion-candidates
                                                (mapcar #'icicle-transform-multi-completion
                                                        icicle-saved-completion-candidates))
                                           (icicle-file-list)))))
    (dired-other-window (cons (generate-new-buffer-name "Icy File Set") (nreverse file-names)))))

(put 'icicle-dired-project 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-dired-project (prompt-for-dir-p)
  "Open Dired on a saved project.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.

Project file names that do not correspond to existing files are
ignored.  Existence of files with relative names is checked in the
directory.

You can use `C-x m' during completion to access Dired bookmarks, if
you use library `bookmark+.el'."
  (interactive "P")
  (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-dired-other-window))
    (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-dired-other-window))
  (unwind-protect
       ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
       (let ((set-name  (completing-read "Project (saved file names): "
                                         (if (and icicle-filesets-as-saved-completion-sets-flag
                                                  (featurep 'filesets) filesets-data)
                                             (append filesets-data icicle-saved-completion-sets)
                                           icicle-saved-completion-sets)
                                         nil nil nil 'icicle-completion-set-history)))
         (icicle-retrieve-candidates-from-set set-name)
         (let* ((default-directory  (if prompt-for-dir-p
                                        (read-file-name "Dired directory: " nil
                                                        default-directory nil)
                                      default-directory))
                (file-names         ()))
           (dolist (f  icicle-saved-completion-candidates)
             (when (file-exists-p f) (push f file-names)))
           (unless file-names (error "No files in project `%s' actually exist" set-name))
           (dired (cons (generate-new-buffer-name set-name)
                        (nreverse (mapcar #'(lambda (file)
                                              (if (file-name-absolute-p file)
                                                  (expand-file-name file)
                                                file))
                                          file-names))))))
    (define-key minibuffer-local-completion-map "\C-xm" nil)))

;;;###autoload
(defun icicle-dired-project-other-window (prompt-for-dir-p) ; Bound to `C-{' in Dired.
  "Open Dired on a saved project in another window.
A project is either a persistent completion set or an Emacs fileset.
With a prefix argument, you are prompted for the directory.
Otherwise, the default directory is assumed.

Project file names that do not correspond to existing files are
ignored.  Existence of files with relative names is checked in the
directory.

You can use `C-x m' during completion to access Dired bookmarks, if
you use library `bookmark+.el'."
  (interactive "P")
  (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-dired-other-window))
    (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-dired-other-window))
  (unwind-protect
       ;; $$$$$$$ Maybe filter sets to get only file-name candidate sets?
       (let ((set-name  (completing-read "Project (saved file names): "
                                         (if (and icicle-filesets-as-saved-completion-sets-flag
                                                  (featurep 'filesets) filesets-data)
                                             (append filesets-data icicle-saved-completion-sets)
                                           icicle-saved-completion-sets)
                                         nil nil nil 'icicle-completion-set-history)))
         (icicle-retrieve-candidates-from-set set-name)
         (let* ((default-directory  (if prompt-for-dir-p
                                        (read-file-name "Dired directory: " nil
                                                        default-directory nil)
                                      default-directory))
                (file-names         ()))
           (dolist (f  icicle-saved-completion-candidates)
             (when (file-exists-p f) (push f file-names)))
           (unless file-names (error "No files in project `%s' actually exist" set-name))
           (dired-other-window (cons (generate-new-buffer-name set-name)
                                     (nreverse (mapcar #'(lambda (file)
                                                           (if (file-name-absolute-p file)
                                                               (expand-file-name file)
                                                             file))
                                                       file-names))))))
    (define-key minibuffer-local-completion-map "\C-xm" nil)))

;;;###autoload
(defun icicle-grep-saved-file-candidates (command-args)
  "Run `grep' on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Saved names that do not correspond to existing files are ignored.
Existence of files with relative names is checked in the default
directory."
  (interactive
   (list
    (let ((file-names  ()))
      (unless icicle-saved-completion-candidates
        (error (substitute-command-keys "No saved completion candidates.  \
Use \\<minibuffer-local-completion-map>`\\[icicle-candidate-set-save]' to save candidates")))
      (unless grep-command (grep-compute-defaults))
      (dolist (f  icicle-saved-completion-candidates) (when (file-exists-p f) (push f file-names)))
      (let ((default  (and (fboundp 'grep-default-command) (grep-default-command))))
        (read-from-minibuffer
         "grep <pattern> <files> :  "
         (let ((up-to-files  (concat grep-command "   ")))
           (cons (concat up-to-files (mapconcat #'identity icicle-saved-completion-candidates " "))
                 (- (length up-to-files) 2)))
         nil nil 'grep-history default)))))
  (grep command-args))

;; Utility function.  Use it to define multi-commands that navigate.
(defun icicle-explore (define-candidates-fn final-action-fn quit-fn error-fn cleanup-fn prompt
                       &rest compl-read-args)
  "Icicle explorer: explore complex completion candidates.
Navigate among locations or other entities represented by a set of
completion candidates.  See `icicle-search' for a typical example.

Arguments:
 DEFINE-CANDIDATES-FN:
   Function of no args called to fill `icicle-candidates-alist' with
   the candidates.
 FINAL-ACTION-FN:
   Function of no args called after the final choice of candidate
   (after both `icicle-explore-final-choice' and
   `icicle-explore-final-choice-full' have been set).  Typically uses
   `icicle-explore-final-choice-full', the full candidate.
 QUIT-FN: Function of no args called if user uses `C-g'.
 ERROR-FN: Function of no args called if an error is raised.
 CLEANUP-FN: Function of no args called after exploring.
 PROMPT: Prompt string for `completing-read'.
 COMPL-READ-ARGS: `completing-read' args other than PROMPT and
   COLLECTION.

If there is only one candidate, then FINAL-ACTION-FN is called
immediately.  The candidate is not available to act on (e.g. using
``C-S-RET').

Returns:
 The result of executing FINAL-ACTION-FN, if that arg is non-nil.
 Otherwise, `icicle-explore-final-choice-full'.

To use `icicle-explore' to define a multi-command, you must also bind
`icicle-candidate-action-fn'.

Though `icicle-explore' is typically used to define navigation
commands, it need not be.  It can be useful anytime you need to use
`completing-read' and also provide specific behavior for quitting
\(`C-g'), completion errors, and final actions."
  (let ((icicle-incremental-completion-flag     'always)
        (icicle-whole-candidate-as-text-prop-p  t)
        (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
        (icicle-act-before-cycle-flag           icicle-act-before-cycle-flag)
        (orig-pt-explore                        (point-marker))
        (orig-win-explore                       (selected-window))
        result)
    (setq icicle-act-before-cycle-flag      nil
          icicle-candidates-alist           nil
          icicle-explore-final-choice       nil
          icicle-explore-final-choice-full  nil)
    (icicle-highlight-lighter)
    (message "Finding candidates...")
    (when define-candidates-fn (funcall define-candidates-fn))
    (unless icicle-candidates-alist (error "No candidates defined"))
    (when (= (length icicle-candidates-alist) 1)
      (setq icicle-explore-final-choice  (icicle-display-cand-from-full-cand
                                          (car icicle-candidates-alist))))
    (unwind-protect
         (condition-case failure
             (progn
               (unless icicle-explore-final-choice
                 (setq icicle-explore-final-choice
                       (let ((icicle-remove-icicles-props-p  nil)) ; Keep Icicles text properties.
                         (apply #'completing-read prompt icicle-candidates-alist compl-read-args))))
               (setq icicle-explore-final-choice-full
                     (funcall icicle-get-alist-candidate-function
                              icicle-explore-final-choice 'no-error-p))
               (unless icicle-explore-final-choice-full (error "No such occurrence"))
               (setq result  (if final-action-fn
                                 (funcall final-action-fn)
                               icicle-explore-final-choice-full)))
           (quit (if quit-fn (funcall quit-fn) (keyboard-quit)))
           (error (when error-fn (funcall error-fn))
                  (error "%s" (error-message-string failure))))
      (setq result  (icicle-unpropertize result)) ; Finally remove any Icicles text properties.
      (when cleanup-fn (funcall cleanup-fn)))
    result))

;;;###autoload
(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
  "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command.

By default, Icicle mode remaps all key sequences that are normally
bound to `execute-extended-command' to
`icicle-execute-extended-command'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute command%s: "         ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray 'commandp t nil 'extended-command-history nil nil
  (;; Bindings
   (alt-fn  nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "command")))
   (use-file-dialog  nil)               ; `mouse-2' in *Completions* shouldn't use file dialog box.
   (last-command     last-command)      ; Save and restore the last command.
   new-last-cmd)                        ; Set in `icicle-execute-extended-command-1'.
  nil  nil
  (setq this-command  new-last-cmd))    ; Final code: this will update `last-command'.

;; Free vars here: `orig-buff' and `orig-window' are bound by `icicle-define-command'.
;;                 `new-last-cmd' is bound in `icicle-execute-extended-command'.
(defun icicle-execute-extended-command-1 (cmd-name)
  "Action function to execute command or named keyboard macro CMD-NAME."
  (when (get-buffer orig-buff) (set-buffer orig-buff))
  (when (window-live-p orig-window) (select-window orig-window))
  (when (string= "" cmd-name) (error "No command name"))

  (let* ((cmd                                       (intern cmd-name))
         ;; Rebind `icicle-candidate-action-fn' to a function that calls the
         ;; candidate CMD-NAME on a single argument that it reads.  This is
         ;; used only if CMD-NAME is a command that, itself, reads an input
         ;; argument with completion.  When that is the case, you can use
         ;; completion on that input, and if you do that, you can use `C-RET'
         ;; to use command CMD-NAME as a multi-command.  In other words, this
         ;; binding allows for two levels of multi-commands.
         (icicle-candidate-action-fn
          (and icicle-candidate-action-fn ; This is nil after the command name is read.
               #'(lambda (arg)
                   (condition-case nil
                       (funcall cmd arg) ; Try to use string candidate `arg'.
                     ;; If that didn't work, use a symbol or number candidate.
                     (wrong-type-argument (funcall cmd (car (read-from-string arg))))
                     (wrong-number-of-arguments ; Punt - show help.
                      (funcall #'icicle-help-on-candidate)))
                   (select-window (minibuffer-window))
                   (select-frame-set-input-focus (selected-frame)))))
         (fn                                        (symbol-function cmd))
         (count                                     (prefix-numeric-value current-prefix-arg))
         ;; Rebind alternative action functions to nil, so we don't override the command we call.
         (icicle-candidate-alt-action-fn            nil)
         (icicle-all-candidates-list-alt-action-fn  nil))
    ;; Message showing what `cmd' is bound to.  This is pretty much a transcription of C code in
    ;; `keyboard.c'.  Not sure it DTRT when there is already a msg in the echo area.
    (when (and suggest-key-bindings (not executing-kbd-macro))
      (let* ((bindings   (where-is-internal cmd overriding-local-map t))
             (curr-msg   (current-message))
             (wait-time  (if curr-msg
                             (or (and (numberp suggest-key-bindings) suggest-key-bindings) 2)
                           0)))
        (when (and bindings (not (and (vectorp bindings) (eq (aref bindings 0) 'mouse-movement))))
          (when (and (sit-for wait-time) (atom unread-command-events))
            (let ((message-log-max  nil)) ; Don't log this message.
              (message "You can run the command `%s' with `%s'"
                       (symbol-name cmd)
                       (key-description bindings)))
            (when (and (sit-for wait-time) curr-msg) (message curr-msg))))))
    (cond ((arrayp fn)
           (let ((this-command  cmd)) (execute-kbd-macro fn count))
           (when (> count 1) (message "(%d times)" count)))
          (t
           (run-hooks 'post-command-hook)
           (run-hooks 'pre-command-hook)
           (let ((enable-recursive-minibuffers  t)
                 ;; Bind, don't set `this-command'.  When you use `C-next', `this-command' needs
                 ;; to be `cmd' during the `C-RET' part, but `last-command' must not be `cmd'
                 ;; during the `next' part.
                 (this-command                  cmd))
             (call-interactively cmd 'record-it))))
    ;; After `M-x' `last-command' must be the command finally entered with `RET' or, if you end
    ;; with `C-g', the last command entered with `C-RET'.
    (setq new-last-cmd  cmd)))

;; Inspired by Emacs partial completion and by library `exec-abbrev-cmd.el' (Tassilo Horn
;; <tassilo@member.fsf.org>).  The idea of command abbreviation is combined here with normal
;; command invocation, in an Icicles multi-command.
;;;###autoload
(icicle-define-command icicle-command-abbrev ; Bound to `C-x SPC' in Icicle mode.
  "Read command name or its abbreviation, read command args, call command.
Read input, then call `icicle-command-abbrev-action' to act on it.

If `icicle-add-proxy-candidates-flag' is non-nil, then command
abbreviations, as well as commands, are available as completion
candidates.  Otherwise, only commands are available.  You can toggle
this user option using `\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]'\
in the minibuffer.

When an abbreviation is available, you can treat it just like a
command.  The rest of this description covers the behavior of choosing
an abbreviation.

If an abbreviation matches a single command name, then that command is
invoked.  If it matches more than one, then you can use completion to
choose one.

Hyphens (`-') in command names divide them into parts.  For example,
`find-file' has two parts: `find' and `file'.  Each character of a
command abbreviation corresponds to one part of each of the commands
that match the abbreviation.  For example, abbreviation `ff' matches
commands `find-file' and `focus-frame', and abbreviation `fg' matches
`find-grep'.

User option `icicle-command-abbrev-match-all-parts-flag' = nil means
that an abbreviation need not match all parts of a command name; it
need match only a prefix.  For example, nil means that abbreviation
`ff' also matches `find-file-other-window' and `fg' also matches
`find-grep-dired'."                     ; Doc string
  icicle-command-abbrev-action          ; Function to perform the action
  prompt obarray  'commandp             ; `completing-read' args
  nil nil 'icicle-command-abbrev-history nil nil
  ((icicle-sort-comparer     'icicle-command-abbrev-used-more-p) ; Bindings.
   (prompt                   "Command or abbrev: ")
   (icicle-proxy-candidates  (let ((ipc  ())
                                   abv)
                               (dolist (entry  icicle-command-abbrev-alist  ipc)
                                 (setq abv  (symbol-name (cadr entry)))
                                 (unless (member abv ipc) (push abv ipc)))))
   (alt-fn                   nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "command")))
   (use-file-dialog          nil)       ; `mouse-2' in *Completions* shouldn't use file dialog box.
   (last-command             last-command)) ; Save and restore the last command.
  (when icicle-proxy-candidates         ; First code
    (put-text-property 0 1 'icicle-fancy-candidates t prompt))
  nil (setq icicle-proxy-candidates  nil)) ; Undo code, last code

(defun icicle-command-abbrev-action (abbrev-or-cmd)
  "Action function for `icicle-command-abbrev'.
If ABBREV-OR-CMD is a command, call it.
If ABBREV-OR-CMD is an abbreviation for a single command, invoke it.
If ABBREV-OR-CMD is an abbreviation for multiple commands, call
`icicle-command-abbrev-command', to let user choose commands.
If ABBREV-OR-CMD is not an abbreviation or a command, raise an error."
  (setq abbrev-or-cmd  (intern abbrev-or-cmd))
  (let* ((not-cmdp                          (not (commandp abbrev-or-cmd)))
         (regexp                            (and (or not-cmdp icicle-command-abbrev-priority-flag)
                                                 (icicle-command-abbrev-regexp abbrev-or-cmd)))
         (icicle-commands-for-abbrev        (and (or not-cmdp icicle-command-abbrev-priority-flag)
                                                 (icicle-command-abbrev-matching-commands regexp)))
         (icicle-add-proxy-candidates-flag  icicle-add-proxy-candidates-flag)
         (icicle-proxy-candidates           icicle-proxy-candidates)
         ;; Rebind alternative action functions to nil, so we don't override command we call.
         (icicle-candidate-alt-action-fn    nil)
         (icicle-all-candidates-list-alt-action-fn  nil))
    (cond ((and not-cmdp (null icicle-commands-for-abbrev))
           (error "No such command or abbreviation `%s'" abbrev-or-cmd))
          (icicle-commands-for-abbrev
           (let* ((icicle-sort-comparer  'icicle-command-abbrev-used-more-p)
                  (cmd
                   (if (null (cdr icicle-commands-for-abbrev))
                       (prog1 (intern (caar icicle-commands-for-abbrev))
                         (push (caar icicle-commands-for-abbrev) extended-command-history)
                         (call-interactively (intern (caar icicle-commands-for-abbrev)) t))
                     (let ((enable-recursive-minibuffers  t))
                       (icicle-remove-Completions-window)
                       (icicle-command-abbrev-command)))))
             (icicle-command-abbrev-record abbrev-or-cmd cmd)))
          ((not not-cmdp) (call-interactively abbrev-or-cmd)))))

(defun icicle-command-abbrev-regexp (abbrev)
  "Return the command-matching regexp for ABBREV."
  (let ((char-list  (append (symbol-name abbrev) ()))
        (str        "^"))
    (dolist (c  char-list) (setq str  (concat str (list c) "[^-]*-")))
    (concat (substring str 0 (1- (length str)))
            (if icicle-command-abbrev-match-all-parts-flag "$" ".*$"))))

(defun icicle-command-abbrev-matching-commands (regexp)
  "Return a completion alist of commands that match REGEXP."
  (mapcar #'list (icicle-remove-if-not
                  #'(lambda (strg) (string-match regexp strg))
                  (let ((cmds  ()))
                    (mapatoms #'(lambda (symb)
                                  (when (commandp symb) (push (symbol-name symb) cmds))))
                    cmds))))

;;;###autoload
(icicle-define-command icicle-command-abbrev-command
  "Read command name, then read its arguments and call command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Command abbreviated%s%s: "   ; `completing-read' arguments
          (cond ((and icicle-current-input (not (string= "" icicle-current-input)))
                 (format " `%s'" icicle-current-input))
                (icicle-candidate-nb
                 (format " `%s'" (elt icicle-completion-candidates icicle-candidate-nb)))
                (t ""))
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  icicle-commands-for-abbrev nil t nil 'extended-command-history nil nil
  (;; Bindings
   (use-file-dialog                   nil) ; `mouse-2' in *Completions* shouldn't use file dialog.
   (alt-fn                            nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "command")))
   (icicle-add-proxy-candidates-flag  nil) ; No abbrevs - just commands here.
   (last-command                      last-command) ; Save and restore the last command.
   new-last-cmd)                        ; Set in `icicle-execute-extended-command-1'.
  nil nil
  (setq this-command  new-last-cmd)     ; Final code: this will update `last-command'.
  'NON-INTERACTIVE)                     ; This is not a real command.

(defun icicle-command-abbrev-record (abbrev command)
  "Record ABBREV and COMMAND in `icicle-command-abbrev-alist'."
  (let ((entry  (assq command icicle-command-abbrev-alist)))
    (when (and abbrev command)
      (if entry
          (setq icicle-command-abbrev-alist  (cons (list command abbrev (1+ (car (cddr entry))))
                                                   (delete entry icicle-command-abbrev-alist)))
        (push (list command abbrev 1) icicle-command-abbrev-alist)))))

;;;###autoload
(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
  "Read the name of a keyboard macro, then execute it."
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute keyboard macro%s: "  ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray #'(lambda (fn) (and (commandp fn) (arrayp (symbol-function fn))))
  t nil 'icicle-kmacro-history nil nil
  ((alt-fn        nil)
   (icicle-candidate-alt-action-fn      ; Bindings
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "command"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "command")))
   (last-command  last-command)))       ; Save and restore the last command.

;;;###autoload
(when (require 'kmacro nil t)
  (icicle-define-command icicle-kmacro  ; Bound to `S-f4' in Icicle mode (Emacs 22+).
    "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names `1', `2', and so on,
for use as completion candidates.

With prefix argument, repeat macro that many times (but see below).
If a macro is still being defined, end it first, then execute it.

Since this is a multi-command, you can execute any number of macros
any number of times in a single invocation.  In particular, you can
execute a given macro repeatedly using `C-RET' (be sure you use `TAB'
first, to make it the current candidate).

If you use a prefix arg for `icicle-kmacro', then each multi-command
action (e.g. `C-RET') repeats the macro that number of times.  However
if you use a prefix arg for an individual action, then the action
repeats the macro that number of times.  Without its own prefix arg,
an action uses the base prefix arg you used for `icicle-kmacro'."
    icicle-kmacro-action                ; Function to perform the action
    (format "Execute keyboard macro%s: " ; `completing-read' args
            (if current-prefix-arg
                (format " (prefix %s)" (prefix-numeric-value current-prefix-arg))
              ""))
    (let ((count  0))
      (setq icicle-kmacro-alist
            (mapcar #'(lambda (x) (cons (format "%d" (setq count  (1+ count))) x))
                    (reverse (if nil kmacro-ring (cons (kmacro-ring-head) kmacro-ring))))))
    nil 'no-exit-wo-match nil 'icicle-kmacro-history
    (and (kmacro-ring-head) (null kmacro-ring) "1") nil
    ((pref-arg  current-prefix-arg))    ; Additional bindings
    (progn                              ; First code
      (when defining-kbd-macro (kmacro-end-or-call-macro current-prefix-arg) (error "Done"))
      (unless (or (kmacro-ring-head) kmacro-ring) (error "No keyboard macro defined"))))

  ;; Free vars here: `orig-buff' and `orig-window' are bound by `icicle-define-command'.
  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer orig-buff) (set-buffer orig-buff))
    (when (window-live-p orig-window) (select-window orig-window))
    (let* ((count  (if current-prefix-arg (prefix-numeric-value current-prefix-arg) pref-arg))
           (macro  (cadr (assoc cand icicle-kmacro-alist))))
      (unless macro (error "No such macro: `%s'" cand))
      (execute-kbd-macro macro count #'kmacro-loop-setup-function)
      (when (> count 1) (message "(%d times)" count)))))

;;;###autoload
(icicle-define-command icicle-set-option-to-t ; Command name
  "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) t) (message "`%s' is now t" opt)) ; Action function
  "Set option to t: " obarray           ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         #'(lambda (x) (and (boundp x) (user-variable-p x) (eq nil (symbol-value x)))))
        (current-prefix-arg
         #'(lambda (x) (and (boundp x) (eq nil (symbol-value x)))))
        (t
         #'(lambda (x) (and (boundp x) (icicle-binary-option-p x) (eq nil (symbol-value x))))))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers          t) ; Bindings
   (icicle-use-candidates-only-once-flag  t)
   (alt-fn                                nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "option")))))

;;;###autoload
(icicle-define-command icicle-clear-history
  "Clear a minibuffer history of selected entries.
You are prompted for the history to clear, then you are prompted for
the entries to delete from it.  You can use multi-command completion
for both inputs.  That is, you can act on multiple histories and
delete multiple entries from each.

For convenience, you can use `S-delete' the same way as `C-RET': Each
of them deletes the current entry candidate from the history.

With a prefix argument, empty the chosen history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-1                ; Function to perform the action
  "History to clear: " icicle-clear-history-hist-vars ; `completing-read' args
  nil t nil nil (symbol-name minibuffer-history-variable) nil
  ((pref-arg                        current-prefix-arg) ; Bindings
   (enable-recursive-minibuffers    t)
   (icicle-transform-function       'icicle-remove-duplicates)
   (icicle-clear-history-hist-vars  `((,(symbol-name minibuffer-history-variable))
                                      (,(symbol-name 'icicle-previous-raw-file-name-inputs))
                                      (,(symbol-name 'icicle-previous-raw-non-file-name-inputs))))
   (icicle-delete-candidate-object  'icicle-clear-history-entry))
  (mapatoms #'(lambda (x) (when (and (boundp x) (consp (symbol-value x)) ; First code
                                     (stringp (car (symbol-value x)))
                                     (string-match "-\\(history\\|ring\\)\\'" (symbol-name x)))
                            (push (list (symbol-name x)) icicle-clear-history-hist-vars)))))

;; Free vars here: `pref-arg' is bound in `icicle-clear-history'.
(defun icicle-clear-history-1 (hist)
  "Action function for `icicle-clear-history' history-variable candidates."
  (setq hist  (intern hist))
  (if (not pref-arg)
      (let* ((icicle-candidate-action-fn              'icicle-clear-history-entry)
             (icicle-transform-function               'icicle-remove-duplicates)
             (icicle-clear-history-hist               hist)
             (icicle-use-candidates-only-once-flag    t)
             (icicle-show-Completions-initially-flag  t)
             (icicle-act-before-cycle-flag            t))
        (when hist                      ; Maybe there are no more, due to deletion actions.
          (funcall icicle-candidate-action-fn
                   (completing-read "Clear input: " (mapcar #'list (symbol-value hist)) nil t))))
    (set hist nil))
  (unless hist (message "History `%s' is now empty" hist))
  nil)

;; Free vars here: `icicle-clear-history-hist' is bound in `icicle-clear-history-1'
;; and in `icicle-clear-current-history'.
(defun icicle-clear-history-entry (cand)
  "Action function for history entry candidates in `icicle-clear-history'."
  (unless (string= "" cand)
    (set icicle-clear-history-hist
         (icicle-remove-if
          #'(lambda (x)
              (string= (icicle-substring-no-properties cand) (icicle-substring-no-properties x)))
          (symbol-value icicle-clear-history-hist)))
    ;; We assume here that CAND was in fact present in the history originally.
    (message "`%s' deleted from history `%s'" cand icicle-clear-history-hist))
  nil)

;;;###autoload
(icicle-define-command icicle-clear-current-history ; Bound to `M-i' in minibuffer.
  "Clear current minibuffer history of selected entries.
You are prompted for the history entries to delete.

With a prefix argument, however, empty the history completely
\(you are not prompted to choose history entries to delete).

`icicle-act-before-cycle-flag' is bound to t here during completion of
history entries, so `C-next' and so on act on the current candidate."
  icicle-clear-history-entry            ; Action function
  "Clear input: " (mapcar #'list (symbol-value icicle-clear-history-hist)) ; `completing-read' args
  nil t nil nil nil nil
  ((pref-arg                                current-prefix-arg) ; Bindings
   (enable-recursive-minibuffers            t)
   (icicle-transform-function               'icicle-remove-duplicates)
   (icicle-use-candidates-only-once-flag    t)
   (icicle-show-Completions-initially-flag  t)
   (icicle-clear-history-hist               minibuffer-history-variable))
  (when pref-arg                        ; Use `error' just to exit and make sure message is seen.
    (icicle-ding)
    (if (not (yes-or-no-p (format "Are you sure you want to empty `%s' completely? "
                                  minibuffer-history-variable)))
        (error "")
      (set minibuffer-history-variable nil)
      (error "History `%s' is now empty" minibuffer-history-variable))))

(when (and icicle-define-alias-commands-flag (not (fboundp 'clear-option)))
  (defalias 'clear-option 'icicle-reset-option-to-nil))
;;;###autoload
(icicle-define-command icicle-reset-option-to-nil ; Command name
  "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) nil) (message "`%s' is now nil" opt)) ; Action function
  "Clear option (set it to nil): " obarray ; `completing-read' args
  (if current-prefix-arg
      #'(lambda (x) (and (boundp x) (symbol-value x)))
    #'(lambda (x) (and (boundp x) (user-variable-p x) (symbol-value x))))
  t nil (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history)
  nil nil
  ((enable-recursive-minibuffers          t) ; Bindings
   (icicle-use-candidates-only-once-flag  t)
   (alt-fn                                nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "option")))))

(when (and icicle-define-alias-commands-flag (not (fboundp 'toggle)))
  (defalias 'toggle 'icicle-toggle-option))
;;;###autoload
(icicle-define-command icicle-toggle-option ; Command name
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Action function
    (let ((sym  (intern opt)))
      (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))
  "Toggle value of option: " obarray    ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         'user-variable-p)
        (current-prefix-arg 'boundp)
        (t 'icicle-binary-option-p))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers  t)    ; Bindings
   (alt-fn                        nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "option")))))

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOL is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

;;;###autoload
(icicle-define-command icicle-increment-option ; Command name
  "Increment option's value using the arrow keys (`up', `down').
Completion candidates are limited to options that have `integer',
`float', and `number' custom types.
This command needs library `doremi.el'." ; Doc string
  (lambda (opt)                         ; Action function
    (let ((sym  (intern opt)))
      (icicle-doremi-increment-variable+ sym (icicle-read-number "Increment (amount): ") t)
      (message "`%s' is now %s" opt (eval sym))))
  "Increment value of option: " obarray ; `completing-read' args
  (lambda (symbol) (memq (get symbol 'custom-type) '(number integer float)))  
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers  t)    ; Bindings
   (alt-fn                        nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "option"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "option"))))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))) ; First code

;;;###autoload
(icicle-define-command icicle-increment-variable ; Command name
  "Increment variable's value using the arrow keys (`up', `down').
With a prefix arg, only numeric user options are candidates.
This command needs library `doremi.el'." ; Doc string
  (lambda (opt)                         ; Action function
    (let ((sym  (intern opt)))
      (icicle-doremi-increment-variable+ sym (icicle-read-number "Increment (amount): ") prefix-arg)
      (message "`%s' is now %s" opt (eval sym))))
  "Increment value of variable: " obarray ; `completing-read' args
  (if prefix-arg
      (lambda (symbol) (memq (get symbol 'custom-type) '(number integer float)))
    'boundp)
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers  t)    ; Bindings
   (prefix-arg                    current-prefix-arg)
   (alt-fn                        nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn
        (setq alt-fn  (icicle-alt-act-fn-for-type (if prefix-arg "option" "variable")))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn
        (icicle-alt-act-fn-for-type (if prefix-arg "option" "variable")))))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))) ; First code

;;;###autoload
(defun icicle-doremi-increment-variable+ (variable &optional increment optionp)
  "Increment VARIABLE by INCREMENT (default 1).
Use `up', `down' or mouse wheel to increase or decrease.  You can use
the `Meta' key (e.g. `M-up') to increment in larger steps.

Interactively, you can choose VARIABLE using completion.
With a prefix arg, only user options are available to choose from.
Raises an error if VARIABLE's value is not a number."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                   (and (symbolp (variable-at-point)) (variable-at-point))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Increment variable: " obarray
                                    (if current-prefix-arg 'user-variable-p 'boundp)
                                    t nil nil (and symb (symbol-name symb)) t))
           (icicle-read-number "Increment (amount): ")
           current-prefix-arg)))
  (unless (require 'doremi nil t) (error "This command needs library `doremi.el'."))
  (unless increment (setq increment 1))
  (unless (numberp (symbol-value variable))
    (error "Variable's value is not a number: %S" (symbol-value variable)))
  (doremi (lambda (new-val)
            (set variable  new-val)
            new-val)
          (symbol-value variable)
          increment))

;;;###autoload
(icicle-define-command icicle-bookmark-list ; Command name
  "Choose a list of bookmark names.
If `icicle-bookmark-types' is non-nil, then it is a list of bookmark
types and only bookmarks of those types are candidates.

You can use `S-delete' during completion to delete a candidate bookmark.
The list of bookmark names (strings) is returned." ; Doc string
  (lambda (name) (push (icicle-substring-no-properties (icicle-transform-multi-completion name))
                       bmk-names))      ; Action function
  "Choose bookmark (`RET' when done): " icicle-candidates-alist nil ; `completing-read' args
  (not icicle-show-multi-completion-flag)
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                 bookmark-completion-ignore-case)
   (icicle-list-use-nth-parts              '(1))
   (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                               nil
                                             (if (facep 'file-name-shadow)
                                                 '((2 (face file-name-shadow))
                                                   (3 (face bookmark-menu-heading)))
                                               '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-transform-before-sort-p         t)
   (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
   (types                                  icicle-bookmark-types)
   (icicle-candidates-alist                ())
   (bmk-names                              ())
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 (append
                  '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                    ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
                  (and (icicle-set-intersection types '("info" "region"))
                       '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types '("gnus" "region"))
                       '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types '("w3m" "region"))
                       '(("by w3m url" (bmkp-w3m-cp) icicle-alpha-p)))
                  (and (icicle-set-difference types
                                              '("bookmark-list" "desktop" "gnus" "info" "man" "w3m"))
                       '(("by bookmark type" (bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp
                                              bmkp-local-file-type-cp bmkp-handler-cp)
                          icicle-alpha-p)))
                  (and (icicle-set-difference
                        types '("bookmark-list" "desktop" "dired" "non-file"))
                       '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
                  (and (icicle-set-intersection types
                                                '("local-file" "file" "dired" "region"))
                       '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                         ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                         ("by last local file access"
                          (bmkp-local-file-accessed-more-recently-cp)
                          icicle-alpha-p)
                         ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                          icicle-alpha-p)))
                  (and (not (equal types '("desktop")))
                       '(("by last buffer or file access"
                          (bmkp-buffer-last-access-cp
                           bmkp-local-file-accessed-more-recently-cp)
                          icicle-alpha-p)))
                  (and (get-buffer "*Bookmark List*")
                       '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                          icicle-alpha-p)))))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))         
   (icicle-candidate-help-fn
    #'(lambda (cand)
        (when (and (featurep 'bookmark+) icicle-show-multi-completion-flag)
          (setq cand  (funcall icicle-get-alist-candidate-function cand))
          (setq cand  (cons (caar cand) (cdr cand))))
        (if (featurep 'bookmark+)
            (if current-prefix-arg
                (bmkp-describe-bookmark-internals cand)
              (bmkp-describe-bookmark cand))
          (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand))))))
  (progn
    (message "Gathering bookmarks...")
    (unless types  (setq types '(all)))
    (dolist (type  types)               ; First code
      (setq icicle-candidates-alist
            (nconc icicle-candidates-alist
                   (if (not (featurep 'bookmark+))
                       (mapcar #'(lambda (cand) (list (icicle-candidate-short-help
                                                       (icicle-bookmark-help-string cand)
                                                       (icicle-bookmark-propertize-candidate cand))))
                               (if (eq type 'all)
                                   bookmark-alist
                                 (funcall (intern (format "bmkp-%s-alist-only" type)))))
                     (bookmark-maybe-load-default-file) ; Load bookmarks, define `bookmark-alist'.
                     (mapcar (if icicle-show-multi-completion-flag
                                 #'(lambda (bmk)
                                     (condition-case nil ; Ignore errors, e.g. from bad bookmark.
                                         (let* ((bname     (bookmark-name-from-full-record bmk))
                                                (guts      (bookmark-get-bookmark-record bmk))
                                                (file      (bookmark-get-filename bmk))
                                                (buf       (bmkp-get-buffer-name bmk))
                                                (file/buf
                                                 (if (and buf
                                                          (equal file bmkp-non-file-filename))
                                                     buf
                                                   file))
                                                (tags      (bmkp-get-tags bmk)))
                                           (cons `(,(icicle-candidate-short-help
                                                     (icicle-bookmark-help-string bname)
                                                     (icicle-bookmark-propertize-candidate bname))
                                                   ,file/buf
                                                   ,@(and tags (list (format "%S" tags))))
                                                 guts))
                                       (error nil)))
                               #'(lambda (bmk)
                                   (condition-case nil ; Ignore errors, e.g. from bad bookmark.
                                       (let ((bname  (bookmark-name-from-full-record bmk))
                                             (guts   (bookmark-get-bookmark-record bmk)))
                                         (cons (icicle-candidate-short-help
                                                (icicle-bookmark-help-string bname)
                                                (icicle-bookmark-propertize-candidate bname))
                                               guts))
                                     (error nil))))
                             (bmkp-sort-and-remove-dups
                              (if (eq type 'all)
                                  bookmark-alist
                                (funcall (intern (format "bmkp-%s-alist-only" type)))))))))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (prog1 (setq bmk-names  (nreverse (delete "" bmk-names))) ; Last code - return the list.
    (icicle-bookmark-cleanup)
    (when (interactive-p) (message "Bookmarks: %S" bmk-names))))

;;;###autoload
(defun icicle-bookmark-cmd (&optional parg) ; Bound to what `bookmark-set' is bound to (`C-x r m').
  "Set bookmark or visit bookmark(s).
With a negative prefix arg, visit bookmark(s), using
  `icicle-bookmark-other-window' (see that command for more info).

Otherwise, set a bookmark, as follows:

* No prefix arg: Prompt for the bookmark name.

  If feature `bookmark+' is present:

  . You can use (lax) completion for the bookmark name.
    The candidates are bookmarks in the current buffer (or all
    bookmarks if there are none in this buffer).

  . If the region is active and nonempty, then use the buffer name
    followed by the region prefix as the default name.

  If feature `bookmark+' is not present, then completion is not
  available, and the default bookmark name is the last one used in
  this buffer.

  Note: You can use command `icicle-bookmark-set' with a numeric
  prefix arg if you want to complete against all bookmark names,
  instead of those for the current buffer.

* Plain prefix arg (`C-u'): Same as no prefix arg, but do not
  overwrite any existing bookmark that has the same name.

* Non-negative numeric prefix arg: Do not prompt for bookmark name.
  If feature `bookmark+' is present and the region is active and
    nonempty, then use the buffer name followed by a prefix of the
    region text as the bookmark name.
  Otherwise, use the buffer name followed by the text of the current
    line, starting at point.
  Use at most `icicle-bookmark-name-length-max' chars, in either case.
  If the prefix arg is 0, then do not overwrite any existing bookmark
    that has the same name.

By default, Icicle mode remaps all key sequences that are normally
bound to `bookmark-set' to `icicle-bookmark-cmd'.  If you do not want
this remapping, then customize option `icicle-top-level-key-bindings'.
In particular, you might prefer to remap `bookmark-set' to
`icicle-bookmark-set' (see Note, above)."
  (interactive "P")
  (if (and parg (< (prefix-numeric-value parg) 0))
      (icicle-bookmark-other-window)
    (if (or (not parg) (consp parg))
        (icicle-bookmark-set nil parg 'PSEUDO-INTERACTIVEP)
      (let* ((regionp    (and (featurep 'bookmark+)  transient-mark-mode  mark-active
                              (not (eq (region-beginning) (region-end)))))
             (name-beg   (if regionp (region-beginning) (point)))
             (name-end   (if regionp (region-end) (save-excursion (end-of-line) (point))))
             (def-name   (concat (buffer-name) ": " (buffer-substring name-beg name-end)))
             (trim-name  (replace-regexp-in-string
                          "\n" " " (substring def-name 0 (min icicle-bookmark-name-length-max
                                                              (length def-name))))))
        (message "Setting bookmark `%s'" trim-name) (sit-for 2)
        (bookmark-set trim-name (and parg (or (consp parg)
                                              (zerop (prefix-numeric-value parg)))))))))

;;;###autoload
(defun icicle-bookmark-set (&optional name parg interactivep) ; `C-x r m'
  "With Bookmark+, this is `bookmark-set' with Icicles multi-completions.
In particular, you can use (lax) completion for the bookmark name.
Without library `bookmark+.el', this is the same as vanilla Emacs
`bookmark-set'."
  (interactive (list nil current-prefix-arg t))
  (if (not (featurep 'bookmark+))
      (bookmark-set name parg)
    (unwind-protect
         (let ((enable-recursive-minibuffers           t) ; In case read input, e.g. File changed...
               (completion-ignore-case                 bookmark-completion-ignore-case)
               (prompt                                 "Bookmark: ")
               (icicle-list-use-nth-parts              '(1))
               (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                                           nil
                                                         (if (facep 'file-name-shadow)
                                                             '((2 (face file-name-shadow))
                                                               (3 (face bookmark-menu-heading)))
                                                           '((3 (face bookmark-menu-heading))))))
               (icicle-transform-function              (and (not (interactive-p))
                                                            icicle-transform-function))
               (icicle-whole-candidate-as-text-prop-p  t)
               (icicle-transform-before-sort-p         t)
               (icicle-sort-orders-alist
                (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
                          ("by bookmark name" . icicle-alpha-p))
                        (and (featurep 'bookmark+)
                             '(("by last bookmark access" (bmkp-bookmark-last-access-cp)
                                icicle-alpha-p)
                               ("by bookmark visit frequency" (bmkp-visited-more-cp)
                                icicle-alpha-p)
                               ("by last buffer or file access"
                                (bmkp-buffer-last-access-cp
                                 bmkp-local-file-accessed-more-recently-cp)
                                icicle-alpha-p)
                               ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                                icicle-alpha-p)
                               ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                               ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                               ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                               ("by last local file access"
                                (bmkp-local-file-accessed-more-recently-cp)
                                icicle-alpha-p)
                               ("by last local file update"
                                (bmkp-local-file-updated-more-recently-cp)
                                icicle-alpha-p)
                               ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                               ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                               ("by w3m url" (bmkp-w3m-cp) icicle-alpha-p)
                               ("by bookmark type"
                                (bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp
                                 bmkp-local-file-type-cp bmkp-handler-cp)
                                icicle-alpha-p)))
                        '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
                          ("case insensitive" . icicle-case-insensitive-string-less-p))))
               (icicle-candidate-help-fn
                #'(lambda (cand)
                    (when (and (featurep 'bookmark+) icicle-show-multi-completion-flag)
                      (setq cand  (funcall icicle-get-alist-candidate-function cand))
                      (setq cand  (cons (caar cand) (cdr cand))))
                    (if (featurep 'bookmark+)
                        (if current-prefix-arg
                            (bmkp-describe-bookmark-internals cand)
                          (bmkp-describe-bookmark cand))
                      (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
               (icicle-candidates-alist
                (if (not (featurep 'bookmark+))
                    (mapcar #'(lambda (cand)
                                (list (icicle-candidate-short-help
                                       (icicle-bookmark-help-string cand)
                                       (icicle-bookmark-propertize-candidate cand))))
                            (bookmark-all-names)) ; Loads bookmarks file.
                  (bookmark-maybe-load-default-file) ; Loads bookmarks file.
                  (mapcar (if icicle-show-multi-completion-flag
                              #'(lambda (bmk)
                                  (let* ((bname     (bookmark-name-from-full-record bmk))
                                         (guts      (bookmark-get-bookmark-record bmk))
                                         (tags      (bmkp-get-tags bmk))
                                         (file      (bookmark-get-filename bmk))
                                         (buf       (bmkp-get-buffer-name bmk))
                                         (file/buf
                                          (if (and buf (equal file bmkp-non-file-filename))
                                              buf
                                            file)))
                                    (cons `(,(icicle-candidate-short-help
                                              (icicle-bookmark-help-string bname)
                                              (icicle-bookmark-propertize-candidate bname))
                                            ,file/buf
                                            ,@(and tags (list (format "%S" tags))))
                                          guts)))
                            #'(lambda (bmk)
                                (let ((bname  (bookmark-name-from-full-record bmk))
                                      (guts   (bookmark-get-bookmark-record bmk)))
                                  (cons (icicle-candidate-short-help
                                         (icicle-bookmark-help-string bname)
                                         (icicle-bookmark-propertize-candidate bname))
                                        guts))))
                          (bmkp-sort-and-remove-dups
                           (and (or (not parg) (consp parg)) ; No numeric PARG: all bookmarks.
                                (or (bmkp-specific-buffers-alist-only)
                                    bookmark-alist)))))))
           (require 'bookmark)
           (when (featurep 'bookmark+)
             ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
             (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
               (define-key (symbol-value map) "\C-\M-b" 'icicle-bookmark-non-file-narrow) ; `C-M-b'
               (define-key (symbol-value map) "\C-\M-d" 'icicle-bookmark-dired-narrow) ; `C-M-d'
               (define-key (symbol-value map) "\C-\M-f" 'icicle-bookmark-file-narrow) ; `C-M-f'
               (define-key (symbol-value map) "\C-\M-g" 'icicle-bookmark-gnus-narrow) ; `C-M-g'
               (define-key (symbol-value map) "\C-\M-m" 'icicle-bookmark-man-narrow) ; `C-M-m'
               (define-key (symbol-value map) "\C-\M-r" 'icicle-bookmark-region-narrow) ; `C-M-r'
               (define-key (symbol-value map) "\C-\M-w" 'icicle-bookmark-w3m-narrow) ; `C-M-w'
               (define-key (symbol-value map) "\C-\M-@" 'icicle-bookmark-remote-file-narrow) ; C-M-@
               (define-key (symbol-value map) [(control meta ?=) ?b] ; `C-M-= b'
                 'icicle-bookmark-specific-buffers-narrow)
               (define-key (symbol-value map) [(control meta ?=) ?f] ; `C-M-= f'
                 'icicle-bookmark-specific-files-narrow)
               (define-key (symbol-value map) [(control meta ?\.)] ; `C-M-= .'
                 'icicle-bookmark-this-buffer-narrow)
               (define-key (symbol-value map) [(control meta ?B)] ; `C-M-B'
                 'icicle-bookmark-bookmark-list-narrow)
               (define-key (symbol-value map) [(control meta ?F)] ; `C-M-F'
                 'icicle-bookmark-local-file-narrow)
               (define-key (symbol-value map) [(control meta ?I)] ; `C-M-I'
                 'icicle-bookmark-info-narrow)
               (define-key (symbol-value map) [(control meta ?K)] ; `C-M-K'
                 'icicle-bookmark-desktop-narrow)))
           (setq bookmark-current-point   (point)
                 bookmark-current-buffer  (current-buffer))
           (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
           (let* ((record   (bookmark-make-record))
                  (regionp  (and transient-mark-mode mark-active (not (eq (mark) (point)))))
                  (regname  (concat (buffer-name) ": "
                                    (buffer-substring (if regionp (region-beginning) (point))
                                                      (if regionp
                                                          (region-end)
                                                        (save-excursion (end-of-line) (point))))))
                  (defname  (bmkp-replace-regexp-in-string
                             "\n" " "
                             (cond (regionp
                                    (save-excursion
                                      (goto-char (region-beginning))
                                      (skip-chars-forward " ") (setq bookmark-yank-point  (point)))
                                    (substring regname 0 (min bmkp-bookmark-name-length-max
                                                              (length regname))))
                                   ((eq major-mode 'w3m-mode) w3m-current-title)
                                   ((eq major-mode 'gnus-summary-mode)
                                    (elt (gnus-summary-article-header) 1))
                                   ((memq major-mode '(Man-mode woman-mode))
                                    (buffer-substring (point-min) (save-excursion
                                                                    (goto-char (point-min))
                                                                    (skip-syntax-forward "^ ")
                                                                    (point))))
                                   (t (car record)))))
                  (bname    (or name
                                (icicle-transform-multi-completion
                                 (bmkp-completing-read-lax "Set bookmark " defname
                                                                icicle-candidates-alist
                                                                nil bookmark-history)))))
             (when (string-equal bname "") (setq bname  defname))
             (bookmark-store bname (cdr record) (consp parg))
             (when (and bmkp-prompt-for-tags-flag interactivep)
               (bmkp-add-tags bname (bmkp-read-tags-completing)))
             (case (and (boundp 'bmkp-auto-light-when-set) bmkp-auto-light-when-set)
               (autonamed-bookmark       (when (bmkp-autonamed-bookmark-p bname)
                                           (bmkp-light-bookmark bname)))
               (non-autonamed-bookmark   (unless (bmkp-autonamed-bookmark-p bname)
                                           (bmkp-light-bookmark bname)))
               (any-bookmark             (bmkp-light-bookmark bname))
               (autonamed-in-buffer      (bmkp-light-bookmarks
                                          (bmkp-remove-if-not
                                           #'bmkp-autonamed-bookmark-p
                                           (bmkp-this-buffer-alist-only)) nil 'MSG))
               (non-autonamed-in-buffer  (bmkp-light-bookmarks
                                          (bmkp-remove-if
                                           #'bmkp-autonamed-bookmark-p
                                           (bmkp-this-buffer-alist-only)) nil 'MSG))
               (all-in-buffer            (bmkp-light-bookmarks-this-buffer nil 'MSG)))
             (run-hooks 'bmkp-after-set-hook)
             (if bookmark-use-annotations
                 (bookmark-edit-annotation bname)
               (goto-char bookmark-current-point))))
      (icicle-bookmark-cleanup))))

;;;###autoload
(icicle-define-command icicle-bookmark  ; Command name
  "Jump to a bookmark.
With a plain prefix argument (`C-u'), reverse the effect of option
`icicle-bookmark-refresh-cache-flag'.

During completion, you can use `S-delete' on a bookmark to delete it.

If you also use library `bookmark+.el', then:

 * `C-M-RET' shows detailed info about the current bookmark candidate.
   `C-u C-M-RET' shows the complete, internal info for the bookmark.
   Likewise, for the other candidate help keys: `C-M-down' etc.
   (And the mode line always shows summary info about the bookmark.)
   
 * You can use `C-,' to sort bookmarks in many different ways,
   according to their properties.

 * In *Completions*, the candidate bookmarks are highlighted according
   to their type.  You can customize the highlighting faces used:

  `bmkp-bad-bookmark'              - possibly bad bookmark
  `bmkp-bookmark-list'             - bookmark list
  `bmkp-buffer'                    - buffer
  `bmkp-desktop'                   - desktop
  `bmkp-function'                  - function bookmark
  `bmkp-gnus'                      - Gnus article
  `bmkp-info'                      - Info node
  `bmkp-local-directory'           - local directory
  `bmkp-local-file-with-region'    - local file with a region
  `bmkp-local-file-without-region' - local file without a region
  `bmkp-man'                       - `man' page
  `bmkp-non-file'                  - non-file (no current buffer)
  `bmkp-remote-file'               - remote-file
  `bmkp-sequence'                  - sequence bookmark
  `bmkp-w3m'                       - W3m URL

 * In *Completions*, if option `icicle-show-multi-completion-flag' is
   non-nil, then each completion candidate is a multi-completion:

    a. the bookmark name
    b. the bookmark file or buffer name
    c. any tags

   You can match any parts of the multi-completion.  You can toggle
   the option (for the next command) using `M-m' during completion.
   For example, you can match all bookmarks that have tags by typing:

     C-M-j . * C-M-j S-TAB

   (Each `C-M-j' inserts `^G\n', which is `icicle-list-join-string'.)

 * You can narrow the current completion candidates to bookmarks of a
   given type:

   `C-M-b'   - non-file (buffer) bookmarks
   `C-M-B'   - bookmark-list bookmarks
   `C-M-d'   - Dired bookmarks
   `C-M-f'   - file bookmarks
   `C-M-F'   - local-file bookmarks
   `C-M-g'   - Gnus bookmarks
   `C-M-I'   - Info bookmarks
   `C-M-K'   - desktop bookmarks
   `C-M-m'   - `man' pages
   `C-M-r'   - bookmarks with regions
   `C-M-w'   - W3m bookmarks
   `C-M-@'   - remote-file bookmarks
   `C-M-.'   - bookmarks for the current buffer
   `C-M-= b' - bookmarks for specific buffers
   `C-M-= f' - bookmarks for specific files

   See also the individual multi-commands for different bookmark
   types: `icicle-bookmark-info-other-window' etc.

If you also use library `crosshairs.el', then the visited bookmark
position is highlighted."               ; Doc string
  (lambda (cand) (icicle-bookmark-jump (icicle-transform-multi-completion cand))) ; Action
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                 bookmark-completion-ignore-case)
   (prompt                                 "Bookmark: ")
   (icicle-list-use-nth-parts              '(1))
   (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                               nil
                                             (if (facep 'file-name-shadow)
                                                 '((2 (face file-name-shadow))
                                                   (3 (face bookmark-menu-heading)))
                                               '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-transform-before-sort-p         t)
   (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                   ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p)
                   ("by last buffer or file access" (bmkp-buffer-last-access-cp
                                                     bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                    icicle-alpha-p)
                   ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                   ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                   ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                   ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                    icicle-alpha-p)
                   ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                   ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                   ("by w3m url" (bmkp-w3m-cp) icicle-alpha-p)
                   ("by bookmark type" (bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp
                                        bmkp-local-file-type-cp bmkp-handler-cp)
                    icicle-alpha-p)))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))
   (icicle-candidate-help-fn
    #'(lambda (cand)
        (when (and (featurep 'bookmark+) icicle-show-multi-completion-flag)
          (setq cand  (funcall icicle-get-alist-candidate-function cand))
          (setq cand  (cons (caar cand) (cdr cand))))
        (if (featurep 'bookmark+)
            (if current-prefix-arg
                (bmkp-describe-bookmark-internals cand)
              (bmkp-describe-bookmark cand))
          (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar #'(lambda (cand)
                    (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                       (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            (cons `(,(icicle-candidate-short-help
                                      (icicle-bookmark-help-string bname)
                                      (icicle-bookmark-propertize-candidate bname))
                                    ,file/buf
                                    ,@(and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)
                                (not (consp current-prefix-arg)))
                           (and icicle-bookmark-refresh-cache-flag (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist
                        (bmkp-sort-and-remove-dups bookmark-alist)))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (define-key (symbol-value map) "\C-\M-b" 'icicle-bookmark-non-file-narrow) ; `C-M-b'
        (define-key (symbol-value map) "\C-\M-d" 'icicle-bookmark-dired-narrow) ; `C-M-d'
        (define-key (symbol-value map) "\C-\M-f" 'icicle-bookmark-file-narrow) ; `C-M-f'
        (define-key (symbol-value map) "\C-\M-g" 'icicle-bookmark-gnus-narrow) ; `C-M-g'
        (define-key (symbol-value map) "\C-\M-m" 'icicle-bookmark-man-narrow) ; `C-M-m'
        (define-key (symbol-value map) "\C-\M-r" 'icicle-bookmark-region-narrow) ; `C-M-r'
        (define-key (symbol-value map) "\C-\M-w" 'icicle-bookmark-w3m-narrow) ; `C-M-w'
        (define-key (symbol-value map) "\C-\M-@" 'icicle-bookmark-remote-file-narrow) ; `C-M-@'
        (define-key (symbol-value map) [(control meta ?=) ?b] ; `C-M-= b'
          'icicle-bookmark-specific-buffers-narrow)
        (define-key (symbol-value map) [(control meta ?=) ?f] ; `C-M-= f'
          'icicle-bookmark-specific-files-narrow)
        (define-key (symbol-value map) [(control meta ?\.)] ; `C-M-= .'
          'icicle-bookmark-this-buffer-narrow)
        (define-key (symbol-value map) [(control meta ?B)] ; `C-M-B'
          'icicle-bookmark-bookmark-list-narrow)
        (define-key (symbol-value map) [(control meta ?F)] ; `C-M-F'
          'icicle-bookmark-local-file-narrow)
        (define-key (symbol-value map) [(control meta ?I)] ; `C-M-I'
          'icicle-bookmark-info-narrow)
        (define-key (symbol-value map) [(control meta ?K)] ; `C-M-K'
          'icicle-bookmark-desktop-narrow))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

;;;###autoload
(icicle-define-command icicle-bookmark-other-window ; Command name
  "Jump to a bookmark in another window.
Same as `icicle-bookmark', but uses another window." ; Doc string
  (lambda (cand) (icicle-bookmark-jump-other-window (icicle-transform-multi-completion cand)))
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                 bookmark-completion-ignore-case)
   (prompt                                 "Bookmark: ")
   (icicle-list-use-nth-parts              '(1))
   (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                               nil
                                             (if (facep 'file-name-shadow)
                                                 '((2 (face file-name-shadow))
                                                   (3 (face bookmark-menu-heading)))
                                               '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p  t)
   (icicle-transform-before-sort-p         t)
   (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                   ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p)
                   ("by last buffer or file access" (bmkp-buffer-last-access-cp
                                                     bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                    icicle-alpha-p)
                   ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                   ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                   ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                   ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                    icicle-alpha-p)
                   ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                   ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                   ("by w3m url" (bmkp-w3m-cp) icicle-alpha-p)
                   ("by bookmark type" (bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp
                                        bmkp-local-file-type-cp bmkp-handler-cp)
                    icicle-alpha-p)))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))
   (icicle-candidate-help-fn
    #'(lambda (cand)
        (when (and (featurep 'bookmark+) icicle-show-multi-completion-flag)
          (setq cand  (funcall icicle-get-alist-candidate-function cand))
          (setq cand  (cons (caar cand) (cdr cand))))
        (if (featurep 'bookmark+)
            (if current-prefix-arg
                (bmkp-describe-bookmark-internals cand)
              (bmkp-describe-bookmark cand))
          (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar #'(lambda (cand)
                    (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                       (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            (cons `(,(icicle-candidate-short-help
                                      (icicle-bookmark-help-string bname)
                                      (icicle-bookmark-propertize-candidate bname))
                                    ,file/buf
                                    ,@(and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)
                                (not (consp current-prefix-arg)))
                           (and icicle-bookmark-refresh-cache-flag (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist
                        (bmkp-sort-and-remove-dups bookmark-alist)))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (define-key (symbol-value map) "\C-\M-b" 'icicle-bookmark-non-file-narrow) ; `C-M-b'
        (define-key (symbol-value map) "\C-\M-d" 'icicle-bookmark-dired-narrow) ; `C-M-d'
        (define-key (symbol-value map) "\C-\M-f" 'icicle-bookmark-file-narrow) ; `C-M-f'
        (define-key (symbol-value map) "\C-\M-g" 'icicle-bookmark-gnus-narrow) ; `C-M-g'
        (define-key (symbol-value map) "\C-\M-m" 'icicle-bookmark-man-narrow) ; `C-M-m'
        (define-key (symbol-value map) "\C-\M-r" 'icicle-bookmark-region-narrow) ; `C-M-r'
        (define-key (symbol-value map) "\C-\M-w" 'icicle-bookmark-w3m-narrow) ; `C-M-w'
        (define-key (symbol-value map) "\C-\M-@" 'icicle-bookmark-remote-file-narrow) ; `C-M-@'
        (define-key (symbol-value map) [(control meta ?=) ?b] ; `C-M-= b'
          'icicle-bookmark-specific-buffers-narrow)
        (define-key (symbol-value map) [(control meta ?=) ?f] ; `C-M-= f'
          'icicle-bookmark-specific-files-narrow)
        (define-key (symbol-value map) [(control meta ?\.)] ; `C-M-= .'
          'icicle-bookmark-this-buffer-narrow)
        (define-key (symbol-value map) [(control meta ?B)] ; `C-M-B'
          'icicle-bookmark-bookmark-list-narrow)
        (define-key (symbol-value map) [(control meta ?F)] ; `C-M-F'
          'icicle-bookmark-local-file-narrow)
        (define-key (symbol-value map) [(control meta ?I)] ; `C-M-I'
          'icicle-bookmark-info-narrow)
        (define-key (symbol-value map) [(control meta ?K)] ; `C-M-K'
          'icicle-bookmark-desktop-narrow))))
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

(defun icicle-bookmark-delete-action (cand)
  "Delete bookmark candidate CAND, then update `bmkp-sorted-alist'."
  (bookmark-delete (icicle-transform-multi-completion cand))
  (when (or (and (not icicle-bookmark-refresh-cache-flag)
                 (not (consp current-prefix-arg)))
            (and icicle-bookmark-refresh-cache-flag (consp current-prefix-arg)))
    (setq bmkp-sorted-alist (bmkp-sort-and-remove-dups bookmark-alist))))

(defun icicle-bookmark-propertize-candidate (cand)
  "Return bookmark name CAND, with a face indicating its type."
  (when (featurep 'bookmark+)
    (put-text-property
     0 (length cand) 'face
     (cond ((bmkp-sequence-bookmark-p cand)        'bmkp-sequence)
           ((bmkp-function-bookmark-p cand)        'bmkp-function)
           ((bmkp-bookmark-list-bookmark-p cand)   'bmkp-bookmark-list)
           ((bmkp-desktop-bookmark-p cand)         'bmkp-desktop)
           ((bmkp-info-bookmark-p cand)            'bmkp-info)
           ((bmkp-man-bookmark-p cand)             'bmkp-man)
           ((bmkp-gnus-bookmark-p cand)            'bmkp-gnus)
           ((bmkp-w3m-bookmark-p cand)             'bmkp-w3m)
           ((bmkp-remote-file-bookmark-p cand)     'bmkp-remote-file)
           ((and (bmkp-file-bookmark-p cand)
                 (file-directory-p
                  (bookmark-get-filename cand)))        'bmkp-local-directory)
           ((and (bmkp-local-file-bookmark-p cand)
                 (bmkp-region-bookmark-p cand))    'bmkp-local-file-with-region)
           ((bmkp-local-file-bookmark-p cand)      'bmkp-local-file-without-region)
           ((and (bmkp-get-buffer-name cand)
                 (get-buffer (bmkp-get-buffer-name cand))
                 (equal (bookmark-get-filename cand)
                        bmkp-non-file-filename))   'bmkp-buffer)
           ((not (bmkp-file-bookmark-p cand))      'bmkp-non-file)
           (t                                           'bmkp-bad-bookmark))
     cand))
  cand)

(defun icicle-bookmark-jump (bookmark)
  "Jump to BOOKMARK.
If `crosshairs.el' is loaded, then highlight the target position.
You probably don't want to use this.  Use `icicle-bookmark' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark))

(defun icicle-bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK in another window.
If `crosshairs.el' is loaded, then highlight the target position.
You probably don't want to use this.  Use
`icicle-bookmark-other-window' instead."
  (interactive (list (bookmark-completing-read "Jump to bookmark (other window)"
                                               bookmark-current-bookmark)))
  (icicle-bookmark-jump-1 bookmark 'other-window))

(defun icicle-bookmark-jump-1 (bookmark &optional other-window-p)
  "Helper function for `icicle-bookmark-jump(-other-window)'."
  (unless bookmark (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (if (fboundp 'bookmark--jump-via)
      (bookmark--jump-via bookmark (if other-window-p 'pop-to-buffer 'switch-to-buffer))
    (let ((cell  (bookmark-jump-noselect bookmark))) ; Emacs < 23 and without `bookmark+.el'.
      (when cell
        (if other-window-p
            (pop-to-buffer (car cell) 'other-window)
          (switch-to-buffer (car cell)))
        (goto-char (cdr cell))
        (progn (run-hooks 'bookmark-after-jump-hook) t)
        ;; If there is an annotation for this bookmark, show it in a buffer.
        (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark)))))
  ;; Unless `bmkp-use-region-flag' and bookmark has a region, show position using crosshairs.
  (unless (and (boundp 'bmkp-use-region-flag) bmkp-use-region-flag
               (fboundp 'bmkp-get-end-position) (bmkp-get-end-position bookmark)
               (/= (bookmark-get-position bookmark) (bmkp-get-end-position bookmark)))
    (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))))
;; $$$$$$   (select-window (minibuffer-window))
;; $$$$$$   (select-frame-set-input-focus (selected-frame)))

(defun icicle-bookmark-help-string (bookmark-name)
  "Return a help string for BOOKMARK-NAME." ; `bmkp-*' functions are defined in `bookmark+.el'.
  ;; Use BOOKMARK-NAME, not full bookmark BMK, as arg to vanilla bookmark functions, for Emacs < 23.
  (let* ((bmk         (bookmark-get-bookmark bookmark-name))
         (buf         (and (fboundp 'bmkp-get-buffer-name) (bmkp-get-buffer-name bmk)))
         (file        (bookmark-get-filename bookmark-name))
         (start       (bookmark-get-position bookmark-name))
         (end         (and (fboundp 'bmkp-get-end-position) (bmkp-get-end-position bmk)))
         (annot       (bookmark-get-annotation bookmark-name))
         (sequence-p  (and (fboundp 'bmkp-sequence-bookmark-p)
                           (bmkp-sequence-bookmark-p bmk)))
         (function-p  (and (fboundp 'bmkp-function-bookmark-p)
                           (bmkp-function-bookmark-p bmk)))
         (blist-p     (and (fboundp 'bmkp-bookmark-list-bookmark-p)
                           (bmkp-bookmark-list-bookmark-p bmk)))
         (desktop-p   (and (fboundp 'bmkp-desktop-bookmark-p)
                           (bmkp-desktop-bookmark-p bmk)))
         (dired-p     (and (fboundp 'bmkp-dired-bookmark-p) (bmkp-dired-bookmark-p bmk)))
         (gnus-p      (and (fboundp 'bmkp-gnus-bookmark-p) (bmkp-gnus-bookmark-p bmk)))
         (info-p      (and (fboundp 'bmkp-info-bookmark-p) (bmkp-info-bookmark-p bmk)))
         (man-p       (and (fboundp 'bmkp-man-bookmark-p) (bmkp-man-bookmark-p bmk)))
         (w3m-p       (and (fboundp 'bmkp-w3m-bookmark-p) (bmkp-w3m-bookmark-p bmk)))
         type-info-p no-position-p)
    (when (or sequence-p function-p) (setq no-position-p  t))
    (concat (setq type-info-p
                  (cond (sequence-p (format "Sequence: %S" (bookmark-prop-get bmk 'sequence)))
                        (function-p (let ((fn  (bookmark-prop-get bmk 'function)))
                                      (if (symbolp fn) (format "Function: `%s'" fn) "Function")))
                        (desktop-p  "Desktop, ")
                        (dired-p    (format "Dired %s, " file))
                        (gnus-p     "Gnus, ")
                        (info-p     "Info, ")
                        (man-p      (let ((man-args  (bookmark-prop-get bmk 'man-args)))
                                      (if man-args
                                          (format "`man %s', " man-args)
                                        ;; WoMan has no variable for the cmd name.
                                        (format "%s, " (bookmark-prop-get bmk 'buffer-name)))))
                        (w3m-p      "W3M URL, ")
                        (t nil)))
            (and (not dired-p)
                 (or (and file (or (not (boundp 'bmkp-non-file-filename))
                                   (not (equal file bmkp-non-file-filename)))
                          (format (if type-info-p "file `%s', " "File `%s', ") file))
                     (and buf (format (if type-info-p "buffer `%s', " "Buffer `%s', ") buf))))
            (and (not no-position-p)
                 (if (and end (> (- end start) 0))
                     (format "from %d to %d (%d chars)" start end (- end start))
                   (format "position %d" start)))
            (and annot (format ", %s" annot)))))

;;; MUST keep this synchronized with any general Icicle-mode `C-M-' bindings in `icicles-mode.el'.
;;  That includes things like `icicle-read+insert-file-name-keys'.
(defun icicle-bookmark-cleanup ()
  "Cleanup code for `icicle-bookmark'.
Remove crosshairs highlighting and unbind filtering keys."
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (featurep 'bookmark+)
    (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
      (define-key (symbol-value map) "\C-\M-b" nil) ; `C-M-b'
      (define-key (symbol-value map) [(control meta ?B)] nil) ; `C-M-B'
      (define-key (symbol-value map) "\C-\M-d" nil) ; `C-M-d'
      (define-key (symbol-value map) "\C-\M-f" nil) ; `C-M-f'
      (define-key (symbol-value map) [(control meta ?F)] nil) ; `C-M-F'
      (dolist (key  icicle-read+insert-file-name-keys) ; `C-M-F' - overrides previous.
        (define-key (symbol-value map) key 'icicle-read+insert-file-name))
      (define-key (symbol-value map) "\C-\M-g" nil) ; `C-M-g'
      (define-key (symbol-value map) [(control meta ?I)] nil) ; `C-M-I' (`C-M-i' is `ESC TAB')
      (define-key (symbol-value map) [(control meta ?K)] nil) ; `C-M-K'
      (define-key (symbol-value map) "\C-\M-m" nil) ; `C-M-m'
      (define-key (symbol-value map) "\C-\M-r" nil) ; `C-M-r'
      (define-key (symbol-value map) "\C-\M-w" nil) ; `C-M-w'
      (define-key (symbol-value map) "\C-\M-@" nil) ; `C-M-@'
      (define-key (symbol-value map) [(control meta ?\.)] ; `C-M-.'
        'icicle-toggle-dot)             ; `icicles-mode.el'.
      (define-key (symbol-value map) [(control meta ?=) ?b] nil) ; `C-M-= b'
      (define-key (symbol-value map) [(control meta ?=) ?f] nil)))) ; `C-M-= f'

(defun icicle-bookmark-cleanup-on-quit ()
  "Do `icicle-bookmark-cleanup', then return to original window."
  (icicle-bookmark-cleanup)
  (when (window-live-p orig-window)     ; `orig-window' is free here.
    (select-window orig-window)
    (select-frame-set-input-focus (selected-frame))))

;;; These are minibuffer commands, but we define them here instead of in `icicles-mcmd.el'.

(defun icicle-bookmark-bookmark-list-narrow () ; Bound to `C-M-B' in minibuffer for completion.
  "Narrow the bookmark candidates to bookmark-list bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-bookmark-list-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-desktop-narrow ()   ; Bound to `C-M-K' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to desktop bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-desktop-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-dired-narrow ()   ; Bound to `C-M-d' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Dired bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-dired-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-file-narrow ()   ; Bound to `C-M-f' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-file-bookmark-p (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-gnus-narrow ()   ; Bound to `C-M-g' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Gnus bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-gnus-bookmark-p (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-info-narrow ()   ; Bound to `C-M-I' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to Info bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-info-bookmark-p (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-local-file-narrow () ; Bound to `C-M-F' for bookmark completion.
  "Narrow the bookmark candidates to local-file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-local-file-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-man-narrow () ; Bound to `C-M-m' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to `man'-page bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-man-bookmark-p (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-non-file-narrow () ; Bound to `C-M-b' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to non-file (buffer-only) bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-non-file-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-region-narrow () ; Bound to `C-M-r' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to bookmarks with regions."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-region-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-remote-file-narrow () ; Bound to `C-M-@' in minibuf for bookmark completion.
  "Narrow the bookmark candidates to remote-file bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-remote-file-bookmark-p
                  (funcall icicle-get-alist-candidate-function (car x))))))

(defun icicle-bookmark-specific-buffers-narrow (buffers) ; `C-M-= b' for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for specific BUFFERS.
You are prompted for the BUFFERS."
  (interactive (let ((icicle-completion-candidates  icicle-completion-candidates))
                 (list (icicle-bookmarked-buffer-list))))
  (icicle-narrow-candidates-with-predicate
   `(lambda (x)
     (member (bmkp-get-buffer-name (funcall icicle-get-alist-candidate-function (car x)))
      ',buffers))))

(defun icicle-bookmark-specific-files-narrow (files) ; `C-M-= f' in minibuf for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for specific FILES.
You are prompted for the FILES."
  (interactive (list (icicle-bookmarked-file-list)))
  (icicle-narrow-candidates-with-predicate
   `(lambda (x)
     (member (bookmark-get-filename (funcall icicle-get-alist-candidate-function (car x)))
      ',files))))

(defun icicle-bookmark-this-buffer-narrow () ; `C-M-.' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to bookmarks for the current buffer."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x)
       (with-current-buffer orig-buff
         (bmkp-this-buffer-p (funcall icicle-get-alist-candidate-function (car x)))))))

(defun icicle-bookmark-w3m-narrow ()    ; Bound to `C-M-w' in minibuffer for bookmark completion.
  "Narrow the bookmark candidates to W3m bookmarks."
  (interactive)
  (icicle-narrow-candidates-with-predicate
   #'(lambda (x) (bmkp-w3m-bookmark-p (funcall icicle-get-alist-candidate-function (car x))))))

(defmacro icicle-define-bookmark-command-1 (otherp type prompt args)
  "Helper macro for `icicle-define*-bookmark-command' macros.
The command defined raises an error unless library `bookmark+.el' can
be loaded."
  `(icicle-define-command
    ,(intern (format "icicle-bookmark-%s%s" type (if otherp "-other-window" ""))) ; Command name
    ,(format "Jump to %s bookmark%s.
Like `icicle-bookmark%s', but with %s bookmarks only.
You need library `bookmark+.el' for this command."
             type (if otherp " in other window" "")
             (if otherp "-other-window" "") type) ; Doc string
    (lambda (cand) (,(if otherp 'icicle-bookmark-jump-other-window 'icicle-bookmark-jump)
                     (icicle-transform-multi-completion cand)))
    prompt1 icicle-candidates-alist nil ; `completing-read' args
    (not icicle-show-multi-completion-flag)
    nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
    nil nil
    ((IGNORED1                               (unless (require 'bookmark+ nil t)
                                               (error "You need library `bookmark+.el' for this \
command")))
     (IGNORED2                               (bookmark-maybe-load-default-file)) ; `bookmark-alist'.
     (enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
     (completion-ignore-case                 bookmark-completion-ignore-case) ; Additional bindings
     (prompt1                                ,(or prompt
                                                  (format "%s%s bookmark: "
                                                          (capitalize (substring type 0 1))
                                                          (substring type 1 (length type)))))
     (icicle-list-use-nth-parts              '(1))
     (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                                 nil
                                               (if (facep 'file-name-shadow)
                                                   '((2 (face file-name-shadow))
                                                     (3 (face bookmark-menu-heading)))
                                                 '((3 (face bookmark-menu-heading))))))
     (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
     (icicle-whole-candidate-as-text-prop-p  t)
     (icicle-transform-before-sort-p         t)
     (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
     (icicle-sort-orders-alist
      (append
       '(("in *Bookmark List* order")   ; Renamed from "turned OFF'.
         ("by bookmark name" . icicle-alpha-p)
         ("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
         ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
       (and (member ,type '("info" "region"))
        '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
       (and (member ,type '("gnus" "region"))
        '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
       (and (member ,type '("w3m" "region"))
        '(("by w3m url" (bmkp-w3m-cp) icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "gnus" "info" "man" "w3m")))
        '(("by bookmark type" (bmkp-info-cp bmkp-gnus-cp bmkp-w3m-cp
                               bmkp-local-file-type-cp bmkp-handler-cp)
           icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "dired" "non-file")))
        '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
       (and (member ,type '("local-file" "file" "dired" "region"))
        '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
          ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
          ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)
          ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
           icicle-alpha-p)))
       (and (not (string= ,type "desktop"))
        '(("by last buffer or file access" (bmkp-buffer-last-access-cp
                                            bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)))
       (and (get-buffer "*Bookmark List*")
        '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
           icicle-alpha-p)))
       '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
         ("case insensitive" . icicle-case-insensitive-string-less-p))))
     (icicle-candidate-help-fn
      #'(lambda (cand)
          (when icicle-show-multi-completion-flag
            (setq cand  (funcall icicle-get-alist-candidate-function cand))
            (setq cand  (cons (caar cand) (cdr cand))))
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))))
     (icicle-candidates-alist
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            ;; Emacs 20 byte-compiler bug prevents using backslash syntax here.
                            (cons (append (list (icicle-candidate-short-help
                                                 (icicle-bookmark-help-string bname)
                                                 (icicle-bookmark-propertize-candidate bname))
                                                file/buf)
                                          (and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
       (bmkp-sort-and-remove-dups
        (funcall ',(intern (format "bmkp-%s-alist-only" type)) ,@args)))))
    nil                                 ; First code
    (icicle-bookmark-cleanup-on-quit)   ; Undo code
    (icicle-bookmark-cleanup)))         ; Last code

(defmacro icicle-define-bookmark-command (type &optional prompt &rest args)
  "Define an Icicles multi-command for jumping to bookmarks of type TYPE.
TYPE is a string to be used for the doc string, default prompt, and in
 function names.  It should be lowercase and contain no spaces.
Optional arg PROMPT is the completion prompt.
ARGS is a list of any additional arguments to be passed to the
appropriate `bmkp-TYPE-alist-only' function."
  `(icicle-define-bookmark-command-1 nil ,type ,prompt ,args))

(defmacro icicle-define-bookmark-other-window-command (type &optional prompt &rest args)
  "Same as `icicle-define-bookmark-command', but command uses other window."
  `(icicle-define-bookmark-command-1 t ,type ,prompt ,args))

;; The following sexps macro-expand to define these commands:
;;  `icicle-bookmark-bookmark-list',
;;  `icicle-bookmark-desktop',
;;  `icicle-bookmark-dired',                `icicle-bookmark-dired-other-window',
;;  `icicle-bookmark-file',                 `icicle-bookmark-file-other-window',
;;  `icicle-bookmark-gnus',                 `icicle-bookmark-gnus-other-window',
;;  `icicle-bookmark-info',                 `icicle-bookmark-info-other-window',
;;  `icicle-bookmark-local-file',           `icicle-bookmark-local-file-other-window',
;;  `icicle-bookmark-man',                  `icicle-bookmark-man-other-window',
;;  `icicle-bookmark-non-file',             `icicle-bookmark-non-file-other-window',
;;  `icicle-bookmark-region',               `icicle-bookmark-region-other-window',
;;  `icicle-bookmark-remote-file',          `icicle-bookmark-remote-file-other-window',
;;  `icicle-bookmark-specific-buffers',     `icicle-bookmark-specific-buffers-other-window'
;;  `icicle-bookmark-specific-files',       `icicle-bookmark-specific-files-other-window'
;;  `icicle-bookmark-all-tags',             `icicle-bookmark-all-tags-other-window'
;;  `icicle-bookmark-all-tags-regexp',      `icicle-bookmark-all-tags-regexp-other-window'
;;  `icicle-bookmark-some-tags',            `icicle-bookmark-some-tags-other-window'
;;  `icicle-bookmark-some-tags-regexp',     `icicle-bookmark-some-tags-regexp-other-window'
;;  `icicle-bookmark-this-buffer',          `icicle-bookmark-this-buffer-other-window'
;;  `icicle-bookmark-w3m',                  `icicle-bookmark-w3m-other-window'

;; Other-window means nothing for a bookmark list or a desktop.
(icicle-define-bookmark-command              "non-file")                      ; `C-x j b'
(icicle-define-bookmark-other-window-command "non-file")                      ; `C-x 4 j b'
(icicle-define-bookmark-command              "bookmark-list")                 ; `C-x j B'
(icicle-define-bookmark-command              "dired")                         ; `C-x j d'
(icicle-define-bookmark-other-window-command "dired")                         ; `C-x 4 j d'
(icicle-define-bookmark-command              "file")                          ; `C-x j f'
(icicle-define-bookmark-other-window-command "file")                          ; `C-x 4 j f'
(icicle-define-bookmark-command              "gnus")                          ; `C-x j g'
(icicle-define-bookmark-other-window-command "gnus")                          ; `C-x 4 j g'
(icicle-define-bookmark-command              "info")                          ; `C-x j i'
(icicle-define-bookmark-other-window-command "info")                          ; `C-x 4 j i'
(icicle-define-bookmark-command              "desktop")                       ; `C-x j K'
(icicle-define-bookmark-command              "local-file")                    ; `C-x j l'
(icicle-define-bookmark-other-window-command "local-file")                    ; `C-x 4 j l'
(icicle-define-bookmark-command              "man")                           ; `C-x j m'
(icicle-define-bookmark-other-window-command "man")                           ; `C-x 4 j m'
(icicle-define-bookmark-command              "remote-file")                   ; `C-x j n'
(icicle-define-bookmark-other-window-command "remote-file")                   ; `C-x 4 j n'
(icicle-define-bookmark-command              "region" "Select region: ")      ; `C-x j r'
(icicle-define-bookmark-other-window-command "region" "Select region: ")      ; `C-x 4 j r'
(icicle-define-bookmark-command              "all-tags" nil                   ; `C-x j t *'
                                             (bmkp-read-tags-completing))
(icicle-define-bookmark-other-window-command "all-tags" nil                   ; `C-x 4 j t *'
                                             (bmkp-read-tags-completing))
(icicle-define-bookmark-command              "some-tags" nil                  ; `C-x j t +'
                                             (bmkp-read-tags-completing))
(icicle-define-bookmark-other-window-command "some-tags" nil                  ; `C-x 4 j t +'
                                             (bmkp-read-tags-completing))
(icicle-define-bookmark-command              "all-tags-regexp" nil            ; `C-x j t % *'
                                             (read-string "Regexp for tags: "))
(icicle-define-bookmark-other-window-command "all-tags-regexp" nil            ; `C-x 4 j t % *'
                                             (read-string "Regexp for tags: "))
(icicle-define-bookmark-command              "some-tags-regexp" nil           ; `C-x j t % +'
                                             (read-string "Regexp for tags: "))
(icicle-define-bookmark-other-window-command "some-tags-regexp" nil           ; `C-x 4 j t % +'
                                             (read-string "Regexp for tags: "))
(icicle-define-bookmark-command              "w3m")                           ; `C-x j w'
(icicle-define-bookmark-other-window-command "w3m")                           ; `C-x 4 j w'
(icicle-define-bookmark-command              "this-buffer")                   ; `C-x j .'
(icicle-define-bookmark-other-window-command "this-buffer")                   ; `C-x 4 j .'
(icicle-define-bookmark-command              "specific-buffers" nil           ; `C-x j = b'
                                             (icicle-bookmarked-buffer-list))
(icicle-define-bookmark-other-window-command "specific-buffers" nil           ; `C-x 4 j = b'
                                             (icicle-bookmarked-buffer-list))
(icicle-define-bookmark-command              "specific-files" nil             ; `C-x j = f'
                                             (icicle-bookmarked-file-list))
(icicle-define-bookmark-other-window-command "specific-files" nil             ; `C-x 4 j = f'
                                             (icicle-bookmarked-file-list))
(defalias 'icicle-select-bookmarked-region 'icicle-bookmark-region-other-window)

;;;###autoload
(defun icicle-bookmarked-buffer-list ()
  "`icicle-buffer-list', but only for bookmarked buffers."
  (interactive)
  (let ((icicle-buffer-predicate  (lambda (buf) (member buf (bmkp-buffer-names)))))
    (icicle-buffer-list)))
  
;;;###autoload
(defun icicle-bookmarked-file-list ()
  "`icicle-buffer-list', but only for bookmarked buffers."
  (interactive)
  (let ((use-file-dialog        nil)
        (icicle-file-predicate  (lambda (file) (member (expand-file-name file) (bmkp-file-names)))))
    (icicle-file-list)))

;;;###autoload
(icicle-define-command icicle-find-first-tag ; Command name
  "Find first tag in current tags table whose name matches your input.
This is similar to standard command `find-tag', with these
differences:

* This is a multi-command, so you can visit any number of tags.

* Only the first tag of several identical tags is a candidate, so you
  cannot visit the others.  That is, there is no equivalent to using
  `M-,' (`tags-loop-continue') after `find-tag' to find additional,
  identical tags.

* If `crosshairs.el' is loaded, the target position is highlighted.

To browse all tags (including duplicates) in all tags tables, use the
more powerful Icicles multi-command `icicle-find-tag'.

By default, Icicle mode remaps all key sequences that are normally
bound to `find-tag-other-window' to `icicle-find-first-tag'.  If you
do not want this remapping, then customize option
`icicle-top-level-key-bindings'."       ; Doc string
  icicle-find-first-tag-action          ; Action function
  "Find tag: "                          ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case  (progn (require 'etags)
                                   (if (and (boundp 'tags-case-fold-search) ; Bindings
                                            (memq tags-case-fold-search '(t nil)))
                                       tags-case-fold-search
                                     case-fold-search)))
   (case-fold-search        completion-ignore-case))
  nil                                   ; First, undo, last code.
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch)))

(defun icicle-find-first-tag-action (cand)
  "Action function for `icicle-find-first-tag'."
  (find-tag cand)
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

;;;###autoload
(icicle-define-command icicle-find-first-tag-other-window ; Command name
  "Find first tag in current tags table whose name matches your input.
Same as `icicle-find-first-tag' except it uses a different window." ; Doc string
  icicle-find-first-tag-other-window-action ; Action function
  "Find tag other window: "             ; `completing-read' args
  (if (fboundp 'tags-lazy-completion-table) (tags-lazy-completion-table) 'tags-complete-tag)
  nil nil nil nil (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
                               'find-tag-default))
  nil
  ((completion-ignore-case  (progn (require 'etags)
                                   (if (and (boundp 'tags-case-fold-search) ; Bindings
                                            (memq tags-case-fold-search '(t nil)))
                                       tags-case-fold-search
                                     case-fold-search)))
   (case-fold-search        completion-ignore-case))
  nil                                   ; First, undo, last code.
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))
  (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch)))

(defun icicle-find-first-tag-other-window-action (cand)
  "Action function for `icicle-find-first-tag-other-window'."
  (find-tag-other-window cand)
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

;;;###autoload
(defun icicle-find-tag (regexp &optional arg)
  "Navigate among all tags that match REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You can use completion to choose a tag in the current tags table as
REGEXP.  You can use `\\[icicle-pop-tag-mark]' to return to your starting point.

All matching tags are shown, including duplicate tags from the same or
different source files.  This means that you do not need `M-,' - you
see all tags as candidates to visit.

By default:

* Tags from all tags files are candidates.
* The source file name is shown after a tag, in buffer *Completions*.

A prefix argument changes this default behavior, as follows:

* ARG = 0 or ARG > 0: only the current tag table is used
* ARG = 0 or ARG < 0: source file names are not shown

By default, Icicle mode remaps all key sequences that are normally
bound to `find-tag' to `icicle-find-tag'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'.

If `crosshairs.el' is loaded, then the target position is highlighted."
  (interactive
   (let* ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                            (memq tags-case-fold-search '(t nil)))
                                       tags-case-fold-search
                                     case-fold-search))
          (case-fold-search        completion-ignore-case))
     (require 'etags)
     (list (completing-read "Find tag matching regexp: "
                            ;; $$$ Or should we just read a regexp against `regexp-history'?
                            (if (fboundp 'tags-lazy-completion-table)
                                (tags-lazy-completion-table) ; Emacs 23+
                              'tags-complete-tag) ; Emacs < 23
                            nil nil nil 'find-tag-history
                            (funcall (or find-tag-default-function
                                         (get major-mode 'find-tag-default-function)
                                         'find-tag-default)))
           current-prefix-arg)))

  (unwind-protect
       (let* ((icicle-whole-candidate-as-text-prop-p  t)
              (icicle-sort-comparer                   nil)
              (icicle-inhibit-sort-p                  t)
              (icicle-candidate-action-fn             'icicle-find-tag-action)
              (icicle-candidate-help-fn               'icicle-find-tag-help)
              (completion-ignore-case                 (if (and (boundp 'tags-case-fold-search)
                                                               (memq tags-case-fold-search
                                                                     '(t nil)))
                                                          tags-case-fold-search
                                                        case-fold-search))
              (case-fold-search                       completion-ignore-case)
              (orig-pt-find-tag                       (point-marker)))

         (ring-insert find-tag-marker-ring orig-pt-find-tag) ; Record starting point.
         (icicle-explore #'(lambda () (icicle-find-tag-define-candidates regexp arg))
                         #'icicle-find-tag-final-act #'icicle-find-tag-quit-or-error
                         #'icicle-find-tag-quit-or-error nil
                         "Choose a tag: " nil nil nil 'find-tag-history))
    (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))))

;;;###autoload
(defun icicle-pop-tag-mark ()
  "Like `pop-tag-mark', but uses `pop-to-buffer', not `switch-to-buffer'.
By default, Icicle mode remaps all key sequences that are normally
bound to `pop-tag-mark' to `icicle-pop-tag-mark'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive)
  (require 'etags)
  (when (ring-empty-p find-tag-marker-ring) (error "No previous locations for find-tag invocation"))
  (let ((marker  (ring-remove find-tag-marker-ring 0)))
    (pop-to-buffer (or (marker-buffer marker) (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))

(defun icicle-find-tag-define-candidates (regexp arg)
  "Define candidates for `icicle-find-tag'.
See `icicle-explore', argument DEFINE-CANDIDATES-FN."
  (save-excursion
    (let ((first-time  t)
          (morep       t))
      (setq icicle-candidates-alist  ())
      (while (and morep (visit-tags-table-buffer (not first-time)))
        (when (and arg (wholenump (prefix-numeric-value arg))) (setq morep  nil))
        (setq first-time               nil
              icicle-candidates-alist  (append icicle-candidates-alist
                                               (nreverse
                                                (icicle-find-tag-define-candidates-1
                                                 regexp (> (prefix-numeric-value arg)
                                                           0)))))))))

(defun icicle-find-tag-define-candidates-1 (regexp show-file-p)
  "Helper function for `icicle-find-tag-define-candidates'.
Returns completion alist of tag information for tags matching REGEXP.
Include file name (label) if SHOW-FILE-P is non-nil.

If SHOW-FILE-P is nil, then alist items look like this:

 (TAG TAG-INFO FILE-PATH GOTO-FUNC)

If SHOW-FILE-P is non-nil, then alist items look like this:

 ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC) or

 (FILE-LABEL TAG-INFO FILE-PATH GOTO-FUNC) if no matching TAG.

TAG-INFO is what `snarf-tag-function' (e.g. `etags-snarf-tag')
returns.  It is a cons (TEXT LINE . POSITION).

TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present."
  (icicle-highlight-lighter)
  (message "Gathering tags...")
  (goto-char (point-min))
  (let ((temp-list  ()))
    (while (re-search-forward (concat regexp ".*\177*") nil t) ; Look before the DEL character.
      (beginning-of-line)
      (let* ((goto-func  goto-tag-location-function) ; e.g. `etags-goto-tag-location'.
             ;; TAG-INFO: If no specific tag, (t nil (point-min)). Else, (TEXT LINE . STARTPOS).
             ;; e.g. TEXT = "(defun foo ()" or just "foo" (if explicit),
             ;;      LINE = "148", STARTPOS = "1723"
             (tag-info (save-excursion (funcall snarf-tag-function))) ; e.g. `etags-snarf-tag'.
             (tag (if (eq t (car tag-info)) nil (car tag-info)))
             ;; FILE-PATH is absolute. FILE-LABEL is relative to `default-directory'.
             (file-path (save-excursion
                          (if tag (file-of-tag) (save-excursion (next-line 1) (file-of-tag)))))
             (file-label (expand-file-name file-path (file-truename default-directory))))
        (when (and tag (not (string= "" tag)) (= (aref tag 0) ?\( ))
          (setq tag  (concat tag " ...)")))
        (when (file-readable-p file-path)
          ;; Add item to alist.
          ;;   Item looks like this:         ((TAG FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          ;;   or like this, if no matching tag: ((FILE-LABEL) TAG-INFO FILE-PATH GOTO-FUNC)
          (cond (tag
                 (push `(,(if show-file-p
                              (list tag ; Make multi-completion cons: add file name to candidate.
                                    (progn (put-text-property 0 (length file-label) 'face
                                                              'icicle-candidate-part file-label)
                                           file-label))
                              tag)
                         ,tag-info ,file-path ,goto-func)
                       temp-list))
                (show-file-p            ; No tag.  Use only the FILE-LABEL.
                 (push `((,(progn (put-text-property 0 (length file-label) 'face
                                                     'icicle-candidate-part file-label)
                                  file-label))
                         ,tag-info ,file-path ,goto-func)
                       temp-list)))))
      (forward-line))
    temp-list))                         ; Return the alist for this TAGS file.

(defun icicle-find-tag-action (ignored-string)
  "Action function for `icicle-find-tag'."
  ;; Ignore (TAG FILE-LABEL) part.  Use only (TAG-INFO FILE-PATH GOTO-FUNC) part.
  (let* ((cand       (cdr (elt (icicle-filter-alist icicle-candidates-alist
                                                    icicle-completion-candidates)
                               icicle-candidate-nb)))
         (tag-info   (nth 0 cand))
         (goto-func  (nth 2 cand)))
    (switch-to-buffer-other-window      ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (condition-case err
        (funcall goto-func tag-info)    ; Go to text at TAG-INFO.
      (error (message (error-message-string err)) (sit-for 2) nil)))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-find-tag-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-find-first-tag'."
  (let* ((cand      (cdr (elt (icicle-filter-alist icicle-candidates-alist
                                                   icicle-completion-candidates)
                              icicle-candidate-nb)))
         (tag-info  (nth 0 cand)))
    (message (if (eq t (car tag-info))
                 "No tag - file name itself matches"
               (format "Line: %d, Position: %d, File: %s"
                       (cadr tag-info) (cddr tag-info) (nth 1 cand))))
    (sit-for 4)))

(defun icicle-find-tag-final-act ()
  "Go to the final tag choice."
  (let ((cand  (cdr icicle-explore-final-choice-full)))
    (unless cand (error "No such occurrence: %s" cand))
    (switch-to-buffer-other-window ; Go to source file at FILE-PATH.
     (if (fboundp 'tag-find-file-of-tag-noselect)
         (tag-find-file-of-tag-noselect (nth 1 cand))
       (find-file-noselect (nth 1 cand))))
    (widen)
    (funcall (nth 2 cand) (nth 0 cand)))) ; Go to text at TAG-INFO.

(defun icicle-find-tag-quit-or-error ()
  "Pop back to the last tag visited."
  (icicle-pop-tag-mark)
  (raise-frame))

;;;###autoload
(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "Select a window or frame, by name or by order.
This command combines Emacs commands `other-window' and `other-frame',
together with Icicles multi-commands `icicle-select-window', and
`icicle-select-frame'.  Use the prefix argument to choose, as follows:

 With no prefix arg or a non-zero numeric prefix arg:
  If the selected frame has multiple windows, then this is
  `other-window'.  Otherwise, it is `other-frame'.

 With a zero prefix arg (e.g. `C-0'):
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows in the frame as candidates.
  Otherwise (single-window frame), this is `icicle-select-frame'.

 With plain `C-u':
  If the selected frame has multiple windows, then this is
  `icicle-select-window' with windows from all visible frames as
  candidates.  Otherwise, this is `icicle-select-frame'.

By default, Icicle mode remaps all key sequences that are normally
bound to `other-window' to `icicle-other-window-or-frame'.  If you do
not want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((consp arg)
           (if (one-window-p) (icicle-select-frame) (icicle-select-window)))
          ((zerop numarg)
           (if (one-window-p)
               (icicle-select-frame)
             (let ((current-prefix-arg  nil)) (icicle-select-window))))
          (t
           (if (one-window-p) (other-frame numarg) (other-window numarg))))))

;;;###autoload
(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
  "Select frame by its name and raise it.
A frame name in this context is suffixed as needed by [NUMBER], to
make it unique.  For example, in a context where frames are named for
their buffers and you have two frames showing buffer *Help*, one of
the frames will be called `*Help*[2]' for use with this command." ; Doc string
  icicle-select-frame-by-name           ; Action function
  "Select frame: "                      ; `completing-read' args
  icicle-frame-alist nil t nil
  (if (boundp 'frame-name-history) 'frame-name-history 'icicle-frame-name-history)
  (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))) nil
  ((icicle-frame-alist  (icicle-make-frame-alist)) ; Bindings
   (alt-fn              nil)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (setq alt-fn  (icicle-alt-act-fn-for-type "frame"))))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn alt-fn (icicle-alt-act-fn-for-type "frame")))))

;;;###autoload
(defun icicle-select-frame-by-name (name &optional frame-alist)
  "Select the frame named NAME, and raise it.
Optional argument FRAME-ALIST is an alist of frames to choose from.
Each element has the form (FNAME . FRAME), where FNAME names FRAME.
See `icicle-make-frame-alist' for more about FNAME."
  (interactive (let* ((alist    (icicle-make-frame-alist))
                      (default  (car (rassoc (selected-frame) alist)))
                      (input    (completing-read "Select Frame: " alist nil t nil
                                                 'frame-name-history default)))
                 (list (if (= (length input) 0) default input)
                       alist)))
  (unless frame-alist (setq frame-alist  (or (and (boundp 'icicle-frame-alist) icicle-frame-alist)
                                             (icicle-make-frame-alist))))
  (let ((frame  (cdr (assoc name frame-alist))))
    (unless frame (error "No such frame: `%s'" name))
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)))

(defun icicle-make-frame-alist ()
  "Return an alist of entries (FNAME . FRAME), where FNAME names FRAME.
Frame parameter `name' is used as FNAME, unless there is more than one
frame with the same name.  In that case, FNAME includes a suffix
\[NUMBER], to make it a unique name.  The NUMBER order among frame
names that differ only by their [NUMBER] is arbitrary."
  (let ((fr-alist  ())
        (count     2)
        fname new-name)
    (dolist (fr  (frame-list))
      (setq fname  (frame-parameter fr 'name))
      (if (not (assoc fname fr-alist))
          (push (cons fname fr) fr-alist)
        (setq new-name  fname)
        (while (assoc new-name fr-alist)
          (setq new-name  (format "%s[%d]" fname count)
                count     (1+ count)))
        (push (cons new-name fr) fr-alist))
      (setq count  2))
    fr-alist))

;; Free vars here: `icicle-window-alist' is bound in Bindings form.
;;;###autoload
(icicle-define-command icicle-select-window ; Command name
  "Select window by its name.
With no prefix arg, candidate windows are those of the selected frame.
With a prefix arg, windows of all visible frames are candidates.

A window name is the name of its displayed buffer, but suffixed as
needed by [NUMBER], to make the name unique.  For example, if you have
two windows showing buffer *Help*, one of the windows will be called
`*Help*[2]' for use with this command." ; Doc string
  icicle-select-window-by-name          ; Action function
  "Select window: "                     ; `completing-read' args
  icicle-window-alist nil t nil nil
  (buffer-name (window-buffer (other-window 1))) nil
  ((icicle-window-alist  (icicle-make-window-alist current-prefix-arg)))) ; Bindings

;; Free vars here: `icicle-window-alist' is bound in `icicle-select-window'.
(defun icicle-select-window-by-name (name &optional window-alist)
  "Select the window named NAME.
Optional argument WINDOW-ALIST is an alist of windows to choose from.

Interactively:
 A prefix arg means windows from all visible frames are candidates.
 No prefix arg means windows from the selected frame are candidates.

Each alist element has the form (WNAME . WINDOW), where WNAME names
WINDOW.  See `icicle-make-window-alist' for more about WNAME.

If `crosshairs.el' is loaded, then the target position is highlighted."
  (interactive (let* ((alist    (icicle-make-window-alist current-prefix-arg))
                      (default  (car (rassoc (selected-window) alist)))
                      (input    (completing-read "Select Window: " alist nil t nil nil default)))
                 (list (if (= (length input) 0) default input) alist)))
  (unless window-alist
    (setq window-alist  (or (and (boundp 'icicle-window-alist) icicle-window-alist)
                            (icicle-make-window-alist))))
  (let ((window  (cdr (assoc name window-alist))))
    (unless window (error "No such window: `%s'" name))
    (select-window window)
    (when (fboundp 'crosshairs-highlight) (crosshairs-highlight))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-make-window-alist (&optional all-p)
  "Return an alist of entries (WNAME . WINDOW), where WNAME names WINDOW.
The name of the buffer in a window is used as its name, unless there
is more than one window displaying the same buffer.  In that case,
WNAME includes a suffix [NUMBER], to make it a unique name.  The
NUMBER order among window names that differ only by their [NUMBER] is
arbitrary.

Non-nil argument ALL-P means use windows from all visible frames.
Otherwise, use only windows from the selected frame."
  (let ((win-alist  ())
        (count      2)
        wname new-name)
    (walk-windows #'(lambda (w)
                      (setq wname  (buffer-name (window-buffer w)))
                      (if (not (assoc wname win-alist))
                          (push (cons wname w) win-alist)
                        (setq new-name  wname)
                        (while (assoc new-name win-alist)
                          (setq new-name  (format "%s[%d]" wname count)
                                count     (1+ count)))
                        (push (cons new-name w) win-alist))
                      (setq count  2))
                  'no-mini
                  (if all-p 'visible 'this-frame))
    win-alist))

;;;###autoload
(icicle-define-command icicle-delete-windows ; Command name
  "Delete windows showing a buffer, anywhere." ; Doc string
  delete-windows-on                     ; Action function
  "Delete windows on buffer: "          ; `completing-read' args
  (let ((cand-bufs  nil))
    (dolist (buf  (buffer-list))
      (when (get-buffer-window buf 0) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (current-buffer)) nil
  ((icicle-use-candidates-only-once-flag  t) ; Bindings
   (icicle-inhibit-try-switch-buffer      t)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))))

;;;###autoload
(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
When called from the minibuffer, remove the *Completions* window.

Otherwise:
 With no prefix argument, delete the selected window.
 With a prefix argument, prompt for a buffer and delete all windows,
   on any frame, that show that buffer.

 With a prefix argument, this is an Icicles multi-command - see
 command `icicle-mode'.  Input-candidate completion and cycling are
 available.  While cycling, these keys with prefix `C-' are active:

 `C-RET'   - Act on current completion candidate only
 `C-down'  - Move to next prefix-completion candidate and act
 `C-up'    - Move to previous prefix-completion candidate and act
 `C-next'  - Move to next apropos-completion candidate and act
 `C-prior' - Move to previous apropos-completion candidate and act
 `C-!'     - Act on *all* candidates (or all that are saved),
             successively (careful!)

 With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
 `C-M-RET', `C-M-down', and so on) provide help about candidates.

 Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
 or `C-g' to quit.

By default, Icicle mode remaps all key sequences that are normally
bound to `delete-window' to `icicle-delete-window'.  If you do not
want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (if (window-minibuffer-p (selected-window))
      (icicle-remove-Completions-window)
    (if bufferp (icicle-delete-windows) (delete-window))))

;;;###autoload
(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
  "Kill a buffer.
With a positive prefix arg, only buffers visiting files are candidates.
With a negative prefix arg, only buffers associated with the selected
frame are candidates.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

By default, Icicle mode remaps all key sequences that are normally
bound to `kill-buffer' to `icicle-kill-buffer'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'.

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  icicle-kill-a-buffer-and-update-completions ; Action function
  "Kill buffer: "                       ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf))) bufflist) nil ; `bufflist' is free here.
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ; Emacs23.
  nil 'buffer-name-history (buffer-name (current-buffer)) nil
  (icicle-buffer-bindings))             ; Bindings

(defun icicle-kill-a-buffer-and-update-completions (buf)
  "Kill buffer BUF and update the set of completions."
  (setq buf  (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update *Completions*.
            (setq minibuffer-completion-table  (mapcar #'(lambda (buf) (list (buffer-name buf)))
                                                       (buffer-list)))
            (icicle-complete-again-update))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(put 'icicle-buffer 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-buffer    ; Bound to `C-x b' in Icicle mode.
  "Switch to a different buffer.
With a positive prefix arg, only buffers visiting files are candidates.
With a negative prefix arg, only buffers associated with the selected
frame are candidates.

You can use `C-x m' during completion to access buffer (non-file)
 bookmarks, if you use library `bookmark+.el'.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-buffers-ido-like' non-nil gives this command a more
Ido-like behavior.

See also command `icicle-buffer-config'.

By default, Icicle mode remaps all key sequences that are normally
bound to `switch-to-buffer' to `icicle-buffer'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'.

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  switch-to-buffer                      ; Action function
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf))) bufflist) nil ; `bufflist' is free here.
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ; Emacs23.
  nil 'buffer-name-history (icicle-default-buffer-names) nil
  (icicle-buffer-bindings)              ; Bindings
  (progn                                ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-non-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-non-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-non-file-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))

;; Free var here: `bufflist' is bound by `icicle-buffer-bindings'.
(defun icicle-default-buffer-names ()
  "Default buffer names (Emacs 23+) or name (< Emacs 23)."
  (let ((bname  (buffer-name (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                 (another-buffer nil t)
                               (other-buffer (current-buffer))))))
    (if (> emacs-major-version 22)      ; Emacs 23 accepts a list of default values.
        (cons bname
              (mapcar #'buffer-name
                      (delete (current-buffer) ; Just keep the first 4.  (This could be an option.)
                              (icicle-first-N 4 (if (boundp 'bufflist) bufflist (buffer-list))))))
      bname)))

;;;###autoload
(icicle-define-command icicle-insert-buffer
  "Multi-command version of `insert-buffer'.
With a positive prefix arg, only buffers visiting files are candidates.
With a negative prefix arg, only buffers associated with the selected
frame are candidates.

You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'.

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  insert-buffer                         ; Action function
  "Buffer: "                            ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf))) bufflist) nil ; `bufflist' is free here.
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ; Emacs23.
  nil 'buffer-name-history (icicle-default-buffer-names) nil
  (icicle-buffer-bindings))             ; Bindings

;;;###autoload
(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
  "Switch to a different buffer in another window.
Same as `icicle-buffer' except it uses a different window." ; Doc string
  switch-to-buffer-other-window         ; Action function
  "Switch to buffer in other window: "  ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf))) bufflist) nil ; `bufflist' is free here.
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ; Emacs23.
  nil 'buffer-name-history (icicle-default-buffer-names) nil
  (icicle-buffer-bindings)              ; Bindings
  (progn                                ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-non-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-non-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-non-file-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))

;;;###autoload
(icicle-define-command icicle-add-buffer-candidate ; Command name
  "Add buffer as an always-show completion candidate.
Add the buffer to `icicle-buffer-extras'.  Save the updated option.
With a positive prefix arg, only buffers visiting files are candidates.
With a negative prefix arg, only buffers associated with the selected
frame are candidates.

You can use `S-delete' on any completion candidate to remove it from
`icicle-buffer-extras'.

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  (lambda (buf)
    (add-to-list 'icicle-buffer-extras buf) ; Action function
    (funcall icicle-customize-save-variable-function 'icicle-buffer-extras icicle-buffer-extras)
    (message "Buffer `%s' added to always-show buffers" buf))
  "Buffer candidate to show always: "   ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf))) bufflist) nil ; `bufflist' is free here.
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ; Emacs23.
  nil 'buffer-name-history (icicle-default-buffer-names) nil
  (icicle-buffer-bindings ((icicle-use-candidates-only-once-flag  t)))) ; Bindings

;;;###autoload
(icicle-define-command icicle-remove-buffer-candidate ; Command name
  "Remove buffer as an always-show completion candidate.
Remove the buffer from `icicle-buffer-extras'.
Save the updated option."               ; Doc string
  icicle-remove-buffer-candidate-action ; Action function
  "Remove buffer from always-show list: " ; `completing-read' args
  (mapcar #'list icicle-buffer-extras) nil t nil 'buffer-name-history (car icicle-buffer-extras) nil
  ((icicle-use-candidates-only-once-flag  t) ; Bindings
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer"))))
  (unless icicle-buffer-extras (error "`icicle-extra-buffers' is empty"))) ; First code

(defun icicle-remove-buffer-candidate-action (buf)
  "Action function for command `icicle-remove-buffer-candidate'."
  (setq icicle-buffer-extras  (delete buf icicle-buffer-extras))
  (funcall icicle-customize-save-variable-function 'icicle-buffer-extras icicle-buffer-extras)
  (message "Buffer `%s' removed from always-show buffers" buf))

;;;###autoload
(icicle-define-command icicle-buffer-config ; Command name
  "Choose a configuration of user options for `icicle-buffer'.
You can use `S-delete' on any configuration during completion to
remove it.  See user option `icicle-buffer-configs'.
See also commands `icicle-add-buffer-config' and
`icicle-remove-buffer-config'."         ; Doc string
  (lambda (config-name)                 ; Action function
    (let ((config  (assoc config-name icicle-buffer-configs)))
      (setq icicle-buffer-match-regexp     (elt config 1)
            icicle-buffer-no-match-regexp  (elt config 2)
            icicle-buffer-predicate        (elt config 3)
            icicle-buffer-extras           (elt config 4)
            icicle-buffer-sort             (elt config 5))))
  "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
  'icicle-buffer-config-history nil nil
  ((icicle-delete-candidate-object  'icicle-remove-buffer-config-action))) ; Bindings

;;;###autoload
(icicle-define-add-to-alist-command icicle-add-buffer-config ; Command name
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  #'(lambda ()
      (let ((name            (read-from-minibuffer "Add buffer configuration.  Name: "))
            (match-regexp    (icicle-read-from-minibuf-nil-default
                              "Regexp to match: " nil nil nil 'regexp-history
                              icicle-buffer-match-regexp))
            (nomatch-regexp  (icicle-read-from-minibuf-nil-default
                              "Regexp not to match: " nil nil nil 'regexp-history
                              icicle-buffer-no-match-regexp))
            (pred            (icicle-read-from-minibuf-nil-default
                              "Predicate to satify: " nil nil nil
                              (if (boundp 'function-name-history)
                                  'function-name-history
                                'icicle-function-name-history)
                              icicle-buffer-predicate))
            (sort-fn         (icicle-read-from-minibuf-nil-default
                              "Sort function: " nil nil t
                              (if (boundp 'function-name-history)
                                  'function-name-history
                                'icicle-function-name-history)
                              (and icicle-buffer-sort (symbol-name icicle-buffer-sort))))
            (extras          (progn (message "Choose extra buffers to show...") (sit-for 1)
                                    (icicle-buffer-list)))) ; Do last, for convenience.
        (list name match-regexp nomatch-regexp pred extras sort-fn)))
  icicle-buffer-configs)

;;;###autoload
(icicle-define-command icicle-buffer-list ; Command name
  "Choose a list of buffer names.
With a positive prefix arg, only buffers visiting files are candidates.
With a negative prefix arg, only buffers associated with the selected
frame are candidates.

You can use `S-delete' during completion to kill a candidate buffer.
The list of names (strings) is returned.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior."                          ; Doc string
  (lambda (name) (push name buf-names)) ; Action function
  "Choose buffer (`RET' when done): "   ; `completing-read' args
  (mapcar #'(lambda (buf) (list (buffer-name buf)))
          (if current-prefix-arg
              (if (wholenump (prefix-numeric-value current-prefix-arg))
                  (icicle-remove-if-not #'(lambda (bf) (buffer-file-name bf)) (buffer-list))
                (cdr (assq 'buffer-list (frame-parameters))))
            (buffer-list)))
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'buffer-name-history nil nil
  ((buf-names                             ()) ; Bindings
   (completion-ignore-case                (or (and (boundp 'read-buffer-completion-ignore-case)
                                                   read-buffer-completion-ignore-case)
                                              completion-ignore-case))
   (icicle-must-match-regexp              icicle-buffer-match-regexp)
   (icicle-must-not-match-regexp          icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate            icicle-buffer-predicate)
   (icicle-extra-candidates               icicle-buffer-extras)
   (icicle-transform-function             'icicle-remove-dups-if-extras)
   (icicle-sort-comparer                  (or icicle-buffer-sort icicle-sort-comparer))
   (icicle-sort-orders-alist
    (append (list '("by last access")   ; Renamed from "turned OFF'.
                  '("*...* last" . icicle-buffer-sort-*...*-last)
                  '("by buffer size" . icicle-buffer-smaller-p)
                  '("by major mode name" . icicle-major-mode-name-less-p)
                  (and (fboundp 'icicle-mode-line-name-less-p)
                       '("by mode-line mode name" . icicle-mode-line-name-less-p))
                  '("by file/process name" . icicle-buffer-file/process-name-less-p))
            (delete '("turned OFF") icicle-sort-orders-alist)))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
   (icicle-delete-candidate-object        'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag             icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag       icicle-buffer-ignore-space-prefix-flag)
   (icicle-use-candidates-only-once-flag  t))
  nil nil                               ; First code, undo code
  (prog1 (setq buf-names  (nreverse (delete "" buf-names))) ; Last code - return the list of buffers
    (when (interactive-p) (message "Buffers: %S" buf-names))))

;;;###autoload
(icicle-define-command icicle-remove-buffer-config ; Command name
  "Remove buffer configuration from `icicle-buffer-configs'.
Save the updated option."               ; Doc string
  icicle-remove-buffer-config-action    ; Action function
  "Remove buffer configuration: "       ; `completing-read' args
  (mapcar #'(lambda (config) (list (car config))) icicle-buffer-configs)
  nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs) nil
  ((icicle-use-candidates-only-once-flag  t))) ; Bindings

(defun icicle-remove-buffer-config-action (config-name)
  "Action function for command `icicle-remove-buffer-config'."
  (setq icicle-buffer-configs  (icicle-assoc-delete-all config-name icicle-buffer-configs))
  (funcall icicle-customize-save-variable-function 'icicle-buffer-configs icicle-buffer-configs)
  (message "Buffer configuration `%s' removed" config-name))

;;;###autoload
(icicle-define-command icicle-face-list ; Command name
  "Choose a list of face names.  The list of names (strings) is returned." ; Doc string
  (lambda (name) (push (icicle-transform-multi-completion name) face-names)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar #'icicle-make-face-candidate (face-list))
  nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
  (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)
  nil nil
  ((prompt                                "Choose face (`RET' when done): ") ; Bindings
   (icicle-list-nth-parts-join-string     ": ")
   (icicle-list-join-string               ": ")
   (icicle-list-end-string                "")
   (icicle-list-use-nth-parts             '(1))
   (icicle-use-candidates-only-once-flag  t)
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "face")))
   (icicle-all-candidates-list-alt-action-fn ; M-|'
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "face")))
   (face-names                            ()))
  (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
  nil                                   ; Undo code
  (prog1 (setq face-names  (nreverse (delete "" face-names))) ; Last code - return list of faces
    (when (interactive-p) (message "Faces: %S" face-names))))

;;;###autoload
(icicle-define-command icicle-color-theme ; Command name
  "Change color theme.
You can use `S-delete' during completion to remove the current
candidate from the list of color themes.

If you use `C-g' during this command, the previous color-theme
snapshot is used to restore that color theme.

Remember too that you can use the pseudo-theme [Reset] to restore the
last theme: `M-x color-theme-select [Reset]'.

By default, each time you invoke this command, a snapshot is first
made of the current color theme (or current colors, if no theme is
used).  Thus, by default, if you use `C-g', the colors restored are
those used before you changed themes using this command.

However, if you use a prefix arg, then this command takes no new
snapshot, unless no snapshot has ever been taken during this Emacs
session.  This can be useful when experimenting, to restore not to the
state just before this command invocation, but to some previous
snapshot.

To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
  (lambda (theme)
    (when (string= "" theme) (error "No theme name entered (empty input)"))
    (funcall  (intern theme)))          ; Action function: just call the theme.
  "Theme: " icicle-color-themes nil t nil ; `completing-read' args
  (if (boundp 'color-theme-history) 'color-theme-history 'icicle-color-theme-history)
  nil nil
  ((icicle-delete-candidate-object  'icicle-color-themes) ; Bindings
   (prefix-arg                      current-prefix-arg))
  (progn (unless (prog1 (require 'color-theme nil t) ; First code
                   (when (and (fboundp 'color-theme-initialize) (not color-theme-initialized))
                     ;; NOTE: We need the `condition-case' because of a BUG in `directory-files'
                     ;; for Emacs 20.  Bug reported to `color-theme.el' maintainer 2009-11-22.  The
                     ;; problem is that the default value of `color-theme-libraries' concats
                     ;; `file-name-directory', which ends in `/', with `/themes', not with `themes'.
                     ;; So the result is `...//themes'.  That is tolerated by Emacs 21+
                     ;; `directory-files', but not for Emacs 20.  Until this `color-theme.el' bug is
                     ;; fixed, Emacs 20 users will need to manually load `color-theme-libraries.el'.
                     (condition-case nil
                         (let ((color-theme-load-all-themes  t))
                           (color-theme-initialize)
                           (setq color-theme-initialized  t))
                       (error nil))))
           (error "This command requires library `color-theme.el'"))
         (unless icicle-color-themes
           (setq icicle-color-themes
                 (delete '("bury-buffer")
                         (mapcar (lambda (entry) (list (symbol-name (car entry))))
                                 color-themes))))
         ;; Create the snapshot, if not available.  Do this so users can also undo using
         ;; pseudo-theme `[Reset]'.
         (when (or (not prefix-arg)
                   (not (assq 'color-theme-snapshot color-themes))
                   (not (commandp 'color-theme-snapshot)))
           (fset 'color-theme-snapshot (color-theme-make-snapshot))
           (setq color-themes  (delq (assq 'color-theme-snapshot color-themes) color-themes)
                 color-themes  (delq (assq 'bury-buffer color-themes) color-themes)
                 color-themes  (append '((color-theme-snapshot
                                          "[Reset]" "Undo changes, if possible.")
                                         (bury-buffer "[Quit]" "Bury this buffer."))
                                       color-themes))))
  (color-theme-snapshot))               ; Undo code

;; Bound to `C-- C-y' via `icicle-yank-maybe-completing'.
;;;###autoload
(icicle-define-command icicle-completing-yank
  "Yank an entry from the `kill-ring', choosing it using completion.
This is like `yank', but it does not rotate the `kill-ring'.
The mark is pushed first, so the yanked text becomes the region.
You can sort the candidates to yank - use `C-,'.
You can use `S-delete' during completion to remove a candidate entry
from the `kill-ring'."                  ; Doc string
  icicle-insert-for-yank                ; Action function
  "Insert: " (mapcar #'list kill-ring) nil t nil 'icicle-kill-history ; `completing-read' args
  (car kill-ring) nil
  ((icicle-transform-function       'icicle-remove-duplicates) ; Bindings
   (icicle-sort-comparer            nil)
   (icicle-delete-candidate-object  'kill-ring)))

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed.
Pushes the mark first, so the inserted text becomes the region."
  (push-mark)
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint  (point)))
      (insert string)
      (let ((inhibit-read-only  t)) (remove-text-properties opoint (point) '(read-only nil))))))

;;;###autoload
(defun icicle-yank-maybe-completing (&optional arg) ;  Bound to `C-y' (or what `yank' was bound to).
  "`icicle-completing-yank', `icicle-yank', or `icicle-yank-function'.
If called from the minibuffer, call `icicle-yank'.
Otherwise:
 With a negative prefix argument, call `icicle-completing-yank'.
 Otherwise, call the value of user option `icicle-yank-function' (by
 default, `yank')."
  (interactive "*P")
  (if (window-minibuffer-p (selected-window))
      (icicle-yank arg)
    (if (wholenump (prefix-numeric-value arg))
        (funcall icicle-yank-function arg)
      (icicle-completing-yank))))

;;;###autoload
(icicle-define-file-command icicle-delete-file ; Command name
  "Delete a file or directory."         ; Doc string
  icicle-delete-file-or-directory       ; Function to perform the action
  "Delete file or directory: " default-directory nil t nil nil ; `read-file-name' args
  (icicle-file-bindings))               ; Bindings

(defun icicle-delete-file-or-directory (file)
  "Delete file or (empty) directory FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

;; $$$$$ (icicle-define-command icicle-file-list ; Command name
;;   "Choose a list of file names.
;; You can use `S-delete' during completion to delete a candidate file.
;; The list of names (strings) is returned." ; Doc string
;;   (lambda (name) (push name file-names)) ; Function to perform the action
;;   "Choose file (`RET' when done): "     ; `completing-read' args
;;   (mapcar #'list (directory-files default-directory nil icicle-re-no-dot))
;;   nil nil nil 'file-name-history nil nil
;;   ((file-names nil)                     ; Additional bindings
;;    (icicle-delete-candidate-object  'icicle-delete-file-or-directory) ; `S-delete' deletes file.
;;    (icicle-use-candidates-only-once-flag  t))
;;   nil nil                               ; First code, undo code
;;   (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return files list
;;     (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload
(icicle-define-file-command icicle-file-list ; Command name
  "Choose a list of file and directory names.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.
You can navigate the directory tree, picking files and directories
anywhere in the tree.

You can use `S-delete' during completion to delete a candidate file.
  Careful: This deletes the file, it does not just remove it as a
  candidate.
The list of file names (strings) is returned.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (name) (push name file-names)) ; Function to perform the action
  "Choose file (`RET' when done): "     ; `read-file-name' args
  nil nil t nil nil
  (icicle-file-bindings                 ; Bindings
   ((file-names                         nil)
    (icicle-comp-base-is-default-dir-p  t)
    ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
    ))
  nil nil                               ; First code, undo code
  (prog1 (setq file-names  (nreverse (delete "" file-names))) ; Last code - return list of files
    (when (interactive-p) (message "Files: %S" file-names))))

;;;###autoload
(when (> emacs-major-version 21)
  (icicle-define-file-command icicle-directory-list ; Command name
    "Choose a list of directory names.
Use multi-command action keys (e.g. `C-RET', `C-mouse-2') to choose,
and a final-choice key (e.g. `RET', `mouse-2') to choose the last one.
You can navigate the directory tree, picking directories anywhere in
the tree.

You can use `S-delete' during completion to delete a candidate (empty)
directory.
  Careful: This deletes the directory, it does not just remove it as a
  candidate.
The list of directory names (strings) is returned.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra directory names to display
 `icicle-file-match-regexp'     - Regexp directory names must match
 `icicle-file-no-match-regexp'  - Regexp dir names must not match
 `icicle-file-predicate'        - Predicate dir names must satisfy
 `icicle-file-sort'             - Sort function for candidates

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."    ; Doc string
    (lambda (name) (push name dir-names)) ; Function to perform the action
    "Choose directory (`RET' when done): " ; `read-file-name' args
    ;; $$$$$$ nil nil t nil #'(lambda (file) (eq t (car (file-attributes file)))) ; PREDICATE
    nil nil t nil #'file-directory-p    ; PREDICATE
    (icicle-file-bindings               ; Bindings
     ((dir-names                          nil)
      (icicle-comp-base-is-default-dir-p  t)
      ;; $$$$$ (icicle-dir-candidate-can-exit-p (not current-prefix-arg))
      ))
    nil nil                             ; First code, undo code
    (prog1 (setq dir-names  (nreverse (delete "" dir-names))) ; Last code - return the list of dirs
      (when (interactive-p) (message "Directories: %S" dir-names)))))

;;;###autoload
(icicle-define-file-command icicle-dired
  "Multi-command version of `dired'.
During completion:
 You can use `C-x m' to access Dired bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory."                           ; Doc string
  (lambda (dir) (dired dir switches))   ; Function to perform the action
  "Dired (directory): " nil default-directory nil nil nil ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((switches               (and current-prefix-arg
                                 (read-string "Dired listing switches: " dired-listing-switches)))
    (icicle-file-sort       (or icicle-file-sort 'icicle-dirs-first-p))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (unless (boundp 'minibuffer-local-filename-completion-map) ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-dired-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-dired-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-dired-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))

;;;###autoload
(icicle-define-file-command icicle-dired-other-window
  "Multi-command version of `dired-other-window'.
During completion:
 You can use `C-x m' to access Dired bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory."                           ; Doc string
  (lambda (dir) (dired-other-window dir switches)) ; Function to perform the action
  "Dired in other window (directory): " nil default-directory nil nil nil ; `read-file-name' args
  (icicle-file-bindings                 ; Bindings
   ((switches               (and current-prefix-arg
                                 (read-string "Dired listing switches: " dired-listing-switches)))
    (icicle-file-sort       (or icicle-file-sort 'icicle-dirs-first-p))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (unless (boundp 'minibuffer-local-filename-completion-map) ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-dired-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-dired-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-dired-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))


(put 'icicle-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-file (arg)                ; Bound to `C-x C-f' in Icicle mode.
  "Visit a file or directory.
With no prefix argument, use relative file names
 (`icicle-find-file').
With a prefix argument, use absolute file names
 (`icicle-find-file-absolute').
With a negative prefix argument, you can choose also by date:
 Completion candidates include the last modification date.

Note that when you use a prefix argument completion matches candidates
as ordinary strings.  It knows nothing of file names per se.  In
particular, you cannot use remote file-name syntax if you use a prefix
argument.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

By default, Icicle mode remaps all key sequences that are normally bound
to `find-file' to `icicle-file'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute))
        (icicle-find-file-absolute))
    (icicle-find-file)))


(put 'icicle-file-other-window 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-file-other-window (arg)   ; Bound to `C-x 4 f' in Icicle mode.
  "Visit a file or directory in another window.
With no prefix argument, use relative file names
 (`icicle-find-file-other-window').
With a prefix argument, use absolute file names
 (`icicle-find-file-absolute-other-window').
With a negative prefix argument, you can choose also by date:
 Completion candidates include the last modification date.

Note that when you use a prefix argument completion matches candidates
as ordinary strings.  It knows nothing of file names per se.  In
particular, you cannot use remote file-name syntax if you use a prefix
argument.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

By default, Icicle mode remaps all key sequences that are normally
bound to `find-file-other-window' to `icicle-file-other-window'.  If
you do not want this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (if arg
      (if (wholenump (prefix-numeric-value arg))
          (let ((current-prefix-arg  nil)) (icicle-find-file-absolute-other-window))
        (icicle-find-file-absolute-other-window))
    (icicle-find-file-other-window)))


(put 'icicle-find-file-absolute 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-absolute ; Command name
  "Visit a file or directory, given its absolute name.
Unlike `icicle-find-file', the completion candidates are absolute, not
relative, file names.  By default, the completion candidates are files
in the current directory, but you can substitute other candidates by
retrieving a saved candidate set.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-hiding-common-match]' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c C-d' (think `cd') to change the `default-directory'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  default-directory 'file-name-history default-directory nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File or directory: ")
    (icicle-abs-file-candidates
     (mapcar #'(lambda (file) (if (file-directory-p file) (concat file "/") file))
             (directory-files default-directory 'full nil 'nosort)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (define-key minibuffer-local-completion-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-must-match-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-completion-map "\C-c\C-d"            'icicle-cd-for-abs-files)
    (define-key minibuffer-local-must-match-map "\C-c\C-d"            'icicle-cd-for-abs-files)
    (define-key minibuffer-local-completion-map "\C-c+"               'icicle-make-directory)
    (define-key minibuffer-local-must-match-map "\C-c+"               'icicle-make-directory)
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Undo code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Last code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil)))


(put 'icicle-find-file-absolute-other-window 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-absolute-other-window ; Command name
  "Visit a file or directory in another window, given its absolute name.
Same as `icicle-find-file-absolute' except uses a different window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  default-directory 'file-name-history default-directory nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File or directory: ")
    (icicle-abs-file-candidates
     (mapcar #'(lambda (file) (if (file-directory-p file) (concat file "/") file))
             (directory-files default-directory 'full nil 'nosort)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (define-key minibuffer-local-completion-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-must-match-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-completion-map "\C-c\C-d"            'icicle-cd-for-abs-files)
    (define-key minibuffer-local-must-match-map "\C-c\C-d"            'icicle-cd-for-abs-files)
    (define-key minibuffer-local-completion-map "\C-c+"               'icicle-make-directory)
    (define-key minibuffer-local-must-match-map "\C-c+"               'icicle-make-directory)
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Undo code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Last code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil)))


;; This is a minibuffer command.  It is in this file because it is used only here.
;;;###autoload
(defun icicle-cd-for-abs-files (dir)
  "Change `default-directory' during `icicle-find-file-absolute'."
  (interactive
   ;; Should not need to bind `minibuffer-completion-predicate'.  Emacs 23.2 bug, per Stefan.
   (let ((enable-recursive-minibuffers     t)
         (minibuffer-completion-predicate  minibuffer-completion-predicate))
     (list (funcall (if (fboundp 'read-directory-name)
                        #'read-directory-name
                      #'read-file-name)
                    "Change default directory: " nil nil
                    (and (member cd-path '(nil ("./"))) (null (getenv "CDPATH")))))))
  (cd dir)
  (let* ((icicle-abs-file-candidates
          (mapcar #'(lambda (file) (if (file-directory-p file) (concat file "/") file))
                  (directory-files default-directory 'full nil 'nosort)))
         (collection
          (mapcar (if icicle-list-use-nth-parts #'icicle-make-file+date-candidate #'list)
                  icicle-abs-file-candidates)))
    (setq minibuffer-completion-table
          (car (icicle-mctize-all collection minibuffer-completion-predicate)))))


(put 'icicle-find-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-file-command icicle-find-file
  "Visit a file or directory.
If you use a prefix argument when you act on a candidate file name,
then you visit the file in read-only mode.

If you use a prefix arg for the command itself, this reverses the
effect of using a prefix arg on individual candidates.  That is, with
a prefix arg for the command, files are visited in read-only mode by
default and a prefix arg for an individual file visits it without
read-only mode.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra absolute file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (file)                        ; Function to perform the action
    (let* ((r-o  (if (eq this-command 'icicle-candidate-action)
                     (or (and init-pref-arg        (not current-prefix-arg))
                         (and (not init-pref-arg)  current-prefix-arg))
                   init-pref-arg))
           (fn   (if r-o 'find-file-read-only 'find-file)))
      (funcall fn file 'WILDCARDS)))
  (concat "File or directory" (and init-pref-arg " (read-only)") ": ") ; `read-file-name' args
  nil (if (and (eq major-mode 'dired-mode) (fboundp 'dired-get-file-for-visit)) ; Emacs 22+.
          (condition-case nil           ; E.g. error because not on file line (ignore)
              (abbreviate-file-name (dired-get-file-for-visit))
            (error nil))
        default-directory)
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil nil
  (icicle-file-bindings                 ; Bindings
   ((init-pref-arg  current-prefix-arg)
    (icicle-all-candidates-list-alt-action-fn ; `M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (unless (boundp 'minibuffer-local-filename-completion-map) ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))

;;;###autoload
(icicle-define-file-command icicle-find-file-other-window
  "Visit a file or directory in another window.
Same as `icicle-find-file' except it uses a different window.

Note: Unlike standard `find-file-other-window', in Dired the file or
directory on the current line is the default value.  Like
`find-file-other-window', if you just hit `RET' without entering any
text, that is the same as if you first use `M-n' to retrieve the
default value and then hit `RET'.  Thus, if you just want to visit, in
a different window, the current directory instead of the file or
directory of the current line, you should use `\\<minibuffer-local-completion-map>\
\\[icicle-erase-minibuffer-or-history-element]' to first empty the
minibuffer.  Or you can just use a different command, such as `\\[dired]',
to visit the current directory."        ; Doc string
  (lambda (file)                        ; Function to perform the action
    (let* ((r-o  (if (eq this-command 'icicle-candidate-action)
                     (or (and init-pref-arg        (not current-prefix-arg))
                         (and (not init-pref-arg)  current-prefix-arg))
                   init-pref-arg))
           (fn   (if r-o 'find-file-read-only-other-window 'find-file-other-window)))
      (funcall fn file 'WILDCARDS)))
  (concat "File or directory" (and init-pref-arg " (read-only)") ": ") ; `read-file-name' args
  nil (if (and (eq major-mode 'dired-mode) (fboundp 'dired-get-file-for-visit)) ; Emacs 22+.
          (condition-case nil           ; E.g. error because not on file line (ignore)
              (abbreviate-file-name (dired-get-file-for-visit))
            (error nil))
        default-directory)
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil nil
  (icicle-file-bindings                 ; Bindings
   ((init-pref-arg  current-prefix-arg)
    (icicle-all-candidates-list-alt-action-fn ; `M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (unless (boundp 'minibuffer-local-filename-completion-map) ; First code
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Undo code
         (define-key minibuffer-local-must-match-map "\C-xm" nil))
  (progn (define-key minibuffer-local-completion-map "\C-xm" nil) ; Last code
         (define-key minibuffer-local-must-match-map "\C-xm" nil)))


(put 'icicle-find-file-read-only 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-find-file-read-only ()    ; Bound to `C-x C-r' in Icicle mode.
  "Visit a file or directory in read-only mode.
If you use a prefix argument when you act on a candidate file name,
then visit the file without read-only mode.

If you use a prefix arg for the command itself, this reverses the
effect of using a prefix arg on individual candidates.  That is, with
a prefix arg for the command, files are not visited in read-only mode
by default and a prefix arg for an individual file visits it in
read-only mode."
  (interactive)
  (let ((current-prefix-arg  (not current-prefix-arg)))
    (icicle-find-file)))

;;;###autoload
(defun icicle-find-file-read-only-other-window ()    ; Bound to `C-x 4 r' in Icicle mode.
  "Visit a file or directory in read-only mode in another window.
Same as `icicle-find-file-read-only' except use a different window."
  (interactive)
  (let ((current-prefix-arg  (not current-prefix-arg)))
    (icicle-find-file-other-window)))


(put 'icicle-recent-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-recent-file ; Command name
  "Open a recently used file.
With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-hiding-common-match]' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

You can use any of the alternative-action keys, such as `C-S-RET', to
remove a candidate file from the recent files list, `recentf-list'.
\(The file itself is not deleted.)

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra absolute file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'file-name-history (car recentf-list) nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                                 "Recent file: ")
    (icicle-abs-file-candidates             (progn (unless (boundp 'recentf-list) (require 'recentf))
                                                   (when (fboundp 'recentf-mode) (recentf-mode 99))
                                                   (unless (consp recentf-list)
                                                     (error "No recently accessed files"))
                                                   recentf-list))
    (icicle-candidate-alt-action-fn         'icicle-remove-from-recentf-candidate-action)
    (icicle-use-candidates-only-once-alt-p  t)
    (icicle-candidate-properties-alist      (and current-prefix-arg
                                                 '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts              (and current-prefix-arg '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion
                                                         files))))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (define-key minibuffer-local-completion-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-must-match-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-completion-map "\C-c+"               'icicle-make-directory)
    (define-key minibuffer-local-must-match-map "\C-c+"               'icicle-make-directory)
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Undo code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Last code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil)))

;;;###autoload
(icicle-define-command icicle-recent-file-other-window ; Command name
  "Open a recently used file in another window.
Same as `icicle-recent-file' except it uses a different window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          icicle-abs-file-candidates)
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'file-name-history (car recentf-list) nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                                 "Recent file: ")
    (icicle-abs-file-candidates             (progn (unless (boundp 'recentf-list) (require 'recentf))
                                                   (when (fboundp 'recentf-mode) (recentf-mode 99))
                                                   (unless (consp recentf-list)
                                                     (error "No recently accessed files"))
                                                   recentf-list))
    (icicle-candidate-alt-action-fn         'icicle-remove-from-recentf-candidate-action)
    (icicle-use-candidates-only-once-alt-p  t)
    (icicle-candidate-properties-alist      (and current-prefix-arg
                                                 '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts              (and current-prefix-arg '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion
                                                         files))))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (define-key minibuffer-local-completion-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-must-match-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-completion-map "\C-c+"               'icicle-make-directory)
    (define-key minibuffer-local-must-match-map "\C-c+"               'icicle-make-directory)
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Undo code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Last code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil)))

;;;###autoload
(icicle-define-command icicle-remove-file-from-recentf-list
  "Remove file from `recentf-list' - the list of recently used files."
  icicle-remove-from-recentf-candidate-action
  "Remove from recent files list, `recentf-list': "
  (mapcar #'list (progn (unless (boundp 'recentf-list) (require 'recentf))
                        (when (fboundp 'recentf-mode) (recentf-mode 99))
                        (unless (consp recentf-list) (error "No recently accessed files"))
                        recentf-list))
  nil (and (fboundp 'confirm-nonexistent-file-or-buffer) ; Emacs23.
           (confirm-nonexistent-file-or-buffer))
  nil 'file-name-history (car recentf-list) nil
  ((icicle-use-candidates-only-once-flag  t)))

(defun icicle-remove-from-recentf-candidate-action (file)
  "Action function for command `icicle-remove-file-from-recentf-list'."
  (setq recentf-list  (delete file recentf-list))
  (message "`%s' removed from `recentf-list'" file))


(put 'icicle-locate-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-locate-file ()
  "Visit a file within a directory or its subdirectories.
With a non-negative (>= 0) prefix argument, you are prompted for the
directory.  Otherwise, the current directory is used.

With a non-positive (<= 0) prefix argument, you can choose also by
date: Completion candidates include the last modification date.

The absolute names of all files within the directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-hiding-common-match]' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

You can use this command to find all files within your file system
that match a regexp, but be aware that gathering and matching the file
names will take some time.

See also command `icicle-locate-file-no-symlinks', which does the same
thing but without following symbolic links.

Remember that you can save the set of files matching your input using
`\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-persistently]'.  You can then retrieve quickly them later using
`\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-persistent]'.

Note that completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `C-c C-d' (think `cd') to change the `default-directory'.
 You can use `C-c +' to create a new directory.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

Directories in `icicle-ignored-directories' are ignored (skipped).  In
addition, these options control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-require-match-flag' - See `icicle-require-match-flag'
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))"
  (interactive)
  (let ((icicle-locate-file-action-fn  'icicle-locate-file-action)
        (no-symlinks-p                 nil))
    (icicle-locate-file-1)))

;;;###autoload
(defun icicle-locate-file-other-window ()
  "Same as `icicle-locate-file' except visit file in a different window.
See also command `icicle-locate-file-no-symlinks-other-window', which
does not follow symbolic links."
  (interactive)
  (let ((icicle-locate-file-action-fn  'icicle-locate-file-other-window-action)
        (no-symlinks-p                 nil))
    (icicle-locate-file-1)))

(put 'icicle-locate-file-no-symlinks 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-locate-file-no-symlinks ()
  "`icicle-locate-file' except do not follow symlinks."
  (interactive)
  (let ((icicle-locate-file-action-fn  'icicle-locate-file-other-window-action)
        (no-symlinks-p                 t))
    (icicle-locate-file-1)))

;;;###autoload
(defun icicle-locate-file-no-symlinks-other-window ()
  "`icicle-locate-file-no-symlinks' except visit file in different window."
  (interactive)
  (let ((icicle-locate-file-action-fn  'icicle-locate-file-other-window-action)
        (no-symlinks-p                 t))
    (icicle-locate-file-1)))

(defun icicle-locate-file-action (file)
  "Action function for `icicle-locate-file'."
  (find-file (icicle-transform-multi-completion file) 'WILDCARDS))

(defun icicle-locate-file-other-window-action (file)
  "Action function for `icicle-locate-file-other-window'."
  (find-file-other-window (icicle-transform-multi-completion file) 'WILDCARDS))

;;;###autoload
(icicle-define-command icicle-locate-file-1
  "Helper function for `icicle-locate-file(-other-window)'." ; Doc string
  ;; `icicle-locate-file-action-fn' is free here.
  (lambda (f) (funcall icicle-locate-file-action-fn f)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if (<= (prefix-numeric-value current-prefix-arg) 0)
              #'icicle-make-file+date-candidate
            #'list)
          icicle-abs-file-candidates)
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File: ")
    (dir                                (if (and current-prefix-arg
                                                 (wholenump (prefix-numeric-value
                                                             current-prefix-arg)))
                                            (read-file-name "Locate under which directory: " nil
                                                            default-directory nil)
                                          default-directory))
    (IGNORED--FOR-SIDE-EFFECT           (progn
                                          (icicle-highlight-lighter)
                                          (message "Gathering files within `%s' (this could take \
a while)..." dir)))
    (icicle-abs-file-candidates         ; `no-symlinks-p' is free here.
     (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil no-symlinks-p))
    (use-dialog-box                     nil)
    (icicle-candidate-properties-alist  (and (<= (prefix-numeric-value current-prefix-arg) 0)
                                             '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and (<= (prefix-numeric-value current-prefix-arg) 0)
                                             '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ")
                                                 (mapcar #'icicle-transform-multi-completion
                                                         files))))))))
  (progn                                ; First code
    (when (<= (prefix-numeric-value current-prefix-arg) 0)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (define-key minibuffer-local-completion-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-must-match-map [(control backspace)] 'icicle-up-directory)
    (define-key minibuffer-local-completion-map "\C-c\C-d"            'icicle-cd-for-loc-files)
    (define-key minibuffer-local-must-match-map "\C-c\C-d"            'icicle-cd-for-loc-files)
    (define-key minibuffer-local-completion-map "\C-c+"               'icicle-make-directory)
    (define-key minibuffer-local-must-match-map "\C-c+"               'icicle-make-directory)
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-file-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-file-other-window)
      (define-key minibuffer-local-must-match-map "\C-xm" 'icicle-bookmark-file-other-window)))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Undo code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  (progn (define-key minibuffer-local-completion-map [(control backspace)] nil) ; Last code
         (define-key minibuffer-local-must-match-map [(control backspace)] nil)
         (define-key minibuffer-local-completion-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-must-match-map "\C-c\C-d"            nil)
         (define-key minibuffer-local-completion-map "\C-c+"               nil)
         (define-key minibuffer-local-must-match-map "\C-c+"               nil)
         (define-key minibuffer-local-completion-map "\C-xm"               nil)
         (define-key minibuffer-local-must-match-map "\C-xm"               nil))
  'NON-INTERACTIVE)                     ; This is not a real command.

;; This is a minibuffer command.  It is in this file because it is used only here.
;;;###autoload
(defun icicle-cd-for-loc-files (dir &optional no-symlinks-p)
  "Change `default-directory' during `icicle-locate-file'.
Optional arg NO-SYMLINKS-P non-nil means do not follow symbolic links."
  (interactive
   (save-selected-window
     ;; Should not need to bind `minibuffer-completion-predicate'.  Emacs 23.2 bug, per Stefan.
     (let ((minibuffer-completion-predicate  minibuffer-completion-predicate))
       (list (funcall (if (fboundp 'read-directory-name)
                          #'read-directory-name
                        #'read-file-name)
                      "Change default directory: " nil nil
                      (and (member cd-path '(nil ("./"))) (null (getenv "CDPATH"))))))))
  (cd dir)
  (let* ((icicle-abs-file-candidates
          (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil no-symlinks-p))
         (collection
          (mapcar (if icicle-list-use-nth-parts #'icicle-make-file+date-candidate #'list)
                  icicle-abs-file-candidates)))
    (setq minibuffer-completion-table
          (car (icicle-mctize-all collection minibuffer-completion-predicate)))))


(put 'icicle-find-file-in-tags-table 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-in-tags-table ; Command name
  "Visit a file listed in a tags table, given the absolute file name.
The completion candidates are absolute, not relative, file names.
You can complete against any and all parts of the absolute name.

By default, the completion candidates are the file names listed in the
current tags table, but you can substitute other candidates by
retrieving a saved candidate set.

Completion here matches candidates as ordinary strings.  It
knows nothing of file names per se.  In particular, you cannot use
remote file-name syntax.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-hiding-common-match]' to hide the common match portion of
each candidate.  That can be particularly helpful for files that are
in a common directory.

With a prefix argument, you can choose also by date: Completion
candidates include the last modification date.

During completion:
 You can use `C-x m' to access file bookmarks, if you use library
  `bookmark+.el'.
 You can use `M-|' to open Dired on the currently matching file names.
 You can use `S-delete' to delete a candidate file or (empty)
  directory.

These options, when non-nil, control candidate matching and filtering:

 `icicle-file-extras'           - Extra file names to display
 `icicle-file-match-regexp'     - Regexp that file names must match
 `icicle-file-no-match-regexp'  - Regexp file names must not match
 `icicle-file-predicate'        - Predicate file names must satisfy
 `icicle-file-sort'             - Sort function for candidates

For example, to show only names of files larger than 5000 bytes, set
`icicle-file-predicate' to:

  (lambda (file) (> (nth 5 (file-attributes file)) 5000))

Option `icicle-file-require-match-flag' can be used to override
option `icicle-require-match-flag'.

Option `icicle-files-ido-like' non-nil gives this command a more
Ido-like behavior."                     ; Doc string
  (lambda (f) (find-file (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action function
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          (save-excursion (let ((enable-recursive-minibuffers  t)) (visit-tags-table-buffer))
                          (tags-table-files)))
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File (in tags table): ")
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (unless (require 'etags nil t) (error "`etags.el' is required"))))


(put 'icicle-find-file-in-tags-table-other-window 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-find-file-in-tags-table-other-window ; Command name
  "Visit a tags-table file in another window, given its absolute name.
Same as `icicle-find-file-in-tags-table', but uses a different window." ; Doc string
  (lambda (f) (find-file-other-window (icicle-transform-multi-completion f) 'WILDCARDS)) ; Action
  prompt                                ; `completing-read' args
  (mapcar (if current-prefix-arg #'icicle-make-file+date-candidate #'list)
          (save-excursion (let ((enable-recursive-minibuffers  t)) (visit-tags-table-buffer))
                          (tags-table-files)))
  nil
  (and (fboundp 'confirm-nonexistent-file-or-buffer) (confirm-nonexistent-file-or-buffer)) ;Emacs23.
  nil 'file-name-history nil nil
  (icicle-file-bindings                 ; Bindings
   ((prompt                             "File (in tags table): ")
    (icicle-special-candidate-regexp    (or icicle-special-candidate-regexp ".+/$"))
    (icicle-candidate-properties-alist  (and current-prefix-arg '((1 (face icicle-candidate-part)))))
    (icicle-list-use-nth-parts          (and current-prefix-arg '(1)))
    (icicle-all-candidates-list-alt-action-fn ; M-|'
     (lambda (files) (let ((enable-recursive-minibuffers  t))
                       (dired-other-window (cons (read-string "Dired buffer name: ") files)))))))
  (progn                                ; First code
    (when current-prefix-arg (put-text-property 0 1 'icicle-fancy-candidates t prompt))
    (unless (require 'etags nil t) (error "`etags.el' is required"))))

(defun icicle-make-file+date-candidate (file)
  "Return a multi-completion candidate: FILE + last modification date."
  (list (list file (format-time-string "%Y %m %d %T " (nth 5 (file-attributes file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd1.el ends here

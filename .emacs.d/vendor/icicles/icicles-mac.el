;;; icicles-mac.el --- Macros for Icicles
;;
;; Filename: icicles-mac.el
;; Description: Macros for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2009, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:24:28 2006
;; Version: 22.0
;; Last-Updated: Sat Jun  5 06:40:17 2010 (-0700)
;;           By: dradams
;;     Update #: 535
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mac.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  macros.  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  Macros defined here:
;;
;;    `icicle-buffer-bindings', `icicle-define-add-to-alist-command',
;;    `icicle-define-command', `icicle-define-file-command',
;;    `icicle-define-sort-command', `icicle-file-bindings',
;;    `icicle-with-selected-window'.
;;
;;  Functions defined here:
;;
;;    `icicle-try-switch-buffer'.
;;
;;  Standard Emacs function defined here for older Emacs versions:
;;
;;    `select-frame-set-input-focus'.
;;
;;  You might also be interested in my library `imenu+.el', which
;;  teaches the macros defined here to Imenu, so the functions defined
;;  with those macros show up in Imenu menus.
;;
;;  I've also included some commented-out code at the end, which you
;;  might want to use in your init file (~/.emacs).  It provides
;;  better indentation for the doc string when you use the macros here
;;  in your code.
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
;;  (@> "Macros")
;;  (@> "Functions")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 20:
;;
;; the function x-focus-frame is not known to be defined.

(eval-when-compile (when (< emacs-major-version 20) (require 'cl))) ;; when, unless

;; Quiet the byte compiler for Emacs versions before 22.  For some reason, a value is required.
(unless (boundp 'minibuffer-completing-symbol)
  (defvar minibuffer-completing-symbol nil)
  (defvar minibuffer-message-timeout 2)
  (defvar minibuffer-prompt-properties nil))

;; Quiet the byte-compiler.
(defvar icicle-inhibit-try-switch-buffer)
(defvar read-file-name-completion-ignore-case)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

(if (fboundp 'with-selected-window)     ; Emacs 22+
    (defalias 'icicle-with-selected-window (symbol-function 'with-selected-window))
  (defmacro icicle-with-selected-window (window &rest body)
    "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window of each frame.  It does not change the order of
recently selected windows.  If the previously selected window of
some frame is no longer live at the end of BODY, that frame's
selected window is left alone.  If the selected window is no
longer live, then whatever window is selected at the end of BODY
remains selected.

This macro uses `save-current-buffer' to save and restore the
current buffer, since otherwise its normal operation could
potentially make a different buffer current.  It does not alter
the buffer list ordering."
    (when (fboundp 'declare) (declare (indent 1) (debug t)))
    ;; Most of this code is a copy of save-selected-window.
    `(let ((save-selected-window-window (selected-window))
           ;; It is necessary to save all of these, because calling
           ;; select-window changes frame-selected-window for whatever
           ;; frame that window is in.
           (save-selected-window-alist
            (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
             (frame-list))))
      (save-current-buffer
        (unwind-protect
             (progn (if (> emacs-major-version 21)
                        (select-window ,window 'norecord) ; Emacs 22+
                      (select-window ,window))
                    ,@body)
          (dolist (elt save-selected-window-alist)
            (and (frame-live-p (car elt))
                 (window-live-p (cadr elt))
                 (if (> emacs-major-version 22)
                     (set-frame-selected-window (car elt) (cadr elt) 'norecord) ; Emacs 23+
                   (set-frame-selected-window (car elt) (cadr elt)))))
          (when (window-live-p save-selected-window-window)
            (if (> emacs-major-version 21)
                (select-window save-selected-window-window 'norecord) ; Emacs 22+
              (select-window save-selected-window-window))))))))

(defmacro icicle-define-add-to-alist-command (command doc-string construct-item-fn alist-var
                                              &optional dont-save)
  "Define COMMAND that adds an item to an alist user option.
Any items with the same key are first removed from the alist.
DOC-STRING is the doc string of COMMAND.
CONSTRUCT-ITEM-FN is a function that constructs the new item.
  It reads user input.
ALIST-VAR is the alist user option.
Optional arg DONT-SAVE non-nil means do not call
`customize-save-variable' to save the updated variable."
  `(defun ,command ()
    ,(concat doc-string "\n\nNote: Any items with the same key are first removed from the alist.")
    (interactive)
    (let ((new-item  (funcall ,construct-item-fn)))
      (setq ,alist-var  (icicle-assoc-delete-all (car new-item) ,alist-var))
      (push new-item ,alist-var)
      ,(unless dont-save `(customize-save-variable ',alist-var ,alist-var))
      (message "Added to `%s': `%S'" ',alist-var new-item))))

(defmacro icicle-buffer-bindings (&optional more-bindings)
  "Bindings to use in multi-command definitions for buffer names.
MORE-BINDINGS is a list of additional bindings, which are created
before the others."
  `(,@more-bindings
    (completion-ignore-case                      (or (and (boundp 'read-buffer-completion-ignore-case)
                                                      read-buffer-completion-ignore-case)
                                                  completion-ignore-case))
    (icicle-show-Completions-initially-flag      (or icicle-show-Completions-initially-flag
                                                     icicle-buffers-ido-like-flag))
    (icicle-top-level-when-sole-completion-flag  (or icicle-top-level-when-sole-completion-flag
                                                     icicle-buffers-ido-like-flag))
    (icicle-default-value                        (if (and icicle-buffers-ido-like-flag
                                                          icicle-default-value)
                                                     icicle-buffers-ido-like-flag
                                                   icicle-default-value))
    (icicle-must-match-regexp                    icicle-buffer-match-regexp)
    (icicle-must-not-match-regexp                icicle-buffer-no-match-regexp)
    (icicle-must-pass-predicate                  icicle-buffer-predicate)
    (icicle-require-match-flag                   icicle-buffer-require-match-flag)
    (icicle-extra-candidates                     icicle-buffer-extras)
    (icicle-ignore-space-prefix-flag             icicle-buffer-ignore-space-prefix-flag)
    (icicle-delete-candidate-object              'icicle-kill-a-buffer) ; `S-delete' kills current buf
    (icicle-transform-function                   'icicle-remove-dups-if-extras)
    (icicle-sort-comparer                        (or icicle-buffer-sort icicle-sort-comparer))
    (icicle-sort-orders-alist
     (append (list
              '("by last access")       ; Renamed from "turned OFF'.
              '("*...* last" . icicle-buffer-sort-*...*-last)
              '("by buffer size" . icicle-buffer-smaller-p)
              '("by major mode name" . icicle-major-mode-name-less-p)
              (and (fboundp 'icicle-mode-line-name-less-p)
               '("by mode-line mode name" . icicle-mode-line-name-less-p))
              '("by file/process name" . icicle-buffer-file/process-name-less-p))
      (delete '("turned OFF") icicle-sort-orders-alist)))
    (icicle-candidate-alt-action-fn
     (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
    (icicle-all-candidates-list-alt-action-fn
     (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
    (bufflist
     (if current-prefix-arg
         (if (wholenump (prefix-numeric-value current-prefix-arg))
             (icicle-remove-if-not #'(lambda (bf) (buffer-file-name bf)) (buffer-list))
           (cdr (assq 'buffer-list (frame-parameters))))
       (buffer-list)))))

(defmacro icicle-file-bindings (&optional more-bindings)
  "Bindings to use in multi-command definitions for file names.
MORE-BINDINGS is a list of additional bindings, which are created
before the others."
  `(,@more-bindings
    (completion-ignore-case
     (or (and (boundp 'read-file-name-completion-ignore-case) read-file-name-completion-ignore-case)
      completion-ignore-case))
    (icicle-show-Completions-initially-flag      (or icicle-show-Completions-initially-flag
                                                  icicle-files-ido-like-flag))
    (icicle-top-level-when-sole-completion-flag  (or icicle-top-level-when-sole-completion-flag
                                                  icicle-files-ido-like-flag))
    (icicle-default-value                        (if (and icicle-files-ido-like-flag
                                                          icicle-default-value)
                                                     icicle-files-ido-like-flag
                                                   ;;  Get default via `M-n', but do not insert it.
                                                   (and (memq icicle-default-value '(t nil))
                                                        icicle-default-value)))
    (icicle-must-match-regexp                    icicle-file-match-regexp)
    (icicle-must-not-match-regexp                icicle-file-no-match-regexp)
    (icicle-must-pass-predicate                  icicle-file-predicate)
    (icicle-require-match-flag                   icicle-file-require-match-flag)
    (icicle-extra-candidates                     icicle-file-extras)
    (icicle-transform-function                   'icicle-remove-dups-if-extras)
    (icicle-sort-comparer                        (or icicle-file-sort icicle-sort-comparer))
    (icicle-ignore-space-prefix-flag             icicle-buffer-ignore-space-prefix-flag)
    (icicle-candidate-alt-action-fn
     (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "file")))
    (icicle-all-candidates-list-alt-action-fn
     (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "file")))
    (icicle-delete-candidate-object   'icicle-delete-file-or-directory)))

(defmacro icicle-define-command
    (command doc-string function prompt collection &optional
     predicate require-match initial-input hist def inherit-input-method
     bindings first-sexp undo-sexp last-sexp not-interactive-p)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-down'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-up'    instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one argument, read as input.
  (If the argument to FUNCTION is a file name or directory name, then
  use macro `icicle-define-file-command', instead.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)
BINDINGS is macroexpanded, so it can also be a macro call that expands
to a list of bindings.  For example, you can use
`icicle-buffer-bindings' here.

In case of user quit (`C-g') or error, an attempt is made to restore
the original buffer.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.
NOT-INTERACTIVE-P non-nil means to define COMMAND as a non-interactive
 function that reads multi-command input.

Other arguments are as for `completing-read'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `completing-read', using PROMPT, COLLECTION,
   PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
   INHERIT-INPUT-METHOD.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that the BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "'\nto "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

\\<minibuffer-local-completion-map>\
`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Move to next prefix-completion candidate and act
`C-up'    - Move to previous prefix-completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

When candidate action and cycling are combined (e.g. `C-next'), user
option `icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'.")
    ,(and (not not-interactive-p) '(interactive))
    (let* ((orig-buff    (current-buffer))
           (orig-window  (selected-window))
           ,@(macroexpand bindings)
           (icicle-candidate-action-fn
            (lambda (candidate)
              (let ((minibuffer-completion-table      minibuffer-completion-table)
                    (minibuffer-completion-predicate  minibuffer-completion-predicate)
                    (minibuffer-completion-confirm    minibuffer-completion-confirm)
                    (minibuffer-completing-file-name  minibuffer-completing-file-name)
                    (minibuffer-completing-symbol     (and (boundp 'minibuffer-completing-symbol)
                                                           minibuffer-completing-symbol))
                    (minibuffer-exit-hook             minibuffer-exit-hook)
                    (minibuffer-help-form             minibuffer-help-form)
                    (minibuffer-history-variable      minibuffer-history-variable)
                    (minibuffer-history-case-insensitive-variables
                     minibuffer-history-case-insensitive-variables)
                    (minibuffer-history-sexp-flag     minibuffer-history-sexp-flag)
                    (minibuffer-message-timeout       (and (boundp 'minibuffer-message-timeout)
                                                           minibuffer-message-timeout))
                    (minibuffer-prompt-properties     (and (boundp 'minibuffer-prompt-properties)
                                                           minibuffer-prompt-properties))
                    (minibuffer-setup-hook            minibuffer-setup-hook)
                    (minibuffer-text-before-history   minibuffer-text-before-history))
                (condition-case in-action-fn
                    ;; Treat 3 cases, because previous use of `icicle-candidate-action-fn'
                    ;; might have killed the buffer or deleted the window.
                    (cond ((and (buffer-live-p orig-buff) (window-live-p orig-window))
                           (with-current-buffer orig-buff
                             (save-selected-window (select-window orig-window)
                                                   (funcall ',function candidate))))
                          ((window-live-p orig-window)
                           (save-selected-window (select-window orig-window)
                                                 (funcall ',function candidate)))
                          (t
                           (funcall ',function candidate)))
                  (error (unless (string= "Cannot switch buffers in minibuffer window"
                                          (error-message-string in-action-fn))
                           (error "%s" (error-message-string in-action-fn)))
                         (when (window-live-p orig-window)
                           (select-window orig-window)
                           (select-frame-set-input-focus (selected-frame)))
                         (funcall ',function candidate)))
                (select-window (minibuffer-window))
                (select-frame-set-input-focus (selected-frame))
                nil))))                 ; Return nil for success.
      ,first-sexp
      (condition-case act-on-choice
          (let ((cmd-choice  (completing-read ,prompt ,collection ,predicate ,require-match
                                              ,initial-input ,hist ,def ,inherit-input-method)))
            ;; Reset after reading input, so that commands can tell whether input has been read.
            (setq icicle-candidate-action-fn  nil)
            (funcall ',function cmd-choice))
        (quit  (icicle-try-switch-buffer orig-buff) ,undo-sexp)
        (error (icicle-try-switch-buffer orig-buff) ,undo-sexp
               (error "%s" (error-message-string act-on-choice))))
      ,last-sexp)))

(defmacro icicle-define-file-command
    (command doc-string function prompt &optional
     dir default-filename require-match initial-input predicate
     bindings first-sexp undo-sexp last-sexp not-interactive-p)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-down'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-up'    instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one file-name or directory-name
argument, read as input.  (Use macro `icicle-define-command' for a
FUNCTION whose argument is not a file or directory name.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)
BINDINGS is macroexpanded, so it can also be a macro call that expands
to a list of bindings.  For example, you can use
`icicle-buffer-bindings' here.

In case of user quit (`C-g') or error, an attempt is made to restore
the original buffer.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.
NOT-INTERACTIVE-P non-nil means to define COMMAND as a non-interactive
 function that reads multi-command input.

Other arguments are as for `read-file-name'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `read-file-name', using PROMPT, DIR,
   DEFAULT-FILENAME, REQUIRE-MATCH, INITIAL-INPUT, and PREDICATE.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that the BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "'\nto "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

\\<minibuffer-local-completion-map>\
`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Move to next prefix-completion candidate and act
`C-up'    - Move to previous prefix-completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

When candidate action and cycling are combined (e.g. `C-next'), user
option `icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'.")
    ,(and (not not-interactive-p) '(interactive))
    (let* ((orig-buff    (current-buffer))
           (orig-window  (selected-window))
           ,@(macroexpand bindings)
           (icicle-candidate-action-fn
            (lambda (candidate)
              (let ((minibuffer-completion-table      minibuffer-completion-table)
                    (minibuffer-completion-predicate  minibuffer-completion-predicate)
                    (minibuffer-completion-confirm    minibuffer-completion-confirm)
                    (minibuffer-completing-file-name  minibuffer-completing-file-name)
                    (minibuffer-completing-symbol     (and (boundp 'minibuffer-completing-symbol)
                                                           minibuffer-completing-symbol))
                    (minibuffer-exit-hook             minibuffer-exit-hook)
                    (minibuffer-help-form             minibuffer-help-form)
                    (minibuffer-history-variable      minibuffer-history-variable)
                    (minibuffer-history-case-insensitive-variables
                     minibuffer-history-case-insensitive-variables)
                    (minibuffer-history-sexp-flag     minibuffer-history-sexp-flag)
                    (minibuffer-message-timeout       (and (boundp 'minibuffer-message-timeout)
                                                           minibuffer-message-timeout))
                    (minibuffer-prompt-properties     (and (boundp 'minibuffer-prompt-properties)
                                                           minibuffer-prompt-properties))
                    (minibuffer-setup-hook            minibuffer-setup-hook)
                    (minibuffer-text-before-history   minibuffer-text-before-history))
                (setq candidate  (expand-file-name
                                  candidate (icicle-file-name-directory icicle-last-input)))
                (condition-case in-action-fn
                    ;; Treat 3 cases, because previous use of `icicle-candidate-action-fn'
                    ;; might have deleted the file or the window.
                    (cond ((and (buffer-live-p orig-buff) (window-live-p orig-window))
                           (with-current-buffer orig-buff
                             (save-selected-window (select-window orig-window)
                                                   (funcall ',function candidate))))
                          ((window-live-p orig-window)
                           (save-selected-window (select-window orig-window)
                                                 (funcall ',function candidate)))
                          (t
                           (funcall ',function candidate)))
                  (error (unless (string= "Cannot switch buffers in minibuffer window"
                                          (error-message-string in-action-fn))
                           (error "%s" (error-message-string in-action-fn)))
                         (when (window-live-p orig-window)
                           (select-window orig-window)
                           (select-frame-set-input-focus (selected-frame)))
                         (funcall ',function candidate)))
                (select-window (minibuffer-window))
                (select-frame-set-input-focus (selected-frame))
                nil))))                 ; Return nil for success.
      ,first-sexp
      (condition-case act-on-choice
          (let ((file-choice
                 (if (< emacs-major-version 21) ; No predicate arg for Emacs 20.
                     (read-file-name ,prompt ,dir ,default-filename ,require-match ,initial-input)
                   (read-file-name ,prompt ,dir ,default-filename ,require-match
                                   ,initial-input ,predicate))))
            ;; Reset after reading input, so that commands can tell whether input has been read.
            (setq icicle-candidate-action-fn  nil) ; Reset after completion.
            (funcall ',function file-choice))
        (quit  (icicle-try-switch-buffer orig-buff) ,undo-sexp)
        (error (icicle-try-switch-buffer orig-buff) ,undo-sexp
               (error "%s" (error-message-string act-on-choice))))
      ,last-sexp)))

(defmacro icicle-define-sort-command (sort-order comparison-fn doc-string)
  "Define a command to sort completions by SORT-ORDER.
SORT-ORDER is a short string (or symbol) describing the sort order.
 It is used after the phrase \"Sorting is now \".  Examples: \"by date\",
 \"alphabetically\", \"directories first\", and \"previously used first\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `icicle-sort-'.

COMPARISON-FN is a function that compares two strings, returning
 non-nil if and only if the first string sorts before the second.

DOC-STRING is the doc string of the new command."
  (unless (stringp sort-order) (setq sort-order  (symbol-name sort-order)))
  (let ((command  (intern (concat "icicle-sort-"
                                  (replace-regexp-in-string "\\s-+" "-" sort-order)))))
    `(progn
      (setq icicle-sort-orders-alist  (icicle-assoc-delete-all
                                       ,sort-order
                                       icicle-sort-orders-alist))
      (push (cons ,sort-order ',comparison-fn) icicle-sort-orders-alist)
      (defun ,command ()
        ,doc-string
        (interactive)
        (setq icicle-sort-comparer  #',comparison-fn)
        (message "Sorting is now %s%s" ,sort-order (if icicle-reverse-sort-p ", REVERSED" ""))
        (icicle-complete-again-update)))))
 
;;(@* "Functions")

;;; Functions --------------------------------------------------------

(defun icicle-try-switch-buffer (buffer)
  "Try to switch to BUFFER, first in same window, then in other window."
  (when (and (buffer-live-p buffer) (not icicle-inhibit-try-switch-buffer))
    (condition-case err-switch-to
        (switch-to-buffer buffer)
      (error (and (string= "Cannot switch buffers in minibuffer window"
                           (error-message-string err-switch-to))
                  ;; Try another window.  Don't bother if the buffer to switch to is a minibuffer.
                  (condition-case err-switch-other
                      (unless (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
                        (switch-to-buffer-other-window buffer))
                    (error (error-message-string err-switch-other))))))))

(unless (fboundp 'select-frame-set-input-focus) ; Defined in Emacs 22.
  (defun select-frame-set-input-focus (frame)
    "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (cond ((eq window-system 'x) (x-focus-frame frame))
          ((eq window-system 'w32) (w32-focus-frame frame)))
    (cond (focus-follows-mouse (set-mouse-position (selected-frame) (1- (frame-width)) 0)))))


;;; Miscellaneous  -----------------------------------------

;; Make Emacs-Lisp mode fontify definitions of Icicles commands.
(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(" (regexp-opt '("icicle-define-add-to-alist-command" "icicle-define-command"
                               "icicle-define-file-command" "icicle-define-sort-command")
                             t)
             ;; $$ "\\s-+\\(\\sw\\(\\sw\\|\\s_\\)+\\)")
             "\\>[ \t'\(]*\\(\\sw+\\)?")
    (1 font-lock-keyword-face)
    ;; Index (2 or 3) depends on whether or not shy groups are supported.
    ,(list (if (string-match "\\(?:\\)" "") 2 3) font-lock-function-name-face nil t))))

;; This is commented out, but you might also want to use it or something similar.  I use it in
;; my init file.  The `icicle-define-*' lines cause doc strings to be indented correctly.
;; (defun lisp-indentation-hack ()
;;   "Better Lisp indenting.  Use in Lisp mode hooks
;; such as `lisp-mode-hook', `emacs-lisp-mode-hook', and
;; `lisp-interaction-mode-hook'."
;;   (unless (assoc "cl-indent" load-history) (load "cl-indent" nil t))
;;   (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
;;   (setq lisp-indent-maximum-backtracking  10)
;;   (put 'define-derived-mode 'common-lisp-indent-function '(4 4 4 2 &body))
;;   (put 'if 'common-lisp-indent-function '(nil nil &body))
;;   (put 'icicle-define-command 'common-lisp-indent-function '(4 &body))
;;   (put 'icicle-define-file-command 'common-lisp-indent-function '(4 &body))
;;   (put 'icicle-define-sort-command 'common-lisp-indent-function '(4 4 &body))
;;   (put 'icicle-define-add-to-alist-command 'common-lisp-indent-function '(4 &body)))

;; (add-hook 'emacs-lisp-mode-hook 'lisp-indentation-hack)
;; (add-hook 'lisp-mode-hook             'lisp-indentation-hack)
;; (add-hook 'lisp-interaction-mode-hook 'lisp-indentation-hack)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mac.el ends here

;;; elk-test.el --- Emacs Lisp testing framework
;;
;; Copyright (C) 2006-2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.3.2
;; Keywords: lisp
;; URL: http://nschum.de/src/emacs/guess-style/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;; Package-Requires: ((fringe-helper "0.1.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; For best visualization, install fringe-helper, which is available at:
;; http://nschum.de/src/emacs/fringe-helper/
;;
;; Use `deftest' to define a test and `elk-test-group' to define test groups.
;; `elk-test-run' can run tests by name, and `elk-test-run-buffer' runs them by
;; buffer.
;;
;; Tests can be defined anywhere, but dedicated (.elk) test files are
;; encouraged.  A major mode for these can be enabled like this:
;;
;; (add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
;;
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code like this:
;;
;; (deftest "test 1"
;;   (assert-eql 5 (+ 2 3)))
;;
;; (deftest "test 2"
;;   (assert-equal '(x y) (list 'x 'y))
;;   (assert-eq 'x (car '(x y))))
;;
;; (deftest "test 3"
;;   (assert-equal '(x y) (list 'y 'x))) ;; this will fail
;;
;; You can then run every test in the current buffer with `elk-test-run-buffer',
;; in a different buffer with `elk-test-run-a-buffer', or individual tests and
;; test groups with `elk-test-run'.
;;
;; To jump to failures, use `next-error', or click on the links in the error
;; buffer.  The optional `elk-test-result-follow-mode' will automatically
;; display the corresponding failure location.  To enable it by default, use the
;; following configuration:
;;
;; (add-hook 'elk-test-result-mode-hook 'elk-test-result-follow-mode)
;;
;;
;; To bind some keys, add the following to your .emacs:
;;
;; (define-key elk-test-mode-map (kbd "M-<f7>") 'elk-test-run-buffer)
;; (define-key emacs-lisp-mode-map (kbd "<f7>") 'elk-test-run-a-buffer)
;;
;;
;; To create your own assertions, use `assert-that'.  For example, the following
;; code defines `assert-eq' using `assert-that':
;;
;; (defmacro assert-eq (expected actual)
;;   "Assert that ACTUAL equals EXPECTED, or signal a warning."
;;   `(assert-that (lambda (actual) (eq ,expected ,actual))
;;                 actual
;;                 "assert-eq"
;;                 (lambda (actual)
;;                   (format "expected <%s>, was <%s>" ,expected ,actual))))
;;
;;
;;; Change Log:
;;
;; 2009-03-21 (0.3.2)
;;    Made fringe-helper dependency optional.
;;    Removed dependency on CL functions.
;;    Fixed deftest highlighting.
;;    Added `elk-test-result-context-lines'.
;;
;; 2008-06-15 (0.3)
;;    Added `elk-test-result-follow-mode'.
;;    Switched to using button package for links.
;;    Added `elk-test-result-mode'.
;;    Made error buffer `next-error' capable.
;;
;; 2008-06-09 (0.2.1)
;;    Fixed link jumping in terminal mode.  (Thanks to Johan BockgÅÂrd).
;;
;; 2008-04-26 (0.2)
;;    Renamed `run-elk-test' and `run-elk-tests-buffer'.
;;    Replaced `elk-test-error' with regular `error'.
;;    Added major mode.
;;    `assert-error' now takes a regular expression as argument.
;;    Removed defsuite functionality (Use .elk files instead).
;;    `elk-test-run-buffer' no longer evaluates the entire buffer.
;;    Test results are now clickable links.
;;    Added mode menu.
;;    Added failure highlighting in fringes.
;;    Added `assert-that'.
;;
;; 2006-11-04 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'fringe-helper nil t)
(require 'newcomment)
(require 'eldoc)
(require 'compile)
(require 'button)

(defgroup elk-test nil
  "Emacs Lisp testing framework"
  :group 'lisp)

(defface elk-test-deftest
  '((default (:inherit font-lock-keyword-face)))
  "*Face used for `deftest' keyword."
  :group 'elk-test)

(defface elk-test-assertion
  '((default (:inherit font-lock-warning-face)))
  "*Face used for assertions in elk tests."
  :group 'elk-test)

(defface elk-test-success
  '((t (:inherit mode-line-buffer-id
        :background "dark olive green"
        :foreground "black")))
  "Face used for displaying a successful test result."
  :group 'elk-test)

(defface elk-test-success-modified
  '((t (:inherit elk-test-success
        :foreground "orange")))
  "Face used for displaying a successful test result in a modified buffer."
  :group 'elk-test)

(defface elk-test-failure
  '((t (:inherit mode-line-buffer-id
        :background "firebrick"
        :foreground "wheat")))
  "Face used for displaying a failed test result."
  :group 'elk-test)

(defface elk-test-fringe
  '((t (:foreground "red"
        :background "red")))
  "*Face used for bitmaps in the fringe."
  :group 'elk-test)

(defcustom elk-test-use-fringe 'left-fringe
  "*Mark failed tests in the fringe?"
  :group 'elk-test
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Left" left-fringe)
                 (const :tag "Right" right-fringe)))

(defface elk-test-failed-region
  nil
  "*Face used for highlighting failures in buffers."
  :group 'elk-test)

(defcustom elk-test-mode-use-eldoc t
  "*Override `eldoc-documentation-function' and enable `eldoc-mode'."
  :group 'elk-test
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom elk-test-run-on-define nil
  "*If non-nil, run elk-test tests/groups immediately when defining them."
  :group 'elk-test
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom elk-test-pop-to-error-buffer t
  "*If non-nil, pop up the error buffer when a test fails."
  :group 'elk-test
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom elk-test-result-context-lines compilation-context-lines
  "Display this many lines of leading context before the current message.
See `compilation-context-lines'."
  :group 'test-case
  :type '(choice integer (const :tag "No window scrolling" nil)))

(defvar elk-test-result-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'bury-buffer)
    keymap)
  "Keymap used for `elk-test-result-mode'.")

(defvar elk-test-alist nil
  "An alist of all defined elk-test tests/groups and their bodies.")

(defun elk-test-clear ()
  "Remove all tests from memory."
  (setq elk-test-alist nil))

(defsubst elk-test-mapcan (func sequence)
  (apply 'nconc (mapcar func sequence)))

(defun elk-test-run (name &optional string-result)
  "Run the test case defined as NAME.
The result is a list of errors strings, unless STRING-RESULT is set, in which
case a message describing the errors or success is displayed and returned."
  (interactive
   (list (completing-read "Test name: " elk-test-alist nil t)))
  (let ((error-list nil)
        (test-or-group (cdr (assoc name elk-test-alist))))
    (if (not test-or-group)
        (error "Undefined test <%s>" name)
      (setq error-list (if (equal (car test-or-group) 'group)
                           ;; is test group
                           (elk-test-mapcan 'elk-test-run (cdr test-or-group))
                         ;; is simple test
                         (elk-test-run-internal test-or-group)))
      (if (or string-result (interactive-p))
          (message (if error-list
                       (mapconcat 'identity error-list "\n")
                     "Test run was successful."))
        error-list))))

(defun elk-test-find-sub-expression (beg end number)
  (save-excursion
    (goto-char beg)
    (down-list)
    (forward-sexp (1+ number))
    (backward-sexp)
    (bounds-of-thing-at-point 'sexp)))

(defun elk-test-run-buffer-internal ()
  (let ((beg (point))
        (sexp (read (current-buffer)))
        errors)
    ;; only evaluate deftest and require
    (cond
     ((and (cdr sexp) (equal (car sexp) 'require))
      ;; (require ...)
      (condition-case err
          (progn
            (eval sexp)
            nil)
        (error (list nil "require"
                     (cons (cons beg (point))
                           (format "Feature \"%s\" not found" (cdr sexp)))))))
     ((and (cddr sexp) (equal (car sexp) 'deftest))
      ;; (deftest ...)
      (let ((i 2)) ;; 2 because we skip deftest and "name"
        (dolist (sexp (cddr sexp))
          (condition-case err
              (eval sexp)
            (error (push (cons (elk-test-find-sub-expression beg (point) i)
                               (error-message-string err)) errors)))
          (incf i)))
      (if errors
          (list* (cons beg (point)) ;; region
                 (cadr sexp)        ;; test name
                 (nreverse errors)) ;; ((region . error-message) ...)
        t)))))

(defvar elk-test-last-buffer nil)

(defun elk-test-run-buffer (&optional buffer show-results)
  "Run tests defined with `deftest' in BUFFER.
Unless SHOW-RESULTS is nil, a buffer is created that lists all errors."
  (interactive (list nil t))
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (setq elk-test-last-buffer (current-buffer))
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (num 0)
          progress errors)
      (condition-case err
          (progn
            (comment-forward)
            (while (not (= (point) (point-max)))
              (setq progress (point))
              (when (setq err (elk-test-run-buffer-internal))
                (incf num)
                (when (consp err)
                  (push err errors)))
              (comment-forward)))
        (error
         (push (list nil (format "Parsing buffer \"%s\" failed" (buffer-name))
                     (cons (cons progress (point-max))
                           (error-message-string err)))
               errors)))
      (setq errors (nreverse errors))
      (when (eq (derived-mode-p 'elk-test-mode) 'elk-test-mode)
        (elk-test-set-buffer-state (if errors 'failure 'success)))
      (when show-results
        (message "%i tests run (%s errors)" num
                 (if errors (length errors) "No"))
        (elk-test-print-errors (current-buffer) errors))
      (elk-test-mark-failures errors elk-test-use-fringe)
      (elk-test-update-menu `((,(current-buffer) . ,errors)))
      errors)))

(defun elk-test-run-a-buffer (buffer)
  "Like `elk-test-run-buffer', but query for buffer to run."
  (interactive
   (let ((buffer-names (mapcar 'buffer-name (elk-test-buffer-list))))
     (list (completing-read "Buffer: " buffer-names
                            nil t (buffer-name elk-test-last-buffer)))))
  (elk-test-run-buffer buffer t))

(defun elk-test-run-internal (test)
  (let (error-list)
    (dolist (sexp test)
      (condition-case err
          (eval sexp)
        (error (push (error-message-string err) error-list))))
    error-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-equal (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (equal ,expected ,actual)
    (error "assert-equal for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eq (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eq ,expected ,actual)
    (error "assert-eq for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eql (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eql ,expected ,actual)
    (error "assert-eql for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-nonnil (value)
  "Assert that VALUE is not nil, or signal a warning."
  `(unless ,value
     (error "assert-nonnil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-t (value)
  "Assert that VALUE is t, or signal a warning."
  `(unless (eq ,value t)
     (error "assert-t for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-nil (value)
  "Assert that VALUE is nil, or signal a warning."
  `(when ,value
     (error "assert-nil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-that (func form &optional assertion-name error-func)
  "Assert that FUNC returns non-nil on evaluated result of FORM.
FUNC must be a function that takes the result of FORM as an argument and
returns nil to designate a failure, or non-nil to designate success.
ASSERTION-NAME is the name to use when printing errors, it defaults to
\"assert-that <FUNC>\".  ERROR-FUNC is a function given the same arguments
as FUNC that returns an error description.  The error description defaults
to \"was <...>\""
  (unless (funcall func form)
    (unless assertion-name
      (setq assertion-name (format "assert-that <%s>" func)))
    `(error "%s for <%s> failed: %s"
            ,assertion-name ',form
            ,(if error-func
                 (funcall error-func form)
               `(format "was <%s>" ,form)))))

(defmacro assert-error (error-message-regexp &rest body)
  "Assert that BODY raises an `error', or signal a warning.
ERROR-MESSAGE-REGEXP is a regular expression describing the expected error
message.  nil accepts any error.  Use nil with caution, as errors like
'wrong-number-of-arguments' (likely caused by typos) will also be caught!"
  `(let (actual-error)
     (condition-case err
         (progn
           ,@body
           ;; should not be reached, if body throws an error
           (setq actual-error
                 (format "assert-error for <%s> failed: did not raise an error"
                         '(progn ,@body)))
           ;; jump out
           (error ""))
       (error
        (if actual-error
            (error actual-error)
          (when ,error-message-regexp
            (setq actual-error (error-message-string err))
            (unless (string-match ,error-message-regexp actual-error)
              (error "assert-error for <%s> failed: expected <%s>, raised <%s>"
                     '(progn . ,body) ,error-message-regexp actual-error))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst elk-test-set-test (name test-data)
  (let ((match (assoc name elk-test-alist)))
    (if match
        (setcdr match test-data)
      (push (cons name test-data) elk-test-alist))))

(defmacro deftest (name &rest body)
  "Define a test case.
Use `assert-equal', `assert-eq', `assert-eql', `assert-nonnil', `assert-t',
`assert-nil' and `assert-error' to verify the code."
  `(progn (elk-test-set-test ,name ',body)
          ,(if elk-test-run-on-define
               `(elk-test-run ',name ,t)
             name)))

(defun elk-test-group (name &rest tests)
  "Define a test group using a collection of test names.
The resulting group can be run by calling `elk-test-run' with parameter NAME."
  (dolist (test tests)
    (unless (cdr (assoc test elk-test-alist))
      (error "Undefined test <%s>" test)))
  (elk-test-set-test name (cons 'group tests))
  (if elk-test-run-on-define
      (elk-test-run name t)
    name))

(defconst elk-test-font-lock-keywords
  `(("(\\_<\\(deftest\\)\\_>" 1 'elk-test-deftest)
    (,(concat "(\\_<" (regexp-opt '("assert-equal" "assert-eq" "assert-eql"
                                    "assert-nonnil" "assert-t" "assert-nil"
                                    "assert-error" "assert-that") t)
              "\\_>") 1 'elk-test-assertion)))

(defun elk-test-enable-font-lock (&optional fontify)
  (interactive "p")
  (font-lock-add-keywords nil elk-test-font-lock-keywords)
  (when fontify
    (font-lock-fontify-buffer)))

(defun elk-test-buffer-changed-hook (a b)
  "Hook used by `elk-test-set-buffer-state' to recognize modifications."
  (elk-test-set-buffer-state 'success-modified))

(defun elk-test-set-buffer-state (state &optional buffer)
  "Set BUFFER's success state to STATE.
STATE may be either 'success, 'success-modified or 'failure.
If the state is set to 'success, a hook will be installed to switch to
'success-modified on a buffer change automatically."
  (with-current-buffer (or buffer (current-buffer))
    (set (make-local-variable 'mode-name)
         (propertize mode-name 'face
                     (case state
                       ('success 'elk-test-success)
                       ('success-modified 'elk-test-success-modified)
                       ('failure 'elk-test-failure)))))
  (if (eq state 'success)
      (add-hook 'before-change-functions 'elk-test-buffer-changed-hook nil t)
    (remove-hook 'before-change-functions 'elk-test-buffer-changed-hook t)))

;;;###autoload
(define-derived-mode elk-test-mode emacs-lisp-mode
  "elk-test"
  "Major mode for editing elk-tests."
  (elk-test-enable-font-lock)
  (when elk-test-mode-use-eldoc
    (set (make-local-variable 'eldoc-documentation-function)
         'elk-test-eldoc-function)
    (eldoc-mode 1))
  (elk-test-update-menu nil))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))

(defsubst elk-test-shorten-string (str)
  "Shorten STR to 40 characters."
  (if (>= (length str) 40)
      (concat (substring str 0 37) "...")
    str))

(defun elk-test-update-menu (errors)
  "Update the mode menu for `elk-test-mode'."
  (easy-menu-define elk-test-menu (list elk-test-mode-map
                                        elk-test-result-mode-map)
    "elk-test commands"
    `("elk-test"
      ["Run buffer tests" elk-test-run-buffer]
      ["Run all buffer tests" elk-test-run-all-buffers]
      "-"
      ["Show error buffer" elk-test-show-error-buffer
       :visible (get-buffer "*elk-test*")]

      ,@(mapcar (lambda (buf)
                  (vector (buffer-name buf)
                          `(lambda ()
                             (interactive)
                             (switch-to-buffer ,buf))
                          :visible `(buffer-live-p ,buf)))
                (elk-test-buffer-list))
      "-" .
      ,(elk-test-mapcan
        (lambda (buffer-errors)
          (mapcar (lambda (err)
                    (vector
                     (concat (cadr err) " - "
                             (elk-test-shorten-string (cdar (cddr err))))
                     `(lambda ()
                        (interactive)
                        (push-mark)
                        (switch-to-buffer ,(car buffer-errors))
                        (goto-char ,(caar (cddr err))))))
                  (cdr buffer-errors)))
        errors)))
  (easy-menu-add elk-test-menu))

(defun elk-test-buffer-list ()
  "List all buffers in `elk-test-mode'."
  (elk-test-mapcan (lambda (b) (when (with-current-buffer b
                                       (eq major-mode 'elk-test-mode))
                                 (cons b nil)))
          (buffer-list)))

(defun elk-test-run-all-buffers (&optional show-results)
  "Run all buffers in `elk-test-mode'."
  (interactive "p")
  (let ((num-buffers 0)
        (num-errors 0)
        all-errors errors)
    (dolist (buffer (elk-test-buffer-list))
      (setq errors (elk-test-run-buffer buffer))
      (incf num-errors (length errors))
      (incf num-buffers)
      (push (cons buffer errors) all-errors))
    (when show-results
      (message "%i test buffers run (%s errors)" num-buffers
               (if errors num-errors "No"))
      (let ((error-buffer (elk-test-prepare-error-buffer)))
        (dolist (err all-errors)
          (elk-test-print-errors (car err) (cdr err) error-buffer))))
    (elk-test-update-menu all-errors)
    errors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elk-test-fringe-regions nil)
(make-variable-buffer-local 'elk-test-fringe-regions)

(defvar elk-test-regions nil)
(make-variable-buffer-local 'elk-test-regions)

(defun elk-test-unmark-failures ()
  "Remove all highlighting from buffer."
  (interactive)
  (when (fboundp 'fringe-helper-remove)
    (mapc 'fringe-helper-remove elk-test-fringe-regions))
  (mapc 'delete-overlay elk-test-regions)
  (setq elk-test-fringe-regions nil
        elk-test-regions nil))

(defun elk-test-mark-failures (failures which-side)
  "Highlight failed tests."
  (elk-test-unmark-failures)
  (save-excursion
    (dolist (failure failures)
      (dolist (form (cddr failure))
        (and which-side window-system (fboundp 'fringe-helper-insert-region)
             (push (fringe-helper-insert-region (caar form) (cdar form)
                                                'filled-square which-side
                                                'elk-test-fringe)
                   elk-test-fringe-regions))
        (push (make-overlay (caar form) (cdar form))
              elk-test-regions)
        (overlay-put (car elk-test-regions)
                     'elk-test-error (cdr form))
        (overlay-put (car elk-test-regions)
                     'face 'elk-test-failed-region)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elk-test-eldoc-function ()
  "Return the error message for the failure at point.
This function is suitable for use as `eldoc-documentation-function'."
  (interactive)
  (let (prop)
    (dolist (ov (overlays-at (point)))
      (when (setq prop (overlay-get ov 'elk-test-error))
        (when (interactive-p)
          (message "%s" prop))
        (return prop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elk-test-error-pos nil)
(make-variable-buffer-local 'elk-test-error-pos)

(define-derived-mode elk-test-result-mode fundamental-mode
  "elk-test result"
  "Major mode for viewing elk-test results."
  (setq buffer-read-only t)
  (setq elk-test-error-pos nil)
  (setq next-error-function 'elk-test-next-error-function))

(defun elk-test-prepare-error-buffer ()
  "Create and prepare a buffer for displaying errors."
  (with-current-buffer (get-buffer-create "*elk-test*")
    (elk-test-result-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (current-buffer))))

(defsubst elk-test-insert-with-button (text type buffer region)
  (let ((beg (point)))
    (insert text)
    (when region
      (make-text-button beg (point) 'type type
                        'elk-test-buffer buffer
                        'elk-test-region region))))

(defun elk-test-button-action (button)
  (elk-test-jump (button-get button 'elk-test-buffer)
                 (button-get button 'elk-test-region)
                 (button-start button)))

(define-button-type 'elk-test-link
  'follow-link t
  'action #'elk-test-button-action)

(define-button-type 'elk-test-test-link
  :supertype 'elk-test-link
  'help-echo "mouse-2, RET: Jump to this test")

(define-button-type 'elk-test-failure-link
  :supertype 'elk-test-link
  'elk-test-error t
  'help-echo "mouse-2, RET: Jump to this failure")

(defun elk-test-print-errors (original-buffer errors &optional error-buffer)
  (with-current-buffer (or error-buffer (elk-test-prepare-error-buffer))
    (let ((inhibit-read-only t))
      (dolist (err errors)
        (insert "<")
        (elk-test-insert-with-button (cadr err) 'elk-test-test-link
                                     original-buffer (car err))
        (insert "> failed:\n")
        (dolist (failure (cddr err))
          (insert "* ")
          (elk-test-insert-with-button (cdr failure)
                                       'elk-test-failure-link
                                       original-buffer (car failure))
          (insert "\n\n")))
      (setq buffer-read-only t))
    (and errors
         elk-test-pop-to-error-buffer
         (pop-to-buffer (current-buffer)))))

(defun elk-test-jump (buffer region from)
  (let ((msg (copy-marker from))
        (mk (make-marker))
        (end-mk (make-marker))
        (compilation-context-lines elk-test-result-context-lines))
    (set-marker mk (car region) buffer)
    (set-marker end-mk (cdr region) buffer)
    (compilation-goto-locus msg mk end-mk)
    (dolist (mk (list mk end-mk msg))
      (set-marker mk nil))))

(defun elk-test-show-error-buffer ()
  "Pop up the buffer with errors created by `elk-test-run-buffer'."
  (interactive)
  (let ((buffer (get-buffer "*elk-test*")))
    (if buffer
        (switch-to-buffer buffer)
      (message "No error buffer found"))))

(add-to-list 'debug-ignored-errors "^Moved past last failure$")

(defun elk-test-next-error (arg)
  (let ((pos elk-test-error-pos))
    (setq arg (* 2 arg))
    (unless (get-text-property pos 'elk-test-error)
      (setq arg (1- arg)))
    (assert (> arg 0))
    (dotimes (i arg)
      (setq pos (next-single-property-change pos 'elk-test-error))
      (unless pos
        (error "Moved past last failure")))
    pos))

(add-to-list 'debug-ignored-errors "^Moved back before first failure$")

(defun elk-test-previous-error (arg)
  (let ((pos elk-test-error-pos))
    (assert (> arg 0))
    (dotimes (i (* 2 arg))
      (setq pos (previous-single-property-change pos 'elk-test-error))
      (unless pos
        (error "Moved back before first failure")))
    pos))

(defun elk-test-next-error-function (arg reset)
  (when (or reset (null elk-test-error-pos))
    (setq elk-test-error-pos (point-min)))
  (setq elk-test-error-pos
        (if (<= arg 0)
            (elk-test-previous-error (- arg))
          (elk-test-next-error arg)))
  (elk-test-jump (get-text-property elk-test-error-pos 'elk-test-buffer)
                 (get-text-property elk-test-error-pos 'elk-test-region)
                 elk-test-error-pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elk-test-result-follow-last-button nil)
(make-variable-buffer-local 'elk-test-follow-last-button)

(define-minor-mode elk-test-result-follow-mode
  "Minor mode for navigating elk-test results.
When turned on, cursor motion in the result buffer causes automatic
display of the corresponding failure location.

Customize `next-error-highlight' to modify the highlighting."
  nil " Fol" nil
  :group 'elk-test
  (if elk-test-result-follow-mode
      (add-hook 'post-command-hook 'elk-test-result-follow-hook nil t)
    (remove-hook 'post-command-hook 'elk-test-result-follow-hook t)))

(defun elk-test-result-follow-hook ()
  (let ((button (button-at (point))))
    (unless (equal button elk-test-result-follow-last-button)
      (setq elk-test-result-follow-last-button button)
      (when button
        (let ((next-error-highlight next-error-highlight-no-select))
          (save-selected-window
            (save-excursion
              (elk-test-button-action button))))))))

(provide 'elk-test)
;;; elk-test.el ends here

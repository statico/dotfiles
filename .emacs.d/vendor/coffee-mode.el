;;; coffee-mode.el --- Major mode to edit CoffeeScript files in Emacs

;; Copyright (C) 2010 Chris Wanstrath

;; Version 0.3.0
;; Keywords: CoffeeScript major mode
;; Author: Chris Wanstrath <chris@ozmm.org>
;; URL: http://github.com/defunkt/coffee-script

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; For commentary please see the README.md or
;; http://github.com/defunkt/coffee-mode#readme

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/vendor
;;     $ git clone git://github.com/defunkt/coffee-mode.git

;; In your emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
;;     (require 'coffee-mode)

;;; Thanks

;; Major thanks to http://xahlee.org/emacs/elisp_syntax_coloring.html
;; the instructions.

;; Also thanks to Jason Blevins's markdown-mode.el and Steve Yegge's
;; js2-mode for guidance.

;; TODO:
;; - Execute {buffer,region,line} and show output in new buffer
;; - Make prototype accessor assignments like `String::length: -> 10` pretty.
;; - mirror-mode - close brackets and parens automatically

;;; Code:

(require 'comint)
(require 'easymenu)
(require 'font-lock)

(eval-when-compile
  (require 'cl))

;;
;; Customizable Variables
;;

(defconst coffee-mode-version "0.3.0"
  "The version of this `coffee-mode'.")

(defgroup coffee nil
  "A CoffeeScript major mode."
  :group 'languages)

(defcustom coffee-debug-mode nil
  "Whether to run in debug mode or not. Logs to `*Messages*'."
  :type 'boolean
  :group 'coffee-mode)

(defcustom coffee-js-mode 'js2-mode
  "The mode to use when viewing compiled JavaScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-cleanup-whitespace t
  "Should we `delete-trailing-whitespace' on save? Probably."
  :type 'boolean
  :group 'coffee)

(defcustom coffee-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'coffee)

(defcustom coffee-command "coffee"
  "The CoffeeScript command used for evaluating code. Must be in your
path."
  :type 'string
  :group 'coffee)

(defcustom coffee-args-repl '("-i")
  "The command line arguments to pass to `coffee-command' to start a REPL."
  :type 'list
  :group 'coffee)

(defcustom coffee-args-compile '("-c")
  "The command line arguments to pass to `coffee-command' when compiling a file."
  :type 'list
  :group 'coffee)

(defcustom coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used when compiling CoffeeScript."
  :type 'string
  :group 'coffee)

(defvar coffee-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar coffee-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

;;
;; Macros
;;

(defmacro setd (var val)
  "Like setq but optionally logs the variable's value using `coffee-debug'."
  (if (and (boundp 'coffee-debug-mode) coffee-debug-mode)
      `(progn
         (coffee-debug "%s: %s" ',var ,val)
         (setq ,var ,val))
    `(setq ,var ,val)))

(defun coffee-debug (string &rest args)
  "Print a message when in debug mode."
  (when coffee-debug-mode
      (apply 'message (append (list string) args))))

(defmacro coffee-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

;;
;; Commands
;;

(defun coffee-repl ()
  "Launch a CoffeeScript REPL using `coffee-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc "*CoffeeREPL*")
    (set-buffer
     (apply 'make-comint "CoffeeREPL"
            coffee-command nil coffee-args-repl)))

  (pop-to-buffer "*CoffeeREPL*"))

(defun coffee-compile-file ()
  "Compiles and saves the current file to disk. Doesn't open in a buffer.."
  (interactive)
  (let ((compiler-output (shell-command-to-string (coffee-command-compile (buffer-file-name)))))
    (if (string= compiler-output "")
        (message "Compiled and saved %s" (concat (substring (buffer-file-name) 0 -6) "js"))
      (message (car (split-string compiler-output "[\n\r]+"))))))

(defun coffee-compile-buffer ()
  "Compiles the current buffer and displays the JS in another buffer."
  (interactive)
  (save-excursion
    (coffee-compile-region (point-min) (point-max))))

(defun coffee-compile-region (start end)
  "Compiles a region and displays the JS in another buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end coffee-command nil
                       (get-buffer-create coffee-compiled-buffer-name)
                       nil
                       "-s" "-p" "--bare")
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (funcall coffee-js-mode)
  (goto-char (point-min)))

(defun coffee-show-version ()
  "Prints the `coffee-mode' version."
  (interactive)
  (message (concat "coffee-mode v" coffee-mode-version)))

(defun coffee-open-reference ()
  "Open browser to CoffeeScript reference."
  (interactive)
  (browse-url "http://jashkenas.github.com/coffee-script/"))

(defun coffee-open-node-reference ()
  "Open browser to node.js documentation."
  (interactive)
  (browse-url "http://nodejs.org/docs/"))

(defun coffee-open-github ()
  "Open browser to `coffee-mode' project on GithHub."
  (interactive)
  (browse-url "http://github.com/defunkt/coffee-mode"))

;;
;; Menubar
;;

(easy-menu-define coffee-mode-menu coffee-mode-map
  "Menu for CoffeeScript mode"
  '("CoffeeScript"
    ["Compile File" coffee-compile-file]
    ["Compile Buffer" coffee-compile-buffer]
    ["Compile Region" coffee-compile-region]
    ["REPL" coffee-repl]
    "---"
    ["CoffeeScript Reference" coffee-open-reference]
    ["node.js Reference" coffee-open-node-reference]
    ["coffee-mode on GitHub" coffee-open-github]
    ["Version" coffee-show-version]
    ))

;;
;; Define Language Syntax
;;

;; String literals
(defvar coffee-string-regexp "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

;; Instance variables (implicit this)
(defvar coffee-this-regexp "@\\(\\w\\|_\\)*\\|this")

;; Prototype::access
(defvar coffee-prototype-regexp "\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)::\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\):")

;; Assignment
(defvar coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\):")

;; Lambda
(defvar coffee-lambda-regexp "\\((.+)\\)?\\s *\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(class\\s +\\(\\S +\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\)\\b")

;; Regular Expressions
(defvar coffee-regexp-regexp "\\/\\(\\\\.\\|\\[\\(\\\\.\\|.\\)+?\\]\\|[^/]\\)+?\\/")

;; JavaScript Keywords
(defvar coffee-js-keywords
      '("if" "else" "new" "return" "try" "catch"
        "finally" "throw" "break" "continue" "for" "in" "while"
        "delete" "instanceof" "typeof" "switch" "super" "extends"
        "class" "until" "loop"))

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
      '("case" "default" "do" "function" "var" "void" "with"
        "const" "let" "debugger" "enum" "export" "import" "native"
        "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
      '("then" "unless" "and" "or" "is"
        "isnt" "not" "of" "by" "where" "when"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))


;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar coffee-font-lock-keywords
  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
  ;; because otherwise the keyword "state" in the function
  ;; "state_entry" would be highlighted.
  `((,coffee-string-regexp . font-lock-string-face)
    (,coffee-this-regexp . font-lock-variable-name-face)
    (,coffee-prototype-regexp . font-lock-variable-name-face)
    (,coffee-assign-regexp . font-lock-type-face)
    (,coffee-regexp-regexp . font-lock-constant-face)
    (,coffee-boolean-regexp . font-lock-constant-face)
    (,coffee-keywords-regexp . font-lock-keyword-face)))

;;
;; Helper Functions
;;

(defun coffee-before-save ()
  "Hook run before file is saved. Deletes whitespace if
`coffee-cleanup-whitespace' is non-nil."
  (when coffee-cleanup-whitespace
    (delete-trailing-whitespace)))

(defun coffee-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defun coffee-command-compile (file-name)
  "The `coffee-command' with args to compile a file."
  (mapconcat 'identity (append (list coffee-command) coffee-args-compile (list file-name)) " "))

;;
;; imenu support
;;

;; This is a pretty naive but workable way of doing it. First we look
;; for any lines that starting with `coffee-assign-regexp' that include
;; `coffee-lambda-regexp' then add those tokens to the list.
;;
;; Should cover cases like these:
;;
;; minus: (x, y) -> x - y
;; String::length: -> 10
;; block: ->
;;   print('potion')
;;
;; Next we look for any line that starts with `class' or
;; `coffee-assign-regexp' followed by `{` and drop into a
;; namespace. This means we search one indentation level deeper for
;; more assignments and add them to the alist prefixed with the
;; namespace name.
;;
;; Should cover cases like these:
;;
;; class Person
;;   print: ->
;;     print 'My name is ' + this.name + '.'
;;
;; class Policeman extends Person
;;   constructor: (rank) ->
;;     @rank: rank
;;   print: ->
;;     print 'My name is ' + this.name + " and I'm a " + this.rank + '.'
;;
;; TODO:
;; app = {
;;   window:  {width: 200, height: 200}
;;   para:    -> 'Welcome.'
;;   button:  -> 'OK'
;; }

(defun coffee-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (interactive)

  ;; This function is called within a `save-excursion' so we're safe.
  (goto-char (point-min))

  (let ((index-alist '()) assign pos indent ns-name ns-indent)
    ;; Go through every assignment that includes -> or => on the same
    ;; line or starts with `class'.
    (while (re-search-forward
            (concat "^\\(\\s *\\)"
                    "\\("
                      coffee-assign-regexp
                      ".+?"
                      coffee-lambda-regexp
                    "\\|"
                      coffee-namespace-regexp
                    "\\)")
            (point-max)
            t)

      (coffee-debug "Match: %s" (match-string 0))

      ;; If this is the start of a new namespace, save the namespace's
      ;; indentation level and name.
      (when (match-string 8)
        ;; Set the name.
        (setq ns-name (match-string 8))

        ;; If this is a class declaration, add :: to the namespace.
        (setq ns-name (concat ns-name "::"))

        ;; Save the indentation level.
        (setq ns-indent (length (match-string 1)))

        ;; Debug
        (coffee-debug "ns: Found %s with indent %s" ns-name ns-indent))

      ;; If this is an assignment, save the token being
      ;; assigned. `Please.print:` will be `Please.print`, `block:`
      ;; will be `block`, etc.
      (when (setq assign (match-string 3))
          ;; The position of the match in the buffer.
          (setq pos (match-beginning 3))

          ;; The indent level of this match
          (setq indent (length (match-string 1)))

          ;; If we're within the context of a namespace, add that to the
          ;; front of the assign, e.g.
          ;; constructor: => Policeman::constructor
          (when (and ns-name (> indent ns-indent))
            (setq assign (concat ns-name assign)))

          (coffee-debug "=: Found %s with indent %s" assign indent)

          ;; Clear the namespace if we're no longer indented deeper
          ;; than it.
          (when (and ns-name (<= indent ns-indent))
            (coffee-debug "ns: Clearing %s" ns-name)
            (setq ns-name nil)
            (setq ns-indent nil))

          ;; Add this to the alist. Done.
          (push (cons assign pos) index-alist)))

    ;; Return the alist.
    index-alist))

;;
;; Indentation
;;

;;; The theory is explained in the README.

(defun coffee-indent-line ()
  "Indent current line as CoffeeScript."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent 0) (cur-indent 0))
        ;; Figure out the indentation of the previous line
        (setd prev-indent (coffee-previous-indent))

        ;; Figure out the current line's indentation
        (setd cur-indent (current-indentation))

        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (coffee-debug "point: %s" (point))
        (coffee-debug "point-at-bol: %s" (point-at-bol))

        (when (= (point-at-bol) (point))
          (forward-char coffee-tab-width))

        (coffee-debug "New indent: %s" (current-indentation))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) coffee-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun coffee-previous-indent ()
  "Return the indentation level of the previous non-blank line."

  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (coffee-line-empty-p) (not (bobp))) (forward-line -1))
        (current-indentation)))))

(defun coffee-line-empty-p ()
  "Is this line empty? Returns non-nil if so, nil if not."
  (or (bobp)
   (string-match "^\\s *$" (coffee-line-as-string))))

(defun coffee-newline-and-indent ()
  "Inserts a newline and indents it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)) (indent-next nil))
    (newline)
    (insert-tab (/ prev-indent coffee-tab-width))

    ;; We need to insert an additional tab because the last line was special.
    (when (coffee-line-wants-indent)
      (insert-tab)))

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the one I'm
  ;; writing right now).
  (when (coffee-previous-line-is-comment)
    (insert "# ")))

;; Indenters help determine whether the current line should be
;; indented further based on the content of the previous line. If a
;; line starts with `class', for instance, you're probably going to
;; want to indent the next line.

(defvar coffee-indenters-bol '("class" "for" "if" "try")
  "Keywords or syntax whose presence at the start of a line means the
next line should probably be indented.")

(defun coffee-indenters-bol-regexp ()
  "Builds a regexp out of `coffee-indenters-bol' words."
  (concat "^" (regexp-opt coffee-indenters-bol 'words)))

(defvar coffee-indenters-eol '(?> ?{ ?\[)
  "Single characters at the end of a line that mean the next line
should probably be indented.")

(defun coffee-line-wants-indent ()
  "Does the current line want to be indented deeper than the previous
line? Returns `t' or `nil'. See the README for more details."
  (interactive)

  (save-excursion
    (let ((indenter-at-bol) (indenter-at-eol))
      ;; Go back a line and to the first character.
      (forward-line -1)
      (backward-to-indentation 0)

      ;; If the next few characters match one of our magic indenter
      ;; keywords, we want to indent the line we were on originally.
      (when (looking-at (coffee-indenters-bol-regexp))
        (setd indenter-at-bol t))

      ;; If that didn't match, go to the back of the line and check to
      ;; see if the last character matches one of our indenter
      ;; characters.
      (when (not indenter-at-bol)
        (end-of-line)

        ;; Optimized for speed - checks only the last character.
        (when (some (lambda (char)
                        (= (char-before) char))
                      coffee-indenters-eol)
          (setd indenter-at-eol t)))

      ;; If we found an indenter, return `t'.
      (or indenter-at-bol indenter-at-eol))))

(defun coffee-previous-line-is-comment ()
  "Returns `t' if the previous line is a CoffeeScript comment."
  (save-excursion
    (forward-line -1)
    (coffee-line-is-comment)))

(defun coffee-line-is-comment ()
  "Returns `t' if the current line is a CoffeeScript comment."
  (save-excursion
    (backward-to-indentation 0)
    (= (char-after) (string-to-char "#"))))

;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode coffee-mode fundamental-mode
  "coffee-mode"
  "Major mode for editing CoffeeScript..."

  ;; key bindings
  (define-key coffee-mode-map (kbd "A-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "A-R") 'coffee-compile-region)
  (define-key coffee-mode-map (kbd "A-M-r") 'coffee-repl)
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'coffee-indent-line)

  ;; imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'coffee-imenu-create-index)

  ;; no tabs
  (setq indent-tabs-mode nil)

  ;; hooks
  (set (make-local-variable 'before-save-hook) 'coffee-before-save))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

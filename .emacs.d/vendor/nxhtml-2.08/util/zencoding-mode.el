;;; zencoding-mode.el --- Unfold CSS-selector-like expressions to markup
;;
;; Copyright (C) 2009, Chris Done
;;
;; Author: Chris Done <chrisdone@gmail.com>
(defconst zencoding-mode:version "0.5")
;; Last-Updated: 2009-11-20 Fri
;; Keywords: convenience
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Unfold CSS-selector-like expressions to markup. Intended to be used
;; with sgml-like languages; xml, html, xhtml, xsl, etc.
;;
;; See `zencoding-mode' for more information.
;;
;; Copy zencoding-mode.el to your load-path and add to your .emacs:
;;
;;    (require 'zencoding-mode)
;;
;; Example setup:
;;
;;    (add-to-list 'load-path "~/Emacs/zencoding/")
;;    (require 'zencoding-mode)
;;    (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
;;
;; Enable the minor mode with M-x zencoding-mode.
;;
;; See ``Test cases'' section for a complete set of expression types.
;;
;; If you are hacking on this project, eval (zencoding-test-cases) to
;; ensure that your changes have not broken anything. Feel free to add
;; new test cases if you add new features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; Modified by Lennart Borgman.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(eval-when-compile (require 'cl))

(defcustom zencoding-preview-default t
  "If non-nil then preview is the default action.
This determines how `zencoding-expand-line' works by default."
  :type 'boolean
  :group 'zencoding)

(defcustom zencoding-insert-flash-time 0.5
  "Time to flash insertion.
Set this to a negative number if you do not want flashing the
expansion after insertion."
  :type '(number :tag "Seconds")
  :group 'zencoding)

(defmacro zencoding-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-parse (regex nums label &rest body)
  "Parse according to a regex and update the `input' variable."
  `(zencoding-aif (zencoding-regex ,regex input ',(number-sequence 0 nums))
                  (let ((input (elt it ,nums)))
                    ,@body)
                  `,`(error ,(concat "expected " ,label))))

(defmacro zencoding-run (parser then-form &rest else-forms)
  "Run a parser and update the input properly, extract the parsed
   expression."
  `(zencoding-pif (,parser input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  ,@(or else-forms '(it))))

(defmacro zencoding-por (parser1 parser2 then-form &rest else-forms)
  "OR two parsers. Try one parser, if it fails try the next."
  `(zencoding-pif (,parser1 input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  (zencoding-pif (,parser2 input)
                                 (let ((input (cdr it))
                                       (expr (car it)))
                                   ,then-form)
                                 ,@else-forms)))

(defun zencoding-regex (regexp string refs)
  "Return a list of (`ref') matches for a `regex' on a `string' or nil."
  (if (string-match (concat "^" regexp "\\([^\n]*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string))
              (if (sequencep refs) refs (list refs)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding parsers

(defun zencoding-expr (input)
  "Parse a zen coding expression. This pretty much defines precedence."
  (zencoding-run zencoding-siblings
                 it
                 (zencoding-run zencoding-parent-child
                                it
                                (zencoding-run zencoding-multiplier
                                               it
                                               (zencoding-run zencoding-pexpr
                                                              it
                                                              (zencoding-run zencoding-tag
                                                                             it
                                                                             '(error "no match, expecting ( or a-zA-Z0-9")))))))

(defun zencoding-multiplier (input)
  (zencoding-por zencoding-pexpr zencoding-tag
                 (let ((multiplier expr))
                   (zencoding-parse "\\*\\([0-9]+\\)" 2 "*n where n is a number"
                                    (let ((multiplicand (read (elt it 1))))
                                      `((list ,(make-list multiplicand multiplier)) . ,input))))
                 '(error "expected *n multiplier")))

(defun zencoding-tag (input)
  "Parse a tag."
  (zencoding-run zencoding-tagname
                 (let ((result it)
                       (tagname (cdr expr)))
                   (zencoding-pif (zencoding-run zencoding-identifier
                                                 (zencoding-tag-classes
                                                  `(tag ,tagname ((id ,(cddr expr)))) input)
                                                 (zencoding-tag-classes `(tag ,tagname ()) input))
                                  (let ((expr-and-input it) (expr (car it)) (input (cdr it)))
                                    (zencoding-pif (zencoding-tag-props expr input)
                                                   it
                                                   expr-and-input))))
                 '(error "expected tagname")))

(defun zencoding-tag-props (tag input)
  (zencoding-run zencoding-props
                 (let ((tagname (cadr tag))
                       (existing-props (caddr tag))
                       (props (cdr expr)))
                   `((tag ,tagname
                          ,(append existing-props props))
                     . ,input))))

(defun zencoding-props (input)
  "Parse many props."
    (zencoding-run zencoding-prop
                   (zencoding-pif (zencoding-props input)
                                  `((props . ,(cons expr (cdar it))) . ,(cdr it))
                                  `((props . ,(list expr)) . ,input))))

(defun zencoding-prop (input)
  (zencoding-parse
   " " 1 "space"
   (zencoding-run
    zencoding-name
    (let ((name (cdr expr)))
      (zencoding-parse "=\\([^\\,\\+\\>\\ )]*\\)" 2
                       "=property value"
                       (let ((value (elt it 1))
                             (input (elt it 2)))
                         `((,(read name) ,value) . ,input)))))))

(defun zencoding-tag-classes (tag input)
  (zencoding-run zencoding-classes
                 (let ((tagname (cadr tag))
                       (props (caddr tag))
                       (classes `(class ,(mapconcat
                                          (lambda (prop)
                                            (cdadr prop))
                                          (cdr expr)
                                          " "))))
                   `((tag ,tagname ,(append props (list classes))) . ,input))
                 `(,tag . ,input)))

(defun zencoding-tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (zencoding-parse "\\([a-zA-Z][a-zA-Z0-9:-]*\\)" 2 "tagname, a-zA-Z0-9"
                   `((tagname . ,(elt it 1)) . ,input)))

(defun zencoding-pexpr (input)
  "A zen coding expression with parentheses around it."
  (zencoding-parse "(" 1 "("
                   (zencoding-run zencoding-expr
                                  (zencoding-aif (zencoding-regex ")" input '(0 1))
                                                 `(,expr . ,(elt it 1))
                                                 '(error "expecting `)'")))))

(defun zencoding-parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any
   expression."
  (zencoding-run zencoding-multiplier
                 (let* ((items (cadr expr))
                        (rest (zencoding-child-sans expr input)))
                   (if (not (eq (car rest) 'error))
                       (let ((child (car rest))
                             (input (cdr rest)))
                         (cons (cons 'list
                                     (cons (mapcar (lambda (parent)
                                                     `(parent-child ,parent ,child))
                                                   items)
                                           nil))
                               input))
                     '(error "expected child")))
                 (zencoding-run zencoding-tag
                                (zencoding-child expr input)
                                '(error "expected parent"))))

(defun zencoding-child-sans (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-expr
                                  it
                                  '(error "expected child"))))

(defun zencoding-child (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-expr
                                  (let ((child expr))
                                    `((parent-child ,parent ,child) . ,input))
                                  '(error "expected child"))))

(defun zencoding-sibling (input)
  (zencoding-por zencoding-pexpr zencoding-multiplier
                 it
                 (zencoding-run zencoding-tag
                                it
                                '(error "expected sibling"))))

(defun zencoding-siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (zencoding-run zencoding-sibling
                 (let ((parent expr))
                   (zencoding-parse "\\+" 1 "+"
                                    (zencoding-run zencoding-expr
                                                   (let ((child expr))
                                                     `((zencoding-siblings ,parent ,child) . ,input))
                                                   '(error "expected second sibling"))))
                 '(error "expected first sibling")))

(defun zencoding-name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (zencoding-parse "\\([a-zA-Z][a-zA-Z0-9-_]*\\)" 2 "class or identifer name"
                   `((name . ,(elt it 1)) . ,input)))

(defun zencoding-class (input)
  "Parse a classname expression, e.g. .foo"
  (zencoding-parse "\\." 1 "."
                   (zencoding-run zencoding-name
                                  `((class ,expr) . ,input)
                                  '(error "expected class name"))))

(defun zencoding-identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (zencoding-parse "#" 1 "#"
                   (zencoding-run zencoding-name
                                  `((identifier . ,expr) . ,input))))

(defun zencoding-classes (input)
  "Parse many classes."
  (zencoding-run zencoding-class
                 (zencoding-pif (zencoding-classes input)
                                `((classes . ,(cons expr (cdar it))) . ,(cdr it))
                                `((classes . ,(list expr)) . ,input))
                 '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to HTML

;; Fix-me: make mode specific
(defvar zencoding-single-tags
  '("br"
    "img"))

(defvar zencoding-inline-tags
  '("a"
    "abbr"
    "acronym"
    "cite"
    "code"
    "dfn"
    "em"
    "h1" "h2" "h3" "h4" "h5" "h6"
    "kbd"
    "q"
    "span"
    "strong"
    "var"))

(defvar zencoding-block-tags
  '("p"))

;; li
;; a
;; em
;; p

(defvar zencoding-leaf-function nil
  "Function to execute when expanding a leaf node in the
  Zencoding AST.")

(defun zencoding-make-tag (tag &optional content)
  (let* ((name (car tag))
         (lf (if
                 (or
                  (member name zencoding-block-tags)
                  (and
                   (> (length name) 1)
                   (not (member name zencoding-inline-tags))
                   ))
                 "\n" ""))
         (single (member name zencoding-single-tags))
        (props (apply 'concat (mapcar
                               (lambda (prop)
                                 (concat " " (symbol-name (car prop))
                                         "=\"" (cadr prop) "\""))
                               (cadr tag)))))
    (concat lf "<" name props ">" lf
            (if single
                ""
              (concat
               (if content content
                 (if zencoding-leaf-function
                     (funcall zencoding-leaf-function)
                   ""))
               lf "</" name ">")))))

(defun zencoding-transform (ast)
  (let ((type (car ast)))
    (cond
     ((eq type 'list)
      (mapconcat 'zencoding-transform (cadr ast) ""))
     ((eq type 'tag)
      (zencoding-make-tag (cdr ast)))
     ((eq type 'parent-child)
      (let ((parent (cdadr ast))
            (children (zencoding-transform (caddr ast))))
        (zencoding-make-tag parent children)))
     ((eq type 'zencoding-siblings)
      (let ((sib1 (zencoding-transform (cadr ast)))
            (sib2 (zencoding-transform (caddr ast))))
        (concat sib1 sib2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test-cases

(defun zencoding-test-cases ()
  (let ((tests '(;; Tags
                 ("a"                      "<a></a>")
                 ("a.x"                    "<a class=\"x\"></a>")
                 ("a#q.x"                  "<a id=\"q\" class=\"x\"></a>")
                 ("a#q.x.y.z"              "<a id=\"q\" class=\"x y z\"></a>")
                 ;; Siblings
                 ("a+b"                    "<a></a><b></b>")
                 ("a+b+c"                  "<a></a><b></b><c></c>")
                 ("a.x+b"                  "<a class=\"x\"></a><b></b>")
                 ("a#q.x+b"                "<a id=\"q\" class=\"x\"></a><b></b>")
                 ("a#q.x.y.z+b"            "<a id=\"q\" class=\"x y z\"></a><b></b>")
                 ("a#q.x.y.z+b#p.l.m.n"    "<a id=\"q\" class=\"x y z\"></a><b id=\"p\" class=\"l m n\"></b>")
                 ;; Parent > child
                 ("a>b"                    "<a><b></b></a>")
                 ("a>b>c"                  "<a><b><c></c></b></a>")
                 ("a.x>b"                  "<a class=\"x\"><b></b></a>")
                 ("a#q.x>b"                "<a id=\"q\" class=\"x\"><b></b></a>")
                 ("a#q.x.y.z>b"            "<a id=\"q\" class=\"x y z\"><b></b></a>")
                 ("a#q.x.y.z>b#p.l.m.n"    "<a id=\"q\" class=\"x y z\"><b id=\"p\" class=\"l m n\"></b></a>")
                 ("a>b+c"                  "<a><b></b><c></c></a>")
                 ("a>b+c>d"                "<a><b></b><c><d></d></c></a>")
                 ;; Multiplication
                 ("a*1"                    "<a></a>")
                 ("a*2"                    "<a></a><a></a>")
                 ("a*2+b*2"                "<a></a><a></a><b></b><b></b>")
                 ("a*2>b*2"                "<a><b></b><b></b></a><a><b></b><b></b></a>")
                 ("a>b*2"                  "<a><b></b><b></b></a>")
                 ("a#q.x>b#q.x*2"          "<a id=\"q\" class=\"x\"><b id=\"q\" class=\"x\"></b><b id=\"q\" class=\"x\"></b></a>")
                 ;; Properties
                 ("a x=y"                  "<a x=\"y\"></a>")
                 ("a x=y m=l"              "<a x=\"y\" m=\"l\"></a>")
                 ("a#foo x=y m=l"          "<a id=\"foo\" x=\"y\" m=\"l\"></a>")
                 ("a.foo x=y m=l"          "<a class=\"foo\" x=\"y\" m=\"l\"></a>")
                 ("a#foo.bar.mu x=y m=l"   "<a id=\"foo\" class=\"bar mu\" x=\"y\" m=\"l\"></a>")
                 ("a x=y+b"                "<a x=\"y\"></a><b></b>")
                 ("a x=y+b x=y"            "<a x=\"y\"></a><b x=\"y\"></b>")
                 ("a x=y>b"                "<a x=\"y\"><b></b></a>")
                 ("a x=y>b x=y"            "<a x=\"y\"><b x=\"y\"></b></a>")
                 ("a x=y>b x=y+c x=y"      "<a x=\"y\"><b x=\"y\"></b><c x=\"y\"></c></a>")
                 ;; Parentheses
                 ("(a)"                    "<a></a>")
                 ("(a)+(b)"                "<a></a><b></b>")
                 ("a>(b)"                  "<a><b></b></a>")
                 ("(a>b)>c"                "<a><b></b></a>")
                 ("(a>b)+c"                "<a><b></b></a><c></c>")
                 ("z+(a>b)+c+k"            "<z></z><a><b></b></a><c></c><k></k>")
                 ("(a)*2"                  "<a></a><a></a>")
                 ("((a)*2)"                "<a></a><a></a>")
                 ("((a)*2)"                "<a></a><a></a>")
                 ("(a>b)*2"                "<a><b></b></a><a><b></b></a>")
                 ("(a+b)*2"                "<a></a><b></b><a></a><b></b>")
                 )))
    (mapc (lambda (input)
            (let ((expected (cadr input))
                  (actual (zencoding-transform (car (zencoding-expr (car input))))))
              (if (not (equal expected actual))
                  (error (concat "Assertion " (car input) " failed:"
                                 expected
                                 " == "
                                 actual)))))
            tests)
    (concat (number-to-string (length tests)) " tests performed. All OK.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding minor mode

;;;###autoload
(defgroup zencoding nil
  "Customization group for zencoding-mode."
  :group 'convenience)

(defun zencoding-expr-on-line ()
  "Extract a zencoding expression and the corresponding bounds
   for the current line."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (expr (zencoding-regex "\\([ \t]*\\)\\([^\n]+\\)" line 2)))
    (if (first expr)
        (list (first expr) start end))))

(defun zencoding-prettify (markup indent)
  (save-match-data
    ;;(setq markup (replace-regexp-in-string "><" ">\n<" markup))
    (setq markup (replace-regexp-in-string "\n\n" "\n" markup))
    (setq markup (replace-regexp-in-string "^\n" "" markup)))
  (with-temp-buffer
    (indent-to indent)
    (insert "<i></i>")
    (insert "\n")
    (let ((here (point)))
      (insert markup)
      (sgml-mode)
      (indent-region here (point-max))
      (buffer-substring-no-properties here (point-max)))))

;;;###autoload
(defun zencoding-expand-line (arg)
  "Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'."
  (interactive "P")
  (let* ((here (point))
         (preview (if zencoding-preview-default (not arg) arg))
         (beg (if preview
                  (progn
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (point))
                (when mark-active (region-beginning))))
         (end (if preview
                  (progn
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (point))
                (when mark-active (region-end)))))
    (if beg
        (progn
          (goto-char here)
          (zencoding-preview beg end))
      (let ((expr (zencoding-expr-on-line)))
        (if expr
            (let* ((markup (zencoding-transform (car (zencoding-expr (first expr)))))
                   (pretty (zencoding-prettify markup (current-indentation))))
              (save-excursion
                (delete-region (second expr) (third expr))
                (zencoding-insert-and-flash pretty))))))))

(defvar zencoding-mode-keymap nil
  "Keymap for zencode minor mode.")

(if zencoding-mode-keymap
    nil
  (progn
    (setq zencoding-mode-keymap (make-sparse-keymap))
    (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)))

;;;###autoload
(define-minor-mode zencoding-mode
  "Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'."
  :lighter " Zen"
  :keymap zencoding-mode-keymap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding yasnippet integration

(defun zencoding-transform-yas (ast)
  (let* ((leaf-count 0)
         (zencoding-leaf-function
          (lambda ()
            (format "$%d" (incf leaf-count)))))
    (zencoding-transform ast)))

;;;###autoload
(defun zencoding-expand-yas ()
  (interactive)
  (let ((expr (zencoding-expr-on-line)))
    (if expr
        (let* ((markup (zencoding-transform-yas (car (zencoding-expr (first expr)))))
               (filled (replace-regexp-in-string "><" ">\n<" markup)))
          (delete-region (second expr) (third expr))
          (insert filled)
          (indent-region (second expr) (point))
          (yas/expand-snippet
           (buffer-substring (second expr) (point))
           (second expr) (point))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real-time preview
;;

;;;;;;;;;;
;; Lennart's version

(defvar zencoding-preview-input nil)
(make-local-variable 'zencoding-preview-input)
(defvar zencoding-preview-output nil)
(make-local-variable 'zencoding-preview-output)
(defvar zencoding-old-show-paren nil)
(make-local-variable 'zencoding-old-show-paren)

(defface zencoding-preview-input
  '((default :box t :inherit secondary-selection))
  "Face for preview input field."
  :group 'zencoding)

(defface zencoding-preview-output
  '((default :inherit highlight))
  "Face for preview output field."
  :group 'zencoding)

(defvar zencoding-preview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'zencoding-preview-accept)
    (define-key map [(control ?g)] 'zencoding-preview-abort)
    map))

(defun zencoding-preview-accept ()
  (interactive)
  (let ((ovli zencoding-preview-input))
    (if (not (and (overlayp ovli)
                  (bufferp (overlay-buffer ovli))))
        (message "Preview is not active")
      (let* ((indent (current-indentation))
             (markup (zencoding-preview-transformed indent)))
        (when markup
          (delete-region (line-beginning-position) (overlay-end ovli))
          (zencoding-insert-and-flash markup)))))
  (zencoding-preview-abort))

(defvar zencoding-flash-ovl nil)
(make-variable-buffer-local 'zencoding-flash-ovl)

(defun zencoding-remove-flash-ovl (buf)
  (with-current-buffer buf
    (when (overlayp zencoding-flash-ovl)
      (delete-overlay zencoding-flash-ovl))
    (setq zencoding-flash-ovl nil)))

(defun zencoding-insert-and-flash (markup)
  (zencoding-remove-flash-ovl (current-buffer))
  (let ((here (point)))
    (insert markup)
    (setq zencoding-flash-ovl (make-overlay here (point)))
    (overlay-put zencoding-flash-ovl 'face 'zencoding-preview-output)
    (when (< 0 zencoding-insert-flash-time)
      (run-with-idle-timer zencoding-insert-flash-time
                           nil 'zencoding-remove-flash-ovl (current-buffer)))))

;;;###autoload
(defun zencoding-preview (beg end)
  "Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (zencoding-preview-abort)
  (if (not beg)
      (message "Region not active")
    (setq zencoding-old-show-paren show-paren-mode)
    (show-paren-mode -1)
    (let ((here (point)))
      (goto-char beg)
      (forward-line 1)
      (unless (= 0 (current-column))
        (insert "\n"))
      (let* ((opos (point))
             (ovli (make-overlay beg end nil nil t))
             (ovlo (make-overlay opos opos))
             (info (propertize " Zen preview. Choose with RET. Cancel by stepping out. \n"
                               'face 'tooltip)))
        (overlay-put ovli 'face 'zencoding-preview-input)
        (overlay-put ovli 'keymap zencoding-preview-keymap)
        (overlay-put ovlo 'face 'zencoding-preview-output)
        (overlay-put ovlo 'before-string info)
        (setq zencoding-preview-input  ovli)
        (setq zencoding-preview-output ovlo)
        (add-hook 'before-change-functions 'zencoding-preview-before-change t t)
        (goto-char here)
        (add-hook 'post-command-hook 'zencoding-preview-post-command t t)))))

(defvar zencoding-preview-pending-abort nil)
(make-variable-buffer-local 'zencoding-preview-pending-abort)

(defun zencoding-preview-before-change (beg end)
  (when
      (or (> beg (overlay-end zencoding-preview-input))
          (< beg (overlay-start zencoding-preview-input))
          (> end (overlay-end zencoding-preview-input))
          (< end (overlay-start zencoding-preview-input)))
    (setq zencoding-preview-pending-abort t)))

(defun zencoding-preview-abort ()
  "Abort zen code preview."
  (interactive)
  (setq zencoding-preview-pending-abort nil)
  (remove-hook 'before-change-functions 'zencoding-preview-before-change t)
  (when (overlayp zencoding-preview-input)
    (delete-overlay zencoding-preview-input))
  (setq zencoding-preview-input nil)
  (when (overlayp zencoding-preview-output)
    (delete-overlay zencoding-preview-output))
  (setq zencoding-preview-output nil)
  (remove-hook 'post-command-hook 'zencoding-preview-post-command t)
  (when zencoding-old-show-paren (show-paren-mode 1)))

(defun zencoding-preview-post-command ()
  (condition-case err
      (zencoding-preview-post-command-1)
    (error (message "zencoding-preview-post: %s" err))))

(defun zencoding-preview-post-command-1 ()
  (if (and (not zencoding-preview-pending-abort)
           (<= (point) (overlay-end zencoding-preview-input))
           (>= (point) (overlay-start zencoding-preview-input)))
      (zencoding-update-preview (current-indentation))
    (zencoding-preview-abort)))

(defun zencoding-preview-transformed (indent)
  (let* ((string (buffer-substring-no-properties
		  (overlay-start zencoding-preview-input)
		  (overlay-end zencoding-preview-input)))
	 (ast    (car (zencoding-expr string))))
    (when (not (eq ast 'error))
      (zencoding-prettify (zencoding-transform ast)
                          indent))))

(defun zencoding-update-preview (indent)
  (let* ((pretty (zencoding-preview-transformed indent))
         (show (when pretty
                 (propertize pretty 'face 'highlight))))
    (when show
      (overlay-put zencoding-preview-output 'after-string
                   (concat show "\n")))))
;; a+bc

;;;;;;;;;;
;; Chris's version

;; (defvar zencoding-realtime-preview-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-c\C-c" 'zencoding-delete-overlay-pair)

;;     map)
;;   "Keymap used in zencoding realtime preview overlays.")

;; ;;;###autoload
;; (defun zencoding-realtime-preview-of-region (beg end)
;;   "Construct a real-time preview for the region BEG to END."
;;   (interactive "r")
;;   (let ((beg2)
;; 	(end2))
;;     (save-excursion
;;       (goto-char beg)
;;       (forward-line)
;;       (setq beg2 (point)
;; 	    end2 (point))
;;       (insert "\n"))
;;     (let ((input-and-output (zencoding-make-overlay-pair beg end beg2 end2)))
;;       (zencoding-handle-overlay-change (car input-and-output) nil nil nil)))
;;   )

;; (defun zencoding-make-overlay-pair (beg1 end1 beg2 end2)
;;   "Construct an input and an output overlay for BEG1 END1 and BEG2 END2"
;;   (let ((input  (make-overlay beg1 end1 nil t t))
;; 	(output (make-overlay beg2 end2)))
;;     ;; Setup input overlay
;;     (overlay-put input  'face '(:underline t))
;;     (overlay-put input  'modification-hooks
;; 		        (list #'zencoding-handle-overlay-change))
;;     (overlay-put input  'output output)
;;     (overlay-put input  'keymap zencoding-realtime-preview-keymap)
;;     ;; Setup output overlay
;;     (overlay-put output 'face '(:overline t))
;;     (overlay-put output 'intangible t)
;;     (overlay-put output 'input input)
;;     ;; Return the overlays.
;;     (list input output))
;;   )

;; (defun zencoding-delete-overlay-pair (&optional one)
;;   "Delete a pair of input and output overlays based on ONE."
;;   (interactive) ;; Since called from keymap
;;   (unless one
;;     (let ((overlays (overlays-at (point))))
;;       (while (and overlays
;; 		  (not (or (overlay-get (car overlays) 'input)
;; 			   (overlay-get (car overlays) 'output))))
;; 	(setq overlays (cdr overlays)))
;;       (setq one (car overlays))))
;;   (when one
;;     (let ((other (or (overlay-get one 'input)
;; 		     (overlay-get one 'output))))
;;       (delete-overlay one)
;;       (delete-overlay other)))
;;   )

;; (defun zencoding-handle-overlay-change (input del beg end &optional old)
;;   "Update preview after overlay change."
;;   (let* ((output (overlay-get input 'output))
;; 	 (start  (overlay-start output))
;; 	 (string (buffer-substring-no-properties
;; 		  (overlay-start input)
;; 		  (overlay-end input)))
;; 	 (ast    (car (zencoding-expr string)))
;; 	 (markup (when (not (eq ast 'error))
;; 		   (zencoding-transform ast))))
;;     (save-excursion
;;       (delete-region start (overlay-end output))
;;       (goto-char start)
;;       (if markup
;; 	  (insert markup)
;; 	(insert (propertize "error" 'face 'font-lock-error-face)))
;;       (move-overlay output start (point))))
;;   )

(provide 'zencoding-mode)

;;; zencoding-mode.el ends here

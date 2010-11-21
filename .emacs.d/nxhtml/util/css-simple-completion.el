;;; css-simple-completion.el --- Partly context aware css completion
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-22 Sun
;; Version:
;; Last-Updated: 2009-11-22 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Simple partly context aware completion. Context is based on
;; guessing mainly.
;;
;; This can be combined with with flymake-css.el that can check the
;; syntax.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
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

;; Fix-me: bad structure, does not fit completion frameworks
(defun css-simple-completing-w-pred (regexp matnum prompt collection)
  (let (pre start len)
    (when (looking-back regexp (line-beginning-position) t)
      (setq pre (downcase (match-string matnum)))
      (setq len (length pre))
      (setq start (match-beginning matnum))
      (unless (try-completion pre collection)
        (throw 'result nil))
      (throw 'result (list start
                           (completing-read prompt
                                            collection
                                            (lambda (alt)
                                              (and (>= (length alt) len)
                                                   (string= pre
                                                            (substring alt 0 len))))
                                            t
                                            pre))))))

(defun css-simple-complete ()
  "Try to complete at current point.
This tries to complete keywords, but no CSS values.

This is of course a pity since the value syntax is a bit
complicated. However you can at least check the syntax with
flymake-css if you want to."
  (interactive)
  (let ((context (css-simple-guess-context))
        result
        cur
        pre
        start)
    (setq result
          (catch 'result

            (case context

              ( 'css-media-ids
                (css-simple-completing-w-pred "\\<[a-z0-9-]*" 0 "Media type: " css-media-ids))

              ( 'css-at-ids
                (css-simple-completing-w-pred "@\\([a-z0-9-]*\\)" 1 "At rule: @" css-at-ids))

              ( 'css-property-ids
                (css-simple-completing-w-pred "\\<[a-z-]*" 0 "CSS property name: " css-property-ids))

              ( 'css-simple-selectors

                ;; Fix-me: Break out the first two
                (when (looking-back "\\W#\\([a-z0-9-]*\\)")
                  (setq cur (match-string 1))
                  (setq start (match-beginning 1))
                  (throw 'result (list (point)
                                       (read-string (concat "Html tag Id: " cur)))))
                (when (looking-back "\\W\\.\\([a-z0-9-]*\\)")
                  (setq cur (match-string 1))
                  (setq start (match-beginning 1))
                  (throw 'result (list (point)
                                       (read-string (concat "CSS class name: " cur)))))

                (css-simple-completing-w-pred "[a-z0-9]:\\([a-z0-9-]*\\)" 1 "Pseudo id: " css-pseudo-ids)

                (css-simple-completing-w-pred "[a-z0-9-]+" 0 "HTML tag: " (cddr css-simple-selectors))

                (when (looking-back "\\<\\(?:#\\|\\.\\)")
                  (setq pre nil)
                  (while t
                    (setq pre (completing-read "HTML tag, id or CSS class: " css-simple-selectors nil nil pre))
                    (if (string= (substring pre 0 1) "#")
                        (if (or (= 1 (length pre))
                                (and (> (length pre) 2)
                                     (string= (substring pre 0 3) "# (")))
                            (throw 'result (list (point) (concat "#" (read-string "Html tag id: #"))))
                          (throw 'result (list (point) pre)))
                      (if (string= (substring pre 0 1) ".")
                          (if (or (= 1 (length pre))
                                  (and (> (length pre) 2)
                                       (string= (substring pre 0 3) ". (")))
                              (throw 'result (list (point) (concat "." (read-string "CSS class name: ."))))
                            (throw 'result (list (point) pre)))
                        (when (member pre css-simple-selectors)
                          (throw 'result (list (point) pre)))))
                    ))))))
    (message "result=%S" result)
    (if result
        (let ((str (cadr result))
              (len (- (point) (car result))))
          (insert (substring str len)))
      (message "No matching alternatives"))))

(defun css-simple-guess-context ()
  "Try to find a context matching none constant.
Return the symbol corresponding to the context or nil if none
could be found.

The symbols are the names of the defconst holding the possibly
matching ids.

* Note: This function assumes that comments are fontified before
  point."
  ;; Kind of hand-written backward parser ... ;-)
  (let ((ignore-case t) ;; fix-me
        (here (point))
        (after-colon (and (not (bobp)) (eq (char-before) ?:)))
        ret)
    (prog1
        (catch 'return
          ;; No completion in comments.
          (when (eq (get-text-property (point) 'face)
                    'font-lock-comment-face)
            (throw 'return nil))

          ;; If we are not on whitespace then don't complete
          (css-simple-skip-backwards-to-code)
          (unless (or (eobp)
                      (= (char-syntax (char-after)) ?\ )
                      (< (point) here))
            (throw 'return nil))

          ;; Skip backwards to see if after first selector
          (let ((here2 (1+ (point))))
            (while (/= here2 (point))
              (setq here2 (point))
              (css-simple-skip-backwards-to-code)
              (when (and (not (bobp))
                         (eq (char-before) ?,))
                (backward-char))
              (skip-chars-backward "#.:a-z0-9-")))
          ;; Selector
          (when (or (bobp)
                    (eq (char-before) ?}))
            (throw 'return 'css-simple-selectors))

          ;; Property names
          (when (memq (char-before) '( ?{ ?\; ))
            (throw 'return 'css-property-ids))

          ;; If we are in the value we can't complete there yet.
          (when (eq (char-before) ?:)
            (throw 'return nil))


          ;; @
          (goto-char here)
          (skip-chars-backward "a-z0-9-")
          (when (eq (char-before) ?@)
            (throw 'return 'css-at-ids))

          ;; @media ids
          (when (looking-back "@media\\W+")
            (throw 'return 'css-media-ids))

          )
      (goto-char here))))
;;; Fix-me: complete these ...
;;css-descriptor-ids ;; Removed or?

(defun css-simple-skip-backwards-to-code ()
  "Skip backwards until we reach code.
Requires that comments are fontified."
  (let ((here (1+ (point))))
    (while (/= here (point))
      (setq here (point))
      (skip-syntax-backward " ")
      (unless (bobp)
        (when (memq (get-text-property (1- (point)) 'face)
                    '(font-lock-comment-face font-lock-comment-delimiter-face))
          (goto-char (or (previous-single-property-change (1- (point)) 'face)
                         (point-min))))))))

(defconst css-simple-selectors
  '(". (for class)"
    "# (for id)"
    ;; HTML 4.01 tags
    "a" "abbr" "acronym" "address" "applet" "area" "b" "base" "basefont" "bdo" "big"
    "blockquote" "body" "br" "button" "caption" "center" "cite" "code" "col"
    "colgroup" "dd" "del" "dfn" "dir" "div" "dl" "dt" "em" "fieldset" "font" "form"
    "frame" "frameset" "head" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "html" "i" "iframe" "img"
    "input" "ins" "kbd" "label" "legend" "li" "link" "map" "menu" "meta" "noframes"
    "noscript" "object" "ol" "optgroup" "option" "p" "param" "pre" "q" "s" "samp"
    "script" "select" "small" "span" "strike" "strong" "style" "sub" "sup" "table"
    "tbody" "td" "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var"
    ))

(provide 'css-simple-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css-simple-completion.el ends here

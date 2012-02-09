;; Mumamo Mode for jekyll posts
;; by Jorge Dias
(require 'mumamo-fun)

;; highlight support for multiple languages
(defconst mumamo-highlight-tag-start-regex
  (rx "{%"
      (0+ space)
      "highlight"
      space
      (submatch
       (1+ (any "a-za-z")))
      (0+ space)
      "%}\n"
      ))

(defvar jekyll-modes-list nil "List of modes for jekyll highlight")

;; You can define which mode to use for your highlight chunks
(add-to-list 'jekyll-modes-list '("ruby" 'ruby-mode))
(add-to-list 'jekyll-modes-list '("js" 'javascript-mode))
(add-to-list 'jekyll-modes-list '("sh" 'shell-script-mode))

;; (add-to-list 'jekyll-modes-list '("erlang" 'erlang-mode))
;; ((add-to-list 'jekyll-modes-list '("html" 'html-mode))

(defun mumamo-highlight-get-mode-for-chunk (string)
  (or
   (eval (elt (elt (member* string jekyll-modes-list :test 'member) 0) 1))
   'fundamental-mode)
)

(defun mumamo-search-fw-exc-start-highlight (pos max)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^{")
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "{% highlight" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 12))
      (when (looking-at mumamo-highlight-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) (mumamo-highlight-get-mode-for-chunk (match-string 1))
        )))))

(defun mumamo-highlight-chunk-end-fw (pos max)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "{% endhighlight %}")))

(defun mumamo-chunk-highlight (pos max)
  "Find {% highlight %}...{% endhighlight %}.  Return range and respective-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-highlight
                                 'mumamo-highlight-chunk-end-fw))

;;; This currently has a bug, so to recognize the chunk the first --- has to be followed by a whitespace
(defun mumamo-chunk-yaml (pos max)
  "Find yaml header.  Return range and 'yaml-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "---" "---" 'borders 'yaml-mode))

(define-mumamo-multi-major-mode jekyll-markdown-mumamo-mode
  "Turn on multiple major modes for jekyll blog posts with main mode `markdown-mode'."
  ("Markdown Family" markdown-mode
   (mumamo-chunk-yaml
    mumamo-chunk-highlight)))

(define-mumamo-multi-major-mode jekyll-textile-mumamo-mode
  "Turn on multiple major modes for jekyll blog posts with main mode `textile-mode'."
  ("Textile Family" textile-mode
   (mumamo-chunk-yaml
    mumamo-chunk-highlight)))

(add-to-list 'auto-mode-alist '("_\\(posts\\|drafts\\).*\\.textile$" . jekyll-textile-mumamo-mode))
(add-to-list 'auto-mode-alist '("_\\(posts\\|drafts\\).*\\.markdown$" . jekyll-markdown-mumamo-mode))

(provide 'jekyll-mumamo)
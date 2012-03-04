;; Mumamo Mode for jekyll posts
;; by Jorge Dias

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

;; (add-to-list 'jekyll-modes-list '("erlang" 'erlang-mode))
;; ((add-to-list 'jekyll-modes-list '("html" 'html-mode))

(defun mumamo-highlight-get-mode-for-chunk (string)
  (or
   (eval (elt (elt (member* string jekyll-modes-list :test 'member) 0) 1))
   'fundamental-mode)
)

(defun mumamo-search-bw-exc-start-highlight (pos min)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MIN is where to stop."
  (goto-char (+ pos 2))
  (let ((marker-start (search-backward "{%" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-highlight-tag-start-regex)
        (setq exc-start (match-end 0))
        (setq exc-mode (mumamo-highlight-get-mode-for-chunk (match-string 1)))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) exc-mode))))))

(defun mumamo-search-bw-exc-end-highlight (pos min)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str pos min "{% endhighlight %}"))

(defun mumamo-search-fw-exc-start-highlight (pos max)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^{")
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^{"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "{% highlight" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 12))
      (when (looking-at mumamo-highlight-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-search-fw-exc-end-highlight (pos max)
  "Helper for `mumamo-chunk-highlight'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "{% endhighlight %}")))

(defun mumamo-chunk-highlight (pos min max)
  "Find {% highlight %}...{% endhighlight %}.  Return range and respective-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-highlight
                              'mumamo-search-bw-exc-end-highlight
                              'mumamo-search-fw-exc-start-highlight
                              'mumamo-search-fw-exc-end-highlight))

;;; This currently has a bug, so to recognize the chunk the first --- has to be followed by a whitespace
(defun mumamo-chunk-yaml (pos min max)
  "Find yaml header.  Return range and 'yaml-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "---" "---\n" t 'yaml-mode t))

(define-mumamo-multi-major-mode jekyll-markdown-mumamo-mode
  "Turn on multiple major modes for jekyll blog posts with main mode `markdown-mode'."
  ("Markdown Family" markdown-mode
   (mumamo-chunk-yaml
    mumamo-chunk-highlight)))

(define-mumamo-multi-major-mode jekyll-textile-mumamo-mode
  "Turn on multiple major modes for jekyll blog posts with main mode `markdown-mode'."
  ("Markdown Family" textile-mode
   (mumamo-chunk-yaml
    mumamo-chunk-highlight)))

(add-to-list 'auto-mode-alist '("_\\(posts\\|drafts\\).*\\.textile$" . jekyll-textile-mumamo-mode))
(add-to-list 'auto-mode-alist '("_\\(posts\\|drafts\\).*\\.markdown$" . jekyll-markdown-mumamo-mode))

(provide 'jekyll-mumamo)
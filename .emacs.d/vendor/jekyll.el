;; jekyll.el
;;
;; Emacs support for Jekyll blogs.
;;
;; From metajack/jekyll. http://github.com/metajack/jekyll
;;
;; To use, just put this file somewhere in the load path and
;;   (require 'jekyll)
;;
;; Here are my key bindings:
;;   C-c b d - Show all drafts
;;   C-c b p - Show all posts
;;   C-c b n - Create new draft
;;   C-c b P - Publish current draft
;;
;; (global-set-key (kbd "C-c b n") 'jekyll-draft-post)
;; (global-set-key (kbd "C-c b P") 'jekyll-publish-post)
;; (global-set-key (kbd "C-c b p") (lambda ()
;;                                   (interactive)
;;                                   (find-file "~/Sources/blog/_posts/")))
;; (global-set-key (kbd "C-c b d") (lambda ()
;;                                   (interactive)
;;                                   (find-file "~/Sources/blog/_drafts/")))


(provide 'jekyll)

(defvar jekyll-directory nil
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".markdown"
  "File extension of Jekyll posts.")
(defvar jekyll-post-template
  "---\ntitle: %s\n---\n\n"
  "Default template for Jekyll posts.  %s will be replace by the post title.")

(defun jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun jekyll-draft-post (title)
  "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
                            (jekyll-make-slug title)
                            jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-publish-post ()
  "Move a draft post to the posts directory, and rename it so that it
contains the date."
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (concat jekyll-directory jekyll-drafts-dir)))
    (message "This is not a draft post.")
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
            (concat jekyll-directory jekyll-drafts-dir)))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat jekyll-directory jekyll-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

;; if mumamo is present we're going to import the mumamo modes
(if (featurep 'mumamo-fun)
    (require 'jekyll-mumamo))
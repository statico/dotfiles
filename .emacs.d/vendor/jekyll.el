;;; jekyll.el --- Emacs support for Jekyll blogs

;; Author: Jack Moffitt
;; 	Jorge Dias
;; Maintainer: Jorge Dias

;;; Commentary:

;; Original version: http://github.com/metajack/jekyll
;;
;; To use, just put this file somewhere in the load path and
;;   (require 'jekyll)
;;
;; Here are the default key bindings:
;;   C-c j d - Show all drafts
;;   C-c j p - Show all posts
;;   C-c j n - Create new draft
;;   C-c j P - Publish current draft
;;
;; To load them just do:
;; (jekyll-init-keybindings)

;;; Code:

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

(defun jekyll-init-keybindings ()
  "Initialize default keybindings for jekyll"
  (global-set-key (kbd "C-c j n") 'jekyll-draft-post)
  (global-set-key (kbd "C-c j P") 'jekyll-publish-post)

  (global-set-key (kbd "C-c j p") (lambda ()
                                    (interactive)
                                    (find-file(concat jekyll-directory "_posts/"))))
  (global-set-key (kbd "C-c j d") (lambda ()
                                    (interactive)
                                    (find-file(concat jekyll-directory "_drafts/"))))
)

;; if mumamo is present we're going to import the mumamo modes
(if (require 'mumamo-fun nil t)
    (require 'jekyll-mumamo))

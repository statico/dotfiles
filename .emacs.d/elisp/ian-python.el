;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use M-RET to toggle hiding if enabled
(add-hook 'hs-minor-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "M-RET") 'hs-toggle-hiding)
             (define-key python-mode-map (kbd "C-M-RET") 'hs-show-all)))

;; Use our local installation of Pymacs and rope.
(setq pymacs-load-path '("~/.emacs.d/python"))
(setenv "PYTHONPATH"(concat (getenv "PYTHONPATH")
                             ":" (expand-file-name "~/.emacs.d/python")))

;; Load Rope/ropemacs only when needed.
(defun init-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-local-prefix "C-c C-p")
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-confirm-saving nil)
  (setq ropemacs-guess-project t))

;; Load pyflakes for flymake with Python.
(defun init-pyflakes ()
  "Sets up flymake to use pyflakes on Python files. Requires 'pyflakes' in path."
  (interactive)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (add-hook 'find-file-hook 'flymake-find-file-hook))

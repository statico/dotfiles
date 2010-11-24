;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use our local installation of Pymacs and rope.
(setenv "PYTHONPATH"
        (concat (getenv "PYTHONPATH") ":"
                (expand-file-name "~/.emacs.d/python")))

;; Basic Pymacs setup instructions.
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; Load Rope.
(when (>= emacs-major-version 23)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-"))

;; Rope Settings
(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c C-p")

;; Python/Flymake/Pylint attempt.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; https://github.com/daleharvey/jshint-mode
(defun init-jshint ()
  (require 'flymake-jshint)
  (add-hook 'javascript-mode-hook
            (lambda () (flymake-mode t))))

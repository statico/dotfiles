;;
;; Make Pymacs, Rope, Ropemacs and YASnippet just work in GNU Emacs 23.
;;
;; Inspired by and partialy stolen from:
;; - http://bit.ly/bzr15C
;; - http://github.com/EnigmaCurry/emacs/blob/master/ryan-python.el
;;

;; Use Dave Love's newer python.el instead of the older python-mode.el
(require 'python)

;; ----------------------------------------------------------------------------
;; Bindings
;; ----------------------------------------------------------------------------

;; "This automatically indents newlines and attempts to locate the
;; cursor at the appropriate, whitespace-sensitive location whenever
;; you press Return."
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-m"
                                 'newline-and-indent)))

;; Don't map <Tab> to YASnippet. In fact, set it to something we'll never use
;; because we'll only ever trigger it indirectly.
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)

;; Instead, map <Tab> to Ryan's python auto-completer.
(define-key python-mode-map "\t" 'ryan-python-tab)

;; ----------------------------------------------------------------------------
;; Environment
;; ----------------------------------------------------------------------------

(setenv "PYTHONPATH"
        (concat (getenv "PYTHONPATH") ":" (expand-file-name "~/.emacs.d/python")))

;; ----------------------------------------------------------------------------
;; Pymacs
;; ----------------------------------------------------------------------------

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; ----------------------------------------------------------------------------
;; Rope and Ropemacs
;; ----------------------------------------------------------------------------

(pymacs-load "ropemacs" "rope-")

(setq ropemacs-enable-autoimport t)

;; ----------------------------------------------------------------------------
;; Auto-completion
;; ----------------------------------------------------------------------------

;;  Integrates:
;;   1) Rope
;;   2) Yasnippet
;;   all with AutoComplete.el

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
      (dolist (element list value)
        (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
      . (lambda ()
          (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
      (if (string= "." (buffer-substring (- (point) 1) (point)))
        (point)
        nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
        (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
          (setq cand
                (delq nil
                      (mapcar (lambda (candidate)
                                (propertize candidate 'source source))
                              (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
          (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
            (set (make-local-variable 'ac-find-function) 'ac-python-find)
            (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
            (set (make-local-variable 'ac-auto-start) nil)))

(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
    (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
           (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
           (set (make-local-variable 'ac-auto-start) nil))

;; ----------------------------------------------------------------------------

(provide 'init-python)

;;; wrap-to-fill.el --- Make a fill-column wide space for editing
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-08-12 Wed
;; Version:
;; Last-Updated: x
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

(eval-when-compile (require 'mumamo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Wrapping

;;;###autoload
(defgroup wrap-to-fill nil
  "Customizing of `wrap-to-fill-column-mode'."
  :group 'convenience)

;;;###autoload
(defcustom wrap-to-fill-left-marg nil
  "Left margin handling for `wrap-to-fill-column-mode'.
Used by `wrap-to-fill-column-mode'. If nil then center the
display columns. Otherwise it should be a number which will be
the left margin."
  :type '(choice (const :tag "Center" nil)
                 (integer :tag "Left margin"))
  :group 'wrap-to-fill)
(make-variable-buffer-local 'wrap-to-fill-left-marg)

(defvar wrap-to-fill--saved-state nil)
;;(make-variable-buffer-local 'wrap-to-fill--saved-state)
(put 'wrap-to-fill--saved-state 'permanent-local t)

;;;###autoload
(defcustom wrap-to-fill-left-marg-modes
  '(text-mode
    fundamental-mode)
  "Major modes where `wrap-to-fill-left-margin' may be nil."
  :type '(repeat command)
  :group 'wrap-to-fill)


         ;;ThisisaVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongWord ThisisaVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongWord

(defun wrap-to-fill-wider ()
  "Increase `fill-column' with 10."
  (interactive)
  (setq fill-column (+ fill-column 10))
  (wrap-to-fill-set-values-in-buffer-windows))

(defun wrap-to-fill-narrower ()
  "Decrease `fill-column' with 10."
  (interactive)
  (setq fill-column (- fill-column 10))
  (wrap-to-fill-set-values-in-buffer-windows))

(defun wrap-to-fill-normal ()
  "Reset `fill-column' to global value."
  (interactive)
  ;;(setq fill-column (default-value 'fill-column))
  (kill-local-variable 'fill-column)
  (wrap-to-fill-set-values-in-buffer-windows))

(defvar wrap-to-fill-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'wrap-to-fill-wider)
    (define-key map [(control ?c) ?-] 'wrap-to-fill-narrower)
    (define-key map [(control ?c) ?0] 'wrap-to-fill-normal)
    map))

;; Fix-me: Maybe make the `wrap-prefix' behavior an option or separate
;; minor mode.

;; Fix-me: better handling of left-column in mumamo buffers (and other
;; if possible).

;;;###autoload
(define-minor-mode wrap-to-fill-column-mode
  "Use `fill-column' display columns in buffer windows.
By default the display columns are centered, but see the option
`wrap-to-fill-left-marg'.

Fix-me:
Note 1: When turning this on `visual-line-mode' is also turned on. This
is not reset when turning off this mode.

Note 2: The text properties 'wrap-prefix and 'wrap-to-fill-prefix
is set by this mode to indent continuation lines.

Key bindings added by this minor mode:

\\{wrap-to-fill-column-mode-map}"
  :lighter " WrapFill"
  :group 'wrap-to-fill
  ;; (message "wrap-to-fill-column-mode %s, cb=%s, major=%s, multi=%s" wrap-to-fill-column-mode (current-buffer)
  ;;          major-mode mumamo-multi-major-mode)
  (if wrap-to-fill-column-mode
      (progn
        ;; Old values (idea from visual-line-mode)
	(set (make-local-variable 'wrap-to-fill--saved-state) nil)
	(dolist (var '(visual-line-mode
                       ;;left-margin-width
                       ;;right-margin-width
                       ))
          (push (list var (symbol-value var) (local-variable-p var))
                wrap-to-fill--saved-state))
        ;; Hooks
        (add-hook 'window-configuration-change-hook 'wrap-to-fill-set-values nil t)
        ;; Wrapping
        (visual-line-mode 1)
        (wrap-to-fill-set-values-in-buffer-windows))
    ;; Hooks
    (remove-hook 'window-configuration-change-hook 'wrap-to-fill-set-values t)
    ;; Old values
    (dolist (saved wrap-to-fill--saved-state)
      (let ((var (nth 0 saved))
            (val (nth 1 saved))
            (loc (nth 2 saved)))
        (cond
         ((eq var 'visual-line-mode)
          (unless val (visual-line-mode -1)))
         (t
          (if loc
              (set (make-local-variable var) val)
            (kill-local-variable var))))))
    (kill-local-variable 'wrap-to-fill--saved-state)
    ;; Margins
    (dolist (win (get-buffer-window-list (current-buffer)))
      (set-window-margins win left-margin-width right-margin-width))
    ;; Indentation
    (let ((here (point))
          (inhibit-field-text-motion t)
          beg-pos
          end-pos)
      (mumamo-with-buffer-prepared-for-jit-lock
       (save-restriction
         (widen)
         (goto-char (point-min))
         (while (< (point) (point-max))
           (setq beg-pos (point))
           (setq end-pos (line-end-position))
           (when (equal (get-text-property beg-pos 'wrap-prefix)
                        (get-text-property beg-pos 'wrap-to-fill-prefix))
             (remove-list-of-text-properties
              beg-pos end-pos
              '(wrap-prefix)))
           (forward-line))
         (remove-list-of-text-properties
          (point-min) (point-max)
          '(wrap-to-fill-prefix)))
       (goto-char here))))
  (wrap-to-fill-font-lock wrap-to-fill-column-mode))
(put 'wrap-to-fill-column-mode 'permanent-local t)

(defcustom wrap-to-fill-major-modes '(org-mode
                                      html-mode
                                      nxhtml-mode)
  "Major modes where to turn on `wrap-to-fill-column-mode'"
  ;;:type '(repeat major-mode)
  :type '(repeat command)
  :group 'wrap-to-fill)

(defun wrap-to-fill-turn-on-in-buffer ()
  "Turn on fun for globalization."
  (when (catch 'turn-on
          (dolist (m wrap-to-fill-major-modes)
            (when (derived-mode-p m)
              (throw 'turn-on t))))
    (wrap-to-fill-column-mode 1)))

(define-globalized-minor-mode wrap-to-fill-column-global-mode wrap-to-fill-column-mode
  wrap-to-fill-turn-on-in-buffer
  :group 'wrap-to-fill)

;; Fix-me: There is a confusion between buffer and window margins
;; here. Also the doc says that left-margin-width and dito right may
;; be nil. However they seem to be 0 by default, but when displaying a
;; buffer in a window then window-margins returns (nil).

(defvar wrap-to-fill-timer nil)
(make-variable-buffer-local 'wrap-to-fill-timer)

(defun wrap-to-fill-set-values ()
  (when (timerp wrap-to-fill-timer)
    (cancel-timer wrap-to-fill-timer))
  (setq wrap-to-fill-timer
        (run-with-idle-timer 0 nil 'wrap-to-fill-set-values-in-timer
                             (selected-window) (current-buffer))))
(put 'wrap-to-fill-set-values 'permanent-local-hook t)

(defun wrap-to-fill-set-values-in-timer (win buf)
  (condition-case err
      (when (buffer-live-p buf)
        (wrap-to-fill-set-values-in-buffer-windows buf))
    (error (message "ERROR wrap-to-fill-set-values-in-timer: %s"
                    (error-message-string err)))))

(defun wrap-to-fill-set-values-in-timer-old (win buf)
  (when (and (window-live-p win) (buffer-live-p buf)
             (eq buf (window-buffer win)))
    (condition-case err
        (with-current-buffer buf
          (when wrap-to-fill-column-mode
            (wrap-to-fill-set-values-in-window win)))
      (error (message "ERROR wrap-to-fill-set-values: %s"
                      (error-message-string err))))))

(defun wrap-to-fill-set-values-in-buffer-windows (&optional buffer)
  "Use `fill-column' display columns in buffer windows."
  (let ((buf-windows (get-buffer-window-list (or buffer
                                                 (current-buffer))
                                             nil
                                             t)))
    (dolist (win buf-windows)
      (if wrap-to-fill-column-mode
          (wrap-to-fill-set-values-in-window win)
        (set-window-buffer nil (current-buffer))))))

(defvar wrap-old-win-width nil)
(make-variable-buffer-local 'wrap-old-win-width)
;; Fix-me: compensate for left-margin-width etc
(defun wrap-to-fill-set-values-in-window (win)
  (with-current-buffer (window-buffer win)
    (when wrap-to-fill-column-mode
      (let* ((win-width (window-width win))
             (win-margs (window-margins win))
             (win-full (+ win-width
                          (or (car win-margs) 0)
                          (or (cdr win-margs) 0)))
             (extra-width (- win-full fill-column))
             (fill-left-marg (unless (memq major-mode wrap-to-fill-left-marg-modes)
                               (or (when (> left-margin-width 0) left-margin-width)
                                   wrap-to-fill-left-marg)))
             (left-marg (if fill-left-marg
                            fill-left-marg
                          (- (/ extra-width 2) 1)))
             ;; Fix-me: Why do I have to subtract 1 here...???
             (right-marg (- win-full fill-column left-marg 1))
             (need-update nil)
             )
        ;; (when wrap-old-win-width
        ;;   (unless (= wrap-old-win-width win-width)
        ;;     (message "-")
        ;;     (message "win-width 0: %s => %s, win-full=%s, e=%s l/r=%s/%s %S %S %S" wrap-old-win-width win-width win-full extra-width left-marg right-marg (window-edges) (window-inside-edges) (window-margins))
        ;;    ))
        (setq wrap-old-win-width win-width)
        (unless (> left-marg 0) (setq left-marg 0))
        (unless (> right-marg 0) (setq right-marg 0))
        (unless nil;(= left-marg (or left-margin-width 0))
          ;;(setq left-margin-width left-marg)
          (setq need-update t))
        (unless nil;(= right-marg (or right-margin-width 0))
          ;;(setq right-margin-width right-marg)
          (setq need-update t))
        ;;(message "win-width a: %s => %s, win-full=%s, e=%s l/r=%s/%s %S %S %S" wrap-old-win-width win-width win-full extra-width left-margin-width right-margin-width (window-edges) (window-inside-edges) (window-margins))
        (when need-update
          ;;(set-window-buffer win (window-buffer win))
          ;;(run-with-idle-timer 0 nil 'set-window-buffer win (window-buffer win))
          ;;(dolist (win (get-buffer-window-list (current-buffer)))
            ;; Fix-me: check window width...
          (set-window-margins win left-marg right-marg)
          ;;)
          ;;(message "win-width b: %s => %s, win-full=%s, e=%s l/r=%s/%s %S %S %S" wrap-old-win-width win-width win-full extra-width left-marg right-marg (window-edges) (window-inside-edges) (window-margins))
          )
        ))))

;; (add-hook 'post-command-hook 'my-win-post-command nil t)
;; (remove-hook 'post-command-hook 'my-win-post-command t)
(defun my-win-post-command ()
  (message "win-post-command: l/r=%s/%s %S %S %S" left-margin-width right-margin-width (window-edges) (window-inside-edges) (window-margins))
           )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font lock

(defun wrap-to-fill-fontify (bound)
  (save-restriction
    (widen)
    (while (< (point) bound)
      (let ((this-bol (if (bolp) (point)
                        (1+ (line-end-position)))))
        (unless (< this-bol bound) (setq this-bol nil))
        (when this-bol
          (goto-char (+ this-bol 0))
          (let (ind-str
                ind-str-fill
                (beg-pos this-bol)
                (end-pos (line-end-position)))
            (when (equal (get-text-property beg-pos 'wrap-prefix)
                         (get-text-property beg-pos 'wrap-to-fill-prefix))
              ;; Find indentation
              (skip-chars-forward "[:blank:]")
              (setq ind-str (buffer-substring-no-properties beg-pos (point)))
              ;; Any special markers like -, * etc
              (if (and (< (1+ (point)) (point-max))
                       (memq (char-after) '(?- ;; 45
                                            ?– ;; 8211
                                            ?*
                                            ))
                       (eq (char-after (1+ (point))) ?\ ))
                  (setq ind-str-fill (concat "  " ind-str))
                (setq ind-str-fill ind-str))
              ;;(setq ind-str-fill (concat "  " ind-str))
              (mumamo-with-buffer-prepared-for-jit-lock
               (put-text-property beg-pos end-pos 'wrap-prefix ind-str-fill)
               (put-text-property beg-pos end-pos 'wrap-to-fill-prefix ind-str-fill))))))
      (forward-line 1))
    ;; Note: doing it line by line and returning t gave problem in mumamo.
    (when nil ;this-bol
      (set-match-data (list (point) (point)))
      t)))

(defun wrap-to-fill-font-lock (on)
  ;; See mlinks.el
  (let* ((add-or-remove (if on 'font-lock-add-keywords 'font-lock-remove-keywords))
         (fontify-fun 'wrap-to-fill-fontify)
         (args (list nil `(( ,fontify-fun ( 0 'font-lock-warning-face t ))))))
    (when fontify-fun
      (when on (setq args (append args (list t))))
      (apply add-or-remove args)
      (font-lock-mode -1)
      (font-lock-mode 1))))

(provide 'wrap-to-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wrap-to-fill.el ends here

;;; pause.el --- Take a break!
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-19 Sat
(defconst pause:version "0.70");; Version:
;; Last-Updated: 2010-01-18 Mon
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
;; If you are using Emacs then don't you need a little reminder to
;; take a pause?  This library makes Emacs remind you of that.  And
;; gives you a link to a yoga exercise to try in the pause.
;;
;; There are essentially two different ways to use this library.
;; Either you run a separate Emacs process that just reminds you of
;; pauses.  To use it that way see `pause-start-in-new-emacs'.
;;
;; Or run it in the current Emacs.  To do that add to your .emacs
;;
;;   (require 'pause)
;;
;; and do
;;
;;   M-x customize-group RET pause RET
;;
;; and set `pause-mode' to t.
;;
;;
;; Note: I am unsure if it works on all systems to use a separate
;;       Emacs process.  It does work on w32 though.  Please tell me
;;       about other systems.
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
;; published by the Free Software Foundation; either version 2, or
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

;;;###autoload
(defgroup pause nil
  "Customize your health personal Emacs health saver!"
  :group 'convenience)

(defcustom pause-after-minutes 15
  "Pause after this number of minutes."
  :type 'number
  :group 'pause)

(defcustom pause-1-minute-delay 60
  "Number of seconds to wait in 1 minutes delay."
  :type 'number
  :group 'pause)

(defcustom pause-idle-delay 5
  "Seconds to wait for user to be idle before pause."
  :type 'number
  :group 'pause)

(defcustom pause-even-if-not-in-emacs t
  "Jump up pause even if not in Emacs."
  :type 'boolean
  :group 'pause)

(defcustom pause-restart-anyway-after 2
  "If user does not use Emacs restart timer after this minutes.
This is used when a user has clicked a link."
  :type 'number
  :group 'pause)

(defcustom pause-tell-again-after 2
  "If user does not exit pause tell again after this minutes."
  :type 'number
  :group 'pause)

(defcustom pause-extra-fun 'pause-start-get-yoga-poses
  "Function to call for extra fun when pausing.
Default is to show a link to a yoga exercise (recommended!).

Set this variable to nil if you do not want any extra fun.

If this variable's value is a function it will be called when the
pause frame has just been shown."
  :type '(choice (function :tag "Extra function")
                 (const :tag "No extra function" nil))
  :group 'pause)

(defvar pause-exited-from-button nil)

(defcustom pause-background-color "orange"
  "Background color during pause."
  :type 'color
  :group 'pause)

(defcustom pause-mode-line-color "sienna"
  "Mode line color during pause."
  :type 'color
  :group 'pause)

(defcustom pause-1-minute-mode-line-color "yellow"
  "Mode line color during 1 minute phase of pause."
  :type 'color
  :group 'pause)

(defface pause-text-face
  '((t (:foreground "sienna" :height 1.5 :bold t)))
  "Face main text in pause buffer."
  :group 'pause)

(defface pause-info-text-face
  '((t (:foreground "yellow")))
  "Face info text in pause buffer."
  :group 'pause)

(defface pause-message-face
  '((t (:inherit secondary-selection)))
  "Face for pause messages."
  :group 'pause)

(defface pause-1-minute-message-face
  '((t (:inherit mode-line-inactive)))
  "Face for pause messages."
  :group 'pause)

(defcustom pause-break-text
  (concat "\n\tHi there,"
          "\n\tYou are worth a PAUSE!"
          "\n\nTry some mindfulness:"
          "\n\t- Look around and observe."
          "\n\t- Listen."
          "\n\t- Feel your body.")
  "Text to show during pause."
  :type 'integer
  :group 'pause)

(defvar pause-el-file (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))

(defvar pause-default-img-dir
  (let ((this-dir (file-name-directory pause-el-file)))
    (expand-file-name "../etc/img/pause/" this-dir)))

(defcustom pause-img-dir pause-default-img-dir
  "Image directory for pause.
A random image is choosen from this directory for pauses."
  :type 'directory
  :group 'pause)



(defvar pause-timer nil)

;;(defvar pause-break-exit-calls nil)

(defun pause-start-timer ()
  (pause-start-timer-1 (* 60 pause-after-minutes)))

(defun pause-start-timer-1 (sec)
  (pause-cancel-timer)
  (setq pause-timer (run-with-timer sec nil 'pause-pre-break)))

(defun pause-one-minute ()
  "Give you another minute ..."
  (pause-start-timer-1 pause-1-minute-delay)
  (message (propertize " OK, I will come back in a minute! -- greatings from pause"
                       'face 'pause-message-face)))

(defun pause-save-me ()
  (pause-start-timer)
  (message (propertize " OK, I will save you again in %d minutes! -- greatings from pause "
                       'face 'pause-message-face)
           pause-after-minutes))

(defun pause-pre-break ()
  (condition-case err
      (save-match-data ;; runs in timer
        (pause-cancel-timer)
        (setq pause-timer (run-with-idle-timer pause-idle-delay nil 'pause-break-in-timer)))
    (error
     (lwarn 'pause-pre-break
            :error "%s" (error-message-string err)))))

(defvar pause-break-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta shift ?p)] 'pause-break-exit)
    (define-key map [tab]         'forward-button)
    (define-key map [(meta tab)]  'backward-button)
    (define-key map [(shift tab)] 'backward-button)
    (define-key map [backtab]     'backward-button)
    map))

(defvar pause-buffer nil)
(defvar pause-frame nil)

(define-derived-mode pause-break-mode nil "Pause"
  "Mode used during pause in pause buffer.

It defines the following key bindings:

\\{pause-break-mode-map}"
  (set (make-local-variable 'buffer-read-only) t)
  (setq show-trailing-whitespace nil)
  ;;(set (make-local-variable 'cursor-type) nil)
  ;; Fix-me: workaround for emacs bug
  ;;(run-with-idle-timer 0 nil 'pause-hide-cursor)
  )

;; Fix-me: make one state var
(defvar pause-break-exit-active nil)
(defvar pause-break-1-minute-state nil)


(defun pause-break ()
  (pause-cancel-timer)
  (let ((wcfg (current-frame-configuration))
        (old-mode-line-bg (face-attribute 'mode-line :background))
        old-frame-bg-color
        old-frame-left-fringe
        old-frame-right-fringe
        old-frame-tool-bar-lines
        old-frame-menu-bar-lines
        old-frame-vertical-scroll-bars)
    (dolist (f (frame-list))
      (add-to-list 'old-frame-bg-color (cons f (frame-parameter f 'background-color)))
      (add-to-list 'old-frame-left-fringe (cons f (frame-parameter f 'left-fringe)))
      (add-to-list 'old-frame-right-fringe (cons f (frame-parameter f 'right-fringe)))
      (add-to-list 'old-frame-tool-bar-lines (cons f (frame-parameter f 'tool-bar-lines)))
      (add-to-list 'old-frame-menu-bar-lines (cons f (frame-parameter f 'menu-bar-lines)))
      (add-to-list 'old-frame-vertical-scroll-bars (cons f (frame-parameter f 'vertical-scroll-bars))))

    ;; Fix-me: Something goes wrong with the window configuration, try a short pause
    (remove-hook 'window-configuration-change-hook 'pause-break-exit)
    (run-with-idle-timer 0.2 nil 'pause-break-show)
    (setq pause-break-exit-active nil)
    (setq pause-break-1-minute-state nil) ;; set in `pause-break-show'
    (setq pause-exited-from-button nil)
    (unwind-protect
        (let ((n 0)
              (debug-on-error nil))
          (while (and (> 3 (setq n (1+ n)))
                      (not pause-break-exit-active)
                      (not pause-break-1-minute-state))
            (condition-case err
                (recursive-edit)
              (error (message "%s" (error-message-string err))))
            (unless (or pause-break-exit-active
                        pause-break-1-minute-state)
              (when (> 2 n) (message "Too early to pause (%s < 2)" n))
              (add-hook 'window-configuration-change-hook 'pause-break-exit))))

      (remove-hook 'window-configuration-change-hook 'pause-break-exit)
      (pause-tell-again-cancel-timer)
      ;;(set-frame-parameter nil 'background-color "white")
      (dolist (f (frame-list))
        (set-frame-parameter f 'background-color     (cdr (assq f old-frame-bg-color)))
        (set-frame-parameter f 'left-fringe          (cdr (assq f old-frame-left-fringe)))
        (set-frame-parameter f 'right-fringe         (cdr (assq f old-frame-right-fringe)))
        (set-frame-parameter f 'tool-bar-lines       (cdr (assq f old-frame-tool-bar-lines)))
        (set-frame-parameter f 'menu-bar-lines       (cdr (assq f old-frame-menu-bar-lines)))
        (set-frame-parameter f 'vertical-scroll-bars (cdr (assq f old-frame-vertical-scroll-bars))))
      ;; Fix-me: The frame grows unless we do redisplay here:
      (redisplay t)
      (set-frame-configuration wcfg t)
      (when pause-frame(lower-frame pause-frame))
      (set-face-attribute 'mode-line nil :background old-mode-line-bg)
      (run-with-idle-timer 2.0 nil 'run-hooks 'pause-break-exit-hook)
      (kill-buffer pause-buffer)
      (cond (pause-exited-from-button
             ;; Do not start timer until we start working again.
             (run-with-idle-timer 1 nil 'add-hook 'post-command-hook 'pause-save-me-post-command)
             ;; But if we do not do that within some minutes then start timer anyway.
             (run-with-idle-timer (* 60 pause-restart-anyway-after) nil 'pause-save-me))
            (pause-break-1-minute-state
             (run-with-idle-timer 0 nil 'pause-one-minute))
            (t
             (run-with-idle-timer 0 nil 'pause-save-me))))))

(defun pause-save-me-post-command ()
  (pause-start-timer))

(defvar pause-break-exit-hook nil
  "Hook run after break exit.
Frame configuration has been restored when this is run.
Please note that it is run in a timer.")

(defun pause-break-show ()
  ;; In timer
  (save-match-data
    (condition-case err
        (pause-break-show-1)
      (error
       ;;(remove-hook 'window-configuration-change-hook 'pause-break-exit)
       (pause-break-exit)
       (message "pause-break-show error: %s" (error-message-string err))))))

(defvar pause-break-last-wcfg-change (float-time))

(defun pause-break-show-1 ()
  ;; Do these first if something goes wrong.
  (setq pause-break-last-wcfg-change (float-time))
  ;;(run-with-idle-timer (* 1.5 (length (frame-list))) nil 'add-hook 'window-configuration-change-hook 'pause-break-exit)

  ;; fix-me: temporary:
  ;;(add-hook 'window-configuration-change-hook 'pause-break-exit)
  (unless pause-extra-fun (run-with-idle-timer 1  nil 'pause-break-message))
  (run-with-idle-timer 10 nil 'pause-break-exit-activate)
  (setq pause-break-1-minute-state t)
  (set-face-attribute 'mode-line nil :background pause-1-minute-mode-line-color)
  (with-current-buffer (setq pause-buffer
                             (get-buffer-create "* P A U S E *"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pause-break-mode)
      (setq left-margin-width 25)
      (pause-insert-img)
      (insert (propertize pause-break-text 'face 'pause-text-face))
      (goto-char (point-min))
      (when (search-forward "mindfulness" nil t)
        (make-text-button (- (point) 11) (point)
                          'face '(:inherit pause-text-face :underline t)
                          'action (lambda (btn)
                                    (browse-url "http://www.jimhopper.com/mindfulness/"))))
      (goto-char (point-max))
      (insert (propertize "\n\nClick on a link below to exit pause\n" 'face 'pause-info-text-face))
      ;;(add-text-properties (point-min) (point-max) (list 'keymap (make-sparse-keymap)))
      (insert-text-button "Exit pause"
                          'action `(lambda (button)
                                     (condition-case err
                                         (pause-break-exit-from-button)
                                       (error (message "%s" (error-message-string err))))))
      (insert "\n")
      (dolist (m '(hl-needed-mode))
        (when (and (boundp m) (symbol-value m))
          (funcall m -1)))))
    (dolist (f (frame-list))
      (pause-max-frame f))
    (pause-tell-again)
    (when pause-extra-fun (funcall pause-extra-fun))
    ;;(setq pause-break-exit-calls 0)
    (setq pause-break-last-wcfg-change (float-time))
    (pause-tell-again-start-timer))

(defun pause-max-frame (f)
  (let* ((avail-width (- (display-pixel-width)
                         (* 2 (frame-parameter f 'border-width))
                         (* 2 (frame-parameter f 'internal-border-width))))
         (avail-height (- (display-pixel-height)
                          (* 2 (frame-parameter f 'border-width))
                          (* 2 (frame-parameter f 'internal-border-width))))
         (cols (/ avail-width (frame-char-width)))
         (rows (- (/ avail-height (frame-char-height)) 2)))
    ;;(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
    ;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
    (setq pause-break-last-wcfg-change (float-time))
    (with-selected-frame f
      (delete-other-windows (frame-first-window f))
      (with-selected-window (frame-first-window)
        (switch-to-buffer pause-buffer)
        (goto-char (point-max))))
    (modify-frame-parameters f
                             `((background-color . ,pause-background-color)
                               (left-fringe . 0)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (vertical-scroll-bars . nil)
                               (left . 0)
                               (top . 0)
                               (width  . ,cols)
                               (height . ,rows)
                               ))))

(defvar pause-tell-again-timer nil)

(defun pause-tell-again-start-timer ()
  (pause-tell-again-cancel-timer)
  (setq pause-tell-again-timer
        (run-with-idle-timer (* 60 pause-tell-again-after) t 'pause-tell-again)))

(defun pause-tell-again-cancel-timer ()
  (when (timerp pause-tell-again-timer)
    (cancel-timer pause-tell-again-timer))
  (setq pause-tell-again-timer nil))

(defun pause-tell-again ()
  (when (and window-system pause-even-if-not-in-emacs)
    (pause-max-frame pause-frame)
    (raise-frame pause-frame)))


(defun pause-break-message ()
  (when (/= 0 (recursion-depth))
    (message "%s" (propertize "Please take a pause! (Or exit now to take it in 1 minute.)"
                              'face 'pause-1-minute-message-face))))

(defun pause-break-exit-activate ()
  (when (/= 0 (recursion-depth))
    (setq pause-break-exit-active t)
    (setq pause-break-1-minute-state nil)
    (set-face-attribute 'mode-line nil :background pause-mode-line-color)
    (message nil)
    (with-current-buffer pause-buffer
      (let ((inhibit-read-only t))
        ;; Fix-me: This interfere with text buttons.
        ;;(add-text-properties (point-min) (point-max) (list 'keymap nil))
        ))))

(defun pause-break-exit ()
  (interactive)
  (let ((elapsed (- (float-time) pause-break-last-wcfg-change)))
    ;;(message "elapsed=%s pause-break-last-wcfg-change=%s" elapsed pause-break-last-wcfg-change)
    (setq pause-break-last-wcfg-change (float-time))
    (when (> elapsed 1.0)
      (setq pause-break-exit-active t)
      (remove-hook 'window-configuration-change-hook 'pause-break-exit)
      ;;(pause-tell-again-cancel-timer)
      (when (/= 0 (recursion-depth))
        (exit-recursive-edit)))))

(defun pause-break-exit-from-button ()
  (setq pause-break-1-minute-state nil)
  (setq pause-exited-from-button t)
  (pause-break-exit))

(defun pause-insert-img ()
  (let* ((inhibit-read-only t)
        img
        src
        (slice '(0 0 200 300))
        (imgs (directory-files pause-img-dir nil nil t))
        skip
        )
    (setq imgs (delete nil
                       (mapcar (lambda (d)
                                 (unless (file-directory-p d) d))
                               imgs)))
    (if (not imgs)
        (setq img "No images found")
      (setq skip (random (length imgs)))
      (while (> skip 0)
        (setq skip (1- skip))
        (setq imgs (cdr imgs)))
      (setq src (expand-file-name (car imgs) pause-img-dir))
      (if (file-exists-p src)
          (condition-case err
              (setq img (create-image src nil nil
                                      :relief 1
                                      ;;:margin inlimg-margins
                                      ))
            (error (setq img (error-message-string err))))
        (setq img (concat "Image not found: " src))))
    (if (stringp img)
        (insert img)
      (insert-image img nil 'left-margin slice)
      )
    ))

(defun pause-hide-cursor ()
  ;; runs in timer, save-match-data
  (with-current-buffer pause-buffer
    (set (make-local-variable 'cursor-type) nil)))

(defun pause-cancel-timer ()
  (remove-hook 'post-command-hook 'pause-save-me-post-command)
  (when (timerp pause-timer) (cancel-timer pause-timer))
  (setq pause-timer nil))

(defun pause-break-in-timer ()
  (save-match-data ;; runs in timer
    (pause-cancel-timer)
    (if (or (active-minibuffer-window)
            (and (boundp 'edebug-active)
                 edebug-active))
        (let ((pause-idle-delay 5))
          (pause-pre-break))
      (let ((there-was-an-error nil))
        (condition-case err
            (pause-break)
          (error
           (setq there-was-an-error t)))
        (when there-was-an-error
          (condition-case err
              (progn
                (select-frame last-event-frame)
                (let ((pause-idle-delay nil))
                  (pause-pre-break)))
            (error
             (lwarn 'pause-break-in-timer2 :error "%s" (error-message-string err))
             )))))))

(defcustom pause-only-when-server-mode t
  "Allow `pause-mode' inly in the Emacs that has server-mode enabled.
This is to prevent multiple Emacs with `pause-mode'."
  :type 'boolean
  :group 'pause)

;;;###autoload
(define-minor-mode pause-mode
  "This minor mode tries to make you take a break.
It will jump up and temporary stop your work - even if you are
not in Emacs.  If you are in Emacs it will however try to be
gentle and wait until you have been idle with the keyboard for a
short while. \(If you are not in Emacs it can't be gentle. How
could it?)

Then it will show you a special screen with a link to a yoga
exercise you can do when you pause.

After the pause you continue your work where you were
interrupted."
  :global t
  :group 'pause
  :set-after '(server-mode)
  (if pause-mode
      (if (and pause-only-when-server-mode
               (not server-mode)
               (not (with-no-warnings (called-interactively-p))))
          (progn
            (setq pause-mode nil)
            (message "Pause mode canceled because not server-mode"))
        (pause-start-timer))
    (pause-cancel-timer)))

;; (emacs-Q "-l" buffer-file-name "--eval" "(pause-temp-err)")
;; (emacs-Q "-l" buffer-file-name "--eval" "(run-with-timer 1 nil 'pause-temp-err)")
;; (pause-temp-err)
(defun pause-temp-err ()
  (switch-to-buffer (get-buffer-create "pause-temp-err buffer"))
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max) (list 'keymap nil))
    (insert-text-button "click to test"
                        'action (lambda (btn)
                                  (message "Click worked")))
    ;;(add-text-properties (point-min) (point-max) (list 'keymap nil))
    ))

;; (customize-group-other-window 'pause)
;; (apply 'custom-set-variables (pause-get-group-saved-customizations 'pause custom-file))
;; (pause-get-group-saved-customizations 'w32shell custom-file)
(defun pause-get-group-saved-customizations (group cus-file)
  "Return customizations saved for GROUP in CUS-FILE."
  (let* ((cus-buf (find-buffer-visiting cus-file))
         (cus-old cus-buf)
         (cus-point (when cus-old (with-current-buffer cus-old (point))))
         (cusg-all (get group 'custom-group))
         (cusg-vars (delq nil (mapcar (lambda (elt)
                                        (when (eq (nth 1 elt) 'custom-variable)
                                          (car elt)))
                                      cusg-all)))
         cus-vars-form
         cus-face-form
         cus-saved-vars
         cus-saved-face)
    (unless cus-buf (setq cus-buf (find-file-noselect cus-file)))
    (with-current-buffer cus-buf
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (progn
                 (while (progn (skip-chars-forward " \t\n\^l")
                               (looking-at ";"))
                   (forward-line 1))
                 (not (eobp)))
          (let ((form (read (current-buffer))))
            (cond
             ((eq (car form) 'custom-set-variables)
              (setq cus-vars-form form))
             ((eq (car form) 'custom-set-faces)
              (setq cus-face-form form))
            )))))
    (dolist (vl (cdr cus-vars-form))
      (when (memq (car (cadr vl)) cusg-vars)
        (setq cus-saved-vars (cons (cadr vl) cus-saved-vars))))
    cus-saved-vars))

;; (emacs-Q "-l" buffer-file-name "--eval" "(pause-start 0.1 nil)")
(defun pause-start (after-minutes cus-file)
  "Start `pause-mode' with interval AFTER-MINUTES.
This bypasses `pause-only-when-server-mode'.

You can use this funciton to start a separate Emacs process that
handles pause, for example like this if you want a pause every 15
minutes:

  emacs -Q -l pause --eval \"(pause-start 15 nil)\"

Note: Another easier alternative might be to use
      `pause-start-in-new-emacs'."
  (interactive "nPause after how many minutes: ")
  (pause-start-1 after-minutes cus-file))

(defun pause-start-1 (after-minutes cus-file)
  (setq debug-on-error t)
  (pause-cancel-timer)
  (when (and cus-file (file-exists-p cus-file))
    (let ((args (pause-get-group-saved-customizations 'pause cus-file)))
      ;;(message "cus-file=%S" cus-file)
      ;;(message "args=%S" args)
      (apply 'custom-set-variables args)))
  (setq pause-after-minutes after-minutes)
  (let ((pause-only-when-server-mode nil))
    (pause-mode 1))
  (switch-to-buffer (get-buffer-create "Pause information"))
  (insert (propertize "Emacs pause\n"
                      'face '(:inherit variable-pitch :height 1.5)))
  (insert (format "Pausing every %d minute.\n" after-minutes))
  (insert "Or, ")
  (insert-text-button "pause now"
                      'action `(lambda (button)
                                 (condition-case err
                                     (pause-break)
                                   (error (message "%s" (error-message-string err))))))
  (insert "!\n")
  ;;(setq buffer-read-only t)
  (pause-break-mode)
  (delete-other-windows)
  (setq mode-line-format nil)
  (setq pause-frame (selected-frame))
  (message nil)
  (set-frame-parameter nil 'background-color pause-background-color))

;; (pause-start-in-new-emacs 0.3)
;; (pause-start-in-new-emacs 15)
;;;###autoload
(defun pause-start-in-new-emacs (after-minutes)
  "Start pause with interval AFTER-MINUTES in a new Emacs instance.
The new Emacs instance will be started with -Q.  However if
`custom-file' is non-nil it will be loaded so you can still
customize pause.

One way of using this function may be to put in your .emacs
something like

  ;; for just one Emacs running pause
  (when server-mode (pause-start-in-new-emacs 15))

See `pause-start' for more info.

"
  (interactive (list pause-after-minutes))
  (let* ((this-emacs (locate-file invocation-name
                                  (list invocation-directory)
                                  exec-suffixes))
         (cus-file (if custom-file custom-file "~/.emacs"))
         (args `("-l" ,pause-el-file
                 "--geometry=40x3"
                 "-D"
                 "--eval" ,(format "(pause-start %s %S)" after-minutes cus-file))))
    (setq args (cons "-Q" args))
    (apply 'call-process this-emacs nil 0 nil args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link to yoga poses

;; (defun w3-download-callback (fname)
;;   (let ((coding-system-for-write 'binary))
;;     (goto-char (point-min))
;;     (search-forward "\n\n" nil t)
;;     (write-region (point) (point-max) fname))
;;   (url-mark-buffer-as-dead (current-buffer))
;;   (message "Download of %s complete." (url-view-url t))
;;   (sit-for 3))

;;(run-with-idle-timer 0 nil 'pause-get-yoga-poses)
(defvar pause-yoga-poses-host-url "http://www.abc-of-yoga.com/")

;;(pause-start-get-yoga-poses)
(defun pause-start-get-yoga-poses ()
  (require 'url-vars)
  (let ((url-show-status nil)) ;; do not show download messages
    (url-retrieve (concat pause-yoga-poses-host-url "yogapractice/mountain.asp")
                  'pause-callback-get-yoga-poses)))

(defun pause-callback-get-yoga-poses (status)
  (let ((pose (pause-random-yoga-pose (pause-get-yoga-poses-1 (current-buffer)))))
    (message nil)
    (when (and pose (buffer-live-p pause-buffer))
      (pause-insert-yoga-link pose))))

(defun pause-insert-yoga-link (pose)
  (with-current-buffer pause-buffer
    (let ((here (point))
          (inhibit-read-only t)
          (pose-url (concat pause-yoga-poses-host-url (car pose))))
      (goto-char (point-max))
      (insert "Link to yoga posture for you: ")
      (insert-text-button (cdr pose)
                          'action `(lambda (button)
                                     (condition-case err
                                         (progn
                                           (browse-url ,pose-url)
                                           (run-with-idle-timer 1 nil 'pause-break-exit-from-button))
                                       (error (message "%s" (error-message-string err))))))
      (insert "\n")
      (pause-break-message))))

(defun pause-get-yoga-poses ()
  (let* ((url-show-status nil) ;; do not show download messages
         (buf (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp")))
    (pause-get-yoga-poses-1 buf)))

;; (setq x (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp"))
;; (setq x (url-retrieve-synchronously "http://www.emacswiki.org/emacs/EmacsFromBazaar"))

;; (defun temp-y ()
;;   (message "before y")
;;   ;;(setq y (url-retrieve-synchronously "http://www.emacswiki.org/emacs/EmacsFromBazaar"))
;;   (setq x (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp"))
;;   (message "after x")
;;   )
;; (run-with-idle-timer 0 nil 'temp-y)

(defun pause-get-yoga-poses-1 (buf)
  (require 'url)
  (setq url-debug t)
  ;; url-insert-file-contents
  (let* ((first-marker "<p>These are all the Yoga Poses covered in this section:</p>")
         (table-patt "<table\\(?:.\\|\n\\)*?</table>")
         table-beg
         table-end
         (pose-patt "<A HREF=\"\\([^\"]*?\\)\" class=\"LinkBold\">\\([^<]*?\\)</A>")
         poses
         (trouble-msg
          (catch 'trouble
            ;;(switch-to-buffer-other-window buf)
            (with-current-buffer buf
              (goto-char 1)
              (rename-buffer "YOGA" t)
              (unless (search-forward first-marker nil t)
                (throw 'trouble "Can't find marker for the poses on the page"))
              (backward-char 10)
              (unless (re-search-forward table-patt nil t)
                (throw 'trouble "Can't find table with poses on the page"))
              (setq table-beg (match-beginning 0))
              (setq table-end (match-end 0))
              (goto-char table-beg)
              (while (re-search-forward pose-patt table-end t)
                (setq poses (cons (cons (match-string 1) (match-string 2))
                                  poses)))
              (unless poses
                (throw 'trouble "Can't find poses in table on the page"))
              (kill-buffer)
              nil))))
    (if trouble-msg
        (progn
          (message "%s" trouble-msg)
          nil)
      (message "Number of yoga poses found=%s" (length poses))
      poses)))

(defun pause-random-yoga-pose (poses)
  (when poses
    (random t)
    (let* ((n-poses (length poses))
           (pose-num (random (1- n-poses)))
           (the-pose (nth pose-num poses)))
      the-pose)))

;;(pause-random-yoga-pose (pause-get-yoga-poses))

(provide 'pause)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pause.el ends here

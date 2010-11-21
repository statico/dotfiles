;;; mlinks.el --- Minor mode making major mode dependent links
;;
;; Author: Lennar Borgman
;; Created: Tue Jan 16 2007
(defconst mlinks:version "0.28") ;;Version:
;; Last-Updated: 2010-01-05 Tue
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   `appmenu', `cl', `mail-prsvr', `mm-util', `ourcomments-util',
;;   `url-expand', `url-methods', `url-parse', `url-util',
;;   `url-vars'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file implements the minor mode `mlinks-mode' that create
;; hyperlinks for different major modes.  Such links can be visible or
;; invisible.  The meanings of the links are defined per mode.
;;
;; Examples:
;;
;; - In in html style modes the links are visible they can mean either
;;   open a file for editing, go to an achnor or view the link in a
;;   web browser etc.
;;
;; - In emacs lisp mode the links are invisible, but maybe highlighed
;;   when point or mouse is on them.  (Having them highlighted when
;;   point is on them can be a quick way to check that you have
;;   spelled a symbol correct.)  The meanings of the links in emacs
;;   lisp mode are go to definition.
;;
;; Common to links that open a buffer in Emacs is that you can the
;; buffer opened in the same window, the other window or in a new
;; frame.  The same key binding is used in all major modes for this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; FIX-ME: url-hexify-string etc
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'appmenu nil t))
(eval-when-compile (require 'mumamo nil t))
(eval-when-compile (require 'ourcomments-util nil t))

(require 'rx)
(require 'url-parse)
(require 'url-expand)

(defvar mlinks-point-hilighter-overlay nil)
(make-variable-buffer-local 'mlinks-point-hilighter-overlay)
(put 'mlinks-point-hilighter-overlay 'permanent-local t)

;;;###autoload
(defgroup mlinks nil
  "Customization group for `mlinks-mode'."
  :group 'nxhtml
  :group 'hypermedia)

(defvar mlinks-link-face 'mlinks-link-face)
(defface mlinks-link-face
  '((t (:inherit highlight)))
  "Face normally active links have on them."
  :group 'mlinks)

(defvar mlinks-hyperactive-link-face 'mlinks-hyperactive-link-face)
(defface mlinks-hyperactive-link-face
  '((t (:inherit isearch)))
  "Face hyper active links have on them."
  :group 'mlinks)

(defvar mlinks-font-lock-face 'mlinks-font-lock-face)
(defface mlinks-font-lock-face
  '((t :inherit link))
  "Default face for MLinks' links."
  :group 'mlinks)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode function bindings

;;(customize-option mlinks-mode-functions)
(defcustom mlinks-mode-functions
  '(
    ;; For message buffer etc.
    (fundamental-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      )
     )
    (emacs-lisp-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      )
     )
    ;; *scractch*
    (lisp-interaction-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      )
     )
    (help-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      )
     )
    (Info-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      )
     )
    (Custom-mode
     ((goto mlinks-elisp-custom-goto)
      (hili mlinks-elisp-hili)
      (hion t)
      (fontify mlinks-custom-fontify)
      )
     )
    (text-mode
     ((goto mlinks-goto-plain-url)
      (hion t)
      (fontify mlinks-plain-urls-fontify)
      )
     )
    (nxhtml-mode
     ((hion t)
      (fontify mlinks-html-fontify)
      (goto mlinks-html-style-goto)
      )
     )
    (nxml-mode
     ((hion t)
      (fontify mlinks-html-fontify)
      (goto mlinks-html-style-goto)
      )
     )
    (sgml-mode
     ((hion t)
      (fontify mlinks-html-fontify)
      (goto mlinks-html-style-goto)
      )
     )
    (html-mode
     ((hion t)
      (fontify mlinks-html-fontify)
      (goto mlinks-html-style-goto)
      )
     )
    )
  "Defines MLinks hyperlinks for major modes.
"
  ;; Each element in the list is a list with two elements

  ;;   \(MAJOR-MODE SETTINGS)

  ;; where MAJOR-MODE is the major mode for which the settings SETTINGS should be used.
  ;; SETTINGS is an association list which can have the following element types

  ;;   \(hili HILIGHT-FUN)  ;; Mandatory
  ;;   \(goto GOTO-FUN)     ;; Mandatory
  ;;   \(hion HION-BOOL)    ;; Optional
  ;;   \(next NEXT-FUN)     ;; Optional
  ;;   \(prev PREV-FUN)     ;; Optional

  ;; Where
  ;; - HILIGHT-FUN is the function to hilight a link when point is
  ;;   inside the link. This is done when Emacs is idle.
  ;; - GOTO-FUN is the function to follow the link at point.
  ;; - HION-BOOL is t or nil depending on if hilighting should be on
  ;;   by default.
  ;; - NEXT-FUN is the function to go to the next link.
  ;; - PREV-FUN is the function to go to the previous link."
  ;;   ;;:type '(repeat (alist :key-type symbol :value-type (alist :key-type symbol :value symbol)))
  :type '(alist :key-type major-mode-function
                :value-type (list
                             (set
                              (const :tag "Enable MLinks in this major mode" hion)
                              (const :tag "Mark All Links" mark)
                              (list :tag "Enable" (const :tag "Hilighting" hili) function)
                              (list :tag "Enable" (const :tag "Follow Link" goto) function)
                              (list :tag "Enable" (const :tag "Goto Next Link" next) function)
                              (list :tag "Enable" (const :tag "Goto Previous Link" prev) function)
                              )))
  :group 'mlinks)


(defun mlinks-get-mode-value (which)
  (let* ((major major-mode)
         (mode-rec (assoc major mlinks-mode-functions)))
    (catch 'mode-rec
      (while (and major
                  (not mode-rec))
        (setq major (get major 'derived-mode-parent))
        (setq mode-rec (assoc major mlinks-mode-functions))
        (when mode-rec (throw 'mode-rec nil))))
    (when mode-rec
      (let* ((mode (car mode-rec))
             (funs-alist (cadr mode-rec))
             (funs (assoc which funs-alist)))
        (cdr funs)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor modes

;; (appmenu-dump-keymap mlinks-mode-map)
(defvar mlinks-mode-map
  (let ((m (make-sparse-keymap "mlinks")))
    (define-key m [(control ?c) ?\r ?\r]   'mlinks-goto)
    (define-key m [(control ?c) ?\r ?w]    'mlinks-goto-other-window)
    (define-key m [(control ?c) ?\r ?f]    'mlinks-goto-other-frame)
    (define-key m [(control ?c) ?\r ?n]    'mlinks-next-saved-position)
    (define-key m [(control ?c) ?\r ?p]    'mlinks-prev-saved-position)
    (define-key m [(control ?c) ?\r S-tab] 'mlinks-backward-link)
    (define-key m [(control ?c) ?\r tab]   'mlinks-forward-link)
    (define-key m [(control ?c) ?\r ?h]    'mlinks-toggle-hilight)
    (define-key m [(control ?c) ?\r ?c]    'mlinks-copy-link-text)
    m))

;;;###autoload
(define-minor-mode mlinks-mode
  "Recognizes certain parts of a buffer as hyperlinks.
The hyperlinks are created in different ways for different major
modes with the help of the functions in the list
`mlinks-mode-functions'.

The hyperlinks can be hilighted when point is over them.  Use
`mlinks-toggle-hilight' to toggle this feature for the current
buffer.

All keybindings in this mode are by default done under the prefi§x
key

  C-c RET

which is supposed to be a kind of mnemonic for link (alluding to
the RET key commonly used in web browser to follow a link).
\(Unfortunately this breaks the rules in info node `Key Binding
Conventions'.) Below are the key bindings defined by this mode:

\\{mlinks-mode-map}

For some major modes `mlinks-backward-link' and
`mlinks-forward-link' will take you to the previous/next link.
By default the link moved to will be active, see
`mlinks-active-links'.

"
  nil
  " L"
  nil
  :keymap mlinks-mode-map
  :group 'mlinks
  (if mlinks-mode
      (progn
        (mlinks-add-appmenu)
        (mlinks-start-point-hilighter)
        (mlinks-add-font-lock))
    (mlinks-stop-point-hilighter)
    (when mlinks-point-hilighter-overlay
      (when (overlayp mlinks-point-hilighter-overlay)
        (delete-overlay mlinks-point-hilighter-overlay))
      (setq mlinks-point-hilighter-overlay nil))
    (mlinks-remove-font-lock)))
(put 'mlinks-mode 'permanent-local t)

(defun mlinks-turn-on-in-buffer ()
  (let ((hion (unless (and (boundp 'mumamo-set-major-running)
                           mumamo-set-major-running)
                (mlinks-get-mode-value 'hion))))
    (when hion (mlinks-mode 1))))

;;;###autoload
(define-globalized-minor-mode mlinks-global-mode mlinks-mode
  mlinks-turn-on-in-buffer
  "Turn on `mlink-mode' in all buffer where it is specified.
This is specified in `mlinks-mode-functions'."
  :group 'mlinks)

;; The problem with global minor modes:
(when (and mlinks-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (mlinks-global-mode 1))

;;(define-toggle mlinks-active-links t
(define-minor-mode mlinks-active-links
  "Use quick movement keys on active links if non-nil.
When moving to an mlink with `mlinks-forward-link' or
`mlinks-backward-link' the link moved to will be in an active
state.  This is marked with a new color \(the face `isearch').
When the new color is shown the following keys are active

\\{mlinks-hyperactive-point-hilighter-keymap}
Any command cancels this state."
  :global t
  :init-value t
  :group 'mlinks)



(defun mlinks-link-text-prop-range (pos)
  (let* ((link-here (get-text-property pos 'mlinks-link))
         (beg (when link-here (previous-single-char-property-change (+ pos 1) 'mlinks-link)))
         (end (when link-here (next-single-char-property-change (- pos 0) 'mlinks-link))))
    (when (and beg end)
      (cons beg end))))

(defun mlinks-link-range (pos)
  (or (mlinks-link-text-prop-range pos)
      (let ((funs-- (mlinks-get-mode-value 'hili)))
        (when funs--
          (save-match-data
            (run-hook-with-args-until-success 'funs--))))))

(defun mlinks-link-at-point ()
  "Get link at point."
  (mlinks-point-hilighter-1)
  (when (and mlinks-point-hilighter-overlay
             (overlay-buffer mlinks-point-hilighter-overlay))
    (let* ((ovl mlinks-point-hilighter-overlay)
           (beg (overlay-start ovl))
           (end (overlay-end ovl)))
      (buffer-substring-no-properties beg end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; At point highligher

(defvar mlinks-point-hilighter-timer nil)

(defun mlinks-stop-point-hilighter ()
  (when (timerp mlinks-point-hilighter-timer)
    (cancel-timer mlinks-point-hilighter-timer)
    (setq mlinks-point-hilighter-timer nil)))

(defun mlinks-start-point-hilighter ()
  (mlinks-stop-point-hilighter)
  (setq mlinks-point-hilighter-timer
        (run-with-idle-timer 0.1 t 'mlinks-point-hilighter)))

(defvar mlinks-link-overlay-priority 100)

(defun mlinks-make-point-hilighter-overlay (bounds)
  (unless mlinks-point-hilighter-overlay
    (setq mlinks-point-hilighter-overlay
          (make-overlay (car bounds) (cdr bounds)))
    (overlay-put mlinks-point-hilighter-overlay 'priority mlinks-link-overlay-priority)
    (overlay-put mlinks-point-hilighter-overlay 'mouse-face 'highlight)
    (mlinks-set-normal-point-hilight)
    ))

(defun mlinks-point-hilighter ()
  "Mark link at point if any.
This moves the hilight point overlay to point or deletes it."
  ;; This runs in a timer, protect it.
  (condition-case err
      (let ((inhibit-point-motion-hooks t))
        (mlinks-point-hilighter-1))
    (error "mlinks-point-hilighter error: %s" (error-message-string err))))

(defun mlinks-point-hilighter-1 ()
  (when mlinks-mode
    (let ((bounds-- (mlinks-link-range (point))))
      (if bounds--
          (if mlinks-point-hilighter-overlay
              (move-overlay mlinks-point-hilighter-overlay (car bounds--) (cdr bounds--))
            (mlinks-make-point-hilighter-overlay bounds--))
        (when mlinks-point-hilighter-overlay
          (delete-overlay mlinks-point-hilighter-overlay))))))

(defvar mlinks-hyperactive-point-hilighter-keymap
  (let ((m (make-sparse-keymap "mlinks")))
    (define-key m [S-tab]   'mlinks-backward-link)
    (define-key m [tab]     'mlinks-forward-link)
    (define-key m "\t"      'mlinks-forward-link)
    (define-key m [?\r]     'mlinks-goto)
    (define-key m [?w]      'mlinks-goto-other-window)
    (define-key m [?f]      'mlinks-goto-other-frame)
    (define-key m [mouse-1] 'mlinks-goto)
    (set-keymap-parent m mlinks-mode-map)
    m))

(defvar mlinks-point-hilighter-keymap
  (let ((m (make-sparse-keymap "mlinks")))
    (define-key m [mouse-1] 'mlinks-goto)
    (set-keymap-parent m mlinks-mode-map)
    m))

(defun mlinks-point-hilighter-pre-command ()
  (condition-case err
      (unless (let ((map (overlay-get mlinks-point-hilighter-overlay 'keymap)))
                (where-is-internal this-command
                                   (list
                                    map)))
        (mlinks-set-normal-point-hilight)
        (unless mlinks-point-hilighter-timer
          (delete-overlay mlinks-point-hilighter-overlay)))
    (error (message "mlinks-point-hilighter-pre-command: %s" err))))
(put 'mlinks-point-hilighter-pre-command 'permanent-local t)

(defun mlinks-set-hyperactive-point-hilight ()
  "Make link hyper active, ie add some special key binding.
Used after jumping specifically to a link. The idea is that the
user may want to easily jump between links in this state."
  (add-hook 'pre-command-hook 'mlinks-point-hilighter-pre-command nil t)
  (mlinks-point-hilighter)
  (overlay-put mlinks-point-hilighter-overlay 'face mlinks-hyperactive-link-face)
  (overlay-put mlinks-point-hilighter-overlay 'keymap mlinks-hyperactive-point-hilighter-keymap))

(defun mlinks-set-normal-point-hilight ()
  "Make link normally active as if you happened to be on it."
  (remove-hook 'pre-command-hook 'mlinks-point-hilighter-pre-command t)
  (mlinks-point-hilighter)
  (overlay-put mlinks-point-hilighter-overlay 'face mlinks-link-face)
  (overlay-put mlinks-point-hilighter-overlay 'keymap mlinks-point-hilighter-keymap))

(defun mlinks-set-point-hilight-after-jump-to ()
  "Set hilight style after jump to link."
  (if mlinks-active-links
      (mlinks-set-hyperactive-point-hilight)
    (mlinks-set-normal-point-hilight)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jumping around

(defvar mlinks-places nil)
(make-variable-buffer-local 'mlinks-placesn)
(put 'mlinks-places 'permanent-local t)

(defvar mlinks-places-n 0)
(make-variable-buffer-local 'mlinks-places-n)
(put 'mlinks-places-n 'permanent-local t)

(defun mlinks-has-links ()
  (or (mlinks-get-mode-value 'fontify)
      (when (and (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode)
        ;; Fix-me: just assume multi major has it... Need a list of
        ;; major modes. There is no way to get such a list for the
        ;; multi major mode (since you can't know what the chunk
        ;; functions will return.  However you can get a list of
        ;; current chunks major mode.
        t
        )))

(defun mlinks-backward-link ()
  "Go to previous `mlinks-mode' link in buffer."
  (interactive)
  (if (not (mlinks-has-links))
      (message "There is no way to go to previous link for this major mode")
    (let ((res (mlinks-prev-link)))
      (if res
          (progn
            (goto-char res)
            (mlinks-set-point-hilight-after-jump-to))
        (message "No previous link found")))))

(defun mlinks-forward-link ()
  "Go to next `mlinks-mode' link in buffer."
  (interactive)
  (if (not (mlinks-has-links))
      (message "There is no way to go to next link for this major mode")
    (let ((res (mlinks-next-link)))
      (if res
          (progn
            (goto-char res)
            (mlinks-set-point-hilight-after-jump-to))
        (message "No next link found")))))


(defun mlinks-goto ()
  "Follow `mlinks-mode' link at current point.
Save the current position so that they can be move to again by
`mlinks-prev-saved-position' and `mlinks-next-saved-position'.

Return non-nil if link was followed, otherewise nil."
  (interactive)
  (mlinks-goto-1 nil))

(defun mlinks-goto-other-window ()
  "Like `mlinks-goto' but opens in other window.
Uses `switch-to-buffer-other-window'."
  (interactive)
  (mlinks-goto-1 'other-window))

(defun mlinks-goto-other-frame ()
  "Like `mlinks-goto' but opens in other frame.
Uses `switch-to-buffer-other-frame'."
  (interactive)
  (mlinks-goto-1 'other-frame))

(defun mlinks-goto-1(where)
  (push-mark)
  (let* ((funs (mlinks-get-mode-value 'goto))
         (old (point-marker))
         (mlinks-temp-buffer-where where)
         (res (run-hook-with-args-until-success 'funs)))
    (if (not res)
        (progn
          (message "Don't know how to follow this MLink link")
          nil)
      (unless (= old (point-marker))
        (let* ((prev (car mlinks-places)))
          (when (or (not prev)
                    ;;(not (markerp prev))
                    (not (marker-buffer prev))
                    (/= old prev))
            (setq mlinks-places (cons old mlinks-places))
            (setq mlinks-places-n (length mlinks-places))))))))


(defun mlinks-prev-saved-position ()
  "Go to previous position saved by `mlinks-goto'."
  (interactive)
  (unless (mlinks-goto-n (1- mlinks-places-n))
    (message "No previous MLink position")))

(defun mlinks-next-saved-position ()
  "Go to next position saved by `mlinks-goto'."
  (interactive)
  (unless (mlinks-goto-n (1+ mlinks-places-n))
    (message "No next MLink position")))

(defun mlinks-goto-n (to)
  (if (not mlinks-places)
      (message "No saved MLinks positions")
    (let ((minp 1)
          (maxp (length mlinks-places)))
      (if (<= to minp)
          (progn
            (setq to minp)
            (message "Going to first MLinks position"))
        (if (>= to maxp)
            (progn
              (setq to maxp)
              (message "Going to last MLinks position"))))
      (setq mlinks-places-n to)
      (let ((n (- maxp to))
            (places mlinks-places)
            place
            buffer
            point)
        (while (> n 0)
          (setq places (cdr places))
          (setq n (1- n)))
        (setq place (car places))
        (mlinks-switch-to-buffer (marker-buffer place))
        (goto-char place)))))

(defvar mlinks-temp-buffer-where nil)
(defun mlinks-switch-to-buffer (buffer)
  (mlinks-switch-to-buffer-1 buffer mlinks-temp-buffer-where))

(defun mlinks-switch-to-buffer-1(buffer where)
  (cond
   ((null where)
    (switch-to-buffer buffer))
   ((eq where 'other-window)
    (switch-to-buffer-other-window buffer))
   ((eq where 'other-frame)
    (switch-to-buffer-other-frame buffer))
   (t
    (error "Invalid argument, where=%s" where))))

;; FIXME: face, var
(defun mlinks-custom (var)
  (customize-option var)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AppMenu support

(defun mlinks-appmenu ()
  (when mlinks-mode
    ;; Fix-me: reverse the list
    (let ((link-val (mlinks-link-at-point))
          (map (make-sparse-keymap "mlinks"))
          (num 2))
      (when (mlinks-get-mode-value 'prev)
        (define-key map [mlinks-next-link]
          (list 'menu-item "Next Link" 'mlinks-forward-link)))
      (when (mlinks-get-mode-value 'next)
        (define-key map [mlinks-prev-link]
          (list 'menu-item "Previous Link" 'mlinks-backward-link)))
      (when link-val
        (let* ((possible (when (member major-mode '(html-mode nxhtml-mode nxml-mode sqml-mode text-mode))
                           (mlinks-html-possible-href-actions link-val)))
               (mailto (assoc 'mailto possible))
               (view-web (assoc 'view-web possible))
               (view-web-base (assoc 'view-web-base possible))
               (edit (assoc 'edit possible))
               (file (nth 1 edit))
               (anchor (nth 2 edit))
               (choices)
               (answer)
               )
          (when (> (length map) num)
            (define-key map [mlinks-href-sep] (list 'menu-item "--")))
          (setq num (length map))
          (when view-web
            (define-key map [mlinks-href-view-web]
              (list 'menu-item "Browse Link Web Url"
                    `(lambda () (interactive)
                       (browse-url ,link-val)))))
          (when view-web-base
            (define-key map [mlinks-href-view-web-based]
              (list 'menu-item "Browse Link Web Url (base URL found)"
                    `(lambda () (interactive)
                       (browse-url (cdr ,view-web-base))))))
          (when mailto
            (define-key map [mlinks-href-mail]
              (list 'menu-item (concat "&Mail to " (substring link-val 7))
                    `(lambda () (interactive)
                       (mlinks-html-mail-to ,link-val)))))
          (when edit
            (when (and (file-exists-p file)
                       (not anchor)
                       (assoc 'upload possible))
              (let ((abs-file (expand-file-name file)))
                (define-key map [mlinks-href-upload]
                  (list 'menu-item "Upload Linked File"
                        `(lambda () (interactive)
                           (html-upl-upload-file ,abs-file))))))
            (when (and (file-exists-p file)
                       (not anchor)
                       (assoc 'edit-gimp possible))
              (let ((abs-file (expand-file-name file)))
                (define-key map [mlinks-href-edit-gimp]
                  (list 'menu-item "Edit Linked File with GIMP"
                        `(lambda () (interactive)
                           (gimpedit-edit-file ,abs-file))))))
            (when (and (file-exists-p file)
                       (assoc 'view-local possible))
              (let ((url (concat "file:///" (expand-file-name file))))
                (when anchor
                  (let ((url-anchor (concat url "#" anchor)))
                    (define-key map [mlinks-href-view-file-at]
                      (list 'menu-item (concat "Browse Linked File URL at #" anchor)
                            `(lambda () (interactive)
                               (browse-url ,url-anchor))))))
                (define-key map [mlinks-href-view-file]
                  (list 'menu-item "&Browse Linked File URL"
                        `(lambda () (interactive)
                           (browse-url ,url))))))
            (when (> (length map) num)
              (define-key map [mlinks-href-sep-2] (list 'menu-item "--")))
            (setq num (length map))
            (unless (equal file (buffer-file-name))
              (define-key map [mlinks-href-edit]
                (list 'menu-item "&Open Linked File"
                      `(lambda () (interactive) (mlinks-goto))))
              (define-key map [mlinks-href-edit-window]
                (list 'menu-item "&Open Linked File in Other Window"
                      `(lambda () (interactive) (mlinks-goto-other-window))))
              (define-key map [mlinks-href-edit-frame]
                (list 'menu-item "&Open Linked File in New Frame"
                      `(lambda () (interactive) (mlinks-goto-other-frame))))
              )
            (when (and (file-exists-p file) anchor)
              (define-key map [mlinks-href-edit-at]
                (list 'menu-item (concat "Open Linked File &at #" anchor)
                      `(lambda () (interactive)
                         (mlinks-goto)))))
            )
          (when (> (length map) num)
            (define-key map [mlinks-href-sep-1] (list 'menu-item "--")))
          (setq num (length map))
          (when link-val
            (define-key map [mlinks-href-copy-link]
              (list 'menu-item "&Copy Link Text"
                    'mlinks-copy-link-text)))))
      (when (> (length map) 2)
        map))))

(defun mlinks-add-appmenu ()
  "Add entries for MLinks to AppMenu."
  (when (featurep 'appmenu)
    (appmenu-add 'mlinks 100 'mlinks-mode "Current MLink" 'mlinks-appmenu)))

(defun mlinks-copy-link-text ()
  "Copy text of `mlinks-mode' link at point to clipboard."
  (interactive)
  (mlinks-point-hilighter)
  (let ((ovl mlinks-point-hilighter-overlay))
    (if (and ovl
             (overlayp ovl)
             (overlay-buffer ovl)
             (eq (current-buffer)
                 (overlay-buffer ovl))
             (<= (overlay-start ovl)
                 (point))
             (>= (overlay-end ovl)
                 (point)))
        (let* ((beg (overlay-start ovl))
               (end (overlay-end ovl))
               (str (buffer-substring beg end)))
          (copy-region-as-kill beg end)
          (message "Copied %d chars to clipboard" (length str)))
      (message "No link here to copy"))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; text-mode etc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mlinks-plain-urls-regexp
  (rx-to-string `(or (submatch (optional "mailto:")
                               (regexp ,(concat
                                         ;;"[a-z0-9$%(*-=?[_][^<>\")!;:,{}]*"
                                         "[a-z0-9$%(*=?[_-][^<>\")!;:,{}]*"
                                         "\@"
                                         "\\(?:[a-z0-9\-]+\.\\)+[a-z0-9]\\{2,4\\}")))
                     (submatch (or (regexp "https?://")
                                   "www.")
                               (1+ (any ,url-get-url-filename-chars))
                               )
                     )))

(defun mlinks-plain-urls-fontify (bound)
  (mlinks-fontify bound mlinks-plain-urls-regexp 0))

(defun mlinks-goto-plain-url ()
  (let* ((range (mlinks-link-range (point)))
         (link  (when range (buffer-substring-no-properties (car range) (cdr range)))))
    ;;(mlinks-html-href-act-on link)
    (when (= 0 (string-match mlinks-plain-urls-regexp link))
      (let ((which (if (match-end 1) 1 2)))
        (cond
         ((= 1 which)
          (mlinks-html-mail-to link)
          t)
         ((= 2 which)
          (browse-url link)
          t)
         (t nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; nxhtml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mlinks-html-style-goto ()
  (mlinks-html-style-mode-fun t))

(defvar mlinks-html-link-regexp
  ;; This value takes care of nxhtml-strval-mode (and is therefore a little bit incorrect ...)
  ;;"\\(?:^\\|[[:space:]]\\)\\(?:href\\|src\\)[[:space:]]*=[[:space:]]*\"\\([^<«\"]*\\)\""
  (rx (or "^" space)
      (or "href" "src")
      (0+ space)
      "="
      (0+ space)
      (submatch
       (or
        (seq "\""
             (and
              (0+ (not (any "\""))))
             "\"")
        (seq "'"
             (and
              (0+ (not (any "\'"))))
             "'")))))

(defun mlinks-html-style-mode-fun (goto)
  (let (start
        end
        bounds)
    (save-excursion
      (forward-char)
      (when (< 0 (skip-chars-forward "^\"'" (line-end-position)))
        (forward-char)
        (save-match-data
          (when (looking-back
                 mlinks-html-link-regexp
                 (line-beginning-position -1))
            (let ((which (if (match-beginning 1) 1 2)))
              (setq start (1+ (match-beginning which)))
              (setq end   (1- (match-end which))))
            (setq bounds (cons start end))))))
    (when start
      (if (not goto)
          bounds
        (let ((href-val (buffer-substring-no-properties start end)))
          (mlinks-html-href-act-on href-val))
        t))))

(defun mlink-check-file-to-edit (file)
  (assert (file-name-absolute-p file))
  (let ((file-dir (file-name-directory file)))
    (unless (file-directory-p file-dir)
      (if (file-directory-p (file-name-directory file))
          (if (yes-or-no-p (format "Directory %s does not exist. Create it? " file-dir))
              (make-directory file-dir)
            (setq file nil))
        (if (yes-or-no-p (format "Directory %s and its parent does not exist. Create them? " file-dir))
            (make-directory file-dir t)
          (setq file nil))))
    file))

(defun mlinks-html-edit-at (file &optional anchor)
  (let ((abs-file (if (file-name-absolute-p file)
                      file
                    (expand-file-name file))))
    (if (or (file-directory-p abs-file)
            (string= abs-file
                     (file-name-as-directory abs-file)))
        (if (file-directory-p abs-file)
            (when (y-or-n-p (format "Do you want to edit the directory %s? : " abs-file))
              (dired abs-file))
          (message "Can't find directory %s" abs-file))
      (when (mlink-check-file-to-edit abs-file)
        (let ((b (find-file-noselect abs-file)))
          (mlinks-switch-to-buffer b))
        (when anchor
          (let ((here (point))
                (anchor-regexp (concat "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\"" anchor "\"")))
            (goto-char (point-min))
            (if (search-forward-regexp anchor-regexp nil t)
                (backward-char 2)
              (message "Anchor \"%s\" not found" anchor)
              (goto-char here))))))))

(defun mlinks-html-mail-to (addr)
  (browse-url addr))

(defun mlinks-html-href-act-on (href-val)
  (if href-val
      (let* ((possible (mlinks-html-possible-href-actions href-val))
             (edit (assoc 'edit possible))
             (file (nth 1 edit))
             (anchor (nth 2 edit))
             )
        (cond (edit
               (mlinks-html-edit-at file anchor)
               t)
              ((assoc 'mailto possible)
               (when (y-or-n-p "This is a mail address.  Do you want to send a message to this mail address? ")
                 (mlinks-html-mail-to href-val)))
              ((assoc 'view-web possible)
               (when (y-or-n-p "Can't edit this URL, it is on the web.  View the URL in your web browser? ")
                 (browse-url href-val)))
              ((assoc 'view-web-base possible)
               (when (y-or-n-p "Can't edit, based URL is to the web.  View resulting URL in your web browser? ")
                 (browse-url (cdr (assoc 'view-web-base possible)))))
              (t
               (message "Do not know how to handle this URL"))
              ))
    (message "No value for href attribute")))

(defun mlinks-html-possible-href-actions (link)
  (let ((urlobj (url-generic-parse-url link))
        (edit nil)
        (possible nil))
    (cond ((member (url-type urlobj) '("http" "https"))
           (add-to-list 'possible (cons 'view-web link)))
          ((member (url-type urlobj) '("mailto"))
           (add-to-list 'possible (cons 'mailto link)))
          ((url-host urlobj)
           (message "Do not know how to handle this URL"))
          (t (setq edit t)))
    (when edit
      (let ((base-href (mlinks-html-find-base-href)))
        (when base-href
          (let ((baseobj (url-generic-parse-url base-href)))
            (setq edit nil)
            (cond ((member (url-type baseobj) '("http" "https"))
                   (add-to-list 'possible (cons 'view-web-base (url-expand-file-name link base-href))))
                  ((url-host urlobj)
                   (message "Do not know how to handle this URL"))
                  (t (setq edit t)))))
        (when edit
          (let* ((full (split-string (url-filename urlobj) "#"))
                 (file (nth 0 full))
                 (anchor (nth 1 full))
                 )
            (when (equal file "")
              (setq file (buffer-file-name)))
            (when base-href
              ;; We know at this point it is not a http url
              (setq file (expand-file-name file base-href)))
            (let ((ext (downcase (file-name-extension file))))
              (when (member ext '("htm" "html"))
                (add-to-list 'possible (cons 'view-local (list file anchor))))
              (when (and (featurep 'gimpedit)
                         (member ext '("gif" "png" "jpg" "jpeg")))
                (add-to-list 'possible (cons 'edit-gimp (list file anchor)))))
            (when (featurep 'html-upl)
              (add-to-list 'possible (cons 'upload (list file anchor))))
            (add-to-list 'possible (cons 'edit (list file anchor)))))))
    possible))

(defun mlinks-html-find-base-href ()
  "Return base href found in the current file."
  (let ((base-href))
    (save-excursion
      (goto-char (point-min))
      (while (and (not base-href)
                  (search-forward-regexp "<!--[^!]*-->\\|<base[[:space:]]" nil t))
        (when (equal " " (char-to-string (char-before)))
          (backward-char 6)
          (when (looking-at "<base [^>]*href *= *\"\\(.*?\\)\"")
            (setq base-href (match-string-no-properties 1))))))
    base-href))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Custom-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mlinks-elisp-custom-goto ()
  (mlinks-elisp-mode-fun 'custom))

(defvar mlinks-custom-link-regexp
  (rx "`"
      (group
       (1+ (not (any "'"))))
      "'"))

(defun mlinks-custom-fontify (bound)
  (mlinks-fontify bound mlinks-custom-link-regexp 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mlinks-elisp-goto ()
  (mlinks-elisp-mode-fun 'source))

(defun mlinks-elisp-hili ()
  (mlinks-elisp-mode-fun nil))

(defun mlinks-elisp-mode-fun (goto)
  (let ((symbol-name (thing-at-point 'symbol)))
    (when symbol-name
      (let ((bounds-- (bounds-of-thing-at-point 'symbol))
            ret--)
        (if (save-excursion
              (goto-char (cdr bounds--))
              (looking-back (concat "(\\(?:require\\|featurep\\)\s+'" symbol-name)
                            (line-beginning-position)))
            (progn
              (setq ret-- bounds--)
              (when goto
                (mlinks-elisp-mode-require symbol-name)))
          (when (mlinks-elisp-mode-symbol symbol-name goto)
            (setq ret-- bounds--)))
        ret--))))

(defun mlinks-elisp-function (symbol)
  "Go to an elisp function."
  (interactive "aElisp function: ")
  (mlinks-elisp-mode-symbol (symbol-name symbol) 'source))

(defun mlinks-elisp-mode-symbol (symbol-name-- goto--)
  ;; Fix-me: use uninterned variables (see mail from Miles)
  ;; Make these names a bit strange because they are boundp at the time of checking:
  (let ((symbol-- (intern-soft symbol-name--))
        defs--)
    (when (and symbol-- (boundp symbol--))
      (add-to-list 'defs-- 'variable))
    (when (fboundp symbol--)
      (add-to-list 'defs-- 'function))
    (when (facep symbol--)
      (add-to-list 'defs-- 'face))
    ;; Avoid some fails hits
    (when (memq symbol--
                '(goto t
                       bounds-- funs-- ret--
                       symbol-- defs-- symbol-name-- goto--))
      (setq defs-- nil))
    (let (defs-places
           def)
      (if (not goto--)
          (progn
            defs--)
        (if (not defs--)
            (progn
              (message "Could not find definition of '%s" symbol-name--)
              nil)
          (dolist (type (cond
                         ((eq goto-- 'source)
                          '(nil defvar defface))
                         ((eq goto-- 'custom)
                          '(defvar defface))
                         (t
                          (error "Bad goto-- value: %s" goto--))))
            (condition-case err
                (add-to-list 'defs-places
                             (cons
                              type
                              (save-excursion
                                (let* ((bp (find-definition-noselect symbol-- type))
                                       (b (car bp))
                                       (p (cdr bp)))
                                  (unless p
                                    (with-current-buffer b
                                      (save-restriction
                                        (widen)
                                        (setq bp (find-definition-noselect symbol-- type)))))
                                  bp))))
              (error
               ;;(lwarn '(mlinks) :error "%s" (error-message-string err))
               (when t
                 (cond
                  ((eq (car err) 'search-failed))
                  ((and (eq (car err) 'error)
                        (string= (error-message-string err)
                                 (format "Don't know where `%s' is defined" symbol--))))
                  (t
                   (message "%s: %s" (car err) (error-message-string err))))))))
          (if (= 1 (length defs-places))
              (setq def (car defs-places))
            (let ((many nil)
                  lnk)
              (dolist (d defs-places)
                (if (not lnk)
                    (setq lnk (cdr d))
                  (unless (equal lnk (cdr d))
                    (setq many t))))
              (if (not many)
                  (setq def (car defs-places))
                (let* ((alts (mapcar (lambda (elt)
                                       (let ((type (car elt))
                                             str)
                                         (setq str
                                               (cond
                                                ((not type)
                                                 "Function")
                                                ((eq type 'defvar)
                                                 "Variable")
                                                ((eq type 'defface)
                                                 "Face")))
                                         (cons str elt)))
                                     defs-places))
                       (stralts (mapcar (lambda (elt)
                                          (car elt))
                                        alts))
                       (completion-ignore-case t)
                       (stralt (completing-read "Type: " stralts nil t))
                       (alt (assoc stralt alts)))
                  (setq def (cdr alt))))))
          (when def
            (cond
             ((eq goto-- 'source)
              ;; Be sure to go to the real sources from CVS:
              (let* ((buf (car (cdr def)))
                     ;; Avoid going to source
                     ;;(file (find-source-lisp-file (with-current-buffer buf buffer-file-name)) )
                     (file (with-current-buffer buf buffer-file-name))
                     (orig-buf (find-file-noselect file)))
                (mlinks-switch-to-buffer orig-buf)
                (let ((p (cdr (cdr def))))
                  ;; Fix-me: Move this test to a more general place.
                  (if (or (< p (point-min))
                          (> p (point-max)))
                      ;; Check for cloned indirect buffers.
                      (progn
                        (setq orig-buf
                              (catch 'view-in-buf
                                (dolist (indirect-buf (buffer-list))
                                  ;;(message "base-buffer=%s, orig-buf=%s, eq => %s" (buffer-base-buffer indirect-buf) orig-buf (eq (buffer-base-buffer indirect-buf) orig-buf))
                                  (when (eq (buffer-base-buffer indirect-buf) orig-buf)
                                    (with-current-buffer indirect-buf
                                      ;;(message "indirect-buf=%s" indirect-buf)
                                      (unless (or (< p (point-min))
                                                  (> p (point-max)))
                                        ;;(message "switching")
                                        ;;(mlinks-switch-to-buffer indirect-buf)
                                        (message "mlinks: Switching to indirect buffer because of narrowing")
                                        (throw 'view-in-buf indirect-buf)
                                        ))
                                    ))))
                        (when orig-buf
                          (mlinks-switch-to-buffer orig-buf))
                        ;;(message "cb=%s" (current-buffer))
                        (if (or (< p (point-min))
                                (> p (point-max)))
                            (when (y-or-n-p (format "%s is invisible because of narrowing. Widen? " symbol--))
                              (widen)
                              (goto-char p))
                          (goto-char p)))
                    (goto-char p)))))
             ((eq goto-- 'custom)
              (mlinks-custom symbol--))
             (t
              (error "Back goto-- value again: %s" goto--)))))))))

(defun mlinks-elisp-mode-require (module)
  (let ((where mlinks-temp-buffer-where))
    (cond
     ((null where)
      (find-library module))
     ((eq where 'other-window)
      (other-window 1)
      (find-library module))
     ((eq where 'other-frame)
      (make-frame-command)
      (find-library module))
     (t
      (error "Invalid argument, where=%s" where)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Helpers when adopting for modes ;;;;;;;;;;;;;;;;;

;;; Save this, do not delete this comment:

;; (defun mlinks-hit-test ()
;;   "Just a helper function for adding support for new modes."
;;   (let* (
;;          (s0 (if (match-string 0) (match-string 0) ""))
;;          (s1 (if (match-string 1) (match-string 1) ""))
;;          (s2 (if (match-string 2) (match-string 2) ""))
;;          (s3 (if (match-string 3) (match-string 3) ""))
;;          )
;;     (message "match0=%s, match1=%s, match2=%s, match3=%s" s0 s1 s2 s3)))

;; (defun mlinks-handle-reg-fun-list (reg-fun-list)
;;   "Just a helper function."
;;   (let (done
;;         regexp
;;         hitfun
;;         m
;;         p
;;         b
;;         )
;;     (dolist (rh reg-fun-list)
;;       (message "rh=%s" rh);(sit-for 2)
;;       (unless done
;;         (setq regexp (car rh))
;;         (setq hitfun (cadr rh))
;;         (message "regexp=%s, hitfun=%s" regexp hitfun);(sit-for 1)
;;         (when (and (save-match-data
;;                      (setq m (re-search-backward regexp (line-beginning-position) t))
;;                      (> p (match-beginning 0))))
;;           (setq done t)
;;           (setq b (match-beginning 0))
;;           (setq e (match-end 0))
;;           )
;;         (if (not (and b e
;;                       (< b p)
;;                       (< p e)))
;;             (message "MLinks Mode did not find any link here")
;;           (goto-char b)
;;           (if (not (looking-at regexp))
;;               (error "Internal error, regexp %s, no match looking-at" regexp)
;;             (let ((last (car mlinks-places))
;;                   (m (make-marker)))
;;               (set-marker m (line-beginning-position))
;;               (when (or (not last)
;;                         (/= m last))
;;                 (setq mlinks-places (cons m mlinks-places))))
;;             (funcall hitfun))
;;           )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font Lock use

(defvar mlinks-link-update-pos-max nil)
(make-variable-buffer-local 'mlinks-link-update-pos-max)
(put 'mlinks-link-update-pos-max 'permanent-local t)

(defun mlinks-remove-font-lock ()
  "Remove info from font-lock."
  (when (mlinks-want-font-locking)
    (mlink-font-lock nil)))

(defun mlinks-add-font-lock ()
  "Add info to font-lock."
  (when (mlinks-want-font-locking)
    (mlink-font-lock t)))

(defun mlinks-want-font-locking ()
  (or (mlinks-get-mode-value 'fontify)
      (mlinks-get-mode-value 'next-mark)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font Lock integration

(defun mlink-font-lock (on)
  (let* ((add-or-remove (if on 'font-lock-add-keywords 'font-lock-remove-keywords))
         (fontify-fun (car (mlinks-get-mode-value 'fontify)))
         (args (list nil `(( ,fontify-fun ( 0 mlinks-font-lock-face t ))))))
    (when fontify-fun
      ;; Note: Had a lot of trouble with this which I modelled first
      ;; after dlink. Using hi-lock as a model made it work with
      ;; mumamo too.
      ;;
      ;; Next arg, HOW, is needed to get it to work with mumamo. This
      ;; adds it last, like hi-lock.
      (when on (setq args (append args (list t))))
      (apply add-or-remove args)
      (font-lock-mode -1)
      (font-lock-mode 1))))

(defun mlinks-html-fontify (bound)
  (mlinks-fontify bound mlinks-html-link-regexp 1))

(defun mlinks-fontify (bound regexp border)
  (let ((start (point))
        end-start
        stop next-stop
        (more t)
        old-beg old-end
        (wn 1)
        ret)
    ;; Note: we shouldnot use save-match-data here. Instead
    ;; set-match-data is called below!
    (if (not (re-search-forward regexp bound t))
        (setq end-start bound)
      (setq ret t)
      (setq end-start (- (point) 2))
      (let* ((which (if (match-beginning 1) 1 2))
             (beg (+ (match-beginning which) border))
             (end (- (match-end which) border)))
        (put-text-property beg end 'mlinks-link t)
        (set-match-data (list (copy-marker end) (copy-marker beg)))))
    (setq stop start)
    (setq next-stop -1)
    (while (and (> 100 (setq wn (1+ wn)))
                (setq next-stop (next-single-char-property-change stop 'mlinks-link nil end-start))
                (/= next-stop stop))
      (setq stop next-stop)
      (if (get-text-property stop 'mlinks-link)
          (setq old-beg stop)
        (when old-beg
          (remove-list-of-text-properties old-beg stop '(mlinks-link 'mouse-face)))))
    ret))

(defun mlinks-next-link ()
  "Find next link, fontify as necessary."
  (let* ((here (point))
         (prev-pos (point))
         (fontified-here (get-text-property (max (point-min) (1- prev-pos)) 'fontified))
         (fontified-to (next-single-char-property-change prev-pos 'fontified))
         (pos (next-single-char-property-change prev-pos 'mlinks-link nil
                                                (or fontified-to (point-max))))
         (fontified-all (and fontified-here (not fontified-to)))
         ready
         next-fontified-to)
    (while (not (or ready
                    (and fontified-all
                         (not pos))))
      (if pos
          (progn
            (unless (get-text-property pos 'mlinks-link)
              ;; Get to next link
              (setq prev-pos pos)
              (setq pos (next-single-char-property-change prev-pos 'mlinks-link nil
                                                          (or fontified-to (point-max)))))
            (when pos
              (setq ready (get-text-property pos 'mlinks-link))
              (setq prev-pos pos)
              (unless ready (setq pos nil))))
        (unless (or fontified-all fontified-to)
          (if (get-text-property prev-pos 'fontified)
              (setq fontified-all
                    (not (setq fontified-to
                               (next-single-char-property-change prev-pos 'fontified))))
            (setq fontified-to ( or (previous-single-char-property-change prev-pos 'fontified)
                                    1))))
        (setq next-fontified-to (min (+ fontified-to 5000)
                                     (point-max)))
        (mumamo-with-buffer-prepared-for-jit-lock
         (progn
           (put-text-property fontified-to next-fontified-to 'fontified t)
           (font-lock-fontify-region fontified-to next-fontified-to)))
        (setq fontified-to (next-single-char-property-change (1- next-fontified-to)
                                                             'fontified))
        (setq fontified-all (not fontified-to))
        (setq pos (next-single-char-property-change prev-pos 'mlinks-link nil
                                                    (or fontified-to (point-max))))))
    (when ready prev-pos)))

(defun mlinks-prev-link ()
  "Find previous link, fontify as necessary."
  (let* ((prev-pos (point))
         (fontified-from (previous-single-char-property-change prev-pos 'fontified))
         (fontified-here (get-text-property (max (point-min) (1- prev-pos)) 'fontified))
         (fontified-all (and fontified-here (not fontified-from)))
         (pos (when fontified-here
                (previous-single-char-property-change prev-pos 'mlinks-link nil
                                                      (or fontified-from 1))))
         ready
         next-fontified-from)
    (while (not (or ready
                    (and fontified-all
                         (not pos))))
      (assert (numberp prev-pos) t)
      (if pos
          (progn
            (when (and (> (1- pos) (point-min))
                       (get-text-property (1- pos) 'mlinks-link))
              ;; Get out of current link
              (setq prev-pos pos)
              (setq pos (previous-single-char-property-change prev-pos 'mlinks-link nil
                                                              (or fontified-from 1))))
            (when pos
              (setq prev-pos pos)
              (setq ready (and (get-text-property pos 'fontified)
                               (or (= 1 pos)
                                   (not (get-text-property (1- pos) 'mlinks-link)))
                               (get-text-property pos 'mlinks-link)))
              (unless ready (setq pos nil))))
        (setq next-fontified-from (max (- fontified-from 5000)
                                       (point-min)))
        (mumamo-with-buffer-prepared-for-jit-lock
         (progn
           (put-text-property next-fontified-from fontified-from 'fontified t)
           (font-lock-fontify-region next-fontified-from fontified-from)))
        (setq fontified-from (previous-single-char-property-change
                              (1+ next-fontified-from) 'fontified))
        (setq fontified-all (not fontified-from))
        (setq pos (previous-single-char-property-change prev-pos 'mlinks-link nil
                                                        (or fontified-from 1)))))
    (when ready pos)))


;;; This is for the problem reported by some Asian users:
;;;
;;;   Lisp error: (invalid-read-syntax "] in a list")
;;;
;; Local Variables:
;; coding: utf-8
;; End:

(provide 'mlinks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mlinks.el ends here

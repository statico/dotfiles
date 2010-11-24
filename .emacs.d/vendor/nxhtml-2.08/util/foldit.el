;;; foldit.el --- Helpers for folding
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-08-10 Mon
;; Version:
;; Last-Updated:
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
;; Defines `foldit-mode' which puts visual clues on hidden regions.
;; Does not do any folding itself but works with `outline-minor-mode'
;; and `hs-minor-mode'.
;;
;; Fix-me: reveal-mode does not work with this and I have no idea why
;; ...
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

;; Fix-me: start-tag-beg/start-tag-end are workarounds for smaller
;; bugs in hs-minor-mode and outline-minor-mode. Maybe try to fix
;; them... - but there are a whole bunch of other invisibilty related
;; bugs that ought to be fixed first since otherwise it is impossible
;; to know where point goes after hiding/unhiding.

(eval-when-compile (require 'cl))
(eval-when-compile (require 'hideshow))
(eval-when-compile (require 'mumamo nil t))
(eval-when-compile (require 'outline))

(defsubst foldit-overlay-priority ()
  (1+ (or (and (boundp 'mlinks-link-overlay-priority)
               mlinks-link-overlay-priority)
          100)))

;;;###autoload
(defgroup foldit nil
  "Customization group for foldit folding helpers."
  :group 'nxhtml)

(defvar foldit-temp-at-point-ovl nil)
(make-variable-buffer-local 'foldit-temp-at-point-ovl)

;;;###autoload
(define-minor-mode foldit-mode
  "Minor mode providing visual aids for folding.
Shows some hints about what you have hidden and how to reveal it.

Supports `hs-minor-mode', `outline-minor-mode' and major modes
derived from `outline-mode'."
  :lighter nil
  (if foldit-mode
      (progn
        ;; Outline
        (add-hook 'outline-view-change-hook 'foldit-outline-change nil t)
        ;; Add our overlays
        (when (or (and (boundp 'outline-minor-mode) outline-minor-mode)
                  ;; Fix-me: mumamo
                  (derived-mode-p 'outline-mode)) (foldit-outline-change))
        ;; hs
        (unless (local-variable-p 'hs-set-up-overlay)
          (set (make-local-variable 'hs-set-up-overlay) 'foldit-hs-set-up-overlay))
        ;; Add our overlays
        (when (or (and (boundp 'hs-minor-mode) hs-minor-mode))
          (save-restriction
            (widen)
            (let (ovl)
              (dolist (ovl (overlays-in (point-min) (point-max)))
                (when (eq (overlay-get ovl 'invisible) 'hs)
                  (funcall hs-set-up-overlay ovl)))))))
    ;; Outline
    (remove-hook 'outline-view-change-hook 'foldit-outline-change t)
    ;; hs
    (when (and (local-variable-p 'hs-set-up-overlay)
               (eq hs-set-up-overlay 'foldit-hs-set-up-overlay))
      (kill-local-variable 'hs-set-up-overlay))
    ;; Remove our overlays
    (save-restriction
      (widen)
      (let (ovl prop)
        (dolist (ovl (overlays-in (point-min) (point-max)))
          (when (setq prop (overlay-get ovl 'foldit))
            (case prop
              ;;('display (overlay-put ovl 'display nil))
              ('foldit (delete-overlay ovl))
              (t (delete-overlay ovl))
              )))))))

(defcustom foldit-avoid '(org-mode)
  "List of major modes to avoid."
  :group 'foldit)

;;;###autoload
(define-globalized-minor-mode foldit-global-mode foldit-mode
  (lambda () (foldit-mode 1))
  :group 'foldit)

(defun foldit-hidden-line-str (hidden-lines type)
  "String to display for hidden lines.
HIDDEN-LINES are the number of lines and TYPE is a string
indicating how they were hidden."
  (propertize (format " ...(%d %slines)" hidden-lines type)
              'face 'shadow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline

(defvar foldit-outline-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'foldit-outline-show-entry)
    (define-key map [down-mouse-1] 'foldit-outline-show-entry)
    (define-key map [S-tab]   'mlinks-backward-link)
    (define-key map [tab]     'mlinks-forward-link)
    (define-key map "\t"      'mlinks-forward-link)
    map))

(defun foldit-outline-change ()
  "Check outline overlays.
Run this in `outline-view-change-hook'."
  ;; We get the variables FROM and TO here from `outline-flag-region'
  ;; so let us use them. But O is hidden...
  (let* (from
         to
         num-lines
         ovl
         (tag ""))
    (cond
     ((and (boundp 'start)
           start
           (boundp 'end)
           end)
      (setq from start)
      (setq to   end))
     (t
      (setq from (point-min))
      (setq to   (point-max))))
    (dolist (ovl (overlays-in from to))
      (when (eq (overlay-get ovl 'invisible) 'outline)
        (setq num-lines (count-lines (overlay-start ovl) (overlay-end ovl)))
        (overlay-put ovl 'display (concat
                                   (propertize "+" 'face 'mode-line)
                                   ""
                                   tag (foldit-hidden-line-str num-lines "")))
        (overlay-put ovl 'foldit 'display) ;; Should be a list...
        (overlay-put ovl 'keymap foldit-outline-keymap)
        (overlay-put ovl 'face 'lazy-highlight)
        (overlay-put ovl 'mouse-face 'highlight)
        (overlay-put ovl 'help-echo "Press RET to show hidden part")
        (overlay-put ovl 'mlinks-link t)
        (overlay-put ovl 'priority (foldit-overlay-priority))
        (mumamo-with-buffer-prepared-for-jit-lock
         (let* ((start-tag-beg (overlay-start ovl))
                (start-tag-end start-tag-beg))
           (put-text-property start-tag-beg (+ start-tag-beg 1)
                              'foldit-tag-end (copy-marker start-tag-end))))
        ))))

(defvar foldit-outline-hide-again-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'foldit-outline-hide-again)
    (define-key map [down-mouse-1] 'foldit-outline-hide-again)
    (define-key map [S-tab]   'mlinks-backward-link)
    (define-key map [tab]     'mlinks-forward-link)
    (define-key map "\t"      'mlinks-forward-link)
    map))

(defun foldit-outline-show-entry ()
  "Show hidden entry."
  (interactive)
  (let ((tag-end (get-text-property (point) 'foldit-tag-end)))
    (show-entry)
    (mumamo-with-buffer-prepared-for-jit-lock
     (set-text-properties (point) (+ (point) 2) 'foldit-tag-end))
    (when tag-end (goto-char tag-end))
    (foldit-add-temp-at-point-overlay "-"
                                      foldit-outline-hide-again-keymap
                                      "Press RET to hide again")))

(defun foldit-outline-hide-again ()
  "Hide entry again."
  (interactive)
  (when (overlayp foldit-temp-at-point-ovl)
    (delete-overlay foldit-temp-at-point-ovl))
  (hide-entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hide/Show

(defvar foldit-hs-start-tag-end-func 'foldit-hs-default-start-tag-end)
(make-variable-buffer-local 'foldit-hs-start-tag-end-func)
(put 'foldit-hs-start-tag-end-func 'permanent-local t)

(defun foldit-hs-default-start-tag-end (beg)
  "Find end of hide/show tag beginning at BEG."
  (min (+ beg 65)
       (save-excursion
         (goto-char beg)
         (line-end-position))))

(defvar foldit-hs-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'foldit-hs-show-block)
    (define-key map [down-mouse-1] 'foldit-hs-show-block)
    (define-key map [S-tab]   'mlinks-backward-link)
    (define-key map [tab]     'mlinks-forward-link)
    (define-key map "\t"      'mlinks-forward-link)
    map))

(defvar foldit-hs-hide-again-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'foldit-hs-hide-again)
    (define-key map [down-mouse-1] 'foldit-hs-hide-again)
    (define-key map [S-tab]   'mlinks-backward-link)
    (define-key map [tab]     'mlinks-forward-link)
    (define-key map "\t"      'mlinks-forward-link)
    map))

(defun foldit-hs-set-up-overlay (ovl)
  "Set up overlay OVL for hide/show."
  (let* ((num-lines (count-lines (overlay-start ovl) (overlay-end ovl)))
         (here (point))
         (start-tag-beg (overlay-start ovl))
         (start-tag-end (funcall foldit-hs-start-tag-end-func start-tag-beg))
         (tag (buffer-substring start-tag-beg start-tag-end)))
    (goto-char here)
    ;;(overlay-put ovl 'isearch-open-invisible t)
    (overlay-put ovl 'display (concat
                               (propertize "+" 'face 'mode-line)
                               " "
                               tag (foldit-hidden-line-str num-lines "h")))
    (overlay-put ovl 'foldit 'display)
    (overlay-put ovl 'keymap foldit-hs-keymap)
    (overlay-put ovl 'face 'next-error)
    (overlay-put ovl 'face 'lazy-highlight)
    (overlay-put ovl 'mouse-face 'highlight)
    (overlay-put ovl 'help-echo "Press RET to show hidden part")
    (overlay-put ovl 'mlinks-link t)
    (overlay-put ovl 'priority (foldit-overlay-priority))
    (mumamo-with-buffer-prepared-for-jit-lock
     (put-text-property start-tag-beg (+ start-tag-beg 1)
                        'foldit-tag-end (copy-marker start-tag-end)))))

(defun foldit-hs-show-block ()
  "Show hidden block."
  (interactive)
  (let ((tag-end (get-text-property (point) 'foldit-tag-end)))
    (hs-show-block)
    (mumamo-with-buffer-prepared-for-jit-lock
     (set-text-properties (point) (+ (point) 2) 'foldit-tag-end))
    (when tag-end (goto-char tag-end))
    (foldit-add-temp-at-point-overlay "-"
                                      foldit-hs-hide-again-keymap
                                    "Press RET to hide again")))

(defun foldit-hs-hide-again ()
  "Hide hide/show block again."
  (interactive)
  (when (overlayp foldit-temp-at-point-ovl)
    (delete-overlay foldit-temp-at-point-ovl))
  (hs-hide-block))


;;; Fix-me: break out this
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun foldit-add-temp-at-point-overlay (marker keymap msg)
  "Add a temporary overlay with a marker MARKER and a keymap KEYMAP.
The overlay is also given the help echo MSG.

This overlay is removed as soon as point moves from current point."
  (let ((ovl (make-overlay (point) (1+ (point))))
        (real (buffer-substring (point) (1+ (point)))))
    (overlay-put ovl 'isearch-open-invisible t)
    (overlay-put ovl 'display (concat
                               (propertize marker 'face 'mode-line)
                               " "
                               msg
                               real))
    (overlay-put ovl 'foldit 'foldit)
    (overlay-put ovl 'keymap keymap)
    (overlay-put ovl 'face 'lazy-highlight)
    (overlay-put ovl 'mouse-face 'highlight)
    (overlay-put ovl 'help-echo msg)
    (overlay-put ovl 'mlinks-link t)
    (overlay-put ovl 'priority (foldit-overlay-priority))
    (setq foldit-temp-at-point-ovl ovl)
    (add-hook 'post-command-hook
              'foldit-remove-temp-at-point-overlay
              nil t)))

(defun foldit-remove-temp-at-point-overlay ()
  "Remove overlay made by `foldit-add-temp-at-point-overlay'."
  (condition-case err
      (unless (and foldit-temp-at-point-ovl
                   (overlay-buffer foldit-temp-at-point-ovl)
                   (= (overlay-start foldit-temp-at-point-ovl)
                      (point)))
        (delete-overlay foldit-temp-at-point-ovl)
        (setq foldit-temp-at-point-ovl nil)
        (remove-hook 'post-command-hook 'foldit-remove-temp-at-point-overlay t)
        )
    (error (message "foldit-remove-temp-at-point-overlay: %s"
                    (propertize (error-message-string err))))))
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



;; (defun put-before-on-invis ()
;;   (let* (o
;;          (io (catch 'io
;;                (dolist (o (overlays-at (1+ (point))))
;;                  (when (overlay-get o 'invisible)
;;                    (throw 'io o)))))
;;          (str (propertize "IOSTRING"
;;                           'face 'secondary-selection
;;                           )))
;;     (overlay-put io 'before-string str)
;;     ;;(overlay-put io 'display "display")
;;     (overlay-put io 'display nil)
;;     ;;(overlay-put io 'after-string "AFTER")
;;     ))

(provide 'foldit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foldit.el ends here

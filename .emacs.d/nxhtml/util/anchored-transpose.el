;;; anchored-transpose.el --- Transposes a phrase around an anchor phrase

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: tools convenience

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;;; Commentary:

;; `anchored-transpose' is an interactive autoload function to transpose
;; portions of a region around an anchor phrase.  In other words it swaps
;; two regions.
;;
;; See C-h f anchored-transpose <ret> for a complete description.

;;; Installing:

;; 1) Put anchored-transpose.el on your load path.
;; 2) Put the following 2 lines in your .emacs
;;    (global-set-key [?\C-x ?t] 'anchored-transpose) ;; Just a suggestion...
;;    (autoload 'anchored-transpose "anchored-transpose" nil t)

;;; History:

;; 2004-09-24 RGB Seems useable enough to release.
;; 2004-10-15 RGB Only comments and doc strings were updated.
;; 2004-10-22 RGB Added support for 2 phrase selection.
;; 2004-12-01 RGB Added secondary selection support.
;; 2005-07-21 RGB Updated help text and comments.
;;                Added support for A  C B  D and C  A D  B selection.
;;                Fixed bug affecting multi line selections.
;; 2005-09-28 RGB Allow swapping regions with no anchor text between.

;; Changes by Lennart Borgman
;; 2009-11-25 LB  Set and clear secondary selection from keyboard.
;;                Always use secondary selection.
;;                Keep selections right after swapping.
;;                Clear them if not used again.
;;                Swap between buffers.
;;                Check for read-only.
;;                Probably broke something... ;-)

;;; Code:

(defvar anchored-transpose-anchor ()
  "begin/end when `anchored-transpose' is in progress else nil")

;;;###autoload
(defun anchored-transpose (beg1 end1 flg1 &optional beg2 end2 flg2 win2)
  "Transpose portions of the region around an anchor phrase.

`this phrase but not that word'    can be transposed into
`that word but not this phrase'

I want this phrase but not that word.
       |----------------------------|. .This is the entire phrase.
                  |-------|. . . . . . .This is the anchor phrase.

First select the entire phrase and type \\[anchored-transpose].
This set the secondary selection.

Then select the anchor phrase and type \\[anchored-transpose]
again.  Alternatively you can do the selections like this:

I want this phrase but not that word.
       |----------|       |---------|   Separate phrase selection.

By default the anchor phrase will automatically include
any surrounding whitespace even if you don't explicitly select
it.  Also, it won't include certain trailing punctuation.  See
`anchored-transpose-do-fuzzy' for details.  A prefix arg prior to
either selection means `no fuzzy logic, use selections
literally'.

You can select the regions to be swapped separately in any
order.

After swapping both primary and secondary selection are still
active.  They will be canceled after second next command if you
do not swap regions again.  \(Second because this allow you to
adjust the regions and try again.)

You can also swap text between different buffers this way.

Typing \\[anchored-transpose] with nothing selected clears any
prior selection, ie secondary selection."
  (interactive `(,(region-beginning) ,(region-end)
                 ,current-prefix-arg
                 ,@anchored-transpose-anchor))
  (setq anchored-transpose-anchor nil)
  (when (and mouse-secondary-overlay
             mark-active
             (overlay-buffer mouse-secondary-overlay)
             (/= (overlay-start mouse-secondary-overlay)
                 (overlay-end mouse-secondary-overlay)))
    (if (eq (overlay-buffer mouse-secondary-overlay) (current-buffer))
        (progn
          (setq beg2 (overlay-start mouse-secondary-overlay))
          (setq end2 (overlay-end mouse-secondary-overlay))
          (setq flg2 flg1)
          (delete-overlay mouse-secondary-overlay))
      (let* ((sec-buf (overlay-buffer mouse-secondary-overlay))
             (sec-win (get-buffer-window sec-buf))
             (sec-new nil))
        (unless sec-win
          (setq sec-new t)
          (setq sec-win (split-window)))
        (with-selected-window sec-win
          (set-window-buffer (selected-window) sec-buf)
          (goto-char (overlay-start mouse-secondary-overlay)))
        (if (not (y-or-n-p "Swap between buffers "))
            (when sec-new (delete-window sec-win))
          (setq beg2 (overlay-start mouse-secondary-overlay))
          (setq end2 (overlay-end mouse-secondary-overlay))
          (setq flg2 flg1)
          (setq win2 sec-win)))))
  (setq win2 (or win2 (selected-window)))
  (if mark-active
      (if end2                     ; then both regions are marked.  swap them.
          (if (not (eq win2 (selected-window)))
              (anchored-transpose-swap beg1 end1 beg2 end2 win2)
            (if (and (< beg1 beg2)        ;A  C B  D
                     (< end1 end2)
                     (> end1 beg2))
                (apply 'anchored-transpose-swap
                       (anchored-transpose-do-fuzzy
                        beg1 beg2 end1 end2 flg1 flg2 flg1 flg2))
              (if (and (> beg1 beg2)      ;C  A D  B
                       (> end1 end2)
                       (> end2 beg1))
                  (apply 'anchored-transpose-swap
                         (anchored-transpose-do-fuzzy
                          beg2 beg1 end2 end1 flg2 flg1 flg2 flg1))
                (if (and (< beg1 beg2)    ;A  C D  B
                         (> end1 end2))
                    (apply 'anchored-transpose-swap
                           (anchored-transpose-do-fuzzy
                            beg1 beg2 end2 end1 flg1 flg2 flg2 flg1))
                  (if (and (> beg1 beg2)  ;C  A B  D
                           (< end1 end2))
                      (apply 'anchored-transpose-swap
                             (anchored-transpose-do-fuzzy
                              beg2 beg1 end1 end2 flg2 flg1 flg1 flg2))
                    (if (<= end1 beg2)    ;A B  C D
                        (apply 'anchored-transpose-swap
                               (anchored-transpose-do-fuzzy
                                beg1 end1 beg2 end2 flg1 flg1 flg2 flg2))
                      (if (<= end2 beg1)  ;C D A B
                          (apply 'anchored-transpose-swap
                                 (anchored-transpose-do-fuzzy
                                  beg2 end2 beg1 end1 flg2 flg2 flg1 flg1))
                        (error "Regions have invalid overlap"))))))))
        ;; 1st of 2 regions.  Save it and wait for the other.
        ;;(setq anchored-transpose-anchor (list beg1 end1 flg1))
        (if (or buffer-read-only
                (get-char-property beg1 'read-only)
                (get-char-property end1 'read-only))
            ;; Fix-me: move test, clean up a bit.
            (message "Buffer text is readonly")
          (set-secondary-selection beg1 end1)
          (setq deactivate-mark t)
          (message "%s" (this-command-keys))
          (message (propertize "Transpose: Select second region and call again - (without selection to cancel)"
                               'face 'secondary-selection))))
    (if (and mouse-secondary-overlay
             (overlay-buffer mouse-secondary-overlay))
        (progn
          (cancel-secondary-selection)
          (message (propertize "Canceled secondary selection" 'face
                               'highlight)))
      (message (propertize "Command requires a marked region" 'face
                           'highlight)))))

;;;###autoload
(defun set-secondary-selection (beg end)
  "Set the secondary selection to the current region.
This must be bound to a mouse drag event."
  (interactive "r")
  (move-overlay mouse-secondary-overlay beg end (current-buffer))
  (when (called-interactively-p 'interactive)
    ;;(deactivate-mark)
    )
  (x-set-selection
   'SECONDARY
   (buffer-substring (overlay-start mouse-secondary-overlay)
                     (overlay-end mouse-secondary-overlay))))

;;;###autoload
(defun cancel-secondary-selection ()
  (interactive)
  (delete-overlay mouse-secondary-overlay)
  (x-set-selection 'SECONDARY nil))

(defun anchored-transpose-do-fuzzy (r1beg r1end r2beg r2end
                                          lit1 lit2 lit3 lit4)
  "Returns the first 4 arguments after adjusting their value if necessary.

I want this phrase but not that word.
       |----------------------------|. .This is the entire phrase.
                  |-------|. . . . . . .This is the anchor phrase.
     R1BEG      R1END   R2BEG     R2END

R1BEG and R1END define the first region and R2BEG and R2END the second.

The flags, LIT1 thru LIT4 indicate if fuzzy logic should be applied to the
beginning of R1BEG, the end of R1END, the beginning of R2BEG, the end of R2END
respectively.  If any flag is nil then fuzzy logic will be applied.  Otherwise
the value passed should be returned LITerally (that is, unchanged).

See `anchored-transpose-fuzzy-begin' and `anchored-transpose-fuzzy-end' for
specifics on what adjustments these routines will make when LITx is nil."
  (list
   (if lit1 r1beg
     (anchored-transpose-fuzzy-begin r1beg r1end "[\t ]+"))
   (if lit2 r1end
     (anchored-transpose-fuzzy-end   r1beg r1end "\\s +"))
   (if lit3 r2beg
     (anchored-transpose-fuzzy-begin r2beg r2end "[\t ]+"))
   (if lit4 r2end
     (anchored-transpose-fuzzy-end   r2beg r2end "\\s *[.!?]"))
   nil))

(defun anchored-transpose-fuzzy-end (beg end what)
  "Returns END or new value for END based on the regexp WHAT.
BEG and END are buffer positions defining a region.  If that region ends
with WHAT then the value for END is adjusted to exclude that matching text.

NOTE: The regexp is applied differently than `looking-back' applies a regexp.

Example: if (buffer-string beg end) contains `1234' the regexp `432' matches
it, not `234' as `looking-back' would.  Also, your regexp never sees the char
at BEG so the match will always leave at least 1 character to transpose.
The reason for not using looking-back is that it's not greedy enough.
\(looking-back \" +\") will only match one space no matter how many exist."
  (let ((str (concat
              (reverse (append (buffer-substring (1+ beg) end) nil)))))
    (if (string-match (concat "`" what) str)
        (- end (length (match-string 0 str)))
      end)))

(defun anchored-transpose-fuzzy-begin (beg end what)
  "Returns BEG or a new value for BEG based on the regexp WHAT.
BEG and END are buffer positions defining a region.  If the region begins
with WHAT then BEG is adjusted to exclude the matching text.

NOTE: Your regexp never sees the last char defined by beg/end.  This insures
at least 1 char is always left to transpose."
  (let ((str (buffer-substring beg (1- end))))
    (if (string-match (concat "`" what) str)
        (+ beg (length (match-string 0 str)))
      beg)))

(defun anchored-transpose-swap (r1beg r1end r2beg r2end win2)
  "Swaps region r1beg/r1end with r2beg/r2end. Flags are currently ignored.
Point is left at r1end."
  (let ((reg1 (buffer-substring r1beg r1end))
        (reg2 nil)
        (old-buffer (current-buffer)))
    (when win2
      (unless (eq (selected-window) win2)
        (select-window win2)
        (set-buffer (window-buffer (selected-window)))))
    (setq reg2 (delete-and-extract-region r2beg r2end))
    (goto-char r2beg)
    (let ((new-mark (point)))
      (insert reg1)
      (push-mark new-mark))
    ;; I want to leave point at the end of phrase 2 in current buffer.
    (save-excursion
      (with-current-buffer old-buffer
        (goto-char r1beg)
        (delete-region r1beg r1end)
        (let ((here (point)))
          (insert reg2)
          (set-secondary-selection here (point)))))
    (setq deactivate-mark nil)
    (when (eq old-buffer (current-buffer))
      (add-hook 'post-command-hook 'anchored-swap-post-command t t))))

(defun anchored-swap-post-command ()
  (condition-case err
      (unless mark-active
        (cancel-secondary-selection)
        (remove-hook 'post-command-hook 'anchored-swap-post-command t))
    (error (message "anchored-swap-post-command: %s" err))))

(provide 'anchored-transpose)

;; Because I like it this way.  So there!
;;; fill-column:78 ***
;;; emacs-lisp-docstring-fill-column:78 ***
;;;
;;; Local Variables: ***
;;; End: ***
;;; anchored-transpose.el ends here.

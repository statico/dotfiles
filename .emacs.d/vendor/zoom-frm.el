;;; zoom-frm.el --- Commands to zoom frame font size.
;;
;; Filename: zoom-frm.el
;; Description: Commands to zoom frame font size.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2011, Drew Adams, all rights reserved.
;; Created: Fri Jan 07 10:24:35 2005
;; Version: 20
;; Last-Updated: Tue Jan  4 15:52:56 2011 (-0800)
;;           By: dradams
;;     Update #: 243
;; URL: http://www.emacswiki.org/cgi-bin/wiki/zoom-frm.el
;; Keywords: frames, extensions, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-cmds', `frame-fns', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Commands to zoom font size.
;;
;;  A few commands are provided for zooming a frame or buffer, so that
;;  its font becomes larger or smaller.  For example, bind `zoom-in'
;;  and `zoom-out' to mouse wheel events to get the zooming effect you
;;  are used to in a Web browser.
;;
;;  With Emacs 20, 21, and 22, you can zoom a frame.  Starting with
;;  Emacs 23, you can zoom either a frame (all buffers shown there) or
;;  a single buffer (in all frames where it is displayed).  User
;;  option `zoom-frame/buffer' determines which kind of zooming (frame
;;  or buffer) is done.  You can customize this option, but you can
;;  also toggle it just by providing a prefix arg (`C-u') to `zoom-in'
;;  or `zoom-out'.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change frame properties.  You
;;    can save any changes you have made, by using Customize.  To
;;    visit a Customize buffer of all unsaved changes you have made,
;;    use command `customize-customized'.
;;
;;    Frame parameter changes, such as font size, can be saved for
;;    future use by all frames or all frames of a certain kind.  For
;;    that, you must change the frame parameters of the correponding
;;    frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the font size of a frame and want to
;;    make that the default font size for new frames in the future.
;;    You will need to update the value of variable
;;    `default-frame-alist' to use the `font' parameter setting of the
;;    changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;
;;  Commands defined here:
;;
;;    `toggle-zoom-frame', `zoom-frm-in', `zoom-frm-out',
;;    `zoom-frm-unzoom', `zoom-in', `zoom-out'.
;;
;;
;;  User options (variables) defined here:
;;
;;    `frame-zoom-font-difference', `zoom-frame/buffer' (Emacs 23+).
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'zoom-frm)
;;
;;  Suggested key bindings:
;;
;;  (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
;;                      (vector (list 'control mouse-wheel-down-event))
;;                    [C-mouse-wheel])    ; Emacs 20, 21
;;                  'zoom-in)
;;  (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
;;    (global-set-key (vector (list 'control mouse-wheel-up-event))
;;                    'zoom-out))
;;
;;  (global-set-key [S-mouse-1]    'zoom-in)
;;  (global-set-key [C-S-mouse-1]  'zoom-out)
;;  ;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
;;  (global-set-key [S-down-mouse-1] nil)
;;
;;  The first two of these mean that in Emacs 22 or later you can hold
;;  the Control key and rotate the wheel to zoom in and out, just as
;;  you do in your Web browser.  In Emacs 20 and 21, however, Control
;;  plus wheeling zooms in, but to zoom out you need to use `C--'
;;  before wheeling with Control.  This is because Emacs 20 and 21 do
;;  not have separate events for the mouse wheel directions, and it is
;;  the prefix arg, not the wheel direction, that determines the
;;  effect.
;;
;;  Note: You can bind the zooming commands to keyboard events, as
;;  well as to mouse events.  You could do this, for instance:
;;
;;  (global-set-key [(control shift ?z)]  'zoom-in)  ; `C-S-z'
;;  (global-set-key [(control ?z)]        'zoom-out) ; `C-z'
;;
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup and defcustom.
;; 2010/07/06 dadams
;;     zoom-(in|out): Put doc strings before interactive spec.  Thx to Yidong Chong.
;; 2009/06/11 dadams
;;     Added buffer zooming, for Emacs 23.
;;       Added zoom-(in|out), group zoom, zoom-frame/buffer.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     frame-zoom-font-difference: Changed :group to Frame-Commands. Added :link.
;; 2005/01/18 dadams
;;     Changed default value of frame-zoom-font-difference.
;;     Added Note on saving changes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'frame-cmds) ;; enlarge-font

;;;;;;;;;;;;;;;;;;;;;;;;




;;; USER OPTIONS (VARIABLES) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup zoom nil
  "Zoom a frame or buffer."
  :group 'frames :group 'Frame-Commands ; Defined in `frame-cmds.el'.
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zoom-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/zoom-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/SetFonts#ChangingFontSize")
  :link '(emacs-commentary-link :tag "Commentary" "zoom-frm"))

;;;###autoload
(defcustom frame-zoom-font-difference 1
  "*Number of points to change the frame font size when zooming
using commands `zoom-frm-in' and `zoom-frm-out'.
The absolute value of this must be less than the current font size,
since the new font size cannot be less than 1 point."
  :type 'integer :group 'zoom)

(when (> emacs-major-version 22)
  (defcustom zoom-frame/buffer 'frame
    "*What to zoom: current frame or current buffer.
See commands `zoom-in' and `zoom-out'."
    :type '(choice (const :tag "Zoom frame"  frame) (const :tag "Zoom buffer" buffer))
    :group 'zoom))


;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (> emacs-major-version 22) (defalias 'zoom-in 'zoom-frm-in))
;;;###autoload
(defun zoom-frm-in (&optional frame flip)
  "Zoom FRAME in by `frame-zoom-font-difference', making text larger.
If `frame-zoom-font-difference' is negative, make text smaller.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text smaller.
This is equal but opposite to `zoom-frm-out'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame (or frame (selected-frame)))
  (let ((zoom-factor (frame-parameter frame 'zoomed))
        (increment (if flip (- frame-zoom-font-difference) frame-zoom-font-difference)))
    (unless zoom-factor (setq zoom-factor 0))
    (setq zoom-factor (+ zoom-factor increment))
    (enlarge-font increment frame)
    (modify-frame-parameters frame (list (cons 'zoomed zoom-factor)))))

(unless (> emacs-major-version 22) (defalias 'zoom-out 'zoom-frm-out))
;;;###autoload
(defun zoom-frm-out (&optional frame flip)
  "Zoom FRAME out by `frame-zoom-font-difference'.
If `frame-zoom-font-difference' is negative, make text larger.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text larger.
This is equal but opposite to `zoom-frm-in'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame (or frame (selected-frame)))
  (let ((frame-zoom-font-difference (- frame-zoom-font-difference)))
    (zoom-frm-in frame flip)))

;;;###autoload
(defun zoom-frm-unzoom (&optional frame)
  "Cancel zoom of FRAME."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (let ((zoom-factor (frame-parameter frame 'zoomed)))
    (if (not zoom-factor)
        (error "Frame is not zoomed")
      (enlarge-font (- zoom-factor) frame)
      (modify-frame-parameters frame '((zoomed))))))

;;;###autoload
(defun toggle-zoom-frame (&optional frame)
  "Alternately zoom/unzoom FRAME by `frame-zoom-font-difference'."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (if (frame-parameter frame 'zoomed)
      (zoom-frm-unzoom frame)
    (zoom-frm-in frame)))

(when (> emacs-major-version 22)
  (defun zoom-in (arg)
    "Zoom current frame or buffer in.
With a prefix arg, toggle between zooming frame and zooming buffer.
Frame zooming uses command `zoom-frm-in'. 
Buffer zooming uses command `text-scale-increase'."
    (interactive "P")
    (when arg
      (setq zoom-frame/buffer  (if (eq zoom-frame/buffer 'frame) 'buffer 'frame)))
    (if (eq zoom-frame/buffer 'frame)
        (zoom-frm-in)
      (with-current-buffer
          (if (string-match "mouse" (format "%S" (event-basic-type
                                                  last-command-event)))
              (window-buffer (posn-window (event-start last-command-event)))
            (current-buffer))
        (text-scale-increase 1))))

  (defun zoom-out (arg)
    "Zoom current frame or buffer out.
With a prefix arg, toggle between zooming frame and zooming buffer.
Frame zooming uses command `zoom-frm-out'. 
Buffer zooming uses command `text-scale-decrease'."
    (interactive "P")
    (when arg
      (setq zoom-frame/buffer  (if (eq zoom-frame/buffer 'frame) 'buffer 'frame)))
    (if (eq zoom-frame/buffer 'frame)
        (zoom-frm-out)
      (with-current-buffer
          (if (string-match "mouse" (format "%S" (event-basic-type
                                                  last-command-event)))
              (window-buffer (posn-window (event-start last-command-event)))
            (current-buffer))
        (text-scale-decrease 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zoom-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zoom-frm.el ends here

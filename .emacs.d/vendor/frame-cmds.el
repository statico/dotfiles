;;; frame-cmds.el --- Frame and window commands (interactive functions).
;;
;; Filename: frame-cmds.el
;; Description: Frame and window commands (interactive functions).
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 16:30:45 1996
;; Version: 21.0
;; Last-Updated: Tue Oct 19 21:18:24 2010 (-0700)
;;           By: dradams
;;     Update #: 2634
;; URL: http://www.emacswiki.org/cgi-bin/wiki/frame-cmds.el
;; Keywords: internal, extensions, mouse, frames, windows, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-fns', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Frame and window commands (interactive functions).
;;
;;
;;  Summary:
;;
;;    Load this library from your init file (~/.emacs or _emacs).
;;    Add the suggested key bindings (below) to  your init file.
;;    Use `M-up|down|left|right' to move frames around.
;;    Use `C-M-up|down|left|right' to resize frames.
;;    Use `C-M-z' or `C-x C-z' to iconify/hide all frames.
;;    Use `C-M-z' in a lone frame to restore all frames.
;;    Use `C-mouse-1' in the minibuffer to restore all frames.
;;    Use `C-mouse-1' in Dired to mark/unmark a file.
;;    Use `C-mouse-3' on the mode line to remove window from frame.
;;    Use `tile-frames-horizontally', `-vertically' to tile frames.
;;    Use `C-x o' to select `other-window' or `other-frame'.
;;
;;  Commands to incrementally resize frames are `enlarge-frame' and
;;  `enlarge-frame-horizontally'.  Sarir Khamsi
;;  [sarir.khamsi@raytheon.com] originally wrote `enlarge-frame',
;;  which he called `sk-grow-frame'.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change frame properties.
;;    You can save any changes you have made, by using Customize.
;;    To visit a Customize buffer of all unsaved changes you have
;;    made, use command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain
;;    kind.  For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined here.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future.  You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined here.
;;
;;  NOTE: If you also use library `fit-frame.el', and you are on MS
;;  Windows, then load that library before `frame-cmds.el'.  The
;;  commands `maximize-frame' and `restore-frame' defined here are
;;  more general and non-Windows-specific than the commands of the
;;  same name defined in `fit-frame.el'.
;;
;;
;;  User options defined here:
;;
;;    `available-screen-pixel-bounds', `enlarge-font-tries',
;;    `frame-config-register', `frame-parameters-to-exclude',
;;    `move-frame-wrap-within-display-flag'
;;    `rename-frame-when-iconify-flag', `show-hide-show-function',
;;    `window-mgr-title-bar-pixel-height'.
;;
;;  Commands defined here:
;;
;;    `delete-1-window-frames-on', `delete/iconify-window',
;;    `delete/iconify-windows-on', `delete-other-frames',
;;    `delete-windows-for', `enlarge-font', `enlarge-frame',
;;    `enlarge-frame-horizontally', `hide-everything', `hide-frame',
;;    `iconify-everything', `iconify/map-frame',
;;    `jump-to-frame-config-register', `maximize-frame',
;;    `maximize-frame-horizontally', `maximize-frame-vertically',
;;    `mouse-iconify/map-frame', `mouse-remove-window',
;;    `mouse-show-hide-mark-unmark', `move-frame-down',
;;    `move-frame-left', `move-frame-right', `move-frame-up',
;;    `other-window-or-frame', `remove-window', `remove-windows-on',
;;    `rename-frame', `rename-non-minibuffer-frame', `restore-frame',
;;    `restore-frame-horizontally', `restore-frame-vertically',
;;    `save-frame-config',
;;    `set-all-frame-alist-parameters-from-frame',
;;    `set-frame-alist-parameter-from-frame', `show-*Help*-buffer',
;;    `show-a-frame-on', `show-buffer-menu', `show-frame',
;;    `show-hide', `shrink-frame', `shrink-frame-horizontally',
;;    `tell-customize-var-has-changed', `tile-frames',
;;    `tile-frames-horizontally', `tile-frames-vertically',
;;    `toggle-max-frame', `toggle-max-frame-horizontally',
;;    `toggle-max-frame-vertically'.
;;
;;  Non-interactive functions defined here:
;;
;;    `assq-delete-all' (Emacs 20), `available-screen-pixel-bounds',
;;    `available-screen-pixel-height', `available-screen-pixel-width',
;;    `effective-screen-pixel-bounds', `enlarged-font-name',
;;    `frame-alist-var-names', `frame-iconified-p',
;;    `frame-parameter-names', `new-frame-position',
;;    `read-args-for-tile-frames', `read-buffer-for-delete-windows',
;;    `smart-tool-bar-pixel-height'.
;;
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `delete-window' - If only one window in frame, `delete-frame'.
;;  `delete-windows-on' -
;;     1) Reads buffer differently.  Only buffers showing windows are candidates.
;;     2) Calls `delete-window', so this also deletes frames where
;;        window showing the BUFFER is the only window.
;;        (That's true also for vanilla Emacs 23+, but not before.)
;;
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(meta up)] 'move-frame-up)
;;   (global-set-key [(meta down)] 'move-frame-down)
;;   (global-set-key [(meta left)] 'move-frame-left)
;;   (global-set-key [(meta right)] 'move-frame-right)
;;   (global-set-key [(control meta down)] 'enlarge-frame)
;;   (global-set-key [(control meta right)] 'enlarge-frame-horizontally)
;;   (global-set-key [(control meta up)] 'shrink-frame)
;;   (global-set-key [(control meta left)] 'shrink-frame-horizontally)
;;   (global-set-key [(control ?x) (control ?z)] 'iconify-everything)
;;   (global-set-key [vertical-line S-down-mouse-1] 'iconify-everything)
;;   (global-set-key [(control ?z)] 'iconify/map-frame)
;;   (global-set-key [mode-line mouse-3] 'mouse-iconify/map-frame)
;;   (global-set-key [mode-line C-mouse-3] 'mouse-remove-window)
;;   (global-set-key [(control meta ?z)] 'show-hide)
;;   (global-set-key [vertical-line C-down-mouse-1] 'show-hide)
;;   (global-set-key [C-down-mouse-1] 'mouse-show-hide-mark-unmark)
;;   (substitute-key-definition 'delete-window 'remove-window global-map)
;;   (define-key ctl-x-map "o" 'other-window-or-frame)
;;   (define-key ctl-x-4-map "1" 'delete-other-frames)
;;   (define-key ctl-x-5-map "h" 'show-*Help*-buffer)
;;   (substitute-key-definition 'delete-window 'delete-windows-for global-map)
;;   (define-key global-map "\C-xt." 'save-frame-config)
;;   (define-key ctl-x-map "o" 'other-window-or-frame)
;;
;;   (defalias 'doremi-prefix (make-sparse-keymap))
;;   (defvar doremi-map (symbol-function 'doremi-prefix) "Keymap for Do Re Mi commands.")
;;   (define-key global-map "\C-xt" 'doremi-prefix)
;;   (define-key doremi-map "." 'save-frame-config)
;;
;;  Customize the menu.  Uncomment this to try it out.
;;
;;   (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
;;   (define-key global-map [menu-bar frames]
;;     (cons "Frames" menu-bar-frames-menu)))
;;   (define-key menu-bar-frames-menu [set-all-params-from-frame]
;;     '(menu-item "Set All Frame Parameters from Frame" set-all-frame-alist-parameters-from-frame
;;       :help "Set frame parameters of a frame to their current values in frame"))
;;   (define-key menu-bar-frames-menu [set-params-from-frame]
;;     '(menu-item "Set Frame Parameter from Frame..." set-frame-alist-parameter-from-frame
;;       :help "Set parameter of a frame alist to its current value in frame"))
;;   (define-key menu-bar-frames-menu [separator-frame-1] '("--"))
;;   (define-key menu-bar-frames-menu [tile-frames-vertically]
;;     '(menu-item "Tile Frames Vertically..." tile-frames-vertically
;;       :help "Tile all visible frames vertically"))
;;   (define-key menu-bar-frames-menu [tile-frames-horizontally]
;;     '(menu-item "Tile Frames Horizontally..." tile-frames-horizontally
;;       :help "Tile all visible frames horizontally"))
;;   (define-key menu-bar-frames-menu [separator-frame-2] '("--"))
;;   (define-key menu-bar-frames-menu [toggle-max-frame-vertically]
;;     '(menu-item "Toggle Max Frame Vertically" toggle-max-frame-vertically
;;       :help "Maximize or restore the selected frame vertically"
;;       :enable (frame-parameter nil 'restore-height)))
;;   (define-key menu-bar-frames-menu [toggle-max-frame-horizontally]
;;     '(menu-item "Toggle Max Frame Horizontally" toggle-max-frame-horizontally
;;       :help "Maximize or restore the selected frame horizontally"
;;       :enable (frame-parameter nil 'restore-width)))
;;   (define-key menu-bar-frames-menu [toggle-max-frame]
;;     '(menu-item "Toggle Max Frame" toggle-max-frame
;;       :help "Maximize or restore the selected frame (in both directions)"
;;       :enable (or (frame-parameter nil 'restore-width) (frame-parameter nil 'restore-height))))
;;   (define-key menu-bar-frames-menu [maximize-frame-vertically]
;;     '(menu-item "Maximize Frame Vertically" maximize-frame-vertically
;;       :help "Maximize the selected frame vertically"))
;;   (define-key menu-bar-frames-menu [maximize-frame-horizontally]
;;     '(menu-item "Maximize Frame Horizontally" maximize-frame-horizontally
;;       :help "Maximize the selected frame horizontally"))
;;   (define-key menu-bar-frames-menu [maximize-frame]
;;     '(menu-item "Maximize Frame" maximize-frame
;;       :help "Maximize the selected frame (in both directions)"))
;;   (define-key menu-bar-frames-menu [separator-frame-3] '("--"))
;;   (define-key menu-bar-frames-menu [iconify-everything]
;;     '(menu-item "Iconify All Frames" iconify-everything
;;       :help "Iconify all frames of session at once"))
;;   (define-key menu-bar-frames-menu [show-hide]
;;     '(menu-item "Hide Frames / Show Buffers" show-hide
;;       :help "Show, if only one frame visible; else hide.")))
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-font+]
;;     '("Save Frame Configuration" . save-frame-config))
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
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2010/10/19 dadams
;;     enlarge-font: Only do frame-update-faces if Emacs 20 (obsolete in 21).
;; 2010/06/04 dadams
;;     Added: (toggle-max|restore)-frame(-horizontally|-vertically).  Thx to Uday Reddy for suggestion.
;;     Renamed max-frame to maximize-frame.
;;     maximize-frame: Save original location & position params for later restoration.
;; 2010/05/25 dadams
;;     Added: max-frame, maximize-frame-horizontally, maximize-frame-vertically.
;; 2009/10/02 dadams
;;     delete-windows-on: Return nil.  Make BUFFER optional: default is current buffer.
;; 2009/08/03 dadams
;;     delete-window: Wrap with save-current-buffer.  Thx to Larry Denenberg.
;; 2009/05/17 dadams
;;     Updated to reflect thumb-frm.el name changes.
;; 2009/01/30 dadams
;;     enlarge-font, enlarged-font-name, enlarge-font-tries:
;;       Removed temporary workaround - Emacs 23 bug #119 was finally fixed.
;; 2009/01/01 dadams
;;     Removed compile-time require of doremi-frm.el to avoid infinite recursion.
;; 2008/12/13 dadams
;;     enlarge-font: Redefined for Emacs 23 - just use :height face attribute.
;;     enlarge-font-tries, enlarged-font-name: Not used for Emacs 23.
;; 2008/10/31 dadams
;;     Updated frame-parameter-names for Emacs 23.
;; 2008/07/29 dadams
;;     Option available-screen-pixel-bounds: Use nil as default value.
;;     available-screen-pixel-bounds: Redefined as the code that defined the option's default value.
;;     Added: effective-screen-pixel-bounds - code taken from old available-screen-pixel-bounds,
;;            but also convert frame geom value to numeric.
;;     Everywhere:
;;       Use effective-screen-pixel-bounds in place of available-screen-pixel-bounds function.
;;       Use available-screen-pixel-bounds function instead of option.
;;     available-screen-pixel-(width|height): Added optional INCLUDE-MINI-P arg.
;;     new-frame-position: Call available-screen-pixel-(width|height) with arg.
;;     save-frame-config: push-current-frame-config -> doremi-push-current-frame-config.
;;     Soft-require doremi-frm.el when byte-compile.
;; 2008/06/02 dadams
;;     Added: available-screen-pixel-bounds (option and function).
;;     tile-frames, available-screen-pixel-(width|height):
;;       Redefined to use available-screen-pixel-bounds.  Thx to Nathaniel Cunningham for input.
;; 2008/05/29 dadams
;;     Fixes for Mac by Nathaniel Cunningham and David Reitter:
;;       window-mgr-title-bar-pixel-height, tile-frames, smart-tool-bar-pixel-height (added).
;; 2007/12/27 dadams
;;      tile-frames: Restored border calculation, but using only external border.
;;      Renamed window-mgr-*-width to window-mgr-*-height and changed default value from 32 to 27.
;; 2007/12/20 dadams
;;      Added: frame-extra-pixels(width|height).  Use in tile-frames.  Thx to David Reitter.
;;      frame-horizontal-extra-pixels: Changed default value from 30 to 32.
;; 2007/10/11 dadams
;;      Added: assq-delete-all (for Emacs 20).
;; 2007/09/02 dadams
;;      Added: available-screen-pixel-(width|height).  Use in tile-frames, new-frame-position.
;; 2007/06/12 dadams
;;      tile-frames: Corrected use of fboundp for thumbnail-frame-p.
;; 2007/05/27 dadams
;;      enlarged-font-name:
;;        Do nothing if null assq of ascii.  Not sure what this means, but gets around Emacs 23 bug.
;; 2006/08/22 dadams
;;      Added: delete-windows-for, read-buffer-for-delete-windows.
;;      delete-windows-on: Use read-buffer-for-delete-windows.
;;      Removed old-delete-windows-on (not used).
;; 2006/05/30 dadams
;;      delete-windows-on: Return nil if buffer arg is nil. Thanks to Slawomir Nowaczyk.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;      Renamed group.  Added :link.
;; 2006/01/04 dadams
;;     Added: other-window-or-frame.
;; 2005/12/29 dadams
;;     mouse-show-hide-mark-unmark: dired-mouse-mark/unmark -> diredp-mouse-mark/unmark.
;; 2005/12/13 dadams
;;     Added: delete-other-frames.
;; 2005/11/18 dadams
;;     enlarge-font: Try to increment or decrment further, testing for an existing font.
;;     Added: enlarge-font-tries, enlarged-font-name.
;; 2005/10/03 dadams
;;     Removed require of icomplete+.el (no longer redefines read-from-minibuffer).
;; 2005/07/03 dadams
;;     Renamed: args-for-tile-frames to read-args-for-tile-frames.
;; 2005/06/19 dadams
;;     tile-frames: Don't tile thumbnail frames.
;; 2005/05/29 dadams
;;     Moved here from frame+.el and fit-frame.el: enlarge-frame*, shrink-frame*.
;;     Added: move-frame-up|down|left|right, move-frame-wrap-within-display-flag,
;;            new-frame-position.
;; 2005/05/28 dadams
;;     show-a-frame-on: Use another-buffer as default for read-buffer, if available.
;; 2005/05/15 dadams
;;     Renamed: minibuffer-frame to 1on1-minibuffer-frame.
;; 2005/05/10 dadams
;;     remove-window: Removed definition; just defalias it to delete-window.
;;     delete-window: (one-window-p) -> (one-window-p t).
;;     set-frame-alist-parameter-from-frame: No longer use destructive fns.
;; 2005/01/19 dadams
;;     set-all-frame-alist-parameters-from-frame:
;;            Added really-all-p and use frame-parameters-to-exclude.
;;     Added: frame-parameters-to-exclude, tell-customize-var-has-changed.
;; 2005/01/18 dadams
;;     Added: set-all-frame-alist-parameters-from-frame, set-frame-alist-parameter-from-frame,
;;            frame-alist-var-names, frame-parameter-names.
;;     Added Note on saving changes.
;; 2005/01/08 dadams
;;     Moved enlarge-font here from doremi-frm.el, where it was called doremi-grow-font.
;; 2005/01/04 dadams
;;     Added rename-frame-when-iconify-flag.
;;       Use it in iconify-everything, (mouse-)iconify/map-frame.
;;     Added (defgroup frame-cmds).
;; 2004/12/23 dadams
;;     frame-config-register, show-hide-show-function, window-mgr-title-bar-pixel-width:
;;         Changed defvar to defcustom.
;; 2004/12/21 dadams
;;     hide-everything, iconify-everything: bind thumbify-instead-of-iconify-flag to nil.
;; 2004/12/10 dadams
;;     tile-frames: Change 15 to (frame-char-height fr) for scroll-bar-width.
;;     tile-frames-*: Corrected doc strings for non-interactive case.
;; 2004/12/09 dadams
;;     Changed compile-time require of strings to a soft require.
;; 2004/10/11 dadams
;;     args-for-tile-frames: Fixed bug when non-existant frame in name history.
;;     tile-frames: show-frame at end (for case where use prefix arg)
;; 2004/09/11 dadams
;;     Moved to doremi-frm.el: frame-config-ring*, frame-config-wo-parameters,
;;                             push-frame-config.
;; 2004/09/07 dadams
;;     Added: jump-to-frame-config-register, push-frame-config, save-frame-config.
;; 2004/09/01 dadams
;;     Added: frame-config-register, show-hide-show-function,
;;            jump-to-frame-config-register.
;;     Rewrote to record frame config: iconify-everything, hide-everything.
;;     Rewrote to use show-hide-show-function: show-hide.
;; 2004/03/22 dadams
;;     Added: tile-frames, tile-frames-vertically, args-for-tile-frames.
;;     Rewrote tile-frames-horizontally to use tile-frames.
;; 2004/03/19 dadams
;;     Added tile-frames-horizontally.
;; 2000/11/27 dadams
;;     hide-frame: fixed bug: Added get-a-frame for frame name read.
;; 2000/09/27 dadams
;;     1. Added: frame-iconified-p.
;;     2. remove-window: only make-frame-invisible if not iconified (HACK).
;; 1999/10/05 dadams
;;     rename-frame: fixed bug if only 1 frame and old-name was a frame.
;; 1999/08/25 dadams
;;     Added: hide-everything, show-buffer-menu, show-hide.
;; 1999/03/17 dadams
;;     delete-1-window-frames-on: ensure a buffer object (not a name).
;; 1996/04/26 dadams
;;     delete/iconify-windows-on, show-a-frame-on: Do nothing if null buffer.
;; 1996/03/12 dadams
;;     delete/iconify-window: Unless one-window-p, do old-delete-window outside of
;;                            save-window-excursion.
;; 1996/03/08 dadams
;;     1. delete-windows-on: a. Fixed incorrect interactive spec (bad paren).
;;                           b. Second arg FRAME also provided interactively now.
;;     2. Added: delete/iconify-window, delete/iconify-windows-on.
;; 1996/02/27 dadams
;;     show-frame: Call make-frame-visible.
;; 1996/02/09 dadams
;;     Added show-*Help*-buffer.
;; 1996/01/30 dadams
;;     1. show-frame: Don't make-frame-visible.  Done by raise-frame anyway.
;;     2. Added show-a-frame-on.
;; 1996/01/09 dadams
;;     Added delete-windows-on and made it interactive.
;; 1996/01/08 dadams
;;     Added rename-non-minibuffer-frame.  Use in iconify-everything,
;;           iconify/map-frame, mouse-iconify/map-frame.
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

(eval-when-compile (require 'cl)) ;; butlast, case, incf, set-difference
                                  ;; (plus, for Emacs 20: dolist and, for Emacs <20: when, unless)
(require 'frame-fns) ;; frame-geom-value-numeric, frames-on, get-frame-name, get-a-frame, read-frame
(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

;; Don't require even to byte-compile, because doremi-frm.el soft-requires frame-cmds.el
;; (eval-when-compile (require 'doremi-frm nil t)) ;; (no error if not found)
;;                                                 ;; doremi-push-current-frame-config

;; Not required here, because this library requires `frame-cmds.el': `thumb-frm.el'.
;; However, `frame-cmds.el' soft-uses `thumfr-thumbnail-frame-p', which is defined
;; in `thumb-frm.el'.

;; Quiet byte-compiler.
(defvar mac-tool-bar-display-mode)

;;;;;;;;;;;;;;;;;;;;;;;




;;; USER OPTIONS (VARIABLES) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup Frame-Commands nil
  "Miscellaneous frame and window commands."
  :group 'frames
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
frame-cmds.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/frame-cmds.el")
  :link '(url-link :tag "Description - `delete-window'"
          "http://www.emacswiki.org/cgi-bin/wiki/FrameModes")
  :link '(url-link :tag "Description - Frame Renaming"
          "http://www.emacswiki.org/cgi-bin/wiki/FrameTitle")
  :link '(url-link :tag "Description - Frame Resizing"
          "http://www.emacswiki.org/cgi-bin/wiki/Shrink-Wrapping_Frames")
  :link '(url-link :tag "Description - Frame Customization"
          "http://www.emacswiki.org/cgi-bin/wiki/CustomizingAndSaving")
  :link '(url-link :tag "Description - Frame Tiling"
          "http://www.emacswiki.org/cgi-bin/wiki/Frame_Tiling_Commands")
  :link '(url-link :tag "Description - General"
          "http://www.emacswiki.org/cgi-bin/wiki/FrameModes")
  :link '(emacs-commentary-link :tag "Commentary" "frame-cmds")
  )

(defcustom rename-frame-when-iconify-flag t
  "*Non-nil means frames are renamed when iconified.
The new name is the name of the current buffer."
  :type 'boolean :group 'Frame-Commands)

(defcustom frame-config-register ?\C-l  ; Control-L is the name of the register.
  "*Character naming register for saving/restoring frame configuration."
  :type 'character :group 'Frame-Commands)

(defcustom show-hide-show-function 'jump-to-frame-config-register
  "*Function to show stuff that is hidden or iconified by `show-hide'.
Candidates include `jump-to-frame-config-register' and `show-buffer-menu'."
  :type '(choice (const :tag "Restore frame configuration" jump-to-frame-config-register)
                 (function :tag "Another function"))
  :group 'Frame-Commands)

(defcustom window-mgr-title-bar-pixel-height (if (eq window-system 'mac) 22 27)
  "*Height of frame title bar provided by the window manager, in pixels.
You might alternatively call this constant the title-bar \"width\" or
\"thickness\".  There is no way for Emacs to determine this, so you
must set it."
  :type 'integer :group 'Frame-Commands)

(defcustom enlarge-font-tries 100
  "Number of times to try to change font-size, when looking for a font.
The font-size portion of a font name is incremented or decremented at
most this many times, before giving up and raising an error."
  :type 'integer :group 'Frame-Commands)

(defcustom frame-parameters-to-exclude '((window-id) (buffer-list) (name) (title) (icon-name))
  "Parameters to exclude in `set-all-frame-alist-parameters-from-frame'.
An alist of the same form as that returned by `frame-parameters'.
The cdr of each alist element is ignored.
These frame parameters are not copied to the target alist."
  :type '(repeat (cons symbol sexp)) :group 'Frame-Commands)

(defcustom move-frame-wrap-within-display-flag t
  "*Non-nil means wrap frame movements within the display.
Commands `move-frame-up', `move-frame-down', `move-frame-left', and
`move-frame-right' then move the frame back onto the display when it
moves off of it.
If nil, you can move the frame as far off the display as you like."
  :type 'boolean :group 'Frame-Commands)

(defcustom available-screen-pixel-bounds nil
  "Upper left and lower right of available screen space for tiling frames.
Integer list: (x0 y0 x1 y1), where (x0, y0) is the upper left position
and (x1, y1) is the lower right position.  Coordinates are in pixels,
measured from the screen absolute origin, (0, 0), at the upper left.

If this is nil, then the available space is calculated.  That should
give good results in most cases."
  :type '(list
          (integer :tag "X0 (upper left) - pixels from screen left")
          (integer :tag "Y0 (upper left) - pixels from screen top")
          (integer :tag "X1 (lower right) - pixels from screen left" )
          (integer :tag "Y1 (lower right) - pixels from screen top"))
  :group 'Frame-Commands)



;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defun save-frame-config ()
  "Save current frame configuration.
You can restore it with \\[jump-to-frame-config-register]."
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (when (featurep 'doremi-frm) (doremi-push-current-frame-config))
  (message
   (substitute-command-keys
    (if (featurep 'doremi-frm)
        (format "Use `\\[jump-to-frame-config-register]' (`C-x r j %c') or \
`\\[doremi-frame-configs]' to restore frames as before (undo)." frame-config-register)
      "Use `\\[jump-to-frame-config-register]' to restore frames as before (undo)."))))

;;;###autoload
(defun jump-to-frame-config-register ()
  "Restore frame configuration saved in `frame-config-register'."
  (interactive)
  (jump-to-register frame-config-register))

;;;###autoload
(defun iconify-everything ()
  "Iconify all frames of session at once.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'."
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (let ((thumfr-thumbify-dont-iconify-flag  nil)) ; Defined in `thumb-frm.el'.
    (dolist (frame  (visible-frame-list))
      (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame frame))
      (iconify-frame frame))))

;;;###autoload
(defun hide-everything ()
  "Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'."
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (let ((minibuf-frame-name
         (and (boundp '1on1-minibuffer-frame)
              (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))))
        (thumfr-thumbify-dont-iconify-flag  nil)) ; Defined in `thumb-frm.el'.
    (dolist (frame  (frame-list))
      (if (eq minibuf-frame-name (cdr (assq 'name (frame-parameters frame))))
          (iconify-frame frame)         ; minibuffer frame
        (make-frame-invisible frame t))))) ; other frames

;;;###autoload
(defun show-hide ()
  "1 frame visible: `show-hide-show-function'; else: `hide-everything'.
This acts as a toggle between showing all frames and showing only an
iconified minibuffer frame."
  (interactive)
  (if (< (length (visible-frame-list)) 2)
      (funcall show-hide-show-function)
    (hide-everything)))

;;;###autoload
(defun show-buffer-menu ()
  "Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after
being made invisible."
  (interactive)
  (let ((minibuf-frame-name
         (and (boundp '1on1-minibuffer-frame)
              (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame))))))
    (dolist (frame (frame-list))
      (if (eq minibuf-frame-name
              (cdr (assq 'name (frame-parameters frame))))
          (make-frame-visible frame)    ; minibuffer frame
        (iconify-frame frame)))         ; other frames
    (buffer-menu)))

;;;###autoload
(defun mouse-show-hide-mark-unmark (event)
  "In minibuffer: `show-hide'.  In dired: mark/unmark; else: buffer menu."
  (interactive "e")
  (if (window-minibuffer-p (posn-window (event-start event)))
      (show-hide)
    (or (and (memq major-mode '(dired-mode vc-dired-mode))
             (fboundp 'diredp-mouse-mark/unmark)
             (diredp-mouse-mark/unmark event)) ; Return nil if not on a file or dir.
        (mouse-buffer-menu event))))

;;;###autoload
(defun iconify/map-frame (&optional iconify-all)
  "Iconify selected frame if now mapped.  Map it if now iconified.
With non-nil prefix arg ICONIFY-ALL, iconify all visible frames."
  (interactive "P")
  (if iconify-all
      (iconify-everything)
    (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame))
    (iconify-or-deiconify-frame)))

;;;###autoload
(defun mouse-iconify/map-frame (event)
  "Iconify frame clicked on, if now mapped.  Map it if now iconified."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame))
  (iconify-or-deiconify-frame))


(or (fboundp 'old-delete-window)
    (fset 'old-delete-window (symbol-function 'delete-window)))


;; REPLACES ORIGINAL (built-in):
;; If WINDOW is the only one in its frame, `delete-frame'.
;;;###autoload
(defun delete-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (if (one-window-p t) (delete-frame) (old-delete-window (selected-window)))))

;;;###autoload
(defun delete-windows-for (&optional buffer)
  "`delete-window' or prompt for buffer and delete its windows.
With no prefix arg, delete the selected window.
With a prefix arg, prompt for a buffer and delete all windows, on any
  frame, that show that buffer."
  (interactive (list (and current-prefix-arg (read-buffer-for-delete-windows))))
  (if buffer (delete-windows-on buffer) (delete-window)))


;; REPLACES ORIGINAL (built-in):
;; 1) Use `read-buffer' in interactive spec.
;; 2) Do not raise an error if BUFFER is a string that does not name a buffer.
;; 3) Call `delete-window', so if you use my `delete-window' then this also deletes
;;    frames where window showing the BUFFER is the only window.
;;;###autoload
(defun delete-windows-on (&optional buffer frame)
  "Delete windows showing BUFFER.
Optional arg BUFFER defaults to the current buffer.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If t, delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is t (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames)."
  (interactive
   (list (read-buffer-for-delete-windows)
         (and current-prefix-arg
              (or (natnump (prefix-numeric-value current-prefix-arg))
                  'visible))))
  (unless buffer (setq buffer  (current-buffer))) ; Like Emacs 23+ - unlike Emacs 21-22.

  ;; `get-buffer-window' interprets FRAME oppositely for t and nil, so switch.
  (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
  (let (win)
    ;; Vanilla Emacs version raises an error if BUFFER is a string that does not name a buffer.
    ;; We do not raise an error - we do nothing.
    (and (get-buffer buffer)
         (while (setq win (get-buffer-window buffer frame))
           (delete-window win))
         nil)))                         ; Return nil always, like vanilla Emacs.

(defun read-buffer-for-delete-windows ()
  "Read buffer name for delete-windows commands.
Only displayed buffers are completion candidates."
  (completing-read "Delete windows on buffer: "
                   (let ((all-bufs   (buffer-list))
                         (cand-bufs  ()))
                     (dolist (buf  all-bufs)
                       (when (get-buffer-window buf t)
                         (push (list (buffer-name buf)) cand-bufs)))
                     cand-bufs)
                   nil t nil 'minibuffer-history (buffer-name (current-buffer)) t))

(defsubst frame-iconified-p (frame)
  "Return non-nil if FRAME is `frame-live-p' and `frame-visible-p'."
  (and (frame-live-p frame) (eq (frame-visible-p frame) 'icon)))

;; (defun remove-window (&optional window)
;;   "Remove WINDOW from the display.  Default is `selected-window'.
;; If WINDOW is the only one in its frame, then:
;;    If WINDOW is dedicated to its buffer, then make its frame invisible.
;;    Otherwise, delete its frame (as well as the window)."
;;   (interactive)
;;   (setq window (or window (selected-window)))
;;   (select-window window)
;;   (if (and (window-dedicated-p (selected-window))
;;            (one-window-p t))
;;       (let ((fr (selected-frame)))
;;         ;; HACK because of Emacs bug: `raise-frame' won't raise a frame
;;         ;; that was first iconified and then made invisible.
;;         ;; So, here we don't make an iconified frame invisible.
;;         (unless (frame-iconified-p fr)
;;           (make-frame-invisible fr)))
;;     (delete-window)))

;; REMOVED old definition, above, because of problems with invisible
;; *Completions* frame when use completion window with subsequent args
;; to a command.  Just use `delete-window' now, which deletes frame if
;; `one-window-p'.  Use a `defalias' because its easier than replacing
;; all my calls to `remove-window' with `delete-window'.
;;
;;;###autoload
(defalias 'remove-window 'delete-window)

;;;###autoload
(defun remove-windows-on (buffer)
  "Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    (dolist (fr (frames-on buffer t))
      (remove-window (get-buffer-window buffer t)))))

;;;###autoload
(defun mouse-remove-window (event)
  "Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (remove-window (posn-window (event-start event))))

;;;###autoload
(defun delete/iconify-window (&optional window frame-p)
  "Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is t, then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list nil (if current-prefix-arg
                 (not (natnump (prefix-numeric-value current-prefix-arg)))
               'window-dedicated-p)))
  (setq window (or window (selected-window)))
  (let ((one-win-p t))
    (save-window-excursion
      (select-window window)
      (if (one-window-p)
          (if frame-p
              (if (eq t frame-p)
                  (iconify-frame)
                (unless (and (symbolp frame-p) (fboundp frame-p))
                  (setq frame-p 'window-dedicated-p))
                (if (funcall frame-p window) (iconify-frame) (delete-frame)))
            (delete-frame))             ; Default.
        (setq one-win-p nil)))
    ;; Do this outside `save-window-excursion'.
    (unless one-win-p (old-delete-window window))))

;;;###autoload
(defun delete/iconify-windows-on (buffer &optional frame frame-p)
  "For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If t, treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is t, then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list (read-buffer "Delete windows on buffer: " (current-buffer) 'existing)
         nil
         (if current-prefix-arg
             (not (natnump (prefix-numeric-value current-prefix-arg)))
           'window-dedicated-p)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; `get-buffer-window' interprets FRAME oppositely for t and nil,
    ;; so switch.
    (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
    (dolist (fr (frames-on buffer frame))
      (delete/iconify-window (get-buffer-window buffer frame) frame-p))))

;;;###autoload
(defun rename-frame (&optional old-name new-name all-named)
  "Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg ALL-NAMED non-nil means rename all frames named FRAME to NEWNAME.

OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.

NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  ;; Convert to frame if string.
  (let ((fr (get-a-frame old-name)))
    (if all-named
        (while fr
          (modify-frame-parameters fr (list (cons 'name new-name)))
          (setq fr (get-a-frame old-name))) ; Get another.
      (when (string= (get-frame-name fr) (get-frame-name))
        (setq fr (selected-frame)))
      (modify-frame-parameters fr (list (cons 'name new-name))))))

;;;###autoload
(defun rename-non-minibuffer-frame (&optional old-name new-name all-named)
  "Unless OLD-NAME names the minibuffer frame, use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.

Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  (let ((fr (get-a-frame old-name)))    ; Convert to frame if string.
    (if (and (boundp '1on1-minibuffer-frame)
             (eq (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))
                 (cdr (assq 'name (frame-parameters fr)))))
        (and (interactive-p)
             (error "Use `rename-frame' if you really want to rename minibuffer"))
      (rename-frame))))

;;;###autoload
(defun show-frame (frame)
  "Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name."
  (interactive (list (read-frame "Frame to make visible: ")))
  (setq frame (get-a-frame frame))
  (make-frame-visible frame)
  (raise-frame frame))

;;;###autoload
(defun hide-frame (frame &optional prefix)
  "Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible."
  (interactive (list (read-frame "Frame to make invisible: ")))
  (make-frame-invisible (get-a-frame frame) prefix))

;;;###autoload
(defun show-a-frame-on (buffer)
  "Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string)."
  (interactive
   (list (read-buffer "Show a frame showing buffer: "
                      (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                          (another-buffer nil t)
                        (other-buffer (current-buffer)))
                      'existing)))
  (when buffer                          ; Do nothing if null BUFFER.
    (let ((fr (car (frames-on buffer)))) (when fr (show-frame fr)))))

;;;###autoload
(defun show-*Help*-buffer ()
  "Raise a frame showing buffer *Help*, without selecting it."
  (interactive) (show-a-frame-on "*Help*"))

;;;###autoload
(defun delete-1-window-frames-on (buffer)
  "Delete all visible 1-window frames showing BUFFER."
  (interactive
   (list (read-buffer "Delete all visible 1-window frames showing buffer: "
                      (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (save-excursion
    (when (buffer-live-p buffer)        ; Do nothing if dead buffer.
      (dolist (fr (frames-on buffer))   ; Is it better to search through
        (save-window-excursion          ; `frames-on' or `get-buffer-window-list'?
          (select-frame fr)
          (when (one-window-p t fr) (delete-frame)))))))

;;;###autoload
(defun delete-other-frames (&optional frame)
  "Delete all frames except FRAME (default: selected frame).
Interactively, use a prefix arg (`\\[universal-argument]') to be prompted for FRAME."
  (interactive (list (if current-prefix-arg
                         (get-a-frame (read-frame "Frame to make invisible: "))
                       (selected-frame))))
  (when frame
    (dolist (fr (frame-list))
      (unless (eq fr frame) (condition-case nil (delete-frame fr) (error nil))))))

;;;###autoload
(defun maximize-frame-horizontally (&optional frame)
  "Maximize selected frame horizontally."
  (interactive (list (selected-frame)))
  (maximize-frame 'horizontal frame))

;;;###autoload
(defun maximize-frame-vertically (&optional frame)
  "Maximize selected frame vertically."
  (interactive (list (selected-frame)))
  (maximize-frame 'vertical frame))

;;;###autoload
(defun maximize-frame (&optional direction frame)
  "Maximize selected frame horizontally, vertically, or both.
With no prefix arg, maximize both directions.
With a non-negative prefix arg, maximize vertically.
With a negative prefix arg, maximize horizontally.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to maximize."
  (interactive (list (if current-prefix-arg
                         (if (natnump (prefix-numeric-value current-prefix-arg))
                             'vertical
                           'horizontal)
                       'both)))
  (unless frame (setq frame  (selected-frame)))
  (unless direction (setq direction 'both))
  (let (;; Size of a frame that uses all of the available screen area,
        ;; but leaving room for a minibuffer frame at bottom of display.
        (fr-pixel-width   (available-screen-pixel-width))
        (fr-pixel-height  (available-screen-pixel-height))
        (fr-origin        (if (eq direction 'horizontal)
                              (car (effective-screen-pixel-bounds))
                            (cadr (effective-screen-pixel-bounds))))
        (orig-left        (frame-parameter frame 'left))
        (orig-top         (frame-parameter frame 'top))
        (orig-width       (frame-parameter frame 'width))
        (orig-height      (frame-parameter frame 'height)))
    (let* ((borders     (* 2 (cdr (assq 'border-width (frame-parameters frame)))))
           (new-left    (if (memq direction '(horizontal both)) fr-origin 0))
           (new-top     (if (memq direction '(horizontal both)) 0         fr-origin))
           ;; Subtract borders, scroll bars, & title bar, then convert pixel sizes to char sizes.
           (new-width   (if (memq direction '(horizontal both))
                            (/ (- fr-pixel-width borders (frame-extra-pixels-width frame))
                               (frame-char-width frame))
                          orig-width))
           (new-height  (if (memq direction '(vertical both))
                            (- (/ (- fr-pixel-height borders
                                     (frame-extra-pixels-height frame)
                                     window-mgr-title-bar-pixel-height
                                     (smart-tool-bar-pixel-height))
                                  (frame-char-height frame))
                               ;; Subtract menu bar unless on Carbon Emacs (menu bar not in the frame).
                               (if (eq window-system 'mac)
                                   0
                                 (cdr (assq 'menu-bar-lines (frame-parameters frame)))))
                          orig-height)))
      (modify-frame-parameters
       frame
       `((left   . ,new-left)
         (width  . ,new-width)
         (top    . ,new-top)
         (height . ,new-height)
         ;; If we actually changed a parameter, record the old one for restoration.
         ,(and new-left   (/= orig-left new-left)     (cons 'restore-left   orig-left))
         ,(and new-top    (/= orig-top  new-top)      (cons 'restore-top    orig-top))
         ,(and new-width  (/= orig-width  new-width)  (cons 'restore-width  orig-width))
         ,(and new-height (/= orig-height new-height) (cons 'restore-height orig-height)))))
    (show-frame frame)
    (incf fr-origin (if (eq direction 'horizontal) fr-pixel-width fr-pixel-height))))

;;;###autoload
(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)
;;;###autoload
(defun toggle-max-frame-horizontally (&optional frame)
  "Toggle maximization of FRAME horizontally.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `left' and `width' frame parameters.

FRAME defaults to the selected frame."
  (interactive (list (selected-frame)))
  (restore-frame 'horizontal frame))

;;;###autoload
(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)
;;;###autoload
(defun toggle-max-frame-vertically (&optional frame)
  "Toggle maximization of FRAME vertically.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `top' and `height' frame parameters.

FRAME defaults to the selected frame."
  (interactive (list (selected-frame)))
  (restore-frame 'vertical frame))

;;;###autoload
(defalias 'restore-frame 'toggle-max-frame)
;;;###autoload
(defun toggle-max-frame (&optional direction frame)
  "Toggle maximization of FRAME horizontally, vertically, or both.
Reverses or (if restored) repeats the effect of the Emacs maximize
commands.  Does not restore from maximization effected outside Emacs.

With no prefix arg, toggle both directions.
With a non-negative prefix arg, toggle only vertically.
With a negative prefix arg, toggle horizontally.

When toggling both, each is toggled from its last maximize or restore
state.  This means that using this after `maximize-horizontal',
`maximize-vertical', `toggle-max-horizontal', or `toggle-max-vertical'
does not necessarily just reverse the effect of that command.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to change.  It defaults to the selected frame."
  (interactive (list (if current-prefix-arg
                         (if (natnump (prefix-numeric-value current-prefix-arg))
                             'vertical
                           'horizontal)
                       'both)))
  (unless frame (setq frame  (selected-frame)))
  (unless direction (setq direction 'both))
  (let ((restore-left    (frame-parameter frame 'restore-left))
        (restore-top     (frame-parameter frame 'restore-top))
        (restore-width   (frame-parameter frame 'restore-width))
        (restore-height  (frame-parameter frame 'restore-height))
        (orig-left       (frame-parameter frame 'left))
        (orig-top        (frame-parameter frame 'top))
        (orig-width      (frame-parameter frame 'width))
        (orig-height     (frame-parameter frame 'height))
        (horiz           (memq direction '(horizontal both)))
        (vert            (memq direction '(vertical both))))
    (modify-frame-parameters
     frame `(,(and horiz restore-left   (cons 'left   restore-left))
             ,(and horiz restore-width  (cons 'width  restore-width))
             ,(and vert  restore-top    (cons 'top    restore-top))
             ,(and vert  restore-height (cons 'height restore-height))
             ,(and horiz restore-left   (cons 'restore-left   orig-left))
             ,(and horiz restore-width  (cons 'restore-width  orig-width))
             ,(and vert  restore-top    (cons 'restore-top    orig-top))
             ,(and vert  restore-height (cons 'restore-height orig-height)))))
  (show-frame frame))

;;;###autoload
(defun tile-frames-horizontally (&optional frames)
  "Tile frames horizontally.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled."
  (interactive (and current-prefix-arg (read-args-for-tile-frames)))
  (tile-frames 'horizontal frames))

;;;###autoload
(defun tile-frames-vertically (&optional frames)
  "Tile frames vertically.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled."
  (interactive (and current-prefix-arg (read-args-for-tile-frames)))
  (tile-frames 'vertical frames))

;;;###autoload
(defun tile-frames (direction frames)
  "Tile visible frames horizontally or vertically, depending on DIRECTION.
Arg DIRECTION is `horizontal' or `vertical'.
Arg FRAMES is the list of frames to tile.  If nil, then tile all visible
frames (except a standalone minibuffer frame, if any)."
  (let ((visible-frames
         (or frames
             (filtered-frame-list       ; Get visible frames, except minibuffer.
              (function
               (lambda (fr)
                (and (eq t (frame-visible-p fr))
                     (or (not (fboundp 'thumfr-thumbnail-frame-p))
                         (not (thumfr-thumbnail-frame-p fr)))
                     (or (not (boundp '1on1-minibuffer-frame))
                         (not (eq (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))
                                  (cdr (assq 'name (frame-parameters fr))))))))))))
        ;; Size of a frame that uses all of the available screen area,
        ;; but leaving room for a minibuffer frame at bottom of display.
        (fr-pixel-width (available-screen-pixel-width))
        (fr-pixel-height (available-screen-pixel-height))
        (fr-origin (if (eq direction 'horizontal)
                       (car (effective-screen-pixel-bounds))
                     (cadr (effective-screen-pixel-bounds)))))
    (case direction                     ; Size of frame in pixels.
      (horizontal (setq fr-pixel-width  (/ fr-pixel-width  (length visible-frames))))
      (vertical   (setq fr-pixel-height (/ fr-pixel-height (length visible-frames))))
      (otherwise (error "Function tile-frames: DIRECTION must be `horizontal' or `vertical'")))
    (dolist (fr visible-frames)
      ;; $$$$$$ (let ((borders (* 2 (+ (cdr (assq 'border-width (frame-parameters fr)))
      ;;                               (cdr (assq 'internal-border-width (frame-parameters fr)))))))
      (let ((borders (* 2 (cdr (assq 'border-width (frame-parameters fr))))))
        (set-frame-size
         fr
         ;; Subtract borders, scroll bars, & title bar, then convert pixel sizes to char sizes.
         (/ (- fr-pixel-width borders (frame-extra-pixels-width fr))
            (frame-char-width fr))
         (- (/ (- fr-pixel-height borders (frame-extra-pixels-height fr)
                  window-mgr-title-bar-pixel-height (smart-tool-bar-pixel-height))
               (frame-char-height fr))
            (if (eq window-system 'mac)
                0                       ; Menu bar for Carbon Emacs is not in the frame.
              (cdr (assq 'menu-bar-lines (frame-parameters fr))))))) ; Subtract `menu-bar-lines'.
      (set-frame-position fr
                          (if (eq direction 'horizontal) fr-origin 0)
                          (if (eq direction 'horizontal) 0 fr-origin))
      (show-frame fr)
      (incf fr-origin (if (eq direction 'horizontal) fr-pixel-width fr-pixel-height)))))

(defun frame-extra-pixels-width (frame)
  "Pixel difference between FRAME total width and its text area width."
  (- (frame-pixel-width frame) (* (frame-char-width frame) (frame-width frame))))

(defun frame-extra-pixels-height (frame)
  "Pixel difference between FRAME total height and its text area height."
  (- (frame-pixel-height frame) (* (frame-char-height frame) (frame-height frame))))

(defun smart-tool-bar-pixel-height (&optional frame)
  "Pixel height of Mac smart tool bar."
  (if (and (boundp 'mac-tool-bar-display-mode) (> (frame-parameter frame 'tool-bar-lines) 0))
      (if (eq mac-tool-bar-display-mode 'icons) 40 56)
    0))

(defun read-args-for-tile-frames ()
  "Read arguments for `tile-frames'."
  (list
   (list
    ;; Note: `read-frame' puts selected-frame name at front of `frame-name-history'.
    (get-a-frame (read-frame "Tile two frames - First frame: " nil t))
    ;; Get next visible frame.  For default (prompt) value:
    ;;   If there is another visible frame in `frame-name-history', use next such.
    ;;   Else if there is another visible frame in internal frame list, use next such.
    ;;   Else use selected frame. (`frame-name-history' is defined in `frame.el'.)
    (get-a-frame
     (read-frame
      "Second frame: "
      (let ((fr-names (cdr frame-name-history))
            (visible-p nil)
            (fr nil))
        (while (and (not fr) fr-names)  ; While no visible frame found and still fr-names to check.
          (setq fr (car fr-names))      ; Name
          (setq fr (get-a-frame fr))    ; Frame
          (setq fr (and fr (eq t (frame-visible-p fr)) fr)) ; Visible frame
          (setq fr-names (cdr fr-names)))

        ;; If no visible frames in history, besides selected-frame,
        ;; then get next visible frame (not its name) from internal frame list.
        (unless fr
          (setq fr (selected-frame))
          (while (and (not visible-p)
                      (setq fr (next-frame fr))
                      (not (equal fr (selected-frame)))) ; equal => no other found.
            (setq visible-p (eq t (frame-visible-p fr)))))
        fr)
      t)))))

(defun available-screen-pixel-bounds ()
  "Returns a value of the same form as `available-screen-pixel-bounds'.
This represents the currently available screen area."
  (or available-screen-pixel-bounds     ; Use the option value, if available.
      (if (fboundp 'mac-display-available-pixel-bounds) ; Mac-OS-specific.
          (mac-display-available-pixel-bounds)
        (list 0 0 (x-display-pixel-width) (x-display-pixel-height)))))

(defun effective-screen-pixel-bounds ()
  "Upper left and lower right of available screen space for tiling frames.
This is `available-screen-pixel-bounds', possibly adjusted to allow
for the standalone minibuffer frame provided by `oneonone.el'."
  (if (boundp '1on1-minibuffer-frame)
      (append (butlast (available-screen-pixel-bounds))
              (list (frame-geom-value-numeric 'top (cdr (assq 'top (frame-parameters
                                                                    1on1-minibuffer-frame))))))
    (available-screen-pixel-bounds)))

(defun available-screen-pixel-width (&optional include-mini-p)
  "Width of the usable screen, in pixels.
Non-nil optional argument `include-mini-p' means include the space
occupied by a standalone minibuffer, if any."
  (let ((bounds (if include-mini-p
                    (available-screen-pixel-bounds)
                  (effective-screen-pixel-bounds))))
    (- (caddr bounds) (car bounds)))) ; X1 - X0

(defun available-screen-pixel-height (&optional include-mini-p)
  "Height of the usable screen, in pixels.
Non-nil optional argument `include-mini-p' means include the
space occupied by a standalone minibuffer, if any."
  (let ((bounds (if include-mini-p
                    (available-screen-pixel-bounds)
                  (effective-screen-pixel-bounds))))
    (- (cadddr bounds) (cadr bounds)))) ; Y1 - Y0

;; Inspired by `sk-grow-frame' from Sarir Khamsi [sarir.khamsi@raytheon.com]
;;;###autoload
(defun enlarge-frame (&optional increment frame)
  "Increase the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (+ (frame-height frame) increment)))

;;;###autoload
(defun enlarge-frame-horizontally (&optional increment frame)
  "Increase the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (+ (frame-width frame) increment)))

;;;###autoload
(defun shrink-frame (&optional increment frame)
  "Decrease the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (- (frame-height frame) increment)))

;;;###autoload
(defun shrink-frame-horizontally (&optional increment frame)
  "Decrease the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (- (frame-width frame) increment)))

;;;###autoload
(defun move-frame-down (&optional increment frame)
  "Move FRAME (default: selected-frame) down by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument."
  (interactive "P")
  (setq increment (if increment (prefix-numeric-value increment) 10)) ; 1 is too small
  (modify-frame-parameters frame
                           (list (list 'top '+ (new-frame-position frame 'top increment)))))

;;;###autoload
(defun move-frame-up (&optional increment frame)
  "Move FRAME (default: selected-frame) up by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument."
  (interactive "P")
  (setq increment (if increment (prefix-numeric-value increment) 10)) ; 1 is too small
  (move-frame-down (- increment)))

;;;###autoload
(defun move-frame-right (&optional increment frame)
  "Move FRAME (default: selected-frame) toward the right by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument."
  (interactive "P")
  (setq increment (if increment (prefix-numeric-value increment) 10)) ; 1 is too small
  (modify-frame-parameters frame
                           (list (list 'left '+ (new-frame-position frame 'left increment)))))

;;;###autoload
(defun move-frame-left (&optional increment frame)
  "Move FRAME (default: selected-frame) toward the left by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument."
  (interactive "P")
  (setq increment (if increment (prefix-numeric-value increment) 10)) ; 1 is too small
  (move-frame-right (- increment)))

;; Helper function.
(defun new-frame-position (frame type incr)
  "Return the new TYPE position of FRAME, incremented by INCR.
TYPE is `left' or `top'.
INCR is the increment to use when changing the position."
  (let ((new-pos
         (+ incr (cadr (frame-geom-value-cons type (cdr (assq type (frame-parameters frame)))))))
        (display-dimension
         (if (eq 'left type)
             (available-screen-pixel-width t)
           (available-screen-pixel-height t)))
        (frame-dimension
         (if (eq 'left type) (frame-pixel-width frame) (frame-pixel-height frame))))
    (if (not move-frame-wrap-within-display-flag)
        new-pos
      (when (< new-pos (- frame-dimension)) (setq new-pos display-dimension))
      (when (> new-pos display-dimension) (setq new-pos (- frame-dimension)))
      new-pos)))

;;; This was a workaround hack for an Emacs 23 bug (#119, aka #1562).
;;; This works OK, but it is not as refined as the version I use, and it does not work for
;;; older Emacs versions.
;;;
;;; (when (> emacs-major-version 22)
;;;   (defun enlarge-font (&optional increment frame)
;;;     "Increase size of font in FRAME by INCREMENT.
;;; Interactively, INCREMENT is given by the prefix argument.
;;; Optional FRAME parameter defaults to current frame."
;;;     (interactive "p")
;;;     (setq frame (or frame (selected-frame)))
;;;     (set-face-attribute
;;;      'default frame :height (+ (* 10 increment)
;;;                                (face-attribute 'default :height frame 'default)))))


;; This still doesn't work 100% well.  For instance, set frame font to
;; "-raster-Terminal-normal-r-normal-normal-12-90-96-96-c-50-ms-oemlatin", then decrease font size.
;; The next smaller existing font on my machine is
;; "-raster-Terminal-normal-r-normal-normal-11-*-96-96-c-*-ms-oemlatin".  Decrease size again.
;; Next smaller font is "-raster-Terminal-bold-r-normal-normal-5-37-96-96-c-60-ms-oemlatin".  Notice
;; the switch to bold from regular.  Cannot decrease any more.  Increase size.  Next larger font is
;; "-raster-Terminal-bold-r-normal-normal-8-*-96-96-c-*-ms-oemlatin".  Can no longer increase size.
;;
;;;###autoload
(defun enlarge-font (&optional increment frame)
  "Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (setq frame (or frame (selected-frame)))
  (let ((fontname (cdr (assq 'font (frame-parameters frame))))
        (count enlarge-font-tries))
    (setq fontname (enlarged-font-name fontname frame increment))
    (while (and (not (x-list-fonts fontname)) (wholenump (setq count (1- count))))
      (setq fontname (enlarged-font-name fontname frame increment)))
    (unless (x-list-fonts fontname) (error "Cannot change font size"))
    (modify-frame-parameters frame (list (cons 'font fontname)))
    ;; Update faces that want a bold or italic version of the default font.
    (when (< emacs-major-version 21) (frame-update-faces frame))))

(defun enlarged-font-name (fontname frame increment)
  "FONTNAME, after enlarging font size of FRAME by INCREMENT.
FONTNAME is the font of FRAME."
  (when (query-fontset fontname)
    (let ((ascii (assq 'ascii (aref (fontset-info fontname frame) 2))))
      (when ascii (setq fontname (nth 2 ascii)))))
  (let ((xlfd-fields (x-decompose-font-name fontname)))
    (unless xlfd-fields (error "Cannot decompose font name"))
    (let ((new-size (+ (string-to-number (aref xlfd-fields xlfd-regexp-pixelsize-subnum))
                       increment)))
      (unless (> new-size 0) (error "New font size is too small: %s" new-size))
      (aset xlfd-fields xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
    ;; Set point size & width to "*", so frame width will adjust to new font size
    (aset xlfd-fields xlfd-regexp-pointsize-subnum "*")
    (aset xlfd-fields xlfd-regexp-avgwidth-subnum "*")
    (x-compose-font-name xlfd-fields)))

;;;###autoload
(defun set-frame-alist-parameter-from-frame (alist parameter &optional frame)
  "Set PARAMETER of frame alist ALIST to its current value in FRAME.
FRAME defaults to the selected frame.  ALIST is a variable (symbol)
whose value is an alist of frame parameters."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                   (and (symbolp (variable-at-point)))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Frame alist to change (variable): "
                                    (frame-alist-var-names) nil t nil nil 'default-frame-alist t))
           (intern (completing-read "Parameter to set:"
                                    (frame-parameter-names) nil t nil nil 'left t))
           (get-a-frame (read-frame "Frame to copy parameter value from: " nil t)))))
  (unless (boundp alist)
    (error "Not a defined Emacs variable: `%s'" alist))
  (set alist (assq-delete-all parameter (copy-alist (eval alist))))
  (set alist (cons (assq parameter (frame-parameters frame)) (eval alist)))
  (tell-customize-var-has-changed alist))

;;; Standard Emacs 21+ function, defined here for Emacs 20.
(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (while (and (consp (car alist)) (eq (car (car alist)) key)) (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr)) (eq (car (car tail-cdr)) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))

;;;###autoload
(defun set-all-frame-alist-parameters-from-frame (alist &optional frame really-all-p)
  "Set frame parameters of ALIST to their current values in FRAME.
Unless optional argument REALLY-ALL-P (prefix arg) is non-nil, the
frame parameters in list `frame-parameters-to-exclude' are
excluded: they are not copied from FRAME to ALIST.
ALIST is a variable (symbol) whose value is an alist of frame parameters.
FRAME defaults to the selected frame."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                   (and (symbolp (variable-at-point)))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Frame alist to change (variable): "
                                    (frame-alist-var-names) nil t nil nil 'default-frame-alist t))
           (get-a-frame (read-frame "Frame to copy parameter values from: " nil t))
           current-prefix-arg)))
  (unless (boundp alist)
    (error "Not a defined Emacs variable: `%s'" alist))
  (set alist (set-difference (frame-parameters frame)
                             (and (not really-all-p) frame-parameters-to-exclude)
                             :key 'car))
  (tell-customize-var-has-changed alist))

(defun frame-alist-var-names ()
  "Return an alist of all variable names that end in \"frame-alist\".
The CAR of each list item is a string variable name.
The CDR is nil."
  (let ((vars nil))
    (mapatoms (lambda (sym) (and (boundp sym)
                                 (setq sym (symbol-name sym))
                                 (string-match "frame-alist$" sym)
                                 (push (list sym) vars))))
    vars))

(defun frame-parameter-names ()
  "Return an alist of all available frame-parameter names.
The CAR of each list item is a string parameter name.
The CDR is nil."
  (let ((params '(("display") ("title") ("name") ("left") ("top") ("icon-left") ("icon-top")
                  ("user-position") ("height") ("width") ("window-id") ("minibuffer")
                  ("buffer-predicate") ("buffer-list") ("font") ("auto-raise") ("auto-lower")
                  ("vertical-scroll-bars") ("horizontal-scroll-bars") ("scroll-bar-width")
                  ("icon-type") ("icon-name") ("foreground-color") ("background-color")
                  ("background-mode") ("mouse-color") ("cursor-color") ("border-color")
                  ("display-type") ("cursor-type") ("border-width") ("internal-border-width")
                  ("unsplittable") ("visibility") ("menu-bar-lines"))))
    (when (> emacs-major-version 20)
      (setq params (nconc params '("fullscreen" "outer-window-id" "tty-color-mode" "left-fringe"
                                   "right-fringe" "tool-bar-lines" "screen-gamma" "line-spacing"
                                   "wait-for-wm" "scroll-bar-foreground" "scroll-bar-background"))))
    (when (> emacs-major-version 21) (setq params (nconc params '("user-size"))))
    (when (> emacs-major-version 22)
      (setq params (nconc params '("display-environment-variable" "term-environment-variable"))))
    params))

;;;###autoload
(defun tell-customize-var-has-changed (variable)
  "Tell Customize to recognize that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option."
  (interactive "vVariable: ")
  (put variable 'customized-value (list (custom-quote (eval variable)))))

;;;###autoload
(defun other-window-or-frame (arg)
  "`other-frame', if `one-window-p'; otherwise, `other-window'."
  (interactive "p")
  (if (one-window-p) (other-frame arg) (other-window arg)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame-cmds.el ends here

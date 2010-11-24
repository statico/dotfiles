;;; icicles-cmd2.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd2.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Thu May 21 13:31:43 2009 (-0700)
;; Version: 22.0
;; Last-Updated: Wed Nov 10 17:17:21 2010 (-0800)
;;           By: dradams
;;     Update #: 2327
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd2.el
;; Keywords: extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `doremi', `easymenu',
;;   `el-swank-fuzzy', `ffap', `ffap-', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `hexrgb', `icicles-cmd1',
;;   `icicles-face', `icicles-fn', `icicles-mcmd', `icicles-opt',
;;   `icicles-var', `kmacro', `levenshtein', `misc-fns', `mwheel',
;;   `pp', `pp+', `regexp-opt', `ring', `ring+', `strings',
;;   `thingatpt', `thingatpt+', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  This is a continuation of library
;;  `icicles-cmd1.el' (a single file for all top-level commands would
;;  be too large to upload to Emacs Wiki).
;;
;;  For commands to be used mainly in the minibuffer or buffer
;;  *Completions*, see `icicles-mcmd.el'.
;;
;;  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  If you use the byte-compiled version of this library,
;;  `icicles-cmd2.elc', in Emacs 23, then it must be byte-compiled
;;  using Emacs 23.  Otherwise, Icicles key completion (and perhaps
;;  other things?) will not work correctly.
;;
;;  Macros defined here:
;;
;;    `icicle-define-search-bookmark-command'.
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+)`a', (+)`any', (+)`buffer', (+)`file', (+)`icicle-anything',
;;    (+)`icicle-apply', `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    (+)`icicle-comint-command', (+)`icicle-comint-search',
;;    (+)`icicle-compilation-search', (+)`icicle-complete-keys',
;;    `icicle-complete-thesaurus-entry',
;;    (+)`icicle-describe-option-of-type', (+)`icicle-doc',
;;    (+)`icicle-exchange-point-and-mark', (+)`icicle-font',
;;    (+)`icicle-frame-bg', (+)`icicle-frame-fg', (+)`icicle-fundoc',
;;    (+)`icicle-goto-global-marker',
;;    (+)`icicle-goto-global-marker-or-pop-global-mark',
;;    (+)`icicle-goto-marker',
;;    (+)`icicle-goto-marker-or-set-mark-command', (+)`icicle-imenu',
;;    (+)`icicle-imenu-command',
;;    (+)`icicle-imenu-non-interactive-function',
;;    `icicle-ido-like-mode', (+)`icicle-Info-goto-node',
;;    (+)`icicle-Info-goto-node-cmd', (+)`icicle-Info-index',
;;    (+)`icicle-Info-index-20', (+)`icicle-Info-index-cmd',
;;    (+)`icicle-Info-menu', `icicle-Info-menu-cmd',
;;    `icicle-Info-virtual-book', `icicle-insert-char',
;;    (+)`icicle-insert-thesaurus-entry', (+)`icicle-keyword-list',
;;    (+)`icicle-map', `icicle-non-whitespace-string-p',
;;    (+)`icicle-object-action', (+)`icicle-occur', (+)`icicle-plist',
;;    `icicle-read-color', `icicle-read-kbd-macro',
;;    (+)`icicle-regexp-list', `icicle-save-string-to-variable',
;;    (+)`icicle-search', (+)`icicle-search-all-tags-bookmark',
;;    (+)`icicle-search-all-tags-regexp-bookmark',
;;    (+)`icicle-search-bookmark',
;;    (+)`icicle-search-bookmark-list-bookmark',
;;    (+)`icicle-search-bookmarks-together',
;;    (+)`icicle-search-buffer', (+)`icicle-search-buff-menu-marked',
;;    (+)`icicle-search-char-property', (+)`icicle-search-defs',
;;    (+)`icicle-search-desktop-bookmark',
;;    (+)`icicle-search-dired-bookmark',
;;    (+)`icicle-search-dired-marked', (+)`icicle-search-file',
;;    (+)`icicle-search-file-bookmark', (+)`icicle-search-generic',
;;    (+)`icicle-search-gnus-bookmark',
;;    `icicle-search-highlight-cleanup',
;;    (+)`icicle-search-ibuffer-marked',
;;    (+)`icicle-search-info-bookmark', (+)`icicle-search-keywords',
;;    (+)`icicle-search-lines',
;;    (+)`icicle-search-local-file-bookmark',
;;    (+)`icicle-search-man-bookmark',
;;    (+)`icicle-search-non-file-bookmark',
;;    (+)`icicle-search-overlay-property',
;;    (+)`icicle-search-paragraphs', (+)`icicle-search-pages',
;;    (+)`icicle-search-region-bookmark',
;;    (+)`icicle-search-remote-file-bookmark',
;;    (+)`icicle-search-sentences',
;;    (+)`icicle-search-some-tags-bookmark',
;;    (+)`icicle-search-some-tags-regexp-bookmark',
;;    (+)`icicle-search-specific-buffers-bookmark',
;;    (+)`icicle-search-specific-files-bookmark',
;;    (+)`icicle-search-text-property',
;;    (+)`icicle-search-this-buffer-bookmark',
;;    (+)`icicle-search-url-bookmark',
;;    (+)`icicle-search-w3m-bookmark', (+)`icicle-search-word',
;;    (+)`icicle-select-frame', `icicle-select-frame-by-name',
;;    `icicle-set-S-TAB-methods-for-command',
;;    `icicle-set-TAB-methods-for-command', (+)`icicle-tags-search',
;;    (+)`icicle-vardoc', (+)`icicle-where-is', (+)`what-which-how'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-add-key+cmd', `icicle-anything-candidate-value',
;;    `icicle-apply-action', `icicle-apply-list-action',
;;    `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type', `icicle-comint-hook-fn',
;;    `icicle-comint-search-get-final-choice',
;;    `icicle-comint-search-get-minibuffer-input',
;;    `icicle-comint-search-send-input', `icicle-compilation-hook-fn',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-keys-1', `icicle-complete-keys-action',
;;    `icicle-describe-opt-action',
;;    `icicle-describe-opt-of-type-complete', `icicle-doc-action',
;;    `icicle-edmacro-parse-keys', `icicle-flat-list',
;;    `icicle-fn-doc-minus-sig', `icicle-font-w-orig-size',
;;    `icicle-funvardoc-action',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-input-delay',
;;    `icicle-get-anything-req-pat-chars',
;;    `icicle-get-anything-types', `icicle-goto-marker-1',
;;    `icicle-goto-marker-1-action', `icicle-group-regexp',
;;    `icicle-imenu-command-p', `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-Info-book-order-p',
;;    `icicle-Info-build-node-completions',
;;    `icicle-Info-build-node-completions-1',
;;    `icicle-Info-goto-node-1', `icicle-Info-goto-node-action',
;;    `icicle-Info-index-action', `icicle-Info-read-node-name',
;;    `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-keys+cmds-w-prefix', `icicle-marker+text',
;;    `icicle-markers', `icicle-next-single-char-property-change',
;;    `icicle-read-args-for-set-completion-methods',
;;    `icicle-read-single-key-description',
;;    `icicle-read-var-value-satisfying',
;;    `icicle-region-or-buffer-limits', `icicle-search-action',
;;    `icicle-search-action-1', `icicle-search-bookmark-action',
;;    `icicle-search-char-property-scan',
;;    `icicle-search-choose-buffers', `icicle-search-cleanup',
;;    `icicle-search-define-candidates',
;;    `icicle-search-define-candidates-1', `icicle-search-final-act',
;;    `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-and-maybe-replace',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-in-context-default-fn',
;;    `icicle-search-property-args', `icicle-search-quit-or-error',
;;    `icicle-search-read-context-regexp', `icicle-search-read-word',
;;    `icicle-search-regexp-scan',
;;    `icicle-search-replace-all-search-hits',
;;    `icicle-search-replace-cand-in-alist',
;;    `icicle-search-replace-cand-in-mct',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match',
;;    `icicle-search-replace-search-hit', `icicle-search-where-arg',
;;    `icicle-set-completion-methods-for-command',
;;    `icicle-this-command-keys-prefix'.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-face', `customize-face-other-window' - Multi-commands.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in
;;  `icicles-doc2.el'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Icicles Top-Level Commands, Part 2")
 
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

(eval-when-compile (require 'cl)) ;; loop
                                  ;; plus, for Emacs < 21: dolist, push
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (require 'comint))
  ;; comint-check-proc, comint-copy-old-input, comint-get-old-input, comint-input-ring,
  ;; comint-prompt-regexp, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node
(eval-when-compile (require 'etags)) ;; tags-case-fold-search, tags-table-files,
                                     ;; visit-tags-table-buffer

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (when (> emacs-major-version 21)
                     (require 'anything nil t))) ;; (no error if not found):
  ;; anything-candidate-cache, anything-get-sources, anything-idle-delay, anything-pattern,
  ;; anything-sources, anything-transform-candidates
(eval-when-compile (require 'yow nil t)) ;; (no error if not found):
  ;; apropos-zippy, yow-after-load-message, yow-file, yow-load-message
(eval-when-compile (require 'cookie1 nil t)) ;; (no error if not found): cookie-cache
(require 'apropos-fn+var nil t) ;; (no error if not found):
  ;; apropos-command, apropos-function, apropos-option, apropos-variable
(require 'hexrgb nil t)  ;; (no error if not found): hexrgb-color-name-to-hex, hexrgb-read-color
(require 'strings nil t) ;; (no error if not found): read-number (my version)

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-define-command
(require 'icicles-mcmd) ;; icicle-search-define-replacement
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-entry-fn, icicle-candidate-nb,
  ;; icicle-candidates-alist, icicle-char-property-value-history, icicle-complete-keys-alist,
  ;; icicle-completion-candidates, icicle-current-input, icicle-extra-candidates,
  ;; icicle-get-alist-candidate-function, icicle-must-match-regexp, icicle-must-not-match-regexp,
  ;; icicle-must-pass-predicate, icicle-saved-completion-candidates, icicle-search-command,
  ;; icicle-search-current-overlay, icicle-search-final-choice, icicle-search-overlays,
  ;; icicle-search-refined-overlays
(require 'icicles-opt)
  ;; icicle-alternative-sort-comparer, icicle-buffer-extras, icicle-buffer-ignore-space-prefix-flag,
  ;; icicle-buffer-match-regexp, icicle-buffer-no-match-regexp, icicle-buffer-predicate,
  ;; icicle-buffer-require-match-flag, icicle-buffer-sort, icicle-complete-keys-self-insert-flag,
  ;; icicle-ignore-space-prefix-flag, icicle-key-descriptions-use-<>-flag,
  ;; icicle-require-match-flag, icicle-saved-completion-sets,
  ;; icicle-search-cleanup-flag, icicle-search-highlight-all-current-flag,
  ;; icicle-search-highlight-threshold, icicle-search-hook, icicle-sort-comparer,
  ;; icicle-transform-function
(require 'icicles-fn) ;; icicle-candidate-short-help, icicle-completing-read-history,
  ;; icicle-highlight-lighter, icicle-insert-cand-in-minibuffer, icicle-kill-a-buffer
(require 'icicles-cmd1) ;; custom-variable-p, icicle-bookmark-cleanup,
  ;; icicle-bookmark-cleanup-on-quit, icicle-bookmark-cmd, icicle-bookmark-help-string,
  ;; icicle-bookmark-history, icicle-bookmark-propertize-candidate, icicle-buffer-list,
  ;; icicle-explore, icicle-face-list, icicle-file-list, icicle-make-frame-alist,
  ;; icicle-select-bookmarked-region



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(defvar anything-sources)               ; In `anything.el'
(defvar anything-candidate-cache)       ; In `anything.el'
(defvar anything-idle-delay)            ; In `anything.el'
(defvar icicle-search-ecm)              ; In `icicle-search'
(defvar icicle-track-pt)                ; In `icicle-insert-thesaurus-entry'

;; (< emacs-major-version 21)
(defvar tooltip-mode)                   ; In `tooltip.el'

;; (< emacs-major-version 22)
(defvar compilation-current-error)
(defvar cookie-cache)
(defvar icicle-complete-keys-alist)   ; In `icicles-var.el'
(defvar Info-menu-entry-name-re)      ; In `info.el'
(defvar Info-read-node-completion-table) ; In `info.el'
(defvar tags-case-fold-search)        ; In `etags.el'
(defvar yow-after-load-message)
(defvar yow-file)
(defvar yow-load-message)

;; (> emacs-major-version 21)
(defvar Info-saved-nodes)               ; In `info+.el'

;; (< emacs-major-version 23)
(defvar read-buffer-completion-ignore-case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Icicles Top-Level Commands, Part 2")
;;; Icicles Top-Level Commands, Part 2 .   .   .   .   .   .   .   .   .


;;;###autoload
(icicle-define-command icicle-font      ; Command name
  "Change font of current frame."       ; Doc string
  (lambda (font) (modify-frame-parameters orig-frame (list (cons 'font font)))) ; Action function
  "Font: "                              ; `completing-read' args
  (let ((fonts  ())
        fws)
    (dolist (ft  (x-list-fonts "*")  fonts) ; Just avoiding two traversals, one to remove nil elts.
      (when (setq fws  (icicle-font-w-orig-size ft)) (push fws fonts)))) ; Ignore nil entries.
  nil t nil (if (boundp 'font-name-history) 'font-name-history 'icicle-font-name-history) nil nil
  ((orig-frame           (selected-frame)) ; Bindings
   (orig-font            (frame-parameter nil 'font))
   (orig-pixelsize       (aref (x-decompose-font-name orig-font) xlfd-regexp-pixelsize-subnum))
   (orig-pointsize       (aref (x-decompose-font-name orig-font) xlfd-regexp-pointsize-subnum))
   (orig-menu-bar        (assq 'menu-bar-lines (frame-parameters orig-frame))))
  ;; First code - remove menu-bar, to avoid Emacs bug that resizes frame.
  (modify-frame-parameters orig-frame (list '(menu-bar-lines . 0)))
  (modify-frame-parameters orig-frame (list (cons 'font orig-font) orig-menu-bar)) ; Undo code.
  (modify-frame-parameters orig-frame (list orig-menu-bar))) ; Last code.

;; Free var here: `orig-pixelsize' is bound in `icicle-font'.
(defun icicle-font-w-orig-size (font)
  "Return a font the same as FONT, but with pixel size `orig-pixelsize'.
Return nil if `x-decompose-font-name' returns nil for FONT.
`orig-pixelsize' is the original pixel size for `icicle-font'."
  (let ((xlfd-fields  (x-decompose-font-name font)))
    (if (not xlfd-fields)               ; Can't handle such font names - return nil.
        nil
      (aset xlfd-fields xlfd-regexp-pixelsize-subnum orig-pixelsize)
      (aset xlfd-fields xlfd-regexp-pointsize-subnum orig-pointsize)
      (let* ((sized-font   (x-compose-font-name xlfd-fields))
             (font-info    (and (or icicle-help-in-mode-line-flag ; Get it only if user will see it.
                                    (and (boundp 'tooltip-mode) tooltip-mode))
                                (font-info sized-font)))
             (iii          (if (< emacs-major-version 21) 3 2))
             (help-string  (if font-info
                               (format "width: %s, height: %s, offset: %s, compose: %s"
                                       (aref font-info iii) (aref font-info (+ iii 1))
                                       (aref font-info (+ iii 2)) (aref font-info (+ iii 3)))
                             "Font is not yet loaded (used)")))
        (icicle-candidate-short-help help-string sized-font)
        (list sized-font)))))

;;;###autoload
(icicle-define-command icicle-frame-bg  ; Command name
  "Change background of current frame.
Read color name or hex RGB color value #RRRRGGGGBBBB with completion.
In addition to standard color names and RGB (red, green, blue) hex
values, the following are also available as proxy color candidates,
provided `icicle-add-proxy-candidates-flag' is non-nil and library
`palette.el' or `eyedropper.el' is used.  In each case, the
corresponding color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.

As always, you can toggle the use of proxy candidates using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]' in
the minibuffer.

You can complete your input against the color name, the RGB value, or
both.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, input can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  You can toggle duplicate removal at any time
using `C-$'.

During completion, candidate help (e.g. `C-M-RET') shows you the RGB
and HSV (hue, saturation, value) color components.

This command is intended only for use in Icicle mode." ; Doc string
  (lambda (color)                       ; Action function
    (modify-frame-parameters
     orig-frame (list (cons 'background-color (icicle-transform-multi-completion color)))))
  prompt named-colors nil t nil         ; `completing-read' args
  (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame                         (selected-frame)) ; Bindings
   (orig-bg                            (frame-parameter nil 'background-color))
   (prompt                             "Background color: ")
   (icicle-list-use-nth-parts          '(2)) ; Use RGB part.
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "color")))
   (icicle-all-candidates-list-alt-action-fn
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "color")))

   icicle-candidate-help-fn     completion-ignore-case             icicle-transform-function
   icicle-sort-orders-alist     icicle-list-nth-parts-join-string  icicle-list-join-string
   icicle-list-end-string       icicle-proxy-candidate-regexp      named-colors
   icicle-proxy-candidates)

  (icicle-color-completion-setup)       ; First code
  (modify-frame-parameters orig-frame (list (cons 'background-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-fg  ; Command name
  "Change foreground of current frame.
See `icicle-frame-bg' - but this is for foreground, not background." ; Doc string
  (lambda (color)                       ; Action function
    (modify-frame-parameters
     orig-frame (list (cons 'foreground-color (icicle-transform-multi-completion color)))))
  prompt named-colors nil t nil         ; `completing-read' args
  (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame                         (selected-frame)) ; Bindings
   (orig-bg                            (frame-parameter nil 'foreground-color))
   (prompt                             "Foreground color: ")
   (icicle-list-use-nth-parts          '(2)) ; Use RGB part.
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "color")))
   (icicle-all-candidates-list-alt-action-fn
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "color")))

   icicle-candidate-help-fn     completion-ignore-case             icicle-transform-function
   icicle-sort-orders-alist     icicle-list-nth-parts-join-string  icicle-list-join-string
   icicle-list-end-string       icicle-proxy-candidate-regexp      named-colors
   icicle-proxy-candidates)

  (icicle-color-completion-setup)       ; First code
  (modify-frame-parameters orig-frame (list (cons 'foreground-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;; Bind this, not `icicle-Info-index', to `i' in Info mode,
;; so plain `Info-index' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-index-cmd ()         ; Bound to `i' in Info mode.
  "If in Icicle mode, run `icicle-Info-index'; else, run `Info-index'.
Note: In Emacs versions prior to version 22, this runs `Info-index'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-index 'Info-index)))

;;;###autoload
(defun icicle-Info-index ()
  "Like `Info-index', but you can use Icicles keys `C-RET', `C-up' etc."
  (interactive)
  (when (and (boundp 'Info-current-file) (equal Info-current-file "dir"))
    (error "The Info directory node has no index; use `m' to select a manual"))
  (let ((info-buf                    (current-buffer))
        (info-window                 (selected-window))
        (icicle-candidate-action-fn  'icicle-Info-index-action)
        (C-x-m                       (lookup-key minibuffer-local-completion-map "\C-xm")))
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-info-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-info-other-window))
    (unwind-protect
         (call-interactively (if (> emacs-major-version 21) 'Info-index 'icicle-Info-index-20))
      (define-key minibuffer-local-completion-map "\C-xm" C-x-m))))

;; Thx to Tamas Patrovics for this Emacs 20 version.
;;;###autoload
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (let* ((symb (or (and (fboundp 'symbol-nearest-point) ; Defined in `thingatpt+.el'.
                        (symbol-nearest-point))
                   (symbol-at-point)))
         (topic (and symb (symbol-name symb))))
    (Info-index "")
    (let ((pattern     "\\* +\\([^:]*\\):.")
          (candidates  ()))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
      (Info-index (completing-read "Index topic: " candidates nil t nil nil topic)))))

;; Free vars here: `info-buf' and `info-window' are bound in `icicle-Info-index'.
(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win  (selected-window)))
    (set-buffer info-buf)
    (select-window info-window)
    (Info-index topic)
    (select-window minibuf-win)))

;; Bind this, not `icicle-Info-menu', to `m' in Info mode,
;; so plain `Info-menu' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-menu-cmd ()    ; Bound to `m' in Info mode.
  "In Icicle mode, run `icicle-Info-menu'; else, `Info-menu'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-menu 'Info-menu)))

;; Free vars here: `Info-menu-entry-name-re' is bound in `info.el'.
(icicle-define-command icicle-Info-menu
  "Go to a menu node."                  ; Doc string
  (lambda (m) (icicle-Info-goto-node (cdr (funcall icicle-get-alist-candidate-function m)))) ; Action
  "Menu item: " icicle-candidates-alist ; `completing-read' args
  nil t nil nil (save-excursion
                  (goto-char (point-min))
                  (unless (search-forward "\n* menu:" nil t) (error "No menu in this node"))
                  (setq menu-eol  (point))
                  (and (< menu-eol opoint)
                       (save-excursion
                         (goto-char opoint) (end-of-line)
                         (and (re-search-backward (concat "\n\\* +\\("
                                                          (if (boundp 'Info-menu-entry-name-re)
                                                              Info-menu-entry-name-re
                                                            "[^:\t\n]*")
                                                          "\\):")
                                                  menu-eol t)
                              (match-string-no-properties 1)))))
  nil
  ((opoint                                 (point)) ; Bindings
   (completion-ignore-case                 t)
   (case-fold-search                       t)
   (icicle-sort-comparer                   nil)
   (icicle-whole-candidate-as-text-prop-p  t)
   (Info-complete-menu-buffer              (current-buffer))
   (icicle-candidates-alist                (mapcar #'(lambda (m) (cons m (Info-extract-menu-item m)))
                                                   (reverse
                                                    (all-completions "" 'Info-complete-menu-item))))
   menu-eol))

;; Bind this, not `icicle-Info-goto-node', to `g' in Info mode,
;; so plain `Info-goto-node' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-goto-node-cmd ()     ; Bound to `g' in Info mode.
  "In Icicle mode, run `icicle-Info-goto-node'; else, `Info-goto-node'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-goto-node 'Info-goto-node)))

;;;###autoload
(defun icicle-Info-goto-node (nodename &optional arg)
  "Go to Info node named NODENAME.
NODENAME has the form NODE or (FILE)NODE-IN-FILE, where:
 NODE names a node in the current Info file or one of its subfiles.
 FILE names an Info file containing node NODE-IN-FILE.
Completion is available for node names in the current Info file.

With a prefix argument:

 * Plain `C-u' means prepend the current Info file name (manual name)
   to each node name.  For example: `(emacs)Paragraphs' instead of
   just `Paragraphs'.

 * A negative numeric prefix arg (e.g. `C--') means present completion
   candidates in book order, and limit the candidates to the current
   node and the rest of the book following it.  In this case, the
   first candidate is `..', which means go up.
   
 * A negative numeric prefix arg (e.g. `C-1') means show the target
   node in a new Info buffer (not available prior to Emacs 21).
   (This applies only to the final completion choice, not to
   intermediate candidate actions using, e.g., `C-RET'.)

In Lisp code, if optional argument ARG is a string, then show the node
in a new Info buffer named `*info-ARG*'.

With no prefix argument, or with a non-negative prefix arg, you can
use `C-,' to choose how to sort completion candidates.  By default,
they are sorted alphabetically.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

`C-mouse-2', `C-RET' - Go to current completion candidate (node)
`C-down'  - Go to next completion candidate
`C-up'    - Go to previous completion candidate
`C-next'  - Go to next apropos-completion candidate
`C-prior' - Go to previous apropos-completion candidate
`C-end'   - Go to next prefix-completion candidate
`C-home'  - Go to previous prefix-completion candidate

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'."
  (interactive
   (let* ((info-buf                         (current-buffer))
          (info-window                      (selected-window))
          (icicle-candidate-action-fn       'icicle-Info-goto-node-action)
          (icicle-Info-only-rest-of-book-p  (< (prefix-numeric-value current-prefix-arg) 0))
          (icicle-sort-orders-alist         (cons '("in book order" .  icicle-Info-book-order-p)
                                                  icicle-sort-orders-alist))
          (icicle-sort-comparer             (if icicle-Info-only-rest-of-book-p
                                                #'icicle-Info-book-order-p
                                              icicle-sort-comparer)))
     (list (icicle-Info-read-node-name "Go to node: " (consp current-prefix-arg))
           current-prefix-arg)))
  (icicle-Info-goto-node-1 nodename arg))

(defun icicle-Info-goto-node-1 (nodename &optional arg)
  "Same as `Info-goto-node', but go up for `..' pseudo-node."
  (if (and (string= nodename "..") (Info-check-pointer "up"))
      (Info-up)
    (if (> emacs-major-version 20)
        (Info-goto-node nodename (and (not icicle-Info-only-rest-of-book-p) arg))
      (Info-goto-node nodename))))

(defun icicle-Info-read-node-name (prompt &optional include-file-p)
  "Read a node name, prompting with PROMPT.
Non-nil INCLUDE-FILE-P means include current Info file in the name.
You can use `C-x m' during completion to access Info bookmarks, if you
 use library `bookmark+.el'."
  (let ((C-x-m  (lookup-key minibuffer-local-completion-map "\C-xm")))
    (when (and (require 'bookmark+ nil t) (fboundp 'icicle-bookmark-info-other-window))
      (define-key minibuffer-local-completion-map "\C-xm" 'icicle-bookmark-info-other-window))
    (unwind-protect
         (let* ((completion-ignore-case           t)
                (Info-read-node-completion-table  (icicle-Info-build-node-completions include-file-p))
                (nodename                         (completing-read prompt 'Info-read-node-name-1
                                                                   nil nil)))
           (if (equal nodename "") (icicle-Info-read-node-name prompt) nodename))
      (define-key minibuffer-local-completion-map "\C-xm" C-x-m))))

(defun icicle-Info-build-node-completions (&optional include-file-p)
  "Build completions list for Info nodes.
This takes `icicle-Info-only-rest-of-book-p' into account.
Non-nil INCLUDE-FILE-P means include current Info file in the name."
  (icicle-highlight-lighter)
  (if (or (not icicle-Info-only-rest-of-book-p) (string= Info-current-node "Top"))
      (icicle-Info-build-node-completions-1 include-file-p)
    (reverse (cons '("..")
                   (member (list Info-current-node)
                           (reverse (icicle-Info-build-node-completions-1 include-file-p)))))))

(defun icicle-Info-build-node-completions-1 (&optional include-file-p)
  "Helper function for `icicle-Info-build-node-completions'.
Use `Info-build-node-completions' to build node list for completion.
Non-nil INCLUDE-FILE-P means include current Info file in the name.
Remove pseudo-node `*'.  (This just fixes a bug in Emacs 21 and 22.1.)"
  (let ((comps  (Info-build-node-completions)))
    (when (equal (car comps) '("*")) (setq comps  (cdr comps)))
    (if include-file-p
        (let ((file  (concat "(" (cond ((stringp Info-current-file)
                                        (replace-regexp-in-string
                                         "%" "%%" (file-name-nondirectory Info-current-file)))
                                       (Info-current-file (format "*%S*" Info-current-file))
                                       (t ""))
                             ")")))
          (mapcar #'(lambda (node) (cons (concat file (car node)) (cdr node))) comps))
      comps)))

;; Free vars here:
;; `info-buf' and `info-window' are bound in `icicle-Info-goto-node'.
;; `Info-read-node-completion-table' is bound in `info.el'.
(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (set-buffer info-buf)
  (select-window info-window)
  (icicle-Info-goto-node-1 node)
  (when icicle-Info-only-rest-of-book-p
    (setq Info-read-node-completion-table  (icicle-Info-build-node-completions)
          icicle-current-input             "")
    (icicle-complete-again-update)
    (if (and (string= Info-current-node "Top") Info-history)
        (let* ((hist  Info-history)
               (last  (car (cdr (car hist)))))
          (while (string= "Top" (car (cdr (car hist)))) (pop hist))
          (setq icicle-candidate-nb
                (1- (length (reverse (member (list (car (cdr (car hist))))
                                             (icicle-Info-build-node-completions-1)))))))
      (setq icicle-candidate-nb  1))     ; Skip `..'.

    ;; $$$$$$ Maybe factor this out. Same thing in several places.  However, here we don't do
    ;; `icicle-maybe-sort-and-strip-candidates' at beginning of first clause.
    (cond ((and icicle-completion-candidates (cdr icicle-completion-candidates)) ; > 1 left.
           (message "Displaying completion candidates...")
           (save-selected-window (icicle-display-candidates-in-Completions))
           (with-current-buffer "*Completions*"
             (goto-char (icicle-start-of-candidates-in-Completions))
             (icicle-move-to-next-completion
              (mod icicle-candidate-nb (length icicle-completion-candidates)))
             (set-window-point (get-buffer-window "*Completions*" 0) (point))
             (setq icicle-last-completion-candidate  (icicle-current-completion-in-Completions))
             (set-buffer-modified-p nil)))
          (icicle-completion-candidates ; Single candidate left
           (save-selected-window (icicle-remove-Completions-window))
           (let ((completion  (icicle-transform-multi-completion
                               (car icicle-completion-candidates))))
             (select-window (active-minibuffer-window))
             (with-current-buffer (window-buffer) ; Need if *Completions* redirected to minibuffer.
               (goto-char (icicle-minibuffer-prompt-end))
               (icicle-clear-minibuffer)
               (insert (if (and (icicle-file-name-input-p) insert-default-directory
                                (or (not (member completion icicle-extra-candidates))
                                    icicle-extra-candidates-dir-insert-p))
                           (icicle-file-name-directory-w-default icicle-current-input)
                         "")
                       completion))))
          (t                            ; No candidates left
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)))))
  (select-window (active-minibuffer-window)))

(defun icicle-Info-book-order-p (s1 s2)
  "Non-nil if Info node S1 comes before node S2 in the book."
  t)        ; This just reverses the default order, which is reversed.

(when (> emacs-major-version 21)
  (defun icicle-Info-virtual-book (nodeset)
    "Open Info on a virtual book of saved Info nodes.
You need library `info+.el' to use this command.
With a prefix arg, you are prompted to choose a persistent saved
 completion set from `icicle-saved-completion-sets'.  The set you
 choose should be a set of saved Info node names.
With no prefix arg, use `icicle-saved-completion-candidates', which
 should be a set of Info node names.  If that is empty, then use
 `Info-saved-nodes'.
Non-interactively, argument NODESET is a list of Info node names."
    (interactive
     (progn (unless (and (require 'info+ nil t) (fboundp 'Info-virtual-book))
              (error "You need library `info+.el' for this command"))
            (list (if (not current-prefix-arg)
                      "Virtual Book"
                    (save-selected-window
                      (completing-read "Saved Info node set: " icicle-saved-completion-sets nil t nil
                                       'icicle-completion-set-history))))))
    (let ((nodes  (and (consp nodeset) nodeset))) ; (), if interactive - NODESET is a string then.
      (when (interactive-p)
        (if (not current-prefix-arg)
            (setq nodes  icicle-saved-completion-candidates)
          (let ((file-name  (cdr (assoc nodeset icicle-saved-completion-sets))))
            (unless (icicle-file-readable-p file-name)
              (error "Cannot read cache file `%s'" file-name))
            (let ((list-buf  (find-file-noselect file-name 'nowarn 'raw)))
              (unwind-protect
                   (condition-case icicle-Info-virtual-book
                       (when (listp (setq nodes  (read list-buf)))
                         (message "Set `%s' read from file `%s'" nodeset file-name))
                     (error (error "Bad cache file.  %s"
                                   (error-message-string icicle-Info-virtual-book))))
                (kill-buffer list-buf))
              (unless (consp nodes) (error "Bad data in cache file `%s'" file-name))))))
      (unless nodes (setq nodes  Info-saved-nodes)) ; In `info+.el'.
      (unless (and nodes (stringp (car nodes))) (error "No saved Info nodes")) ; Minimal check.
      (unless (stringp nodeset) (setq nodeset "Virtual Book")) ; Non-interactive - NODESET is a list.
      (Info-virtual-book nodeset nodes))))

;;;###autoload
(icicle-define-command icicle-insert-thesaurus-entry ; Command name
  "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-insert-thesaurus-entry-cand-fn ; Action function
  "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
  nil nil nil 'icicle-dictionary-history nil nil
  ((icicle-track-pt  (point)))          ; Bindings
  (progn                                ; First code
    (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
      (error "You must first load library `synonyms.el'"))
    (synonyms-ensure-synonyms-read-from-cache))
  (when (window-live-p orig-window)     ; Undo code
    (select-window orig-window)
    (select-frame-set-input-focus (selected-frame))
    (goto-char icicle-track-pt))
  (when (window-live-p orig-window)     ; Last code
    (select-window orig-window)
    (select-frame-set-input-focus (selected-frame))
    (goto-char icicle-track-pt)))

;; Free vars here: `orig-buff' is bound in `icicle-insert-thesaurus-entry'.
(defun icicle-insert-thesaurus-entry-cand-fn (string)
  "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
  (set-buffer orig-buff)
  (goto-char icicle-track-pt)
  (insert string " ")
  (setq icicle-track-pt  (point))
  (with-current-buffer (window-buffer (minibuffer-window)) (icicle-clear-minibuffer))
  (save-selected-window (icicle-remove-Completions-window)))

;;;###autoload
(defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
  "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
  (interactive (list (word-at-point)))
  (unless word (error "No word at point to complete"))
  (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
    (error "You must first load library `synonyms.el'"))
  (synonyms-ensure-synonyms-read-from-cache)
  (when (and (looking-at "\\b") (not (looking-at "\\s-"))) (forward-word 1))
  (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
  (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                           nil nil word 'icicle-dictionary-history word))
  (unless (looking-at "\\s-") (insert " ")))

;;;###autoload
(icicle-define-command icicle-where-is  ; Command name
  "Show keyboard/menu/mouse sequences that invoke specified command.
This is a multi-command version of `where-is'.

With no prefix argument, only commands actually bound to keys are
completion candidates.  With a prefix argument, all commands are
candidates.  NOTE: This is a significant difference from vanilla
`where-is', which shows all commands as candidates, even those that
are not bound.

With a plain (non-numeric) prefix argument, `C-u', insert the message
in the current buffer.  (This is the same for vanilla `where-is'.)

By default, Icicle mode remaps all key sequences that are normally
bound to `where-is' to `icicle-where-is'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'." ; Doc string
  (lambda (x) (let ((symb  (intern-soft x))) ; Action function
                (where-is symb (and pref-arg (consp pref-arg)))))
  (if pref-arg "Where is command: " "Where is bound command: ")
  obarray nil t nil nil ; `completing-read' args
  (let ((fn  (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                 (function-called-at-point))))
    (and fn (symbol-name fn)))
  t
  ((pref-arg  current-prefix-arg)       ; Bindings
   (icicle-must-pass-after-match-predicate
    (if pref-arg
        #'(lambda (c) (commandp (intern c)))
      #'(lambda (c)
          (setq c  (intern c))
          (with-current-buffer orig-buff
            (and (commandp c) (where-is-internal c overriding-local-map 'non-ascii))))))
   (icicle-candidate-help-fn
    #'(lambda (c)
        (with-current-buffer orig-buff
          (let* ((keys   (where-is-internal (intern-soft c) overriding-local-map))
                 (keys1  (mapconcat 'key-description keys ", ")))
            (message (if (string= "" keys1)
                         (format "`%s' is not on any key" c)
                       (format "`%s' is on `%s'" c keys1)))
            (sit-for 3)))))
   (icicle-candidate-alt-action-fn
    (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "command")))
   (icicle-all-candidates-list-alt-action-fn
    (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "command")))))

;;;###autoload
(icicle-define-command icicle-describe-option-of-type ; Bound to `C-h C-o'.  Command name
  "Describe a user option that was defined with a given `defcustom' type.
Enter patterns for the OPTION name and TYPE definition in the
minibuffer, separated by `icicle-list-join-string', which is \"^G^J\",
by default.  (`^G' here means the Control-g character, input using
`C-h C-g'.  Likewise, for `^J'.)

OPTION is a regexp that is matched against option names.

Depending on the prefix arg, TYPE is interpreted as either of these:

 - a regexp to match against the option type

 - a definition acceptable for `defcustom' :type, or its first symbol,
   for example, (choice (integer) (regexp)) or `choice'

In the second case, depending on the prefix arg, TYPE can be matched
against the option type, or it can be matched against either the
option type or one of its subtypes.

In the second case also, depending on the prefix arg, if TYPE does not
match some option's type, that option might still be a candidate, if
its current value satisfies TYPE.

In sum, the prefix arg determines the type-matching behavior, as
follows:

 - None:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a regexp.

 - `C-u':     OPTION is defined with TYPE or a subtype of TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Negative:  OPTION is defined with TYPE (exact match).
              TYPE is a regexp.

 - Positive:  OPTION is defined with TYPE,
                or its current value is compatible with TYPE.
              TYPE is a type definition or its first symbol.

 - Zero:      OPTION is defined with TYPE or a subtype of TYPE.
              TYPE is a type definition or its first symbol.

 - `C-u C-u': OPTION is defined with TYPE (exact match).
              TYPE is a type definition or its first symbol.

You can change these prefix-arg key sequences by customizing option
`icicle-option-type-prefix-arg-list'.  For example, if you tend to use
the matching defined here for `C-u', you might want to make that the
default behavior (no prefix arg).  You can assign any of the six
behaviors to any of the prefix-arg keys.

If TYPE is nil, then *all* options that match OPTION are candidates.

Note that options defined in libraries that have not been loaded can
be candidates, but their type will appear as nil, since it is not
known before loading the option definition.

You can match your input against the option name or the type
definition or both.  Use `C-M-j' (equivalent here to `C-q C-g C-j') to
input the default separator.

For example, to match all Icicles options whose type matches `string'
\(according to the prefix arg), use `S-TAB' with this input:

icicle.*^G
string$

If you instead want all Icicles options whose type definition contains
`string', as in (repeat string), then use this:

icicle.*^G
\[^^G]*string

Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.'  here instead of `[^^G]', then only the first lines of
type definitions are searched for `string', because `.' matches any
character except a newline.  (The first `^' in `[^^G]' is a circumflex
character.  The second `^' is part of `^G', the printed representation
of a Control-g character.)

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-describe-opt-action            ; Action function
  prompt                                ; `completing-read' args
  'icicle-describe-opt-of-type-complete nil nil nil nil nil nil
  ((prompt                             "OPTION `C-M-j' TYPE (`RET' when done): ") ; Bindings
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   ;; Bind `icicle-apropos-complete-match-fn' to nil to prevent automatic input matching
   ;; in `icicle-unsorted-apropos-candidates' etc., because `icicle-describe-opt-of-type-complete'
   ;; does everything.
   (icicle-apropos-complete-match-fn   nil)
   (icicle-candidate-help-fn           'icicle-describe-opt-action)
   ;; $$$ (icicle-highlight-input-completion-failure nil)
   (pref-arg                           current-prefix-arg))
  (progn (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
         (icicle-highlight-lighter)
         (message "Gathering user options and their types...")))

(defun icicle-describe-opt-action (opt+type)
  "Action function for `icicle-describe-option-of-type'."
  (let ((icicle-list-use-nth-parts  '(1)))
    (describe-variable (intern (icicle-transform-multi-completion opt+type)))))

;; Free var here: `pref-arg' - it is bound in `icicle-describe-option-of-type'.
(defun icicle-describe-opt-of-type-complete (strg pred completion-mode)
  "Completion function for `icicle-describe-option-of-type'.
This is used as the value of `minibuffer-completion-table'."
  (setq strg  icicle-current-input)
  ;; Parse strg into its option part and its type part: OPS  and TPS.
  ;; Make raw alist of all options and their types: ((a . ta) (b . tb)...).
  (let* ((num-prefix  (prefix-numeric-value pref-arg))
         (mode        (cond ((not pref-arg) ; No prefix arg
                             (nth 4 icicle-option-type-prefix-arg-list))
                            ((and (consp pref-arg) (= 16 num-prefix)) ; C-u C-u
                             (nth 0 icicle-option-type-prefix-arg-list))
                            ((consp pref-arg) (nth 2 icicle-option-type-prefix-arg-list)) ; C-u
                            ((zerop num-prefix) (nth 1 icicle-option-type-prefix-arg-list)) ; C-0
                            ((wholenump num-prefix) ; C-9
                             (nth 3 icicle-option-type-prefix-arg-list))
                            (t (nth 5 icicle-option-type-prefix-arg-list)))) ; C--
         (ops         (let ((icicle-list-use-nth-parts  '(1)))
                        (icicle-transform-multi-completion strg)))
         (tps         (let ((icicle-list-use-nth-parts  '(2)))
                        (icicle-transform-multi-completion strg)))
         (tp          (and (not (string= "" tps))
                           ;; Use regexp if no prefix arg or negative; else use sexp.
                           (if (memq mode '(inherit-or-regexp direct-or-regexp)) tps (read tps))))
         (result      nil))
    (mapatoms
     #'(lambda (symb)
         (when (if (fboundp 'custom-variable-p) (custom-variable-p symb) (user-variable-p symb))
           (condition-case nil
               (push (list symb (get symb 'custom-type)) result)
             (error nil)))))
    ;; Keep only candidates that correspond to input.
    (setq result
          (let ((ops-re  (if (memq icicle-current-completion-mode '(nil apropos))
                             ops
                           (concat "^" ops))))
            (icicle-remove-if-not
             #'(lambda (opt+typ)
                 (and (string-match ops-re (symbol-name (car opt+typ)))
                      (or (null tp)
                          (icicle-var-is-of-type-p (car opt+typ) (list tp)
                                                   (case mode
                                                     ((inherit inherit-or-regexp) 'inherit)
                                                     ((direct  direct-or-regexp)  'direct)
                                                     (inherit-or-value     'inherit-or-value)
                                                     (direct-or-value      'direct-or-value))))))
             result)))
    ;; Change alist entries to multi-completions: "op^G^Jtp".  Add short help for mode-line, tooltip.
    (setq result
          (mapcar #'(lambda (entry)
                      (let* ((opt+typ-string  (concat (mapconcat #'(lambda (e) (pp-to-string e))
                                                                 entry icicle-list-join-string)
                                                      icicle-list-end-string))
                             (doc       ; Don't bother to look up doc, if user won't see it.
                              (and (or icicle-help-in-mode-line-flag
                                       (and (boundp 'tooltip-mode) tooltip-mode))
                                   (documentation-property (car entry) 'variable-documentation t)))
                             (doc1  (and (stringp doc)
                                         (string-match ".+$" doc) (match-string 0 doc))))
                        (when doc1 (icicle-candidate-short-help doc1 opt+typ-string))
                        opt+typ-string))
                  result))
    (if completion-mode
        result                          ; `all-completions', `test-completion'
      (try-completion strg (mapcar #'list result) pred)))) ; `try-completion'

;;;###autoload
(icicle-define-command icicle-vardoc    ; Command name
  "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
variable name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with `S-TAB' to match all variables whose names contain \"dired\" and
whose documentation contains \"list\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

With a non-negative prefix argument, use the same documentation that
was gathered the last time `icicle-vardoc' was called.  Use a
non-negative prefix arg to save the time that would be needed to
gather the documentation.

With a non-positive prefix arg, use only user variables (options) as
candidates.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-funvardoc-action               ; Action function
  prompt                                ; `completing-read' args
  (let* ((num-arg         (prefix-numeric-value pref-arg))
         (options-only-p  (<= num-arg 0))
         (result          (and pref-arg (>= num-arg 0)
                               (if options-only-p
                                   icicle-vardoc-last-initial-option-cand-set
                                 icicle-vardoc-last-initial-cand-set))))
    (unless result                      ; COLLECTION arg is an alist whose items are ((symb doc)).
      (mapatoms #'(lambda (symb)        ; Each completion candidate is a list of strings.
                    (when (and (boundp symb)
                               (or (wholenump (prefix-numeric-value pref-arg))
                                   (user-variable-p symb)))
                      (let ((doc  (documentation-property symb 'variable-documentation)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result))))))
      (if options-only-p
          (setq icicle-vardoc-last-initial-option-cand-set  result)
        (setq icicle-vardoc-last-initial-cand-set  result)))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "VAR `C-M-j' DOC (`RET' when done): ") ; Bindings
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-candidate-help-fn           'icicle-funvardoc-action)
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering variable descriptions...")))

(defun icicle-funvardoc-action (entry)
  "Action function for `icicle-vardoc', `icicle-fundoc', `icicle-plist'."
  (with-output-to-temp-buffer "*Help*" (princ entry)))

;;;###autoload
(icicle-define-command icicle-fundoc    ; Command name
  "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string'
\(\"^G^J\", by default).  You can match an input regexp against the
function name or the documentation or both.  Use `C-M-j' (equivalent
here to `C-q C-g C-j') to input the default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with `S-TAB' to match all functions whose names contain \"dired\" and
whose documentation contains \"file\".  Here, `[^^G]' matches any
character except ^G, which includes newline.  If you use `.*' here,
instead, then only the first lines of doc strings are searched.

With a prefix argument, use the same documentation that was gathered
the last time `icicle-fundoc' was called.  Use a prefix arg to save
the time that would be needed to gather the documentation.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-funvardoc-action               ; Action function
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg icicle-fundoc-last-initial-cand-set)))
    (unless result                      ; COLLECTION arg is an alist whose items are ((symb doc)).
      (mapatoms
       #'(lambda (symb)                 ; Each completion candidate is a list of strings.
           (when (fboundp symb)
             ;; Ignore symbols that produce errors.  Example: In Emacs 20, `any', which is defalias'd
             ;; to `icicle-anything', raises this error: "Symbol's function definition is void: any".
             ;; This is caused by the `after' advice `ad-advised-docstring' that is defined by Emacs
             ;; itself for function `documentation'.  It is not a problem for Emacs 22+. 
             (let ((doc  (condition-case nil (documentation symb) (error nil))))
               (when (and doc (icicle-non-whitespace-string-p (icicle-fn-doc-minus-sig doc)))
                 (push (list (list (symbol-name symb) doc)) result))))))
      (setq icicle-fundoc-last-initial-cand-set  result))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "FUNC `C-M-j' DOC (`RET' when done): ") ; Bindings
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-candidate-help-fn           'icicle-funvardoc-action)
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering function descriptions...")))

(defun icicle-fn-doc-minus-sig (docstring)
  "Return DOCSTRING minus the function signature (usage info)."
  (let ((sig-p  (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)))
    (if sig-p (substring docstring 0 (match-beginning 0)) docstring)))

;;;###autoload
(icicle-define-command icicle-plist     ; Command name
  "Choose a symbol and its property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-M-j' (equivalent here to
`C-q C-g C-j') to input the default separator.

With a positive prefix argument, use the same initial set of
candidates that were gathered the last time `icicle-plist' was called.
Use a positive prefix arg to save the time that would be needed to
gather the plists.

With a negative prefix arg, do not pretty-print each property list, in
buffers `*Help* and `*Completions*'.  Generation of the complete set
of candidates is about twice as fast when not pretty-printed, but the
time to match your input and display candidates is the same, and the
match-and-display time for empty input,is much longer than the
generation time.

The time to repeat (positive prefix arg) is the same, whether or not
candidates were pretty-printed the first time.

Note: Plists are never pretty-printed for Emacs 20, because that seems
to cause an Emacs crash.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-funvardoc-action               ; Action function
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg (wholenump (prefix-numeric-value pref-arg))
                      icicle-plist-last-initial-cand-set)))
    (unless result                      ; COLLECTION arg: an alist with items ((symb plist-string))
      (mapatoms
       #'(lambda (symb)                 ; Each completion candidate is a list of strings.
           (condition-case nil          ; Ignore symbols that produce errors.
               (let ((plist  (symbol-plist symb)))
                 (when plist
                   (push (list (list (symbol-name symb)
                                     (if (or (< (prefix-numeric-value pref-arg) 0)
                                             (< emacs-major-version 21)) ; Emacs 20 crash if pprint.
                                         (format "%s" plist)
                                       (pp-to-string plist))))
                         result)))
             (error nil))))
      (setq icicle-plist-last-initial-cand-set  result))
    result)
  nil nil nil nil nil nil
  ((prompt                             "SYMB `C-M-j' PLIST (`RET' when done): ") ; Bindings
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering property lists...")))

;;;###autoload
(icicle-define-command icicle-doc       ; Command name
  "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the symbol.

Each candidate for completion is a symbol name plus its type
\(FUNCTION, VARIABLE, or FACE) and its documentation.  These candidate
components are separated by `icicle-list-join-string' (\"^G^J\", by
default).  You can match an input regexp against the symbol name,
type, or the documentation or any combination of the three.  Use
`C-M-j' (equivalent here to `C-q C-g C-j') to input the default
separator.

With a prefix argument, use the same documentation that was gathered
the last time `icicle-doc' was called.  Use a prefix arg to save the
time that would be needed to gather the documentation.

Remember that you can use \\<minibuffer-local-completion-map>\
`\\[icicle-toggle-incremental-completion]' to toggle incremental completion." ; Doc string
  icicle-doc-action                     ; Action function: display the doc.
  prompt                                ; `completing-read' args
  (let ((result  (and pref-arg icicle-doc-last-initial-cand-set))
        doc)                            ; Each completion candidate is a list of strings.
    (unless result                      ; COLLECTION arg is an alist with items (doc . symb).
      (mapatoms
       #'(lambda (symb)
           (progn
             (when (and (functionp symb) ; Function's doc.
                        ;; Ignore symbols that produce errors.  See comment for `icicle-fundoc'.
                        (setq doc  (condition-case nil (documentation symb) (error nil)))
                        (setq doc  (icicle-fn-doc-minus-sig doc))
                        (icicle-non-whitespace-string-p doc)
                        (setq doc  (concat doc "\n\n")))
               (push (cons (list (concat (symbol-name symb) icicle-list-join-string "FUNCTION") doc)
                           symb)
                     result))
             (when (and (boundp symb)   ; Variable's doc (and keymap var's bindings if remove nil)
                        (setq doc  (documentation-property symb 'variable-documentation))
                        (icicle-non-whitespace-string-p doc))
               (when (and nil           ; $$$ Remove nil to get keymaps, but it slows things down.
                          (fboundp 'describe-keymap)
                          (keymapp (symbol-value symb)))
                 (setq doc  (concat (symbol-name symb) ":\n" doc "\n\n" ; Keymap variable's doc.
                                    (substitute-command-keys
                                     (concat "\\{" (symbol-name symb) "}"))
                                    "\n\n")))
               (setq doc  (concat doc "\n\n"))
               (push (cons (list (concat (symbol-name symb) icicle-list-join-string "VARIABLE") doc)
                           symb)
                     result))
             (when (and (facep symb)
                        (setq doc  (documentation-property symb 'face-documentation)))
               (push (cons (list (concat (symbol-name symb) icicle-list-join-string "FACE") doc)
                           symb)
                     result)))))
      (setq icicle-doc-last-initial-cand-set  result))
    result)
  nil nil nil 'icicle-doc-history nil nil
  ((prompt                             "Find doc with regexp: ") ; Bindings
   (icicle-candidate-properties-alist  '((1 (face icicle-candidate-part))))
   (icicle-list-use-nth-parts          '(1))
   (icicle-transform-function          'icicle-remove-duplicates) ; Duplicates are due to `fset's.
   (icicle-candidate-help-fn           'icicle-doc-action)
   (pref-arg                           current-prefix-arg))
  (progn
    (put-text-property 0 1 'icicle-fancy-candidates t prompt) ; First code
    (icicle-highlight-lighter)
    (message "Gathering documentation...")))

(defun icicle-doc-action (entry)
  "Completion action function for `icicle-doc': Display the doc."
  (let ((symb  (intern (icicle-transform-multi-completion entry))))
    (cond ((fboundp symb) (describe-function symb))
          ;; $$$ This works fine, but it slows things down:
          ;; ((and (fboundp 'describe-keymap) (boundp symb) (keymapp (symbol-value symb)))
          ;;  (describe-keymap symb))
          ((and symb (boundp symb)) (describe-variable symb))
          ((facep symb) (describe-face symb)))
    symb))

;;;###autoload
(defun icicle-non-whitespace-string-p (string)
  "Return non-nil if STRING contains a non-whitespace character.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable  (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string) (> (length string) 0) (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload
(defun icicle-apropos (apropos-regexp &optional do-all)
  "Like `apropos', but lets you see the list of matches (with `S-TAB').
Function names are highlighted using face `icicle-special-candidate'."
  (interactive
   (list
    (unwind-protect
         (progn
           (mapatoms #'(lambda (symb) (when (fboundp symb) (put symb 'icicle-special-candidate t))))
           (let ((icicle-fancy-candidates-p  t)
                 (icicle-candidate-alt-action-fn
                  (or icicle-candidate-alt-action-fn
                      (icicle-alt-act-fn-for-type "symbol")))
                 (icicle-all-candidates-list-alt-action-fn
                  (or icicle-all-candidates-list-alt-action-fn
                      (icicle-alt-act-fn-for-type "symbol"))))
             (completing-read "Apropos symbol (regexp or words): " obarray
                              nil nil nil 'regexp-history)))
      (mapatoms #'(lambda (symb) (put symb 'icicle-special-candidate nil))))
    current-prefix-arg))
  (apropos apropos-regexp do-all))

;;;###autoload
(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern)
     "Show variables that match PATTERN.
This includes variables that are not user options.
User options are highlighted using face `icicle-special-candidate'.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN."
     (interactive
      (list
       (unwind-protect
            (progn
              (mapatoms #'(lambda (symb)
                            (when (user-variable-p symb) (put symb 'icicle-special-candidate t))))
              (let ((icicle-fancy-candidates-p  t)
                    (icicle-must-pass-after-match-predicate
                     #'(lambda (s)
                         (setq s  (intern s))
                         (and (boundp s) (get s 'variable-documentation))))
                    (icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "variable")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "variable"))))
                (completing-read
                 (concat "Apropos variable (regexp" (and (>= emacs-major-version 22) " or words")
                         "): ")
                 obarray nil nil nil 'regexp-history)))
         (mapatoms #'(lambda (symb) (put symb 'icicle-special-candidate nil))))))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-option' for a description of PATTERN."
     (interactive
      (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (user-variable-p (intern s)))))
        (list (completing-read
               (concat "Apropos user option (regexp" (and (>= emacs-major-version 22) " or words")
                       "): ") obarray nil nil nil 'regexp-history))))
     (let ((apropos-do-all  nil)
           (icicle-candidate-alt-action-fn
            (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "option")))
           (icicle-all-candidates-list-alt-action-fn
            (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "option"))))
       (apropos-option pattern)))

   (defun icicle-apropos-function (pattern)
     "Show functions that match PATTERN.
This includes functions that are not commands.
Command names are highlighted using face `icicle-special-candidate'.
You can see the list of matches with `S-TAB'.
See `apropos-function' for a description of PATTERN."
     (interactive
      (list 
       (unwind-protect
            (progn
              (mapatoms #'(lambda (symb)
                            (when (commandp symb) (put symb 'icicle-special-candidate t))))
              (let ((icicle-fancy-candidates-p               t)
                    (icicle-must-pass-after-match-predicate  #'(lambda (s) (fboundp (intern s))))
                    (icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "function")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "function"))))
                (completing-read
                 (concat "Apropos function (regexp" (and (>= emacs-major-version 22) " or words")
                         "): ") obarray nil nil nil 'regexp-history)))
         (mapatoms #'(lambda (symb) (put symb 'icicle-special-candidate nil))))))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-command' for a description of PATTERN."
     (interactive
      (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (commandp (intern s))))
            (icicle-candidate-alt-action-fn
             (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "command")))
            (icicle-all-candidates-list-alt-action-fn
             (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "command"))))
        (list (completing-read
               (concat "Apropos command (regexp" (and (>= emacs-major-version 22) " or words")
                       "): ") obarray nil nil nil 'regexp-history))))
     (let ((apropos-do-all  nil))  (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN.

By default, only user options are candidates.  With optional prefix
DO-ALL, or if `apropos-do-all' is non-nil, all variables are
candidates.  In that case, the user-option candidates are highlighted
using face `icicle-special-candidate'."
     (interactive
      (list
       (unwind-protect
            (progn
              (unless (or (boundp 'apropos-do-all) (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (when (or current-prefix-arg apropos-do-all)
                (mapatoms #'(lambda (symb)
                              (when (user-variable-p symb) (put symb 'icicle-special-candidate t)))))
              (let ((icicle-fancy-candidates-p  (or current-prefix-arg apropos-do-all))
                    (icicle-must-pass-after-match-predicate
                     (if (or current-prefix-arg apropos-do-all)
                         #'(lambda (s)
                             (setq s  (intern s))
                             (and (boundp s) (get s 'variable-documentation)))
                       #'(lambda (s) (user-variable-p (intern s)))))
                    (icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                         "variable"
                                                       "option"))))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                         "variable"
                                                       "option")))))
                (completing-read
                 (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                        "variable" "user option")
                         " (regexp" (and (>= emacs-major-version 22) " or words") "): ")
                 obarray nil nil nil 'regexp-history)))
         (when (or current-prefix-arg apropos-do-all)
           (mapatoms #'(lambda (symb) (put symb 'icicle-special-candidate nil)))))
       current-prefix-arg))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-command' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.  In that case, the command
candidates are highlighted using face `icicle-special-candidate'.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
     (interactive
      (list 
       (unwind-protect
            (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (when (or current-prefix-arg apropos-do-all)
                (mapatoms #'(lambda (symb)
                              (when (commandp symb) (put symb 'icicle-special-candidate t)))))
              (let ((icicle-fancy-candidates-p  (or current-prefix-arg apropos-do-all))
                    (icicle-must-pass-after-match-predicate
                     (if current-prefix-arg
                         #'(lambda (s) (fboundp (intern s)))
                       #'(lambda (s) (commandp (intern s)))))
                    (icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                         "function"
                                                       "command"))))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type (if icicle-fancy-candidates-p
                                                         "function"
                                                       "command")))))
                (completing-read
                 (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                        "command or function" "command")
                         "(regexp" (and (>= emacs-major-version 22) " or words") "): ")
                 obarray nil nil nil 'regexp-history)))
         (when (or current-prefix-arg apropos-do-all)
           (mapatoms #'(lambda (symb) (put symb 'icicle-special-candidate nil)))))
       current-prefix-arg))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression input REGEXP.
Return the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search     t)
                             (cookie-table-symbol  (intern yow-file cookie-cache))
                             (string-table         (symbol-value cookie-table-symbol))
                             (table                (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches  (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches  (cdr matches))
          (and matches (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

(defalias 'icicle-map 'icicle-apply)
;;;###autoload
(defun icicle-apply (alist fn &optional nomsg predicate initial-input hist def inherit-input-method)
  "Selectively apply a function to elements in an alist.
Argument ALIST is an alist such as can be used as the COLLECTION
argument for Icicles `completing-read'.  Its elements can represent
multi-completions, for example.  Interactively, COLLECTION is a
variable (a symbol) whose value is an alist.

Argument FN is a function.

Optional argument NOMSG non-nil means do not display an informative
message each time FN is applied.  If nil, then a message shows the key
of the alist element that FN is applied to and the result of the
application.

The remaining arguments are optional.  They are the arguments
PREDICATE, INITIAL-INPUT, HIST, DEF, and INHERIT-INPUT-METHOD for
`completing-read' (that is, all of the `completing-read' args other
than PROMPT, COLLECTION, and REQUIRE-MATCH).  During `icicle-apply'
completion, a match is required (REQUIRE-MATCH is t).

Interactively, you are prompted for both arguments.  Completion is
available for each.  The completion list for ALIST candidates is the
set of variables whose value is a cons.  With no prefix argument, the
names of these variables must end with \"alist\".  With a prefix
argument, the first car of each variable value must itself be a cons.

After choosing the ALIST and FN, you are prompted to choose one or
more keys of the alist elements, and FN is applied to each element
that has a key that you choose.  Multi-command completion is available
for choosing these candidates: you can apply FN to any number of
elements, any number of times.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist, and the result of
applying FN to an alist element is simply the value of that key.  If
you choose, for example, candidate \"\\.el\\'\", then the result is
`cdr' applied to the alist element (\"\\.el\\'\" . emacs-lisp-mode),
which is the symbol `emacs-lisp-mode'.  In this case, the function
performs simple lookup.

If FN were instead (lambda (x) (describe-function (cdr x))), then the
result of choosing candidate \"\\.el\\'\" would be to display the help
for function `emacs-lisp-mode'.

NOTE: `icicle-apply' does not, by itself, impose any particular sort
order.  Neither does it inhibit sorting.  If you call this function
from Lisp code and you want it to use a certain sort order or you want
no sorting, then bind `icicle-sort-comparer' accordingly.

During completion you can use multi-command keys.  Each displays the
value of applying FN to an alist element whose key is a completion
candidate.

`C-RET'   - Act on current completion candidate only
`C-down'  - Move to next completion candidate and act
`C-up'    - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`C-!'     - Act on *each* candidate (or each that is saved), in turn.
`M-!'     - Act on the list of *all* candidates (or all saved).

Note that `M-!' applies FN to the *list* of chosen alist elements,
whereas `C-!' applies FN to each chosen element, in turn.  For
example, if FN is `length' and your input is `\.el', then `M-!' displays
the result of applying `length' to the list of chosen elements:

 ((\"\\.el\\'\" . emacs-lisp-mode) (\"\\.elc'\" . emacs-lisp-mode))

which is 2.

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see command
`icicle-mode'.

`icicle-apply' overrides `icicle-ignore-space-prefix-flag', binding it
to nil so that candidates with initial spaces can be matched."
  (interactive
   (list (symbol-value
          (intern
           (let ((icicle-must-pass-after-match-predicate
                  `(lambda (s)
                      (setq s  (intern s))
                      (and (boundp s) (consp (symbol-value s))
                           ,(if current-prefix-arg
                                '(consp (car (symbol-value s)))
                                '(string-match "alist$" (symbol-name s))))))
                 (icicle-candidate-alt-action-fn
                  (or icicle-candidate-alt-action-fn
                      (icicle-alt-act-fn-for-type "variable")))
                 (icicle-all-candidates-list-alt-action-fn
                  (or icicle-all-candidates-list-alt-action-fn
                      (icicle-alt-act-fn-for-type "variable"))))
             (completing-read "Alist (variable): " obarray nil t nil
                              (if (boundp 'variable-name-history)
                                  'variable-name-history
                                'icicle-variable-name-history)))))
         (read
          (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (functionp (intern s))))
                (icicle-candidate-alt-action-fn
                 (or icicle-candidate-alt-action-fn
                     (icicle-alt-act-fn-for-type "function")))
                (icicle-all-candidates-list-alt-action-fn
                 (or icicle-all-candidates-list-alt-action-fn
                     (icicle-alt-act-fn-for-type "function"))))
            (completing-read "Function: " obarray nil nil nil (if (boundp 'function-name-history)
                                                                  'function-name-history
                                                                'icicle-function-name-history))))))

  (setq icicle-candidate-entry-fn  fn)  ; Save in global variable - used by `icicle-apply-action'.
  (let ((icicle-candidate-action-fn            'icicle-apply-action)
        (icicle-all-candidates-list-action-fn  'icicle-apply-list-action)
        (icicle-ignore-space-prefix-flag       nil)
        (icicle-apply-nomsg                    nomsg)
        (enable-recursive-minibuffers          t))
    (icicle-explore
     #'(lambda ()
         (setq icicle-candidates-alist  ; Ensure that keys of ALIST are strings or conses.
               (mapcar #'(lambda (key+val)
                           (if (consp (car key+val))
                               key+val  ; Multi-completion candidate: (("aaa" "bbb") . ccc)
                             (cons (format "%s" (car key+val)) (cdr key+val))))
                       alist)))
     #'(lambda ()
         (let ((result  (funcall icicle-candidate-entry-fn icicle-explore-final-choice-full)))
           (unless nomsg
             (message "Key: %s,  Result: %s" (car icicle-explore-final-choice-full) result))
           result))                     ; Return result.
     nil nil nil "Choose an occurrence: " predicate t initial-input hist def inherit-input-method)))

(defun icicle-apply-action (string)
  "Completion action function for `icicle-apply'."
  (unwind-protect
      (condition-case icicle-apply-action
	  (progn
	    (icicle-highlight-candidate-in-Completions)
	    ;; Apply function to candidate element and display it.
	    (let* ((key+value  (funcall icicle-get-alist-candidate-function string))
		   (result     (funcall icicle-candidate-entry-fn key+value)))
	      (unless icicle-apply-nomsg
		(message "Key: %s,  Result: %s" (car key+value) result)))
	    nil)			; Return nil for success.
	(error "%s" (error-message-string icicle-apply-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-apply-list-action (strings)
  "Completion action list function for `icicle-apply'."
  (unwind-protect
       (condition-case icicle-apply-list-action
           (progn                       ; Apply function to candidate element and display it.
             (message "Result: %s" (funcall icicle-candidate-entry-fn
                                            (mapcar icicle-get-alist-candidate-function strings)))
             nil)                       ; Return nil for success.
         (error "%s" (error-message-string icicle-apply-list-action))) ; Return error msg.
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

;;;###autoload
(defun icicle-goto-marker-or-set-mark-command (arg) ; Bound to `C-@', `C-SPC'.
  "With prefix arg < 0, `icicle-goto-marker'; else `set-mark-command'.
By default, Icicle mode remaps all key sequences that are normally
bound to `set-mark-command' to
`icicle-goto-marker-or-set-mark-command'.  If you do not want this
remapping, then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (if (not (wholenump (prefix-numeric-value arg)))
      (icicle-goto-marker)
    (setq this-command 'set-mark-command) ; Let `C-SPC C-SPC' activate if not `transient-mark-mode'.
    (set-mark-command arg)))

;;;###autoload
(defun icicle-goto-global-marker-or-pop-global-mark (arg) ; Bound to `C-x C-@', `C-x C-SPC'.
  "With prefix arg < 0, `icicle-goto-global-marker'; else `pop-global-mark'.
By default, Icicle mode remaps all key sequences that are normally
bound to `pop-global-mark' to
`icicle-goto-global-marker-or-pop-global-mark'.  If you do not want
this remapping, then customize option
`icicle-top-level-key-bindings'."
  (interactive "P")
  (if (wholenump (prefix-numeric-value arg))
      (pop-global-mark)
    (icicle-goto-global-marker)))

;;;###autoload
(defun icicle-goto-marker ()            ; Bound to `C-- C-@', `C-- C-SPC'.
  "Go to a marker in this buffer, choosing it by the line that includes it.
If `crosshairs.el' is loaded, then the target position is highlighted.

By default, candidates are sorted in marker order, that is, with
respect to their buffer positions.  Use `C-M-,' or `C-,' to change the
sort order.

During completion you can use these keys:

`C-RET'   - Goto marker named by current completion candidate
`C-down'  - Goto marker named by next completion candidate
`C-up'    - Goto marker named by previous completion candidate
`C-next'  - Goto marker named by next apropos-completion candidate
`C-prior' - Goto marker named by previous apropos-completion candidate
`C-end'   - Goto marker named by next prefix-completion candidate
`C-home'  - Goto marker named by previous prefix-completion candidate
`S-delete' - Delete marker named by current completion candidate

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to choose a candidate as the final
destination, or `C-g' to quit.  This is an Icicles command - see
command `icicle-mode'."
  (interactive)
  (let ((icicle-sort-orders-alist  (cons '("by position" .  icicle-cdr-lessp)
                                         icicle-sort-orders-alist))
        (icicle-sort-comparer      'icicle-cdr-lessp))
    (icicle-goto-marker-1 mark-ring)))

;;;###autoload
(defun icicle-goto-global-marker ()     ; Bound to `C-- C-x C-@', `C-- C-x C-SPC'.
  "Like `icicle-goto-marker', but visits global, not local, markers.
If user option `icicle-show-multi-completion-flag' is non-nil, then
each completion candidate is annotated (prefixed) with the name of the
marker's buffer, to facilitate orientation."
  (interactive)
  (let ((icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        (icicle-list-end-string             "")
        (icicle-sort-orders-alist
         (cons '("by buffer, then by position" . icicle-part-1-cdr-lessp)
               icicle-sort-orders-alist))
        (icicle-sort-comparer               'icicle-part-1-cdr-lessp)
        (icicle-candidate-properties-alist  (and icicle-show-multi-completion-flag
                                                 '((1 (face icicle-candidate-part))))))
    (icicle-goto-marker-1 global-mark-ring)))

(defun icicle-goto-marker-1 (ring)
  "Helper function for `icicle-goto-marker', `icicle-goto-global-marker'.
RING is the marker ring to use."
  (unwind-protect
       (let* ((global-ring-p
               (memq this-command '(icicle-goto-global-marker
                                    icicle-goto-global-marker-or-pop-global-mark)))
              (markers
               (if (and (not global-ring-p) (marker-buffer (mark-marker)))
                   (cons (mark-marker) (icicle-markers ring))
                 (icicle-markers ring)))
              (icicle-delete-candidate-object
               #'(lambda (cand)
                   (let ((mrkr+txt  (funcall icicle-get-alist-candidate-function cand)))
                     (move-marker (cdr mrkr+txt) nil))))
              (icicle-alternative-sort-comparer  nil)
              (icicle-last-sort-comparer         nil)
              (orig-buff                         (current-buffer)))
         (unless (consp markers)
           (error (if global-ring-p "No global markers" "No markers in this buffer")))
         (cond ((cdr markers)
                (icicle-apply (mapcar #'(lambda (mrkr) (icicle-marker+text mrkr global-ring-p))
                                      markers)
                              #'icicle-goto-marker-1-action
                              'nomsg
                              (lambda (cand)
                                (marker-buffer (cdr cand)))))
               ((= (point) (car markers)) (message "Already at marker: %d" (point)))
               (t
                (icicle-goto-marker-1-action (icicle-marker+text (car markers) global-ring-p)))))
    (when (fboundp 'crosshairs-unhighlight) (crosshairs-unhighlight 'even-if-frame-switch))))

(defun icicle-goto-marker-1-action (cand)
  "Action function for `icicle-goto-marker-1'."
  (pop-to-buffer (marker-buffer (cdr cand)))
  (select-frame-set-input-focus (selected-frame))
  (goto-char (cdr cand))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))

(defun icicle-marker+text (marker &optional globalp)
  "Cons of text line that includes MARKER with MARKER itself.
If the marker is on an empty line, then text \"<EMPTY LINE>\" is used.
If both optional argument GLOBALP and option
`icicle-show-multi-completion-flag' are non-nil, then the text is
prefixed by MARKER's buffer name."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((line  (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `end-of-line'.
                     (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
                                                     (save-excursion (end-of-line) (point)))))
            (buff  (and globalp icicle-show-multi-completion-flag (buffer-name)))
            (help  (and (or icicle-help-in-mode-line-flag ; Get it only if user will see it.
                            (and (boundp 'tooltip-mode) tooltip-mode))
                        (format "Line: %d, Char: %d" (line-number-at-pos) (point)))))
        (when (string= "" line) (setq line  "<EMPTY LINE>"))
        (when help
          (icicle-candidate-short-help help line)
          (when (and globalp icicle-show-multi-completion-flag)
            (icicle-candidate-short-help help buff)))
        (if (and globalp icicle-show-multi-completion-flag)
            (cons (list buff line) marker)
          (cons line marker))))))

(defun icicle-markers (ring)
  "Marks in mark RING that are in live buffers other than a minibuffer."
  (let ((markers  ()))
    (dolist (mkr  ring)
      (when (and (buffer-live-p (marker-buffer mkr))
                 (not (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
                                    (buffer-name (marker-buffer mkr)))))
        (push mkr markers)))
    markers))

;;;###autoload
(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark' or save a region or select a saved region.
With no prefix arg, invoke `exchange-point-and-mark'.
If you use library `bookmark+.el', then you can use a prefix arg.

 * Plain `C-u': select (activate) one or more bookmarked regions.

 * Numeric prefix arg: bookmark (save) the active region using
   `icicle-bookmark-cmd'.

   Arg < 0: Prompt for the bookmark name.
   Arg > 0: Do not prompt for the bookmark name.  Use the buffer name
            plus a prefix of the region text as the bookmark name.
   Arg = 0: Same as > 0, except do not overwrite any existing bookmark
            with the same name.

By default, Icicle mode remaps all key sequences that are normally
bound to `exchange-point-and-mark' to
`icicle-exchange-point-and-mark'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (let ((bplus  (featurep 'bookmark+)))
    (if (not arg)
        (call-interactively #'exchange-point-and-mark)
      (unless bplus (error "You must load library `bookmark+.el' to use a prefix arg"))
      (cond ((atom arg)
             (unless (and transient-mark-mode mark-active (not (eq (mark) (point))))
               (error "Cannot bookmark inactive region: you must activate it first"))
             (icicle-bookmark-cmd (and (natnump (prefix-numeric-value arg)) 9)))
            (t
             (bookmark-maybe-load-default-file)
             (unless (consp (bmkp-region-alist-only)) (error "No bookmarked regions"))
             (call-interactively #'icicle-select-bookmarked-region))))))

;;;###autoload
(defun icicle-search-generic ()         ; Bound to `C-x `'.
  "Run `icicle-search-command'.  By default, this is `icicle-search'.
In Compilation and Grep modes, this is `icicle-compilation-search'.
In Comint, Shell, GUD, and Inferior Lisp modes, this is
   `icicle-comint-search'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload
(defun icicle-search (beg end scan-fn-or-regexp require-match ; Bound to `C-c `'.
                      &optional where &rest args)
  "Search for matches, with completion, cycling, and hit replacement.
Interactively, search for regexp matches.  You are prompted for a
regexp, which you enter using `RET'.  The search hits (matches) are
available as completion candidates.  You can then use apropos
completion to filter the candidates using a different regexp, which
you can change dynamically (as always).  You can replace individual
matches with another string, as in `query-replace' or
`query-replace-regexp'.  By default, candidates appear in order of
buffer occurrence, but you can sort them in various ways.

Non-interactively, search can be for regexp matches or any other kind
of matches.  Argument SCAN-FN-OR-REGEXP is the regexp to match, or it
is a function that defines an alist of buffer zones to search.  You
can navigate among the matching buffer zones (defined either as regexp
matches or via function), called search \"contexts\", and you can
match another regexp against the text in a search context.  See the
end of this description for information about the other arguments.

If the search-context regexp contains regexp subgroups, that is,
subexpressions of the form `\(...\)', then you are prompted for the
subgroup to use to define the search contexts.  Subgroup 0 means the
context is whatever matches the whole regexp.  Subgroup 1 means the
context is whatever matches the first subgroup, and so on.  The
subgroup number is the number of occurrences of `\(', starting at the
beginning of the regexp.

Search respects `icicle-regexp-quote-flag' and
`icicle-search-whole-word-flag'.  You can toggle these during search,
by using `C-`' and `M-q', respectively.  If `icicle-regexp-quote-flag'
is non-nil, then regexp special characters are quoted, so that they
become non-special.  If `icicle-search-whole-word-flag' is non-nil,
then whole-word searching is done.  During word search, all characters
in the search string you type are treated as if they were word
constituents: the search string is matched literally, but only at word
boundaries.  (You can also use `\\[icicle-search-word]' to perform
word search.)


Optional Behaviors: Prefix Argument
-----------------------------------

By default, search only the current buffer.  Search the active region,
or, if there is none, then search the entire buffer.

With a prefix argument, you can search multiple buffers, files, or
bookmarks, as follows:

- With a plain prefix arg (`C-u'), search bookmarks.  This is the
same as command `icicle-search-bookmarks-together'.  (To search
bookmarks one at a time instead of together, use multi-command
`icicle-search-bookmark'.)

- With a non-negative numeric prefix arg, search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.  Any existing buffers can be chosen.  If the
prefix arg is 99, then only buffers visiting files are candidates.
This is the same as command `icicle-search-buffer'.

- With a negative numeric prefix arg, search multiple files
completely.  You are prompted for the files to search - all of each
file is searched.  Any existing files in the current directory can be
chosen.  This is the same as command `icicle-search-file'.


Navigation and Help
-------------------

The use of completion for this command is special.  It is not unusual
in this context to have multiple completion candidates that are
identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one simply by
completing it in the minibuffer, because the destination would be
ambiguous.  That is, simply completing your input and entering the
completion with `RET' will not take you to its occurrence in the
search buffer, unless it is unique.

Instead, choose search hits to visit using any of the candidate-action
keys: `C-RET', `C-mouse-2', `C-down', `C-up', `C-next', `C-prior',
`C-end', and `C-home'.  All but the first two of these cycle among the
search hits.  The current candidate in *Completions* corresponds to
the current location visited (it is not off by one, as is usually the
case in Icicles).

As always, the `C-M-' keys provide help on individual candidates:
`C-M-RET', `C-M-mouse-2', `C-M-down', `C-M-up', `C-M-next',
`C-M-prior', `C-M-end', and `C-M-home'.  For `icicle-search', they
indicate the buffer and position of the search hit.

You can cycle among candidates without moving to their occurrences in
the search buffer, using `down', `up', `next', `prior', `end', or
`home' (no `C-' modifier).


Highlighting
------------

In the search buffer (that is, where the hits are), `icicle-search'
does the following:

- Highlights the current match (buffer zone) for the initial (context)
  regexp, using face `icicle-search-main-regexp-current'.

- Highlights the first `icicle-search-highlight-threshold' context
  matches, using face `icicle-search-main-regexp-others'.

- Highlights 1-8 context levels, within each search context.  This
  happens only if your initial (context) regexp has \\(...\\) groups
  and option `icicle-search-highlight-context-levels-flag' is non-nil.

- Highlights the match for your current input, using face
  `icicle-search-current-input'.  Highlights all such matches if
  option `icicle-search-highlight-all-current-flag' is non-nil;
  otherwise, highlights just the currently visited match.
  You can toggle this option using `C-^'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' during Icicles search
using `C-.'  in the minibuffer.


Search and Replace
------------------

You can replace the current search match by using any of the
alternative action keys: `C-S-RET', `C-S-mouse-2' (in *Completions*),
`C-S-down', `C-S-up', `C-S-next', `C-S-prior', `C-S-end', and
`C-S-home'.  You can use `M-|' to replace all matches at once.  (And
remember that you can activate the region to limit the
search-and-replace space.)


At the first use of any of these, you are prompted for the replacement
string; it is used thereafter, or until you use `M-,'
\(`icicle-search-define-replacement') to change it (anytime).

Unlike `query-replace', you need not visit search matches successively
or exhaustively.  You can visit and replace selected matches in any
order.

What is meant here by a \"search match\"?  It can be either an entire
search context or just a part of the context that your current
minibuffer input matches.

`C-,' toggles option `icicle-search-replace-whole-candidate-flag'.  By
default, the entire current search context is replaced, that is,
whatever matches the context regexp that you entered initially using
`RET'.  However, you can use `C-,' anytime during searching to toggle
between this default behavior and replacement of whatever your current
minibuffer input matches.

Remember this:

 - If `icicle-search-replace-whole-candidate-flag' is non-nil, then
   the granularity of replacement is a complete search context.  In
   this case, replacement behaves similarly to `query-replace-regexp'.
   You can still use minibuffer input to filter the set of search
   contexts, but replacement is on a whole-context basis.

 - If `icicle-search-replace-whole-candidate-flag' is nil, then you
   can replace multiple input matches separately within a search
   context (using `C-S-RET').  This behavior is unique to Icicles.
   You cannot, however skip over one input match and replace the next
   one in the same context - `C-S-RET' always replaces the first
   available match.

If `icicle-search-replace-whole-candidate-flag' is non-nil, then you
can use the navigational alternative action keys, `C-S-down',
`C-S-up', `C-S-next', `C-S-prior', `C-S-end', and `C-S-home',
repeatedly to replace successive search contexts.  At the buffer
limits, these commands wraps around to the other buffer limit (last
search context to first, and vice versa).

Search traversal using these go-to-next-context-and-replace keys is
always by search context, not by individual input match.  This means
that you cannot use these keys to replace input matches within a
search context (except for the first such match, if
`icicle-search-replace-whole-candidate-flag' is nil).

If your input matches multiple parts of a search context, and you want
to replace these in order, then use `C-S-RET' repeatedly.  You can
traverse all matches of your input in the order they appear in the
buffer by repeating `C-S-RET' (provided the replacement text does not
also match your input - see below).  At the buffer limits, repeating
`C-S-RET' wraps around too.

`C-S-RET' always replaces the first input match in the current search
context or, if there are no matches, then the first input match in the
next context.  This behavior has these important consequences:

* If you repeat `C-S-RET' and the previous replacement no longer
  matches your input, then `C-S-RET' moves on to the next input match
  (which is now the first one) and replaces that.  This is why you can
  usually just repeat `C-S-RET' to successively replaces matches of
  your input, including from one context to the next.

* If, on the other hand, after replacement the text still matches your
  input, then repeating `C-S-RET' will just replace that match.
  For example, if you replace the input match `ab' by `abcd', then
  repeating `C-S-RET' produces `abcd', then `abcdcd', then `abcdcd'...

* You cannot replace an input match, skip the next match, and then
  replace the following one, all in the same context.  You can,
  however, replace some matches and then skip (e.g. `C-next') to the
  next context.

What your input matches, hence what gets replaced if
`icicle-search-replace-whole-candidate-flag' is nil, depends on a few
Icicles options:

 - `icicle-regexp-quote-flag' determines whether to use regexp
   matching or literal matching.

 - `icicle-search-highlight-all-current-flag',
   `icicle-expand-input-to-common-match-flag' and
   `icicle-search-replace-common-match-flag' together determine
   whether to replace exactly what your input matches in the current
   search hit or the expanded common match (ECM) of your input among
   all search hits.  If any of these options is nil, then your exact
   input match is replaced; if they are all non-nil, then the ECM is
   replaced.

Finally, the replacement string can be nearly anything that is allowed
as a replacement by `query-replace-regexp'.  In Emacs 22 or later,
this includes Lisp sexp evaluation via `\,' and constructs such as
`\#' and `\N' (back references).  You can also use `\?', but it is not
very useful - you might as well use `M-,' instead, to change the
replacement text.


Using Regexps
-------------

At any time, you can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' (command
`icicle-insert-string-from-variable') to insert text (e.g. a regexp)
from a variable into the minibuffer.  For example, you can search for
ends of sentences by using `C-u \\[icicle-insert-string-from-variable]' and choosing variable
`sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `C-=' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.


Additional Information
----------------------

If user option `icicle-show-multi-completion-flag' is non-nil, then
each candidate is annotated with the name of the buffer where the
search hit occurs, to facilitate orientation.  Note that even when the
value is nil, you can use `C-M-mouse-2' and so on to see the buffer
name, as well as the position of the hit in the buffer.

Completion is lax if `icicle-show-multi-completion-flag' is non-nil;
otherwise, it is strict.

After you visit a completion candidate, the hooks in variable
`icicle-search-hook' are run.

`icicle-search' overrides `icicle-ignore-space-prefix-flag', binding
it to nil, so that candidates with initial spaces can be matched.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates if
REQUIRE-MATCH is nil.


Non-Interactive Use
-------------------

Function `icicle-search' is not only a powerful command, it is also a
building block for creating your own Icicles search-and-replace
commands.  When called non-interactively, these are the
`icicle-search' arguments:

BEG is the beginning of the region to search; END is the end.
SCAN-FN-OR-REGEXP: Regexp or function that determines the set of
  initial candidates (match zones).  If a function, it is passed, as
  arguments, the buffer to search, the beginning and end of the search
  region in that buffer, and ARGS.
REQUIRE-MATCH is passed to `completing-read'.
Optional arg WHERE is a list of bookmarks, buffers, or files to be
  searched.  If nil, then search only the current buffer or region.
  (To search bookmarks you must also use library `bookmark+.el').
ARGS are arguments that are passed to function SCAN-FN-OR-REGEXP.

Note that if SCAN-FN-OR-REGEXP is a regexp string, then function
`icicle-search-regexp-scan' is used to determine the set of match
zones.  You can limit hits to regexp matches that also satisfy a
predicate, by using `(PREDICATE)' as ARGS: PREDICATE is then passed to
`icicle-search-regexp-scan' as its PREDICATE argument.

This command is intended for use only in Icicle mode."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (setq icicle-search-context-regexp  (and (stringp scan-fn-or-regexp) scan-fn-or-regexp))
  (let ((icicle-candidate-action-fn         (or icicle-candidate-action-fn 'icicle-search-action))
        (icicle-candidate-help-fn           'icicle-search-help)
        (icicle-all-candidates-list-alt-action-fn
         (or icicle-all-candidates-list-alt-action-fn 'icicle-search-replace-all-search-hits))
        (icicle-candidate-alt-action-fn
         (or icicle-candidate-alt-action-fn 'icicle-search-replace-search-hit))
        (icicle-update-input-hook           (list 'icicle-search-highlight-all-input-matches))
        (icicle-search-ecm                  nil)
        (icicle-searching-p                 t)
        (icicle-search-replacement          nil)
        (icicle-current-input               "")
        (icicle-list-nth-parts-join-string  "\t")
        (icicle-list-join-string            "\t")
        (icicle-list-end-string             "")
        (icicle-list-use-nth-parts          '(1))
        (icicle-sort-comparer               nil)

        ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would inhibit
        ;; sorting, because we would be depending on the alist order.
        ;;    (icicle-inhibit-sort-p              t)

        (icicle-no-match-hook               icicle-no-match-hook)
        (completion-ignore-case             case-fold-search)
        (replace-count                      0)) ; Defined in `replace.el'.  Used for replacement.
    (add-hook 'icicle-no-match-hook (lambda () (when (overlayp icicle-search-current-overlay)
                                                 (delete-overlay icicle-search-current-overlay))))
    (setq icicle-search-final-choice
          (icicle-explore #'(lambda () (icicle-search-define-candidates beg end scan-fn-or-regexp
                                                                        require-match where args))
                          #'icicle-search-final-act #'icicle-search-quit-or-error
                          #'icicle-search-quit-or-error #'icicle-search-cleanup
                          "Choose an occurrence: " nil require-match nil 'icicle-search-history))))

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun icicle-region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is not active or is empty, then use bob and eob."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-read-context-regexp (&optional prompt pred init hist def i-i-m)
  "Read context regexp and determine `icicle-search-context-level'.
Read the regexp using completion against previous regexp input.
The arguments are for use by `completing-read' to read the regexp.
 HIST (or `regexp-history' if HIST is nil) is used for the
 `completing-read' COLLECTION argument.
 The REQUIRE-MATCH arg to `completing-read' is nil.
 A default prompt is used if PROMPT is nil."
  (setq hist    (or hist 'regexp-history)
        prompt  (or prompt "Find (regexp): "))
  (let* ((icicle-candidate-action-fn  nil)
         (icicle-candidate-help-fn    nil)
         (regexp                      (icicle-completing-read-history
                                       prompt 'regexp-history pred init def i-i-m)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp  (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (setq prompt                       "Subgroup to use as search context [0, 1, 2,...]: "
          icicle-search-context-level  (if (string-match "\\\\(" regexp)
                                           (truncate (if (fboundp 'read-number)
                                                         (read-number prompt 0)
                                                       (read-from-minibuffer ; Hope for a number.
                                                        prompt nil nil nil nil 0)))
                                         0))
    regexp))

(defun icicle-search-where-arg ()
  "Return WHERE arg for `icicle-search*' commands, based on prefix arg."
  (cond ((consp current-prefix-arg)
         (unless (require 'bookmark+ nil t) (error "This requires library `bookmark+.el'"))
         (message "Searching multiple bookmarks...") (sit-for 1)
         (let ((icicle-show-Completions-initially-flag  t)
               (icicle-bookmark-types                   '(all)))
           (save-selected-window (icicle-bookmark-list))))
        ((wholenump current-prefix-arg)
         (message "Searching multiple buffers...") (sit-for 1)
         (icicle-search-choose-buffers (= 99 (prefix-numeric-value current-prefix-arg))))
        (current-prefix-arg
         (message "Searching multiple files...") (sit-for 1)
         (let ((icicle-show-Completions-initially-flag  t))
           (save-selected-window (icicle-file-list))))
        (t nil)))

(defun icicle-search-choose-buffers (files-only-p)
  "Choose multiple buffers to search.
FILES-ONLY-P non-nil means that only buffers visiting files are
candidates."
  (let ((icicle-show-Completions-initially-flag  t))
    (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag  'partial-match-ok)
                               (current-prefix-arg                files-only-p))
                           (save-selected-window (icicle-buffer-list))))))

(defun icicle-search-read-word ()
  "Read a word to search for (whole-word search).
Regexp special characters within the word are escaped (quoted)."
  (setq icicle-search-context-level  0)
  (concat "\\b"
          (regexp-quote (icicle-completing-read-history "Search for whole word: "
                                                        'icicle-search-history))
          "\\b"))

(defun icicle-search-final-act ()
  "Go to the final search hit choice, then run `icicle-search-hook'.
The hit's frame is raised and selected."
  (let* ((marker  (cdr icicle-explore-final-choice-full))
         (buf     (marker-buffer marker)))
    (unless (bufferp buf) (error "No such buffer: %s" buf))
    (pop-to-buffer buf)
    (raise-frame)
    (goto-char (marker-position marker))
    (select-frame-set-input-focus (selected-frame))
    (run-hooks 'icicle-search-hook)))

;; Free vars here: `orig-pt-explore', `orig-win-explore' are bound in `icicle-explore'.
(defun icicle-search-quit-or-error ()
  "Return to the starting point."
  (when (window-live-p orig-win-explore)
    (select-window orig-win-explore)
    (goto-char orig-pt-explore)))

;; Free vars here: `orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-cleanup ()
  "Clean up search highlighting, if `icicle-search-highlight-cleanup'.
Select original window."
  (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
  (when (window-live-p orig-win-explore)
    (select-window orig-win-explore)
    (select-frame-set-input-focus (selected-frame))))

(defun icicle-search-define-candidates (beg end scan-fn-or-regexp require-match where args)
  "Define completion candidates for `icicle-search'.
The arguments are the same as for `icicle-search'."
  (when (and icicle-regexp-quote-flag
             (not icicle-search-whole-word-flag)
             (stringp scan-fn-or-regexp))
    (setq scan-fn-or-regexp  (regexp-quote scan-fn-or-regexp)))
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf  where)
           (icicle-search-define-candidates-1 buf nil nil scan-fn-or-regexp args)))
        ((and (consp where) (stringp (car where)) ; List of files - search files.
              (file-exists-p (car where)))
         (dolist (file  where)
           (icicle-search-define-candidates-1 (find-file-noselect file 'nowarn) nil nil
                                              scan-fn-or-regexp args)))
        ((consp where)                ; Search all bookmarked regions.
         (unless (require 'bookmark+ nil t) (error "This requires library `bookmark+.el'"))
         (let ((non-existent-buffers  ())
               buf+beg buf beg end)
           (dolist (bmk  where)
             (setq buf+beg  (bookmark-jump-noselect bmk)
                   buf      (car buf+beg)
                   beg      (cdr buf+beg)
                   end      (bmkp-get-end-position bmk))
             (if (bufferp buf)
                 (icicle-search-define-candidates-1 buf beg end scan-fn-or-regexp args)
               (push buf non-existent-buffers)))
           (when non-existent-buffers
             (message "Skipping regions in non-existent buffers: `%s'"
                      (mapconcat #'identity (icicle-remove-duplicates non-existent-buffers)
                                 "', `"))
             (sit-for 3))))
        (t                              ; Search this buffer only.
         (icicle-search-define-candidates-1 nil beg end scan-fn-or-regexp args)))
  (unless icicle-candidates-alist  (if (functionp scan-fn-or-regexp)
                                       (error "No search hits")
                                     (error "No search hits for `%s'" scan-fn-or-regexp))))

(defun icicle-search-define-candidates-1 (buffer beg end scan-fn-or-regexp args)
  "Helper function for `icicle-search-define-candidates'.
BUFFER is a buffer to scan for candidates.
The other arguments are the same as for `icicle-search'."
  (if (functionp scan-fn-or-regexp)
      (apply scan-fn-or-regexp buffer beg end args)
    (apply 'icicle-search-regexp-scan buffer beg end scan-fn-or-regexp args)))

(defun icicle-search-regexp-scan (buffer beg end regexp &optional predicate)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
If BEG and END are non-nil, scan only between positions BEG and END.
If REGEXP has subgroups, then use what the Nth subgroup matches as the
 search context (hit), where N = `icicle-search-context-level'.
 If N=0, then use the overall match of REGEXP as the search context.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil, then push only the hits for which it holds.

Highlight the matches in face `icicle-search-main-regexp-others'."
  (setq regexp  (or regexp (icicle-search-read-context-regexp)))
  (let ((add-bufname-p  (and buffer icicle-show-multi-completion-flag))
        (temp-list      ())
        (last-beg       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)     ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg end)
          (setq beg  (point-min)
                end  (point-max)))
        (condition-case icicle-search-regexp-scan
            (save-excursion
              (goto-char (setq last-beg  beg))
              (while (and beg (< beg end) (not (eobp)))
                (while (and (setq beg  (re-search-forward regexp end t))
                            (eq last-beg beg)
                            (not (eobp)))
                  (forward-char) (setq beg  (1+ beg))) ; Matched again, same place.  Advance 1 char.
                (when beg
                  (unless (match-beginning icicle-search-context-level)
                    (error "Search context has no subgroup of level %d - try a lower number"
                           icicle-search-context-level))
                  (let* ((hit-string  (buffer-substring-no-properties
                                       (match-beginning icicle-search-context-level)
                                       (match-end icicle-search-context-level)))
                         (end-marker  (copy-marker (match-end icicle-search-context-level))))
                    (when (and (not (string= "" hit-string))
                               (or (not predicate)
                                   (save-match-data (funcall predicate hit-string end-marker))))
                      (icicle-candidate-short-help
                       (concat (and add-bufname-p
                                    (format "Buffer: `%s', "
                                            (buffer-name (marker-buffer end-marker))))
                               (format "Position: %d, Length: %d"
                                       (marker-position end-marker) (length hit-string)))
                       hit-string)
                      ;; Add whole candidate to `temp-list'.  Whole candidate is
                      ;; (`hit-string' . `end-marker') or ((`hit-string' BUFNAME) . `end-marker').
                      (push (cons (if add-bufname-p
                                      (list hit-string
                                            (let ((string  (copy-sequence (buffer-name))))
                                              (put-text-property 0 (length string) 'face
                                                                 'icicle-candidate-part string)
                                              string))
                                    hit-string)
                                  end-marker)
                            temp-list)
                      ;; Highlight search context in buffer.
                      (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                                icicle-search-highlight-threshold)
                        (let ((ov  (make-overlay (match-beginning icicle-search-context-level)
                                                 (match-end icicle-search-context-level))))
                          (push ov icicle-search-overlays)
                          (overlay-put ov 'priority 200) ; > ediff's 100+, < isearch-overlay's 1001.
                          (overlay-put ov 'face 'icicle-search-main-regexp-others))))))
                (setq last-beg  beg))
              (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
          (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
          (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                 (error (error-message-string icicle-search-regexp-scan))))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search context, what INPUT matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input  (or input icicle-current-input))
    (unless (or (string= "" input) (null icicle-search-overlays))
      (let ((hits  ()))
        (save-excursion
          (dolist (ov  icicle-search-overlays)
            (set-buffer (overlay-buffer ov))
            (save-restriction ; Search within the current search context.
              (narrow-to-region (overlay-start ov) (overlay-end ov))
              (goto-char (point-min))
              (when (condition-case nil (re-search-forward input nil 'move-to-end) (error nil))
                (push (buffer-substring-no-properties (point-min) (point-max)) hits))))
          (when (and icicle-expand-input-to-common-match-flag  hits)
            (setq icicle-search-ecm  (icicle-expanded-common-match input hits)))
          (dolist (ov  icicle-search-overlays)
            (set-buffer (overlay-buffer ov))
            (save-restriction ; Search within the current search context.
              (narrow-to-region (overlay-start ov) (overlay-end ov))
              (when (member (buffer-substring-no-properties (point-min) (point-max)) hits)
                (goto-char (point-min))
                (save-match-data
                  (while (condition-case nil
                             (re-search-forward (or icicle-search-ecm input) nil 'move-to-end)
                           (error nil))
                    (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
                    (push ov icicle-search-refined-overlays)
                    (overlay-put ov 'priority 220)
                    (overlay-put ov 'face 'icicle-search-current-input)))))))))))

(defun icicle-search-replace-search-hit (candidate) ; Bound to `C-S-RET' (`icicle-search').
  "Replace search hit CANDIDATE with `icicle-search-replacement'."
  ;; NOTE: We allow side effects during replacement.
  ;; In particular, `icicle-completion-candidates', `icicle-candidate-nb', and `icicle-last-input'
  ;; can change.

  (let (;; (icicle-candidate-nb          icicle-candidate-nb)
        ;; (icicle-completion-candidates icicle-completion-candidates)
        ;; (icicle-last-input            icicle-last-input)
        (icicle-last-completion-command  icicle-last-completion-command)
        (compl-win                       (get-buffer-window "*Completions*" 0)))
    (unless (or icicle-candidate-nb icicle-all-candidates-action)
      (error "No current candidate.  Cycle or complete to get to a candidate"))
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win icicle-completion-candidates)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates)))))
  (setq icicle-candidate-nb  (or icicle-candidate-nb 0)) ; Replace-all has nil, so use 0.
  (funcall icicle-candidate-action-fn candidate icicle-search-replacement)) ; Call with second arg.

(defun icicle-search-replace-all-search-hits (candidates) ; Bound to `M-|' (for `icicle-search').
  "Default alternative list action function for `icicle-search'.
CANDIDATES is a list of search-hit strings.  They are all matched by
the initial regexp (context regexp)."
  (let ((icicle-last-completion-command  icicle-last-completion-command)
        (compl-win                       (get-buffer-window "*Completions*" 0)))
;;; $$$$$$ These are now avoided always for all candidates, in `icicle-all-candidates-action-1'.
;;;     (icicle-minibuffer-message-ok-p  nil) ; Avoid delays from `icicle-msg-maybe-in-minibuffer'.
;;;     (icicle-help-in-mode-line-flag   nil)) ; Avoid delays for individual candidate help.
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win icicle-completion-candidates)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates))))
    (dolist (cand+mrker  (mapcar icicle-get-alist-candidate-function candidates))
      (icicle-search-action-1 cand+mrker icicle-search-replacement)))
  (select-window (minibuffer-window))
  (select-frame-set-input-focus (selected-frame)))

(defun icicle-search-action (string &optional replace-string) ; Bound to `C-RET' (`icicle-search').
  "Default completion action function for `icicle-search'.
STRING is a search-hit string.  It is matched by the initial regexp
\(context regexp).

1. Move to the STRING occurrence in original buffer.  Highlight it.
2. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the STRING occurrence.
3. If REPLACE-STRING is non-nil, replace the current match with it.
   If `icicle-search-replace-whole-candidate-flag' is non-nil, replace
   the entire STRING occurrence.  Otherwise, replace only the part
   that matches the current input.
4. Highlight the current candidate in *Completions*.

   Note: The replacement can be nearly anything allowed as a
   replacement by `query-replace-regexp', including Lisp-evaluation
   constructs (`\,...')."
  (prog1
      (let* ((icicle-whole-candidate-as-text-prop-p  t)
             ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would bind that
             ;; to nil to force using the alist, because we would be performing side effects on it.
             (cand+mrker  (funcall icicle-get-alist-candidate-function string)))
        (icicle-search-action-1 cand+mrker replace-string))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))))

;; Free vars here: `orig-win-explore' is bound in `icicle-explore'.
(defun icicle-search-action-1 (cand+mrker &optional replace-string)
  "Same as `icicle-search-action', but using full candidate, not string.
CAND+MRKER is a full alist completion-candidate entry, not just a
display string as in `icicle-search-action'."
  (when icicle-completion-candidates
    (condition-case icicle-search-action-1
        (progn
          ;; Move cursor to the match in the original buffer and highlight it.
          (let* ((candidate   (if (consp (car-safe cand+mrker))
                                  (car-safe (car-safe cand+mrker))
                                (car-safe cand+mrker)))
                 (marker      (cdr-safe cand+mrker))
                 (icicle-search-in-context-fn
                  (or icicle-search-in-context-fn 'icicle-search-in-context-default-fn)))
            (unless marker (error "No such occurrence"))
            (condition-case icicle-search-action-1-save-window
                (save-selected-window
                  (when (window-live-p orig-win-explore) (select-window orig-win-explore))
                  (let ((completion-ignore-case  case-fold-search)
                        (buf                     (marker-buffer marker)))
                    (unless (bufferp buf) (error "No such buffer: %s" buf))
                    (pop-to-buffer buf)
                    (raise-frame)
                    (goto-char marker)
                    (unless (pos-visible-in-window-p) (recenter -2))
                    ;; Highlight current search context using `icicle-search-main-regexp-current'.
                    (icicle-place-overlay (- marker (length candidate)) marker
                                          'icicle-search-current-overlay
                                          'icicle-search-main-regexp-current
                                          202 buf)
                    (funcall icicle-search-in-context-fn cand+mrker replace-string)
                    (icicle-highlight-candidate-in-Completions)
                    (run-hooks 'icicle-search-hook)))
              (error        ; Ignore disappearance of `*Completions*'.
               (unless (string-match "Wrong type argument: window-live-p,"
                                     (error-message-string icicle-search-action-1-save-window))
                 (error (message (error-message-string icicle-search-action-1-save-window))
                        (error-message-string icicle-search-action-1-save-window)))))
            nil))                       ; Return nil for success.
      (error (message (error-message-string icicle-search-action-1))
             (error-message-string icicle-search-action-1)))))

(defun icicle-search-in-context-default-fn (cand+mrker replace-string)
  "Default value of `icicle-search-in-context-fn'."
  (let ((candidate  (if (consp (car-safe cand+mrker))
                        (car-safe (car-safe cand+mrker))
                      (car-safe cand+mrker)))
        (marker     (cdr-safe cand+mrker)))
    ;; Highlight match and possibly replace.  If replacement tried, then update the dialog state.
    (when (save-excursion (save-restriction ; Search within the current search context.
                            (narrow-to-region (- marker (length candidate)) marker)
                            (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))
      
      ;; Update, since replacement might have changed the current candidate:
      ;; Rehighlight current context, update last candidate, update candidate in minibuffer.
      (if icicle-search-highlight-all-current-flag
          (let ((icicle-search-highlight-all-current-flag  nil))
            (icicle-search-highlight-input-matches-here))
        (let ((ov  icicle-search-current-overlay))
          (save-restriction (narrow-to-region (overlay-start ov) (overlay-end ov))
                            (icicle-search-highlight-input-matches-here))))
      (if (null icicle-completion-candidates) ; If have already replaced all, then no candidates.
          (when (overlayp icicle-search-current-overlay)
            (delete-overlay icicle-search-current-overlay))
        (let* ((cand+mrker  (funcall icicle-get-alist-candidate-function
                                     (setq icicle-last-completion-candidate
                                           (elt icicle-completion-candidates icicle-candidate-nb))))
               (marker      (cdr-safe cand+mrker)))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            ;; Highlight current search context using `icicle-search-main-regexp-current'.
            (icicle-place-overlay (- marker (if (consp (car cand+mrker))
                                                (length (caar cand+mrker))
                                              (length (car cand+mrker))))
                                  marker 'icicle-search-current-overlay
                                  'icicle-search-main-regexp-current 202 (current-buffer))
            (unless icicle-search-highlight-all-current-flag
              (let ((ov  icicle-search-current-overlay))
                (save-restriction (narrow-to-region (overlay-start ov) (overlay-end ov))
                                  (icicle-search-highlight-input-matches-here)))))
          (save-selected-window
            (select-window (minibuffer-window))
            (icicle-clear-minibuffer)
            (setq icicle-nb-of-other-cycle-candidates  (length icicle-completion-candidates))
            (icicle-insert-cand-in-minibuffer icicle-last-completion-candidate t)
            (icicle-show-help-in-mode-line icicle-last-completion-candidate))))))
  (let ((icicle-candidate-nb  icicle-candidate-nb))
    (icicle-complete-again-update)))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-and-maybe-replace (cand+mrker replace-string)
  "Highlight within search context and replace using REPLACE-STRING.
If REPLACE-STRING is nil, no replacement occurs.
Arguments are the same as for `icicle-search-in-context-fn'.
Return non-nil if replacement occurred, nil otherwise."
  (icicle-search-highlight-context-levels)
  (icicle-search-highlight-input-matches-here)
  (let ((replacement-p  nil))
    (when replace-string
      (setq replacement-p  t)
      (goto-char (point-min))
      (let ((candidate  (if (consp (car-safe cand+mrker))
                            (car-safe (car-safe cand+mrker))
                          (car-safe cand+mrker)))
            (ecm        (and icicle-search-replace-common-match-flag icicle-search-ecm)))
        (cond (icicle-search-replace-whole-candidate-flag
               (cond ((string= candidate replace-string) ; Sanity check only.
                      (save-restriction (widen) (message "Replacement = candidate, and \
current input matches candidate") (sit-for 2))
                      (setq replacement-p  nil))
                     (t
                      (set-match-data (list (point-min) (point-max)))
                      (icicle-search-replace-match replace-string
                                                   (icicle-search-replace-fixed-case-p
                                                    icicle-search-context-regexp)))))
              ((not (save-excursion (re-search-forward (or ecm icicle-current-input) nil t)))
               (save-restriction (widen)
                                 (message "Text to be replaced not found in candidate") (sit-for 2))
               (setq replacement-p  nil))
              (t
               (save-match-data
                 (let ((first-p  t))
                   ;; The condition order is important.  Don't search unless first time (or all)
                   (while (and (or first-p icicle-all-candidates-action)
                               (re-search-forward (or ecm icicle-current-input) nil 'move-to-end))
                     (setq first-p  nil)
                     (icicle-search-replace-match replace-string
                                                  (icicle-search-replace-fixed-case-p
                                                   icicle-current-input)))))))
        (when replacement-p
          ;; Update the alist and `minibuffer-completion-table' with the new text.

          ;; An ALTERNATIVE approach would be to use `icicle-search-replace-cand-in-alist'.
          ;; In that case we would:
          ;; 1. Bind `icicle-whole-candidate-as-text-prop-p' to nil (in `icicle-search-action'
          ;;    and `icicle-search-help').
          ;; 2. Use these two lines, instead of calling `icicle-search-replace-cand-in-mct'.
          ;;    (icicle-search-replace-cand-in-alist cand+mrker
          ;;                                         (buffer-substring (point-min) (point-max)))
          ;;    (setq minibuffer-completion-table
          ;;          (car (icicle-mctize-all icicle-candidates-alist nil)))
          ;;  If we used that method (as we used to), then users could not sort the search hits.

          (icicle-search-replace-cand-in-mct cand+mrker (buffer-substring (point-min) (point-max)))

          ;; If we are replacing input matches within a search context, and there are no more matches
          ;; in the current context, then this context is removed as a candidate. If the current
          ;; action command is one that moves to the next or previous candidate, then we might need
          ;; to adjust the current candidate number, to compensate for the removal.
          ;;
          ;; If the current action command is one (e.g. `C-S-next'), that moves to the next candidate
          ;; to do its action, then move back one.  If the current action acts on the previous
          ;; candidate (e.g. `C-S-prior'), and that previous candidate is the last one, then move
          ;; forward one candidate, to the first.
          (when (and icicle-acting-on-next/prev
                     (not (save-excursion (goto-char (point-min))
                                          (re-search-forward icicle-current-input nil t))))
            (let ((nb-cands  (1- (length icicle-completion-candidates)))) ; -1 for replaced one.
              (unless (wholenump nb-cands) (setq nb-cands  0))
              (setq icicle-candidate-nb  (cond ((not icicle-candidate-nb) 0)
                                               ((eq icicle-acting-on-next/prev 'forward)
                                                (if (zerop icicle-candidate-nb)
                                                    (1- nb-cands)
                                                  (1- icicle-candidate-nb)))
                                               ((eq icicle-candidate-nb nb-cands)  0)
                                               (t icicle-candidate-nb)))
              (when (> icicle-candidate-nb nb-cands) (setq icicle-candidate-nb  0))
              (when (< icicle-candidate-nb 0) (setq icicle-candidate-nb  nb-cands))))

          (let ((icicle-candidate-nb             icicle-candidate-nb)
                (icicle-minibuffer-message-ok-p  nil)) ; Inhibit no-candidates message.
            (icicle-complete-again-update))

          ;; If we are using `C-S-RET' and we are on the last candidate, then wrap to the first one.
          (when (and (not icicle-acting-on-next/prev)
                     (or (not icicle-candidate-nb)
                         (>= icicle-candidate-nb (length icicle-completion-candidates))))
            (setq icicle-candidate-nb  0))
          (icicle-highlight-candidate-in-Completions)
          (icicle-search-highlight-context-levels))))
    replacement-p)) ; Return indication of whether we tried to replace something.

(defun icicle-search-replace-match (replace-string fixedcase)
  "Replace current match with REPLACE-STRING, interpreting escapes.
Treat REPLACE-STRING as it would be treated by `query-replace-regexp'.
FIXEDCASE is as for `replace-match'.  Non-nil means do not alter case."
  (if (fboundp 'query-replace-compile-replacement) ; Emacs 22.
      (let ((compiled
             (save-match-data
               (query-replace-compile-replacement replace-string
                                                  (not icicle-search-replace-literally-flag)))))
        (condition-case icicle-search-replace-match1
            (let ((enable-recursive-minibuffers    t) ; So we can read input from \?.
                  ;; Save and restore these, because we might read input from \?.
                  (icicle-last-completion-command  icicle-last-completion-command)
                  (icicle-last-input               icicle-last-input))
              (replace-match-maybe-edit
               (if (consp compiled)
                   ;; `replace-count' is free here, bound in `icicle-search'.
                   (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
                 compiled)
               fixedcase icicle-search-replace-literally-flag nil (match-data)))
          (buffer-read-only (ding) (error "Buffer is read-only"))
          (error (icicle-remove-Completions-window) (error "No match for %s" replace-string))))
    (condition-case icicle-search-replace-match2 ; Emacs < 22.  Try to interpret `\'.
        (replace-match replace-string fixedcase icicle-search-replace-literally-flag)
      (error (replace-match replace-string fixedcase t))))) ;   If error, replace literally.

(defun icicle-search-highlight-context-levels ()
  "Highlight context levels differently (up to 8 levels).
No such highlighting is done if any of these conditions holds:
 * `icicle-search-context-level' is not 0 (search context < regexp).
 * `icicle-search-highlight-context-levels-flag' is nil.
 * `icicle-search-context-regexp' is nil (non-regexp searching)."
  (unless (or (/= icicle-search-context-level 0)
              (not icicle-search-highlight-context-levels-flag)
              (not icicle-search-context-regexp)) ; E.g. text-property searching
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (save-match-data
      (let ((level       1)
            (max-levels  (min (regexp-opt-depth icicle-search-context-regexp) 8)))
        (goto-char (point-min))
        (re-search-forward icicle-search-context-regexp nil t)
        (condition-case nil
            (while (<= level max-levels)
              (let ((ov  (make-overlay (match-beginning level) (match-end level))))
                (push ov icicle-search-level-overlays)
                (overlay-put ov 'priority (+ 205 level)) ; > ediff's 100+, < isearch-overlay's 1001.
                (overlay-put ov 'face (intern (concat "icicle-search-context-level-"
                                                      (number-to-string level)))))
              (setq level  (1+ level)))
          (error nil))))))

;; Free var here: `icicle-search-ecm' is bound in `icicle-search'.
(defun icicle-search-highlight-input-matches-here ()
  "Highlight all input matches in the current search context."
  (unless (or (> 0 icicle-search-highlight-threshold) (string= "" icicle-current-input))
    (goto-char (point-min))
    (when (and (not icicle-search-highlight-all-current-flag)
               (overlayp icicle-search-refined-overlays))
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  nil))
    (unless icicle-search-highlight-all-current-flag
      (while icicle-search-refined-overlays
        (delete-overlay (car icicle-search-refined-overlays))
        (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays))))
    (let ((ov  nil))
      (save-match-data
        (while (and (not (eobp)) (re-search-forward (or icicle-search-ecm icicle-current-input)
                                                    nil 'move-to-end))
          (setq ov  (make-overlay (match-beginning 0) (match-end 0)))
          (push ov icicle-search-refined-overlays)
          (overlay-put ov 'priority 220) ; Greater than any possible context-level priority (213).
          (overlay-put ov 'face 'icicle-search-current-input))))))

(defun icicle-search-replace-fixed-case-p (from)
  "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil.
Retuns non-nil if FROM is a string and one of the following holds:
 * FROM is not all lowercase
 * `case-replace' or `case-fold-search' is nil"
  (and from (not (and case-fold-search case-replace (string= from (downcase from))))))

;; Not used for now - this could replace using mct.  In that case, user must not be able to sort.
(defun icicle-search-replace-cand-in-alist (cand+mrker new-cand)
  "In `icicle-candidates-alist', replace car of CAND+MRKER by NEW-CAND.
Replace only the first occurrence of CAND+MRKER in
`icicle-candidates-alist'.  (There should be only one.)"
  (let ((newlist  icicle-candidates-alist))
    (catch 'icicle-search-replace-cand-in-alist
      (while newlist
        (when (equal (car newlist) cand+mrker)
          (setcar newlist (cons new-cand (cdr-safe cand+mrker)))
          (throw 'icicle-search-replace-cand-in-alist nil))
        (setq newlist  (cdr newlist))))
    icicle-candidates-alist))

(defun icicle-search-replace-cand-in-mct (cand+mrker new-cand)
  "Replace candidate in `minibuffer-completion-table'.
Update CAND+MRKER itself to use NEW-CAND (replacement string).
Any text properties on CAND+MRKER's string are preserved.
Use this only with a `minibuffer-completion-table' derived from an alist."
  (let ((newlist  minibuffer-completion-table))
    (catch 'icicle-search-replace-cand-in-mct
      ;; CAND+MRKER: ("aa" . c) or (("aa" "bb") . c)
      ;; `minibuffer-completion-table' entry: ("aa" "aa" . c) or ("aa^G^Jbb" . (("aa" "bb") . c))
      (while newlist
        (when (equal (cdr (car newlist)) cand+mrker)
          (let ((new-compl  (if (consp (car cand+mrker)) ; New completion: "QQ" or ("QQ" "bb")
                                (cons new-cand (cdar cand+mrker))
                              new-cand))
                (old-cand   (if (consp (car cand+mrker)) (caar cand+mrker) (car cand+mrker)))
                rep-cand)
            (setcar newlist (icicle-mctized-full-candidate (cons new-compl (cdr-safe cand+mrker))))
            ;; NEWLIST is done.
            ;; Now update CAND+MRKER to reflect the replacement but with the text properties it had.
            ;; (cdar NEWLIST) is the new cand+mrker.  Its car or caar is the replaced candidate.
            ;; It is the first field of the multi-completion, in the latter case.
            (setq rep-cand  (if (consp (car cand+mrker)) (caar (cdar newlist)) (car (cdar newlist))))
            (let ((len-old  (length old-cand))
                  (len-rep  (length rep-cand))
                  (ii       0)
                  props)
              (while (< ii len-old)
                (setq props  (text-properties-at ii old-cand))
                (when (<= ii len-rep) (add-text-properties ii (1+ ii) props rep-cand))
                (setq ii  (1+ ii)))
              (let ((last-props  (text-properties-at (1- len-old) old-cand)))
                (when (> len-rep len-old)
                  (add-text-properties len-old len-rep last-props rep-cand))))
            (if (consp (car cand+mrker))
                (setcar (car cand+mrker) rep-cand)
              (setcar cand+mrker rep-cand)))
          (throw 'icicle-search-replace-cand-in-mct nil))
        (setq newlist  (cdr newlist))))
    minibuffer-completion-table))

(defun icicle-search-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-search' commands."
  (icicle-msg-maybe-in-minibuffer
   (let* ((icicle-whole-candidate-as-text-prop-p  t)
          ;; Alternative: If we used `icicle-search-replace-cand-in-alist', then we would bind that
          ;; to nil to force using the alist, because we would be performing side effects on it.
          (marker  (cdr (funcall icicle-get-alist-candidate-function cand))))
     (concat "Buffer: `" (buffer-name (marker-buffer marker))
             (format "', Position: %d" (marker-position marker))))))

;;;###autoload
(defun icicle-search-keywords (beg end keywords require-match ; Bound to `C-c ^'.
                               &optional where &rest args)
  "Search with one or more keywords, which can each be a regexp.
Text that matches *any* of the keywords is found.

You can use completion to choose one or more previously entered
regexps (using `C-RET', `C-mouse-2', `C-next', and so on), or you can
enter new keywords (using `C-RET').  Use `RET' or `mouse-2' to choose
the last keyword.

Keywords are interpreted as regexps.  You can change to substring
completion instead, matching regexp special characters literally, by
using `C-`' during completion to toggle `icicle-regexp-quote-flag'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the `icicle-search'
documentation."
  (interactive
   `(,@(icicle-region-or-buffer-limits)
       ,(icicle-group-regexp (mapconcat #'icicle-group-regexp (icicle-keyword-list) "\\|"))
       ,(not icicle-show-multi-completion-flag)
       ,(icicle-search-where-arg)))
  (icicle-search beg end keywords (not icicle-show-multi-completion-flag) where))

(defalias 'icicle-regexp-list 'icicle-keyword-list)
;;;###autoload
(icicle-define-command icicle-keyword-list ; Command name
  "Choose a list of keywords. The list of keywords (strings) is returned.
You can choose from keywords entered previously or enter new keywords
using `C-RET'.  Each keyword is a regexp.  The regexps are OR'd, and
the resulting regexp is usable for `icicle-search'." ; Doc string
  (lambda (name)                        ; Action function
    (push name keywords)
    (message "Added keyword `%s'" name))
  "Choose keyword (regexp) (`RET' when done): " ; `completing-read' args
  (mapcar #'list (icicle-remove-duplicates regexp-history)) nil nil nil 'regexp-history nil nil
  ((keywords                              nil) ; Bindings
   (icicle-use-candidates-only-once-flag  t))
  nil nil                               ; First code, undo code
  (prog1 (setq keywords  (nreverse (delete "" keywords))) ; Last code - return the list of keywords.
    (when (interactive-p) (message "Keywords (regexps): %S" keywords))))

(defun icicle-group-regexp (regexp)
  "Wrap REGEXP between regexp parens, as a regexp group."
  (concat "\\(" regexp "\\)"))

;;;###autoload
(icicle-define-command icicle-search-bookmark ; Command name
  "Search bookmarked text.
If you use library `bookmark+.el', and a bookmark specifies a nonempty
region, then search only the text in that region.

See also `icicle-search-bookmarks-together', which searches bookmarks
together instead of one at a time.

1. Enter a context regexp, to define the possible search-hit contexts.
2. Choose a bookmark using completion.  It is opened.
3. (Optional) Type some text to be matched in the search contexts.
4. Navigate to matches (search hits) using `C-next' etc.
5. Finish with that bookmark using `RET' (stay) or `C-g' (skip).
6. (Optional) Repeat steps 2-5 for other bookmarks." ; Doc string
  icicle-search-bookmark-action         ; Action function
  prompt icicle-candidates-alist nil (not icicle-show-multi-completion-flag) ; `completing-read' args
  nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) nil
  ((enable-recursive-minibuffers             t) ; In case we read input, e.g. File changed on disk...
   (completion-ignore-case                   bookmark-completion-ignore-case)
   (prompt                                   "Search bookmark: ")
   (regexp                                   (icicle-search-read-context-regexp))
   (icicle-list-use-nth-parts                '(1))
   (icicle-candidate-properties-alist        (if (not icicle-show-multi-completion-flag)
                                                 nil
                                               (if (facep 'file-name-shadow)
                                                   '((2 (face file-name-shadow))
                                                     (3 (face bookmark-menu-heading)))
                                                 '((3 (face bookmark-menu-heading))))))
   (icicle-transform-function                (if (interactive-p) nil icicle-transform-function))
   (icicle-whole-candidate-as-text-prop-p    t)
   (icicle-transform-before-sort-p           t)
   (icicle-delete-candidate-object           'icicle-bookmark-delete-action)
   (bookmark-automatically-show-annotations  nil) ; Do not show annotations
   (icicle-sort-orders-alist
    (append '(("in *Bookmark List* order") ; Renamed from "turned OFF'.
              ("by bookmark name" . icicle-alpha-p))
            (and (featurep 'bookmark+)
                 '(("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
                   ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p)
                   ("by last buffer or file access" (bmkp-buffer-last-access-cp
                                                     bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
                    icicle-alpha-p)
                   ("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
                   ("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)
                   ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
                   ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
                    icicle-alpha-p)
                   ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
                    icicle-alpha-p)
                   ("by Info location" (bmkp-info-cp) icicle-alpha-p)
                   ("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)
                   ("by URL" (bmkp-url-cp) icicle-alpha-p)
                   ("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                                        bmkp-local-file-type-cp bmkp-handler-cp)
                    icicle-alpha-p)))
            '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
              ("case insensitive" . icicle-case-insensitive-string-less-p))))
   (icicle-candidate-help-fn
    #'(lambda (cand)
        (when (and (featurep 'bookmark+) icicle-show-multi-completion-flag)
          (setq cand  (funcall icicle-get-alist-candidate-function cand))
          (setq cand  (cons (caar cand) (cdr cand))))
        (if (featurep 'bookmark+)
            (if current-prefix-arg
                (bmkp-describe-bookmark-internals cand)
              (bmkp-describe-bookmark cand))
          (icicle-msg-maybe-in-minibuffer (icicle-bookmark-help-string cand)))))
   (icicle-candidates-alist
    (if (not (featurep 'bookmark+))
        (mapcar #'(lambda (cand)
                    (list (icicle-candidate-short-help (icicle-bookmark-help-string cand)
                                                       (icicle-bookmark-propertize-candidate cand))))
                (bookmark-all-names))   ; Loads bookmarks file, defining `bookmark-alist'.
      (bookmark-maybe-load-default-file) ; Loads bookmarks file, defining `bookmark-alist'.
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            (cons `(,(icicle-candidate-short-help
                                      (icicle-bookmark-help-string bname)
                                      (icicle-bookmark-propertize-candidate bname))
                                    ,file/buf
                                    ,@(and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
              (or (and (or (and (not icicle-bookmark-refresh-cache-flag)
                                (not (consp current-prefix-arg)))
                           (and icicle-bookmark-refresh-cache-flag (consp current-prefix-arg)))
                       bmkp-sorted-alist)
                  (setq bmkp-sorted-alist
                        (bmkp-sort-and-remove-dups bookmark-alist)))))))
  (progn                                ; First code
    (require 'bookmark)
    (when (featurep 'bookmark+)
      ;; Bind keys to narrow bookmark candidates by type.  Lax is for multi-completion case.
      (dolist (map  '(minibuffer-local-must-match-map minibuffer-local-completion-map))
        (define-key (symbol-value map) "\C-\M-b" 'icicle-bookmark-non-file-narrow)
        (define-key (symbol-value map) "\C-\M-d" 'icicle-bookmark-dired-narrow)
        (define-key (symbol-value map) "\C-\M-f" 'icicle-bookmark-file-narrow)
        (define-key (symbol-value map) "\C-\M-g" 'icicle-bookmark-gnus-narrow)
        (define-key (symbol-value map) "\C-\M-i" 'icicle-bookmark-info-narrow)
        (define-key (symbol-value map) "\C-\M-m" 'icicle-bookmark-man-narrow)
        (define-key (symbol-value map) "\C-\M-r" 'icicle-bookmark-region-narrow)
        (define-key (symbol-value map) "\C-\M-u" 'icicle-bookmark-url-narrow)
        (define-key (symbol-value map) "\C-\M-w" 'icicle-bookmark-w3m-narrow)
        (define-key (symbol-value map) "\C-\M-@" 'icicle-bookmark-remote-file-narrow)
        (define-key (symbol-value map) [(control meta ?B)]
          'icicle-bookmark-bookmark-list-narrow) ; `C-M-B'
        (define-key (symbol-value map) [(control meta ?F)]
          'icicle-bookmark-local-file-narrow) ; `C-M-F'
        (define-key (symbol-value map) [(control meta ?K)]
          'icicle-bookmark-desktop-narrow)))) ; `C-M-K'
  (icicle-bookmark-cleanup-on-quit)     ; Undo code
  (icicle-bookmark-cleanup))            ; Last code

(defun icicle-search-bookmark-action (bookmark-name)
  "Action function for `icicle-search-bookmark'."
  (setq bookmark-name  (icicle-transform-multi-completion bookmark-name))
  (bookmark-jump-other-window bookmark-name)
  (setq mark-active  nil)               ; Unhighlight region, so you can see search hits etc.
  (let ((icicle-show-Completions-initially-flag  t)
        (icicle-candidate-action-fn              'icicle-search-action)
        (enable-recursive-minibuffers            t)
        (beg
         (or (and (featurep 'bookmark+) (bmkp-region-bookmark-p bookmark-name)
                  (bookmark-get-position bookmark-name))
             (point-min)))
        (end
         (or (and (featurep 'bookmark+) (bmkp-region-bookmark-p bookmark-name)
                  (bmkp-get-end-position bookmark-name))
             (point-max))))
    (when (= beg end) (setq beg  (point-min)    end  (point-max)))
    (icicle-search beg end regexp t))
  (with-current-buffer (window-buffer (minibuffer-window)) (icicle-erase-minibuffer)))

;; Similar to `icicle-define-bookmark-command-1' in `icicles-cmd1.el'.  Could combine them.
(defmacro icicle-define-search-bookmark-command (type &optional prompt &rest args)
  "Define Icicles multi-command for searching bookmarks of type TYPE.
TYPE is a string to be used for the doc string, default prompt, and in
 function names.  It should be lowercase and contain no spaces.
Optional arg PROMPT is the completion prompt.
ARGS is a list of any additional arguments to be passed to the
 appropriate `bmkp-TYPE-alist-only' function.

The command defined raises an error unless library `bookmark+.el' can
be loaded."
  `(icicle-define-command
    ,(intern (format "icicle-search-%s-bookmark" type)) ; Command name
    ,(format "Search %s bookmark text.
Like `icicle-search-bookmark', but with %s bookmarks only.
You need library `bookmark+.el' for this command." type type) ; Doc string
    icicle-search-bookmark-action       ; Action function
    prompt1 icicle-candidates-alist nil ; `completing-read' args
    (not icicle-show-multi-completion-flag)
    nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
    nil nil
    ((IGNORED1                                 (unless (require 'bookmark+ nil t) ; Bindings
                                                 (error "You need library `bookmark+.el' for this \
command")))
     (IGNORED2                                 (bookmark-maybe-load-default-file)) ; `bookmark-alist'.
     (enable-recursive-minibuffers             t) ; In case we read input, e.g. File changed on...
     (completion-ignore-case                   bookmark-completion-ignore-case)
     (prompt1                                  ,(or prompt (format "Search %s bookmark: " type)))
     (icicle-list-use-nth-parts                '(1))
     (icicle-candidate-properties-alist        (if (not icicle-show-multi-completion-flag)
                                                   nil
                                                 (if (facep 'file-name-shadow)
                                                     '((2 (face file-name-shadow))
                                                       (3 (face bookmark-menu-heading)))
                                                   '((3 (face bookmark-menu-heading))))))
     (icicle-transform-function                (if (interactive-p) nil icicle-transform-function))
     (icicle-whole-candidate-as-text-prop-p    t)
     (icicle-transform-before-sort-p           t)
     (icicle-delete-candidate-object           'icicle-bookmark-delete-action)
     (regexp                                   (icicle-search-read-context-regexp))
     (bookmark-automatically-show-annotations  nil) ; Do not show annotations
     (icicle-sort-orders-alist
      (append
       '(("in *Bookmark List* order")   ; Renamed from "turned OFF'.
         ("by bookmark name" . icicle-alpha-p)
         ("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
         ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
       (and (member ,type '("info" "region"))
        '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
       (and (member ,type '("gnus" "region"))
        '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
       (and (member ,type '("url" "region"))
        '(("by URL" (bmkp-url-cp) icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "gnus" "info" "man" "url")))
        '(("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                               bmkp-local-file-type-cp bmkp-handler-cp)
           icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "dired" "non-file")))
        '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
       (and (member ,type '("local-file" "file" "dired" "region"))
        '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
          ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
          ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)
          ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
           icicle-alpha-p)))
       (and (not (string= ,type "desktop"))
        '(("by last buffer or file access" (bmkp-buffer-last-access-cp
                                            bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)))
       (and (get-buffer "*Bookmark List*")
        '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
           icicle-alpha-p)))
       '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
         ("case insensitive" . icicle-case-insensitive-string-less-p))))
     (icicle-candidate-help-fn
      #'(lambda (cand)
          (when icicle-show-multi-completion-flag
            (setq cand  (funcall icicle-get-alist-candidate-function cand))
            (setq cand  (cons (caar cand) (cdr cand))))
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))))
     (icicle-candidates-alist
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            ;; Emacs 20 byte-compiler bug prevents using backslash syntax here.
                            (cons (append (list (icicle-candidate-short-help
                                                 (icicle-bookmark-help-string bname)
                                                 (icicle-bookmark-propertize-candidate bname))
                                                file/buf)
                                          (and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    (condition-case nil ; Ignore errors, e.g. from bad or stale bookmark records.
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
       (bmkp-sort-and-remove-dups (funcall ',(intern (format "bmkp-%s-alist-only" type))
                                   ,@args)))))
    nil                                 ; First code
    (icicle-bookmark-cleanup-on-quit)   ; Undo code
    (icicle-bookmark-cleanup)))         ; Last code

;; The following sexps macro-expand to define these commands:
;;  `icicle-search-all-tags-bookmark'
;;  `icicle-search-all-tags-regexp-bookmark'
;;  `icicle-search-bookmark-list-bookmark'
;;  `icicle-search-desktop-bookmark'
;;  `icicle-search-dired-bookmark'
;;  `icicle-search-file-bookmark'
;;  `icicle-search-gnus-bookmark'
;;  `icicle-search-info-bookmark'
;;  `icicle-search-local-file-bookmark'
;;  `icicle-search-man-bookmark'
;;  `icicle-search-non-file-bookmark'
;;  `icicle-search-region-bookmark'
;;  `icicle-search-remote-file-bookmark'
;;  `icicle-search-some-tags-bookmark'
;;  `icicle-search-some-tags-regexp-bookmark'
;;  `icicle-search-specific-buffers-bookmark'
;;  `icicle-search-specific-files-bookmark'
;;  `icicle-search-this-buffer-bookmark'
;;  `icicle-search-url-bookmark'
;;  `icicle-search-w3m-bookmark'

(icicle-define-search-bookmark-command "all-tags" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "all-tags-regexp" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "bookmark-list")
(icicle-define-search-bookmark-command "desktop")
(icicle-define-search-bookmark-command "dired")
(icicle-define-search-bookmark-command "file")
(icicle-define-search-bookmark-command "gnus")
(icicle-define-search-bookmark-command "info")
(icicle-define-search-bookmark-command "local-file")
(icicle-define-search-bookmark-command "man")
(icicle-define-search-bookmark-command "non-file")
(icicle-define-search-bookmark-command "region" "Search region: ")
(icicle-define-search-bookmark-command "remote-file")
(icicle-define-search-bookmark-command "some-tags" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "some-tags-regexp" nil (bmkp-read-tags-completing))
(icicle-define-search-bookmark-command "specific-buffers" nil (icicle-bookmarked-buffer-list))
(icicle-define-search-bookmark-command "specific-files" nil (icicle-bookmarked-file-list))
(icicle-define-search-bookmark-command "this-buffer")
(icicle-define-search-bookmark-command "url")
(icicle-define-search-bookmark-command "w3m")

;;;###autoload
(defun icicle-search-char-property (beg end require-match
                                    &optional where prop values predicate)
  "Search for text that has a character property with a certain value.
If the property is `face' or `font-lock-face', then you can pick
multiple faces, using completion.  Text is then searched that has a
face property that includes any of the selected faces.  If you choose
no face (empty input), then text with any face is found.

By \"character property\" is meant either an overlay property or a
text property.  If you want to search for only an overlay property or
only a text property, then use `icicle-search-overlay-property' or
`icicle-search-text-property' instead.

Non-interactively, arguments BEG, END, REQUIRE-MATCH, and WHERE are as
for `icicle-search'.  Arguments PROP, VALUES, and PREDICATE are passed
to `icicle-search-char-property-scan' to define the search contexts.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values nil
                 predicate))

;;;###autoload
(defun icicle-search-overlay-property (beg end require-match &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only overlay property.
That is, do not also search a text property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'overlay
                 predicate))

;;;###autoload
(defun icicle-search-text-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only text property.
That is, do not also search an overlay property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'text
                 predicate))

(defun icicle-search-property-args ()
  "Read and return interactive arguments for `icicle-search-*-property'."
  (let* ((where    (icicle-search-where-arg))
         (beg+end  (icicle-region-or-buffer-limits))
         (beg1     (car beg+end))
         (end1     (cadr beg+end))
         (props    (mapcar #'(lambda (prop) (list (symbol-name prop)))
                           (icicle-char-properties-in-buffers where beg1 end1)))
         (prop     (intern (completing-read "Property to search: " props nil nil nil nil "face")))
         (values   (if (memq prop '(face font-lock-face))
                       (let ((faces  (icicle-face-list)))
                         (if faces (mapcar #'intern faces) (face-list))) ; Default: all faces.
                     (list (intern (icicle-completing-read-history
                                    "Property value: " 'icicle-char-property-value-history))))))
    `(,beg1 ,end1 ,(not icicle-show-multi-completion-flag) ,where ,prop ,values)))

(defun icicle-char-properties-in-buffers (where beg end &optional type)
  "List of all character properties in WHERE.
The other arguments are passed to `icicle-char-properties-in-buffer'.
Only the character properties are included, not their values.
WHERE is a list of buffers, a list of files, or a list of region
  bookmarks (in which case you must also use library `bookmark+.el').
  If nil, then only the current buffer is used.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf  where) (icicle-char-properties-in-buffer buf nil nil type)))
        ((and (consp where)             ; List of files - search files.
              (stringp (car where))
              (file-exists-p (car where)))
         (dolist (file  where)
           (icicle-char-properties-in-buffer (find-file-noselect file) nil nil type)))
        ((consp where)                  ; Search bookmarked regions.
         (unless (require 'bookmark+ nil t) (error "This requires library `bookmark+.el'"))
         (let (buf+beg buf beg end)
           (dolist (bmk  where)
             (setq buf+beg  (bookmark-jump-noselect bmk)
                   buf      (car buf+beg)
                   beg      (cdr buf+beg)
                   end      (bmkp-get-end-position bmk))
             (when (bufferp buf) (icicle-char-properties-in-buffer (get-buffer buf) beg end type)))))
        (t                              ; Search this buffer only.
         (icicle-char-properties-in-buffer (current-buffer) beg end type))))

(defun icicle-char-properties-in-buffer (&optional buffer beg end type)
  "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((props  ())
        ovrlays curr-props)
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg end)
          (setq beg  (point-min)
                end  (point-max)))
        (when (or (not type) (eq type 'overlay)) ; Get overlay properties.
          (setq ovrlays  (overlays-in beg end))
          (dolist (ovrly  ovrlays)
            (setq curr-props  (overlay-properties ovrly))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))
        (when (or (not type) (eq type 'text)) ; Get text properties.
          (while (< beg end)
            (setq beg         (or (next-property-change beg nil end) end)
                  curr-props  (text-properties-at beg))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))))
    props))

(defun icicle-search-char-property-scan (buffer beg end prop values type predicate)
  "Scan BUFFER from BEG to END for character property PROP with VALUES.
Push hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are nil, scan entire BUFFER.

Find text with a PROP value that overlaps with VALUES.  That is, if
the value of PROP is an atom, then it must be a member of VALUES; if
it is a list, then at least one list element must be a member of
VALUES.

TYPE is `overlay', `text', or nil, and specifies the type of character
property - nil means look for both overlay and text properties.

If PREDICATE is non-nil, then push only the hits for which it holds.
PREDICATE is nil or a Boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context"
  (let ((add-bufname-p  (and buffer icicle-show-multi-completion-flag))
        (temp-list      ())
        (zone-end       nil))
    (unless buffer (setq buffer  (current-buffer)))
    (when (bufferp buffer)              ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg end)
          (setq beg  (point-min)
                end  (point-max)))
        (condition-case icicle-search-char-property-scan
            (save-excursion
              (while (and (< beg end)
                          (let* ((charval  (and (or (not type) (eq type 'overlay))
                                                (get-char-property beg prop)))
                                 (textval  (and (or (not type) (eq type 'text))
                                                (get-text-property beg prop)))
                                 (currval  (icicle-flat-list charval textval)))
                            (not (icicle-set-intersection values currval))))
                (setq beg  (icicle-next-single-char-property-change beg prop nil end)))
              (while (and beg (< beg end))
                (setq zone-end  (or (icicle-next-single-char-property-change beg prop nil end) end))
                (let* ((hit-string  (buffer-substring-no-properties beg zone-end))
                       (end-marker  (copy-marker zone-end)))
                  (when (or (not predicate)
                            (save-match-data (funcall predicate hit-string end-marker)))
                    (icicle-candidate-short-help
                     (concat (and add-bufname-p
                                  (format "Buffer: `%s', " (buffer-name (marker-buffer end-marker))))
                             (format "Position: %d, Length: %d"
                                     (marker-position end-marker) (length hit-string)))
                     hit-string)
                    (push (cons (if add-bufname-p
                                    (list hit-string
                                          (let ((string  (copy-sequence (buffer-name))))
                                            (put-text-property 0 (length string)
                                                               'face 'icicle-candidate-part string)
                                            string))
                                  hit-string)
                                end-marker)
                          temp-list)
                    ;; Highlight search context in buffer.
                    (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                              icicle-search-highlight-threshold)
                      (let ((ov  (make-overlay beg zone-end)))
                        (push ov icicle-search-overlays)
                        (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                        (overlay-put ov 'face 'icicle-search-main-regexp-others)))))
                (setq beg  zone-end)
                (while (and (< beg end)
                            (let* ((charval  (and (or (not type) (eq type 'overlay))
                                                  (get-char-property beg prop)))
                                   (textval  (and (or (not type) (eq type 'text))
                                                  (get-text-property beg prop)))
                                   (currval  (icicle-flat-list charval textval)))
                              (not (icicle-set-intersection values currval))))
                  (setq beg  (icicle-next-single-char-property-change beg prop nil end))))
              (setq icicle-candidates-alist  (append icicle-candidates-alist (nreverse temp-list))))
          (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
          (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                 (error (error-message-string icicle-search-char-property-scan))))))))

(defun icicle-flat-list (val1 val2)
  "Return a flat list with all values in VAL1 and VAL2."
  (let ((result  nil))
    (unless (listp val1) (setq val1  (list val1)))
    (unless (listp val2) (setq val2  (list val2)))
    (while val1 (add-to-list 'result (pop val1)))
    (while val2 (add-to-list 'result (pop val2)))
    result))

(if (fboundp 'next-single-char-property-change)
    (defalias 'icicle-next-single-char-property-change 'next-single-char-property-change)
  (defun icicle-next-single-char-property-change (position prop &optional object limit)
    "Position of next change of PROP for text property or overlay change.
Scans characters forward from buffer position POSITION until property
PROP changes.  Returns the position of that change.

POSITION is a buffer position (integer or marker).

Optional third arg OBJECT is ignored.  It is present for compatibility
 with Emacs 22.

If optional fourth arg LIMIT is non-nil, search stops at position
LIMIT.  LIMIT is returned if nothing is found before LIMIT.

The property values are compared with `eq'.  If the property is
constant all the way to the end of the buffer, then the last valid
buffer position is returned."
    (save-excursion
      (goto-char position)
      (let ((propval  (get-char-property (point) prop))
            (end      (min limit (point-max))))
        (while (and (< (point) end) (eq (get-char-property (point) prop) propval))
          (goto-char (min (next-overlay-change (point))
                          (next-single-property-change (point) prop nil end)))))
      (point))))

;;;###autoload
(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit  t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays  (cdr icicle-search-overlays)))
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays  (cdr icicle-search-level-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays  ()))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays  (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload
(defun icicle-search-word (beg end word-regexp require-match ; Bound to `C-c $'.
                           &optional where &rest args)
  "Search for a whole word.
Word search is literal: regexp special characters are treated as
non-special.  In fact, they are also treated as if they were
word-constituent characters.  That is, your typed input is searched
for literally, but matches must begin and end on a word boundary.
This also means that you can include whitespace within the \"word\"
being sought.

At the prompt for a word, you can use completion against previous
Icicles search inputs to choose the word, or you can enter a new word.

Non-interactively, WORD-REGEXP should be a regexp that matches a word.
The other arguments are the same as for `icicle-search'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(icicle-search-read-word)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (icicle-search beg end word-regexp (not icicle-show-multi-completion-flag) where))

;;;###autoload
(defun icicle-search-bookmarks-together (scan-fn-or-regexp require-match &rest args)
  "Search bookmarked regions (together).
The arguments are the same as for `icicle-search', but without
arguments BEG, END, and WHERE.

This is the same as using a plain prefix arg, `C-u', with
`icicle-search'.

You first choose all of the bookmarked regions to search.  Then your
input is matched against a multi-completion composed of (a) the region
text that matches the regexp and (b) the region's buffer name.

An alternative is multi-command `icicle-search-bookmark', which
searches the bookmarked regions you choose one at a time."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((current-prefix-arg  '(4))) (icicle-search-where-arg))
         args))

;;;###autoload
(defun icicle-search-buffer (scan-fn-or-regexp require-match &rest args)
  "Search multiple buffers completely.
Same as using a non-negative numeric prefix arg, such as `C-9', with
`icicle-search'.  You are prompted for the buffers to search.  All of
each buffer is searched.  Any existing buffers can be chosen.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag  t))
           (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag  'partial-match-ok))
                                  (icicle-buffer-list))))
         args))

;;;###autoload
(defun icicle-search-file (scan-fn-or-regexp require-match &rest args)
  "Search multiple files completely.
Same as using a negative numeric prefix arg, such as `C--', with
`icicle-search'.  You are prompted for the files to search.  All of
each file is searched.  Any existing files in the current directory
can be chosen.  Arguments are the same as for `icicle-search', but
without arguments BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag  t)) (icicle-file-list))
         args))

;;;###autoload
(defun icicle-search-dired-marked (scan-fn-or-regexp require-match &rest args)
  "Search the marked files in Dired.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (eq major-mode 'dired-mode)
    (error "Command `icicle-search-dired-marked' must be called from a Dired buffer"))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match (dired-get-marked-files) args))

;;;###autoload
(defun icicle-search-ibuffer-marked (scan-fn-or-regexp require-match &rest args)
  "Search the marked buffers in Ibuffer, in order.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (eq major-mode 'ibuffer-mode)
    (error "Command `icicle-search-ibuffer-marked' must be called from an Ibuffer buffer"))
  (let ((marked-bufs (nreverse (ibuffer-get-marked-buffers))))
    (unless marked-bufs (setq marked-bufs  (list (ibuffer-current-buffer t))))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match marked-bufs args)))

;;;###autoload
(defun icicle-search-buff-menu-marked (scan-fn-or-regexp require-match &rest args)
  "Search the marked buffers in Buffer Menu, in order.
Arguments are the same as for `icicle-search', but without arguments
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-show-multi-completion-flag)))
  (unless (eq major-mode 'Buffer-menu-mode)
    (error "Command `icicle-search-buff-menu-marked' must be called from a Buffer Menu buffer"))
  (let ((marked-bufs  ()))
    (save-excursion
      (Buffer-menu-beginning)
      (while (re-search-forward "^>" nil t) (push (Buffer-menu-buffer t) marked-bufs)))
    (setq marked-bufs  (nreverse marked-bufs))
    (unless marked-bufs (setq marked-bufs  (list (Buffer-menu-buffer t))))
    (apply #'icicle-search nil nil scan-fn-or-regexp require-match marked-bufs args)))

(defalias 'icicle-search-lines 'icicle-occur)
;;;###autoload
(defun icicle-occur (beg end &optional buffers) ; Bound to `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of one or more buffers.  Use
`S-TAB' to show matching lines.  Use `C-RET' or `C-mouse-2' to go to
the line of the current candidate.  Use `C-down', `C-up', `C-next',
`C-prior', `C-end', or `C-home', to cycle among the matching lines.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end ".*" (not icicle-show-multi-completion-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-search-sentences (beg end &optional buffers)
  "`icicle-search' with sentences as contexts.
Type a regexp to match within each sentence of one or more buffers.
Use `S-TAB' to show matching sentences.  Use `C-RET' or `C-mouse-2' to
go to the line of the current candidate.  Use `C-down', `C-up',
`C-next', `C-prior', `C-end', or `C-home' to cycle among the matching
sentences.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end (concat "[A-Z][^.?!]+[.?!]")
                               (not icicle-show-multi-completion-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-search-paragraphs (beg end &optional buffers)
  "`icicle-search' with paragraphs as contexts.
Type a regexp to match within each paragraph of one or more buffers.
Use `S-TAB' to show matching paragraph.  Use `C-RET' or `C-mouse-2' to
go to the line of the current candidate.  Use `C-down', `C-up',
`C-next', `C-prior', `C-end', or `C-home' to cycle among the matching
paragraphs.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end "\\(.+\n\\)+"
                               (not icicle-show-multi-completion-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-search-pages (beg end &optional buffers)
  "`icicle-search' with pages as contexts.
Type a regexp to match within each page of one or more buffers.  Use
`S-TAB' to show matching page.  Use `C-RET' or `C-mouse-2' to go to
the line of the current candidate.  Use `C-down', `C-up', `C-next',
`C-prior', `C-end', or `C-home', to cycle among the matching pages.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground        'icicle-search-main-regexp-others))
        (bg (face-background        'icicle-search-main-regexp-others))
        (icicle-transform-function  (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end "\\([^\f]*[\f]\\|[^\f]+$\\)"
                               (not icicle-show-multi-completion-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-comint-search (beg end)   ; Bound to `C-x `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`C-down', `C-up', `C-next', `C-prior', `C-end', or `C-home' to cycle
among your previous inputs.

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note that previous commands are identified by looking through the
shell buffer for a shell prompt.  This is not foolproof.  If, for
instance you use command `ls', the output includes an auto-save file
such as #foo.el#, and `#' in the first column represents a shell
prompt, then #foo.el# will be misinterpreted as a previous command.

Also, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous shell commands that are not visible in the current buffer.
See also \\<comint-mode-map>\\[icicle-comint-command] for another way to reuse commands,
including those from previous sessions.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments, see the doc for command
`icicle-search'."
  (interactive (icicle-region-or-buffer-limits))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook           icicle-search-hook)
        (icicle-transform-function  'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-search-send-input)
    (unwind-protect
         (icicle-search beg end
                        (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
      (remove-hook 'icicle-search-hook 'icicle-comint-search-send-input)))
  (goto-char (point-max)))

(defun icicle-comint-search-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-search-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-search-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-search-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand         (icicle-minibuf-input))
         (input-start  (and (string-match comint-prompt-regexp cand) (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-search-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start  (and (string-match comint-prompt-regexp icicle-explore-final-choice)
                           (match-end 0))))
    (if input-start
        (substring icicle-explore-final-choice input-start)
      icicle-explore-final-choice)))

;;;###autoload
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
  "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as *shell* or *inferior-lisp*.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands." ; Doc string
  insert                                ; Action function
  "Choose a previous command: "         ; `completing-read' args
  (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
  (aref (cddr comint-input-ring) 0) nil
  ((icicle-transform-function  'icicle-remove-duplicates))) ; Bindings

(defun icicle-comint-hook-fn ()
  "Hook to set up Comint mode for Icicles."
  (set (make-local-variable 'icicle-search-command) 'icicle-comint-search))

;;;###autoload
(defun icicle-compilation-search (beg end) ; Bound to `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but show the matching compilation-buffer hit.
Use this in a compilation buffer, such as `*grep*', searching for a
regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to show
the target-buffer hit corresponding to the current completion
candidate.  Use `C-down', `C-up', `C-next', `C-prior', `C-end', or
`C-home' to cycle among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp.

In Emacs 22 and later, you can replace search-hit text, as in
`icicle-search'.  In earlier Emacs versions, you cannot replace text.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments, see the doc for command
`icicle-search'."
  (interactive (icicle-region-or-buffer-limits))
  (unless (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))
    (error "Current buffer must be a compilation buffer"))
  (save-excursion (goto-char (point-min))
                  (compilation-next-error 1)
                  (setq beg  (if beg (max beg (point)) (point))))
  (let ((icicle-transform-function    (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alt-action-fn
         (if (boundp 'compilation-highlight-overlay) ; Emacs 22 test.
             icicle-candidate-alt-action-fn
           #'(lambda (cand)
               (message "Cannot replace matching text in Emacs before version 22"))))
        (next-error-highlight
         ;; Highlight indefinitely.  `until-move' should be part of Emacs (patch sent), but it's not.
         (if (and (featurep 'compile+) (featurep 'simple+)) 'until-move 1000000))
        (icicle-search-in-context-fn  'icicle-compilation-search-in-context-fn)
        (fg (face-foreground          'icicle-search-main-regexp-others))
        (bg (face-background          'icicle-search-main-regexp-others)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" t))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

(defun icicle-compilation-search-in-context-fn (cand+mrker replace-string)
  "`icicle-search-in-context-fn' used for `icicle-compilation-search'.
If `crosshairs.el' is loaded, then the target position is highlighted."
  (if (not (fboundp 'compilation-next-error-function))
      (compile-goto-error)              ; Emacs 20, 21.
    (setq compilation-current-error  (point)) ; Emacs 22+.
    (compilation-next-error-function 0 nil))
  (save-excursion
    (save-restriction
      (let ((inhibit-field-text-motion  t)) ; Just to be sure, for `end-of-line'.
        (narrow-to-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
      (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))
  (when (fboundp 'crosshairs-highlight) (crosshairs-highlight 'line-only 'nomsg))
  (let ((icicle-candidate-nb  icicle-candidate-nb)) (icicle-complete-again-update)))

(defun icicle-compilation-hook-fn ()
  "Hook setting `icicle-search-command' for compilation modes.
Used on `compilation-mode-hook' and `compilation-minor-mode-hook'."
  (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search))

(defalias 'icicle-search-defs 'icicle-imenu)
;;;###autoload
(defun icicle-imenu (beg end require-match &optional where) ; Bound to `C-c ='.
  "Go to an Imenu entry using `icicle-search'.
Recommended: Use library `imenu+.el' also.
In Emacs-Lisp mode, `imenu+.el' classifies definitions using these
submenus:

 1. Keys         - keys in the global keymap
 2. Keys in Maps - keys in keymaps other than global keymap
 3. Functions    - functions, whether interactive or not
 4. Macros       - macros defined with `defmacro'
 5. User Options - user variables, from `defcustom'
 6. Variables    - other variables (non-options), from `defvar'
 7. Faces        - faces, from `defface'
 8. Other        - other definitions

Note: If you use this command with a prefix argument, then the Imenu
mode (and `imenu-generic-expression') of the current buffer determines
what kinds of definitions are found.  So, if you want to search for
definitions in a certain language, then invoke this command from a
buffer in that language.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless imenu-generic-expression (error "No Imenu for this buffer"))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn  slist)                ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc #'(lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar #'(lambda (menu) ; Name an unlabeled menu `Others'.
                                         (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     imenu-generic-expression)))
                  (submenu  (let ((icicle-show-Completions-initially-flag  t))
                              (completing-read "Choose: " menus nil t)))
                  (regexp   (cadr (assoc submenu menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No match"))
             (icicle-search beg end regexp require-match where)))
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

(defun icicle-imenu-command (beg end require-match &optional where)
  "Go to an Emacs command definition using `icicle-search'.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are not considered commands by `icicle-imenu-command'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn  slist)                ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc #'(lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar #'(lambda (menu) ; Name an unlabeled menu `Others'.
                                         (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     (if (boundp 'emacs-lisp-imenu-generic-expression)
                                         emacs-lisp-imenu-generic-expression
                                       lisp-imenu-generic-expression))))
                  (submenu  (or (assoc "Functions" menus) (assoc "Others" menus)
                                (error "No command definitions in buffer")))
                  (regexp   (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where 'icicle-imenu-command-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-non-interactive-function (beg end require-match &optional where)
  "Go to an Emacs non-interactive function definition with `icicle-search'.
This uses `commandp' to distinguish currently defined commands from
other functions.  This means that if the buffer has not yet been
evaluated, then all of its function definitions are considered
non-interactive by `icicle-imenu-non-interactive-function'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for
information about the arguments and the use of a prefix argument to
search multiple regions, buffers, or files, see the doc for command
`icicle-search'."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-show-multi-completion-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode"))
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist))
    (dolist (syn  slist)                ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc #'(lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus    (icicle-remove-if-not
                             #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                             (mapcar #'(lambda (menu) ; Name an unlabeled menu `Others'.
                                         (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                     (if (boundp 'emacs-lisp-imenu-generic-expression)
                                         emacs-lisp-imenu-generic-expression
                                       lisp-imenu-generic-expression))))
                  (submenu  (or (assoc "Functions" menus) (assoc "Others" menus)
                                (error "No command definitions in buffer")))
                  (regexp   (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function
                   (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where
                            'icicle-imenu-non-interactive-function-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-command-p (ignored-hit-string ignored-marker)
  "Return non-nil for a command definition.
Predicate for `icicle-search'.
Both arguments are ignored here."
  (let ((indx  (if (< emacs-major-version 21) 6 2)))
    (commandp (intern-soft
               (buffer-substring-no-properties (match-beginning indx) (match-end indx))))))

(defun icicle-imenu-non-interactive-function-p (ignored-hit-string ignored-marker)
  "Return non-nil for a non-interactive function definition.
Predicate for `icicle-search'.
Both arguments are ignored here."
  (let* ((indx  (if (< emacs-major-version 21) 6 2))
         (fn    (intern-soft
                 (buffer-substring-no-properties (match-beginning indx) (match-end indx)))))
    (and (fboundp fn) (not (commandp fn)))))

;;;###autoload
(defun icicle-tags-search (regexp &optional arg)
  "Search all source files listed in tags tables for matches for REGEXP.
You are prompted for the REGEXP to match.  Enter REGEXP with `RET'.
You do not need `M-,' - you see all matches as search hits to visit.

All tags in a tags file are used, including duplicate tags from the
same or different source files.

By default, all tags files are used, but if you provide a prefix
argument then only the current tag table is used.

If your TAGS file references source files that no longer exist, those
files are listed.  In that case, you might want to update your TAGS
file."
  (interactive
   (let ((completion-ignore-case  (if (and (boundp 'tags-case-fold-search)
                                           (memq tags-case-fold-search '(t nil)))
                                      tags-case-fold-search
                                    case-fold-search)))
     (require 'etags)
     (list (icicle-search-read-context-regexp "Search files with tags for regexp: ")
           current-prefix-arg)))
  (let ((files  ()))
    (save-excursion
      (let ((first-time  t)
            (morep       t))
        (while (and morep (visit-tags-table-buffer (not first-time)))
          (when arg (setq morep  nil))
          (setq first-time  nil)
          (let ((tail  (last files)))
            (if tail
                (setcdr tail (mapcar 'expand-file-name (tags-table-files)))
              (setq files  (mapcar 'expand-file-name (tags-table-files))))))))
    (let ((tail              files)     ; Remove names of non-existent or unreadable files.
          (unreadable-files  ()))
      (while tail
        (if (file-readable-p (car tail))
            (setq tail  (cdr tail))
          (push (car tail) unreadable-files)
          (setcar tail (cadr tail))
          (setcdr tail (cddr tail))))
      (when unreadable-files
        (with-output-to-temp-buffer "*Unreadable Files*"
          (princ "These missing or unreadable files were ignored:") (terpri) (terpri)
          (dolist (file  unreadable-files) (princ file) (terpri)))))
    (select-window (minibuffer-window))
    (select-frame-set-input-focus (selected-frame))
    (icicle-search nil nil regexp nil files)))

;;;###autoload
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
You are prompted for the string to save.  Typically, you store a
regexp or part of a regexp in the variable.

By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.

You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers  t)
         (icicle-must-pass-after-match-predicate  #'(lambda (s) (boundp (intern s))))
         (var
          (if askp
              (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "variable")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "variable"))))
                (intern (completing-read "Variable: " obarray nil nil nil
                                         (if (boundp 'variable-name-history)
                                             'variable-name-history
                                           'icicle-variable-name-history)
                                         (symbol-name 'icicle-input-string))))
            'icicle-input-string))
         (text                          (icicle-completing-read-history
                                         (format "Text to save in `%s': " var))))
    (set var text)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'any)))
  (defalias 'any 'icicle-anything))
(when (> emacs-major-version 21)
  (defun icicle-anything (type)
    "Act on an object of type TYPE.
You are prompted for the type, then for an object of that type.  The
type is either the declared `type' of an Anything source, or its
`name' if it has no `type'.

This command is available only if you use library `anything.el'.

This is an Icicles multi-command: You can act on multiple objects in
multiple ways during a single command invocation.  When you choose an
object using `RET' or `mouse-2', the default action is applied to it.
The default action is also applied to the current completion candidate
when you use `C-RET', `C-mouse-2', and so on.

You can apply a different action by using an alternative action key:
`C-S-RET', `C-S-mouse-2', and so on.  This lets you choose the action
to apply using completion. You can use `C-RET', `C-mouse-2', and so
on, to perform multiple actions.

This command is intended for use only in Icicle mode."
    (interactive
     (let ((icicle-show-Completions-initially-flag  t)
           (icicle-whole-candidate-as-text-prop-p   icicle-anything-transform-candidates-flag))
       (unless (require 'anything nil t) (error "You must load library `anything.el' first"))
       (list (intern (completing-read "What (type): " (icicle-remove-duplicates
                                                       (mapcar #'list (icicle-get-anything-types)))
                                      nil t)))))
    (icicle-object-action type)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'file)))
  (defun file ()
    "Act on a file.  You are prompted for the file and the action.
During file-name completion, you can delete the file named by the
current candidate, using `S-delete'.

This is just `icicle-object-action' with type `file'."
    (interactive) (icicle-object-action 'file)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'buffer)))
  (defun buffer ()
    "Act on a buffer.  You are prompted for the buffer and the action.
During buffer-name completion, you can kill the buffer named by the
current candidate, using `S-delete'.

This is just `icicle-object-action' with type `buffer'."
    (interactive) (icicle-object-action 'buffer)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'a)))
  (defalias 'a 'icicle-object-action))
(when (and icicle-define-alias-commands-flag (not (fboundp 'what-which-how)))
  (defalias 'what-which-how 'icicle-object-action))
;;;###autoload
(defun icicle-object-action (&optional type)
  "Act on an object of type TYPE (a symbol).
You are prompted for the type (\"What\"), then for an object of that
type (\"Which\"), then for the action function to apply to the
object (\"How\").  For Anything types (see below), you are not
prompted for the action function.

The \"type\" of an object is one of these:

a. A type defining an entry `icicle-predicate-types-alist'.
   These are type predicates, such as `bufferp', `keywordp', or `atom'.

b. The `type' of an Anything source, or its `name' if it has no
   `type'.  This is available only if you use library `anything.el'
   and option `icicle-use-anything-candidates-flag' is non-nil.

c. A type defining an entry in user option
   `icicle-type-actions-alist'.

In the case of Anything types (only), this is a multi-command:
* `C-RET', `C-mouse-2', and so on perform the default action.
* `C-S-RET', `C-S-mouse-2', and so on let you choose the action using
  completion.

Though this is not a multi-command for non-Anything types, for types
`buffer' and `file' you can use `S-delete' during completion to delete
the object (buffer or file) named by the current completion candidate.

Objects of types (b) and (c) are easily associated with names.  Their
names are the completion candidates.  So, for instance, if you choose
type `buffer', then you can act on a buffer by choosing its name.

Objects of predicate type (type a) are not necessarily named.  The
completion candidates for these objects are variables (symbols) whose
values are the objects acted upon.  So, for instance, if you choose
type `bufferp', then you can choose a variable whose value is a
buffer, in order to act on that buffer.  Whereas a buffer is always
named, an object of type `stringp' is not.  The value of variable
`emacs-version' is one such string that you can act on.

Anything types and Anything actions are highlighted when used as
candidates in *Completions*, using face `icicle-special-candidate'.

Be aware that the action function you choose must accommodate the
object you choose as its only an argument.  Also, completion of the
function candidate itself is not strict, so you can enter a lambda
form.

With a prefix argument, the result of applying the function to the
object is pretty-printed using `icicle-pp-eval-expression'.
Otherwise, the function is called for its effect only, and its value
is not displayed.

You can use a prefix argument similarly when you act on an individual
function (\"How\") candidate to apply it to the object, without ending
completion.  That is, `C-u C-RET', `C-u C-mouse-2', and so on, will
pretty-print the result of the individual action.

This command is intended for use only in Icicle mode."
  (interactive)
  (let* ((anything-loaded-p         (and (> emacs-major-version 21)
                                         icicle-use-anything-candidates-flag
                                         (require 'anything nil t)))
         (anything-types            (and (not type) anything-loaded-p (icicle-get-anything-types)))
         (typ
          (or type
              (let ((icicle-show-Completions-initially-flag  t))
                (intern
                 (completing-read "What (type): "
                                  (icicle-remove-duplicates (append (mapcar #'list anything-types)
                                                                    icicle-type-actions-alist
                                                                    icicle-predicate-types-alist))
                                  nil t)))))
         (predicate-type-p          (and (assoc (symbol-name typ) icicle-predicate-types-alist)
                                         (not (memq (symbol-name typ) anything-types))))
         (anything-candidates       (and anything-loaded-p (not predicate-type-p)
                                         (icicle-get-anything-candidates-of-type typ)))
         (anything-default-actions  (and anything-candidates
                                         (icicle-get-anything-default-actions-for-type typ)))
         (anything-actions          (and anything-candidates
                                         (icicle-get-anything-actions-for-type typ)))
         (icicle-saved-completion-candidate
          (cond (predicate-type-p (icicle-read-var-value-satisfying typ))
                (anything-candidates
                 (icicle-choose-anything-candidate typ anything-candidates
                                                   anything-default-actions anything-actions))
                ((member (symbol-name typ) (and anything-loaded-p (icicle-get-anything-types)))
                 (error "No candidates for type `%s'" (symbol-name typ)))
                (t (icicle-choose-candidate-of-type typ))))
         (icicle-candidate-action-fn    ; For "how".
          #'(lambda (fn) (icicle-apply-to-saved-candidate fn anything-candidates typ)))
         (icicle-candidate-alt-action-fn ; For "how".
          (and anything-candidates #'(lambda (fn) (icicle-apply-to-saved-candidate fn t typ)))))
    (funcall (icicle-alt-act-fn-for-type
              (if predicate-type-p
                  (or (cdr (assoc (symbol-name typ) icicle-predicate-types-alist)) (symbol-name typ))
                (symbol-name typ)))
             icicle-saved-completion-candidate)))
    
(when (> emacs-major-version 21)
  (defun icicle-get-anything-types ()
    "Return list of types defined in `anything-sources'.  See `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let ((types  ())
               type)
           (dolist (source  (anything-get-sources))
             (if (setq type  (assoc-default 'type source))
                 (push (symbol-name type) types)
               (when (setq type  (assoc-default 'name source)) (push type types))))
           (setq types
                 (mapcar #'(lambda (typ)
                             (setq typ  (copy-sequence typ))
                             (put-text-property 0 (length typ) 'face 'icicle-special-candidate typ)
                             typ)
                         (icicle-remove-duplicates types)))))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates-of-type (type)
    "Return list of Anything candidates for type TYPE.
Used only when `anything-sources' is non-nil - see `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let ((anything-candidate-cache  ())
               (candidates                nil))
           (dolist (source  (anything-get-sources))
             (let ((init-fn  (assoc-default 'init source))) (when init-fn (funcall init-fn)))
             (when (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
               (setq candidates  (icicle-get-anything-cached-candidates source))))
           (when (and (not (functionp candidates)) (consp candidates))
             (mapcar #'(lambda (cand) (if (consp cand) cand (list cand))) candidates))
           candidates))))

;; Similar to `anything-get-cached-candidates' in `anything.el', but ignores processes.
;; Free var here: `anything-candidate-cache'.
(when (> emacs-major-version 21)
  (defun icicle-get-anything-cached-candidates (source)
    "Return cached value of candidates for Anything SOURCE.
Cache the candidates if there is not yet a cached value."
    (let* ((source-name      (assoc-default 'name source))
           (candidate-cache  (assoc source-name anything-candidate-cache))
           candidates)
      (if candidate-cache
          (setq candidates  (cdr candidate-cache))
        (setq candidates  (icicle-get-anything-candidates source))
        (when (processp candidates) (setq candidates  ()))
        (setq candidate-cache  (cons source-name candidates))
        (push candidate-cache anything-candidate-cache))
      candidates)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates (source)
    "Return the list of candidates from Anything SOURCE."
    (let* ((candidate-source  (assoc-default 'candidates source))
           (candidates
            (cond ((functionp candidate-source)
                   `(lambda (string pred mode)
                     (let ((anything-pattern  icicle-current-input))
                       (setq string  anything-pattern)
                       (let ((all-cands  (funcall ,candidate-source)))
                         (setq all-cands
                               (icicle-remove-if-not
                                #'(lambda (cand)
                                    (string-match (if (eq 'prefix icicle-current-completion-mode)
                                                      (concat "^" (regexp-quote string))
                                                    string)
                                                  cand))
                                all-cands))
                         (cond ((eq mode t) all-cands)
                               ((eq mode nil)
                                (icicle-expanded-common-match icicle-current-input all-cands))
                               ((eq mode 'lambda) t))))))
                  ((listp candidate-source) candidate-source)
                  ((and (symbolp candidate-source) (boundp candidate-source))
                   (symbol-value candidate-source))
                  (t
                   (error
                    (concat "Source `candidates' value is not a function, variable or list: %s")
                    candidate-source)))))
      (if (or (not icicle-anything-transform-candidates-flag) (processp candidates))
          candidates
        (anything-transform-candidates candidates source)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of actions for type TYPE.
The display string for each action is highlighted using face
`icicle-special-candidate'."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions  faced-act)
      (dolist (source  (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (dolist (action  this-source-actions)
            (unless (member action all-sources-actions)
              (setq faced-act  (copy-sequence (car action))) ; Highlight Anything action.
              (put-text-property 0 (length faced-act) 'face 'icicle-special-candidate faced-act)
              (push (cons faced-act (cdr action)) all-sources-actions)))))
      (setq icicle-candidates-alist  (sort all-sources-actions
                                           #'(lambda (a1 a2)
                                               (funcall icicle-sort-comparer (car a1) (car a2))))))))
(when (> emacs-major-version 21)
  (defun icicle-choose-anything-candidate (type candidates default-actions actions)
    "Read an Anything object of type TYPE with completion, and return it.
During completion, you can act on selected completion candidates, in
turn, using the action keys (`C-RET', `C-mouse-2', `C-down', etc.).
CANDIDATES is the list of candidates of type TYPE.
DEFAULT-ACTIONS is the list of default actions for type TYPE.
ACTIONS is the list of all actions for type TYPE."
    (let* ((win                                         (selected-window))
           (icicle-sort-comparer                        nil)
           (icicle-transform-function                   nil)
           (icicle-Completions-display-min-input-chars  (icicle-get-anything-req-pat-chars type))
           (icicle-incremental-completion-delay         (icicle-get-anything-input-delay type))
           (icicle-whole-candidate-as-text-prop-p       icicle-anything-transform-candidates-flag)
           (icicle-candidates-alist
            (if (or (functionp candidates) icicle-whole-candidate-as-text-prop-p)
                candidates
              icicle-candidates-alist))
           (icicle-candidate-action-fn
            #'(lambda (obj)
                (when icicle-whole-candidate-as-text-prop-p
                  (setq obj  (icicle-anything-candidate-value obj)))
                (let ((enable-recursive-minibuffers  t))
                  (with-selected-window win
                    (if (null (cdr default-actions))
                        (funcall (cdar default-actions) obj)
                      (funcall (completing-read "How (action): " default-actions nil t) obj))))
                (select-window (minibuffer-window))
                (select-frame-set-input-focus (selected-frame))
                (icicle-raise-Completions-frame)))
           (icicle-candidate-alt-action-fn
            `(lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj  (icicle-anything-candidate-value obj)))
              (let ((icicle-show-Completions-initially-flag  t)
                    (icicle-saved-completion-candidate       obj)
                    (icicle-candidates-alist                 actions)
                    (enable-recursive-minibuffers            t))
                (with-selected-window win
                  (icicle-apply-to-saved-candidate
                   (let ((enable-recursive-minibuffers      t)
                         (icicle-last-completion-candidate  icicle-last-completion-candidate)
                         (icicle-candidate-alt-action-fn    nil)
                         (icicle-candidate-action-fn
                          `(lambda (actn) (with-selected-window win
                                            (let ((enable-recursive-minibuffers  t)
                                                  (icicle-candidates-alist       actions))
                                              (icicle-apply-to-saved-candidate actn t ,type))))))
                     (completing-read "How (action): " actions nil t))
                   t
                   ,type)))))
           (orig-action-fn  icicle-candidate-action-fn)
           (icicle-candidate-help-fn
            (if icicle-whole-candidate-as-text-prop-p
                #'(lambda (obj)
                    (let ((icicle-candidate-help-fn  nil))
                      (icicle-help-on-candidate-symbol
                       (intern (icicle-anything-candidate-value obj)))))
              icicle-candidate-help-fn))
           (icicle-candidate-action-fn
            (if icicle-whole-candidate-as-text-prop-p
                #'(lambda (obj)
                    (let ((icicle-last-input  (icicle-anything-candidate-value obj)))
                      (funcall orig-action-fn obj)))
              icicle-candidate-action-fn)))
      (if icicle-whole-candidate-as-text-prop-p
          (icicle-anything-candidate-value
           (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t))
        (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-req-pat-chars (type)
    "Return max `required-pattern' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-Completions-display-min-input-chars'."
    (let ((req-pat              icicle-Completions-display-min-input-chars)
          (req-pat-this-source  nil))
      (dolist (source  (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq req-pat-this-source  (assoc-default 'requires-pattern source)))
          (setq req-pat  (max req-pat req-pat-this-source))))
      req-pat)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-input-delay (type)
    "Return max `delay' value for sources of type TYPE.
The value returned is also always at least as big as
`icicle-incremental-completion-delay'."
    (let ((delay              icicle-incremental-completion-delay)
          (delay-this-source  nil))
      (dolist (source  (anything-get-sources))
        (when (and (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
                   (setq delay-this-source  (and (assoc 'delayed source) anything-idle-delay)))
          (setq delay  (max delay delay-this-source))))
      delay)))

(when (> emacs-major-version 21)
  (defun icicle-anything-candidate-value (candidate)
    "Return the real value associated with string CANDIDATE."
    (or (cdr-safe (funcall icicle-get-alist-candidate-function candidate)) candidate)))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-default-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of default actions for type TYPE."
    (setq icicle-candidates-alist  ())
    (let ((all-sources-actions  ())
          this-source-actions)
      (dolist (source  (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions  (assoc-default 'action source))
          (unless (memq (car this-source-actions) all-sources-actions)
            (push (car this-source-actions) all-sources-actions))))
      (setq icicle-candidates-alist
            (sort all-sources-actions   ; Must sort, or `icicle-candidate-nb' will be wrong.
                  #'(lambda (a1 a2) (funcall icicle-sort-comparer (car a1) (car a2))))))))

(defun icicle-choose-candidate-of-type (type)
  "Read an object of type TYPE (a symbol) with completion, and return it.
These options, when non-nil, control buffer candidate matching and
filtering:
 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'               - Extra buffers to display
 `icicle-buffer-match-regexp'         - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'      - Regexp buffers must not match
 `icicle-buffer-predicate'            - Predicate buffer must satisfy
 `icicle-buffer-sort'                 - Sort function for candidates"
  (let ((orig-window  (selected-window))) ; For alternative actions.
    (case type
      (buffer
       (let ((completion-ignore-case          (or (and (boundp 'read-buffer-completion-ignore-case)
                                                       read-buffer-completion-ignore-case)
                                                  completion-ignore-case))
             (icicle-must-match-regexp        icicle-buffer-match-regexp)
             (icicle-must-not-match-regexp    icicle-buffer-no-match-regexp)
             (icicle-must-pass-predicate      icicle-buffer-predicate)
             (icicle-extra-candidates         icicle-buffer-extras)
             (icicle-transform-function       'icicle-remove-dups-if-extras)
             (icicle-sort-comparer            (or icicle-buffer-sort icicle-sort-comparer))
             (icicle-sort-orders-alist
              (append (list '("by last access") ; Renamed from "turned OFF'.
                            '("*...* last" . icicle-buffer-sort-*...*-last)
                            '("by buffer size" . icicle-buffer-smaller-p)
                            '("by major mode name" . icicle-major-mode-name-less-p)
                            (and (fboundp 'icicle-mode-line-name-less-p)
                                 '("by mode-line mode name" . icicle-mode-line-name-less-p))
                            '("by file/process name" . icicle-buffer-file/process-name-less-p))
                      (delete '("turned OFF") icicle-sort-orders-alist)))
             (icicle-candidate-alt-action-fn
              (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
             (icicle-all-candidates-list-alt-action-fn
              (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
             (icicle-delete-candidate-object  'icicle-kill-a-buffer) ; `S-delete' kills buffer.
             (icicle-require-match-flag        icicle-buffer-require-match-flag)
             (icicle-ignore-space-prefix-flag  icicle-buffer-ignore-space-prefix-flag))
         (get-buffer-create
          (completing-read "Which (buffer): " (mapcar #'(lambda (buf) (list (buffer-name buf)))
                                                      (buffer-list))
                           nil
                           (and (fboundp 'confirm-nonexistent-file-or-buffer) ; Emacs 23.
                                (confirm-nonexistent-file-or-buffer))
                           nil 'buffer-name-history nil nil))))
      (color (icicle-read-color 1))     ; Use the color name (only).
      (command (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (commandp (intern s))))
                     (icicle-candidate-alt-action-fn
                      (or icicle-candidate-alt-action-fn
                          (icicle-alt-act-fn-for-type "command")))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn
                          (icicle-alt-act-fn-for-type "command"))))
                 (intern (completing-read "Which (command): " obarray))))
      (face (let ((icicle-candidate-alt-action-fn
                   (or icicle-candidate-alt-action-fn
                       (icicle-alt-act-fn-for-type "face")))
                  (icicle-all-candidates-list-alt-action-fn
                   (or icicle-all-candidates-list-alt-action-fn
                       (icicle-alt-act-fn-for-type "face"))))
              (intern (completing-read "Which (face): " (mapcar #'(lambda (x) (list (format "%s" x)))
                                                                (face-list))))))
      (file (let ((icicle-candidate-alt-action-fn
                   (or icicle-candidate-alt-action-fn
                       (icicle-alt-act-fn-for-type "file")))
                  (icicle-all-candidates-list-alt-action-fn
                   (or icicle-all-candidates-list-alt-action-fn
                       (icicle-alt-act-fn-for-type "file")))
                  (icicle-delete-candidate-object  'icicle-delete-file-or-directory)) ; `S-delete'
              (read-file-name "Which (file): " nil
                              (and (eq major-mode 'dired-mode)
                                   (fboundp 'dired-get-file-for-visit) ; Emacs 22+.
                                   (condition-case nil ; E.g. error because not on file line (ignore)
                                       (abbreviate-file-name (dired-get-file-for-visit))
                                     (error nil))))))
      (frame (let ((frame-alist  (icicle-make-frame-alist))
                   (icicle-candidate-alt-action-fn
                    (or icicle-candidate-alt-action-fn
                        (icicle-alt-act-fn-for-type "frame")))
                   (icicle-all-candidates-list-alt-action-fn
                    (or icicle-all-candidates-list-alt-action-fn
                        (icicle-alt-act-fn-for-type "frame"))))
               (cdr (assoc (completing-read "Which (frame): " frame-alist) frame-alist))))
      (function (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (fboundp (intern s))))
                      (icicle-candidate-alt-action-fn
                       (or icicle-candidate-alt-action-fn
                           (icicle-alt-act-fn-for-type "function")))
                      (icicle-all-candidates-list-alt-action-fn
                       (or icicle-all-candidates-list-alt-action-fn
                           (icicle-alt-act-fn-for-type "function"))))
                  (intern (completing-read "Which (function): " obarray))))
      (option (let ((icicle-must-pass-after-match-predicate  #'(lambda (s)
                                                                 (user-variable-p (intern s))))
                    (icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "option")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "option"))))
                (intern (completing-read "Which (user option): " obarray))))
      (process (let ((icicle-candidate-alt-action-fn
                      (or icicle-candidate-alt-action-fn
                          (icicle-alt-act-fn-for-type "process")))
                     (icicle-all-candidates-list-alt-action-fn
                      (or icicle-all-candidates-list-alt-action-fn
                          (icicle-alt-act-fn-for-type "process"))))
                 (get-process
                  (completing-read
                   "Which (process): " (mapcar #'(lambda (proc) (list (process-name proc)))
                                               (process-list))))))
      (symbol (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "symbol")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "symbol"))))
                (intern (completing-read "Which (symbol): " obarray))))
      (variable (let ((icicle-must-pass-after-match-predicate  #'(lambda (s) (boundp (intern s))))
                      (icicle-candidate-alt-action-fn
                       (or icicle-candidate-alt-action-fn
                           (icicle-alt-act-fn-for-type "variable")))
                      (icicle-all-candidates-list-alt-action-fn
                       (or icicle-all-candidates-list-alt-action-fn
                           (icicle-alt-act-fn-for-type "variable"))))
                  (intern (completing-read "Which (variable): " obarray))))
      (window (let ((icicle-candidate-alt-action-fn
                     (or icicle-candidate-alt-action-fn
                         (icicle-alt-act-fn-for-type "window")))
                    (icicle-all-candidates-list-alt-action-fn
                     (or icicle-all-candidates-list-alt-action-fn
                         (icicle-alt-act-fn-for-type "window")))
                    (buffers  ()))
                (walk-windows #'(lambda (win)
                                  (push (list (format "%s" (window-buffer win))) buffers))
                              nil t)
                (get-buffer-window (completing-read "Window showing buffer: " buffers) 0)))
      (otherwise (error "Bad object type: %S" type)))))

(defun icicle-read-var-value-satisfying (pred)
  "Read a variable that satisfies predicate PRED and returns its value."
  (symbol-value
   (let ((orig-window                             (selected-window))
         (icicle-must-pass-after-match-predicate  `(lambda (s)
                                                    (setq s  (intern s))
                                                    (and (boundp s)
                                                     (funcall ',pred (symbol-value s)))))
         (icicle-candidate-alt-action-fn          (or icicle-candidate-alt-action-fn
                                                      (icicle-alt-act-fn-for-type "variable")))
         (icicle-all-candidates-list-alt-action-fn
          (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "variable"))))
     (intern (completing-read (format "Which (%s value of variable): " pred) obarray)))))

;;;###autoload
(when (fboundp 'map-keymap)             ; Emacs 22.

  ;; This is a quick-and-dirty definition, not an efficient one.
  ;; It gathers all key bindings and then throws most of them away!  Good enough.
  (defun icicle-insert-char ()
    "Insert a character, using key completion.
Keys bound to `self-insert-command' are completion candidates."
    (interactive)
    (barf-if-buffer-read-only)
    (let ((icicle-complete-keys-self-insert-flag  t)
          (icicle-must-match-regexp               "^.+  =  self-insert-command"))
      (icicle-complete-keys)))

  (defun icicle-complete-keys ()        ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `icicle-complete-keys-self-insert-flag' is non-nil, then
keys bound to `self-insert-command' are included as possible
completion candidates; otherwise (the default), they are not.  Command
`icicle-insert-char' works like `icicle-complete-keys', but in
includes only keys bound to `self-insert-command' - use it to insert a
character that is difficult or impossible to type with your keyboard.

You can use `C-M-,' at any time to switch between sorting with local
bindings first and sorting with prefix keys first.  You can use `C-,'
at any time to change the sort order among these two and sorting by
command name.

While cycling, these keys describe candidates:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Move to next completion candidate and describe
`C-up'    - Move to previous completion candidate and describe
`C-next'  - Move to next apropos-completion candidate and describe
`C-prior' - Move to previous apropos-completion candidate and describe
`C-end'   - Move to next prefix-completion candidate and describe
`C-home'  - Move to previous prefix-completion candidate and describe
`C-!'     - Describe *all* candidates (or all that are saved),
            successively - use the [back] button in buffer *Help* to
            visit the descriptions

When candidate action and cycling are combined (e.g. `C-next'), option
`icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.  This is an Icicles command - see command
`icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function               'icicle-remove-duplicates)
           (orig-sort-orders-alist                  icicle-sort-orders-alist) ; For recursive use.
           (orig-show-initially-flag                icicle-show-Completions-initially-flag)
           (icicle-show-Completions-initially-flag  t)
           (icicle-candidate-action-fn              'icicle-complete-keys-action)
           (enable-recursive-minibuffers            t)
           ;; `orig-(buff|win)-key-complete' are used free in `icicle-complete-keys-action'.
           (orig-buff-key-complete                  (current-buffer))
           (orig-win-key-complete                   (selected-window))
           (icicle-completing-keys-p                t) ; Provide a condition to test key completion.
           (icicle-sort-comparer                    'icicle-local-keys-first-p)
           (icicle-alternative-sort-comparer        'icicle-prefix-keys-first-p)
           (icicle-sort-orders-alist
            '(("by key name, local bindings first" . icicle-local-keys-first-p)
              ("by key name, prefix keys first" . icicle-prefix-keys-first-p)
              ("by command name" . icicle-command-names-alphabetic-p)
              ("turned OFF")))
           (icicle-hist-cands-no-highlight          '("..")))
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key  (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  ;; Free vars here: `icicle-complete-keys-alist' is bound in `icicles-var.el'.
  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    (let ((orig-extra-cands  icicle-extra-candidates)) ; Free in `icicle-complete-keys-action'.
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((this-cmd-keys      ; For error report - e.g. mouse cmd.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (prefix-description
                     (icicle-key-description prefix (not icicle-key-descriptions-use-<>-flag)))
                    (prompt  (concat "Complete keys" (and (not (string= "" prefix-description))
                                                          (concat " " prefix-description))
                                     ": ")))
               (put-text-property 0 1 'icicle-fancy-candidates t prompt)
               (icicle-complete-keys-action
                (completing-read prompt icicle-complete-keys-alist nil t nil nil
                                 ;;$$ (if (equal [] prefix) nil "\\.\\.")
                                 ))))
        (mapc #'(lambda (cand) (put (car cand) 'icicle-special-candidate nil)) ; Reset the property.
              icicle-complete-keys-alist))))

  ;; Free vars here:
  ;; `orig-buff-key-complete' and `orig-win-key-complete' are bound in `icicle-complete-keys'.
  ;; `prefix', `orig-extra-cands', and `this-cmd-keys' are bound in `icicle-complete-keys-1'.
  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding    (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key            (car-safe key+binding))
           (binding        (cdr-safe key+binding))
           (cmd-name       nil)
           (action-window  (selected-window)))
      (unwind-protect
           (progn
             (set-buffer orig-buff-key-complete)
             (select-window orig-win-key-complete)
             (if (string= ".." candidate)
                 (setq cmd-name  "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate) (match-beginning 2))
                 (error "No match"))
               (setq cmd-name  (substring candidate (match-beginning 2) (match-end 2))))
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append prefix nil)))))
                   ((and key (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command  'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat prefix key)))
                   (t
                    (setq this-command             binding
                          last-command             binding
                          icicle-extra-candidates  orig-extra-cands) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-char  (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-char  (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event  1) ; So *Completions* mouse-click info is ignored.
                    (condition-case try-command ; Bind so vanilla context when invoke chosen cmd.
                        (let ((icicle-show-Completions-initially-flag  orig-show-initially-flag)
                              (icicle-candidate-action-fn              nil)
                              (icicle-completing-keys-p                nil)
                              (icicle-sort-orders-alist                orig-sort-orders-alist)
                              (icicle-sort-comparer                    'icicle-case-string-less-p)
                              (icicle-alternative-sort-comparer
                               'icicle-historical-alphabetic-p))
                          (call-interactively binding nil this-cmd-keys))
                      (error (error (error-message-string try-command)))))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map  nil))
      (setq icicle-complete-keys-alist  ())
      (dolist (map  (current-active-maps t))
        (setq prefix-map  (lookup-key map prefix))
        ;; NOTE: `icicle-add-key+cmd' uses `prefix' and `map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix)
        (push (list (intern (propertize ".." 'face 'icicle-multi-command-completion)))
              icicle-complete-keys-alist))
      icicle-complete-keys-alist))

  ;; Free vars here: `prefix' and `map' are bound in `icicle-keys+cmds-w-prefix'.
  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))
            (null (cdr-safe (cdr-safe binding))))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding)) (null (cdr-safe binding)))
       (setq binding  nil))             ; So `keymapp' test, below, fails.

      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition  (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil    ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding  (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding  nil))))

      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding  (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding  (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding) (consp (car binding))) (setq binding  (cdr binding)))))

    ;; Follow indirections to ultimate symbol naming a command.
    (while (and (symbolp binding) (fboundp binding) (keymapp (symbol-function binding)))
      (setq binding  (symbol-function binding)))

    ;; `prefix' and `map' are free here, bound in `icicle-keys+cmds-w-prefix'.
    (cond ((and (or (keymapp binding)
                    (and (commandp binding)
                         (equal binding (key-binding (vconcat prefix (vector event))))
                         (not (eq binding 'icicle-complete-keys))))
                (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                    (and icicle-complete-keys-self-insert-flag ; Insert normal char.
                         (char-valid-p event))))
           (let* ((key-desc   (propertize (single-key-description
                                           event
                                           (not icicle-key-descriptions-use-<>-flag))
                                          'face 'icicle-candidate-part))
                  (candidate  (intern (concat key-desc "  =  " (if (keymapp binding)
                                                                   "..."
                                                                 (prin1-to-string binding))))))
             ;; Skip keys bound to `undefined'.
             (unless (string= "undefined" (prin1-to-string binding))
               (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist))
             (when (eq map (current-local-map)) (put candidate 'icicle-special-candidate t))))
          ((and (integerp event) (generic-char-p event) ; Insert generic char.
                (eq 'self-insert-command binding))
           (ignore))))                  ; Placeholder for future use.

  ;; $$ No longer used.  Was used in `icicle-complete-keys-1'.
  (defun icicle-read-single-key-description (string need-vector &optional no-angles)
    "If STRING contains a space, then the vector containing the symbol named STRING.
Otherwise, call `icicle-read-kbd-macro'.
Other args are as for `icicle-read-kbd-macro'."
    (cond ((and no-angles (string-match " " string)) (vector (intern string)))
          ((string-match "^<\\([^>]* [^>]*\\)>" string)
           (vector (intern (substring string (match-beginning 1) (match-end 1)))))
          (t (icicle-read-kbd-macro string need-vector no-angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)                       ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus  (selected-frame))
          (cmd               (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (fboundp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window  (get-buffer-window "*Help*" 0))
             (help-frame   (and help-window (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))                      ; Let minibuffer contents show immmediately.

  (defun icicle-read-kbd-macro (start &optional end no-angles)
    "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always.

Optional argument NO-ANGLES non-nil means to expect key
descriptions not to use angle brackets (<...>).  For example:

 (icicle-read-kbd-macro \"<mode-line>\" t)   returns [mode-line]
 (icicle-read-kbd-macro  \"mode-line\"  t t) returns [mode-line]"
    (interactive "r")
    (if (stringp start)
        (icicle-edmacro-parse-keys start end no-angles)
      (setq last-kbd-macro
            (icicle-edmacro-parse-keys (buffer-substring start end) nil no-angles))))

  (defun icicle-edmacro-parse-keys (string &optional need-vector no-angles)
    "Same as `edmacro-parse-keys', but with added NO-ANGLES argument.
NO-ANGLES is the same as for `icicle-read-kbd-macro'."
    (let ((case-fold-search  nil)
          (pos               0)
          (res               []))
      (while (and (< pos (length string))
                  (string-match "[^ \t\n\f]+" string pos))
        (let ((word   (substring string (match-beginning 0) (match-end 0)))
              (key    nil)
              (times  1))
          (setq pos  (match-end 0))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times  (string-to-number (substring word 0 (match-end 1)))
                  word   (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key  (vconcat (if (eq (key-binding [?\M-x])
                                             'execute-extended-command)
                                         [?\M-x]
                                       (or (car (where-is-internal
                                                 'execute-extended-command))
                                           [?\M-x]))
                                     (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos  (string-match "$" string pos)))
                ((and (string-match (if no-angles
                                        "^\\(\\([ACHMsS]-\\)*\\)\\(..+\\)$"
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$")
                                    word)
                      (or (not no-angles)
                          (save-match-data (not (string-match "^\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word  (concat (substring word (match-beginning 1)
                                                       (match-end 1))
                                            (substring word (match-beginning 3)
                                                       (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key  (list (intern word))))
                (t
                 (let ((orig-word  word)
                       (prefix     0)
                       (bits       0))
                   (while (string-match "^[ACHMsS]-." word)
                     (incf bits (cdr (assq (aref word 0)
                                           '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                             (?H . ?\H-\^@) (?M . ?\M-\^@)
                                             (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (incf prefix 2)
                     (callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (incf bits ?\C-\^@)
                     (incf prefix)
                     (callf substring word 1))
                   (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                               ("LFD" . "\n") ("TAB" . "\t")
                                               ("ESC" . "\e") ("SPC" . " ")
                                               ("DEL" . "\177")))))
                     (when found (setq word  (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (loop for ch across word
                           for n = 0 then (+ (* n 8) ch -48)
                           finally do (setq word  (vector n))))
                   (cond ((= bits 0)
                          (setq key  word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key  (loop for x across word collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                         (t
                          (setq key  (list (+ bits (aref word 0)))))))))
          (when key
            (loop repeat times do (callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res  (edmacro-subseq res 2 -2)))
      (if (and (not need-vector)
               (loop for ch across res
                     always (and (char-valid-p ch)
                                 (let ((ch2  (logand ch (lognot ?\M-\^@))))
                                   (and (>= ch2 0) (<= ch2 127))))))
          (concat (loop for ch across res
                        collect (if (= (logand ch ?\M-\^@) 0)
                                    ch (+ ch 128))))
        res))))

;;;###autoload
(when (fboundp 'define-minor-mode)      ; Emacs 21+ ------------
  (eval '(define-minor-mode icicle-ido-like-mode
          "Ido-like mode for use with Icicles.
No, this mode does not pretend to give you exactly the Ido behavior.

Turning the mode ON sets these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
Turning the mode OFF sets those options to non-nil.

A positive prefix arg turns the mode on and also sets option
`icicle-max-candidates' to the prefix-arg numeric value.  By default,
that option is nil, meaning that there is no limit to the number of
completion candidates.

Since Ido shows only a few completion candidates, you might want to
customize that option or use a prefix arg with this mode to set it.
You can also use `C-x #' in the minibuffer to increment or decrement
the option at any time during completion.

Turning the mode off by toggling (no prefix arg) resets option
`icicle-max-candidates' to nil.  If you have customized that option to
a non-nil value and do not want to lose that preference, then use a
zero or negative prefix arg to turn the mode off.

See also these options, which control how much time you have to edit
input before automatic incremental completion and automatic acceptance
of a sole candidate kick in:

 `icicle-incremental-completion-delay'
 `icicle-top-level-when-sole-completion-delay'

When you use this mode, you might also want to use nil or t as the
value of option `icicle-default-value', in order to not insert the
default value in the minibuffer.  If you want to change that option
dynamically for the mode, use `icicle-ido-like-mode-hook'.  E.g.:

 (add-hook 'icicle-ido-like-mode-hook
           (lambda () (setq icicle-default-value
                       (if icicle-ido-like-mode t 'insert-end))))"
          nil nil nil :global t :group 'Icicles-Miscellaneous
          (setq
           icicle-show-Completions-initially-flag      icicle-ido-like-mode
           icicle-top-level-when-sole-completion-flag  icicle-ido-like-mode)
          (if icicle-ido-like-mode
              (when (and current-prefix-arg (not (eq 'toggle current-prefix-arg)))
                (setq icicle-max-candidates  (prefix-numeric-value current-prefix-arg)))
            (unless (and current-prefix-arg (not (eq 'toggle current-prefix-arg)))
              (setq icicle-max-candidates  nil))))))

;; See also `hexrgb-read-color' in `hexrgb.el'.
;;;###autoload
(defun icicle-read-color (&optional arg prompt)
  "Read a color name or hex RGB color value #RRRRGGGGBBBB.
A string value is returned.
Interactively, optional argument ARG is the prefix arg.
Optional argument PROMPT is the prompt to use (default \"Color: \").

In addition to standard color names and RGB (red, green, blue) hex
values, the following are also available as proxy color candidates,
provided `icicle-add-proxy-candidates-flag' is non-nil and library
`palette.el' or `eyedropper.el' is used.  In each case, the
corresponding color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the text cursor
* `*point background*'   - background under the text cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

In addition, the names of user options (variables) whose custom type
is `color' are also proxy candidates, but with `'' as a prefix and
suffix.  So, for example, option `icicle-region-background' appears as
proxy color candidate `'icicle-region-background''.

As always, you can toggle the use of proxy candidates using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-proxy-candidates]' in
the minibuffer.

With plain `C-u', use `hexrgb-read-color', which lets you complete a
color name or input any valid RGB hex value (without completion).

With no prefix arg, return a string with both the color name and the
RGB value, separated by `icicle-list-nth-parts-join-string'.

With a numeric prefix arg of 0 or 1, return the color name.  With any
other numeric prefix arg, return the RGB value.

In the plain `C-u' case, your input is checked to ensure that it
represents a valid color.

In all other cases:

- You can complete your input against the color name, the RGB value,
  or both.

- If you enter input without completing or cycling, the input is not
  checked: whatever is entered is returned as the string value.

From Emacs Lisp, ARG controls what is returned.  If ARG is nil,
`icicle-list-use-nth-parts' can also be used to control the behavior.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, input can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  You can toggle duplicate removal at any time
using `C-$'.

During completion, candidate help (e.g. `C-M-RET') shows you the RGB
and HSV (hue, saturation, value) color components.

This command is intended only for use in Icicle mode (but it can be
used with `C-u', with Icicle mode turned off)."
  (interactive "P")
  (unless (featurep 'hexrgb) (error "`icicle-read-color' requires library `hexrgb.el'"))
  (let (color)
    (if (consp arg)                     ; Plain `C-u': complete against color name only,
        (setq color  (hexrgb-read-color t)) ; and be able to input any valid RGB string.

      ;; Complete against name+RGB pairs, but user can enter invalid value without completing.
      (let ((icicle-list-use-nth-parts
             (or (and arg (if (< arg 2) '(1) '(2))) ; 1 or 2, by program or via `C-1' or `C-2'.
                 icicle-list-use-nth-parts ; Bound externally by program.
                 '(1 2)))               ; Both parts, by default.
            (mouse-pseudo-color-p  nil)

            icicle-candidate-help-fn           completion-ignore-case
            icicle-transform-function          icicle-sort-orders-alist
            icicle-list-nth-parts-join-string  icicle-list-join-string
            icicle-list-end-string             icicle-proxy-candidate-regexp
            named-colors                       icicle-proxy-candidates)
        ;; Copy the prompt string because `icicle-color-completion-setup' puts a text prop on it.
        (setq prompt  (copy-sequence (or prompt "Color: ")))
        (icicle-color-completion-setup)
        (setq icicle-proxy-candidates
              (append icicle-proxy-candidates
                      (mapcar           ; Convert multi-completions to strings.
                       #'(lambda (entry)
                           (concat (mapconcat #'identity (car entry) icicle-list-join-string)
                                   icicle-list-end-string))
                       '((("*mouse-2 foreground*")) (("*mouse-2 background*")))))
              color  (icicle-transform-multi-completion
                      (let ((orig-window  (selected-window))
                            (icicle-candidate-alt-action-fn
                             (or icicle-candidate-alt-action-fn
                                 (icicle-alt-act-fn-for-type "color")))
                            (icicle-all-candidates-list-alt-action-fn
                             (or icicle-all-candidates-list-alt-action-fn
                                 (icicle-alt-act-fn-for-type "color"))))
                        (completing-read prompt named-colors))))
        (when (fboundp 'eyedrop-foreground-at-point)
          (cond ((string-match "^\*mouse-2 foreground\*" color)
                 (setq color  (prog1 (eyedrop-foreground-at-mouse
                                      (read-event
                                       "Click `mouse-2' anywhere to choose foreground color"))
                                (read-event)) ; Discard mouse up event.
                       mouse-pseudo-color-p  t))
                ((string-match "^\*mouse-2 background\*" color)
                 (setq color  (prog1 (eyedrop-background-at-mouse
                                      (read-event
                                       "Click `mouse-2' anywhere to choose background color"))
                                (read-event)) ; Discard mouse up event.
                       mouse-pseudo-color-p  t))))
        (when mouse-pseudo-color-p
          (let ((icicle-list-nth-parts-join-string  ": ")
                (icicle-list-join-string            ": ")
                (icicle-list-end-string             "")
                (icicle-list-use-nth-parts
                 (or (and arg (if (< arg 2) '(1) '(2))) ; 1 or 2, by program or via `C-1' or `C-2'.
                     icicle-list-use-nth-parts ; Bound externally by program.
                     '(1 2))))          ; Both parts, by default.
            (setq color  (icicle-transform-multi-completion
                          (concat color ": " (hexrgb-color-name-to-hex color))))))))
    (when (interactive-p) (message "Color: `%s'" color))
    color))

(defun icicle-set-TAB-methods-for-command (command methods &optional arg msgp)
  "Set the possible TAB completion methods for COMMAND.
This works by advising COMMAND.
With a negative prefix arg, restore the original, unadvised behavior.
With a non-negative prefix arg, advise but do not enable the advice.
 You can later enable the advice and activate it, using
 `ad-enable-advice' and `ad-activate'.  The advice name is
 `icicle-TAB-completion-methods' (same name as option).
See also `icicle-set-S-TAB-methods-for-command'."
  (interactive (icicle-read-args-for-set-completion-methods 'icicle-TAB-completion-methods))
  (icicle-set-completion-methods-for-command
   command methods 'icicle-TAB-completion-methods arg msgp))

(defun icicle-set-S-TAB-methods-for-command (command methods &optional arg msgp)
  "Set the possible S-TAB completion methods for COMMAND.
This works by advising COMMAND.
With a negative prefix arg, restore the original, unadvised behavior.
With a non-negative prefix arg, advise but do not enable the advice.
 You can later enable the advice and activate it, using
 `ad-enable-advice' and `ad-activate'.  The advice name is
 `icicle-S-TAB-completion-methods-alist' (same name as option).
See also `icicle-set-TAB-methods-for-command'."
  (interactive (icicle-read-args-for-set-completion-methods 'icicle-S-TAB-completion-methods-alist))
  (icicle-set-completion-methods-for-command
   command methods 'icicle-S-TAB-completion-methods-alist arg msgp))

(defun icicle-read-args-for-set-completion-methods (var)
  "Read arguments for `icicle-set-(S-)TAB-methods-for-command'.
VAR is symbol `icicle-(S-)TAB-completion-methods(-alist)'."
  (let ((command  (intern (let ((icicle-must-pass-after-match-predicate
                                 #'(lambda (c) (commandp (intern c)))))
                            (completing-read "Command: " obarray nil t))))
        (methods  ()))
    (unless (< (prefix-numeric-value current-prefix-arg) 0)
      (let ((prompt  (if (eq var 'icicle-TAB-completion-methods) "TAB methods: " "S-TAB methods: "))
            (cmeths  (symbol-value var))
            meth)
        (setq cmeths  (if (consp (car cmeths))
                          cmeths
                        (mapcar (lambda (m) (list (symbol-name m))) cmeths)))
        (while (not (string= "" (setq meth  (completing-read prompt cmeths nil t))))
          (if (eq var 'icicle-TAB-completion-methods)
              (push (intern meth) methods)
            (push (assoc meth icicle-S-TAB-completion-methods-alist) methods)))
        (setq methods  (nreverse methods))))
    (list command methods current-prefix-arg (prefix-numeric-value current-prefix-arg))))
    
(defun icicle-set-completion-methods-for-command (command methods var arg msgp)
  "Set the possible completion methods for COMMAND.
This works by advising COMMAND.
VAR is the methods option to affect, `icicle-TAB-completion-methods'
 or `icicle-S-TAB-completion-methods-alist'.
Negative ARG means restore the original, unadvised behavior.
Non-negative ARG means advise but do not enable the advice.
Null ARG means advise and enable."
  (let ((type  (if (eq var 'icicle-TAB-completion-methods) "TAB methods" "S-TAB methods")))
    (cond ((< (prefix-numeric-value arg) 0)
           (condition-case nil
               (ad-remove-advice command 'around var)
             (error nil))
           (ad-activate command)        ; This restores original definition unless otherwise advised.
           (when msgp (message "`%s' now uses default %s" command type)))
          (t
           (ad-add-advice command `(,var
                                    nil ,(not arg)
                                    (advice . (lambda () (let ((,var  ',methods)) ad-do-it))))
                          'around 'first)
           (ad-activate command)
           (when msgp (message "`%s' %s: %s" command type
                               (if (consp (car methods)) (mapcar #'car methods) methods)))))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd2.el ends here

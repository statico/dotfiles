;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Sat Nov 20 15:30:27 2010 (-0800)
;;           By: dradams
;;     Update #: 6804
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mode.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `apropos-fn+var', `avoid', `cl',
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `custom',
;;   `dired', `dired+', `dired-aux', `dired-x', `doremi', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `el-swank-fuzzy',
;;   `ffap', `ffap-', `fit-frame', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `help+20', `hexrgb', `icicles-cmd1',
;;   `icicles-cmd2', `icicles-face', `icicles-fn', `icicles-mcmd',
;;   `icicles-opt', `icicles-var', `info', `info+', `kmacro',
;;   `levenshtein', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mkhtml', `mkhtml-htmlize', `mwheel', `pp', `pp+', `regexp-opt',
;;   `ring', `ring+', `second-sel', `strings', `thingatpt',
;;   `thingatpt+', `unaccent', `w32-browser', `w32browser-dlgopen',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Commands defined here:
;;
;;    `icicle-handle-switch-frame', `icicle-mode', `icy-mode',
;;    `icicle-skip-this-command', `old-bbdb-complete-name',
;;    `old-comint-dynamic-complete',
;;    `old-comint-replace-by-expanded-filename',
;;    `old-dired-read-shell-command', `old-ess-complete-object-name',
;;    `old-gud-gdb-complete-command', `old-read-shell-command',
;;    `orig-read-file-name'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-add-menu-item-to-cmd-history',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-bind-key-completion-keys-for-map-var',
;;    `icicle-bind-key-completion-keys-in-keymaps-from',
;;    `icicle-bind-other-keymap-keys',
;;    `icicle-cancel-Help-redirection', `icicle-define-cycling-keys',
;;    `icicle-define-icicle-maps', `icicle-define-minibuffer-maps',
;;    `icicle-minibuffer-setup', `icicle-rebind-global',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-restore-completion-keys',
;;    `icicle-restore-other-keymap-keys',
;;    `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
;;    `icicle-S-iso-lefttab-to-S-TAB', `icicle-top-level-prep',
;;    `icicle-unbind-isearch-keys',
;;    `icicle-unbind-key-completion-keys-for-map-var',
;;    `icicle-unbind-key-completion-keys-in-keymaps-from',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-bookmark-menu-map', `icicle-custom-menu-map',
;;    `icicle-describe-menu-map', `icicle-edit-menu-map',
;;    `icicle-file-menu-map', `icicle-frames-menu-map',
;;    `icicle-info-menu-map', `icicle-mode-map',
;;    `icicle-options-menu-map', `icicle-search-menu-map',
;;    `icicle-search-tags-menu-map'.
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
;;  (@> "User Options (alphabetical)")
;;  (@> "Internal variables (alphabetical)")
;;  (@> "Icicle mode command")
;;  (@> "Other Icicles functions that define Icicle mode")
 
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'icicles-opt)
  ;; icicle-buffer-configs, icicle-buffer-extras, icicle-change-region-background-flag,
  ;; icicle-default-cycling-mode, icicle-incremental-completion-flag,
  ;; icicle-default-value, icicle-kmacro-ring-max, icicle-minibuffer-setup-hook,
  ;; icicle-modal-cycle-down-keys, icicle-modal-cycle-up-keys,
  ;; icicle-redefine-standard-commands-flag, icicle-regexp-search-ring-max,
  ;; icicle-region-background, icicle-search-ring-max, icicle-show-Completions-initially-flag,
  ;; icicle-top-level-key-bindings, icicle-touche-pas-aux-menus-flag,
  ;; icicle-word-completion-keys, icicle-yank-function
(require 'icicles-fn) ;; assq-delete-all, icicle-completing-p,
                      ;; icicle-isearch-complete-past-string, icicle-toggle-icicle-mode-twice,
                      ;; icicle-unhighlight-lighter
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-cmd-calling-for-completion,
  ;; icicle-completing-p, icicle-completion-candidates,
  ;; icicle-current-completion-mode, icicle-default-directory, icicle-ignored-extensions,
  ;; icicle-ignored-extensions-regexp, icicle-incremental-completion-p, icicle-initial-value,
  ;; icicle-last-completion-candidate, icicle-last-completion-command, icicle-last-input,
  ;; icicle-menu-map, icicle-pre-minibuffer-buffer, icicle-minor-mode-map-entry,
  ;; icicle-saved-completion-candidates, icicle-saved-kmacro-ring-max,
  ;; icicle-saved-regexp-search-ring-max, icicle-saved-region-background,
  ;; icicle-saved-search-ring-max, icicle-search-current-overlay, icicle-search-overlays,
  ;; icicle-search-refined-overlays
(require 'icicles-cmd1)
  ;; icicle-add-buffer-candidate, icicle-add-buffer-config, icicle-bbdb-complete-name,
  ;; icicle-customize-face, icicle-customize-face-other-window, icicle-dabbrev-completion,
  ;; icicle-select-bookmarked-region
(require 'icicles-cmd2)
  ;; icicle-imenu, icicle-occur, icicle-search, icicle-search-bookmark,
  ;; icicle-search-bookmarks-together, icicle-search-buffer, icicle-search-file

;; Use `condition-case' because if `mb-depth.el' can't be found, `mb-depth+.el' is not provided.
(when (>= emacs-major-version 22) (condition-case nil (require 'mb-depth+ nil t) (error nil)))
  ;; (no error if not found): minibuffer-depth-indicate-mode

(eval-when-compile (require 'cl)) ;; push, dolist, case
                                  ;; plus, for Emacs < 20: when, unless
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-menu-bar-operate-menu, diredp-menu-bar-subdir-menu
(require 'dired) ;; dired-mode-map
(require 'menu-bar+ nil t) ;; (no error if not found):
  ;; menu-bar-apropos-menu, menu-bar-describe-menu, menu-bar-edit-menu,
  ;; menu-bar-file-menu, menu-bar-frames-menu, menu-bar-options-menu, menu-bar-search-tags-menu

;; `icicle-apropos-complete' is used here.  It is defined in `icicles-mcmd.el'.
;; `icicle-file-name-input-p' is used here.  It is defined in `icicles-fn.el'.

;;; Defvars to quiet byte-compiler:
(when (< emacs-major-version 22)
  (defvar kmacro-ring-max)
  (defvar minibuffer-local-filename-completion-map)
  (defvar minibuffer-local-must-match-filename-map)
  (defvar minibuffer-local-filename-must-match-map)
  (defvar mouse-wheel-down-event)
  (defvar mouse-wheel-up-event)
  (defvar read-file-name-function))

(defvar Buffer-menu-mode-map)           ; Defined in `buff-menu.el'.
(defvar comint-mode-map)                ; Defined in `comint.el'.
(defvar crm-local-completion-map)       ; Defined in `crm.el'.
(defvar crm-local-must-match-map)       ; Defined in `crm.el'.
(defvar dired-mode-map)                 ; Defined in `dired.el'.
(defvar gud-minibuffer-local-map)       ; Defined in `gud.el'.
(defvar ibuffer-mode-map)               ; Defined in `ibuffer.el'.
(defvar ibuffer-mode-operate-map)       ; Defined in `ibuffer.el'.
(defvar icicle-crm-local-completion-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-crm-local-must-match-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-kmacro-ring-max)         ; In `icicles-opt.el' for Emacs 22+.
(defvar icicle-saved-kmacro-ring-max)   ; In `icicles-var.el' for Emacs 22+.
(defvar ielm-map)                       ; Defined in `ielm.el'.
(defvar inferior-tcl-mode-map)          ; Defined in `tcl.el'.
(defvar Info-mode-map)                  ; Defined in `info.el'.
(defvar isearch-mode-map)               ; Defined in `isearch.el'.
(defvar old-crm-local-completion-map)   ; In `icicles-fn.el' after load `crm.el'.
(defvar old-crm-local-must-match-map)   ; In `icicles-fn.el' after load `crm.el'.
(defvar shell-mode-map)                 ; Defined in `shell.el'.
(defvar sh-mode-map)                    ; Defined in `sh-script.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options (alphabetical)")

;;; User Options (alphabetical) --------------------------------------

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Icicles-Miscellaneous :require 'icicles))

;;;###autoload
(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-define-minibuffer-maps' for minibuffer bindings and
bindings in *Completions*.")
 
;;(@* "Icicle mode command")

;;; Icicle mode command ----------------------------------------------

;; Main command.  Inspired from `icomplete-mode'.
(defalias 'icy-mode 'icicle-mode)
;;;###autoload
(when (fboundp 'define-minor-mode)      ; Emacs 21+ ------------
  (when (> emacs-major-version 22)
    (defadvice call-interactively (after icicle-save-to-history disable activate)
      "Save command to `icicle-interactive-history'."
      ;; If command's input is not a parameterized (e.g. mouse) event, record it.
      (let* ((fn   (ad-get-arg 0))
             (int  (interactive-form fn)))
        (when (and (symbolp fn) (consp int) (or (not (stringp (cadr int)))
                                                (string= (cadr int) "")
                                                (not (eq ?e (aref (cadr int) 0)))))
          (pushnew (symbol-name fn) icicle-interactive-history))))
    (when (boundp 'savehist-save-hook)  ; Do not save `icicle-interactive-history' (too large).
      (add-hook 'savehist-save-hook
                (lambda () (setq savehist-minibuffer-history-variables
                                 (delq 'icicle-interactive-history
                                       savehist-minibuffer-history-variables))))))
  (when (> emacs-major-version 21)
    (defadvice describe-face (before icicle-respect-WYSIWYG activate)
      "`read-face-name' respects `icicle-WYSIWYG-Completions-flag'.
If non-nil, then it does not use `completing-read-multiple' (which
cannot take advantage of WYSIWYG)."
      (interactive (list (read-face-name "Describe face" "= `default' face"
                                         (not icicle-WYSIWYG-Completions-flag))))))

  (eval '(define-minor-mode icicle-mode
          "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode.
In many cases there are also `other-window' versions.

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-entry-to-saved-completion-set' - Add completion to a set
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to a bookmark
`icicle-bookmark-bookmark-list'        - Jump to a bookmark list
`icicle-bookmark-desktop'              - Jump to a desktop bookmark
`icicle-bookmark-dired'                - Jump to a Dired bookmark
`icicle-bookmark-file'                 - Jump to a file bookmark
`icicle-bookmark-gnus'                 - Jump to a Gnus bookmark
`icicle-bookmark-info'                 - Jump to an Info bookmark
`icicle-bookmark-local-file'           - Jump to local-file bookmark
`icicle-bookmark-man'                  - Jump to a `man'-page bookmark
`icicle-bookmark-non-file'             - Jump to a buffer bookmark
`icicle-bookmark-region'               - Jump to a region bookmark
`icicle-bookmark-remote-file'          - Jump to a remote file
`icicle-bookmark-specific-buffers'     - Jump to a bookmarked buffer
`icicle-bookmark-specific-files'       - Jump to a bookmarked file
`icicle-bookmark-this-buffer'          - Jump to bookmark for this buf
`icicle-bookmark-url'                  - Jump to a URL bookmark
`icicle-bookmark-w3m'                  - Jump to a W3M (URL) bookmark
`icicle-buffer'                        - Switch to buffer
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-change-alternative-sort-order' - Choose an alternative sort
`icicle-change-sort-order'             - Choose a sort order
`icicle-clear-current-history'         - Clear current history entries
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-dynamic-complete'       - Text completion in shell
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-keys'                 - Complete keys
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completing-yank'               - `yank' using completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-delete-file'                   - Delete file/directory
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows'                - Delete all windows for buffer
`icicle-dired'                         - Multi-command Dired
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-doremi-candidate-width-factor+' - +/- candidate column width
`icicle-doremi-increment-max-candidates+' - +/- max number candidates
`icicle-doremi-increment-swank-prefix-length+' - +/- swank prefix
`icicle-doremi-increment-swank-timeout+' - +/- swank completion msec
`icicle-doremi-increment-variable+'    - Increment var using Do Re Mi
`icicle-doremi-inter-candidates-min-spaces+' - +/- candidate spacing
`icicle-doremi-zoom-Completions+'      - +/- *Completions* text size
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-face-list'                     - Choose a list of face names
`icicle-file-list'                     - Choose a list of file names
`icicle-file'                          - Visit file/directory
`icicle-find-file'                     -       same: relative only
`icicle-find-file-absolute'            -       same: absolute only
`icicle-find-file-in-tags-table'       - File in tags table
`icicle-find-first-tag'                - Visit definition with tag
`icicle-find-tag'                      - Visit definition with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu*'                        - Navigate among Imenu entries
`icicle-increment-option'              - Increment numeric option
`icicle-increment-variable'            - Increment numeric variable
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-Info-virtual-book'             - Open a virtual Info book
`icicle-insert-buffer'                 - Multi-command `insert-buffer'
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-keyword-list'                  - Choose a list of keywords
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'                   - Visit file(s) in a directory
`icicle-minibuffer-help'               - Show Icicles minibuffer help
`icicle-mode' or `icy-mode'            - Toggle Icicle mode
`icicle-next-S-TAB-completion-method'  - Next S-TAB completion method
`icicle-next-TAB-completion-method'    - Next TAB completion method
`icicle-occur'                         - Incremental `occur'
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'                   - Open recently used file(s)
`icicle-recompute-shell-command-candidates' - Update from search path
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-entry-from-saved-completion-set' - From a saved set
`icicle-remove-file-from-recentf-list' - Remove from recent files list
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-bookmark'               - Search bookmarks separately
`icicle-search-bookmark-list-bookmark' - Search bookmark lists
`icicle-search-bookmarks-together'     - Search bookmarks together
`icicle-search-char-property'          - Search for overlay/text props
`icicle-search-dired-bookmark'         - Search Dired bookmarks
`icicle-search-dired-marked'           - Search marked files in Dired
`icicle-search-file'                   - Search multiple files
`icicle-search-file-bookmark'          - Search bookmarked files
`icicle-search-gnus-bookmark'          - Search bookmarked Gnus msgs
`icicle-search-ibuffer-marked'         - Search marked bufs in Ibuffer
`icicle-search-info-bookmark'          - Search bookmarked Info nodes
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-local-file-bookmark'    - Search bookmarked local files
`icicle-search-man-bookmark'           - Search bookmarked `man' pages
`icicle-search-non-file-bookmark'      - Search bookmarked buffers
`icicle-search-overlay-property'       - Search for overlay properties
`icicle-search-pages'                  - Search Emacs pages
`icicle-search-paragraphs'             - Search Emacs paragraphs
`icicle-search-region-bookmark'        - Search bookmarked regions
`icicle-search-remote-file-bookmark'   - Search remote bookmarks
`icicle-search-sentences'              - Search sentences as contexts
`icicle-search-text-property'          - Search for faces etc.
`icicle-search-url-bookmark'           - Search bookmarked URLs
`icicle-search-word'                   - Whole-word search
`icicle-select-bookmarked-region'      - Select bookmarked regions
`icicle-select-frame'                  - Select a frame by name
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-alternative-sorting'    - Swap alternative sort
`icicle-toggle-angle-brackets'         - Toggle using angle brackets
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-dot'                    - Toggle `.' matching newlines
`icicle-toggle-expand-to-common-match' - Toggle input ECM expansion
`icicle-toggle-hiding-common-match'    - Toggle match in *Completions*
`icicle-toggle-highlight-all-current'  - Toggle max search highlight
`icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
`icicle-toggle-ignored-extensions'     - Toggle ignored files
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-replace-common-match' - Toggle ECM replacement
`icicle-toggle-search-replace-whole'   - Toggle replacing whole hit
`icicle-toggle-search-whole-word'      - Toggle whole-word searching
`icicle-toggle-show-multi-completion'  - Toggle multi-completions
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-maybe-completing'         - `yank' maybe using completion
`toggle' (alias)                       - Toggle binary user option

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-minibuffer-help]' \
when the minibuffer is active.

Note: Depending on your platform, if you use Icicles in a text
terminal (that is, without a window system/manager), then you might
need to change some of the key bindings, if some of the default
bindings are not available to you."
          :global t :group 'Icicles-Miscellaneous :lighter " Icy" :init-value nil
          (cond (icicle-mode
                 ;; (when (interactive-p)
                 ;;   (unless (or window-system (and (fboundp 'daemonp) (daemonp)))
                 ;;     (with-output-to-temp-buffer "*Attention*"
                 ;;       (princ "You are using Icicles in a text terminal (no window ")
                 ;;       (princ "system/manager).\n\nIcicles makes use of many keys that are ")
                 ;;      (princ "unavailable when running\nEmacs in a text terminal.  You will ")
                 ;;       (princ "want to rebind those keys.\n")
                 ;;       (princ "See the Icicles doc, section Key Bindings.\n"))
                 ;;  (message "Icicles uses keys that might not be suitable for a text terminal")
                 ;;     (sit-for 5)))
                 (icicle-define-icicle-maps)
                 (icicle-bind-other-keymap-keys)
                 (add-hook 'minibuffer-setup-hook       'icicle-minibuffer-setup)
                 (add-hook 'minibuffer-exit-hook        'icicle-cancel-Help-redirection)
                 (add-hook 'minibuffer-exit-hook        'icicle-restore-region-face)
                 (add-hook 'minibuffer-exit-hook        'icicle-unhighlight-lighter)
                 (add-hook 'icicle-post-command-hook    'icicle-activate-mark 'append)
                 (add-hook 'completion-setup-hook       'icicle-set-calling-cmd 'append)
                 (when icicle-customize-save-flag
                   (add-hook 'kill-emacs-hook           'icicle-command-abbrev-save))
                 (add-hook 'comint-mode-hook            'icicle-comint-hook-fn)
                 (add-hook 'compilation-mode-hook       'icicle-compilation-hook-fn)
                 (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                 (add-hook 'temp-buffer-show-hook       'icicle-fit-completions-window)
                 (icicle-undo-std-completion-faces)
                 (icicle-redefine-std-completion-fns)
                 (icicle-redefine-standard-commands)
                 (icicle-redefine-standard-options)
                 (when (ad-find-some-advice 'describe-face 'before 'icicle-respect-WYSIWYG)
                   (ad-enable-advice 'describe-face 'before 'icicle-respect-WYSIWYG))
                 (when (fboundp 'minibuffer-depth-indicate-mode) ; Defined in `mb-depth(+).el'
                   (minibuffer-depth-indicate-mode 99))
                 (if icicle-menu-items-to-history-flag
                     (add-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
                   (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history))
                 (when (> emacs-major-version 22)
                   (when icicle-populate-interactive-history-flag
                     (ad-enable-advice 'call-interactively 'after 'icicle-save-to-history))
                   (ad-activate 'call-interactively))
                 (dolist (fn  icicle-inhibit-advice-functions)
                   (when (and (fboundp fn) (ad-is-active fn))
                     (push (cons fn (ad-copy-advice-info fn)) icicle-advice-info-list)
                     (ad-deactivate fn))))
                (t
                 (makunbound 'icicle-mode-map)
                 (icicle-restore-other-keymap-keys)
                 (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                 (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                 (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                 (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                 ;; The pre- and post-command hooks are local to the minibuffer,
                 ;; So they are added in `icicle-minibuffer-setup', not here.
                 ;; Nevertheless, they are removed here when Icicle mode is exited.
                 (remove-hook 'pre-command-hook         'icicle-top-level-prep)
                 (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
                 (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
                 (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                 (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
                 (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                 (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                 (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                 (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
                 ;; $$ Should restore standard completion faces here.
                 (icicle-restore-std-completion-fns)
                 (icicle-restore-standard-commands)
                 (icicle-restore-standard-options)
                 (when (ad-find-some-advice 'describe-face 'before 'icicle-respect-WYSIWYG)
                   (ad-disable-advice 'describe-face 'before 'icicle-respect-WYSIWYG))
                 (when (fboundp 'minibuffer-depth-indicate-mode)
                   (minibuffer-depth-indicate-mode -99))
                 (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
                 (when (> emacs-major-version 22)
                   (ad-disable-advice 'call-interactively 'after 'icicle-save-to-history)
                   (ad-activate 'call-interactively))
                 (dolist (fn  icicle-inhibit-advice-functions)
                   (let ((info  (memq fn icicle-advice-info-list)))
                     (when (and (fboundp fn) info)
                       (ad-set-advice-info fn info)
                       (when (ad-is-active fn) (ad-activate fn)))))))
          (unless (eq icicle-guess-commands-in-path 'load)
            (setq icicle-shell-command-candidates-cache  ())) ; Reset - toggle Icy to update.
          (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
          (icicle-define-minibuffer-maps icicle-mode)
          (run-hooks 'icicle-mode-hook)
          (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF")))))

(unless (fboundp 'define-minor-mode)    ; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode.
In many cases there are also `other-window' versions.

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-entry-to-saved-completion-set' - Add completion to a set
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to a bookmark
`icicle-bookmark-bookmark-list'        - Jump to a bookmark list
`icicle-bookmark-desktop'              - Jump to a desktop bookmark
`icicle-bookmark-dired'                - Jump to a Dired bookmark
`icicle-bookmark-file'                 - Jump to a file bookmark
`icicle-bookmark-gnus'                 - Jump to a Gnus bookmark
`icicle-bookmark-info'                 - Jump to an Info bookmark
`icicle-bookmark-local-file'           - Jump to local-file bookmark
`icicle-bookmark-man'                  - Jump to a `man'-page bookmark
`icicle-bookmark-non-file'             - Jump to a buffer bookmark
`icicle-bookmark-region'               - Jump to a region bookmark
`icicle-bookmark-remote-file'          - Jump to a remote file
`icicle-bookmark-specific-buffers'     - Jump to a bookmarked buffer
`icicle-bookmark-specific-files'       - Jump to a bookmarked file
`icicle-bookmark-this-buffer'          - Jump to bookmark for this buf
`icicle-bookmark-url'                  - Jump to a URL bookmark
`icicle-bookmark-w3m'                  - Jump to a W3M (URL) bookmark
`icicle-buffer'                        - Switch to buffer
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-change-alternative-sort-order' - Choose an alternative sort
`icicle-change-sort-order'             - Choose a sort order
`icicle-clear-current-history'         - Clear current history entries
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-dynamic-complete'       - Text completion in shell
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completing-yank'               - `yank' using completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-delete-file'                   - Delete file/directory
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows'                - Delete all windows for buffer
`icicle-dired'                         - Multi-command Dired
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-doremi-candidate-width-factor+' - +/- candidate column width
`icicle-doremi-increment-max-candidates+' - +/- max number candidates
`icicle-doremi-increment-swank-prefix-length+' - +/- swank prefix
`icicle-doremi-increment-swank-timeout+' - +/- swank completion msec
`icicle-doremi-increment-variable+'    - Increment var using Do Re Mi
`icicle-doremi-inter-candidates-min-spaces+' - +/- candidate spacing
`icicle-doremi-zoom-Completions+'      - +/- *Completions* text size
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-face-list'                     - Choose a list of face names
`icicle-file-list'                     - Choose a list of file names
`icicle-file'                          - Visit file/directory
`icicle-find-file'                     -       same: relative only
`icicle-find-file-absolute'            -       same: absolute only
`icicle-find-file-in-tags-table'       - File in tags table
`icicle-find-first-tag'                - Visit definition with tag
`icicle-find-tag'                      - Visit definition with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu*'                        - Navigate among Imenu entries
`icicle-increment-option'              - Increment numeric option
`icicle-increment-variable'            - Increment numeric variable
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-insert-buffer'                 - Multi-command `insert-buffer'
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-keyword-list'                  - Choose a list of keywords
`icicle-kill-buffer'                   - Kill buffer
`icicle-locate-file'                   - Visit file(s) in a directory
`icicle-minibuffer-help'               - Show Icicles minibuffer help
`icicle-mode' or `icy-mode'            - Toggle Icicle mode
`icicle-next-S-TAB-completion-method'  - Next S-TAB completion method
`icicle-next-TAB-completion-method'    - Next TAB completion method
`icicle-occur'                         - Incremental `occur'
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'                   - Open recently used file(s)
`icicle-recompute-shell-command-candidates' - Update from search path
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-entry-from-saved-completion-set' - From a saved set
`icicle-remove-file-from-recentf-list' - Remove from recent files list
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-bookmark'               - Search bookmarks separately
`icicle-search-bookmark-list-bookmark' - Search bookmark lists
`icicle-search-bookmarks-together'     - Search bookmarks together
`icicle-search-char-property'          - Search for overlay/text props
`icicle-search-dired-bookmark'         - Search Dired bookmarks
`icicle-search-dired-marked'           - Search marked files in Dired
`icicle-search-file'                   - Search multiple files
`icicle-search-file-bookmark'          - Search bookmarked files
`icicle-search-gnus-bookmark'          - Search bookmarked Gnus msgs
`icicle-search-ibuffer-marked'         - Search marked bufs in Ibuffer
`icicle-search-info-bookmark'          - Search bookmarked Info nodes
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-local-file-bookmark'    - Search bookmarked local files
`icicle-search-man-bookmark'           - Search bookmarked `man' pages
`icicle-search-non-file-bookmark'      - Search bookmarked buffers
`icicle-search-overlay-property'       - Search for overlay properties
`icicle-search-pages'                  - Search Emacs pages
`icicle-search-paragraphs'             - Search Emacs paragraphs
`icicle-search-region-bookmark'        - Search bookmarked regions
`icicle-search-remote-file-bookmark'   - Search remote bookmarks
`icicle-search-sentences'              - Search sentences as contexts
`icicle-search-text-property'          - Search for faces etc.
`icicle-search-url-bookmark'           - Search bookmarked URLs
`icicle-search-word'                   - Whole-word search
`icicle-select-bookmarked-region'      - Select bookmarked regions
`icicle-select-frame'                  - Select a frame by name
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-alternative-sorting'    - Swap alternative sort
`icicle-toggle-angle-brackets'         - Toggle using angle brackets
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-dot'                    - Toggle `.' matching newlines
`icicle-toggle-expand-to-common-match' - Toggle input ECM expansion
`icicle-toggle-hiding-common-match'    - Toggle match in *Completions*
`icicle-toggle-highlight-all-current'  - Toggle max search highlight
`icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
`icicle-toggle-ignored-extensions'     - Toggle ignored files
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-replace-common-match' - Toggle ECM replacement
`icicle-toggle-search-replace-whole'   - Toggle replacing whole hit
`icicle-toggle-search-whole-word'      - Toggle whole-word searching
`icicle-toggle-show-multi-completion'  - Toggle multi-completions
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-maybe-completing'         - `yank' maybe using completion
`toggle' (alias)                       - Toggle binary user option

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-minibuffer-help]' \
when the minibuffer is active.

Note: Depending on your platform, if you use Icicles in a text
terminal (that is, without a window system/manager), then you might
need to change some of the key bindings, if some of the default
bindings are not available to you."
    (interactive "P")
    (setq icicle-mode  (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-define-minibuffer-maps icicle-mode)
    (cond (icicle-mode
           ;; (when (interactive-p)
           ;;   (unless (or window-system (and (fboundp 'daemonp) (daemonp)))
           ;;     (with-output-to-temp-buffer "*Attention*"
           ;;       (princ "You are using Icicles in a text terminal (no window ")
           ;;       (princ "system/manager).\n\nIcicles makes use of many keys that are ")
           ;;       (princ "unavailable when running\nEmacs in a text terminal.  You will ")
           ;;       (princ "want to rebind those keys.\n")
           ;;       (princ "See the Icicles doc, section Key Bindings.\n"))
           ;;     (message "Icicles uses keys that might not be suitable for a text terminal")
           ;;     (sit-for 5)))
           (icicle-define-icicle-maps)
           (icicle-bind-other-keymap-keys)
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook       'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook        'icicle-cancel-Help-redirection)
           (add-hook 'minibuffer-exit-hook        'icicle-restore-region-face)
           (add-hook 'minibuffer-exit-hook        'icicle-unhighlight-lighter)
           (add-hook 'icicle-post-command-hook    'icicle-activate-mark 'append)
           (add-hook 'completion-setup-hook       'icicle-set-calling-cmd 'append)
           (when icicle-customize-save-flag
             (add-hook 'kill-emacs-hook           'icicle-command-abbrev-save))
           (add-hook 'comint-mode-hook            'icicle-comint-hook-fn)
           (add-hook 'compilation-mode-hook       'icicle-compilation-hook-fn)
           (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (add-hook 'temp-buffer-show-hook       'icicle-fit-completions-window)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-commands)
           (icicle-redefine-standard-options)
           (if icicle-menu-items-to-history-flag
               (add-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
             (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history))
           (dolist (fn  icicle-inhibit-advice-functions)
             (when (and (fboundp fn) (ad-is-active fn))
               (push (cons fn (ad-copy-advice-info fn)) icicle-advice-info-list)
               (ad-deactivate fn)))
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (makunbound 'icicle-mode-map)
           (icicle-restore-other-keymap-keys)
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (remove-hook 'pre-command-hook         'icicle-top-level-prep)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
           (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-commands)
           (icicle-restore-standard-options)
           (unless (eq icicle-guess-commands-in-path 'load)
             (setq icicle-shell-command-candidates-cache  ())) ; Reset - toggle Icy to update.
           (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
           (dolist (fn  icicle-inhibit-advice-functions)
             (let ((info  (memq fn icicle-advice-info-list)))
               (when (and (fboundp fn) info)
                 (ad-set-advice-info fn info)
                 (when (ad-is-active fn) (ad-activate fn)))))
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF")))

    (add-to-list 'minor-mode-alist '(icicle-mode " Icy"))))

(defun icicle-add-menu-item-to-cmd-history ()
  "Add `this-command' to command history, if it is a menu item.
Menu items that are not associated with a command symbol are ignored.
Used on `pre-command-hook'."
  (condition-case nil                   ; Just in case, since this is on `pre-command-hook'.
      (when (and (> (length (this-command-keys-vector)) 0)
                 (equal '(menu-bar) (elt (this-command-keys-vector) 0))
                 ;; Exclude uninterned symbols such as `menu-function-356'.
                 (symbolp this-command) (or (< emacs-major-version 21) (intern-soft this-command)))
        (pushnew (symbol-name this-command) extended-command-history))
    (error nil)))

(defun icicle-top-level-prep ()
  "Do top-level stuff.  Used in `pre-command-hook'."
  ;; Reset `icicle-current-TAB-method' and `icicle-apropos-complete-match-fn' if temporary.
  ;; Save this top-level command as `icicle-last-top-level-command'
  ;; Reset `icicle-candidates-alist' to ().
  (when (= 0 (recursion-depth))
    (let ((TAB-method  (get 'icicle-last-top-level-command 'icicle-current-TAB-method))
          (apropos-fn  (get 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn)))
      (when TAB-method (setq icicle-current-TAB-method  TAB-method))
      (when apropos-fn (setq icicle-apropos-complete-match-fn apropos-fn)))
    (setq icicle-last-top-level-command   this-command
          icicle-candidates-alist         ())))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map  (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define Icicles menu-bar menu.  Create it only once.  Sacrifice latest bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map  (make-sparse-keymap "Icicles"))
    (define-key icicle-menu-map [icicle-mode] '(menu-item "Turn Off Icicle Mode" icicle-mode))
    (define-key icicle-menu-map [icicle-abort]
      '(menu-item "Cancel Minibuffer" icicle-abort-recursive-edit
        :enable (active-minibuffer-window)))
    (define-key icicle-menu-map [icicle-report-bug]
      '(menu-item "Send Bug Report" icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '(menu-item "Customize Icicles" icicle-customize-icicles-group))
    (define-key icicle-menu-map [icicle-help]
      '(menu-item "Help" icicle-minibuffer-help
        :help "Display help for minibuffer input and completion" :keys "C-?"))
    (define-key icicle-menu-map [icicle-separator-last] '("--"))

    (unless icicle-touche-pas-aux-menus-flag ; Use Dired > Multiple or Operate menu.
      (defvar icicle-dired-multiple-menu-map (make-sparse-keymap)
        "Icicles submenu for `Dired' > `Multiple' (or `Operate') menu.")
      (if (boundp 'diredp-menu-bar-operate-menu) ; Defined in `dired+.el'.
          (define-key diredp-menu-bar-operate-menu [icicles]
            (list 'menu-item "Icicles" icicle-dired-multiple-menu-map :visible 'icicle-mode))
        (define-key dired-mode-map [menu-bar operate icicles]
          (list 'menu-item "Icicles" icicle-dired-multiple-menu-map :visible 'icicle-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-search-dired-marked]
        '(menu-item "Search (and Replace)..." icicle-search-dired-marked
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-more]
        '(menu-item "Save as More Completion Candidates" icicle-dired-save-marked-more
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked]
        '(menu-item "Save as Completion Candidates" icicle-dired-save-marked
          :visible icicle-mode :enable (eq major-mode 'dired-mode)))
      (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-as-project]
        '(menu-item "Save as Project" icicle-dired-save-marked-as-project
          :visible icicle-mode :enable (eq major-mode 'dired-mode))))

    (unless icicle-touche-pas-aux-menus-flag ; Use Dired > Multiple or Operate menu.
      (defvar icicle-dired-dir-menu-map (make-sparse-keymap)
        "Icicles submenu for `Dired' > `Dir' (or `Subdir') menu.")
      (if (boundp 'diredp-menu-bar-subdir-menu) ; Defined in `dired+.el'.
          (define-key diredp-menu-bar-subdir-menu [icicles]
            (list 'menu-item "Icicles" icicle-dired-dir-menu-map :visible 'icicle-mode))
        (define-key dired-mode-map [menu-bar subdir icicles]
          (list 'menu-item "Icicles" icicle-dired-dir-menu-map :visible 'icicle-mode)))
      (define-key icicle-dired-dir-menu-map [icicle-dired-saved-file-candidates-other-window]
        '(menu-item "Open Dired for Chosen Files..."
          icicle-dired-saved-file-candidates-other-window
          :visible icicle-mode
          :enable (and icicle-saved-completion-candidates (eq major-mode 'dired-mode))))
      (define-key icicle-dired-dir-menu-map [icicle-dired-project-other-window]
        '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
          :visible icicle-mode
          :enable (and icicle-saved-completion-sets (eq major-mode 'dired-mode)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'Info-mode-menu)) ; Use Info menu, if available.
           (defvar icicle-info-menu-map (make-sparse-keymap)
             "Icicles submenu for `Info' menu.")
           (define-key Info-mode-menu [icicles]
             (list 'menu-item "Icicles" icicle-info-menu-map :visible 'icicle-mode))
           (when (fboundp 'icicle-Info-virtual-book)
             (define-key icicle-info-menu-map [icicle-Info-virtual-book]
               '(menu-item "Virtual Book" icicle-Info-virtual-book :visible icicle-mode
                 :enable (eq major-mode 'Info-mode))))
           (define-key icicle-info-menu-map [icicle-Info-goto-node]
             '(menu-item "+ Go to Node..." icicle-Info-goto-node :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "g"))
           (define-key icicle-info-menu-map [icicle-Info-menu]
             '(menu-item "+ Go to Menu Node..." icicle-Info-menu :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "m"))
           (define-key icicle-info-menu-map [icicle-Info-index]
             '(menu-item "+ Look Up in Index..." icicle-Info-index :visible icicle-mode
               :enable (eq major-mode 'Info-mode) :keys "i")))
          (t
           (when (fboundp 'icicle-Info-virtual-book)
             (define-key icicle-menu-map [icicle-Info-virtual-book]
               '(menu-item "Virtual Book" icicle-Info-virtual-book
                 :enable (eq major-mode 'Info-mode))))
           (define-key icicle-menu-map [icicle-Info-goto-node]
             '(menu-item "+ Go to Node..." icicle-Info-goto-node
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-Info-menu]
             '(menu-item "+ Go to Menu Node..." icicle-Info-menu
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-Info-index]
             '(menu-item "+ Look Up in Index..." icicle-Info-index
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-separator-Info]
             '(menu-item "--" icicle-separator-Info :visible icicle-mode
               :enable (eq major-mode 'Info-mode)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Defined in `menu-bar+.el'.
           (defvar icicle-frames-menu-map (make-sparse-keymap)
             "Icicles submenu for `Frames' menu.")
           (define-key menu-bar-frames-menu [icicles]
             (list 'menu-item "Icicles" icicle-frames-menu-map :visible 'icicle-mode))
           (define-key icicle-frames-menu-map [icicle-font]
             '(menu-item "+ Change Font" icicle-font :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-frames-menu-map [icicle-frame-fg]
             '(menu-item "+ Change Foreground..." icicle-frame-fg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-frames-menu-map [icicle-frame-bg]
             '(menu-item "+ Change Background..." icicle-frame-bg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-font]
             '(menu-item "+ Change Font of Frame..." icicle-font
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-fg]
             '(menu-item "+ Change Foreground of Frame..." icicle-frame-fg
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-bg]
             '(menu-item "+ Change Background of Frame..." icicle-frame-bg
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-frame] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-describe-menu)) ; Use Describe menu, if available.
           (defvar icicle-describe-menu-map (make-sparse-keymap)
             "Icicles submenu for `Describe' menu.")
           (define-key menu-bar-describe-menu [icicles]
             (list 'menu-item "Icicles" icicle-describe-menu-map :visible 'icicle-mode))
           (define-key icicle-describe-menu-map [icicle-plist]
             '(menu-item "+ Symbol with Property List..." icicle-plist :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-doc]
             '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-fundoc]
             '(menu-item "+ Function with Name, Doc..." icicle-fundoc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-vardoc]
             '(menu-item "+ Variable with Name, Doc..." icicle-vardoc :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-describe-option-of-type]
             '(menu-item "+ Option of Type..." icicle-describe-option-of-type
               :visible icicle-mode))
           (define-key icicle-describe-menu-map [icicle-where-is]
             '(menu-item "+ Where Is..." icicle-where-is :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-plist]
             '(menu-item "+ Symbol with Property List..." icicle-plist))
           (define-key icicle-menu-map [icicle-doc]
             '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc))
           (define-key icicle-menu-map [icicle-fundoc]
             '(menu-item "+ Describe Function with Name, Doc..." icicle-fundoc))
           (define-key icicle-menu-map [icicle-vardoc]
             '(menu-item "+ Describe Variable with Name, Doc..." icicle-vardoc))
           (define-key icicle-menu-map [icicle-describe-option-of-type]
             '(menu-item "+ Option of Type..." icicle-describe-option-of-type))
           (define-key icicle-menu-map [icicle-where-is]
             '(menu-item "+ Where Is..." icicle-where-is))
           (define-key icicle-menu-map [icicle-separator-doc] '("--"))))

    (define-key icicle-menu-map [icicle-apply]
      '(menu-item "+ Apply Function to Alist Items..." icicle-apply))
    (define-key icicle-menu-map [icicle-save-string-to-variable]
      '(menu-item "Save String to Variable..." icicle-save-string-to-variable))
    (define-key icicle-menu-map [icicle-color-theme]
      '(menu-item "+ Choose Color Theme..." icicle-color-theme
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-remove-saved-completion-set]
      '(menu-item "+ Remove Saved Candidate Set..." icicle-remove-saved-completion-set
        :enable icicle-saved-completion-sets))
    (define-key icicle-menu-map [icicle-add/update-saved-completion-set]
      '(menu-item "Add/Update Saved Candidate Set..." icicle-add/update-saved-completion-set))
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-menu-map [icicle-kmacro]
        '(menu-item "+ Execute Nth Keyboard Macro..." icicle-kmacro
          :enable (or (kmacro-ring-head) kmacro-ring))))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '(menu-item "+ Execute Named Keyboard Macro..." icicle-execute-named-keyboard-macro))
    (define-key icicle-menu-map [icicle-separator-misc] '("--"))
    (define-key icicle-menu-map [icicle-imenu]
      '(menu-item "+ Imenu..." icicle-imenu
        :enable imenu-generic-expression))
    (define-key icicle-menu-map [icicle-goto-global-marker]
      '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
        :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
    (define-key icicle-menu-map [icicle-goto-marker]
      '(menu-item "+ Go To Marker..." icicle-goto-marker
        :enable (mark t) :keys "C-- C-SPC"))
    (define-key icicle-menu-map [icicle-separator-goto] '("--"))
    (define-key icicle-menu-map [icicle-search-bookmarks-together]
      '(menu-item "+ Search Bookmarks Together..." icicle-search-bookmarks-together
        :enable (featurep 'bookmark+) :keys "C-u C-`"))
    (define-key icicle-menu-map [icicle-search-bookmark]
      '(menu-item "+ Search Bookmarks Separately..." icicle-search-bookmark
        :enable (featurep 'bookmark+)))
    (define-key icicle-menu-map [icicle-select-bookmarked-region]
      '(menu-item "+ Select Bookmarked Region..." icicle-select-bookmarked-region
        :enable (featurep 'bookmark+) :keys "C-u C-x C-x"))
    (define-key icicle-menu-map [icicle-separator-region] '("--"))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-apropos-menu)) ; Use Apropos menu, if available.
           (defvar icicle-apropos-menu-map (make-sparse-keymap)
             "Icicles submenu for `Apropos' menu.")
           (define-key menu-bar-apropos-menu [icicles]
             (list 'menu-item "Icicles" icicle-apropos-menu-map :visible 'icicle-mode))
           (define-key icicle-apropos-menu-map [icicle-apropos-zippy]
             '(menu-item "Zippy..." icicle-apropos-zippy :visible icicle-mode))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-apropos-menu-map [icicle-apropos]
                    '(menu-item "Symbols..." icicle-apropos :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-function]
                    '(menu-item "Functions..." icicle-apropos-function :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-variable]
                    '(menu-item "Variables..." icicle-apropos-variable :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-option]
                    '(menu-item "Options..." icicle-apropos-option :visible icicle-mode))
                  (define-key icicle-apropos-menu-map [icicle-apropos-command]
                    '(menu-item "Commands..." icicle-apropos-command :visible icicle-mode)))
                 (t
                  (define-key icicle-apropos-menu-map [icicle-apropos-variable]
                    '(menu-item "Variables..." icicle-apropos-variable
                      :visible icicle-mode))))
           (define-key icicle-apropos-menu-map [icicle-apropos-command]
             '(menu-item "Commands..." icicle-apropos-command :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-apropos-zippy]
             '(menu-item "Apropos Zippy..." icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-menu-map [icicle-apropos]
                    '(menu-item "Apropos..." icicle-apropos))
                  (define-key icicle-menu-map [icicle-apropos-function]
                    '(menu-item "Apropos Functions..." icicle-apropos-function))
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-option]
                    '(menu-item "Apropos Options..." icicle-apropos-option))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command)))
                 (t
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command))))
           (define-key icicle-menu-map [icicle-separator-apropos] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-options-menu)) ; Use Options menu, if available.
           (defvar icicle-options-menu-map (make-sparse-keymap)
             "Icicles submenu for `Options' menu.")
           (define-key menu-bar-options-menu [icicles]
             (list 'menu-item "Icicles" icicle-options-menu-map :visible 'icicle-mode))
           (define-key icicle-options-menu-map [icicle-set-option-to-t]
             '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-reset-option-to-nil]
             '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-toggle-option]
             '(menu-item "+ Toggle Any Option..." icicle-toggle-option :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-separator-options-general] '("--"))
           (define-key icicle-options-menu-map [icicle-toggle-search-cleanup]
             '(menu-item "Toggle Removal of Search Highlighting"
               icicle-toggle-search-cleanup :visible icicle-mode :keys "C-."))
           (define-key icicle-options-menu-map [icicle-toggle-search-replace-common-match]
             '(menu-item "Toggle Replacing Longest Common Match"
               icicle-toggle-search-replace-common-match :visible icicle-mode
               :enable icicle-searching-p :keys "M-;"))
           (define-key icicle-options-menu-map [icicle-toggle-search-replace-whole]
             '(menu-item "Toggle Replacing Whole Search Hit"
               icicle-toggle-search-replace-whole :visible icicle-mode
               :enable icicle-searching-p :keys "M-_"))
           (define-key icicle-options-menu-map [icicle-toggle-highlight-all-current]
             '(menu-item "Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current :visible icicle-mode
               :enable icicle-searching-p :keys "C-^"))
           (define-key icicle-options-menu-map [icicle-separator-options-search] '("--"))
           (define-key icicle-options-menu-map [icicle-regexp-quote-input]
             '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
               :visible icicle-mode :keys "C-M-;"))
           (define-key icicle-options-menu-map [icicle-toggle-regexp-quote]
             '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote
               :visible icicle-mode :keys "C-`"))
           (define-key icicle-options-menu-map [icicle-toggle-dot]
             '(menu-item "Toggle `.' Matching Newlines Too" icicle-toggle-dot
               :visible icicle-mode :keys "C-M-."))
           (define-key icicle-options-menu-map [icicle-toggle-incremental-completion]
             '(menu-item "Toggle Incremental Completion"
               icicle-toggle-incremental-completion :visible icicle-mode :keys "C-#"))
           (define-key icicle-options-menu-map [icicle-toggle-show-multi-completion]
             '(menu-item "Toggle Showing Multi-Completions"
               icicle-toggle-show-multi-completion :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-toggle-hiding-common-match]
             '(menu-item "Toggle Hiding Common Match"
               icicle-toggle-hiding-common-match :visible icicle-mode :keys "C-x ."))
           (define-key icicle-options-menu-map [icicle-toggle-expand-to-common-match]
             '(menu-item "Toggle Longest Common Match"
               icicle-toggle-expand-to-common-match :visible icicle-mode :keys "C-;"))
           (define-key icicle-options-menu-map [icicle-toggle-ignored-space-prefix]
             '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
               :visible icicle-mode :keys "M-_"))
           (define-key icicle-options-menu-map [icicle-toggle-ignored-extensions]
             '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
               :visible icicle-mode :keys "C-."))
           (define-key icicle-options-menu-map [icicle-toggle-remote-file-testing]
             '(menu-item "Toggle Remote File Handling" icicle-toggle-remote-file-testing
               :visible icicle-mode :enable (not icicle-searching-p) :keys "C-^"))
           (define-key icicle-options-menu-map [icicle-toggle-angle-brackets]
             '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-toggle-highlight-historical-candidates]
             '(menu-item "Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates :visible icicle-mode :keys "C-pause"))
           (define-key icicle-options-menu-map [icicle-toggle-case-sensitivity]
             '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity
               :visible icicle-mode :keys "C-A"))
           (define-key icicle-options-menu-map [icicle-toggle-proxy-candidates]
             '(menu-item "Toggle Including Proxy Candidates" icicle-toggle-proxy-candidates
               :visible icicle-mode :keys "C-M-_"))
           (define-key icicle-options-menu-map [icicle-toggle-transforming]
             '(menu-item "Toggle Duplicate Removal" icicle-toggle-transforming
               :visible icicle-mode :keys "C-$"))
           (define-key icicle-options-menu-map [icicle-toggle-C-for-actions]
             '(menu-item "Toggle Using `C-' for Actions" icicle-toggle-C-for-actions
               :visible icicle-mode :keys "M-g"))
           (define-key icicle-options-menu-map [icicle-toggle-~-for-home-dir]
             '(menu-item "Toggle Using `~' for $HOME" icicle-toggle-~-for-home-dir
               :visible icicle-mode :keys "M-~"))
           (define-key icicle-options-menu-map [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions
               :visible icicle-mode))
           (define-key icicle-options-menu-map [icicle-next-TAB-completion-method]
             '(menu-item "Next `TAB' Completion Method"
               icicle-next-TAB-completion-method :visible icicle-mode :keys "C-("))
           (define-key icicle-options-menu-map [icicle-next-S-TAB-completion-method]
             '(menu-item "Next `S-TAB' Completion Method" icicle-next-S-TAB-completion-method
               :visible icicle-mode :keys "M-("))
           (define-key icicle-options-menu-map [icicle-separator-options-sort] '("--"))
           (define-key icicle-options-menu-map [icicle-toggle-alternative-sorting]
             '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting
               :visible icicle-mode :keys "C-M-,"))
           (define-key icicle-options-menu-map [icicle-change-alternative-sort-order]
             '(menu-item "Change Alternative Sort Order"
               icicle-change-alternative-sort-order :visible icicle-mode :keys "M-,"))
           (define-key icicle-options-menu-map [icicle-change-sort-order]
             '(menu-item "Change Sort Order" icicle-change-sort-order :visible icicle-mode
               :enable (not icicle-inhibit-sort-p) :keys "C-,"))
           (when (fboundp 'doremi)
             (define-key icicle-options-menu-map [icicle-separator-options-doremi]
               '(menu-item "--" nil :visible (or (get-buffer-window "*Completions*" 'visible)
                                              (eq (icicle-current-TAB-method) 'swank)
                                              (active-minibuffer-window))))
             (when (fboundp 'text-scale-increase) ; Emacs 23+.
               (define-key icicle-options-menu-map [icicle-doremi-zoom-Completions+]
                 '(menu-item "*Completions* Zoom Factor - Do Re Mi"
                   icicle-doremi-zoom-Completions+
                   :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                   :keys "C-x -")))
             (define-key icicle-options-menu-map [icicle-doremi-inter-candidates-min-spaces+]
               '(menu-item "Inter-Candidate Spacing - Do Re Mi"
                 icicle-doremi-inter-candidates-min-spaces+
                 :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                 :keys "C-x |"))
             (define-key icicle-options-menu-map [icicle-doremi-candidate-width-factor+]
               '(menu-item "Candidate Column Width - Do Re Mi"
                 icicle-doremi-candidate-width-factor+
                 :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                 :keys "C-x w"))
             (define-key icicle-options-menu-map [icicle-doremi-increment-swank-prefix-length+]
               '(menu-item "Swank Min Match Chars - Do Re Mi"
                 icicle-doremi-increment-swank-prefix-length+
                 :visible (and icicle-mode (eq (icicle-current-TAB-method) 'swank)) :keys "C-x 2"))
             (define-key icicle-options-menu-map [icicle-doremi-increment-swank-timeout+]
               '(menu-item "Swank Timeout - Do Re Mi"
                 icicle-doremi-increment-swank-timeout+
                 :visible (and icicle-mode (eq (icicle-current-TAB-method) 'swank)) :keys "C-x 1"))
             (define-key icicle-options-menu-map [icicle-doremi-increment-max-candidates+]
               '(menu-item "Max # of Completions - Do Re Mi"
                 icicle-doremi-increment-max-candidates+
                 :visible (and icicle-mode (active-minibuffer-window)) :keys "C-x #"))))
          (t
           (define-key icicle-menu-map [icicle-set-option-to-t]
             '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t))
           (define-key icicle-menu-map [icicle-reset-option-to-nil]
             '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil))
           (define-key icicle-menu-map [icicle-toggle-option]
             '(menu-item "+ Toggle Any Option..." icicle-toggle-option))
           (define-key icicle-menu-map [icicle-toggle-C-for-actions]
             '(menu-item "Toggle Using `C-' for Actions" icicle-toggle-C-for-actions :keys "M-g"))
           (define-key icicle-menu-map [icicle-toggle-~-for-home-dir]
             '(menu-item "Toggle Using `~' for $HOME" icicle-toggle-~-for-home-dir :keys "M-~"))
           (define-key icicle-menu-map [icicle-next-TAB-completion-method]
             '(menu-item "Next `TAB' Completion Method" icicle-next-TAB-completion-method
               :keys "C-("))
           (define-key icicle-menu-map [icicle-next-S-TAB-completion-method]
             '(menu-item "Next `S-TAB' Completion Method" icicle-next-S-TAB-completion-method
               :keys "M-("))
           (define-key icicle-menu-map [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions))
           (define-key icicle-menu-map [icicle-toggle-search-cleanup]
             '(menu-item "Toggle Removal of Search Highlighting" icicle-toggle-search-cleanup
               :keys "C-."))
           (define-key icicle-menu-map [icicle-toggle-search-replace-common-match]
             '(menu-item "Toggle Replacing Longest Common Match"
               icicle-toggle-search-replace-common-match :enable icicle-searching-p :keys "M-;"))
           (define-key icicle-menu-map [icicle-toggle-search-replace-whole]
             '(menu-item "Toggle Replacing Whole Search Hit" icicle-toggle-search-replace-whole
               :enable icicle-searching-p :keys "M-_"))
           (define-key icicle-menu-map [icicle-toggle-highlight-all-current]
             '(menu-item "Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current :enable icicle-searching-p :keys "C-^"))
           (define-key icicle-menu-map [icicle-regexp-quote-input]
             '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input :keys "C-M-;"))
           (define-key icicle-menu-map [icicle-toggle-regexp-quote]
             '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote :keys "C-`"))
           (define-key icicle-menu-map [icicle-toggle-dot]
             '(menu-item "Toggle `.' Matching Newlines Too" icicle-toggle-dot :keys "C-M-."))
           (define-key icicle-menu-map [icicle-toggle-incremental-completion]
             '(menu-item "Toggle Incremental Completion" icicle-toggle-incremental-completion
               :keys "C-#"))
           (define-key icicle-menu-map [icicle-toggle-show-multi-completion]
             '(menu-item "Toggle Showing Multi-Completions" icicle-toggle-show-multi-completion))
           (define-key icicle-menu-map [icicle-toggle-hiding-common-match]
             '(menu-item "Toggle Hiding Common Match" icicle-toggle-hiding-common-match
               :keys "C-x ."))
           (define-key icicle-menu-map [icicle-toggle-expand-to-common-match]
             '(menu-item "Toggle Longest Common Match" icicle-toggle-expand-to-common-match
               :keys "C-;"))
           (define-key icicle-menu-map [icicle-toggle-ignored-space-prefix]
             '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
               :keys "M-_"))
           (define-key icicle-menu-map [icicle-toggle-ignored-extensions]
             '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
               :keys "C-."))
           (define-key icicle-menu-map [icicle-toggle-remote-file-testing]
             '(menu-item "Toggle Remote File Handling" icicle-toggle-remote-file-testing
               :enable (not icicle-searching-p) :keys "C-^"))
           (define-key icicle-menu-map [icicle-toggle-angle-brackets]
             '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets))
           (define-key icicle-menu-map [icicle-toggle-highlight-historical-candidates]
             '(menu-item "Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates :keys "C-pause"))
           (define-key icicle-menu-map [icicle-toggle-case-sensitivity]
             '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity :keys "C-A"))
           (define-key icicle-menu-map [icicle-toggle-proxy-candidates]
             '(menu-item "Toggle Including Proxy Candidates" icicle-toggle-proxy-candidates
               :keys "C-M-_"))
           (define-key icicle-menu-map [icicle-toggle-transforming]
             '(menu-item "Toggle Duplicate Removal" icicle-toggle-transforming :keys "C-$"))
           (define-key icicle-menu-map [icicle-toggle-alternative-sorting]
             '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting :keys "C-M-,"))
           (define-key icicle-menu-map [icicle-change-alternative-sort-order]
             '(menu-item "Change Alternative Sort Order" icicle-change-alternative-sort-order
               :keys "M-,"))
           (define-key icicle-menu-map [icicle-change-sort-order]
             '(menu-item "Change Sort Order" icicle-change-sort-order
               :enable (not icicle-inhibit-sort-p) :keys "C-,"))
           (when (fboundp 'doremi)
             (when (fboundp 'text-scale-increase) ; Emacs 23+.
               (define-key icicle-menu-map [icicle-doremi-zoom-Completions+]
                 '(menu-item "*Completions* Zoom Factor - Do Re Mi"
                   icicle-doremi-zoom-Completions+
                   :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                   :keys "C-x -")))
             (define-key icicle-menu-map [icicle-doremi-inter-candidates-min-spaces+]
               '(menu-item "Inter-Candidate Spacing - Do Re Mi"
                 icicle-doremi-inter-candidates-min-spaces+
                 :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                 :keys "C-x |"))
             (define-key icicle-menu-map [icicle-doremi-candidate-width-factor+]
               '(menu-item "Candidate Column Width - Do Re Mi"
                 icicle-doremi-candidate-width-factor+
                 :visible (and icicle-mode (get-buffer-window "*Completions*" 'visible))
                 :keys "C-x w"))
             (define-key icicle-menu-map [icicle-doremi-increment-swank-prefix-length+]
               '(menu-item "Swank Min Match Chars - Do Re Mi"
                 icicle-doremi-increment-swank-prefix-length+
                 :visible (and icicle-mode (eq (icicle-current-TAB-method) 'swank)) :keys "C-x 2"))
             (define-key icicle-menu-map [icicle-doremi-increment-swank-timeout+]
               '(menu-item "Swank Timeout - Do Re Mi"
                 icicle-doremi-increment-swank-timeout+
                 :visible (and icicle-mode (eq (icicle-current-TAB-method) 'swank)) :keys "C-x 1"))
             (define-key icicle-menu-map [icicle-doremi-increment-max-candidates+]
               '(menu-item "Max # of Completions - Do Re Mi"
                 icicle-doremi-increment-max-candidates+
                 :visible (and icicle-mode (active-minibuffer-window)) :keys "C-x #")))
           (define-key icicle-menu-map [icicle-separator-toggle] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-custom-menu)) ; Use Customize menu, if available.
           (defvar icicle-custom-menu-map (make-sparse-keymap)
             "Icicles submenu for `Customize' menu.")
           (define-key menu-bar-custom-menu [icicles]
             (list 'menu-item "Icicles" icicle-custom-menu-map :visible 'icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-groups]
             '(menu-item "Groups Matching Regexp..." icicle-customize-apropos-groups
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-faces]
             '(menu-item "Faces Matching Regexp..." icicle-customize-apropos-faces
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-face]
             '(menu-item "+ Face..." icicle-customize-face :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos-options]
             '(menu-item "Options Matching Regexp..." icicle-customize-apropos-options
               :visible icicle-mode))
           (define-key icicle-custom-menu-map [icicle-customize-apropos]
             '(menu-item "Settings Matching Regexp..." icicle-customize-apropos
               :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-separator-customize] '("--"))
           (define-key icicle-menu-map [icicle-customize-apropos-groups]
             '(menu-item "Groups Matching Regexp..." icicle-customize-apropos-groups))
           (define-key icicle-menu-map [icicle-customize-apropos-faces]
             '(menu-item "Faces Matching Regexp..." icicle-customize-apropos-faces))
           (define-key icicle-menu-map [icicle-customize-face]
             '(menu-item "+ Face..." icicle-customize-face))
           (define-key icicle-menu-map [icicle-customize-apropos-options]
             '(menu-item "Options Matching Regexp..." icicle-customize-apropos-options))
           (define-key icicle-menu-map [icicle-customize-apropos]
             '(menu-item "Settings Matching Regexp..." icicle-customize-apropos))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-edit-menu)) ; Use Edit menu, if available.
           (defvar icicle-edit-menu-map (make-sparse-keymap)
             "Icicles submenu for `Edit' menu.")
           (define-key menu-bar-edit-menu [icicles]
             (list 'menu-item "Icicles" icicle-edit-menu-map :visible 'icicle-mode))
           (define-key icicle-edit-menu-map [icicle-complete-thesaurus-entry]
             '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-edit-menu-map [icicle-insert-thesaurus-entry]
             '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-edit-menu-map [icicle-completing-yank]
             '(menu-item "+ Paste Copied Text..." icicle-completing-yank :visible icicle-mode
               :enable (not buffer-read-only) :keys "C-- C-y")))
          (t
           (define-key icicle-menu-map [icicle-separator-edit] '("--"))
           (define-key icicle-menu-map [icicle-complete-thesaurus-entry]
             '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-insert-thesaurus-entry]
             '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-completing-yank]
             '(menu-item "+ Paste Copied Text..." icicle-completing-yank
               :enable (not buffer-read-only) :keys "C-- C-y"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-file-menu)) ; Use File menu, if available.
           (defvar icicle-file-menu-map (make-sparse-keymap)
             "Icicles submenu for `File' menu.")
           (define-key menu-bar-file-menu [icicles]
             (list 'menu-item "Icicles" icicle-file-menu-map :visible 'icicle-mode))
           (define-key icicle-file-menu-map [icicle-kill-buffer]
             '(menu-item "+ Kill Buffer..." icicle-kill-buffer :visible icicle-mode :keys "C-x k"))
           (define-key icicle-file-menu-map [icicle-delete-file]
             '(menu-item "+ Delete File..." icicle-delete-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (featurep 'recentf)
             (define-key icicle-file-menu-map [icicle-remove-file-from-recentf-list]
               '(menu-item "+ Remove from Recent Files List..."
                 icicle-remove-file-from-recentf-list :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-file-menu-map [icicle-recent-file-other-window]
               '(menu-item "+ Open Recent File (Other Window)..."
                 icicle-recent-file-other-window :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-file-menu-map [icicle-recent-file]
               '(menu-item "+ Open Recent File..." icicle-recent-file :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "Open Dired for Chosen Files..."
               icicle-dired-saved-file-candidates-other-window :visible icicle-mode
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-dired-project-other-window]
             '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
               :visible icicle-mode
               :enable (and icicle-saved-completion-sets
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-file-menu-map [icicle-locate-file-other-window]
             '(menu-item "+ Open File Under Directory (Other Window)..."
               icicle-locate-file-other-window :visible icicle-mode))
           (define-key icicle-file-menu-map [icicle-locate-file]
             '(menu-item "+ Open File Under Directory..." icicle-locate-file
               :visible icicle-mode))
           (define-key icicle-file-menu-map [icicle-file-other-window]
             '(menu-item "+ Open File or Directory (Other Window)..."
               icicle-file-other-window :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-file-menu-map [icicle-file]
             '(menu-item "+ Open File or Directory..." icicle-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-kill-buffer]
             '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
           (define-key icicle-menu-map [icicle-delete-file]
             '(menu-item "+ Delete File..." icicle-delete-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (featurep 'recentf)
             (define-key icicle-menu-map [icicle-remove-file-from-recentf-list]
               '(menu-item "+ Remove from Recent Files List..."
                 icicle-remove-file-from-recentf-list
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-recent-file-other-window]
               '(menu-item "+ Open Recent File (Other Window)..." icicle-recent-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-recent-file]
               '(menu-item "+ Open Recent File..." icicle-recent-file
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "Open Dired for Chosen Files..."
               icicle-dired-saved-file-candidates-other-window
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-project-other-window]
             '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
               :visible icicle-mode
               :enable (and icicle-saved-completion-sets
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-locate-file-other-window]
             '(menu-item "+ Open File Under Directory (Other Window)..."
               icicle-locate-file-other-window))
           (define-key icicle-menu-map [icicle-locate-file]
             '(menu-item "+ Open File Under Directory..." icicle-locate-file))
           (define-key icicle-menu-map [icicle-file-other-window]
             '(menu-item "+ Open File or Directory (Other Window)..."
               icicle-file-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-file]
             '(menu-item "+ Open File or Directory ..." icicle-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))
    (define-key icicle-menu-map [icicle-buffer-list]
      '(menu-item "+ Buffer List..." icicle-buffer-list))
    (define-key icicle-menu-map [icicle-remove-buffer-config]
      '(menu-item "+ Remove Buffer Configuration..." icicle-remove-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-add-buffer-config]
      '(menu-item "New Buffer Configuration..." icicle-add-buffer-config))
    (define-key icicle-menu-map [icicle-buffer-config]
      '(menu-item "+ Choose Buffer Configuration..." icicle-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-remove-buffer-candidate]
      '(menu-item "+ Don't Always Include Buffer..." icicle-remove-buffer-candidate
        :enable icicle-buffer-extras))
    (define-key icicle-menu-map [icicle-add-buffer-candidate]
      '(menu-item "+ Always Include Buffer..." icicle-add-buffer-candidate))
    (define-key icicle-menu-map [icicle-kill-buffer]
      '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
    (define-key icicle-menu-map [icicle-insert-buffer]
      '(menu-item "+ Insert Buffer..." icicle-insert-buffer))
    (define-key icicle-menu-map [icicle-delete-windows]
      '(menu-item "+ Delete Windows on Buffer..." icicle-delete-windows :keys "C-u C-x 0"))
    (define-key icicle-menu-map [icicle-buffer-other-window]
      '(menu-item "+ Switch to Buffer (Other Window)..." icicle-buffer-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-buffer]
      '(menu-item "+ Switch to Buffer..." icicle-buffer
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
           (require 'bookmark)          ; `bookmark-buffer-name' is not autoloaded.
           (defvar icicle-bookmark-menu-map (make-sparse-keymap)
             "Icicles submenu for `Bookmarks' menu.")
           (define-key menu-bar-bookmark-map [icicles]
             (list 'menu-item "Icicles" icicle-bookmark-menu-map :visible 'icicle-mode))
           (define-key icicle-bookmark-menu-map [icicle-goto-global-marker]
             '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
               :visible icicle-mode
               :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
           (define-key icicle-bookmark-menu-map [icicle-goto-marker]
             '(menu-item "+ Go To Marker..." icicle-goto-marker
               :visible icicle-mode
               :enable (consp (icicle-markers mark-ring)) :keys "C-- C-SPC"))
           (define-key icicle-bookmark-menu-map [icicle-separator-goto] '("--"))
           (when (featurep 'bookmark+)
             (define-key icicle-bookmark-menu-map [icicle-bookmark-non-file-other-window]
               '(menu-item "+ Jump to Buffer (Non-File) Bookmark..."
                 icicle-bookmark-non-file-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-region-other-window]
               '(menu-item "+ Jump to Region Bookmark..." icicle-bookmark-region-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-url-other-window]
               '(menu-item "+ Jump to URL Bookmark..." icicle-bookmark-url-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-gnus-other-window]
               '(menu-item "+ Jump to Gnus Bookmark..." icicle-bookmark-gnus-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-man-other-window]
               '(menu-item "+ Jump to `man' Bookmark..." icicle-bookmark-man-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-info-other-window]
               '(menu-item "+ Jump to Info Bookmark..." icicle-bookmark-info-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-remote-file-other-window]
               '(menu-item "+ Jump to Remote-File Bookmark..."
                 icicle-bookmark-remote-file-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-local-file-other-window]
               '(menu-item "+ Jump to Local-File Bookmark..."
                 icicle-bookmark-local-file-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-file-other-window]
               '(menu-item "+ Jump to File Bookmark..." icicle-bookmark-file-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-dired-other-window]
               '(menu-item "+ Jump to Dired Bookmark..." icicle-bookmark-dired-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-bookmark-list-other-window]
               '(menu-item "+ Jump to Bookmark-List Bookmark..."
                 icicle-bookmark-bookmark-list-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-bookmark-menu-map [icicle-bookmark-desktop-other-window]
               '(menu-item "+ Jump to Desktop Bookmark..." icicle-bookmark-desktop-other-window
                 :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             )
           (define-key icicle-bookmark-menu-map [icicle-bookmark-other-window]
             '(menu-item "+ Jump to Bookmark..." icicle-bookmark-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-bookmark-menu-map [icicle-bookmark]
             '(menu-item "+ Jump to Bookmark (Same Window)..." icicle-bookmark :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (when (featurep 'bookmark+)
             (define-key icicle-menu-map [icicle-bookmark-non-file-other-window]
               '(menu-item "+ Jump to Buffer (Non-File) Bookmark..."
                 icicle-bookmark-non-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-region-other-window]
               '(menu-item "+ Jump to Region Bookmark..." icicle-bookmark-region-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-url-other-window]
               '(menu-item "+ Jump to URL Bookmark..." icicle-bookmark-url-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-gnus-other-window]
               '(menu-item "+ Jump to Gnus Bookmark..." icicle-bookmark-gnus-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-man-other-window]
               '(menu-item "+ Jump to `man' Bookmark..." icicle-bookmark-man-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-info-other-window]
               '(menu-item "+ Jump to Info Bookmark..." icicle-bookmark-info-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-remote-file-other-window]
               '(menu-item "+ Jump to Remote-File Bookmark..."
                 icicle-bookmark-remote-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-local-file-other-window]
               '(menu-item "+ Jump to Local-File Bookmark..."
                 icicle-bookmark-local-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-file-other-window]
               '(menu-item "+ Jump to File Bookmark..." icicle-bookmark-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-dired-other-window]
               '(menu-item "+ Jump to Dired Bookmark..." icicle-bookmark-dired-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-bookmark-list-other-window]
               '(menu-item "+ Jump to Bookmark-List Bookmark..."
                 icicle-bookmark-bookmark-list-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-bookmark-desktop-other-window]
               '(menu-item "+ Jump to Desktop Bookmark..." icicle-bookmark-desktop-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             )
           (define-key icicle-menu-map [icicle-bookmark-other-window]
             '(menu-item "+ Jump To Bookmark..." icicle-bookmark-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-bookmark]
             '(menu-item "+ Jump To Bookmark (Same Window)..." icicle-bookmark
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-bookmark-buffer] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-tags-menu)) ; Use Tags menu, if available - `menu-bar+.el'
           (defvar icicle-search-tags-menu-map (make-sparse-keymap)
             "Icicles submenu for `Tags' submenu of Search menu.")
           (define-key menu-bar-search-tags-menu [icicles]
             (list 'menu-item "Icicles" icicle-search-tags-menu-map :visible 'icicle-mode))
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-search-tags-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "+ Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-find-tag]
             '(menu-item "+ Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          ((and (not icicle-touche-pas-aux-menus-flag) ; Use Search, if available and no Tags menu.
                (boundp 'menu-bar-search-menu))
           (defvar icicle-search-menu-map (make-sparse-keymap)
             "Icicles submenu for `Search' menu.")
           (define-key menu-bar-search-menu [icicles]
             (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode))
           (defvar icicle-search-tags-menu-map (make-sparse-keymap)
             "Icicles submenu for `Tags' submenu of `Search' menu.")
           (define-key icicle-search-menu-map [icicles-tags]
             (list 'menu-item "Tags" icicle-search-tags-menu-map :visible 'icicle-mode))
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-search-tags-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "+ Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-find-tag]
             '(menu-item "+ Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-search-tags-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
             '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark :visible icicle-mode
               :enable (and (boundp 'find-tag-marker-ring)
                        (not (ring-empty-p find-tag-marker-ring))
                        (not (window-minibuffer-p
                              (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-find-first-tag-other-window]
             '(menu-item "Find First Tag ..." icicle-find-first-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-find-tag]
             '(menu-item "Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-menu)) ; Use Search menu, if available.
           (defvar icicle-search-menu-map (make-sparse-keymap)
             "Icicles submenu for `Search' menu.")
           (define-key menu-bar-search-menu [icicles]
             (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode))
           (define-key icicle-search-menu-map [icicle-goto-global-marker]
             '(menu-item "+ Go To Global Marker..." icicle-goto-global-marker
               :visible icicle-mode
               :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"))
           (define-key icicle-search-menu-map [icicle-goto-marker]
             '(menu-item "+ Go To Marker..." icicle-goto-marker
               :visible icicle-mode
               :enable (consp (icicle-markers mark-ring)) :keys "C-- C-SPC"))
           (define-key icicle-search-menu-map [icicle-separator-goto] '("--"))
           (define-key icicle-search-menu-map [icicle-search-highlight-cleanup]
             '(menu-item "Remove Search Highlighting..." icicle-search-highlight-cleanup
               :visible icicle-mode
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays) icicle-search-refined-overlays)))
           (define-key icicle-search-menu-map [icicle-compilation-search]
             '(menu-item "+ Search Compilation/Grep Hits (Regexp)..."
               icicle-compilation-search :visible icicle-mode
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))
               :keys "C-`"))
           (define-key icicle-search-menu-map [icicle-grep-saved-file-candidates]
             '(menu-item "Grep Saved File-Name Candidates..."
               icicle-grep-saved-file-candidates
               :visible icicle-mode :enable icicle-saved-completion-candidates))
           (define-key icicle-search-menu-map [icicle-imenu-non-interactive-function]
             '(menu-item "+ Search Non-Command Fn Definition (Regexp)..."
               icicle-imenu-non-interactive-function
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-search-menu-map [icicle-imenu-command]
             '(menu-item "+ Search Command Definition (Regexp)..." icicle-imenu-command
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-search-menu-map [icicle-imenu]
             '(menu-item "+ Search Definition (Regexp)..." icicle-imenu
               :visible icicle-mode :enable imenu-generic-expression))
           (define-key icicle-search-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-bookmarks-together]
             '(menu-item "+ Search Bookmarks Together..." icicle-search-bookmarks-together
               :visible (and icicle-mode (featurep 'bookmark+)) :keys "C-u C-`"))
           (define-key icicle-search-menu-map [icicle-search-bookmark]
             '(menu-item "+ Search Bookmarks Separately..." icicle-search-bookmark
               :visible (and icicle-mode (featurep 'bookmark+))))
           (define-key icicle-search-menu-map [icicle-search-file]
             '(menu-item "+ Search Files (Regexp)..." icicle-search-file
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-buffer]
             '(menu-item "+ Search Buffers (Regexp)..." icicle-search-buffer
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-text-property]
             '(menu-item "+ Search Text Property..." icicle-search-text-property
               :visible icicle-mode))
           (define-key icicle-search-menu-map [icicle-search-word]
             '(menu-item "+ Search for Word..." icicle-search-word :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-menu-map [icicle-search-keywords]
             '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-search-menu-map [icicle-search]
             '(menu-item "+ Search (Regexp)..." icicle-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
               :keys "C-`"))
           (define-key icicle-search-menu-map [icicle-occur]
             '(menu-item "+ Occur (Regexp)..." icicle-occur :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-search-highlight-cleanup]
             '(menu-item "Remove Search Highlighting..." icicle-search-highlight-cleanup
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays)
                        icicle-search-refined-overlays)))
           (define-key icicle-menu-map [icicle-compilation-search]
             '(menu-item "+ Search Compilation/Grep Hits (Regexp)..." icicle-compilation-search
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))))
           (define-key icicle-menu-map [icicle-grep-saved-file-candidates]
             '(menu-item "Grep Saved File-Name Candidates..."
               icicle-grep-saved-file-candidates :enable icicle-saved-completion-candidates))
           (define-key icicle-menu-map [icicle-imenu-non-interactive-function]
             '(menu-item "Search Non-Command Fn Definition (Regexp)..."
               icicle-imenu-non-interactive-function :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu-command]
             '(menu-item "Search Command Definition (Regexp)..." icicle-imenu-command
               :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu]
             '(menu-item "+ Search Definition (Regexp)..." icicle-imenu
               :enable imenu-generic-expression))
           (define-key icicle-menu-map [icicle-tags-search]
             '(menu-item "+ Search Tagged Files ..." icicle-tags-search))
           (define-key icicle-menu-map [icicle-search-bookmarks-together]
             '(menu-item "+ Search Bookmarks Together..." icicle-search-bookmarks-together
               :visible (featurep 'bookmark+) :keys "C-u C-`"))
           (define-key icicle-menu-map [icicle-search-bookmark]
             '(menu-item "+ Search Bookmarks Separately..." icicle-search-bookmark
               :visible (featurep 'bookmark+)))
           (define-key icicle-menu-map [icicle-search-file]
             '(menu-item "+ Search Files (Regexp)..." icicle-search-file))
           (define-key icicle-menu-map [icicle-search-buffer]
             '(menu-item "+ Search Buffers (Regexp)..." icicle-search-buffer))
           (define-key icicle-menu-map [icicle-search-text-property]
             '(menu-item "+ Search Text Property..." icicle-search-text-property))
           (define-key icicle-menu-map [icicle-search-word]
             '(menu-item "+ Search for Word..." icicle-search-word
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search-keywords]
             '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search]
             '(menu-item "+ Search (Regexp)..." icicle-search
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-occur]
             '(menu-item "+ Occur (Regexp)..." icicle-occur
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))))

  ;; Install Icicles menu-bar menu.
  (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" icicle-menu-map))

  ;; Optional `icicle-mode-map' bindings - governed by `icicle-top-level-key-bindings'.
  (icicle-bind-top-level-commands)

  ;; Install or update `icicle-mode-map'.
  (if icicle-minor-mode-map-entry
      (setcdr icicle-minor-mode-map-entry icicle-mode-map)
    (setq icicle-minor-mode-map-entry  (cons 'icicle-mode icicle-mode-map))
    (add-to-list 'minor-mode-map-alist icicle-minor-mode-map-entry)))

(defun icicle-S-iso-lefttab-to-S-TAB (strg)
  "Return string STRG, but with \"S-iso-lefttab\" replaced by \"S-TAB\"."
  (replace-regexp-in-string "S-iso-lefttab" "S-TAB" strg))

(defun icicle-bind-other-keymap-keys ()
  "Bind some keys in maps other than minibuffer maps and `icicle-mode-map'"

  ;; Bind Isearch keys.
  (icicle-bind-isearch-keys)

  ;; Bind keys in Comint mode.
  (when (boundp 'comint-mode-map)
    (define-key comint-mode-map "\C-c\C-i"         'icicle-comint-command)
    (define-key comint-mode-map [(control ?c) tab] 'icicle-comint-command))

  ;; Bind keys in Shell mode.
  (when (and (boundp 'shell-mode-map) icicle-redefine-standard-commands-flag)
    (define-key shell-mode-map "\t" 'icicle-comint-dynamic-complete))

  ;; Bind keys in Shell Script mode.
  (when (and (boundp 'sh-mode-map) icicle-redefine-standard-commands-flag)
    (icicle-remap 'comint-dynamic-complete 'icicle-comint-dynamic-complete sh-mode-map))

  ;; Bind keys in Ielm mode.
  (when (and (boundp 'ielm-map) icicle-redefine-standard-commands-flag)
    (define-key ielm-map "\t" 'icicle-comint-dynamic-complete))

  ;; Bind keys in Tcl mode.
  (when (and (boundp 'inferior-tcl-mode-map) icicle-redefine-standard-commands-flag)
    (define-key inferior-tcl-mode-map "\t" 'icicle-comint-dynamic-complete))

  ;; Bind keys in GUD (Debugger) mode.
  (when (and (boundp 'gud-minibuffer-local-map) icicle-redefine-standard-commands-flag)
    (define-key gud-minibuffer-local-map "\t" 'icicle-comint-dynamic-complete-filename))

  ;; Bind keys in Info mode.
  (when (and (boundp 'Info-mode-map) icicle-redefine-standard-commands-flag)
    (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map) ; `g'
    (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map) ; `i'
    (icicle-remap 'Info-menu       'icicle-Info-menu-cmd       Info-mode-map)) ; `m'

  ;; Bind some keys in Dired mode.
  (when (boundp 'dired-mode-map)
    (unless (lookup-key dired-mode-map [(control meta ?<)]) ; Dired `C-M-<'
      (define-key dired-mode-map [(control meta ?<)]
        'icicle-dired-saved-file-candidates-other-window))
    (unless (lookup-key dired-mode-map [(control ?{)]) ; Dired `C-{'
      (define-key dired-mode-map [(control ?{)] 'icicle-dired-project-other-window))
    (unless (lookup-key dired-mode-map [(control meta ?>)]) ; Dired `C-M->'
      (define-key dired-mode-map [(control meta ?>)] 'icicle-dired-save-marked))
    (unless (lookup-key dired-mode-map [(control ?>)]) ; Dired `C->'
      (define-key dired-mode-map [(control ?>)] 'icicle-dired-save-marked-more))
    (unless (lookup-key dired-mode-map [(control meta ?})]) ; Dired `C-M-}'
      (define-key dired-mode-map [(control meta ?})] 'icicle-dired-save-marked-to-variable))
    (unless (lookup-key dired-mode-map [(control ?})]) ; Dired `C-}'
      (define-key dired-mode-map [(control ?})] 'icicle-dired-save-marked-as-project))
    (let ((defn  (lookup-key dired-mode-map "\M-si"))) ; Dired `M-s i'
      (unless (and defn (not (integerp defn)))
        (define-key dired-mode-map "\M-si" 'icicle-search-dired-marked))))

  ;; Bind keys in Ibuffer mode.
  (when (boundp 'ibuffer-mode-map)
    (let ((defn  (lookup-key ibuffer-mode-map "\M-si"))) ; Ibuffer `M-s i'
      (unless (and defn (not (integerp defn)))
        (define-key ibuffer-mode-map "\M-si" 'icicle-search-ibuffer-marked))
      (unless icicle-touche-pas-aux-menus-flag ; Use Ibuffer > Operate menu.
        (define-key ibuffer-mode-operate-map [icicle-search-ibuffer-marked]
          '(menu-item "Icicles Search (and Replace)..." icicle-search-ibuffer-marked
            :visible icicle-mode :enable (eq major-mode 'ibuffer-mode))))))

  ;; Bind keys in Buffer Menu mode.
  (when (boundp 'Buffer-menu-mode-map)
    (let ((defn  (lookup-key Buffer-menu-mode-map "\M-si"))) ; Buffer-Menu `M-s i'
      (unless (and defn (not (integerp defn)))
        (define-key Buffer-menu-mode-map "\M-si" 'icicle-search-buff-menu-marked))))

  ;; Bind `S-TAB' in major maps, for key completion.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-bind-key-completion-keys-in-keymaps-from (current-global-map))
    (mapcar #'icicle-bind-key-completion-keys-for-map-var icicle-keymaps-for-key-completion))

  ;; Prevent `this-command' from being set to `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] 'icicle-skip-this-command)
  (define-key global-map [switch-frame] 'icicle-handle-switch-frame))

(defun icicle-bind-isearch-keys ()
  "Bind Icicles Isearch commands."
  (dolist (key icicle-search-from-isearch-keys)
    (define-key isearch-mode-map key
      (lambda ()
        (interactive)
        (isearch-done)
        (icicle-search (point-min) (point-max) (icicle-isearch-complete-past-string) t))))
  (dolist (key icicle-isearch-complete-keys)
    (define-key isearch-mode-map key 'icicle-isearch-complete))
  (cond ((fboundp 'isearch-moccur)      ; Defined in `moccur.el'.
         (define-key isearch-mode-map (kbd "C-o") 'isearch-moccur))
        ((fboundp 'isearch-occur)       ; Defined in `occur-schroeder.el'.
         (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))))

(defun icicle-bind-key-completion-keys-for-map-var (keymap-var)
  "Bind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (icicle-bind-key-completion-keys-in-keymaps-from temp)))))

(defun icicle-bind-key-completion-keys-in-keymaps-from (map)
  "Bind keys in `icicle-key-complete-keys' to `icicle-complete-keys'.
Each key in `icicle-complete-keys' is bound in all keymaps accessible
from keymap MAP."
  (dolist (key+map  (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      ;; We could try to exclude menu maps, by testing (not (keymap-prompt map)).
      ;; But we want to include at least some menu maps - those, such as `facemenu-keymap',
      ;; that are bound to keyboard keys. (when (and (keymapp map) (not (keymap-prompt map)))...)
      (when (keymapp map)
        (dolist (key  icicle-key-complete-keys)
          (when (or icicle-complete-key-anyway-flag (not (lookup-key map key)))
            (condition-case nil (define-key map key 'icicle-complete-keys) (error nil))))))))

(defun icicle-restore-other-keymap-keys ()
  "Restore some bindings changed by `icicle-bind-other-keymap-keys'."

  ;; Unbind Isearch keys.
  (icicle-unbind-isearch-keys)

  ;; Unbind keys in Comint mode.
  (when (boundp 'comint-mode-map)
    (define-key comint-mode-map "\C-c\C-i"         nil)
    (define-key comint-mode-map [(control ?c) tab] nil))

  ;; Unbind keys in Shell mode.
  (when (and (boundp 'shell-mode-map) icicle-redefine-standard-commands-flag)
    (define-key shell-mode-map "\t" 'comint-dynamic-complete))

  ;; Unbind keys in Shell Script mode.
  (when (and (boundp 'sh-mode-map) icicle-redefine-standard-commands-flag)
    (icicle-unmap 'comint-dynamic-complete sh-mode-map 'icicle-comint-dynamic-complete))

  ;; Unbind keys in Ielm mode.
  (when (and (boundp 'ielm-map) icicle-redefine-standard-commands-flag)
    (define-key ielm-map "\t" 'comint-dynamic-complete))

  ;; Unbind keys in Tcl mode.
  (when (and (boundp 'inferior-tcl-mode-map) icicle-redefine-standard-commands-flag)
    (define-key inferior-tcl-mode-map "\t" 'comint-dynamic-complete))

  ;; Bind keys in GUD (Debugger) mode.
  (when (and (boundp 'gud-minibuffer-local-map) icicle-redefine-standard-commands-flag)
    (define-key gud-minibuffer-local-map "\t" 'comint-dynamic-complete-filename))

  ;; Unbind keys in Info mode.
  (when (and (boundp 'Info-mode-map) icicle-redefine-standard-commands-flag)
    (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
    (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)
    (icicle-unmap 'Info-menu      Info-mode-map 'icicle-Info-menu-cmd))

  ;; Unbind keys in Dired mode.
  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map [(control meta ?<)] nil)
    (define-key dired-mode-map [(control ?{)]      nil)
    (define-key dired-mode-map [(control meta ?>)] nil)
    (define-key dired-mode-map [(control ?>)]      nil)
    (define-key dired-mode-map [(control meta ?})] nil)
    (define-key dired-mode-map [(control ?})]      nil)
    (define-key dired-mode-map "\M-si"             nil))

  ;; Unbind keys in Ibuffer mode.
  (when (boundp 'ibuffer-mode-map)
    (define-key ibuffer-mode-map "\M-si" nil))

  ;; Unbind keys in Buffer Menu mode.
  (when (boundp 'Buffer-menu-mode-map)
    (define-key Buffer-menu-mode-map "\M-si" nil))

  ;; Unbind `S-TAB' in major maps.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-unbind-key-completion-keys-in-keymaps-from (current-global-map))
    (mapcar #'icicle-unbind-key-completion-keys-for-map-var icicle-keymaps-for-key-completion))

  ;; Restore prevention of `this-command' being `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] nil)
  (define-key global-map [switch-frame] 'handle-switch-frame))

(defun icicle-unbind-isearch-keys ()
  "Unbind Icicles Isearch commands."
  (dolist (key icicle-search-from-isearch-keys) (define-key isearch-mode-map key nil))
  (dolist (key icicle-isearch-complete-keys) (define-key isearch-mode-map key nil))
  (define-key isearch-mode-map "\M-\t" 'isearch-complete)
  (when (fboundp 'isearch-moccur)       ; Restore `moccur.el' binding.
    (define-key isearch-mode-map (kbd "M-o") 'isearch-moccur))
  (define-key isearch-mode-map (kbd "C-o") nil))

(defun icicle-unbind-key-completion-keys-for-map-var (keymap-var)
  "Unbind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (icicle-unbind-key-completion-keys-in-keymaps-from temp)))))

(defun icicle-unbind-key-completion-keys-in-keymaps-from (map)
  "Unbind `icicle-key-complete-keys' in keymaps accessible from MAP."
  (dolist (key+map (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      (when (and (keymapp map) (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (dolist (key icicle-key-complete-keys)
          (when (eq (lookup-key map key) 'icicle-complete-keys)
            (condition-case nil (define-key map key nil) (error nil))))))))
 
;;(@* "Other Icicles functions that define Icicle mode")

;;; Other Icicles functions that define Icicle mode ------------------

(defun icicle-skip-this-command ()
  "Prevent `handle-switch-frame' from being added to `this-command'."
  (interactive)
  (setq this-command  last-command))

(defun icicle-handle-switch-frame (event)
  "Call `handle-switch-frame', but don't add it to `this-command'."
  (interactive "e")
  (handle-switch-frame event)
  (setq this-command  last-command))

(defun icicle-define-minibuffer-maps (turn-on-p)
  "Define keymaps for the minibuffer and buffer *Completions*."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))
       (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
         '(menu-item "Quit" icicle-abort-recursive-edit
           :help "Cancel minibuffer input or recursive edit"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help] '("--"))
       (define-key map [menu-bar minibuf completion-help]
         '(menu-item "Icicles Help" icicle-minibuffer-help
           :help "Display help for minibuffer input and completion"))
       (define-key map [menu-bar minibuf separator-last] '("--"))
       (define-key map [menu-bar minibuf icicle-read+insert-file-name]
         '(menu-item "Insert File Name" icicle-read+insert-file-name
           :help "Read and insert a file name using (lax) completion."))
       (define-key map [menu-bar minibuf icicle-clear-current-history]
         '(menu-item "Clear History Entries" icicle-clear-current-history
           :help "Clear current minibuffer history of selected entries."))
       (define-key map [menu-bar minibuf icicle-insert-history-element]
         '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
           :enable (consp (symbol-value minibuffer-history-variable))
           :help "Use completion to insert a previously entered input in the minibuffer."))

       (define-key map [(control ??)]            'icicle-minibuffer-help) ; `C-?'
       (define-key map [(control ?g)]            'icicle-abort-recursive-edit) ; `C-g'
       (define-key map [M-S-backspace]           'icicle-erase-minibuffer) ; `M-S-backspace'
       (define-key map [M-S-delete]              'icicle-erase-minibuffer) ; `M-S-delete'
       (define-key map [(meta ?.)]               'icicle-insert-string-at-point) ; `M-.'
       (define-key map [(control ?=)]            'icicle-insert-string-from-variable) ; `C-='
       (define-key map [(meta ?o)]               'icicle-insert-history-element) ; `M-o'
       (define-key map [(meta ?i)]               'icicle-clear-current-history) ; `M-i'
       (define-key map [(meta ?k)]             'icicle-erase-minibuffer-or-history-element) ; `M-k'
       (define-key map [(meta ?:)]               'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
       (define-key map [(control ?a)]            'icicle-beginning-of-line+) ; `C-a'
       (define-key map [(control ?e)]            'icicle-end-of-line+) ; `C-e'
       (dolist (key  icicle-completing-read+insert-keys)
         (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys)
         (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
       (define-key map "\n"                      'icicle-insert-newline-in-minibuffer) ; `C-j'
       (when (fboundp 'yank-secondary)  ; Defined in `second-sel.el'.
         (define-key map "\C-\M-y" 'icicle-yank-secondary))) ; `C-M-y'

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-minibuffer-help
             :help "Display help for minibuffer input and completion"))
         (define-key map [menu-bar minibuf separator-last] '("--"))
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]
           '(menu-item "Insert File Name" icicle-read+insert-file-name
             :help "Read and insert a file name using (lax) completion."))
         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries."))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ??)]            'icicle-minibuffer-help) ; `C-?'
         (define-key map [(control ?g)]            'icicle-abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace]           'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map [M-S-delete]              'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map [(meta ?.)]               'icicle-insert-string-at-point) ; `M-.'
         (define-key map [(control ?=)]            'icicle-insert-string-from-variable) ; `C-='
         (define-key map [(meta ?o)]               'icicle-insert-history-element) ; `M-o'
         (define-key map [(meta ?i)]               'icicle-clear-current-history) ; `M-i'
         (define-key map [(meta ?k)]           'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map [(meta ?:)]              'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map [(control ?a)]            'icicle-beginning-of-line+) ; `C-a'
         (define-key map [(control ?e)]            'icicle-end-of-line+) ; `C-e'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         (define-key map "\n"                      'icicle-insert-newline-in-minibuffer) ; `C-j'
         (when (fboundp 'yank-secondary) ; Defined in `second-sel.el'.
           (define-key map "\C-\M-y" 'icicle-yank-secondary)))) ; `C-M-y'

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-minibuffer-help
             :help "Display help for minibuffer input and completion"))
         (define-key map [menu-bar minibuf separator-last] '("--"))
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]
           '(menu-item "Insert File Name" icicle-read+insert-file-name
             :help "Read and insert a file name using (lax) completion."))
         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries."))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ??)]            'icicle-minibuffer-help) ; `C-?'
         (define-key map [(control ?g)]            'icicle-abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace]           'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map [M-S-delete]              'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map [(meta ?.)]               'icicle-insert-string-at-point) ; `M-.'
         (define-key map [(control ?=)]            'icicle-insert-string-from-variable) ; `C-='
         (define-key map [(meta ?o)]               'icicle-insert-history-element) ; `M-o'
         (define-key map [(meta ?i)]               'icicle-clear-current-history) ; `M-i'
         (define-key map [(meta ?k)]           'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map [(meta ?:)]              'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map [(control ?a)]            'icicle-beginning-of-line+) ; `C-a'
         (define-key map [(control ?e)]            'icicle-end-of-line+) ; `C-e'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         (define-key map "\n"                      'icicle-insert-newline-in-minibuffer) ; `C-j'
         (when (fboundp 'yank-secondary) ; Defined in `second-sel.el'.
           (define-key map "\C-\M-y" 'icicle-yank-secondary)))) ; `C-M-y'

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-bind-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map [(control ?g)]
         'icicle-abort-recursive-edit)  ; `C-g' - need it anyway, even if inherit completion map.
       ;; Override the binding of `C-j' to `minibuffer-complete-and-exit'.
       (define-key minibuffer-local-must-match-map "\n"
         'icicle-insert-newline-in-minibuffer)) ; `C-j' (newline)
     (define-key minibuffer-local-must-match-map [S-return] ; `S-RET'
       'icicle-apropos-complete-and-exit)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively.  For Emacs 23.1,
     ;; `minibuffer-local-must-match-filename-map' is an alias for
     ;; `minibuffer-local-filename-must-match-map'.  But for Emacs 23.2, there is no such alias!
     (when (boundp 'minibuffer-local-filename-completion-map)
       (when (and (featurep 'bookmark+) (fboundp 'icicle-bookmark-file-other-window))
         (define-key minibuffer-local-filename-completion-map
             "\C-xm" 'icicle-bookmark-file-other-window))
       (define-key minibuffer-local-filename-completion-map
         [(control backspace)] 'icicle-up-directory)
       (define-key minibuffer-local-filename-completion-map "\C-c+" 'icicle-make-directory))
     ;; Use the old map name, for Emacs 22-23.1.
     (when (boundp 'minibuffer-local-must-match-filename-map)
       (when (and (featurep 'bookmark+) (fboundp 'icicle-bookmark-file-other-window))
         (define-key minibuffer-local-must-match-filename-map
             "\C-xm" 'icicle-bookmark-file-other-window))
       (define-key minibuffer-local-must-match-filename-map
         [(control backspace)] 'icicle-up-directory)
       (define-key minibuffer-local-must-match-filename-map "\C-c+" 'icicle-make-directory))
     ;; Use the new name, for Emacs 23.2+.
     (when (boundp 'minibuffer-local-filename-must-match-map)
       (when (and (featurep 'bookmark+) (fboundp 'icicle-bookmark-file-other-window))
         (define-key minibuffer-local-filename-must-match-map
             "\C-xm" 'icicle-bookmark-file-other-window))
       (define-key minibuffer-local-filename-must-match-map
         [(control backspace)] 'icicle-up-directory)
       (define-key minibuffer-local-filename-must-match-map "\C-c+" 'icicle-make-directory))

     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on `C-insert'.  Do not allow normal input.
     (let ((map  completion-list-mode-map))
       (define-key map [(control ??)]     'icicle-minibuffer-help) ; `C-?'
       (define-key map [(control ?g)]     'icicle-abort-recursive-edit) ; `C-g'
       (define-key map "q"                'icicle-abort-recursive-edit) ; `q'
       (define-key map [(control insert)] 'icicle-insert-completion) ; `C-insert'
       (define-key map [down]             'icicle-next-line) ; `down'
       (define-key map [up]               'icicle-previous-line) ; `up'
       (define-key map [right]            'icicle-move-to-next-completion) ; `right'
       (define-key map [left]             'icicle-move-to-previous-completion) ; `left'
       (dolist (key icicle-previous-candidate-keys)
         (define-key map key 'icicle-move-to-previous-completion)) ; `S-TAB'
       (define-key map [(control ?i)]     'icicle-move-to-next-completion) ; `TAB'
       (define-key map [tab]              'icicle-move-to-next-completion) ; `TAB'
       (when (boundp 'mouse-wheel-down-event) ; Emacs 22+ -  `wheel-down', `wheel-up'
         (define-key map (vector mouse-wheel-down-event) 'icicle-scroll-Completions-up)
         (define-key map (vector mouse-wheel-up-event) 'icicle-scroll-Completions))
       (define-key map [S-down-mouse-2]   'icicle-mouse-remove-candidate) ; `S-mouse-2'
       (define-key map [S-mouse-2]        'ignore)
       (define-key map [C-S-down-mouse-2] 'icicle-mouse-candidate-alt-action) ; `C-S-mouse-2'
       (define-key map [C-S-mouse-2]      'ignore)
       (define-key map [C-down-mouse-2]   'icicle-mouse-candidate-action) ; `C-mouse-2'
       (define-key map [C-mouse-2]        'ignore)
       (define-key map [C-M-return]       'icicle-help-on-candidate) ; `C-M-RET'
       (define-key map [C-M-down-mouse-2] 'icicle-mouse-help-on-candidate) ; `C-M-mouse-2'
       (define-key map [C-M-mouse-2]      'ignore)
       (define-key map [M-S-down-mouse-2] 'icicle-mouse-save/unsave-candidate) ; `M-S-mouse-2'
       (define-key map [M-S-mouse-2]      'ignore)
       (define-key map [M-down-mouse-2]   'icicle-mouse-candidate-read-fn-invoke) ; `M-mouse-2'
       (define-key map [M-mouse-2]        'ignore)
       (define-key map [C-down-mouse-3]   'icicle-Completions-mouse-3-menu) ; `C-mouse-3'
       (define-key map [C-mouse-3]        'ignore)
       (define-key map [M-down-mouse-3]   'icicle-mouse-candidate-set-save-more) ; `M-mouse-3'
       (define-key map [M-mouse-3]        'ignore)
       (define-key map [M-S-down-mouse-3] 'icicle-mouse-candidate-set-save) ; `M-S-mouse-3'
       (define-key map [M-S-mouse-3]      'ignore)
       (define-key map [mouse-3]          'icicle-mouse-save-then-kill) ; `mouse-3'
       (define-key map [(control ?>)]     'icicle-candidate-set-save-more) ; `C->'
       (define-key map [(control meta ?>)] 'icicle-candidate-set-save) ; `C-M->'
       (define-key map [(control ?\))]    'icicle-candidate-set-save-more-selected) ; `C-)'
       (define-key map [(control meta ?\))] 'icicle-candidate-set-save-selected) ; `C-M-)'
       (define-key map [(control meta ?<)] 'icicle-candidate-set-retrieve) ; `C-M-<'
       (define-key map [(control ?l)]      'icicle-retrieve-previous-input) ; `C-l'
       (define-key map [(control ?a)]      'icicle-beginning-of-line+) ; `C-a'
       (define-key map [(control ?e)]      'icicle-end-of-line+) ; `C-e'
       ;; (suppress-keymap map) ; Inhibit character self-insertion.
       ))

    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))
       (define-key map [menu-bar minibuf quit]
         '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help]                nil)
       (define-key map [menu-bar minibuf completion-help]               nil)
       (define-key map [menu-bar minibuf separator-last]                nil)
       (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
       (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

       (define-key map [(control ??)]            nil) ; `C-?'
       (define-key map [(control ?g)]            (if (and (fboundp 'minibuffer-keyboard-quit)
                                                          delete-selection-mode)
                                                     'minibuffer-keyboard-quit
                                                   'abort-recursive-edit)) ; `C-g'
       (define-key map [M-S-backspace]           nil) ; `M-S-DEL'
       (define-key map [M-S-delete]              nil) ; `M-S-delete'
       (define-key map [(meta ?.)]               nil) ; `M-.'
       (define-key map [(control ?=)]            nil) ; `C-='
       (define-key map [(meta ?o)]               nil) ; `M-o'
       (define-key map [(meta ?i)]               nil) ; `M-i'
       (define-key map [(meta ?k)]               nil) ; `M-k'
       (define-key map [(meta ?:)]               nil) ; `M-:'
       (define-key map [(control ?a)]            nil) ; `C-a'
       (define-key map [(control ?e)]            nil) ; `C-e'
       (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
       (define-key map "\n"                      'exit-minibuffer) ; `C-j'
       (define-key map "\C-\M-y"                 nil)) ; `C-M-y'

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                nil)
         (define-key map [menu-bar minibuf completion-help]               nil)
         (define-key map [menu-bar minibuf separator-last]                nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

         (define-key map [(control ??)]            nil) ; `C-?'
         (define-key map [(control ?g)]            (if (and (fboundp 'minibuffer-keyboard-quit)
                                                            delete-selection-mode)
                                                       'minibuffer-keyboard-quit
                                                     'abort-recursive-edit)) ; `C-g'
         (define-key map [M-S-backspace]           nil) ; `M-S-DEL'
         (define-key map [M-S-delete]              nil) ; `M-S-delete'
         (define-key map [(meta ?.)]               nil) ; `M-.'
         (define-key map [(control ?=)]            nil) ; `C-='
         (define-key map [(meta ?o)]               nil) ; `M-o'
         (define-key map [(meta ?i)]               nil) ; `M-i'
         (define-key map [(meta ?k)]               nil) ; `M-k'
         (define-key map [(meta ?:)]               nil) ; `M-:'
         (define-key map [(control ?a)]            nil) ; `C-a'
         (define-key map [(control ?e)]            nil) ; `C-e'
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         (define-key map "\n"                      'exit-minibuffer) ; `C-j'
         (define-key map "\C-\M-y"                 nil))) ; `C-M-y'

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                nil)
         (define-key map [menu-bar minibuf completion-help]               nil)
         (define-key map [menu-bar minibuf separator-last]                nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]  nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

         (define-key map [(control ??)]            nil) ; `C-?'
         (define-key map [(control ?g)]            (if (and (fboundp 'minibuffer-keyboard-quit)
                                                            delete-selection-mode)
                                                       'minibuffer-keyboard-quit
                                                     'abort-recursive-edit)) ; `C-g'
         (define-key map [M-S-backspace]           nil) ; `M-S-DEL'
         (define-key map [M-S-delete]              nil) ; `M-S-delete'
         (define-key map [(meta ?.)]               nil) ; `M-.'
         (define-key map [(control ?=)]            nil) ; `C-='
         (define-key map [(meta ?o)]               nil) ; `M-o'
         (define-key map [(meta ?i)]               nil) ; `M-i'
         (define-key map [(meta ?k)]               nil) ; `M-k'
         (define-key map [(meta ?:)]               nil) ; `M-:'
         (define-key map [(control ?a)]            nil) ; `C-a'
         (define-key map [(control ?e)]            nil) ; `C-e'
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         (define-key map "\n"                      'exit-minibuffer))) ; `C-j'

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-restore-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map [(control ?g)]
         (if (and (fboundp 'minibuffer-keyboard-quit)
                  delete-selection-mode)
             'minibuffer-keyboard-quit
           'abort-recursive-edit))  ; `C-g' - need it anyway, even if inherit completion map.
       (define-key minibuffer-local-must-match-map "\n" 'minibuffer-complete-and-exit)) ; `C-j'
     (define-key minibuffer-local-must-match-map [S-return] nil)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively.  For Emacs 23.1,
     ;; `minibuffer-local-must-match-filename-map' is an alias for
     ;; `minibuffer-local-filename-must-match-map'.  But for Emacs 23.2, there is no such alias!
     (when (boundp 'minibuffer-local-filename-completion-map)
       (define-key minibuffer-local-filename-completion-map "\C-xm"               nil)
       (define-key minibuffer-local-filename-completion-map [(control backspace)] nil)
       (define-key minibuffer-local-filename-completion-map "\C-c+"               nil))
     ;; Use the old name, for Emacs 22-23.1.
     (when (boundp 'minibuffer-local-must-match-filename-map)
       (define-key minibuffer-local-must-match-filename-map "\C-xm"               nil)
       (define-key minibuffer-local-must-match-filename-map [(control backspace)] nil))
     ;; Use the new name, for Emacs 23.2+.
     (when (boundp 'minibuffer-local-filename-must-match-map)
       (define-key minibuffer-local-filename-must-match-map "\C-xm"               nil)
       (define-key minibuffer-local-filename-must-match-map [(control backspace)] nil)
       (define-key minibuffer-local-filename-must-match-map "\C-c+"               nil))

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (let ((map  completion-list-mode-map))
       (define-key map [(control ?g)]       nil)
       (define-key map "q"                  nil)
       (define-key map [(control insert)]   nil)
       (dolist (key icicle-prefix-cycle-next-keys)     (define-key map key nil))
       (dolist (key icicle-prefix-cycle-previous-keys) (define-key map key nil))
       (dolist (key icicle-previous-candidate-keys)   (define-key map key nil))
       (define-key map [(control ?i)]       nil)
       (define-key map [tab]                nil)
       (define-key map [S-down-mouse-2]     nil)
       (define-key map [S-mouse-2]          nil)
       (define-key map [C-S-down-mouse-2]   nil)
       (define-key map [C-S-mouse-2]        nil)
       (define-key map [C-down-mouse-2]     nil)
       (define-key map [C-mouse-2]          nil)
       (define-key map [C-M-return]         nil)
       (define-key map [C-M-down-mouse-2]   nil)
       (define-key map [C-M-mouse-2]        nil)
       (define-key map [M-S-down-mouse-2]   nil)
       (define-key map [M-S-mouse-2]        nil)
       (define-key map [M-down-mouse-2]     nil)
       (define-key map [M-mouse-2]          nil)
       (define-key map [C-down-mouse-3]     nil)
       (define-key map [M-down-mouse-3]     nil)
       (define-key map [M-mouse-3]          nil)
       (define-key map [M-S-down-mouse-3]   nil)
       (define-key map [M-S-mouse-3]        nil)
       (define-key map [mouse-3]            nil)
       (define-key map [C-mouse-3]          nil)
       (define-key map [(control ?>)]       nil)
       (define-key map [(control meta ?>)]  nil)
       (define-key map [(control ?\))]      nil)
       (define-key map [(control meta ?\))] nil)
       (define-key map [(control meta ?<)]  nil)
       (define-key map [(control ?l)]       nil)
       (define-key map [(control ?a)]       nil)
       (define-key map [(control ?e)]       nil)
       (define-key map [down]               nil)
       (define-key map [up]                 nil)
       ;; Do these last:
       (define-key map [right]              'next-completion)
       (define-key map [left]               'previous-completion))))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-minibuffer-help]' in minibuffer for help."))))

(defun icicle-unmap (command map current)
  "In MAP, unbind any keys that are bound to COMMAND.
If command remapping is available, remap COMMAND to nil in MAP,
unbinding it.
Otherwise, bind COMMAND to whatever CURRENT is bound to in MAP."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap command) nil)
    (substitute-key-definition current command map)))

(defun icicle-rebind-global (old new map)
  "Bind command NEW in MAP to all keys currently bound globally to OLD."
  (substitute-key-definition old new map (current-global-map)))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
      '(menu-item "Quit" icicle-abort-recursive-edit
        :help "Cancel minibuffer input or recursive edit"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help] '("--"))
    (define-key map [menu-bar minibuf completion-help]
      '(menu-item "Icicles Help" icicle-minibuffer-help
        :help "Display help for minibuffer input and completion"))
    (define-key map [menu-bar minibuf separator-last] '("--"))
    (define-key map [menu-bar minibuf icicle-read+insert-file-name]
      '(menu-item "Insert File Name" icicle-read+insert-file-name
        :help "Read and insert a file name using (lax) completion."))
    (define-key map [menu-bar minibuf icicle-clear-current-history]
      '(menu-item "Clear History Entries" icicle-clear-current-history
        :help "Clear current minibuffer history of selected entries."))
    (define-key map [menu-bar minibuf icicle-insert-history-element]
      '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
        :enable (consp (symbol-value minibuffer-history-variable))
        :help "Use completion to insert a previously entered input in the minibuffer.")))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]
    '(menu-item "Restore Next Completion Input" icicle-retrieve-next-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle forward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]
    '(menu-item "Restore Previous Completion Input" icicle-retrieve-previous-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle backward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf separator-C-l] '("--"))
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf alt-action-list-all]
    '(menu-item "Alt Act on List of Candidates" icicle-all-candidates-list-alt-action
      :help "Apply the alternative action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-alt-action-fn))
  (define-key map [menu-bar minibuf alt-action-all]
    '(menu-item "Alt Act on Each Candidate" icicle-all-candidates-alt-action
      :help "Apply the alternative action to each matching completion candidates"
      :enable icicle-candidate-alt-action-fn))
  (define-key map [menu-bar minibuf action-list-all]
    '(menu-item "Act on List of Candidates" icicle-all-candidates-list-action
      :help "Apply the command action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-action-fn))
  (define-key map [menu-bar minibuf action-all]
    '(menu-item "Act on Each Candidate" icicle-all-candidates-action
      :help "Apply the command action to each matching completion candidates"
      :enable icicle-candidate-action-fn))
  (define-key map [menu-bar minibuf separator-actions] '("--"))
  (define-key map [menu-bar minibuf set-define]
    '(menu-item "Define Candidates by Lisp Sexp" icicle-candidate-set-define
      :help "Define the set of current completion candidates by evaluating a sexp"))
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]
    '(menu-item "Keep Only Previously Entered" icicle-keep-only-past-inputs
      :enable (and icicle-completion-candidates (consp (symbol-value minibuffer-history-variable)))
      :help "Removed candidates that you have not previously chosen and entered"))
  (define-key map [menu-bar minibuf set-union]
    '(menu-item "Add (Union) Saved Candidate Set" icicle-candidate-set-union
      :enable icicle-saved-completion-candidates
      :help "Set union between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-difference]
    '(menu-item "Subtract Saved Candidate Set" icicle-candidate-set-difference
      :enable icicle-saved-completion-candidates
      :help "Set difference between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-intersection]
    '(menu-item "Intersect Saved Candidate Set" icicle-candidate-set-intersection
      :enable icicle-saved-completion-candidates
      :help "Set intersection between the current and saved candidates"))
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]
    '(menu-item "Save Predicate to Variable" icicle-save-predicate-to-variable
      :help "Save the current completion predicate to a variable"))
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate]
    '(menu-item "Satisfy Also Predicate..." icicle-narrow-candidates-with-predicate
      :help "Match another input pattern (narrow completions set)"
      :enable icicle-completion-candidates))
  (define-key map [menu-bar minibuf icicle-narrow-candidates]
    '(menu-item "Match Also Regexp..." icicle-narrow-candidates
      :enable icicle-completion-candidates
      :help "Match another input pattern (narrow completions set)"))
  (define-key map [menu-bar minibuf icicle-widen-candidates]
    '(menu-item "Match Alternative..." icicle-widen-candidates
      :enable icicle-completion-candidates
      :help "Match alternative input pattern (widen completions set)"))
  (define-key map [menu-bar minibuf set-complement]
    '(menu-item "Complement Candidates" icicle-candidate-set-complement
      :help "Complement the set of current completion candidates"))
  (define-key map [menu-bar minibuf separator-set1] '("--"))
  (define-key map [menu-bar minibuf set-swap]
    '(menu-item "Swap Saved and Current Sets" icicle-candidate-set-swap
      :enable icicle-saved-completion-candidates
      :help "Swap the saved and current sets of completion candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected]
    '(menu-item "Save More Selected (Region) Candidates"
      icicle-candidate-set-save-more-selected
      :help "Add the candidates in the region to the saved candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]
    '(menu-item "Save Selected (Region) Candidates"
      icicle-candidate-set-save-selected
      :help "Save the candidates in the region, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]
    '(menu-item "Save More Candidates" icicle-candidate-set-save-more
      :help "Add current completion candidates to saved candidates set"))
  (define-key map [menu-bar minibuf set-save-to-cache-file]
    '(menu-item "    to Cache File..." icicle-candidate-set-save-persistently
      :help "Save current completion candidates to a cache file, for later recall"))
  (define-key map [menu-bar minibuf set-save-to-variable]
    '(menu-item "    to Variable..." icicle-candidate-set-save-to-variable
      :help "Save current completion candidates to a variable, for later recall"))
  (define-key map [menu-bar minibuf set-save]
    '(menu-item "Save Candidates" icicle-candidate-set-save
      :help "Save the set of current completion candidates, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-retrieve-more]
    '(menu-item "Retrieve More Saved Candidates"
      icicle-candidate-set-retrieve-more
      :help "Add saved candidates to current completion candidates"))
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
    '(menu-item "    from Cache File..."
      icicle-candidate-set-retrieve-persistent
      :help "Retrieve saved completion candidates from a cache file, making them current"))
  (define-key map [menu-bar minibuf set-retrieve-from-variable]
    '(menu-item "    from Variable..." icicle-candidate-set-retrieve-from-variable
      :help "Retrieve saved completion candidates from variable, making them current"))
  (define-key map [menu-bar minibuf set-retrieve]
    '(menu-item "Retrieve Saved Candidates" icicle-candidate-set-retrieve
      :enable icicle-saved-completion-candidates
      :help "Retrieve the saved set of completion candidates, making it current"))
  (define-key map [menu-bar minibuf separator-set2] '("--"))
  (define-key map [menu-bar minibuf word-complete]
    '(menu-item "Word-Complete" icicle-prefix-word-complete
      :help "Complete at most one word of prefix"))
  (define-key map [menu-bar minibuf prefix-complete]
    '(menu-item "Prefix-Complete" icicle-prefix-complete
      :help "Complete prefix as far as possible"))
  (define-key map [menu-bar minibuf apropos-complete]
    '(menu-item "Apropos-Complete" icicle-apropos-complete :keys "S-TAB"
      :help "Complete regular expression as far as possible and list completions"))

  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument ; `C-u'
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument ; `M--'
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument ; `C-9'
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char ; `C-d', `deletechar'
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word ; `M-DEL'
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word ; `M-d'
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp ; `C-M-backspace'
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp ; `C-M-k', `C-M-delete'
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence ; `C-x DEL'
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph ; `C-backspace'
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph ; `C-delete'
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line ; `C-k', `deleteline'
                map (current-global-map))
  (icicle-remap 'reposition-window             'icicle-goto/kill-failed-input ; `C-M-l'
                map (current-global-map))
  (icicle-remap 'transpose-chars               'icicle-transpose-chars ; `C-t'
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words ; `M-t'
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps ; `C-M-t'
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop ; `M-y', `M-insert'
                map (current-global-map))
  (icicle-remap 'mouse-yank-secondary          'icicle-mouse-yank-secondary ; `M-mouse-2'
                map (current-global-map))

  ;; Bind additional keys.
  (dolist (key icicle-word-completion-keys)
    (define-key map key 'icicle-prefix-word-complete)) ; `M-SPC'
  (dolist (key icicle-apropos-complete-keys)
    (define-key map key 'icicle-apropos-complete)) ; `S-TAB'
  (dolist (key icicle-prefix-complete-keys) (define-key map key 'icicle-prefix-complete)) ; `TAB'
  (dolist (key icicle-apropos-complete-no-display-keys)
    (define-key map key 'icicle-apropos-complete-no-display)) ; `C-M-S-TAB'
  (dolist (key icicle-prefix-complete-no-display-keys)
    (define-key map key 'icicle-prefix-complete-no-display)) ; `C-M-TAB'

  (icicle-define-cycling-keys map)      ;     `up',     `down',     `prior',     `next',
                                        ;   `C-up',   `C-down',   `C-prior',   `C-next',
                                        ; `C-M-up', `C-M-down', `C-M-prior', `C-M-next',
                                        ; `C-S-up', `C-S-down', `C-S-prior', `C-S-next',
  (define-key map [(control help)]           'icicle-help-on-candidate) ; `C-help'
  (define-key map [(control meta help)]      'icicle-help-on-candidate) ; `C-M-help'
  (define-key map [(control f1)]             'icicle-help-on-candidate) ; `C-f1'
  (define-key map [(control meta f1)]        'icicle-help-on-candidate) ; `C-M-f1'
  (define-key map [(control meta return)]    'icicle-help-on-candidate) ; `C-M-RET'
  (define-key map [(meta return)]            'icicle-candidate-read-fn-invoke) ;`M-RET' as `M-return'
  (define-key map "\C-\M-m"                  'icicle-candidate-read-fn-invoke) ;`M-RET' as `ESC RET'
  (define-key map [(control shift return)]   'icicle-candidate-alt-action) ; `C-S-RET'
  (define-key map [delete]                   'icicle-remove-candidate) ; `delete'
  (define-key map [(shift delete)]           'icicle-delete-candidate-object) ; `S-delete'
  (define-key map [(control ?w)]             'icicle-kill-region) ; `C-w'
  (define-key map [(control return)]         'icicle-candidate-action) ; `C-RET'
  (define-key map [(control ?!)]             'icicle-all-candidates-action) ; `C-!'
  (define-key map [(control ?|)]             'icicle-all-candidates-alt-action) ; `C-|'
  (define-key map [(meta ?!)]                'icicle-all-candidates-list-action) ; `M-!'
  (define-key map [(meta ?|)]                'icicle-all-candidates-list-alt-action) ; `M-|'
  (define-key map [(control meta ?/)]        'icicle-prefix-complete) ; `C-M-/', for `dabbrev.el'.
  (define-key map [(meta ?h)]                'icicle-history) ; `M-h'
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs) ; `M-pause'
  (define-key map [(control pause)]     'icicle-toggle-highlight-historical-candidates) ; `C-pause'
  (define-key map [(control meta pause)]     'icicle-other-history) ; `C-M-pause'
  (define-key map [(control insert)]         'icicle-switch-to-Completions-buf) ; `C-insert'
  (define-key map [insert]                   'icicle-save/unsave-candidate) ; `insert'

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    ;; Note: `setup-keys.el' binds `C-o' to `1on1-fit-minibuffer-frame' if defined.
    (define-key map [(control ?a)]           'icicle-beginning-of-line+) ; `C-a'
    (define-key map [(control ?e)]           'icicle-end-of-line+) ; `C-e'
    (define-key map [(control ?=)]           'icicle-insert-string-from-variable) ; `C-='
    ;; Replaces `tab-to-tab-stop':
    (define-key map [(meta ?i)]              'icicle-clear-current-history) ; `M-i'
    ;; Replaces `kill-sentence':
    (define-key map [(meta ?k)]              'icicle-erase-minibuffer-or-history-element) ; `M-k'
    (define-key map [(meta ?o)]              'icicle-insert-history-element) ; `M-o'
    (define-key map [(meta ?.)]              'icicle-insert-string-at-point) ; `M-.'
    (define-key map [(meta ?:)]              'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
    (when (fboundp 'yank-secondary)     ; Defined in `second-sel.el'.
      (define-key map "\C-\M-y"              'icicle-yank-secondary)) ; `C-M-y'
    (define-key map [M-S-backspace]          'icicle-erase-minibuffer) ; `M-S-backspace'
    (define-key map [M-S-delete]             'icicle-erase-minibuffer) ; `M-S-delete'
    )

  ;; Need `C-g', even if `minibuffer-local-completion-map' inherits from `minibuffer-local-map'.
  (define-key map [(control ?g)]             'icicle-abort-recursive-edit) ; `C-g'
  (define-key map [(meta ?q)]                'icicle-dispatch-M-q) ; `M-q'
  (define-key map [(control ?l)]             'icicle-retrieve-previous-input) ; `C-l'
  (define-key map [(control shift ?l)]       'icicle-retrieve-next-input) ; `C-L' (`C-S-l')
  (define-key map [(meta ?$)]                'icicle-candidate-set-truncate) ; `M-$'
  (define-key map [(control ?~)]             'icicle-candidate-set-complement) ; `C-~'
  (define-key map [(control ?-)]             'icicle-candidate-set-difference) ; `C--'
  (define-key map [(control ?+)]             'icicle-candidate-set-union) ; `C-+'
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection) ; `C-*'
  (define-key map [(control ?>)]             'icicle-candidate-set-save-more) ; `C->'
  (define-key map [(control meta ?>)]        'icicle-candidate-set-save) ; `C-M->'
  (define-key map [(control ?\()]            'icicle-next-TAB-completion-method) ; `C-('
  (define-key map [(meta ?\()]               'icicle-next-S-TAB-completion-method) ; `M-('
  (define-key map [(control ?\))]            'icicle-candidate-set-save-more-selected) ; `C-)'
  (define-key map [(control meta ?\))]       'icicle-candidate-set-save-selected) ; `C-M-)'
  (define-key map [(control meta ?<)]        'icicle-candidate-set-retrieve) ; `C-M-<'
  (define-key map [(control meta ?})]        'icicle-candidate-set-save-to-variable) ; `C-M-}'
  (define-key map [(control meta ?{)]       'icicle-candidate-set-retrieve-from-variable) ; `C-M-{'
  (define-key map [(control ?})]             'icicle-candidate-set-save-persistently) ; `C-}'
  (define-key map [(control ?{)]             'icicle-candidate-set-retrieve-persistent) ; `C-{'
  (define-key map [(control ?%)]             'icicle-candidate-set-swap) ; `C-%'
  (define-key map [(control ?:)]             'icicle-candidate-set-define) ; `C-:'
  (define-key map [(control meta ?j)]        'icicle-insert-list-join-string) ; `C-M-j'
  (define-key map [(control ?,)]             'icicle-change-sort-order) ; `C-,'
  (define-key map [(control meta ?\;)]       'icicle-regexp-quote-input) ; `C-M-;'
  (define-key map [(control ?`)]             'icicle-toggle-regexp-quote) ; `C-`'
  (define-key map [(control meta ?\.)]       'icicle-toggle-dot) ; `C-M-.'
  (define-key map [(control meta ?`)]        'icicle-toggle-literal-replacement) ; `C-M-`'
  (define-key map [(control ?<)]             'icicle-candidate-set-retrieve-more) ; `C-<'
  (define-key map [(control meta ?_)]        'icicle-toggle-proxy-candidates) ; `C-M-_'
  (define-key map [(control ?$)]             'icicle-toggle-transforming) ; `C-$'
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [(control ??)]           'icicle-minibuffer-help)) ; `C-?'
  (define-key map [(control ?.)]             'icicle-dispatch-C-.) ; `C-.'
  (define-key map [(control ?#)]             'icicle-toggle-incremental-completion) ; `C-#'
  (define-key map [(control ?\;)]            'icicle-toggle-expand-to-common-match) ; `C-;'
  (define-key map [(meta ?\;)]               'icicle-toggle-search-replace-common-match) ; `M-;'
  (define-key map [(control ?^)]             'icicle-dispatch-C-^) ; `C-^'
  (define-key map [(control shift ?a)]       'icicle-toggle-case-sensitivity) ; `C-S-a' (`C-A')
  (define-key map [(meta ?~)]                'icicle-toggle-~-for-home-dir) ; `M-~'
  (define-key map [(meta ?g)]                'icicle-toggle-C-for-actions) ; `M-g'
  (define-key map [(meta ?,)]                'icicle-dispatch-M-comma) ; `M-,'
  (define-key map [(control meta ?,)]        'icicle-toggle-alternative-sorting) ; `C-M-,'
  (define-key map [(meta ?+)]                'icicle-widen-candidates) ; `M-+'
  (define-key map [(meta ?*)]                'icicle-narrow-candidates) ; `M-*'
  (define-key map [(meta ?&)]                'icicle-narrow-candidates-with-predicate) ; `M-&'
  (define-key map [(meta ?_)]                'icicle-dispatch-M-_) ; `M-_'
  (define-key map [(control meta ?&)]        'icicle-save-predicate-to-variable) ; `C-M-&'
  (define-key map [(shift ?\ )]              'icicle-apropos-complete-and-narrow) ; `S-SPC'
  (define-key map [(shift backspace)]        'icicle-apropos-complete-and-widen) ; `S-DEL'
  (define-key map "\C-v"                     'icicle-scroll-Completions) ; `C-v'
  (define-key map "\M-v"                     'icicle-scroll-Completions-up) ; `M-v'
  (define-key map "."                        'icicle-insert-dot-command) ; `.'
  (define-key map "\M-m"                     'icicle-toggle-show-multi-completion) ; `M-m'
  (define-key map "\C-x."                    'icicle-toggle-hiding-common-match) ; `C-x .'
  (when (fboundp 'doremi)
    (define-key map "\C-xw"                  'icicle-doremi-candidate-width-factor+) ; `C-x w'
    (define-key map "\C-x|"                  'icicle-doremi-inter-candidates-min-spaces+) ; `C-x |'
    (define-key map "\C-x#"                  'icicle-doremi-increment-max-candidates+) ; `C-x #'
    (when (fboundp 'text-scale-increase) ; Emacs 23+.
      (define-key map "\C-x-"                'icicle-doremi-zoom-Completions+)) ; `C-x -'
    (when (eq (icicle-current-TAB-method) 'swank)
      (define-key map "\C-x1"                'icicle-doremi-increment-swank-timeout+)
      (define-key map "\C-x2"                'icicle-doremi-increment-swank-prefix-length+)))
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert) ; `?'
  (define-key map " "                        'icicle-self-insert) ; " "
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map "\n"                     'icicle-insert-newline-in-minibuffer))) ; `C-j'

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit]
      '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help]                        nil)
    (define-key map [menu-bar minibuf completion-help]                       nil)
    (define-key map [menu-bar minibuf separator-last]                        nil)
    (define-key map [menu-bar minibuf icicle-clear-current-history]          nil)
    (define-key map [menu-bar minibuf icicle-insert-history-element]         nil))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]              nil)
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]          nil)
  (define-key map [menu-bar minibuf separator-C-l]                           nil)
  (define-key map [menu-bar minibuf alt-action-list-all]                     nil)
  (define-key map [menu-bar minibuf alt-action-all]                          nil)
  (define-key map [menu-bar minibuf action-list-all]                         nil)
  (define-key map [menu-bar minibuf action-all]                              nil)
  (define-key map [menu-bar minibuf separator-actions]                       nil)
  (define-key map [menu-bar minibuf set-define]                              nil)
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]            nil)
  (define-key map [menu-bar minibuf set-union]                               nil)
  (define-key map [menu-bar minibuf set-difference]                          nil)
  (define-key map [menu-bar minibuf set-intersection]                        nil)
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]       nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate] nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates]                nil)
  (define-key map [menu-bar minibuf icicle-widen-candidates]                 nil)
  (define-key map [menu-bar minibuf set-complement]                          nil)
  (define-key map [menu-bar minibuf separator-set1]                          nil)
  (define-key map [menu-bar minibuf set-swap]                                nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected] nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]      nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]          nil)
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]            nil)
  (define-key map [menu-bar minibuf set-retrieve-from-variable]              nil)
  (define-key map [menu-bar minibuf set-retrieve]                            nil)
  (define-key map [menu-bar minibuf set-save-to-cache-file]                  nil)
  (define-key map [menu-bar minibuf set-save-to-variable]                    nil)
  (define-key map [menu-bar minibuf set-save]                                nil)
  (define-key map [menu-bar minibuf separator-set2]                          nil)
  (define-key map [menu-bar minibuf word-complete]                           nil)
  (define-key map [menu-bar minibuf prefix-complete]                         nil)
  (define-key map [menu-bar minibuf apropos-complete]                        nil)
  (define-key map [menu-bar minibuf ?\?]
    '(menu-item "List Completions" minibuffer-completion-help
      :help "Display all possible completions"))
  (define-key map [menu-bar minibuf space]
    '(menu-item "Complete Word" minibuffer-complete-word :help "Complete at most one word"))
  (define-key map [menu-bar minibuf tab]
    '(menu-item "Complete" minibuffer-complete :help "Complete as far as possible"))

  ;; Unmap commands that were bound for completion.
  (icicle-unmap 'self-insert-command           map 'icicle-self-insert)
  (icicle-unmap 'universal-argument            map 'icicle-universal-argument)
  (icicle-unmap 'negative-argument             map 'icicle-negative-argument)
  (icicle-unmap 'digit-argument                map 'icicle-digit-argument)
  (icicle-unmap 'backward-delete-char-untabify map 'icicle-backward-delete-char-untabify)
  (icicle-unmap 'delete-backward-char          map 'icicle-delete-backward-char)
  (icicle-unmap 'delete-char                   map 'icicle-delete-char)
  (icicle-unmap 'backward-kill-word            map 'icicle-backward-kill-word)
  (icicle-unmap 'kill-word                     map 'icicle-kill-word)
  (icicle-unmap 'backward-kill-sexp            map 'icicle-backward-kill-sexp)
  (icicle-unmap 'kill-sexp                     map 'icicle-kill-sexp)
  (icicle-unmap 'backward-kill-sentence        map 'icicle-backward-kill-sentence)
  (icicle-unmap 'backward-kill-paragraph       map 'icicle-backward-kill-paragraph)
  (icicle-unmap 'kill-paragraph                map 'icicle-kill-paragraph)
  (icicle-unmap 'kill-line                     map 'icicle-kill-line)
  (icicle-unmap 'reposition-window             map 'icicle-goto/kill-failed-input)
  (icicle-unmap 'transpose-chars               map 'icicle-transpose-chars)
  (icicle-unmap 'transpose-words               map 'icicle-transpose-words)
  (icicle-unmap 'transpose-sexps               map 'icicle-transpose-sexps)
  (icicle-unmap 'yank-pop                      map 'icicle-yank-pop)
  (icicle-unmap 'mouse-yank-secondary          map 'icicle-mouse-yank-secondary)

  ;; Restore additional bindings.
  ;; Do the option keys first, so they can be rebound as needed.
  (dolist (key icicle-word-completion-keys)                   (define-key map key nil))
  (dolist (key icicle-apropos-complete-keys)                  (define-key map key nil))
  (dolist (key icicle-prefix-complete-keys)                   (define-key map key nil))
  (dolist (key icicle-apropos-complete-no-display-keys)       (define-key map key nil))
  (dolist (key icicle-prefix-complete-no-display-keys)        (define-key map key nil))

  (dolist (key icicle-prefix-cycle-previous-keys)             (define-key map key nil))
  (dolist (key icicle-prefix-cycle-next-keys)                 (define-key map key nil))
  (dolist (key icicle-apropos-cycle-previous-keys)            (define-key map key nil))
  (dolist (key icicle-apropos-cycle-next-keys)                (define-key map key nil))
  (dolist (key icicle-prefix-cycle-previous-action-keys)      (define-key map key nil))
  (dolist (key icicle-prefix-cycle-previous-alt-action-keys)  (define-key map key nil))
  (dolist (key icicle-prefix-cycle-next-action-keys)          (define-key map key nil))
  (dolist (key icicle-prefix-cycle-next-alt-action-keys)      (define-key map key nil))
  (dolist (key icicle-apropos-cycle-previous-action-keys)     (define-key map key nil))
  (dolist (key icicle-apropos-cycle-previous-alt-action-keys) (define-key map key nil))
  (dolist (key icicle-apropos-cycle-next-action-keys)         (define-key map key nil))
  (dolist (key icicle-apropos-cycle-next-alt-action-keys)     (define-key map key nil))
  (dolist (key icicle-modal-cycle-up-keys)                    (define-key map key nil))
  (dolist (key icicle-modal-cycle-down-keys)                  (define-key map key nil))
  (dolist (key icicle-modal-cycle-up-action-keys)             (define-key map key nil))
  (dolist (key icicle-modal-cycle-up-alt-action-keys)         (define-key map key nil))
  (dolist (key icicle-modal-cycle-down-action-keys)           (define-key map key nil))
  (dolist (key icicle-modal-cycle-down-alt-action-keys)       (define-key map key nil))
  (dolist (key icicle-modal-cycle-up-help-keys)               (define-key map key nil))
  (dolist (key icicle-modal-cycle-down-help-keys)             (define-key map key nil))
  (dolist (key icicle-prefix-cycle-previous-help-keys)        (define-key map key nil))
  (dolist (key icicle-prefix-cycle-next-help-keys)            (define-key map key nil))
  (dolist (key icicle-apropos-cycle-previous-help-keys)       (define-key map key nil))
  (dolist (key icicle-apropos-cycle-next-help-keys)           (define-key map key nil))

  (define-key map [(control help)]           nil)
  (define-key map [(control meta help)]      nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control meta f1)]        nil)
  (define-key map [(control meta return)]    nil)
  (define-key map [(meta return)]            nil)
  (define-key map "\C-\M-m"                  nil)
  (define-key map [(control shift return)]   nil)
  (define-key map [delete]                   nil)
  (define-key map [(shift delete)]           nil)
  (define-key map [(control ?w)]             nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [(control ?|)]             nil)
  (define-key map [(meta ?!)]                nil)
  (define-key map [(meta ?|)]                nil)
  (define-key map [(control meta ?/)]        nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [(control pause)]          nil)
  (define-key map [(control meta pause)]     nil)
  (define-key map [(control insert)]         nil)
  (define-key map [insert]                   nil)

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [(control ?a)]           nil)
    (define-key map [(control ?e)]           nil)
    (define-key map [(control ?=)]           nil)
    (define-key map [(meta ?i)]              nil)
    (define-key map [(meta ?k)]              nil)
    (define-key map [(meta ?o)]              nil)
    (define-key map [(meta ?.)]              nil)
    (define-key map [(meta ?:)]              nil)
    (define-key map "\C-\M-y"                nil)
    (define-key map [M-S-backspace]          nil)
    (define-key map [M-S-delete]             nil)
    )

  (define-key map [(meta ?q)]                nil)
  (define-key map [(control ?l)]             nil)
  (define-key map [(control shift ?l)]       nil)
  (define-key map [(meta ?$)]                nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control ?>)]             nil)
  (define-key map [(control meta ?>)]        nil)
  (define-key map [(control ?\()]            nil)
  (define-key map [(meta ?\()]               nil)
  (define-key map [(control ?\))]            nil)
  (define-key map [(control meta ?\))]       nil)
  (define-key map [(control meta ?<)]        nil)
  (define-key map [(control meta ?})]        nil)
  (define-key map [(control meta ?{)]        nil)
  (define-key map [(control ?})]             nil)
  (define-key map [(control ?{)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control meta ?j)]        nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ? )]             nil)
  (define-key map [(control meta ?\;)]       nil)
  (define-key map [(control ?`)]             nil)
  (define-key map [(control meta ?`)]        nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(control meta ?_)]        nil)
  (define-key map [(control ?$)]             nil)
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [(control ??)]           nil))
  (define-key map [(control ?.)]             nil)
  (define-key map [(control ?#)]             nil)
  (define-key map [(control ?\;)]            nil)
  (define-key map [(control meta ?\.)]       nil)
  (define-key map [(meta ?\;)]               nil)
  (define-key map [(control ?^)]             nil)
  (define-key map [(control shift ?a)]       nil)
  (define-key map [(meta ?~)]                nil)
  (define-key map [(meta ?g)]                nil)
  (define-key map [(meta ?,)]                nil)
  (define-key map [(control meta ?,)]        nil)
  (define-key map [(meta ?+)]                nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map [(meta ?&)]                nil)
  (define-key map [(meta ?_)]                nil)
  (define-key map [(control meta ?&)]        nil)
  (define-key map [(shift ?\ )]              nil)
  (define-key map [(shift backspace)]        nil)
  (define-key map "\C-v"                     nil)
  (define-key map "\M-v"                     nil)
  (define-key map "."                        nil)
  (define-key map "\M-m"                     nil)
  (define-key map "\C-x."                    nil)
  (when (fboundp 'doremi)
    (define-key map "\C-xw"                  nil)
    (define-key map "\C-x|"                  nil)
    (define-key map "\C-x#"                  nil)
    (when (fboundp 'text-scale-increase)
      (define-key map "\C-x-"                nil))
    (define-key map "\C-x1"                  nil)
    (define-key map "\C-x2"                  nil))
  ;; Do these last. -----------------
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map " "                        'minibuffer-complete-word)
  (define-key map [(control ?g)]             (if (and (fboundp 'minibuffer-keyboard-quit)
                                                      delete-selection-mode)
                                                 'minibuffer-keyboard-quit
                                               'abort-recursive-edit))
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map "\n"                     'exit-minibuffer))
  (define-key map [(meta ?p)]                'previous-history-element)
  (define-key map [(meta ?n)]                'next-history-element)
  (define-key map [up]                       'previous-history-element)
  (define-key map [down]                     'next-history-element)
  (define-key map [(meta ?v)]                'switch-to-completions)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [next]                     'next-history-element))

(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icicle-mode (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
    ;; The pre- and post-command hooks are local to the
    ;; minibuffer, so they are added here, not in `icicle-mode'.
    ;; They are removed in `icicle-mode' when mode is exited.
    (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook  'icicle-top-level-prep) ; This must not be LOCAL (nil LOCAL arg).
    (add-hook 'pre-command-hook  'icicle-run-icicle-pre-command-hook nil t)
    (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'icicle-run-icicle-post-command-hook nil t)
    ;; Change the region background here dynamically.  It would be better to
    ;; just use a buffer-local face, but those don't yet exist.
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background  (face-background 'region))
      (when icicle-change-region-background-flag
        (set-face-background 'region icicle-region-background)))
    ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
    ;; or `completing-read'.  Reset other stuff too.
    (setq icicle-candidate-nb                    nil
          icicle-completion-candidates           nil
          ;; This is so that cycling works right initially, without first hitting `TAB' or `S-TAB'.
          icicle-current-completion-mode         (and (< (minibuffer-depth) 2)
                                                      (case icicle-default-cycling-mode
                                                        ((nil)      nil)
                                                        (apropos    'apropos)
                                                        (prefix     'prefix)
                                                        (otherwise  nil)))
          icicle-next-apropos-complete-cycles-p  nil
          icicle-next-prefix-complete-cycles-p   nil
          icicle-default-directory               default-directory
          icicle-incremental-completion-p        icicle-incremental-completion-flag
          icicle-initial-value                   nil
          icicle-cmd-reading-input               this-command
          icicle-last-completion-command         nil
          icicle-last-completion-candidate       nil
          icicle-last-input                      nil
          icicle-input-fail-pos                  nil
          icicle-saved-proxy-candidates          nil
          ;; `other-buffer' doesn't work, because it looks for a buffer only from the same frame.
          icicle-pre-minibuffer-buffer           (cadr (buffer-list)) ; $$$$$$ (other-buffer nil t)
          )
    (when (and (icicle-completing-p) (> emacs-major-version 20))
      (let ((prompt-prefix   (if icicle-candidate-action-fn "+ " ". ")))
        (put-text-property 0 1 'face
                           (cond ((and icicle-candidate-action-fn (icicle-require-match-p))
                                  '(icicle-multi-command-completion icicle-mustmatch-completion))
                                 (icicle-candidate-action-fn 'icicle-multi-command-completion)
                                 ((icicle-require-match-p)
                                  '(icicle-completion icicle-mustmatch-completion))
                                 (t 'icicle-completion))
                           prompt-prefix)
        (if (overlayp icicle-completion-prompt-overlay)
            (move-overlay icicle-completion-prompt-overlay (point-min) (point-min))
          (setq icicle-completion-prompt-overlay  (make-overlay (point-min) (point-min))))
        (overlay-put icicle-completion-prompt-overlay 'before-string prompt-prefix)))
    (unless icicle-add-proxy-candidates-flag
      (setq icicle-saved-proxy-candidates  (prog1 icicle-proxy-candidates
                                             (setq icicle-proxy-candidates
                                                   icicle-saved-proxy-candidates))))
    (while icicle-saved-candidate-overlays
      (delete-overlay (car icicle-saved-candidate-overlays))
      (setq icicle-saved-candidate-overlays  (cdr icicle-saved-candidate-overlays)))
    (icicle-update-ignored-extensions-regexp)
    (when (memq icicle-default-value '(preselect-start preselect-end))
      (icicle-select-minibuffer-contents))
    (when (and icicle-show-Completions-initially-flag
               (not icicle-progressive-completing-p) ; If narrowed, then we have already completed.
               (icicle-completing-p)    ; Function initializes variable `icicle-completing-p'.
               (sit-for icicle-incremental-completion-delay)) ; Let user interrupt.
      (case icicle-default-cycling-mode
        (apropos    (icicle-apropos-complete))
        (otherwise  (icicle-prefix-complete)))) ; Prefix completion, by default.
    (run-hooks 'icicle-minibuffer-setup-hook)))

(defun icicle-define-cycling-keys (map)
  "Define keys for cycling candidates.
The modal keys are defined first, then the non-modal keys.
That means that in case of conflict mode-specific cyling wins.
For example, if you define both `icicle-modal-cycle-up-keys' and
`icicle-prefix-cycle-previous-keys' as ([up]), the latter gets the
binding."
  (cond (icicle-use-C-for-actions-flag  ; Use `C-' for actions, no `C-' for plain cycling.
         ;; Modal cycling keys.
         (dolist (key icicle-modal-cycle-up-keys)
           (define-key map key 'icicle-previous-candidate-per-mode)) ; `up'
         (dolist (key icicle-modal-cycle-down-keys)
           (define-key map key 'icicle-next-candidate-per-mode)) ; `down'
         (dolist (key icicle-modal-cycle-up-action-keys)
           (define-key map key 'icicle-previous-candidate-per-mode-action)) ; `C-up'
         (dolist (key icicle-modal-cycle-down-action-keys)
           (define-key map key 'icicle-next-candidate-per-mode-action)) ; `C-down'
         ;; Non-modal cycling keys.  In case of conflict, these will prevail over modal keys.
         (dolist (key icicle-prefix-cycle-previous-keys)
           (define-key map key 'icicle-previous-prefix-candidate)) ; `home'
         (dolist (key icicle-prefix-cycle-next-keys)
           (define-key map key 'icicle-next-prefix-candidate)) ; `end'
         (dolist (key icicle-apropos-cycle-previous-keys)
           (define-key map key 'icicle-previous-apropos-candidate)) ; `prior'
         (dolist (key icicle-apropos-cycle-next-keys)
           (define-key map key 'icicle-next-apropos-candidate)) ; `next'
         (dolist (key icicle-prefix-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-prefix-candidate-action)) ; `C-home'
         (dolist (key icicle-prefix-cycle-next-action-keys)
           (define-key map key 'icicle-next-prefix-candidate-action)) ; `C-end'
         (dolist (key icicle-apropos-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-apropos-candidate-action)) ; `C-prior'
         (dolist (key icicle-apropos-cycle-next-action-keys)
           (define-key map key 'icicle-next-apropos-candidate-action))) ; `C-next'

        (t                              ; Use `C-' for plain cycling, NO `C-' for action.
         ;; Modal cycling keys.  At least some of these will overwrite non-modal keys.
         (dolist (key icicle-modal-cycle-up-keys)
           (define-key map key 'icicle-previous-candidate-per-mode-action)) ; `up'
         (dolist (key icicle-modal-cycle-down-keys)
           (define-key map key 'icicle-next-candidate-per-mode-action)) ; `down'
         (dolist (key icicle-modal-cycle-up-action-keys)
           (define-key map key 'icicle-previous-candidate-per-mode)) ; `C-up'
         (dolist (key icicle-modal-cycle-down-action-keys)
           (define-key map key 'icicle-next-candidate-per-mode)) ; `C-down'
         ;; Non-modal cycling keys.  In case of conflict, these will prevail over modal keys.
         (dolist (key icicle-prefix-cycle-previous-keys)
           (define-key map key 'icicle-previous-prefix-candidate-action)) ; `home'
         (dolist (key icicle-prefix-cycle-next-keys)
           (define-key map key 'icicle-next-prefix-candidate-action)) ; `end'
         (dolist (key icicle-apropos-cycle-previous-keys)
           (define-key map key 'icicle-previous-apropos-candidate-action)) ; `prior'
         (dolist (key icicle-apropos-cycle-next-keys)
           (define-key map key 'icicle-next-apropos-candidate-action)) ; `next'
         (dolist (key icicle-prefix-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-prefix-candidate)) ; `C-home'
         (dolist (key icicle-prefix-cycle-next-action-keys)
           (define-key map key 'icicle-next-prefix-candidate)) ; `C-end'
         (dolist (key icicle-apropos-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-apropos-candidate)) ; `C-prior'
         (dolist (key icicle-apropos-cycle-next-action-keys)
           (define-key map key 'icicle-next-apropos-candidate))))

  ;; Help and alternative-action keys are NOT controlled by `icicle-use-C-for-actions-flag'.
  ;;
  ;; Define modal cycling help and alternative action keys.
  (dolist (key icicle-modal-cycle-up-help-keys)
    (define-key map key 'icicle-previous-candidate-per-mode-help)) ; `C-M-up'
  (dolist (key icicle-modal-cycle-down-help-keys)
    (define-key map key 'icicle-next-candidate-per-mode-help)) ; `C-M-down'
  (dolist (key icicle-modal-cycle-up-alt-action-keys)
    (define-key map key 'icicle-previous-candidate-per-mode-alt-action)) ; `C-S-up'
  (dolist (key icicle-modal-cycle-down-alt-action-keys)
    (define-key map key 'icicle-next-candidate-per-mode-alt-action)) ; `C-S-down'
  ;; Define non-modal cycling help and alternative action keys.
  (dolist (key icicle-prefix-cycle-previous-help-keys)
    (define-key map key 'icicle-help-on-previous-prefix-candidate)) ; `C-M-home'
  (dolist (key icicle-prefix-cycle-next-help-keys)
    (define-key map key 'icicle-help-on-next-prefix-candidate)) ; `C-M-end'
  (dolist (key icicle-apropos-cycle-previous-help-keys)
    (define-key map key 'icicle-help-on-previous-apropos-candidate)) ; `C-M-prior'
  (dolist (key icicle-apropos-cycle-next-help-keys)
    (define-key map key 'icicle-help-on-next-apropos-candidate)) ; `C-M-next'
  (dolist (key icicle-prefix-cycle-previous-alt-action-keys)
    (define-key map key 'icicle-previous-prefix-candidate-alt-action)) ; `C-S-home'
  (dolist (key icicle-prefix-cycle-next-alt-action-keys)
    (define-key map key 'icicle-next-prefix-candidate-alt-action)) ; `C-S-end'
  (dolist (key icicle-apropos-cycle-previous-alt-action-keys)
    (define-key map key 'icicle-previous-apropos-candidate-alt-action)) ; `C-S-prior'
  (dolist (key icicle-apropos-cycle-next-alt-action-keys)
    (define-key map key 'icicle-next-apropos-candidate-alt-action))) ; `C-S-next'

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (let ((min  (icicle-minibuffer-prompt-end)))
    (set-mark (if (eq 'preselect-start icicle-default-value) (point-max) min))
    (goto-char (if (eq 'preselect-start icicle-default-value) min (point-max)))))

;; $$$ (defadvice next-history-element (after icicle-select-minibuffer-contents activate)
;;   "Select minibuffer contents and leave point at its beginning."
;;   (when (and icicle-mode (memq icicle-default-value '(preselect-start preselect-end)))
;;     (icicle-select-minibuffer-contents)
;;     (setq deactivate-mark  nil)))

(defun icicle-cancel-Help-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window  (get-buffer-window "*Help*" 0))
         (help-frame   (and help-window (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion  this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p) ; Defined in `icicles-fn.el'.
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")

          ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
          ;; `completion-ignored-extensions' changes.
          icicle-ignored-extensions  completion-ignored-extensions)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when (and icicle-change-region-background-flag (= 1 (recursion-depth)))
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window))
             icicle-completing-p
             (not executing-kbd-macro))
    (setq deactivate-mark  nil)))

(defun icicle-redefine-standard-commands ()
  "Replace some standard Emacs functions and menus with Icicles versions.
No such replacement is done if option
`icicle-redefine-standard-commands-flag' is nil."
  (when (and (fboundp 'icicle-completing-read) icicle-redefine-standard-commands-flag)
    (when (fboundp 'old-bbdb-complete-name)
      (defalias 'bbdb-complete-name                   'icicle-bbdb-complete-name))
    (when (fboundp 'old-comint-dynamic-complete)
      (defalias 'comint-dynamic-complete              'icicle-comint-dynamic-complete))
    (when (fboundp 'old-comint-dynamic-complete-filename)
      (defalias 'comint-dynamic-complete-filename     'icicle-comint-dynamic-complete-filename))
    (when (fboundp 'old-comint-replace-by-expanded-filename)
      (defalias 'comint-replace-by-expanded-filename  'icicle-comint-replace-by-expanded-filename))
    (when (fboundp 'old-ess-complete-object-name)
      (defalias 'ess-complete-object-name             'icicle-ess-complete-object-name))
    (when (fboundp 'old-gud-gdb-complete-command)
      (defalias 'gud-gdb-complete-command             'icicle-gud-gdb-complete-command))
    (when (fboundp 'old-dired-read-shell-command)
      (defalias 'dired-read-shell-command             'icicle-dired-read-shell-command))
    (when (fboundp 'old-read-shell-command)
      (defalias 'read-shell-command                   'icicle-read-shell-command))
    (when (fboundp 'old-recentf-make-menu-items)
      (defalias 'recentf-make-menu-items              'icicle-recentf-make-menu-items))
    (defalias 'customize-apropos                      'icicle-customize-apropos)
    (defalias 'customize-apropos-faces                'icicle-customize-apropos-faces)
    (defalias 'customize-apropos-groups               'icicle-customize-apropos-groups)
    (defalias 'customize-apropos-options              'icicle-customize-apropos-options)
    (when (fboundp 'old-customize-apropos-options-of-type)
      (defalias 'customize-apropos-options-of-type    'icicle-customize-apropos-options-of-type))
    (defalias 'customize-face                         'icicle-customize-face)
    (defalias 'customize-face-other-window            'icicle-customize-face-other-window)
    (defalias 'dabbrev-completion                     'icicle-dabbrev-completion)
    (if (fboundp 'completion-at-point)  ; Emacs 23.2 - return fn that does all of the completion.
        (defun lisp-completion-at-point () #'icicle-lisp-complete-symbol)
      (defalias 'lisp-complete-symbol                 'icicle-lisp-complete-symbol))
    (when (fboundp 'old-minibuffer-default-add-completions)
      (defalias 'minibuffer-default-add-completions   'icicle-minibuffer-default-add-completions))
    (defalias 'read-from-minibuffer                   'icicle-read-from-minibuffer)
    (defalias 'read-string                            'icicle-read-string)
    (defalias 'repeat-complex-command                 'icicle-repeat-complex-command)
    ))

(defun icicle-restore-standard-commands ()
  "Restore standard Emacs functions replaced in Icicle mode.
See `icicle-redefine-standard-commands'."
  (when (and (fboundp 'old-completing-read) icicle-redefine-standard-commands-flag)
    (when (fboundp 'old-bbdb-complete-name)
      (defalias 'bbdb-complete-name                   'old-bbdb-complete-name))
    (when (fboundp 'old-comint-dynamic-complete)
      (defalias 'comint-dynamic-complete              'old-comint-dynamic-complete))
    (when (fboundp 'old-comint-dynamic-complete-filename)
      (defalias 'comint-dynamic-complete-filename     'old-comint-dynamic-complete-filename))
    (when (fboundp 'old-comint-replace-by-expanded-filename)
      (defalias 'comint-replace-by-expanded-filename  'old-comint-replace-by-expanded-filename))
    (when (fboundp 'old-ess-complete-object-name)
      (defalias 'ess-complete-object-name             'old-ess-complete-object-name))
    (when (fboundp 'old-gud-gdb-complete-command)
      (defalias 'gud-gdb-complete-command             'old-gud-gdb-complete-command))
    (when (fboundp 'old-dired-read-shell-command)
      (defalias 'dired-read-shell-command             'old-dired-read-shell-command))
    (when (fboundp 'old-read-shell-command)
      (defalias 'read-shell-command                   'old-read-shell-command))
    (when (fboundp 'old-recentf-make-menu-items)
      (defalias 'recentf-make-menu-items              'old-recentf-make-menu-items))
    (defalias 'customize-apropos                      'old-customize-apropos)
    (defalias 'customize-apropos-faces                'old-customize-apropos-faces)
    (defalias 'customize-apropos-groups               'old-customize-apropos-groups)
    (defalias 'customize-apropos-options              'old-customize-apropos-options)
    (when (fboundp 'old-customize-apropos-options-of-type)
      (defalias 'customize-apropos-options-of-type    'old-customize-apropos-options-of-type))
    (defalias 'customize-face                         'old-customize-face)
    (defalias 'customize-face-other-window            'old-customize-face-other-window)
    (defalias 'dabbrev-completion                     'old-dabbrev-completion)
    (if (fboundp 'completion-at-point)  ; Emacs 23.2.  It no longer uses `lisp-complete-symbol'.
        (defalias 'lisp-completion-at-point           'old-lisp-completion-at-point)
      (defalias 'lisp-complete-symbol                 'old-lisp-complete-symbol))
    (when (fboundp 'old-minibuffer-default-add-completions)
      (defalias 'minibuffer-default-add-completions   'old-minibuffer-default-add-completions))
    (defalias 'read-from-minibuffer                   'old-read-from-minibuffer)
    (defalias 'read-string                            'old-read-string)
    (defalias 'repeat-complex-command                 'old-repeat-complex-command)
    ))

;;; In Emacs versions before 22:
;;; Save original `read-file-name'.  We redefine it as `icicle-read-file-name' (which calls it).
;;; Then we restore it when you quit Icicle mode.  (In Emacs 22+, no redefinition is needed.)
(unless (or (boundp 'read-file-name-function) (fboundp 'orig-read-file-name))
(defalias 'orig-read-file-name (symbol-function 'read-file-name)))

(defun icicle-redefine-std-completion-fns ()
  "Replace some standard functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'choose-completion            'icicle-choose-completion)
    (defalias 'choose-completion-string     'icicle-choose-completion-string)
    (defalias 'completing-read              'icicle-completing-read)
    (defalias 'completion-setup-function    'icicle-completion-setup-function)
    (unless (> emacs-major-version 22)
      (defalias 'dired-smart-shell-command  'icicle-dired-smart-shell-command))
    (defalias 'display-completion-list      'icicle-display-completion-list)
    (defalias 'exit-minibuffer              'icicle-exit-minibuffer)
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'icicle-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'icicle-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'icicle-mouse-choose-completion)
    (defalias 'next-history-element         'icicle-next-history-element)
    (defalias 'read-face-name               'icicle-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq icicle-old-read-file-name-fn  (prog1 (and (not (eq read-file-name-function
                                                                 'icicle-read-file-name))
                                                        read-file-name-function)
                                              (setq read-file-name-function
                                                    'icicle-read-file-name)))
      (defalias 'read-file-name             'icicle-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'icicle-read-number)
      (defalias 'read-number                'icicle-read-number))
    (unless (> emacs-major-version 22)
      (defalias 'shell-command              'icicle-shell-command))
    (unless (> emacs-major-version 22)
      (defalias 'shell-command-on-region    'icicle-shell-command-on-region))
    (defalias 'switch-to-completions        'icicle-switch-to-completions)
    (when (fboundp 'icicle-completing-read-multiple)
      (defalias 'completing-read-multiple   'icicle-completing-read-multiple)
      (setq crm-local-completion-map  icicle-crm-local-completion-map
            crm-local-must-match-map  icicle-crm-local-must-match-map))
    (when (> emacs-major-version 22)
      (defalias 'sit-for                    'icicle-sit-for))
    ))

(defun icicle-restore-std-completion-fns ()
  "Restore some standard functions that were replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'choose-completion            'old-choose-completion)
    (defalias 'choose-completion-string     'old-choose-completion-string)
    (defalias 'completing-read              'old-completing-read)
    (defalias 'completion-setup-function    'old-completion-setup-function)
    (when (fboundp 'old-dired-smart-shell-command) ; Emacs 23
      (defalias 'dired-smart-shell-command  'old-dired-smart-shell-command))
    (defalias 'display-completion-list      'old-display-completion-list)
    (defalias 'exit-minibuffer              'old-exit-minibuffer)
    (when (fboundp 'old-face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'old-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'old-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'old-mouse-choose-completion)
    (defalias 'next-history-element         'old-next-history-element)
    (defalias 'read-face-name               'old-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq read-file-name-function  (and (not (eq icicle-old-read-file-name-fn
                                                     'icicle-read-file-name))
                                            icicle-old-read-file-name-fn))
      (defalias 'read-file-name             'orig-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'old-read-number)
      (defalias 'read-number                'old-read-number))
    (when (fboundp 'old-shell-command) ; Emacs 23
      (defalias 'shell-command              'old-shell-command))
    (when (fboundp 'old-shell-command-on-region) ; Emacs 23
      (defalias 'shell-command-on-region    'old-shell-command-on-region))
    (defalias 'switch-to-completions        'old-switch-to-completions)
    (when (fboundp 'old-completing-read-multiple)
      (defalias 'completing-read-multiple   'old-completing-read-multiple)
      (setq crm-local-completion-map  old-crm-local-completion-map
            crm-local-must-match-map  old-crm-local-must-match-map))
    (when (> emacs-major-version 22)
      (defalias 'sit-for                    'old-sit-for))
    ))

;; Free vars here: `icicle-saved-kmacro-ring-max' is bound in `icicles-var.el'.
(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max         search-ring-max ; Save it.
          search-ring-max                      icicle-search-ring-max
          icicle-saved-regexp-search-ring-max  regexp-search-ring-max ; Save it.
          regexp-search-ring-max               icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max)
    (setq icicle-saved-kmacro-ring-max  kmacro-ring-max ; Save it.
          kmacro-ring-max               icicle-kmacro-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max         icicle-saved-search-ring-max
          regexp-search-ring-max  icicle-saved-regexp-search-ring-max)))

;; This is used only in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))


;;; Save original functions, so they can be restored when leave Icicle mode.
;;; Toggle Icicle mode after loading the library (and `icicles-mode.el'),
;;; to pick up the original definition.
;;;
;;; Note: The `boundp' test for `icicle-mode' is just in case the form gets evaluated while
;;; loading `icicles-mode.el' (e.g. the library gets loaded while loading `icicles-mode.el').

;;; `comint.el' - `comint-dynamic-complete', `comint-replace-by-expanded-filename'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'comint-dynamic-complete)
                          (not (fboundp 'old-comint-dynamic-complete)))
                 (defalias 'old-comint-dynamic-complete (symbol-function 'comint-dynamic-complete)))
               (when (and (fboundp 'comint-replace-by-expanded-filename)
                          (not (fboundp 'old-comint-replace-by-expanded-filename)))
                 (defalias 'old-comint-replace-by-expanded-filename
                     (symbol-function 'comint-replace-by-expanded-filename)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'comint) (eval-after-load "icicles-mode" form) (eval-after-load "comint" form)))

;;; `ess-site.el' - `ess-complete-object-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'ess-complete-object-name)
                          (not (fboundp 'old-ess-complete-object-name)))
                 (defalias 'old-ess-complete-object-name (symbol-function
                                                          'ess-complete-object-name)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'ess-site) (eval-after-load "icicles-mode" form) (eval-after-load "ess-site" form)))

;;; `gud.el' - `gud-gdb-complete-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'gud-gdb-complete-command)
                          (not (fboundp 'old-gud-gdb-complete-command)))
                 (defalias 'old-gud-gdb-complete-command (symbol-function
                                                          'gud-gdb-complete-command)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'gud) (eval-after-load "icicles-mode" form) (eval-after-load "gud" form)))

;;; `bbdb-com.el' -  `bbdb-complete-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'bbdb-complete-name) (not (fboundp 'old-bbdb-complete-name)))
                 (defalias 'old-bbdb-complete-name (symbol-function 'bbdb-complete-name)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'bbdb-com) (eval-after-load "icicles-mode" form) (eval-after-load "bbdb-com" form)))

;;; `dired-aux.el' - `dired-read-shell-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'dired-read-shell-command)
                          (not (fboundp 'old-dired-read-shell-command)))
                 (defalias 'old-dired-read-shell-command (symbol-function
                                                          'dired-read-shell-command)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'dired-aux)
      (eval-after-load "icicles-mode" form)
    (eval-after-load "dired-aux" form)))

;;; `dired-x.el' - `dired-read-shell-command', `dired-smart-shell-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'dired-read-shell-command)
                          (not (fboundp 'old-dired-read-shell-command)))
                 (defalias 'old-dired-read-shell-command (symbol-function
                                                          'dired-read-shell-command)))
               (unless (fboundp 'read-shell-command) ; `dired-smart-shell-command' in Emacs < 23.
                 (when (and (fboundp 'dired-smart-shell-command)
                            (not (fboundp 'old-dired-smart-shell-command)))
                   (defalias 'old-dired-smart-shell-command (symbol-function
                                                             'dired-smart-shell-command))))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'dired-x) (eval-after-load "icicles-mode" form) (eval-after-load "dired-x" form)))

;;; `simple.el' - `read-shell-command' - Emacs 23+.
(when (> emacs-major-version 22)
  ;; `simple.el' is preloaded for Emacs 23+, so just do it now.
  (let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
    (when icyp (icicle-mode -1))
    (when (and (fboundp 'read-shell-command) (not (fboundp 'old-read-shell-command)))
      (defalias 'old-read-shell-command (symbol-function 'read-shell-command)))
    (when icyp (icicle-mode 1))))

;;; `recentf.el' - `recentf-make-menu-items'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode) icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'recentf-make-menu-items)
                          (not (fboundp 'old-recentf-make-menu-items)))
                 (defalias 'old-recentf-make-menu-items (symbol-function 'recentf-make-menu-items)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'recentf) (eval-after-load "icicles-mode" form) (eval-after-load "recentf" form)))

      
;; Do this last.
;;
;; When these libraries are first loaded, toggle Icicle mode to pick up the definitions
(dolist (library '("bookmark+" "buff-menu" "comint" "dired" "ess-site" "gud" "ibuffer"
                   "idlw-shell"         ; (untested - I don't have an `idl' program)
                   "ielm" "info" "net-utils" "rlogin" "shell" "sh-script" "tcl"))
  (unless (if (fboundp 'load-history-regexp) ; Emacs 22+
              (load-history-filename-element (load-history-regexp library))
            (assoc library load-history))
    (eval-after-load library '(icicle-toggle-icicle-mode-twice))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here

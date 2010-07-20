;;; icicles-chg.el --- Change logs for Icicles libraries.
;;
;; Filename: icicles-chg.el
;; Description: Change logs for Icicles libraries.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2007-2009, Drew Adams, all rights reserved.
;; Created: Tue Nov 27 07:47:53 2007
;; Version: 22.0
;; Last-Updated: Fri Jul  2 15:40:09 2010 (-0700)
;;           By: dradams
;;     Update #: 5556
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-chg.el
;; Keywords: extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Change logs for Icicles libraries.
;;
;;  This file consolidates the change logs for all Icicles libraries.
;;  It contains no code.
;;
;;  Libraries `icicles-doc1.el' and `icicles-doc2.el' contain only
;;  documentation, and they do not have change logs.  Initially,
;;  everything was in one library, `icicles.el', so its change log is
;;  the oldest.
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
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "CHANGE LOG FOR `icicles-cmd1.el'")
;;  (@> "CHANGE LOG FOR `icicles-cmd2.el'")
;;  (@> "CHANGE LOG FOR `icicles-face.el'")
;;  (@> "CHANGE LOG FOR `icicles-fn.el'")
;;  (@> "CHANGE LOG FOR `icicles-mac.el'")
;;  (@> "CHANGE LOG FOR `icicles-mcmd.el'")
;;  (@> "CHANGE LOG FOR `icicles-mode.el'")
;;  (@> "CHANGE LOG FOR `icicles-opt.el'")
;;  (@> "CHANGE LOG FOR `icicles-var.el'")
;;  (@> "CHANGE LOG FOR `icicles.el'")
;;  (@> "CHANGE LOG FOR `icicles-cmd.el'" - Deprecated file)
 
;;;(@* "CHANGE LOG FOR `icicles-cmd1.el'")
;;
;; 2010/07/02 dadams
;;     icicle-bookmark-set: Added INTERACTIVEP arg.  Prompt for tags when bmkp-prompt-for-tags-flag.
;;                          Highlight bookmark if bmkp-auto-light-when-set.
;;     icicle-bookmark-cmd: Call icicle-bookmark-set with new INTERACTIVEP arg.
;; 2010/06/25 dadams
;;     icicle-find-file(-other-window):
;;       Use default-directory, not nil, as third arg to read-file-name.  Thx to Thomas Lim.
;;       Note: This more or less reverts a change I made (why?) on 2008/12/27 (see that, below).
;; 2010/06/18 dadams
;;     Renamed: bookmarkp-* to bmkp-*.
;; 2010/06/11 dadams
;;     icicle-find-file-absolute*, icicle-recent-file*, icicle-locate-file-1:
;;       Bind C-c + to icicle-make-directory.
;;     icicle-find-file-in-tags-table*: Removed bindings: C-x m, C-backspace.
;; 2010/06/10 dadams
;;     icicle-cd-for-abs-files: Bound enable-recursive-minibuffers to t.
;; 2010/06/08 dadams
;;     Added: icicle-bookmark-delete-action: Make it refresh the cache.
;;     icicle-bookmark(-list|-other-window), icicle-define-bookmark-command-1:
;;       Use icicle-bookmark-delete-action.
;;       Use condition-case to filter out bad bookmarks (so no error).  Thx to M. Heerdegen.
;; 2010/06/04 dadams
;;     Applied renamings of doremi commands (added +).
;;     icicle-(buffer|*file|directory)*: Updated doc string to mention icicle-(buffers|files)-ido-like.
;; 2010/05/30 dadams
;;     Added: icicle-locate-file-no-symlinks(-other-window), icicle-locate-file(-other-window)-action,
;;            icicle-locate-file-1.
;;     icicle-locate-file*: Moved body to icicle-locate-file-1.  Use named action functions.
;;                          Respect icicle-ignored-directories (via *-files-within).
;;     icicle-cd-for-loc-files: Added optional arg NO-SYMLINKS-P.
;;     icicle-dired-saved-file-candidates(-other-window): Handle multi-completion candidates.
;;     Thx to M. Heerdegen.
;; 2010/05/28 dadams
;;     icicle-(recent|locate)-file(-other-window), 
;;       Use *-transform-multi-completion in *-all-candidates-list-alt-action-fn.  Thx to M. Heerdegen.
;; 2010/05/27 dadams
;;     icicle-cd-for-loc-files: Wrap interactive spec in save-selected-window.  Thx to M. Heerdegen.
;; 2010/05/26 dadams
;;     icicle-bookmark-set: Removed pseudo-default.  Thx to Michael Heerdegen.
;; 2010/05/24 dadams
;;     icicle-comint-replace-orig-completion-fns: Rewrote to not use case.  Thx to Michael Heerdegen.
;; 2010/05/21 dadams
;;     icicle-bookmark-help-string:
;;       Use BOOKMARK-NAME, not BMK, as arg to vanilla fns, for Emacs < 23.  Thx to Alexander Haeckel.
;; 2010-05-18 dadams
;;     Added: icicle-cd-for-abs-files, icicle-cd-for-loc-files.
;;     icicle-find-file-absolute*, icicle-locate-file*: Bind C-c C-d to icicle-cd-for-(abs|loc)-files.
;;     icicle-locate-file*: Make icicle-list-use-nth-parts be nil if no non-positive prefix arg.
;; 2010/05/16 dadams
;;     icicle-define-bookmark-command-1: Defined command requires bookmark+.el.
;;     Added: icicle-bookmark-specific-(buffers|files)-narrow, icicle-bookmark-this-buffer-narrow.
;;     icicle-bookmark(-other-window), icicle-bookmark-set:
;;       Bound C-M-= (b|f), C-M-. for narrowing to specific-(buffers|files), this-buffer.
;;     icicle-bookmark-cleanup: Updated for added narrowing keys.
;; 2010/05/15 dadams
;;     Added: icicle-define-bookmark-command(-1).  Added ARGS param for use as arg for *-alist-only.
;;     Added same-window commands for bookmark jumping.
;;     Added: *-specific-(buffers|files)*, *-this-buffer*, *-(all|some)-tags(-regexp)* jump commands,
;;            *-bookmarked-(buffer|file)-list.
;;     icicle-bookmark-set: *-selected-buffers-alist-only -> *-specific-buffers-alist-only.
;; 2010/05/06 dadams
;;     icicle-bookmark-set: Removed spurious format call with its arg.
;; 2010/04/29 dadams
;;     icicle-explore: Bind icicle-remove-icicles-props-p to nil around call to completing-read.
;;                     Call icicle-unpropertize at the end (without that binding).
;; 2010/04/20 dadams
;;     icicle-dired(-other-window): Use icicle-dirs-first-p, not icicle-dirs-last-p, and don't reverse.
;; 2010/04/17 dadams
;;     Added: icicle-bookmark-set.
;;     icicle-bookmark-cmd: Use icicle-bookmark-set, not bookmark-set.  Simpler PARG code in that case.
;;     icicle-bookmark(-list|-other-window), icicle-define-bookmark-other-window-command:
;;       Use icicle-transform-multi-completion in icicle-delete-candidate-object.
;; 2010/04/10 dadams
;;     Corrected prefix arg use for icicle-find-file(-read-only)(-other-window).  Thx to M. Heerdegen.
;; 2010/04/09 dadams
;;     Added: icicle-find-file-read-only(-other-window).
;;     icicle-find-file(-other-window):
;;       Prefix arg on individual candidate means visit read-only.
;;       Prefix arg for the command means reverse the prefix arg effect: read-only for all by default.
;; 2010/04/02 dadams
;;     Added: icicle-bookmark-list.
;;     icicle-bookmark-cleanup: Clean up both minibuffer maps.
;; 2010/03/27 dadams
;;     icicle-define-bookmark-other-window-command: Use optional PROMPT arg.
;; 2010/03/19 dadams
;;     icicle-define-bookmark-other-window-command: Rich multi-completions per icicle-bookmark.
;; 2010/03/16 dadams
;;     icicle-bookmark(-other-window):
;;       Use tags, not just file names, in the multi-completions.
;;       Use the default join string, ^G^J, and end string, ^J^J.
;;       Use just icicle-candidate-properties-alist to highlight the file name.
;;       Remove extra quote marks from face names.
;; 2010/03/14 dadams
;;     icicle-bookmark(-other-window):
;;       Copy file/buffer name string for candidate-part face, so we don't touch bookmark-alist.
;;       Handle icicle-bookmark-refresh-cache-flag and C-u.
;;       Bind the narrow keys in minibuffer-local-completion also (lax for multi-completion).
;;       Use face file-name-shadow, not icicle-candidate-part (too distracting).
;;     icicle-bookmark-*-narrow: Use icicle-get-alist-candidate-function.
;;     icicle-define-bookmark-other-window-command: Rewrote to handle multi-completion.
;;     icicle-bookmark-non-file-narrow: Use bookmarkp-non-file-bookmark-p.
;;     icicle-bookmark-propertize-candidate: Typo in a face name.
;; 2010/03/13 dadams
;;     icicle-bookmark(-other-window):
;;       If Bookmark+ available then, depending on icicle-show-multi-completion-flag, use a
;;         multi-completion showing file or buffer.  Use icicle-candidates-alist to do that.
;;       Don't put icicle-fancy-candidates prop on prompt.
;;       Don't use icicle-sorted-bookmark-alist (removed).  Recompute each time.
;; 2010/03/10 dadams
;;     icicle-bookmark-help-string:
;;       Mention the type in the help: sequence, function, bookmark list, desktop, etc.
;;       Don't show position for types that don't use one.  Don't show file for buffer & Dired.
;; 2010/03/09 dadams
;;     icicle-color-theme: Initialize variable icicle-color-themes here, not in icicles-opt.el.
;; 2010/03/04 dadams
;;     icicle-bookmark(-other-window): Use bookmarkp-describe-bookmark(-internals) for C-M-RET.
;; 2010/03/03 dadams
;;     icicle-bookmark(-other-window), icicle-define-bookmark-other-window-command:
;;       Use bookmarkp-sort-and-remove-dups.
;;       Bind icicle-sort-orders-alist, using bookmarkp predicates.
;;     icicle-bookmark(-other-window):
;;       Set icicle-sorted-bookmark-alist and bookmarkp-sorted-alist.
;;       Use bookmarkp-sorted-alist.
;;       Don't append original icicle-sort-orders-alist.  Just include a couple of its entries.
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer,
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist.
;; 2010/02/28 dadams
;;     icicle-send-bug-report: Formatted body a bit.
;; 2010/02/14 dadams
;;     Added: icicle-bookmark-bookmark-list-other-window, icicle-bookmark-bookmark-list-narrow.
;;     icicle-bookmark(-other-window):
;;       Bound to C-M- prefix: icicle-bookmark-bookmark-list-narrow.  Updated doc string.
;; 2010/02/13 dadams
;;     Added: icicle-bookmark-(desktop|man)-other-window,
;;            icicle-bookmark-(dired|desktop|man)-narrow.
;;     icicle-define-bookmark-other-window-command: Raise error if bookmark+.el not found.
;;     icicle-bookmark(-other-window):
;;       Bound to C-M- prefix: icicle-bookmark-(dired|desktop|man)-narrow.
;;     icicle-bookmark-propertize-candidate:
;;       Handle also: sequence, function, bookmark list, desktop, man, buffer, bad bookmarks.
;; 2010/02/02 dadams
;;     icicle-bookmark-jump-1: Don't select minibuffer window and give it focus.
;; 2010/01/30 dadams
;;     icicle-dired(-other-window), icicle-(find|recent|locate)-file(-absolute)(-other-window)
;;       icicle-find-file-in-tags-table(-other-window):
;;         Bind icicle-all-candidates-list-alt-action-fn to open Dired on matching files.
;; 2010/01/13 dadams
;;     icicle-recent-file(-other-window):
;;       Restore C-S-RET as icicle-remove-from-recentf-candidate-action (accidentally removed).
;; 2010/01/12 dadams
;;     Added: icicle-pp-display-expression.
;;     icicle-pp-eval-expression: Use icicle-pp-display-expression.
;;     icicle-bbdb-complete-name: save-excursion + set-buffer -> with-current-buffer.
;; 2009/12/21 dadams
;;     fset -> defalias.
;; 2009/12/13 dadams
;;     Added: icicle-bookmark-dired-other-window, icicle-dired(-other-window).
;;     *-buffer*, *-dired-project*, *-find-file(-absolute)*, *-(recent|locate)-file*,
;;       *-find-file-in-tags-table*:
;;         Bind C-x m to icicle-bookmark-(non-file|dired|file)-other-window.
;; 2009/11/27 dadams
;;     Added: icicle(-doremi)-increment-(variable|option).
;; 2009/11/25 dadams
;;     icicle-color-theme: Raise error for empty input.  Thx to Ahei.
;; 2009/11/24 dadams
;;     icicle-color-theme: Take a snapshot each time invoked, unless prefix arg.
;; 2009/11/22 dadams
;;     icicle-color-theme: Use color-theme-initialize instead of load-library, to load themes.
;; 2009/11/21 dadams
;;     icicle-color-theme: Use color-theme-snapshot to let C-g undo changes.
;;                         Try to load color-theme-library.el (available with version 6.6.0).
;; 2009/11/17 dadams
;;     icicle-bbdb-complete-name: Bind completion-case-ignore.
;; 2009/11/14 dadams
;;     icicle-bbdb-complete-name: Replace macro bbdb-hashtable by its expansion.
;; 2009/09/21 dadams
;;     icicle-lisp-complete-symbol:
;;       Complete symbol in buffer as far as possible first.  Show completions initially.
;; 2009/09/17 dadams
;;     icicle-delete-file, icicle-(file|directory)-list,
;;       icicle-find-file(-absolute)(-other-window),
;;       icicle-(recent|locate)-file(-other-window),
;;       icicle-find-file-in-tags-table(-other-window): Use icicle-file-bindings (new macro).
;; 2009/09/16 dadams
;;     Added: icicle-insert-buffer.
;;     icicle-kill-buffer, icicle-buffer(-other-window), icicle-add-buffer-candidate:
;;       Use icicle-buffer-bindings (new macro).
;; 2009/08/29 dadams
;;     Added: icicle-define-bookmark-other-window-command, icicle-select-bookmarked-region,
;;            icicle-bookmark(-region|-info|-gnus|-w3m|(-non|-local|-remote)-file)-other-window.
;;     icicle-bookmark-propertize-candidate: Updated to reflect renamed bookmark+.el face names.
;;     icicle-bookmark-file-narrow: Use bookmarkp-file-bookmark-p.
;;     (lambda...) -> #'(lambda...).
;; 2009/08/25 dadams
;;     Added: icicle-bookmark-cleanup-on-quit.
;;     icicle-bookmark(-other-window): Use icicle-bookmark-cleanup-on-quit.
;;     icicle-bookmark-cleanup: Removed code to return to original window a focus minibuffer.
;; 2009/08/24 dadams
;;     Added: icicle-bookmark-propertize-candidate.
;;     icicle-bookmark(-other-window): Put faces on bookmark candidates according to type.
;; 2009/08/23 dadams
;;     Added: icicle-bookmark-(region|info|gnus|w3m|(non-|local-|remote-)file)-narrow.
;;     icicle-bookmark(-other-window): Bind keys to bookmark candidate narrowing commands.
;;     icicle-bookmark-cleanup: Unbind the same commands.
;;     icicle-bookmark-cmd:
;;       Let bookmark+.el (latest version) handle prompting for name, even for region bookmark.
;;       Remove any newlines in bookmark name, when no prompting (thx to Thierry Volpiatto).
;; 2009/08/21 dadams
;;     icicle-bookmark-jump-1: Typo.
;; 2009/08/20 dadams
;;     icicle-bookmark-cmd: Use icicle-bookmark-other-window, not icicle-bookmark.
;;                          Fix trimmed-name: no longer than def-name.
;;     icicle-bookmark(-other-window): Bind enable-recursive-minibuffers, in case need to read.
;;     icicle-bookmark-jump-1: No crosshair highlighting for region bookmarks.
;; 2009/08/19 dadams
;;     Added: icicle-bookmark-help-string.
;;     icicle-bookmark-cmd: Handle creation of region bookmarks (new default values).
;;                          icicle-bookmark-name-length-max now applies to whole bookmark name.
;;     icicle-bookmark(-other-window):
;;       Apply icicle-candidate-short-help to candidates.  Use also for the help function.
;;     icicle-bookmark-jump-1: Use bookmark--jump-via if it is defined.
;; 2009/06/21 dadams
;;     icicle-bookmark-jump-1: Removed temporary Emacs 23 workaround for (fixed) bug #1175.
;; 2009/06/07 dadams
;;     icicle-get-alist-candidate -> funcall icicle-get-alist-candidate-function.
;; 2009/05/22 dadams
;;     Require icicles-mac.el if load-library doesn't find it.
;;     Created - Split off from icicles-cmd.el.
 
;;;(@* "CHANGE LOG FOR `icicles-cmd2.el'")
;;
;; 2010/06/21 dadams
;;     icicle-comint-search: Wrap search with unwind-protect to remove hook.  Thx to M. Heerdegen.
;; 2010/06/18 dadams
;;     icicle-search-replace-match: Specific error message for read-only buffer.  Thx to M. Heerdegen.
;;     Renamed: bookmarkp-* to bmkp-*.
;; 2010/06/12 dadams
;;     icicle-goto-marker-or-set-mark-command:
;;       Set this-command so C-SPC C-SPC activates when not t-m mode.  Thx to Chris Hecker.
;; 2010/06/11 dadams
;;     icicle-search-action-1: (unless (pos-visible-in-window-p) (recenter -2)).  Thx to M. Heerdegen.
;; 2010/06/08 dadams
;;     icicle-search-bookmark, icicle-define-search-bookmark-command:
;;       Use icicle-bookmark-delete-action.
;;       Use condition-case to filter out bad bookmarks (so no error).
;; 2010/06/04 dadams
;;     Added: icicle-ido-like-mode.
;; 2010/05/16 dadams
;;     Added: icicle-search-(all|some)-tags(-regexp)-bookmark, icicle-search-desktop-bookmark,
;;            icicle-search-specific-(buffers|files)-bookmark, icicle-search-this-buffer-bookmark.
;;     icicle-define-search-bookmark-command: Defined cmd requires bookmark+.el.  Added &rest arg ARGS.
;; 2010/05/15 dadams
;;     icicle-search-highlight-and-maybe-replace: If candidate number is nil, set to 0.
;;     icicle-search: Updated doc string: Can sort.  Don't say not to use C-|.
;; 2010/05/09 dadams
;;     Icicles search: Change to allow sorting.  Use mctized candidates, not alist + candidate nb.
;;       icicle-search-highlight-and-maybe-replace:
;;         Use *-replace-cand-in-mct, not *-replace-cand-in-alist.  Do not *-input-matches-here.
;;       icicle-search-replace-cand-in-mct: Rewrote.
;;         Handle multi-completions also.  Update CAND+MRKER to use replacement string, but keep props.
;;       icicle-search-in-context-default-fn: Rewrote.
;;         Update icicle-last-completion-candidate to current candidate.  Insert that.
;;         Recompute input-match highlighting in current context.  Remove current if no candidates.
;;       icicle-search-action-1: Do nothing if no candidates.
;;       icicle-search-help: Bind icicle-whole-candidate-as-text-prop-p to t, not nil.
;; 2010/05/04 dadams
;;     icicle-object-action: Pass TYP to icicle-apply-to-saved-candidate. (UNTESTED)
;;     icicle-choose-anything-candidate: Pass TYPE to icicle-apply-to-saved-candidate. (UNTESTED)
;;     icicle-apply, icicle-read-var-value-satisfying: Bug fix: Removed #'.
;; 2010/04/30 dadams
;;     icicle-search: Delete icicle-search-current-overlay via icicle-no-match-hook (& restore hook). 
;; 2010/04/29 dadams
;;     icicle-apply: Added optional args predicate initial-input hist def inherit-input-method.
;;                   Pass them to icicle-explore.
;;     icicle-goto-marker-1: Pass a PREDICATE arg that ensures the marker points somewhere.
;; 2010/04/25 dadams
;;     icicle-complete-keys-action:
;;       Bind icicle-*, esp. *-initially-flag, around call of this-command-keys.  Thx to M Heerdegen.
;;     icicle-complete-keys: Save some icicle-* vars to reuse in icicle-complete-keys-action.
;; 2010/04/17 dadams
;;     icicle-search-bookmark, icicle-define-search-bookmark-command:
;;       Use icicle-transform-multi-completion in icicle-delete-candidate-object.
;; 2010/04/08 dadams
;;     icicle-get-anything-candidates: Removed #' - it was preventing the fn from being functionp.
;; 2010/04/03 dadams
;;     Removed: icicle-search-desktop-bookmark (no need).
;; 2010/04/02 dadams
;;     Removed:
;;       icicle-add-region, icicle-delete-region-from-alist, icicle-purge-bad-file-regions,
;;       icicle-region-add-buffers, icicle-region-add-short-help, icicle-region-help,
;;       icicle-region-open-all-files, icicle-regions, icicle-region-sorted, 
;;       icicle-remove-all-regions-action, icicle-remove-all-regions-in-buffer, icicle-remove-region,
;;       icicle-search-all-regions, icicle-search-region(-action), icicle-select-region(-action).
;;     Added:
;;       icicle-search-bookmark-list-bookmark, icicle-search-bookmarks-together,
;;       icicle-search-desktop-bookmark, icicle-search-dired-bookmark, icicle-search-man-bookmark.
;;     icicle-exchange-point-and-mark, icicle-search(-define-candidates),
;;       icicle-char-properties-in-buffers:
;;         Use only region bookmarks, not Icicles saved regions.
;;     icicle-exchange-point-and-mark: Negative prefix arg prompts for bookmark name.
;;     icicle-search-define-candidates(-1): Raise the no-candidates error in parent, not in (-1).
;;     icicle-search-bookmark: Use full multi-completions.  Use bookmark sort orders.
;;                             Define narrowing keys in both minibuffer maps.
;;     icicle-search-bookmark-action: Transform multi-completion. Use bookmark posns only for region.
;;     icicle-define-search-bookmark-command: Added PROMPT arg.  Use multi-completions, bookmark sorts.
;; 2010/03/28 dadams
;;     Renamed: icicle-search-all-regions to icicle-search-region.
;;     Removed: old icicle-search-region (use icicle-search-region-bookmark instead),
;;              icicle-region-add-buffers, icicle-region-add-short-help, icicle-region-help,
;;              icicle-region-sorted, icicle-region-open-all-files, icicle-add-region,
;;              icicle-remove-region, icicle-delete-region-from-alist,
;;              icicle-purge-bad-file-regions, icicle-remove-all-regions-(in-buffer|action).
;;     icicle-search-all-regions: Use region bookmarks, not icicle-region-alist.
;; 2010/03/27 dadams
;;     Added: icicle-search-(bookmark-list|desktop|dired|man)-bookmark.
;;     icicle-search-bookmark: Use multi-completions.
;;     icicle-search-bookmark-action:
;;       Use icicle-transform-multi-completion.  Use both minibuffer completion maps.
;;       Search region if region bookmark.
;;     icicle-define-search-bookmark-command: Added optional PROMPT arg.  Use multi-completions.
;; 2010/03/13 dadams
;;     Applied renaming of icicle-add-buffer-name-flag to icicle-show-multi-completion-flag.
;; 2010/03/09 dadams
;;     icicle-regions: Use icicle-reversible-sort (with KEY arg), not sort.
;; 2010/03/03 dadams
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist,
;;                        icicle-alternative-sort-function to icicle-alternative-sort-comparer,
;;                        icicle-last-sort-function to icicle-last-sort-comparer.
;; 2010/02/06 dadams
;;     icicle-where-is: Make sure orig-buff is current when look up the bindings.
;; 2010/01/12 dadams
;;     icicle-insert-thesaurus-entry-cand-fn, icicle-marker+text,
;;       icicle-search-(bookmark|region)-action, icicle-char-properties-in-buffer,
;;       icicle-search-char-property-scan:
;;         save-excursion + set-buffer -> with-current-buffer (+ save-excursion).
;;     icicle-search-regexp-scan: set-buffer -> with-current-buffer.
;; 2009/12/13 dadams
;;     icicle-Info-read-node-name: Bind C-x m to icicle-bookmark-info-other-window.
;; 2009/11/24 dadams
;;     icicle-read-color: Copy the prompt string, so *-color-completion-setup can put props.
;; 2009/11/22 dadams
;;     icicle-frame-(bg|fg): Don't apply *-make-color-candidate to named-colors (done already).
;; 2009/11/03 dadams
;;     icicle-frame-(bg|fg): Use named-colors, not x-defined-colors.
;;     icicle-read-color: No need to bind icicle-transform-function, since we use hexrgb.el.
;; 2009/09/05 dadams
;;     icicle-search-replace-all-search-hits:
;;       Bind to nil: icicle-minibuffer-message-ok-p, icicle-help-in-mode-line-flag.
;;     icicle-search-action-1: Add condition-case to ignore disappearance of *Completions* win.
;;     icicle-search-highlight-and-maybe-replace:
;;       Apply renaming of icicle-acting-on-next/prev (no -p).
;;       Use length of *-completion-candidates, not mct.
;;       Fix assignment of new candidate nb, and for both directions.
;;       Bind icicle-minibuffer-message-ok-p to inhibit no-candidates msg.
;;       Wrap around to first only if not icicle-acting-on-next/prev.
;; 2009/09/02 dadams
;;     icicle-exchange-point-and-mark: Respect icicle-region-bookmarks-flag.
;; 2009/08/29 dadams
;;     Added: icicle-define-search-bookmark-command,
;;            icicle-search(-region|-info|-gnus|-w3m|(-non|-local|-remote)-file)-bookmark,
;;            icicle-search-bookmark-action.
;;     Moved (and redefined) to icicles-cmd1.el: icicle-select-bookmarked-region.
;;     (lambda...) -> #'(lambda...).
;; 2009/08/25 dadams
;;     Added icicle-select-bookmarked-region.
;;     icicle-exchange-point-and-mark:
;;       If bookmark+ is loaded, use region bookmarks.
;;       Raise error if try to save inactive region or try to select with no saved regions.
;; 2009/08/11 dadams
;;     Added: icicle-search-replace-all-search-hits.
;;     Renamed: icicle-search-replace-candidate to icicle-search-replace-cand-in-alist.
;;     Added: icicle-search-replace-cand-in-mct (not used, for now).
;;     icicle-search:
;;       Bind icicle-all-candidates-list-alt-action-fn to icicle-search-replace-all-search-hits.
;;       Bind replace-count to 0.  Bind icicle-current-input to empty string.
;;       Updated doc string.
;;     icicle-search-replace-search-hit:
;;       Do not bind icicle-completion-candidates, icicle-candidate-nb, icicle-last-input.
;;       No lack-of-current-candidate error if icicle-all-candidates-action-p.
;;     Added: icicle-search-action-1 (factored out from icicle-search-action.
;;     icicle-search-action, icicle-search-help:
;;       Bind icicle-whole-candidate-as-text-prop-p to nil, to force use of alist.
;;     icicle-search-action(-1): Do not bind icicle-candidate-nb, so don't save and restore it.
;;     icicle-search-in-context-default-fn: If replacement tried, then update the dialog state. 
;;     icicle-search-highlight-and-maybe-replace: REWRITE.
;;       Msg if string to be replaced is not found in candidate.
;;       Swap order: Don't search unless first time (or replacing all).
;;       icicle-search-replace-candidate -> icicle-search-replace-cand-in-alist.
;;       If replacement done, then: Update alist, minibuffer-completion-table, and
;;         minibuffer content.  Change candidate nb if navigating next/prev.  Complete again.
;;         Return indication of whether we tried to replace something.
;;     icicle-search-replace-cand-in-alist: Added catch, to skip whole list traversal.
;;     Moved byte-compile quieting defvars to top level.  Added one for tooltip-mode.
;; 2009/07/20 dadams
;;     icicle-font: Ensure no nil elements in COLLECTION arg to completing-read, for Emacs 22+.
;; 2009/06/07 dadams
;;     icicle-get-alist-candidate -> funcall icicle-get-alist-candidate-function.
;;     Added: icicle-purge-bad-file-regions.
;;     icicle-add-region, icicle-(select|search)-region-action:
;;       Special-case Info buffers.  Thx to Thierry Volpiatto.
;; 2009/05/28 dadams
;;     Bind icicle-search-replacement to nil.  (Somehow forgot when moved to icicle-explore.)
;;     icicle-search-replace-search-hit: Raise an error if icicle-candidate-nb is nil.
;; 2009/05/26 dadams
;;     icicle-compilation-search: Use value 'until-move for next-error-highlight.
;; 2009/05/22 dadams
;;     Added: icicle-Info-virtual-book. 
;;     Require icicles-mac.el if load-library doesn't find it.
;;     icicle-search: Set icicle-search-final-choice to result.
;;     Created - Split off from icicles-cmd.el.
 
;;;(@* "CHANGE LOG FOR `icicles-face.el'")
;;
;; 2010/05/05 dadams
;;     icicle-mustmatch-completion: Changed line-width from 2 to -2.
;; 2010/04/08 dadams
;;     Added autoload cookies.
;; 2010/03/13 dadams
;;     Made icicle-candidate-part paler (light background): #EF84FFEAF427, not #DB17FFF4E581.
;; 2010/02/17 dadams
;;     Moved functions here from icicles-opt.el: icicle-increment-color-(hue|saturation). 
;;     So no longer require icicles-opt.el.
;; 2009/04/18 dadams
;;     Removed load-time warning about using Icicles in a text terminal.
;; 2009/04/12 dadams
;;     Added: face icicle-mode-line-help.
;; 2009/01/13 dadams
;;     Added: icicle-extra-candidate.
;; 2008/10/26 dadams
;;     Added group: Icicles-Files.
;; 2008/03/29 dadams
;;     Redefined and renamed faces for completion status indicators.
;;       Renamed: icicle-completing-mustmatch-prompt-prefix to icicle-mustmatch-completion,
;;                icicle-mode-lighter-highlight(-plus) to icicle(-multi-command)-completion.
;;       Removed: icicle-completing-prompt-prefix, icicle-prompt-suffix.
;; 2008/03/09 dadams
;;     Added: icicle-mode-lighter-highlight.
;; 2007/12/10 dadams
;;     Added: icicle-input-completion-fail-lax.
;; 2007/11/18 dadams
;;     Added: icicle-proxy-candidate.
;;     icicle-special-candidate: No longer use a raised box.
;; 2007/10/22 dadams
;;     icicle-special-candidate: Raised box.
;; 2007/08/19 dadams
;;     Added: icicle-input-completion-fail.
;; 2007/06/21 dadams
;;     Added: icicle-candidate-part.  Changed definition of icicle-special-candidate.
;; 2007/06/17 dadams
;;     Added: icicle-saved-candidate.
;; 2007/06/12 dadams
;;     icicle-prompt-suffix: No box for dark background.
;; 2007/06/11 dadams
;;     Better defaults for dark background frames.
;; 2007/06/07 dadams
;;     Changed emacs-commentary-link to point to icicles-doc1.el and icicles-doc2.el.
;; 2007/05/22 dadams
;;     Protected icicle-search-context-level-1 to *-8, in case hexrgb is not loaded.
;; 2007/04/20 dadams
;;     Added: icicle-search-context-level-1 through *-8.
;;     Require icicles-opt.el.
;; 2006/12/22 dadams
;;     Renamed group icicles to Icicles.
;;     Added Icicles subgroups, and assigned them instead of group Icicles:
;;      -Buffers, -Completions-Display, -Key-Bindings, -Key-Completion, -Matching,
;;      -Minibuffer-Display, -Miscellaneous, -Searching.
;; 2006/11/06 dadams
;;     icicle-search-highlight-all-flag -> icicle-search-highlight-threshold (integer)
;; 2006/10/16 dadams
;;     icicle-special-candidate: changed background from Pink to #DB17FFF4E581.
;; 2006/10/04 dadams
;;     Added: icicle-special-candidate.
;; 2006/08/13 dadams
;;     Added: icicle-completing-prompt-prefix.
;; 2006/07/16 dadams
;;     Added dark-background face suggestions from Le Wang - thx.
;; 2006/06/30 dadams
;;     Added: minibuffer-prompt for Emacs < 22 (e.g. Emacs 21.4 has propertize).
;; 2006/04/28 dadams
;;     Added: icicle-whitespace-highlight.
;; 2006/04/14 dadams
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/07 dadams
;;     Added: icicle-search-main-regexp-others.
;;     Renamed: icicle-search-main-regexp to icicle-search-main-regexp-current.
;; 2006/03/27 dadams
;;     Added: icicle-search-*-regexp.
;; 2006/03/22 dadams
;;     Renamed: icicle-root-highlight-* to icicle-match-highlight-*.
;; 2006/03/21 dadams
;;     Added: icicle-common-match-highlight-Completions.
;;     icicle-root-highlight-Completions: Changed default face.
;; 2006/03/08 dadams
;;     Added: icicle-current-candidate-highlight.
 
;;;(@* "CHANGE LOG FOR `icicles-fn.el'")
;;
;; 2010/06/18 dadams
;;     icicle-completing-read:
;;       Initialize icicle-completions-format-internal to icicle-completions-format.
;;     icicle-mctized-full-candidate:
;;       Set icicle-completions-format-internal to horizontal for multi-line multi-completions.
;;     icicle-insert-candidates:
;;       Remove face property from end of candidate to next column.
;;       Replace icicle-completions-format by icicle-completions-format-internal.
;;     Renamed: bookmarkp-* to bmkp-*.
;; 2010/06/14 dadams
;;     icicle-read-from-minibuffer:
;;       If add file-name default to prompt, remove dir part first.  Thx to Chris Hecker.
;;     icicle-next-candidate: Do not icicle-recompute-candidates if this command was repeated.
;;     icicle-increment-cand-nb+signal-end: 
;;       Signal wrap from initial, not from 0.  Negative INCR -> start at end.  Thx to M. Heerdegen.
;;     icicle-call-then-update-Completions:
;;       Set icicle-last-input to nil so icicle-save-or-restore-input makes next-candidate recompute.
;; 2010/06/12 dadams
;;     icicle-fit-completions-window: Let-bind window-min-height to prevent deletion in Emacs 23.
;; 2010/06/10 dadams
;;     icicle-maybe-sort-and-strip-candidates:
;;       Set icicle-completion-candidates to result of *maybe...maybe*.  (It broke C-~.)
;; 2010/06/09 dadams
;;     icicle-isearch-complete-past-string: Use the last-used ring.  Thx to Michael Heerdegen.
;; 2010/06/08 dadams
;;     icicle-display-candidates-in-Completions: Show also total when truncated: N shown / M.
;;     icicle-maybe-sort-maybe-truncate: Save icicle-nb-candidates-before-truncation before truncating.
;; 2010/06/04 dadams
;;     Added: icicle-clear-lighter, icicle-maybe-sort-maybe-truncate, icicle-take.
;;     icicle-(prefix|apropos)-candidates, icicle-strip-ignored-files-and-sort,
;;       icicle-maybe-sort-and-strip-candidates, icicle-display-Completions:
;;         Use icicle-maybe-sort-maybe-truncate, not icicle-reversible-sort.
;;     icicle-(un)highlight-lighter: Use icicle-clear-lighter (factored out).
;; 2010/05/30 dadams
;;     Added: icicle-files-within-1.
;;     icicle-files-within:
;;       Use icicle-files-within-1.  Optionally don't follow symlinks (new arg NO-SYMLINKS-P).
;;       Don't process same dir twice.  Respect icicle-ignored-directories.  Thx to Michael Heerdegen.
;; 2010/05/18 dadams
;;     icicle-save-or-restore-input:
;;       Use icicle-file-directory-p, not file-directory-p (fails: ~/foo//usr/).  Thx to M. Heerdegen.
;; 2010/05/04 dadams
;;     icicle-alt-act-fn-for-type: Pass TYPE to icicle-apply-to-saved-candidate.
;; 2010/04/30 dadams
;;     icicle(-file-name)-unsorted-(apropos|prefix)-candidates, icicle-fuzzy-candidates:
;;       Set icicle-common-match-string to nil when no candidates.
;; 2010/04/29 dadams
;;     icicle-unpropertize: Do nothing unless icicle-remove-icicles-props-p.
;; 2010/04/27 dadams
;;     icicle-show-help-in-mode-line: Test value, not just boundp, of icicle-completing-keys-p.
;; 2010/04/21 dadams
;;     icicle-mode-line-name-less-p: Removed bogus calls to symbol-name.  Thx to Michael Heerdegen.
;; 2010/04/20 dadams
;;     Added: icicle-dirs-first-p.
;; 2010/04/02/dadams
;;     icicle-completing-p: Cache t, not the keymap portion.
;; 2010/03/16 dadams
;;     icicle-display-candidates-in-Completions, treating icicle-candidate-properties-alist:
;;       For subsequent matches of join string, skip over the last join string (not 1 char).
;;     icicle-candidate-short-help: Treat each property individually.
;; 2010/03/13 dadams
;;     icicle-reversible-sort: Respect icicle-transform-before-sort-p.
;; 2010/03/10 dadams
;;     icicle-show-help-in-mode-line:
;;       Put this test first: If candidate is a string with a help property, use that.
;;       Use get-file-buffer, not get-buffer, for a visited file.
;; 2010/03/09 dadams
;;     icicle-reversible-sort: Added optional KEY arg.  Set LIST to sort result and return it.
;;     icicle-multi-sort: Set RESULT to funcall result.
;; 2010/03/03 dadams
;;     Added: icicle-multi-sort, icicle-make-plain-predicate, icicle-alpha-p.
;;     icicle-reversible-sort: Use icicle-sort-comparer and icicle-multi-sort.
;;                             Reset to unsorted if an error is raised.
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer,
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist.
;;     icicle-read-shell-command-completing: Removed extra binding for icicle-sort-function.
;; 2010/01/12 dadams
;;     icicle-display-candidates-in-Completions: set-buffer -> with-current-buffer.
;; 2009/12/25 dadams
;;     icicle-strip-ignored-files-and-sort:
;;       Call completion-ignored-build-apply and icicle-update-ignored-extensions-regexp.
;; 2009/12/21 dadams
;;     fset -> defalias.
;; 2009/12/07 dadams
;;     Added: icicle-minibuffer-default-add-dired-shell-commands.
;;     icicle-dired-read-shell-command: Instead of using minibuffer-default-add-shell-commands,
;;       use icicle-minibuffer-default-add-dired-shell-commands.
;;     icicle-read-shell-command-completing: Bind insert-default-directory to nil.
;;     icicle-dir-prefix-wo-wildcards: Don't ever return nil - return "" if need be.
;;     icicle-read-string:
;;       Handle a consp default-value when user enters "": use the car.  Thx to Sakuma Ryo.
;; 2009/12/03 dadams
;;     icicle-completing-read:
;;       Make sure we don't use a nil def value for init value.  Thx to Sebastian Luque.
;; 2009/11/27 dadams
;;     *-display-candidates-in-Completions, *-prefix-candidates, *-fuzzy-candidates:
;;       Handle swank completions too.
;; 2009/11/25 dadams
;;     icicle-insert-candidates: Handle vertical layout: icicle-completions-format.
;; 2009/11/03 dadams
;;     icicle-color-completion-setup: Use hexrgb-defined-colors, not x-defined-colors.
;;                                    No icicle-transform-function, since we use hexrgb.el.
;; 2009/10/25 dadams
;;     Added: icicle-dir-prefix-wo-wildcards.  Use in icicle-insert-cand-in-minibuffer.
;;     icicle-save-or-restore-input: Remove test comparing last completion with current input.
;;     icicle-abbreviate-or-expand-file-name: If arg is absolute, do not expand.
;;     icicle-display-candidates-in-Completions, icicle-prefix-candidates,
;;       icicle-not-basic-prefix-completion-p:
;;         Updated for new completion methods.  Use *-current-TAB-method, not old fuzzy flag.
;; 2009/10/24 dadams
;;     icicle-unsorted-file-name-*-candidates, icicle-*-any-file-name-candidates-p:
;;       Got rid of slashed-p.
;;     icicle-unsorted-file-name-apropos-candidates:
;;       Set icicle-common-match-string to nil if there are no candidates.
;;     icicle-apropos-any-file-name-candidates-p:
;;       When input is a dir, remove final /, so we don't non-match highlight the / (empty dir).
;;       Bind case-fold-search, for apropos matching.
;;     icicle-abbreviate-or-expand-file-name: Set DIR to nil if it's relative.
;; 2009/10/22 dadams
;;     Added: icicle-file-name-directory.
;;     icicle-completion-setup-function, icicle-display-candidates-in-Completions,
;;       icicle-save-or-restore-input, icicle-file-name-directory-w-default,
;;       icicle-historical-alphabetic-p:
;;         Use icicle-file-name-directory, not file-name-directory.
;; 2009/10/17 dadams
;;     icicle-completion-try-completion: Return RES.
;; 2009/10/12 dadams
;;     icicle-unsorted-prefix-candidates, icicle-prefix-any-candidates-p:
;;       Use length of INPUT, not length from point backward to field-beginning.
;;     icicle-input-from-minibuffer: Added optional LEAVE-ENVVARS-P arg.
;;     icicle-next-candidate, icicle-highlight-complete-input:
;;       Call icicle-input-from-minibuffer with LEAVE-ENVVARS-P arg.
;;     icicle-completion-all-completions:
;;       If not basic completion (Emacs 23) and input ends in $, then append $ to cands also.
;; 2009/09/25 dadams
;;     icicle-file-name-prefix-candidates: Use whole input, not just nondirectory.
;;     Added: icicle-not-basic-prefix-completion-p.  Use where appropriate.
;; 2009/09/19 dadams
;;     icicle-unpropertize: Remove the internal text properties added by Icicles.
;;     icicle-completing-read, icicle-read-file-name: Call icicle-unpropertize unconditionally.
;; 2009/09/12 dadams
;;     icicle-kill-a-buffer: Bind enable-recursive-minibuffers, to confirm modified buffer.
;; 2009/09/05 dadams
;;     icicle-msg-maybe-in-minibuffer: Do nothing if icicle-minibuffer-message-ok-p is nil.
;; 2009/08/19 dadams
;;     icicle-candidate-short-help: Return (possibly propertized) STRING.
;; 2009/08/09 dadams
;;     Added: icicle-insert-cand-in-minibuffer - factored out code from icicle-next-candidate.
;;     eval-after-load "crm":
;;       Test that icy-mode is available using featurep, not fboundp.  Thx to Michael Heerdegen.
;; 2009/07/26 dadams
;;     icicle-completing-read, icicle-read-file-name-1:
;;       Bind minibuffer-history-variable to itself.
;; 2009/07/13 dadams
;;     icicle-read-face-name (Emacs 20 version): Use copy-sequence on prompt, before add prop.
;; 2009/07/12 dadams
;;     icicle-display-completion-list:
;;       Use different protection (fboundp) for fix for latest Emacs 23 crap (base size in cdr).
;; 2009/07/11 dadams
;;     icicle-next-candidate:
;;       If icicle-regexp-quote-flag, regexp-quote before string-match for highlighting root.
;;     icicle-place-cursor: If icicle-regexp-quote-flag, regexp-quote input to search for it.
;; 2009/07/02 dadams
;;     icicle-displayable-cand-from-saved-set:
;;       If icicle-readable-to-markers returns an atom, just use that.
;; 2009/06/17 dadams
;;     icicle-fit-completions-window: Scale text size initially.
;; 2009/06/07 dadams
;;     icicle-get-alist-candidate -> funcall icicle-get-alist-candidate-function.
;;     icicle-mctize-all:  If PRED is nil, so is NEWPRED (use and instead of when).
;; 2009/05/22 dadams
;;     Require icicles-mac.el if load-library doesn't find it.
;; 2009/05/18 dadams
;;     icicle-display-candidates-in-Completions: deactivate-mark in *Completions* after display.
;; 2009/05/17 dadams
;;     icicle-next-candidate: Updated to reflect thumb-frm.el name changes.
;; 2009/05/17 dadams
;;     Added: icicle-toggle-icicle-mode-twice.
;;     In eval-after-load crm: Use icicle-toggle-icicle-mode-twice, not icy-mode calls.
;; 2009/05/15 dadams
;;     icicle-unhighlight-lighter: Wrap redisplay in condition-case, like elsewhere.
;; 2009/05/11 dadams
;;     Added: icicle-upcase.  Use in place of upcase everywhere, to work around Emacs 20 bug.
;;     Added: icicle-local-keys-first-p.
;;     icicle-display-candidates-in-Completions:
;;       Don't highlight historical candidate if in icicle-hist-cands-no-highlight.
;; 2009/05/09 dadams
;;     icicle-input-from-minibuffer: Keep text properties when pick up input.
;;     icicle-highlight-input-noncompletion(-rest): Use an overlay instead of text property.
;;     icicle-show-help-in-mode-line: Fix special case for pseudo-key/cmd ..: go up to prefix.
;; 2009/05/07 dadams
;;     icicle-display-candidates-in-Completions, in the code that hides common match:
;;       Don't reset display property to nil.
;;       Use icicle-common-match-string, not current input.
;;     icicle-expanded-common-match-1:
;;       If input doesn't match candidate, return nil.  Throw nil, not input, everywhere.
;;     *-unsorted(-file-name)-apropos-candidates, *-apropos-any(-file-name)-candidates-p:
;;       Protect calls to icicle-apropos-complete-match-fn with condition-case, for Emacs 20.
;;     icicle-place-cursor: Added optional arg.
;; 2009/05/05 dadams
;;     icicle-alt-act-fn-for-type:
;;       Bind completion-ignore-case, based on read-buffer-completion-ignore-case.
;; 2009/05/03 dadams
;;     Use (fboundp 'minibuffer-default-add-completions), not (> emacs-major version 22).
;; 2009/05/01 dadams
;;     Renamed: icicle-choose-action-for-type to icicle-alt-act-fn-for-type.
;;     icicle-type-actions-alist: Rewrote.  Handle both list of objs and single obj.
;;     icicle-quote-file-name-part-of-cmd: Rewrote.  Quote file name always, and only file name.
;; 2009/04/30 dadams
;;     icicle-show-in-mode-line: Clear any message (e.g. Computing completion candidates...).
;; 2009/04/29 dadams
;;     icicle-get-alist-candidate: If NO-ERROR-P is 'no-error-no-msg, just return nil (no msg).
;;     icicle-choose-action-for-type: Use lax completion.
;; 2009/04/28 dadams
;;     icicle-choose-action-for-type: 
;;       Moved here from icicles-mac.el, changed to a function, and locally bind
;;       *-alt-action-fn  to icicle-choose-action-for-type function.
;;       For undefined TYPE, provide all functions as candidates.
;;     Added eval-when-compile of load-library icicles-mac.
;; 2009/04/27 dadams
;;     icicle-recompute-candidates:
;;       Keep no-display completion mode, if that's current.
;;       Set icicle-last-completion-command only if completion type, not user input, changed.
;;     icicle-complete-again-update: Added icicle-prefix-complete-no-display to first case.
;; 2009/04/19 dadams
;;     Use unless instead of or for fset's. (cosmetic)
;;     icicle-completing-read, icicle-read-from-minibuffer, icicle-read-string:
;;       Renamed history parameter to HIST-m@%=!$+&^*z, so C-h f output looks less strange.
;; 2009/04/18 dadams
;;     Soft-require hexrgb.el unconditionally, not just when there is a window-system.
;; 2009/04/12 dadams
;;     icicle-display-candidates-in-Completions: Added number of candidates to mode-line.
;;     icicle-show-help-in-mode-line: Use face icicle-show-help-in-mode-line.  Fix prefix keys.
;; 2009/04/11 dadams
;;     icicle-candidate-short-help:
;;       Do nothing if either icicle-help-in-mode-line-flag or tooltip-mode is nil.
;;     icicle-show-help-in-mode-line:
;;       If no symbol help, try string.
;;       Handle lambdas, menu-function-# from easy-menu, and prefix keys.
;;     icicle-make-color-candidate: Construct short help only if user will see it.
;; 2009/04/10 dadams
;;     Added: icicle-candidate-short-help, icicle-color-completion-setup (from i*-read-color).
;;     Moved here from icicle-cmds.el:
;;      icicle-remove-color-duplicates, icicle-color-help, icicle-make-color-candidate.
;;     icicle-make-color-candidate: Added short help, using icicle-candidate-short-help.
;;     icicle-show-help-in-mode-line:
;;       Treat :icicle-candidate-help text property on strings.
;;       Use candidate, not cand, for stringp clause, and use icicle-transform-multi-completion.
;;       Renamed: :icicle-candidate-help to icicle-mode-line-help.
;; 2009/04/08 dadams
;;     icicle-show-help-in-mode-line: Treat absolute file names too.
;; 2009/04/07 dadams
;;     icicle-show-help-in-mode-line: (bufferp (get-buffer...)...) -> (get-buffer...) Duh.
;; 2009/04/06 dadams
;;     Added: icicle-show-help-in-mode-line - from code in icicle-next-candidate:
;;              Added arg.  Handle: faces, buffer names, file names, :icicle-candidate-help.
;;     Added: icicle-help-line-(buffer|file).
;;     icicle-next-candidate: Use icicle-show-help-in-mode-line.
;; 2009/04/05 dadams
;;     Added: icicle-show-in-mode-line.
;;     icicle-next-candidate: Use icicle-show-in-mode-line to show help in mode-line.
;; 2009/04/04 dadams
;;     Added: icicle-buffer-smaller-p, icicle-major-mode-name-less-p,
;;            icicle-mode-line-name-less-p, icicle-buffer-file/process-name-less-p.
;; 2009/04/03 dadams
;;     icicle-read-from-minibuffer: Save filtered default val as icicle-filtered-default-value.
;;     icicle-lisp-vanilla-completing-read: Use icicle-filtered-default-value, not DEF, at end.
;; 2009/03/29 dadams
;;     icicle-read-shell-command: If non-nil initial-contents, punt to use original (old-*).
;; 2009/03/27 dadams
;;     Don't fset minibuffer-default-add-completions unless > Emacs 22.
;; 2009/03/27 dadams
;;     icicle-read-from-minibuffer: Use icicle-filter-wo-input on default-value (all values).
;;     icicle-completing-read, icicle-read-file-name-1:
;;       Use icicle-filter-wo-input on default value only to get init value.
;;     Added: icicle-minibuffer-default-add-completions, icicle-first-N.
;; 2009/03/26 dadams
;;     icicle-completing-read, icicle-read-file-name-1:
;;       Filter default values using icicle-filter-wo-input.
;;     icicle-filter-wo-input: Return the candidate, if it passes filtering.
;; 2009/03/16 dadams
;;     Added: icicle-recentf-make-menu-items.
;; 2009/03/10 dadams
;;     icicle-read-shell-command-completing: Applied renamings: icicle-guess-commands-in-path,
;;       icicle-shell-command-candidates-cache, icicle-recompute-shell-command-candidates.
;;     Moved to icicles-opt.el and renamed: icicle-shell-command-candidates.
;; 2009/03/08 dadams
;;     Added: icicle-quote-file-name-part-of-cmd.
;;     icicle-read-shell-command-completing:
;;       Call icicle-quote-file-name-part-of-cmd to escape spaces etc. in file names.
;;       Removed unneeded minibuffer-local-*-map let bindings.
;;     icicle-dired-smart-shell-command:
;;       Protected dired-default-directory with fboundp, for Emacs 20.
;; 2009/03/01 dadams
;;     icicle-read-from-minibuffer:
;;       No longer use icicle-complete-on-demand-cmd and on-demand-map.
;; 2009/02/28 dadams
;;     Don't fset old-dired-smart-shell-command here - do after load Dired-X (icicles-mode.el).
;;     Added for Emacs 20: definition of replace-regexp-in-string.
;;     icicle-read-(number|string-completing):
;;       No need for fboundp of replace-regexp-in-string, since provide it now for Emacs 20.
;;     icicle-read-file-name: Treat directory candidates as special candidates.
;;     icicle-read-shell-command-completing:
;;       Candidate help depends on candidate type.
;;       Use existing icicle-extra-candidates as its own default, not icicle-file-extras.
;; 2009/02/27 dadams
;;     icicle-shell-command: Fixed typo: shell-command -> old-shell-command.
;;     icicle-read-shell-command-completing: Append icicle-file-extras to i*-extra-candidates.
;; 2009/02/23 dadams
;;     icicle-read-shell-command-completing:
;;       Bind icicle-extra-candidates-dir-insert-p, not insert-default-directory, to nil.
;;     icicle-next-candidate: Protect dir insertion with icicle-extra-candidates-dir-insert-p.
;; 2009/02/22 dadams
;;     icicle-dired-read-shell-command: Use minibuffer-default-add-shell-commands if available.
;; 2009/02/20 dadams
;;     icicle-read-from-minibuffer: Treat icicle-complete-on-demand-cmd - on-demand completion.
;;     Added: icicle-dired-smart-shell-command, icicle-read-shell-command-completing,
;;            icicle-dired-read-shell-command, icicle-extra-candidates-first-p,
;;            icicle-require-match-p, icicle-shell-command(-on-region).
;;     icicle-read-shell-command:
;;       Call icicle-read-shell-command-completing.  Define always, not just when mailcap.
;;     icicle-dired-guess-shell-command: Just call icicle-read-shell-command-completing.
;;     icicle-shell-command-candidates: Cache completions in icicle-shell-command-candidates.
;;     icicle-highlight-input-noncompletion(-rest), icicle-highlight-lighter:
;;       Use function icicle-require-match-p, not var.
;;     icicle-completing-p: Test also whether parent map is a completion map.  Always cache.
;; 2009/02/17 dadams
;;     icicle-read-shell-command: Handle nil default-value.  Thx to Kao Felix.
;;     icicle-read-file-name: Append new proxy cands to any existing ones.
;;                            Don't assume that a proxy cand is a variable.
;; 2009/01/25 dadams
;;     Added: *-dired-guess-shell-command, *-read-shell-command, *-shell-command-candidates.
;; 2009/01/17 dadams
;;     icicle-display-candidates-in-Completions: Highlight Levenshtein (1) match.
;;     icicle-unsorted-apropos(-file-name)-candidates, icicle-display-Completions,
;;       icicle-apropos-any(-file-name)-candidates-p: Removed PCM.
;;     Added: icicle-levenshtein(-strict|-one)-match, icicle-levenshtein-one-regexp,
;;            icicle-substrings-of-length.
;; 2009/01/15 dadams
;;     Lose "icicle-" for fboundp's: completion-all-completions, completion-try-completion.
;; 2009/01/14 dadams
;;     Added: icicle-remove-dups-if-extras.
;; 2009/01/13 dadams
;;     Added: icicle-completion-all-completions, icicle-completion-try-completion.
;;     icicle-unsorted(-file-name)-(prefix|apropos)-candidates, icicle-display-Completions,
;;       icicle-any-(prefix|apropos)(-file-name)-candidates-p:
;;         Respect icicle-respect-completion-styles-p (Emacs 23).
;;     icicle-unsorted-file-name-prefix-candidates: Removed matching "^"++(regexp-quote input).
;;     icicle(-unsorted)(-file-name)-(prefix|apropos)-candidates:
;;       Call icicle-transform-candidates for extra candidates and proxy candidates too.
;;     icicle-display-candidates-in-Completions: Highlight extra candidates.
;;     icicle-face-valid-attribute-values:
;;       Use font-family-list instead of x-font-family-list, if available.
;; 2009/01/03 dadams
;;     icicle-call-then-update-Completions:
;;       If icicle-last-completion-command is nil, test icicle-current-completion-mode - don't
;;       just call icicle-prefix-complete.  E.g. icicle-search shouldn't end with a single
;;       prefix match.
;; 2008/12/25 dadams
;;     Added: icicle-save-raw-input (factored out from icicle-save-or-restore-input).
;;     icicle-save-or-restore-input:
;;       Change test whether last-command is cycling cmd to add also not being completing cmd.
;;       Use icicle-completing-p instead of testing this-command for being a cycling cmd.
;;       Use icicle-save-raw-input.
;;     icicle-next-candidate: Set icicle-cycling-p (new var) to t.
;; 2008/12/20 dadams
;;     icicle-save-or-restore-input: Don't restore last input if this is a completing command.
;; 2008/12/07 dadams
;;     icicle-completing-read, icicle-read-file-name-1:
;;       Removed icicle-prompt.  Don't add completion prompt prefix, except for Emacs 20.
;; 2008/12/06 dadams
;;     icicle-call-then-update-Completions:
;;       To update, call icicle-last-completion-command, not icicle-(apropos|prefix)-complete.
;; 2008/12/02 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates:
;;       Call icicle-filter-wo-input before filtering with user input (as the doc string says).
;; 2008/11/28 dadams
;;     icicle-recompute-candidates, icicle-save-or-restore-input:
;;       Test property icicle-(prefix|apropos)-completing-command, not eq cmds.
;; 2008/11/23 dadams
;;     icicle-read-file-name:
;;       Don't initialize icicle-proxy-candidates unless icicle-add-proxy-candidates-flag.
;;       Don't add + to completion indicator if Emacs 23.
;; 2008/11/22 dadams
;;     icicle-completing-read:
;;       Bind icicle-fancy-cands-internal-p.  Do not call icicle-mctize-all unless fancy cands.
;;     icicle-mctized-full-candidate:
;;       No-op unless icicle-fancy-cands-internal-p or icicle-whole-candidate-as-text-prop-p.
;;     icicle-read-(number|char-exclusive|string-completing|face-name):
;;       Put icicle-fancy-candidates property on prompt if proxy candidates.
;;     icicle-display-candidates-in-Completions:
;;       Don't transform candidates unless icicle-fancy-cands-internal-p.
;; 2008/11/18 dadams
;;     icicle-display-candidates-in-Completions:
;;       Add icicle-special-candidate property as text property to candidates in
;;         icicle-completion-candidates, instead of just to display candidate, so returned also.
;;       Added support for icicle-display-string property.
;;       For these properties: look up symbol first in minibuffer-completion-table if obarray.
;; 2008/11/14 dadams
;;     icicle-display-candidates-in-Completions:
;;       Respect icicle-hide-common-match-in-Completions-flag.
;; 2008/11/10 dadams
;;     icicle(-unsorted)(-file-name)-(apropos|prefix)-candidates:
;;       Moved special and proxy candidates outside match filtering,
;;         reversing a change made on 2007/11/18.  Dunno why that change was made.
;;       But filter special and proxy candidates using the input.
;; 2008/11/09 dadams
;;     Added: icicle-join-nth-parts, icicle-display-cand-from-full-cand.
;;     icicle-transform-multi-completion: Use icicle-join-nth-parts.
;;     icicle-transform-multi-completion: Don't test icicle-list-join-string (always non-nil).
;;     icicle-first-matching-candidate:
;;       Fix: set res properly, not to the tested value.  Thx to Hannes Janetzek.
;; 2008/11/07 dadams
;;     icicle-color-name-w-bg: Return input if hexrgb.el is not loaded.  Thx to Fabrice Knevez.
;; 2008/11/04 dadams
;;     icicle-read-face-name: Soft-require eyedropper.el or palette.el.
;; 2008/11/03 dadams
;;     icicle-expanded-common-match: Bug fix for Emacs 20: If error matching, just return INPUT.
;;     icicle-highlight-input-noncompletion: Erase any message first before highlighting.
;; 2008/11/02 dadams
;;     icicle-display-candidates-in-Completions: For icicle-candidate-properties-alist, use
;;       narrowed buffer content, not i-current-completion-in-Completions.
;; 2008/10/24 dadams
;;     icicle-first-matching-candidate:
;;       Use regexp-quote on CAND.
;;       Add icicle-list-end-string to each entry in CANDIDATES.
;;       Iteration, not recursion.
;;     icicle-get-alist-candidate: Added optional arg NO-ERROR-P.
;; 2008/10/18 dadams
;;     Replaced customize-save-variable by funcall icicle-customize-save-variable-function.
;; 2008/10/14 dadams
;;     Applied renaming of icicle-completion-help to icicle-minibuffer-help.
;; 2008/10/06 dadams
;;     Don't toggle icy-mode after loading crm.el unless icy-mode is defined (!).
;;     icicle-ms-windows-NET-USE: If hash tables not available, just run the NET USE process.
;; 2008/10/04 dadams
;;     Substitute Icicles version of completing-read-multiple and its maps.  Thx to Per Nordlow.
;;       Added: (icicle|old)-completing-read-multiple, icicle-define-crm-completion-map,
;;              (icicle|old)-crm-local-(completion|must-match)-map.
;;     icicle-display-completion-list: Protect against Emacs 23 nonsense with base-size in cdr.
;; 2008/09/30 dadams
;;     Added: icicle-ms-windows-NET-USE.  Thx to Thomas Lim.
;;     icicle-file-remote-p: Use icicle-ms-windows-NET-USE.
;;     icicle-highlight-input-noncompletion: Return file-local-p if test shows it's local.
;;     icicle-call-then-update-Completions:
;;       Treat also known file-local-p return from icicle-highlight-input-noncompletion.
;;     Renamed icicle-isearch-complete-1 to icicle-isearch-complete-past-string and moved it
;;       here from icicles-mcmd.el.
;;     icicle-isearch-complete-past-string: Always use regexp-search-ring.  Changed prompt.
;; 2008/09/27 dadams
;;     icicle-highlight-input-noncompletion: Fixed typo: implicit-remote -> implicit.
;; 2008/09/20 dadams
;;     icicle-(apropos|prefix)-candidates, icicle-maybe-sort-and-strip-candidates:
;;       Strip ignored files if icicle-abs-file-candidates.
;; 2008/09/19 dadams
;;     icicle-get-candidates-from-saved-set: Added optional arg DONT-EXPAND-FILESETS-P.
;;                                           Use icicle-kill-a-buffer, not kill-buffer.
;;     Moved icicle-kill-a-buffer here from icicles-cmd.el.
;;     Added: icicle-unpropertize.
;;     icicle-completing-read, icicle-read-file-name: Use icicle-unpropertize.
;; 2008/09/16 dadams
;;     filesets-get-filelist: Fixed :tree so it includes files in subdirs.
;;     Added: icicle-filesets-files-under.
;; 2008/09/15 dadams
;;     Added: icicle-saved-fileset-p, icicle-displayable-cand-from-saved-set.
;;     icicle-get-candidates-from-saved-set:
;;       Rewrote code to convert saved candidates to displayable candidates.
;;         Use icicle-displayable-cand-from-*, icicle-saved-fileset-p.  Require filesets.el.
;;       Moved filesets-get-fileset-from-name into and of first cond clause.
;;     Wrapped defun of filesets-get-filelist in eval-after-load.
;; 2008/09/13 dadams
;;     Added: icicle-get-candidates-from-saved-set (factored from code in
;;            icicle-retrieve-candidates-from-set), filesets-get-filelist (redefined),
;;            icicle-explicit-saved-completion-candidates.
;;     Moved here from icicles-mcmd.el: icicle-readable-to-markers.
;; 2008/09/04 dadams
;;     icicle-read-file-name-1: Removed unused code for non icicle-mode case.
;; 2008/09/03 dadams
;;     Removed: icicle-(un)highlight-crosshairs, icicle-unhighlight-crosshairs+cleanup.
;; 2008/09/01 dadams
;;     Added: icicle-(un)highlight-crosshairs, icicle-unhighlight-crosshairs+cleanup.
;; 2008/08/29 dadams
;;     icicle-mctize-all:
;;       Do it for all list collections (new icicle-mctized-full-candidate).
;;       Adjust PRED to use cdr only for conses with string cars.
;;     icicle-mctized-full-candidate:
;;       Treat all kinds of list entries: strings, symbols, and conses with symbol args, etc.
;; 2008/08/25 dadams
;;     icicle-display-candidates-in-Completions: Do nothing if NO-DISPLAY-P is 'no-msg.
;; 2008/08/24 dadams
;;     icicle-filter-wo-input: Filter out empty-string candidates: "".
;;     Added: icicle-minibuf-input-sans-dir.
;;     Renamed: icicle-minibuffer-contents-from-minibuffer to icicle-input-from-minibuffer,
;;              icicle-minibuffer-contents to icicle-minibuf-input.
;;     icicle-display-candidates-in-Completions, icicle-next-candidate, icicle-place-cursor:
;;       Use icicle-minibuf-input-sans-dir.
;; 2008/08/23 dadams
;;     icicle-display-candidates-in-Completions:
;;       Wrap preliminary display of *Completions* in save-selected-window.
;; 2008/08/22 dadams
;;     icicle-display-candidates-in-Completions:
;;       Display *Completions* before calling with-output-to-temp-buffer and filling it.
;;        This is so we can know the window width, to determine the correct formatting.
;;     icicle-insert-candidates:
;;       Don't use lru window or *-Completions-*-default-width (removed).  Failsafe width 40.
;; 2008/08/21 dadams
;;     icicle-completing-read:
;;       Bind minibuffer-completing-file-name to nil if not completing a file name.
;;       Removed setq of minibuffer-completion-table. Already do it in *-lisp-vanilla-*.
;; 2008/08/20 dadams
;;     icicle-insert-candidates:
;;       Turn off mouse-face after insert candidate.
;;       Fixup whitespace correctly: Don't remove whitespace that is part of a candidate.
;;     Added: icicle-ding.
;;     icicle-read-number: Replaced ding by icicle-ding.
;; 2008/08/18 dadams
;;     Moved here from icicles-cmd.el (and renamed from *-less-p): icicle-cdr-lessp.
;;     Added: icicle-delete-count, icicle-mctize-all, icicle-mctized-(display|full)-candidate,
;;            icicle-part-1-cdr-lessp.
;;     Renamed: icicle-delete-if(-not)     to icicle-remove-if(-not),
;;              icicle-put-alist-candidate to icicle-put-whole-cand-prop,
;;              icicle-update-completions  to icicle-complete-again-update.
;;     icicle-completing-read: Factored out transformation to MCT into new fn icicle-mctize-all.
;; 2008/08/08 dadams
;;     icicle-completing-read: Updated doc string for Emacs 23.
;; 2008/08/03 dadams
;;     icicle-completing-read: Convert the predicate to apply to just the cdr (= original cons).
;;     icicle-save-or-restore-input: Don't save empty string ("") to C-l history.
;; 2008/07/27 dadams
;;     Added: icicle-2nd-part-string-less-p.
;;     Moved here from icicles-mcmd.el: icicle-transform-multi-completion.
;; 2008/07/19 dadams
;;     icicle-choose-completion-string: Don't move to point-max unless in minibuffer.
;; 2008/06/24 dadams
;;     Make *-must-match-filename-map an alias for *-filename-completion-map.  Use the latter.
;; 2008/06/22 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Remove text properties from result (only) if icicle-unpropertize-*-flag.
;; 2008/06/21 dadams
;;     icicle-read-file-name: Remove text properties from result file name.
;; 2008/06/01 dadams
;;     icicle-lisp-vanilla-completing-read: Typo: minibuffer-local-must-match-filename-map.
;;     Put sort predicate properties on some sort predicate symbols, e.g. icicle-dirs-last-p.
;; 2008/05/22 dadams
;;     icicle-read-file-name: Bind minibuffer-completing-file-name to t.
;;     icicle-read-file-name-1: Do not prepend ". " if Emacs 23+, since it uses completing-read.
;;     icicle-(un)highlight-lighter: Reflect case-sensitivity in lighter.
;; 2008/05/11 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates:
;;       Pass input to all-completions only if not icicle-apropos-complete-match-fn.
;; 2008/05/11 dadams
;;     Renamed icicle-fit-Completions-window to icicle-fit-completions-window.
;;     icicle-fit-completions-window: Use current window - not necessarily for *Completions*.
;; 2008/05/06 dadams
;;     icicle-highlight-lighter: Wrap redisplay in condition-case to ignore any errors.
;; 2008/05/05 dadams
;;     icicle-file-name-input-p: Redefined to just use minibuffer-completing-file-name.
;; 2008/05/01 dadams
;;     icicle-lisp-vanilla-completing-read, icicle-lisp-vanilla-completing-read,
;;       icicle-read-from-minibuffer, icicle-read-number, icicle-read-string-completing:
;;         Adapted to Emacs 23 change to allow list of strings for default value.
;; 2008/04/25 dadams
;;     icicle-call-then-update-Completions:
;;       nil icicle-test-for-remote-files-flag means don't test for remote file name.
;;     icicle-highlight-input-noncompletion:
;;       Correction: implicit, not always, in combination with not incremental completion.
;;       Added check for icicle-completing-command if value is explicit*.
;;       Added check for icicle-test-for-remote-files-flag if *-strict with lax completion.
;;       Protect call to icicle-file-remote-p with check of icicle-test-for-remote-files-flag.
;; 2008/04/18 dadams
;;     Renamed icicle-init-value-flag to icicle-default-value.
;;     icicle-read-from-minibuffer:
;;       If icicle-default-value is t, add to prompt.  Thx to Dominique Quatrevaux.
;;     icicle-completing-read, icicle-read-file-name-1, icicle-read-from-minibuffer:
;;       Change icicle-default-value test to rule out t also.
;;     icicle-completion-setup-function:
;;       Don't call file-name-directory unless minibuffer-completing-file-name.
;; 2008/04/01 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates:
;;       If icicle-unsorted-apropos-candidates pass input to all-completions.
;;     icicle-expanded-common-match-1: 
;;       Use icicle-apropos-complete-match-fn throughout, but return nil if that is nil.
;;     icicle-display-candidates-in-Completions:
;;       If icicle-apropos-complete-match-fn is nil, don't try to highlight input match.
;; 2008/03/31 dadams
;;     icicle-display-candidates-in-Completions:
;;       Allow for no completion type, for No completions msg.
;; 2008/03/30 dadams
;;     icicle-read-file-name-1:
;;       Bind read-file-name-function to nil.
;;       Funcall icicle-old-read-file-name-fn instead of old-read-file-name.
;;     icicle-(un)highlight-lighter: Respect icicle-highlight-lighter-flag.
;;     Top-level: Moved fset for old-read-file-name to icicles-mode.el and renamed it.
;; 2008/03/29 dadams
;;     icicle-completing-read, icicle-read-file-name-1:
;;       Combine new faces for single-character minibuffer completion status indicator.
;;       Call icicle-highlight-lighter after establishing require-match, not at beginning.
;;       No longer use completing-prompt-prefix(-symb).
;;       No longer use icicle-reminder-prompt-flag (no longer add help to prompt).
;;     Removed: icicle-control-reminder-prompt.
;;     icicle-highlight-lighter:
;;       Combine faces for highlighting.  Indicate multi-command and strict/lax completion.
;;     icicle-unhighlight-lighter: Add + for multi-command.  Don't bother to propertize strg.
;;     icicle-file-remote-p: If name matches Windows drive letter, don't try other remote tests.
;; 2008/03/11 dadams
;;     icicle-completing-read-history:
;;       Convert cons list elements to strings.  Lets you use M-o with command-history.
;; 2008/03/09 dadams
;;     Added: icicle-(un)highlight-lighter.
;;     icicle-completing-read, icicle-read-file-name: Call icicle-highlight-lighter.
;; 2008/03/08 dadams
;;     icicle-completing-p: Replaced where-is-internal test with completion keymaps test.
;; 2008/03/05 dadams
;;     icicle-completing-read:
;;       Copy TABLE etc. only if candidates are strings.  Thx to Damon Permezel for bug report.
;;     icicle-files-within: Skip inaccessible directories and unreadable files.  Thx to Damon.
;; 2008/02/28 dadams
;;     icicle-completing-read: Fixed test for multicompletion: List of strings, not just list.
;; 2008/02/24 dadams
;;     Added: icicle-scatter(-match).
;;     icicle-display-*-in-Completions: Highlighting of input match can use icicle-scatter.
;;                                      Use icicle-apropos-complete-match-fn in message.
;;     icicle-expanded-common-match-1: Use icicle-apropos-complete-match-fn, not string-match.
;; 2008/02/23 dadams
;;     icicle-completing-read:
;;       Change initial-input to a cons with 0 position if icicle-init-value-flag is *-start.
;;     icicle-lisp-vanilla-completing-read: Set position to end of initial-input, by default.
;;                                          Convert zero-based position to one-based.
;; 2008/02/22 dadams
;;     icicle-completing-read:
;;       For all alist candidates (not just multi-completion):
;;         Copy the car and replace the cdr with the whole candidate.
;;     icicle-get-alist-candidate: Get and return just whole icicle-whole-candidate property.
;;     icicle-completion-setup-function (for Emacs 20):
;;       minibuffer-prompt-end -> icicle-minibuffer-prompt-end.
;; 2008/02/16 dadams
;;     icicle-get-alist-candidate:
;;       Reconstitute whole candidate, by consing string onto data, which is only the cdr now.
;;     icicle-completing-read: Use icicle-put-alist-candidate to put candidate data onto string.
;;     Added: icicle-put-alist-candidate.  Put only the cdr (data), not whole, onto the string.
;; 2008/02/14 dadams
;;     Added: icicle-substring-no-properties.
;; 2008/02/11 dadams
;;     icicle-read-string-completing: Moved save-match-data so replace-match can use match data.
;; 2008/02/06 dadams
;;     icicle-highlight-input-noncompletion:
;;       Wait icicle-highlight-input-completion-failure-delay before highlighting.
;;       Don't highlight if past icicle-highlight-input-completion-failure-threshold.
;;       Combined input-pending test with highlighting test, and moved it before file-name test.
;;       Conditionalized face removal.
;;       Return nil when input within delay preempts highlighting.
;;     icicle-highlight-initial-whitespace: Removed only vestigial whitespace highlighting.
;;     icicle-read-number: Moved save-match-data outside cond.
;; 2008/02/03 dadams
;;     icicle-choose-completion-string:
;;       Go to point-max before insert choice.  Respect icicle-dir-candidate-can-exit-p.
;;     icicle-completion-setup-function:
;;       Set default dir only if this is a completion cmd or not *-comp-base-is-default-dir-p.
;;       If icicle-comp-base-is-default-dir-p, set completion-base-size to default-dir length.
;;     icicle-read-file-name: Bind ffap vars to prevent slowing down ffap-guesser.
;; 2008/01/29 dadams
;;     Added: icicle-(apropos|prefix)-any(-file-name)-candidates-p,
;;            icicle-subst-envvar-in-file-name, icicle-highlight-input-noncompletion-rest,
;;            icicle-any-candidates-p, icicle-file-remote-p.
;;     icicle-minibuffer-contents-from-minibuffer: Use icicle-subst-envvar-in-file-name.
;;     icicle-call-then-update-Completions:
;;       Reinitialize icicle-input-fail-pos to nil.
;;       If we know input is a remote file name from failure highlighting, skip remote test.
;;       Use icicle-file-remote-p, not file-remote-p.
;;     icicle-highlight-input-noncompletion: Rewrote.
;;       Takes no args now.
;;       Do nothing if input pending or input is empty.
;;       Use icicle-file-remote-p, not file-remote-p.  Return value indicating remote file name.
;;       Use new values of icicle-highlight-input-completion-failure, including always.
;;       First check through last two chars, then icicle-highlight-input-noncompletion-rest.
;;     icicle-highlight-input-noncompletion-rest (was in icicle-highlight-input-noncompletion):
;;       No longer use icicle-max-chars-noncompletion-highlight.
;;       Use icicle-any-candidates-p, so don't compute candidate lists.
;; 2008/01/15 dadams
;;     icicle-command-abbrev-save: Added condition-case with message in case of error.
;;     icicle-control-reminder-prompt: Added message.
;; 2008/01/13 dadams
;;     icicle-read-face-name: Use icicle-transform-multi-completion.
;;     icicle-next-candidate: Do not use icicle-transform-multi-completion.
;;     icicle-next-candidate, icicle-save-or-restore-input: Do not treat handle-switch-frame.
;; 2008/01/08 dadams
;;     Renamed icicle-expanded-common-match to icicle-expanded-common-match-1.
;;     New icicle-expanded-common-match uses longest of two tries.
;;     icicle-expanded-common-match-1, first loop: Match ecm only up to orig-match-end.
;;     icicle-read-file-name: Use icicle-var-is-of-type-p for proxy candidates.
;;     icicle-choose-completion-string, icicle-strip-ignored-files-and-sort,
;;       icicle-filter-wo-input, icicle-first-matching-candidate,
;;       icicle-(proxy-candidate|prefix-keys)-first-p,  icicle-var-(matches|inherits)-type-p,
;;       icicle-read-(number|face-name|file-name|string-completing),
;;       icicle-unsorted(-file-name)-prefix-candidates, icicle-expanded-common-match-1,
;;       icicle-next-candidate, icicle-remove-dots: Wrap string-match in save-match-data.
;;     icicle-display-candidates-in-Completions: Moved save-match-data locations.
;; 2008/01/05 dadams
;;     icicle-next-candidate: Don't refresh Completions if last-command = handle-switch-frame.
;; 2008/01/04 dadams
;;     icicle-insert-candidates: Put property icicle-keep-newline on final \n of candidate.
;; 2007/12/31 dadams
;;     icicle-choose-completion-string: In minibuffer, delete all input before inserting.
;; 2007/12/27 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates:
;;       Use null *-apropos-*-match-fn, not function-valued TABLE to inhibit input match.
;; 2007/12/26 dadams
;;     icicle-next-candidate: Raise *Completions* frame at the end.
;; 2007/12/24 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates:
;;       Don't match input regexp if minibuffer-completion-table is a function.
;;     icicle-var-inherits-type-p:
;;       Recheck var-type match after set var-type to its car.
;;       Handle string (regexp) TYPES elements.
;;     icicle-value-satisfies-type-p: Skip type check for string type (regexp).
;;     icicle-var-is-of-type-p: Doc string.  Use icicle-var-matches-type-p.
;;     Added: icicle-var-matches-type-p, icicle-custom-type.
;; 2007/12/23 dadams
;;     icicle-var-is-of-type-p:
;;       Added MODE arg.  Use icicle-var-inherits-type-p, icicle-var-val-satisfies-type-p.
;;       Redefined as MODE choice, not just a simple or.  Treat more cases.
;;     Added: icicle-var-inherits-type-p, icicle-var-val-satisfies-type-p,
;;            icicle-value-satisfies-type-p.
;;     icicle-read-(number|char-exclusive|string-completing|face-name):
;;       Don't fill icicle-proxy-candidates unless icicle-add-proxy-candidates-flag.
;;       Corrected doc string to refer to icicle-add-proxy-candidates-flag.
;; 2007/12/22 dadams
;;     icicle-var-is-of-type-p:
;;       Check supertypes also.  Use both :validate and :match.
;;       Wrap type check in condition-case. Use widget-put instead of plist-put.
;;     Added soft require of wid-edit+.el.
;; 2007/12/21 dadams
;;     icicle-var-is-of-type-p: Use :validate, not :match, for the test.
;; 2007/12/19 dadams
;;     icicle-display-candidates-in-Completions:
;;       Ensure icicle-last-input is non-nil in (file-name-directory icicle-last-input).
;; 2007/12/14 dadams
;;     icicle-fit-Completions-window:
;;       Don't try to get a property if it's not a symbol.  Thx to Mike Mattie.
;; 2007/12/11 dadams
;;     Added: icicle-read-char-exclusive.
;;     icicle-read-face-name: Include face-name vars as proxy candidates.
;; 2007/12/10 dadams
;;     icicle-highlight-input-noncompletion: Use face icicle-input-completion-fail-lax also.
;; 2007/12/09 dadams
;;     icicle-highlight-input-noncompletion: Respect icicle-max-chars-noncompletion-highlight.
;; 2007/12/08 dadams
;;     icicle-read-file-name:
;;       Include file-name variables as proxy candidates.  Reset icicle-proxy-candidates at end.
;;     icicle-read-number: float type is not defined before Emacs 22.
;;     icicle-read-string-completing:
;;       Set default to "" if nil, but only after completing-read.
;;       Set car of hist to var value, replacing var name.  Treat consp hist also.
;; 2007/12/03 dadams
;;     Renamed icicle-longest-common-match to icicle-expanded-common-match.
;; 2007/12/02 dadams
;;     Added: icicle-var-is-of-type-p.
;;     icicle-read-(number|string-completing):
;;       Use icicle-var-is-of-type-p, to handle compatible types.
;; 2007/11/30 dadams
;;     icicle-read-file-name, icicle-read-(number|string-completing),
;;       icicle-display-candidates-in-Completions, icicle-proxy-candidate-first-p:
;;         Use only membership in icicle-proxy-candidates, not icicle-proxy-candidate property.
;;     icicle-choose-completion-string:
;;       Condition for exit: could be no minibuffer-completion-table if extra or proxy cands.
;; 2007/11/29 dadams
;;     icicle-read-(number|string-completing):
;;       Treat icicle-add-proxy-candidates-flag and icicle-proxy-candidates.
;;     icicle-display-candidates-in-Completions: Treat empty icicle-completion-candidates.
;; 2007/11/27 dadams
;;     Added: icicle-read-number, icicle-read-string-completing.
;;     icicle-read-file-name: Remove icicle-proxy-candidate property in unwind-protect.
;; 2007/11/25 dadams
;;     Added: icicle-command-abbrev(save|-used-more-p).
;; 2007/11/24 dadams
;;     icicle-longest-common-match,  first loop: string-match -> not string-match.
;;                                               len-first -> length of lcm.
;;     Added: icicle-proxy-candidate-first-p.
;; 2007/11/18 dadams
;;     icicle(-unsorted)(-file-name)-(apropos|prefix)-candidates:
;;       Include also icicle-proxy-candidates.
;;       Move special and proxy candidates inside match filtering.
;;     icicle-display-candidates-in-Completions:
;;       Don't expand directory when highlighting special candidate.
;; 2007/11/02 dadams
;;     icicle-longest-common-match:
;;       First loop: (1) match against at most len-first, (2) put the match into the and test.
;; 2007/10/28 dadams
;;     icicle-display-candidates-in-Completions:
;;       Always highlight longest common match for prefix completion.
;;     icicle-unsorted(-file-name)-prefix-candidates, icicle-fuzzy-candidates:
;;       Set icicle-common-match-string, regardless of icicle-expand-input-*-flag.
;;     icicle-save-or-restore-input:
;;       Update to lcm even if no input change, if completion mode changed.
;;       Update to lcm if prefix completing, even if icicle-expand-input-*-flag is nil.
;;       Save input for C-l even if this command is an icicle-completing-command (?).
;;       Don't reset raw input to "" if cycling, so keep highlight in *Completions*.
;;     icicle-longest-common-match: Test prefix through embedded input each time.
;; 2007/10/26 dadams
;;     icicle-read-face-name (Emacs 22): Fix the treatment of default value.
;; 2007/10/22 dadams
;;     icicle-display-candidates-in-Completions:
;;       Highlight only regexp matching part of special candidates.
;;       Highlight special cands regardless of icicle-highlight-historical-candidates-flag.
;; 2007/10/02 dadams
;;     icicle-next-candidate:
;;       Apply abbreviate-file-name to file-name input.  Thx to Joonhwan Lee.
;; 2007/09/29 dadams
;;     Added: icicle-fuzzy-candidates.
;;     icicle-prefix-candidates: Treat fuzzy completion.
;;     icicle-display-candidates-in-Completions:
;;       For prefix completion, highlight literal input match in Completions.
;; 2007/09/26 dadams
;;     icicle-read-from-minibuffer: Removed keep-all arg - see also 2006/06/01.
;; 2007/09/22 dadamms
;;     icicle-completing-read, icicle-read-from-minibuffer:
;;       Ensure that init arg is string when get it from default arg via icicle-init-*-flag.
;; 2007/09/18 dadams
;;     icicle-call-then-update-Completions:
;;       Test icicle-current-completion-mode, not icicle-last-completion-command.
;; 2007/09/16 dadams
;;     icicle-call-then-update-Completions:
;;       Don't complete if file-remote-p.
;;       Highlight completion failure only if input > icicle-*-display-min-input-chars.
;;     icicle-highlight-input-noncompletion:
;;       Don't highlight unless also icicle-incremental-completion-flag.
;;       Call icicle-file-name-*-candidates for file-name input.
;;       Don't highlight if file-remote-p and reading file name.  Instead, remove highlighting.
;; 2007/09/14 dadams
;;     icicle-highlight-input-noncompletion, icicle-update-completions,
;;       icicle-recompute-candidates:
;;         Wrapped condition-case around candidates computation.
;; 2007/09/02 dadams
;;     icicle-display-candidates-in-Completions:
;;       Restore point and window point to start of candidates in *Completions*.
;; 2007/08/21 dadams
;;     icicle-highlight-input-noncompletion: Remove any vestigial highlighting on matched part.
;; 2007/08/19 dadams
;;     Added: icicle-lisp-vanilla-completing-read, icicle-highlight-input-noncompletion.
;;     icicle-completing-read:
;;       Allow reading and returning string candidates with properties:
;;         bind minibuffer-allow-text-properties.
;;       Put whole candidate on string as text property.
;;       Use icicle-lisp-vanilla-completing-read, not old-completing-read.
;;     icicle-call-then-update-Completions: Call icicle-highlight-input-noncompletion.
;;     icicle-get-alist-candidate:
;;       If icicle-whole-candidate-as-text-prop-p, try to get full candidate from text prop.
;; 2007/08/16 dadams
;;     icicle-insert-candidates: Don't reset text props if endpos > point.  Thx Chris Hecker.
;; 2007/08/14 dadams
;;     icicle-increment-cand-nb+signal-end: Removed audible bell - use visible bell only.
;; 2007/07/22 dadams
;;     icicle-read-face-name (Emacs 22 version):
;;       Revert multiple branch to not use icicle-make-face-candidate.
;;     Moved here from icicles-mode.el: icicle-completing-p.
;; 2007/07/06 dadams
;;     icicle-display-candidates-in-Completions: Leave cursor at start of candidates.
;; 2007/07/03 dadams
;;     icicle-save-or-restore-input:
;;       Add current-raw-input to icicle-previous(-non)-file-*-raw-inputs, respecting max len.
;;       Don't save input if current command is C-l or C-L.
;;       If don't save raw input, set it to empty string.
;;     Added: icicle-put-at-head.
;;     icicle-highlight-complete-input: Ensure no error treatment in call to search-forward.
;;     icicle-display-candidates-in-Completions:
;;       Ensure non-nil current(-raw)-input, for highlight.
;; 2007/06/23 dadams
;;     Added: icicle-completing-read-history.
;;     Moved here from icicles-cmd.el: icicle-read-from-minibuf-nil-default.
;; 2007/06/20 dadams
;;     icicle-make-face-candidate, icicle-read-face-name:
;;       Use new string value of icicle-WYSIWYG-Completions-flag.
;; 2007/06/19 dadams
;;     icicle-read-face-name:
;;       Use a multi-completion, depending on icicle-WYSIWYG-Completions-flag.
;;       For Emacs 22, isolate the multiple case and do nothing for it.
;;     icicle-make-face-candidate: Treat also whole-number value for icicle-WYSIWYG-*-flag.
;; 2007/06/17 dadams
;;     icicle-make-face-candidate: Respect icicle-WYSIWYG-Completions-flag.
;;     icicle-display-candidates-in-Completions: highlight saved candidates.
;;     icicle-place-overlay: Added priority arg.
;; 2007/06/13 dadams
;;     icicle-display-candidates-in-Completions:
;;       Upgrade icicle-incremental-completion-p only if redisplaying.  Thx Mark Elston.
;; 2007/06/10 dadams
;;     Removed unconditional add-hook for icicle-fit-Completions-window.
;; 2007/06/09 dadams
;;     icicle-insert-candidates: Don't mouse-face last char of candidate if it is a newline.
;;     icicle-display-candidates-in-Completions: Treat icicle-candidate-properties-alist last.
;; 2007/06/07 dadams
;;     icicle-read-face-name: Use (icicle-)face-name-history.
;; 2007/06/05 dadams
;;     Added soft require of hexrgb.el, but only if window-system.
;;     icicle-color*: Protected with featurep hexrgb and error message.
;; 2007/06/01 dadams
;;     icicle-completing-read, icicle-read-from-minibuffer, icicle-read-string:
;;       Use M@R%M=X!L$S+P&L^T*Z to avoid name capture by minibuffer-history-variable's value.
;;     icicle-display-candidates-in-Completions, icicle-historical-alphabetic-p,
;;       icicle-most-recent-first-p:
;;         Ensure value of minibuffer-history-variable is bound.
;; 2007/05/29 dadams
;;     icicle-call-then-update-Completions: Don't recomplete if only one candidate.
;; 2007/05/24 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Fix length test for consp initial-input.  Thx to Anupam Sengupta.
;; 2007/05/04 dadams
;;     icicle-unsorted-*: C-g quits to top-level.
;;     icicle-candidate-set-1, icicle-scroll-or-update-*, icicle-msg-maybe-in-minibuffer:
;;       Move to minibuffer window for minibuffer-message.
;;     icicle-display-candidates-in-Completions, icicle-highlight-initial-whitespace,
;;     icicle-unsorted-file-name-*-candidates, icicle-longest-common-match,
;;     icicle-next-candidate, icicle-place-cursor, icicle-highlight-complete-input,
;;       icicle-special-candidates-first-p, icicle-case-string-less-p:
;;         Use read-file-name-completion-ignore-case, if completing file name.
;;     Moved mention of read-file-name-completion-ignore-case and
;;       icicle-cycle-into-subdirs-flag from icicle-completing-read to icicle-read-file-name.
;;     Added empty defvars for Emacs 22 standard vars, to quiet byte compiler.
;; 2007/04/29 dadams
;;     Added: icicle-last-modified-first-p.
;;     icicle-call-then-update-Completions: Delete icicle-complete-input-overlay.
;; 2007/04/08 dadams
;;     Added: icicle-highlight-candidate-in-Completions, from code in icicle-next-candidate.
;;            But changed to: 1) make Completions dedicated and 2) not bind case-fold-search.
;;     icicle-next-candidate: Use icicle-highlight-candidate-in-Completions.
;; 2007/04/02 dadams
;;     Moved here from icicles-cmd.el: icicle-filter-alist, icicle-first-matching-candidate.
;; 2007/04/01 dadams
;;     icicle-insert-candidates: Don't fixup-whitespace if bolp.
;; 2007/03/30 dadams
;;     icicle-fit-Completions-window: Don't resize *Completions* window if split horizontally.
;;     icicle-insert-candidates:
;;       Calculate nb of columns using max-candidate-len, not number of candidates.
;;       Indent at least one colwidth, and leave less space between candidates.
;; 2007/03/26 dadams
;;     icicle-completion-setup-function:
;;       Protected minibuffer-completing-symbol with boundp (not define in Emacs 23).
;; 2007/03/23 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Record require-match in icicle-require-match-p.
;; 2007/03/17 dadams
;;     icicle-read-face-name: Undo Emacs 21+ brain-dead treatment of PROMPT arg.
;; 2007/03/14 dadams
;;     icicle-fit-Completions-window:
;;       Don't let *Completions* take over the frame, so don't lose other window.
;;       Respect icicle-Completions-window-max-height property of top-level command.
;; 2007/03/12 dadams
;;     Added: icicle-fit-Completions-window.  Use in temp-buffer-show-hook.
;;     icicle-display-completion-list:
;;       Print help lines here, not in icicle-completion-setup-*, so window fit includes them.
;;       Put face on string candidates intro string, Possible completions are:.
;;     icicle-completion-setup-function:
;;       Don't print the help lines here.
;;       Updated wrt latest Emacs 22 CVS version.
;;     icicle-insert-Completions-help-string: Remove second newline at end.
;;     icicle-start-of-candidates-in-Completions: Advance 1 or 2 lines, not 0 or 3.
;; 2007/03/10 dadams
;;     icicle-display-completion-list: Rewrote to adjust columns to window width.
;;     Added: icicle-insert-candidates.
;; 2007/03/09 dadams
;;     Moved icicle-get-alist-candidate here from icicles-cmd.el.
;; 2007/03/07 dadams
;;     icicle-choose-completion-string, icicle-next-candidate:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/04 dadams
;;     icicle-highlight-initial-whitespace: Removed unused local var input-start-position.
;;     icicle-completing-read: Do not initialize icicle-candidates-alist.
;; 2007/03/03 dadams
;;     icicle-reversible-sort: Don't display Sorting candidates message - too annoying.
;; 2007/03/02 dadams
;;     icicle-completion-setup-function for Emacs 22: Don't use file-name-directory if nil.
;; 2007/03/01 dadams
;;     icicle-completing-read: Initialize icicle-candidates-alist.
;; 2007/02/24 dadams
;;     icicle-next-candidate:
;;       Transform multi-completion icicle-last-completion-candidate.
;;       If last-command is icicle(mouse)-remove-candidate don't reset common match string or
;;         redisplay *Completions*.
;;     icicle-recompute-candidates:
;;       Don't recompute if icicle-last-completion-command is icicle-mouse-remove-candidate.
;; 2007/02/18 dadams
;;     icicle-save-or-restore-input: Use "" if file-name-directory is nil.  Thx Shreevatsa R.
;; 2007/02/17 dadams
;;     icicle-reversible-sort: No Sorting... message if icicle-edit-update-p.  Thx Shreevatsa.
;; 2007/02/05 dadams
;;     icicle-completing-read: Added info about multi-completions to doc string.
;; 2007/02/04 dadams
;;     icicle-display-candidates-in-Completions: Fixed composition of multiple faces.
;; 2007/02/03 dadams
;;     Renamed icicle-icompleting-p to icicle-edit-update-p.
;;     Require icicles-var.el.  Removed eval-when-compile for require of icicles-opt.
;; 2007/02/02 dadams
;;     icicle-case-string-less-p: Use var, not function, icicle-completing-p (else too slow).
;;     icicle-reversible-sort: Added sorting progress message.
;; 2007/01/29 dadams
;;     icicle-display-candidates-in-Completions:
;;       Allow for consp proplist value of icicle-special-candidate.
;;     icicle-special-candidates-first-p: Added neither-special case.  Treat letter case.
;;     Renamed: icicle-case-insensitive-string-lessp to icicle-case-insensitive-string-less-p.
;;     Added: icicle-case-string-less-p.
;;     icicle-historical-alphabetic-p, icicle-most-recent-first-p, icicle-dirs-last-p,
;;       icicle-part-N-lessp, icicle-prefix-keys-first-p:
;;         Use icicle-case-string-less-p, not string-lessp.
;;     icicle-prefix-keys-first-p: Ignore case.
;; 2007/01/28 dadams
;;     Added: icicle-command-names-alphabetic-p.
;;     Moved here from icicles-cmd.el:
;;       icicle-prefix-keys-first-p, icicle-special-candidates-first-p.
;; 2007/01/23 dadams
;;     Added: icicle-read-face-name, icicle-make-face-candidate,
;;            icicle-face-valid-attribute-values, icicle-color-name-w-bg.
;;     icicle-choose-completion-string: Added Emacs 21 version.
;;     icicle-display-candidates-in-Completions:
;;       Only highlight past inputs if icicle-highlight-historical-candidates-flag.
;; 2007/01/22 dadams
;;     icicle-part-N-lessp, icicle-color-*-lessp: Do nothing if strings are not multipart.
;;     icicle-display-candidates-in-Completions:
;;       Highlight past inputs after treat *-prop*-alist.
;;     icicle-delete-whitespace-from-string: Added optional args.
;; 2007/01/21 dadams
;;     Added: icicle-part-*-lessp, icicle-color-*-lessp.
;; 2007/01/20 dadams
;;     Added: icicle-display-completion-list.
;; 2007/01/19 dadams
;;     icicle-display-candidates-in-Completions: Treat icicle-candidate-properties-alist.
;; 2007/01/15 dadams
;;     Added: icicle-reversible-sort.  Use it where standard sort function was used.
;;     Renamed: icicle-sort-and-strip-ignored to icicle-strip-ignored-files-and-sort,
;;              icicle-sort-dirs-last to icicle-dirs-last-p,
;;              icicle-sort-case-insensitively to icicle-case-insensitive-string-lessp.
;;     Grouped sort functions together.
;; 2007/01/14 dadams
;;     icicle-next-candidate: Use icicle-transform-multi-completion.  Thx to Rubikitch.
;;     icicle-transform-candidates: Updated doc string.
;; 2007/01/12 dadams
;;     icicle-next-candidate: Use icicle-list-use-nth-parts.  Thx to Rubikitch.
;;     icicle-display-candidates-in-Completions: Added message when no-display-p.
;; 2007/01/07 dadams
;;     icicle-completing-read: Updated doc string for Emacs 22.
;; 2007/01/06 dadams
;;     Added: icicle-abbreviate-or-expand-file-name.
;;     icicle-fix-default-directory: Use icicle-abbreviate-or-expand-file-name.
;;     icicle-save-or-restore-input: expand-file-name -> icicle-abbreviate-or-expand-file-name.
;;     icicle-completion-setup-function: Don't set default-directory to nil if minibuf empty.
;;     icicle-read-file-name: Bug fix: Don't set initial-input to icicle-initial-value if "".
;; 2007/01/05 dadams
;;     icicle-completing-read, icicle-read-file-name:
;;       Use existing string value of icicle-initial-value.  Thx to rubikitch for suggestion.
;; 2007/01/01 dadams
;;     Added assq-delete-all for Emacs 20 (moved here from icicles-mode.el).
;;     Added: icicle-assoc-delete-all.
;; 2006/12/25 dadams
;;     Added: icicle-most-recent-first-p.
;;     icicle-update-completions: Added optional no-display arg.
;;     Moved here from icicles-opt.el: icicle-historical-alphabetic-p.
;; 2006/11/10 dadams
;;     icicle-completing-read, icicle-read-file-name: Prefix prompt by + if a multi-command.
;; 2006/10/15 dadams
;;     icicle-save-or-restore-input:
;;       Change test from cmd is same as last to input is same as last.
;;     icicle-rebind-completion-maps:
;;       When turn off, bind C-M-mouse-2 and C-down-mouse-2 to nil.
;;     icicle-display-candidates-in-Completions: Accumulate (merge) highlight faces.
;;     Moved to icicles-mode.el:
;;       icicle-bind-isearch-keys, icicle-rebind-completion-maps,
;;       icicle-(redefine|restore)-standard-(commands|options),
;;       icicle-(redefine|restore)-std-completion-fns), icicle-(re|un)map,
;;       icicle-(bind|restore)-completion-keys, icicle-minibuffer-setup,
;;       icicle-cancel-*Help*-redirection, icicle-activate-mark,
;;       icicle-run-icicle-(pre|post)-command-hook, icicle-set-calling-cmd,
;;       icicle-undo-std-completion-faces, icicle-update-ignored-extensions-regexp,
;;       icicle-completing-p, icicle-restore-region-face.
;;     Removed eval-when-compile of *-face, *-var, *-mac, *-cmd.
;;     Removed some defvars for quieting byte compiler.
;; 2006/10/05 dadams
;;     icicle-display-candidates-in-Completions: Highlight candidates that are special.
;; 2006/10/03 dadams
;;     icicle-display-candidates-in-Completions:
;;       Removed predicate filtering, as the pred doesn't necessarily apply to the candidate.
;;       This has been in the code forever, so commented it out, in case it is needed ;-).
;; 2006/10/01 dadams
;;     icicle-alternative-sort -> icicle-toggle-alternative-sorting.
;;     icicle-update-completions: Treat icicle-prefix-word-complete case too.
;; 2006/09/30 dadams
;;     Added: icicle-key-description.
;;     icicle-(bind|restore)-completion-keys:
;;       Bind icicle-candidate-set-(save|retrieve) to C-M-(<|>), not C-(<|>).
;;       Bind icicle-toggle-angle-brackets to C-<.
;;       No longer remap help-command to icicle-completion-help.
;;       Bind icicle-completion-help to C-?.
;;       Rename [menu-bar minibuf C-h] to [menu-bar minibuf completion-help].
;;     icicle-completing-p: Bug fix: Use where-is-internal, not minibuffer-completion-table.
;; 2006/09/22 dadams
;;     icicle-minibuffer-setup:
;;       Apropos-complete, don't prefix-complete, when icicle-show-Completions-initially-flag.
;; 2006/09/17 dadams
;;     icicle-completing-p: Ensure minibuffer is active too.
;; 2006/09/16 dadams
;;     Bound icicle-insert-key-description to M-q.
;;     icicle-completing-read:
;;       Use icicle-list-join-string only to join parts of candidate (alist key).
;;       Append icicle-list-end-string instead.
;;     icicle-msg-maybe-in-minibuffer: Fixed doc string (active -> inactive).
;; 2006/09/12 dadams
;;     icicle-minibuffer-setup: Set icicle-pre-minibuffer-buffer.
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;; 2006/09/03 dadams
;;     Renamed icicle-show-Completions-help to icicle-show-Completions-help-flag.
;; 2006/08/27 dadams
;;     Bind Quit in Minibuf menu to icicle-abort-minibuffer-input.
;; 2006/08/22 dadams
;;     icicle-save-or-restore-input: If icicle-last-completion-candidate is nil, don't try to restore.
;; 2006/08/18 dadams
;;     icicle-minibuffer-setup: Reset icicle-last-completion-candidate to nil.
;;     icicle-rebind-completion-maps:
;;       Added icicle-Info-goto-node to icicle-completion-help-string.
;; 2006/08/15 dadams
;;     icicle-(bind|restore)-completion-keys:
;;       Bind icicle-help-on-(previous|next)-(apropos|prefix)-candidate.
;;       Reorder bindings.  Bind C-mouse-2 to 'ignore, not nil.
;;     icicle-rebind-completion-maps: Bind icicle-help-on-* in completion-list-mode-map.
;;     Added: icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     icicle-completing-read, icicle-read-file-name: Use icicle-completing*-prompt-prefix.
;; 2006/08/04 dadams
;;     icicle-call-then-update-Completions:
;;       Call icicle-last-completion-command, not just prefix or apropos (so prefix-word too).
;;     icicle-completing-read, icicle-read-file-name, icicle-next-candidate,
;;       icicle-recompute-candidates, icicle-call-then-update-Completions:
;;         Use icicle-remove-Completions-window.
;;     icicle-(bind|restore)-completion-keys: Bound icicle-pp-eval-expression to M-:.
;; 2006/08/03 dadams
;;     icicle-completion-setup-function: Removed useless highlighting code at end (Emacs 20).
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string.
;; 2006/07/30 dadams
;;     icicle-call-then-update-Completions: save match-data.
;; 2006/07/29 dadams
;;     icicle-activate-mark: Do it only if icicle-completing-p.  Thx to Le Wang.
;;     icicle-rebind-completion-maps:
;;       Updated to use icicle-dispatch-C-..
;;       Added icicle-toggle-search-cleanup to icicle-completion-help-string.
;;     icicle-bind-completion-keys:
;;       Use icicle-dispatch-C-. instead of icicle-toggle-ignored-extensions.
;; 2006/07/28 dadams
;;     icicle-longest-common-match: Treat special case of input such as "$" or "\\>$".
;; 2006/07/24 dadams
;;     icicle-call-then-update-Completions: Deactivate mark at the end.  Thx to Le Wang.
;; 2006/07/23 dadams
;;     Added: icicle-transform-candidates.
;;     icicle-rebind-completion-maps, icicle-(bind|restore)-completion-keys:
;;       Added icicle-toggle-transforming.
;;     icicle-unsorted(-file-name)-*-candidates: Use icicle-transform-candidates.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added icicle-insert-help-string, icicle-start-of-completions (factored from existing).
;;       icicle-completion-setup-function: Use icicle-insert-help-string.
;;       icicle-display-candidates-in-Completions:
;;         Use icicle-start-of-completions, and adjust loop accordingly.
;;       icicle-minibuffer-setup:
;;         Reset icicle-current-completion-type.
;;         Bind (up|down) to icicle-*-context-candidate, not (previous|next)-history-element.
;;       icicle-next-candidate: Use icicle-start-of-completions.
;;       icicle-scroll-or-update-Completions: Use icicle-scroll-completions.
;;     Renamed: icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;              icicle-insert-help-string to icicle-insert-Completions-help-string,
;;              icicle-current-completion-type to icicle-current-completion-mode,
;;              icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;              icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-minibuffer-setup:
;;       Replaced icicle-display-Completions with icicle-prefix-complete, to get initial
;;         highlight.
;; 2006/07/18 dadams
;;     icicle-call-then-update-Completions:
;;       Delete *Completions* window, depending on icicle-Completions-display-min-input-chars.
;;         Thx to Damien Elmes.
;;     icicle-rebind-completion-maps: Add icicle-toggle-case-sensitivity to help list.
;;     icicle-bind-completion-keys: Bind icicle-toggle-case-sensitivity to S-C-a (i.e. C-A).
;; 2006/07/17 dadams
;;     icicle-call-then-update-Completions: sit-for delay if no candidates.  Thx Damien Elmes.
;; 2006/07/09 dadams
;;     icicle-save-or-restore-input:
;;       Put back test: current input differs from last cycling candidate (user has edited it).
;;     icicle-next-candidate: Removed filtering with predicate (vestigial cruft).
;; 2006/07/08 dadams
;;     icicle-save-or-restore-input: Restore if currently cycling, not if not completing.
;; 2006/07/07 dadams
;;     icicle-display-candidates-in-Completions: Fixed test for historical candidate.
;;     Bound icicle-alternative-sort to M-,.  Updated icicle-completion-help-string.
;; 2006/07/05 dadams
;;     icicle-save-or-restore-input:
;;       For restoring: 1) No longer test if current input = *-last-completion-candidate.
;;                      2) No longer test if current input = icicle-initial-value.
;;       No longer save icicle-current-input as icicle-last-completion-candidate.
;;       Simplified the code.
;;     icicle-call-then-update-Completions: Do not set this-command or last-command.
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-unsorted(-file-name)-prefix-candidates: Update icicle-common-match-string.
;;     icicle-unsorted-file-name-prefix-candidates:
;;       If prefix matches an empty directory, then use that directory as the sole completion.
;;     icicle-next-candidate: Use icicle-*-cycling-command properties.
;;                            Removed regexp-p arg in calls to icicle-save-or-restore-input.
;;     icicle-save-or-restore-input:
;;       Update icicle-common-*-string and icicle-current-regexp-input even if not regexp-p.
;;       Removed optional regexp-p argument.
;;       Do not update icicle-last-completion-candidate.
;;       Use icicle-*-*ing-command properties.
;;     icicle-recompute-candidates: Use icicle-*-cycling-command properties.
;; 2006/07/03 dadams
;;     Bug fixes -
;;       icicle-next-candidate:
;;         Don't reset icicle-common-match-string if this is an apropos cycling command
;;           and last command was an apropos command (cycling or completing).
;;         Do icicle-save-or-restore-input a second time, after recompute candidates,
;;           to pick up the common match.
;;         Always pass icicle-current-input to icicle-place-cursor.
;;       icicle-save-or-restore-input:
;;         Don't do anything if last command was a cycling command.
;;         Don't save input as regexp for C-l if this command is a cycling command,
;;           unless it is the first or it follows a completion command.
;; 2006/07/02 dadams
;;     icicle-place-cursor: position point & mark at least past prompt.  Thx to Peter Povinec.
;; 2006/06/09 dadams
;;     icicle(-file-name)-(apropos|prefix)-candidates: Reset icicle-candidate-nb to nil.
;;     icicle-recompute-candidates: Don't reset icicle-candidate-nb to nil.
;;     icicle-place-cursor: Prevent error on search-forward.
;; 2006/06/08 dadams
;;     icicle-save-or-restore-input: Do not restore if current command is completion.
;;     Added: icicle-expand-file-name.
;;     icicle-next-candidate: Don't pass NTH arg to icicle-display-candidates-in-Completions.
;; 2006/06/06 dadams
;;     icicle-control-reminder-prompt: condition-case, since it's on kill-emacs-hook.
;; 2006/06/01 dadams
;;     icicle-read-from-minibuffer: Emacs 22 removed the keep-all arg it had added.
;; 2006/05/31 dadams
;;     icicle-barf-if-outside*: Simplified.
;; 2006/05/30 dadams
;;     Bind icicle-erase-minibuffer-or-history to M-k also in non-completion minibuffer maps.
;; 2006/05/26 dadams
;;     Bind icicle-erase-minibuffer-or-history to M-k.
;;     Do not remap (or unmap) kill-sentence (it is on M-k in global map).
;; 2006/05/19 dadams
;;     Added: icicle-control-reminder-prompt.
;;     icicle-reminder-*-flag, icicle-read-file-name: Treat new values of icicle-reminder*.
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;; 2006/05/16 dadams
;;     icicle-recompute-candidates: Add new saved-last-input arg (replaces icicle-last-input).
;;     icicle-next-candidate: Pass saved old last input to icicle-recompute-candidates.
;; 2006/05/15 dadams
;;     Reverted change to icicle-unsorted(-file-name)-apropos-candidates,
;;       icicle-display-Completions: Use icicle-completion-nospace-flag, not nil.
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-toggle-incremental-completion: C-#, icicle-toggle-ignored-space-prefix: C-^.
;; 2006/05/13 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates, icicle-display-Completions:
;;       Use nil, not icicle-completion-nospace-flag.
;; 2006/05/12 dadams
;;     icicle-completion-help-string: Added faces and commands. Cleanup.
;;     Moved from icicles-cmd.el: icicle-barf-if-outside-*.
;; 2006/05/09 dadams
;;     icicle-display-*: Only issue Displaying... message when more candidates than threshold.
;; 2006/05/01 dadams
;;     icicle-save-or-restore-input: No-restore test is non-nil, not non-"", icicle-last-input.
;;     icicle-minibuffer-setup: Reset icicle-last-input to nil, not "".
;;     icicle-next-candidate: Highlight initial whitespace before underline root.
;; 2006/04/28 dadams
;;     icicle-save-or-restore-input:
;;       Restore empty input if it is not a file name.
;;       Don't expand empty common-match-string file-name input (it would lose trailing /).
;;     Added: icicle-highlight-initial-whitespace.
;;     icicle-next-candidate, icicle-call-then-update-Completions:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/14 dadams
;;     icicle-call-then-update-Completions: Call icicle-update-input-hook.
;;     Bound icicle-insert-string-from-variable to C-=.  Update icicle-completion-help-string.
;; 2006/04/09 dadams
;;     icicle-bind-completion-keys, icicle-minibuffer-setup:
;;       Deal with icicle-arrows-respect-completion-type-flag.
;;     icicle-display-candidates-in-Completions:
;;       Bug fix: regexp-quote common match when highlighting it.
;;     icicle-clear-minibuffer: Remove interactive spec.
;;     Moved to icicles-cmd.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/02 dadams
;;     Bound icicle-toggle-regexp-quote.
;; 2006/03/31 dadams
;;     icicle-next-candidate:
;;       Apply icicle-place-cursor to icicle-current-regexp-input if regexp-p.
;;     icicle-save-or-restore-input:
;;       Don't set icicle-current-regexp-input if this is a next-candidate action.
;; 2006/03/27 dadams
;;     icicle-place-overlay: Made generic: added args overlay, face, buffer, properties.
;; 2006/03/25 dadams
;;     icicle-call-then-update-Completions: Corrected use of icicle-incremental-completion*.
;; 2006/03/24 dadams
;;     Renamed icicle-expand-input-to-common-match to icicle-longest-common-match.  Rewrote it.
;;     icicle-call-then-update-Completions:
;;       Use icicle-incremental-completion-delay and -threshold.
;;     Mapped icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-expand-input-to-common-match:
;;       Return the longest common match.  Don't set icicle-common-match-string here.
;;     icicle-unsorted-*apropos-candidates: Set icicle-common-match-string here explicitly.
;;     Added: icicle-maybe-sort-and-strip-candidates.  Use in icicle-candidate-set-1.
;; 2006/03/22 dadams
;;     icicle-display-candidates-in-Completions:
;;       Removed root arg (always use icicle-current-input).
;;       Always highlight normal match part.
;;       Highlight common-match part if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input:
;;       Update regexp even if not icicle-expand-input-to-common-match-flag.
;;     icicle-recompute-candidates: If no candidates, then delete *Completions* window.
;;     icicle-next-candidate: Set default-directory only if icicle-file-name-input-p.
;;     Applied renamings of icicle-match-* faces.
;; 2006/03/21 dadams
;;     icicle-expand-input-to-common-match:
;;       Bug fixes:
;;         If no overlap between first and second candidates, then no common match.
;;         If no match with another candidate, then no common match.
;;         Input must match computed common match.
;;         When checking others, check only the added (pre|suf)fix, and reduce those as needed.
;;     icicle-save-or-restore-input:
;;       Bug fixes:
;;         When icicle-expand-input-to-common-match-flag, expand using directory from the
;;           input, not the default-directory.  Thx to cacher3.ericsson.net for report.
;;         Do test for case-only difference only when case-fold-search.
;;         If input is a directory (with slash), then use it as is.
;;         Save icicle-current-regexp-input if no icicle-common-match-string too.
;;     icicle-display-candidates-in-Completions: Use icicle-common-match-highlight-Completions.
;; 2006/03/20 dadams
;;     icicle-save-or-restore-input: Set icicle-current-regexp-input too.
;;                                   Corrected letter-case test.
;; 2006/03/19 dadams
;;     Added: icicle-expand-input-to-common-match.
;;     icicle-unsorted*-apropos-candidates:
;;       Set icicle-common-match-string if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input:
;;       Added regexp-p arg.  Update input to icicle-common-match-string if appropriate.
;;     icicle-next-candidate: Reset icicle-common-match-string.
;; 2006/03/17 dadams
;;     icicle-file-(read|writ)able-p: Put non-empty string condition first.
;;     Added: icicle-delete-whitespace-from-string.
;;     icicle-files-within: Moved here from icicle-cmd.el.
;; 2006/03/14 dadams
;;     Removed: icicle-reset-icicle-completing-p.
;;     icicle-completing-read, icicle-read-file-name: Removed icicle-icicle-completing-p.
;;     icicle-display-*: Added Displaying... message.
;; 2006/03/13 dadams
;;     Added: icicle-file-(read|writ)able-p.  Bound them to C-{ and C-} in minibuffer.
;;     icicle-rebind-completion-maps, icicle-bind-completion-keys: Added the new commands.
;;     icicle-recompute-candidates: Forgot icicle-keep-only-past-inputs in other branch.
;; 2006/03/10 dadams
;;     icicle-save-or-restore-input: Bug fix (thx Toby Cubitt) - Not relative to default dir.
;;       Use directory-file-name, so don't include /.
;;       Use file-name-nondirectory, not file-relative-name if not cycling into subdirs.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;;     Added new icicle-minibuffer-contents, which can be called outside minibuffer.
;; 2006/03/08 dadams
;;     icicle-place-overlay: Use new face, icicle-current-candidate-highlight.
;; 2006/03/05 dadams
;;     Bound icicle-toggle-incremental-completion to C-^ in minibuffer.
;;     Updated icicle-completion-help-string with C-^ binding.
;;     icicle-display-candidates-in-Completions:
;;       Allow for on-the-fly changes to icicle-incremental-completion-flag.
;; 2006/03/01 dadams
;;     Added: icicle-clear-minibuffer.  Use in icicle-next-candidate.
;; 2006/02/27 dadams
;;     icicle-call-then-update-Completions: Set last-command to fn arg.
 
;;;(@* "CHANGE LOG FOR `icicles-mac.el'")
;;
;; 2010/06/05 dadams
;;     icicle-file-bindings: Put back non-insertion for non-ido-like case.
;; 2010/06/04 dadams
;;     icicle-(buffer|file)-bindings: Bind vars that depend on icicle-(buffers|files)-ido-like-flag.
;; 2010/03/03 dadams
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer,
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist.
;; 2009/12/21 dadams
;;     fset -> defalias.
;; 2009/12/13 dadams
;;     icicle-define-sort-command: Add REVERSED to msg if reversed.
;; 2009/10/22 dadams
;;     icicle-define-file-command: Use icicle-file-name-directory, not file-name-directory.
;; 2009/09/17 dadams
;;     Added: icicle-file-bindings.
;; 2009/09/16 dadams
;;     Added: icicle-buffer-bindings.
;;     icicle-define(-file)-command: Macroexpand bindings, (e.g. to use icicle-buffer-bindings).
;; 2009/04/28 dadams
;;     Moved icicle-choose-action-for-type to icicles-fn.el and changed to a function.
;; 2009/04/26 dadams
;;     Added: icicle-choose-action-for-type, icicle-with-selected-window.
;; 2008/08/31 dadams
;;     icicle-define(-file)-command: Select window before call select-frame-set-input-focus.
;; 2008/08/18 dadams
;;     icicle-try-switch-buffer: Do nothing if icicle-inhibit-try-switch-buffer is non-nil.
;;     Use renaming from icicles-fn.el: icicle-complete-again-update.
;; 2008/03/29 dadams
;;     icicle-define(-file)-command: Do not call icicle-highlight-lighter.
;; 2008/03/09 dadams
;;     icicle-define(-file)-command: Call icicle-highlight-lighter.
;; 2007/11/25 dadams
;;     icicle-define(-file)-command:
;;       Bound minibuffer variables, so they are restored after action function
;;         (in case it uses minibuffer for completion).
;;       Return nil after, not before select-frame-set-input-focus.
;;       Added optional arg not-interactive-p.
;;     Quiet the byte compiler for Emacs versions before 22.
;; 2007/10/14 dadams
;;     icicle-define(-file)-command:
;;       Updated generated doc to reflect icicle-act-before-cycle-flag.
;; 2007/05/01 dadams
;;     icicle-define(-file)-command: Reset icicle-candidate-action-fn after reading input.
;; 2007/04/15 dadams
;;     icicle-define(-file)-command:
;;       Simplified action fn: Removed unwind-protect and outer condition-case,
;;       so don't return error msg now, and only set minibuf focus if succeed.
;;     icicle-define(-file)-command, icicle-try-switch-buffer: Removed "%s" from handlers.
;; 2007/02/06 dadams
;;     icicle-define(-file)-command: Mention mouse bindings in command doc strings.
;; 2007/01/15 dadams
;;     Added: icicle-define-sort-command.
;;     Updated font-lock-add-keywords.  Added lisp-indentation-hack (commented out).
;; 2007/01/06 dadams
;;     font-lock-add-keywords: 2 or 3, not 1 or 2, is index after add icicle-define-add-to-*.
;;                             Use lax matching, so no error if no match.
;; 2007/01/01 dadams
;;     Added: icicle-define-add-to-alist-command.
;;     Removed compile-time require of icicles-var.el.
;;     font-lock-add-keywords:
;;       "\\>[ \t'\(]*\\(\\sw+\\)?", not "\\s-+\\(\\sw\\(\\sw\\|\\s_\\)+\\)".
;;       Added icicle-define-add-to-alist-command.
;; 2006/10/14 dadams
;;     Require icicles-var.el.
;;     Moved conditional eval-when-compile to top level.
;; 2006/09/24 dadams
;;     icicle-define(-file)-command: Corrected bindings mentioned in doc strings.
;; 2006/08/27 dadams
;;     icicle-define(-file)-command: Ensure orig-window is live before using it.
;; 2006/08/23 dadams
;;     Added: icicle-try-switch-buffer.  Use it in icicle-define(-file)-command.
;; 2006/08/03 dadams
;;     icicle-define(-file)-command:
;;       (error (error-message-string...)) -> (error "%s" (error-message-string...)).
;; 2006/05/16 dadams
;;     icicle-define(-file)-command: Treat cases where user wiped out orig-buff or orig-window.
;; 2006/03/31 dadams
;;     icicle-define(-file)-command: Wrap action fn in unwind-protect to select minibuf frame.
;; 2006/03/11 dadams
;;     icicle-define-file-command: Expand file in directory of icicle-last-input.
;; 2006/03/08 dadams
;;     icicle-define(-file)-command: Bug fix (thx to TobyCubitt):
;;       Make sure icicle-candidate-action-fn runs FUNCTION in original buffer and window.
;; 2006/03/07 dadams
;;     icicle-define(-file)-command: Mention in doc string that BINDINGS are not in effect
;;       within icicle-candidate-action-fn.
 
;;;(@* "CHANGE LOG FOR `icicles-mcmd.el'")
;;
;; 2010/06/18 dadams
;;     icicle-nb-of-candidate-in-Completions(-horiz), icicle-move-to-next-completion:
;;       Replace icicle-completions-format by icicle-completions-format-internal.
;;     icicle-scroll-Completions: Do nothing if *Completions* is not displayed.
;; 2010/06/16 dadams
;;     Added: icicle-col-wise-cand-nb.
;;     Renamed icicle-nb-of-candidate-in-Completions to icicle-nb-of-cand-in-Completions-horiz.
;;     Rewrote icicle-nb-of-candidate-in-Completions to handle vertical format also.
;;     icicle-candidate-set-save-selected-1:
;;       After setting BEG to next face chg, if BEG > END then raise error.
;;       After setting END to previous fact chg, BEG > END then swap.
;;       After extending, if BEG > END then swap.
;; 2010/06/14 dadams
;;     icicle-narrow-candidates: Handle Emacs 23.2+: Use completing-read with read-file-name-internal.
;;     icicle-search-define-replacement:
;;       Wrap *-remove-Completions-window in save-selected-window.  Thx to M. Heerdegen.
;;     icicle-help-on-candidate-symbol: Show combined help for fns, vars, and faces (Emacs 22+).
;; 2010/06/11 dadams
;;     Added: icicle-make-directory.
;;     icicle-read+insert-file-name: Bind icicle-make-directory to C-c +.
;; 2010/06/08 dadams
;;     icicle-doremi-increment-max-candidates+: Plain C-u resets icicle-max-candidates to nil.
;; 2010/06/07 dadams
;;     icicle-exit-minibuffer: Do not fiddle with faces if not in minibuffer.  Thx to M. Heerdegen.
;;     icicle-prefix-complete-1:
;;       Sole completion case: Move removal of *Completions* before the top-level throw.
;; 2010/06/04 dadams
;;     Added: icicle-doremi-increment-max-candidates+.
;;     Renamed by adding +: icicle-doremi-increment-swank-(timeout|prefix-length).
;;     icicle-help-string-completion: Mention missing doremi toggle keys.
;;     icicle-doremi-zoom-Completions+: Show *Completions* if not shown.
;; 2010/05/15 dadams
;;     icicle-all-candidates-action-1: Bind *-minibuffer-message-ok-p, *-help-in-mode-line-flag to nil.
;; 2010/05/09 dadams
;;     Added: icicle-dispatch-M-_.
;;     Removed: icicle-dispatch-C-comma.
;;     icicle-help-string-completion:
;;       Removed extra arg to format (icicle-key-descriptions-use-<>-flag).  Corrected arg order.
;;       Updated to reflect binding changes.
;;     icicle-toggle-highlight-all-current: Focus to minibuffer before show msg.
;; 2010/05/04 dadams
;;     icicle-change-sort-order: Use save-selected-window.  Thx to Michael Heerdegen.
;;     icicle-apply-to-saved-candidate:
;;       Only call sit-for if current-message.  Added TYPE arg.  Thx to Michael H.
;; 2010/05/03 dadams
;;     icicle-toggle-remote-file-testing: Updated for Emacs 23.2+  Thx to Michael Albinus.
;; 2010/04/30 dadams
;;     icicle-(apropos|prefix)-complete-1: Run icicle-no-match-hook when no candidates.
;; 2010/04/28 dadams
;;     icicle-remove-cand-from-lists: Fix pred a la icicle-mctize-all.  Thx to M. Heerdegen.
;; 2010/04/27 dadams
;;     icicle-apply-to-saved-candidate: Added sit-for for non-C-u case, to see any msg displayed.
;;     icicle-help-on-candidate: Test value, not just boundp, of icicle-completing-keys-p.
;; 2010/04/26 dadams
;;     icicle-toggle-highlight-all-current:
;;       Save/restore cand nb.  Re-complete, rehighlight.  Go to current cand only if it is defined.
;; 2010/04/21 dadams
;;     Added: icicle-sit-for (Emacs 23+), so user input interrupts sit-for after C-u in minibuffer.
;; 2010/04/20 dadams
;;     Added: icicle-sort-by-directories-first.
;;     icicle-nb-of-candidate-in-Completions:
;;       Bind icicle-completions-format to nil (for calls to *-move-to-next-*).  Thx to M. Heerdegen.
;; 2010/04/13 dadams
;;     icicle-(apropos|prefix)-complete-1: Fixed recent file-name completion bugs.
;;       When icicle-whole-candidate-as-text-prop-p is t and icicle-expand-input-to-common-match-flag
;;         is nil, expand the input, unless it is a directory.
;;       Expand file-name input to the common match for the current candidate.
;; 2010/04/11 dadams
;;     icicle-(apropos|prefix)-complete-1: Fix last fix:
;;       Put whole-cand prop on sole candidate only if not a dir, and use icicle-expanded-common-match.
;; 2010/04/09 dadams
;;     icicle-(apropos|prefix)-complete-1: When sole candidate, set icicle-current-input to it.
;;       Needed, in order to get icicle-whole-candidate property when
;;       icicle-expand-input-to-common-match-flag is nil.  Thx to Michael Heerdegen.
;; 2010/03/13 dadams
;;     Added: icicle-toggle-show-multi-completion.
;; 2010/03/03 dadams
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist,
;;                        icicle-alternative-sort-function to icicle-alternative-sort-comparer,
;;                        icicle-last-sort-function to icicle-last-sort-comparer.
;; 2010/03/02 dadams
;;     icicle-remove-Completions-window: Do nothing unless *Completions* is shown.
;;     icicle-delete-windows-on: Do nothing unless buffer is visible.
;;                               Do not delete frame if it is the only one.
;; 2010/01/12 dadams
;;     icicle-mouse-choose-completion, icicle-insert-string-at-point,
;;       icicle-mouse-candidate-action-1, icicle-mouse-remove-candidate,
;;       icicle-mouse-candidate-read-fn-invoke, icicle-Completions-mouse-3-menu,
;;       icicle-mouse-save/unsave-candidate:
;;         set-buffer -> with-current-buffer.
;;     icicle-mouse-candidate-read-fn-invoke, icicle-Completions-mouse-3-menu,
;;       icicle-mouse-save/unsave-candidate:
;;         Removed unused local var BUFFER.
;;     icicle-mouse-choose-completion: Removed unused local var ORIG-BUFFER.
;; 2009/12/21 dadams
;;     icicle-narrow-candidates:
;;       Add fn to minibuffer-setup-hook to make the reference buffer be the new minibuffer.
;;     fset -> defalias.
;; 2009/12/13 dadams
;;     icicle-change-sort-order: Add REVERSED to msg when reversed.
;; 2009/11/27 dadams
;;     Added: icicle-doremi-increment-swank-(prefix-length|timeout).
;;     *-next-TAB-completion-method, *-prefix-complete-1: Handle swank completions too.
;;     *-next-TAB-completion-method: Bind icicle-doremi-increment-swank-(prefix-length|timeout).
;; 2009/11/26 dadams
;;     icicle-next-TAB-completion-method: Do not set icicle-inhibit-sort-p to t for fuzzy.
;; 2009/11/25 dadams
;;     Added: icicle-completions-format, icicle-row-wise-cand-nb.
;;     icicle-move-to-next-completion: Handle completions laid out vertically.
;; 2009/11/07 dadams
;;     Renamed Icicles doremi cmds (added +).  Applied other doremi cmd renamings (added +).
;; 2009/10/25 dadams
;;     icicle-prefix-complete-1: When sole cand, use the candidate, but without any dir.
;;     Renamed: icicle-next-apropos-match-function to icicle-next-S-TAB-completion-method,
;;              icicle-toggle-fuzzy-completion to icicle-next-TAB-completion-method (rewrote).
;;     icicle-(prefix|apropos)-complete-1: Updated no/sole msgs per new completion methods.
;;     icicle-Completions-mouse-3-menu: Updated with the new command names.
;;     Updated icicle-help-string-completion.
;; 2009/10/24 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Removed code treating empty dir via *-any-*-p and ("").
;;       When only one candidate, set *-last-completion-candidate to:
;;         If file-name completion:
;;           If empty input, the input; if dir candidate, input + /; else the sole candidate.
;;         Else the sole candidate.
;; 2009/10/22 dadams
;;     icicle-insert-input, icicle-candidate-action-1, icicle-keep-only-past-inputs,
;;       icicle-apropos-complete-and-(narrow|widen):
;;         Use icicle-file-name-directory, not file-name-directory.
;; 2009/10/21 dadams
;;     icicle-prefix-complete-1:
;;       For empty dir we use "" as pseudo-cand.  Ensure not "" when later test for / last char.
;; 2009/10/12 dadams
;;     Added: icicle-input-is-a-completion-p.
;;     icicle-minibuffer-complete-and-exit, icicle-input-is-a-completion-p:
;;       Use icicle-input-is-a-completion-p.
;;     icicle-prefix-complete-1:
;;       For file-name input: Set, and use, current input without substituting env vars.
;;                            When sole candidate ends in /, add a / to current input also.
;; 2009/09/26 dadams
;;     icicle-narrow-candidates(-with-predicate): Bind icicle-progressive-completing-p to t.
;; 2009/09/25 dadams
;;     icicle-prefix-complete-1, icicle-transform-sole-candidate:
;;       Use icicle-current-input, not (car icicle-completion-candidates).
;;       Don't set icicle-current-input to (car icicle-completion-candidates) if no-catch.
;; 2009/09/12 dadams
;;     icicle-delete-candidate-object: Message if no candidates, in non-ALLP case also.
;;     icicle-delete-candidate-object-1: Bind icicle-completion-candidates to save & restore it.
;;     icicle-candidate-action-1, icicle-remove-candidate-display-others,
;;       icicle-delete-candidate-object, icicle-help-on-candidate,
;;       icicle-candidate-read-fn-invoke:
;;         Bind icicle-help-in-mode-line-flag to nil, to avoid help-display delay.
;;     icicle-update-and-next: Do nothing if user hit another key and there are more candidates.
;; 2009/09/05 dadams
;;     icicle-narrow-candidates: Don't raise an error if no candidates.  E.g. C-~.
;;     Use backward and forward as the values of icicle-cycling-command prop for nav commands.
;;     Apply renaming of icicle-acting-on-next/prev (removed -p).
;;     icicle-successive-action: Bind it to value of icicle-cycling-command (nav direction).
;;     icicle-all-candidates-list-alt-action: Raise error if null icicle-completion-candidates.
;;     icicle-search-define-replacement: Prevent immediate incremental completion kicking in.
;;     icicle-(widen|narrow)-candidates(-with-predicate):
;;       Use literal text for (S-)TAB in error message, to avoid S-iso-*.
;; 2009/09/02 dadams
;;     icicle-(prefix|apropos)-complete-1, icicle-narrow-candidates(-with-predicate):
;;       Impose icicle-top-level-when-sole-completion-delay when *-flag is non-nil.
;; 2009/08/27 dadams
;;     icicle-goto/kill-failed-input: Do nothing if the overlay is nowhere.
;;     icicle-mouse-yank-secondary: If yank-secondary is defined, then pass prefix arg also.
;; 2009/08/23 dadams
;;     icicle-narrow-candidates-with-predicate: Added optional arg PREDICATE.
;; 2009/08/20 dadams
;;     icicle-successive-action: Don't call icicle-show-help-in-mode-line unless string arg.
;; 2009/08/11 dadams
;;     icicle-all-candidates-action-1: Added ALTP arg.  Use in call to *-candidate-action-1.
;;     icicle-all-candidates(-list)-alt-action: Use new ALTP arg in call with *-alt-action-fn.
;;     icicle-successive-action: Bind icicle-acting-on-next/prev-p around call to action fn.
;;     icicle(-mouse)-candidate-action-1:
;;       No longer bind icicle-* to selves.  You must do it in the action fn if you need it.
;;     icicle-help-string-completion: C-| -> M-| for Replace all.
;;     icicle-change-history-variable: Protect with boundp: *-populate-interactive-history-flag.
;;     Wrap require of icicles-var.el in eval-and-compile.
;; 2009/08/01 dadams
;;     icicle-change-history-variable:
;;       Add icicle-interactive-history to choices only if *-populate-interactive-history-flag.
;; 2009/07/29 dadams
;;     icicle-other-history:
;;       Call icicle-use-interactive-command-history only if Emacs 23+ and non-nil history.
;; 2009/07/27 dadams
;;     icicle-help-string-completion: Mention icicle-other-history.
;; 2009/07/26 dadams
;;     Added: icicle-change-history-variable, icicle-other-history,
;;            icicle-use-interactive-command-history.
;;     icicle-history:
;;       Use icicle-cycling-p instead of get icicle-cycling-command, to avoid completion cmds.
;; 2009/07/02 dadams
;;     icicle-candidate-set-save-1: If icicle-get-alist-candidate-function returns nil, use CAND
;; 2009/06/26 dadams
;;     icicle-doremi-zoom-Completions, icicle-doremi-candidate-width-factor:
;;       Use new key-list options, doremi-...-keys (not -key).  You will need the latest DoReMi.
;; 2009/06/18 dadams
;;     doremi-buffer-font-size: Changed increment (doremi-*-key) bindings to =, -, M-=, M-i.
;;     icicle-help-string-completion: Added icicle-doremi-zoom-Completions.
;; 2009/06/17 dadams
;;     Added: icicle-doremi-zoom-Completions.
;;     icicle-doremi-candidate-width-factor: If no candidates, show message; don't use colors.
;; 2009/06/07 dadams
;;     icicle-get-alist-candidate -> funcall icicle-get-alist-candidate-function.
;; 2009/05/27 dadams
;;     icicle-minibuffer-complete-and-exit:
;;       Don't exit if any completion is done, unless icicle-require-match-p is t.
;; 2009/05/22 dadams
;;     Require icicles-mac.el if load-library doesn't find it.
;; 2009/05/18 dadams
;;     icicle-candidate-set-save-selected-1: If empty inactive region and MOREP, raise error.
;; 2009/05/17 dadams
;;     icicle-toggle-C-for-actions: Use icicle-toggle-icicle-mode-twice, not icy-mode calls.
;; 2009/05/15 dadams
;;     icicle-(prefix|apropos)-complete-1: Fit minibuffer frame after inserting current input.
;; 2009/05/11 dadams
;;     icicle-upcase-if-ignore-case, icicle-next-apropos-match-function: Use icicle-upcase.
;; 2009/05/09 dadams
;;     Added: icicle-looking(-back)-at-anychar-regexp-p, icicle-(forward|backward)-char-dots,
;;            icicle-backward-delete-char-untabify-dots, icicle-delete-backward-char-dots,
;;            icicle-delete-char-dots, icicle-transpose-chars-dots. icicle-insert-dot(-command),
;;            icicle-anychar-regexp, icicle-toggle-dot, icicle-convert-dots.
;;     icicle-backward-delete-char-untabify, icicle-delete-backward-char, icicle-delete-char,
;;       icicle-transpose-chars: Handle dots.
;;     icicle-prefix-complete: Convert dots and set icicle-dot-string-internal.
;;     icicle-goto/kill-failed-input: Use overlay.
;;     icicle-help-string-completion, icicle-Completions-mouse-3-menu:
;;       Added dot toggling.  Changed binding for icicle-toggle-hiding-common-match.
;; 2009/05/08 dadams
;;     icicle-(prefix|apropos)-complete-1: Fixed typo introduced 2009-05-05.
;; 2009/05/07 dadams
;;     icicle-retrieve-(previous|last)-input: Call icicle-place-cursor with optional second arg.
;; 2009/05/05 dadams
;;     toggle-icicle-case-sensitivity: Treat read-buffer-completion-ignore-case also (Emacs 23).
;; 2009/05/03 dadams
;;     icicle-(prefix|apropos)-complete-1:
;;       Don't stop cycling if last command was an action (treat as if it was a cycling cmd).
;;     icicle-all-candidates(-list)(-alt)-action, icicle(-mouse)-candidate(-alt)-action,
;;       icicle-delete-candidate-object, icicle(-mouse)-help-on-candidate:
;;         Put property icicle-action-command.
;; 2009/05/02 dadams
;;     icicle-candidate-alt-action: Respect icicle-alternative-actions-alist.
;;     icicle-minibuffer-help: Corrected wrt alt action-for-all keys.
;; 2009/04/30 dadams
;;     icicle-next-(prefix|apropos)-candidate, icicle-(prefix|apropos)-complete-1,
;;       icicle-successive-action:
;;         Reset icicle-next-(apropos|prefix)-complete-cycles-p to nil, as appropriate.
;;     icicle-prefix-complete-1: Show mode-line help only at very end, and even after cycling.
;; 2009/04/29 dadams
;;     icicle-apply-to-saved-candidate:
;;       Pass no-error-no-msg to icicle-get-alist-candidate function.
;;       If icicle-get-alist-candidate finds no function, get function from car of entry.
;;     icicle-apropos-complete-and-(narrow|widen): Handle icicle-apropos-complete-no-display.
;; 2009/04/27 dadams
;;     icicle-(prefix|apropos)-complete-1:
;;       Set icicle-last-completion-command to icicle-*-complete-no-display if arg NO-DISPLAY.
;;     icicle-history: If icicle-last-completion-command is nil, call icicle-apropos-complete.
;; 2009/04/26 dadams
;;     Wrap load of icicles-mac in eval-when-compile, and use load-library, not require.
;;     Require icicles-opt before icicles-var (not important).
;; 2009/04/20 dadams
;;     Added: icicle-(previous|next)-candidate-per-mode-alt-action.
;; 2009/03/19 dadams
;;     mouse-choose-completion: Don't fset unless standard function is defined.
;;     Use when/unless instead of or for fset's. (cosmetic)
;; 2009/04/15 dadams
;;     Added: icicle-(next|previous)-candidate-per-mode-help.
;;     icicle-successive-action: Set mode only if known.  Leave it alone for per-mode functions.
;;     icicle-(apropos|prefix)-complete-1:
;;       Don't show help in mode-line when icicle-top-level-when-sole-completion-flag = t.
;; 2009/04/12 dadams
;;     icicle-successive-action: Inhibit help in mode line until after action is finished.
;; 2009/04/06 dadams
;;     icicle-(prefix|apropos)-complete-1, icicle-candidate-set-retrieve-1,
;;       icicle-keep-only-past-inputs:
;;         Call icicle-show-help-in-mode-line for completed input.
;;     icicle-(prefix|apropos)-complete-1:
;;       Don't highlight complete input or show help if icicle-*-complete-and-exit-p is bound.
;; 2009/04/04 dadams
;;     Renamed: icicle-sort-by-last-use to icicle-sort-by-last-use-as-input.
;;     icicle-change-sort-order: Purge any nil entries from icicle-sort-functions-alist.
;;     icicle-current-sort-functions: Respect icicle-buffer-name-sort-predicate.
;;     icicle-reverse-sort-order: Echo icicle-current-sort-order.
;; 2009/03/16 dadams
;;     icicle-candidate-action-1: Respect icicle-use-candidates-only-once-alt-p.
;; 2009/03/10 dadams
;;     icicle-remove-Completions-window: Wrap with condition-case, to ignore "Attempt..." error.
;; 2009/03/01 dadams
;;     Added: icicle-completing-read+insert, icicle-read+insert-file-name.
;; 2009/02/28 dadams
;;     No soft require of subr-21.el now, since provide replace-regexp-in-string for Emacs 20.
;; 2009/02/23 dadams
;;     icicle-mouse-choose-completion, icicle-(prefix|apropos)-complete-1,
;;       icicle-insert-completion, icicle-mouse-candidate-action-1, icicle-update-and-next,
;;       icicle-narrow-candidates(-with-predicate), icicle-keep-only-past-inputs,
;;       icicle-insert-input:
;;         Protect dir insertion with icicle-extra-candidates-dir-insert-p.
;;     icicle-choose-completion: Use icicle-extra-candidates-dir-insert-p to reset base-size=0.
;; 2009/02/20 dadams
;;     Added: icicle-sort-extra-candidates-first.
;;     icicle-candidate-action-1: Use function icicle-require-match-p, not variable.
;; 2009/02/17 dadams
;;     icicle-exit-minibuffer: Don't put nil face property on input.  Thx to Daniel Clemente.
;; 2009/02/04 dadams
;;     icicle-(prefix|apropos)-complete-1:
;;       Do not set icicle-current-input to icicle-last-input unless also icicle-cycling-p.
;;       Do not cycle unless also was already cycling or icicle-next-*-complete-cycles-p.
;;       Set icicle-next-*-complete-cycles-p, at end, to save whether input was completed some.
;; 2009/02/01 dadams
;;     Added: icicle-up-directory, icicle-replace-input-w-parent-dir.
;; 2009/01/24 dadams
;;     icicle-narrow-candidates(-with-predicate):
;;       Bind icicle-apropos-complete-match-fn to icicle-last-apropos-complete-match-fn.
;;     icicle-next-apropos-match-function: Save new value as *-last-apropos-complete-match-fn.
;; 2009/01/18 dadams
;;     icicle-(prefix|apropos)-complete-1:
;;       Don't set icicle-current-input to icicle-last-input if icicle-edit-update-p.
;; 2009/01/17 dadams
;;     icicle-next-apropos-match-function:
;;       Add Levenshtein distance in message: (1).
;;       Don't do icicle-complete-again-update - too slow.
;; 2009/01/14 dadams
;;     icicle-remove-cand-from-lists: Treat icicle-remove-dups-if-extras also.
;; 2009/01/13 dadams
;;     icicle-delete-windows-on: Delete frame even if it has a minibuffer, if it's not active.
;; 2008/12/26 dadams
;;     Added: icicle-widen-candidates, icicle-apropos-complete-and-widen.
;;       Added icicle-widen-candidates to icicle-Completions-mouse-3-menu.
;; 2008/12/25 dadams
;;     icicle-retrieve-previous-input: Corrected logic following cycling.
;;       Remember whether repeated C-l calls follow cycling: local var was-cycling-p.
;;       If so, then use current raw input for second C-l.  Otherwise use previous raw input.
;;       Use icicle-cycling-p instead of testing if this command is a cycling command.
;;       Handle case of "" raw input, which is never in list of saved raw inputs.
;;       Restore current raw input after re-completing.
;;     icicle-(prefix|apropos)-complete-1: Reset icicle-cycling-p (new var) to nil.
;;     icicle-regexp-quote-input: Reset icicle-expand-input-to-common-match-flag to nil.
;; 2008/12/22 dadams
;;     Added: icicle-regexp-quote-input.
;;       Added it to icicle-help-string-completion, icicle-Completions-mouse-3-menu.
;; 2008/12/21 dadams
;;     icicle-minibuffer-complete-and-exit:
;;       Rewrote to fit new Emacs 23 behavior.  Thx to Daniel Clemente.
;;         Test minibuffer-completion-confirm first, before we auto-complete.
;;         icicle-last-input -> icicle-current-input (bug fix bc of call to *-no-display).
;;     icicle-prefix-complete-1: Bind free var word-complete-input.
;;     icicle-(apropos|prefix)-complete-1: No messages if NO-DISPLAY-P is 'no-msg.
;; 2008/12/20 dadams
;;     icicle-(apropos|prefix)-complete-1, icicle-narrow-candidates(-with-predicate):
;;       Expand file-name choice if thrown to icicle-read-top.
;; 2008/12/10 dadams
;;     icicle-(apropos|prefix)-complete-1: Don't pick up icicle-last-input unless also:
;;         (1) same completion mode, (2) icicle-completion-candidates is not nil.
;;     icicle-prefix-complete-1: If WORD-P, don't pick up icicle-last-input unless same command.
;;       Bind minibuffer-message-timeout to 0 during minibuffer-complete-word.
;;     icicle-apropos-complete-1:
;;       Typo in last cond clause: prefix -> apropos, in i-apropos-completing-command.
;; 2008/12/06 dadams
;;     icicle-prefix-complete-1: Fixes for word completion.
;;       Don't initialize *-current-input to *-last-input if word-p and icicle-edit-update-p.
;;       Recompute candidates after word complete also if editing or didn't repeat last command.
;;       Cycle if last command was not a prefix completion and input doesn't end in `-'.
;; 2008/12/05 dadams
;;     Let repeated completion commands cycle.  Thx to Andrey Zhdanov for the suggestion.
;;       icicle-(apropos| prefix)-complete-1:
;;         Call icicle-next-candidate, not scroll, when repeated.  Special treatment for word.
;;       icicle-(apropos|prefix)-complete*:
;;         Put icicle(-apropos|-prefix)-cycling-command property on command symbols.
;; 2008/12/02 dadams
;;     icicle-minibuffer-complete-and-exit:
;;       Updated for Emacs 23's minibuffer-confirm-exit-commands.
;; 2008/11/29 dadams
;;     icicle-prefix-word-complete:
;;       Redefined to use (new) icicle-prefix-complete-1.
;;       Put property icicle-completing-command.
;;     icicle-prefix-complete-1:
;;       Useful now also for icicle-prefix-word-complete.  Added arg word-p.
;;     icicle-(prefix|apropos)-complete-1, icicle-switch-to-Completions-buf:
;;       Test property icicle-prefix-completing-command, not eq cmds.
;;     icicle-(prefix|apropos)(-word)-complete(-no-display):
;;       Put property icicle-(prefix|apropos)-completing-command.
;;     icicle-next-candidate-per-mode: Use case, not cond.
;;     icicle-end-of-line+: Bind inhibit-field-text-motion, call end-of-line to get past prompt.
;; 2008/11/18 dadams
;;     icicle-minibuffer-complete-and-exit: Allow exit if input matches a current candidate.
;;     icicle-exit-minibuffer: Remove all Icicles minibuffer faces, but only those.
;; 2008/11/14 dadams
;;     Added: icicle-toggle-hiding-common-match.  
;;     icicle-help-string-completion, icicle-Completions-mouse-3-menu: Mention it.
;; 2008/11/10 dadams
;;     icicle-minibuffer-complete-and-exit:
;;       Use icicle-exit-minibuffer, not old-exit-minibuffer, so remove *Completions*.
;; 2008/11/09 dadams
;;     icicle-maybe-multi-completion-completing-p, icicle-transform-sole-candidate:
;;       Don't test icicle-list-join-string (always non-nil).
;; 2008/11/03 dadams
;;     icicle-(apropos|prefix)-complete-1: Added message Computing completion candidates...
;; 2008/11/02 dadams
;;     icicle-upcase-if-ignore-case: condition-case, to prevent error on bad chars (Emacs 20).
;; 2008/10/27 dadams
;;     Added: icicle-upcase-if-ignore-case.
;;     icicle-minibuffer-complete-and-exit, icicle-(prefix|apropos)-complete-1:
;;       Use icicle-upcase-if-ignore-case.
;; 2008/10/24 dadams
;;     icicle-minibuffer-complete-and-exit: If icicle-candidates-alist, then just filter it.
;; 2008/10/18 dadams
;;     Replaced customize-save-variable by funcall icicle-customize-save-variable-function.
;; 2008/10/14 dadams
;;     Added: icicle-help-string(-non)-completion.
;;     icicle-help-string-completion:
;;       Renamed from icicle-update-help-string in icicles-mode.el.
;;       Moved common part to icicles-var.el as icicle-general-help-string.
;;     Renamed: icicle-completion-help to icicle-minibuffer-help.
;;     icicle-minibuffer-help: If not completing, use icicle-help-string-non-completion.
;;                             Move Send an Icicles bug report to the bottom.
;; 2008/10/11 dadams
;;     icicle-(next|previous)-line: Fixed so it highlights also candidates in first column.
;;     icicle-kill-failed-input: Made it two-stage.  Renamed to icicle-goto/kill-failed-input.
;; 2008/10/10 dadams
;;     Added: icicle-(next|previous)-candidate-per-mode-action.
;; 2008/10/09 dadams
;;     Updated icicle-Completions-mouse-3-menu for C-<.
;; 2008/10/08 dadams
;;     Added: icicle-candidate-set-retrieve-more, icicle-candidate-set-retrieve-1.
;;     icicle-candidate-set-retrieve: Use icicle-candidate-set-retrieve-1.
;;     icicle-insert-string-at-point:
;;       Use icicle-pre-minibuffer-buffer, not (cadr (buffer-list)).  Thx to Andrey Zhdanov.
;;     icicle-beginning-of-line+: Don't move into prompt.  Thx to Andrey Zhdanov.
;; 2008/10/06 dadams
;;     icicle-self-insert: Do self-insert-command if executing-kbd-macro.  Thx to Tomer Levin.
;; 2008/10/01 dadams
;;     icicle-completion-help: Use icicle-update-help-string, not icicle-completion-help-string.
;; 2008/09/30 dadams
;;     Renamed icicle-isearch-complete-1 to icicle-isearch-complete-past-string and moved it
;;       to icicles-fn.el.
;; 2008/09/20 dadams
;;     icicle-toggle-ignored-extensions: Append $ to each extension.
;;     icicle-dispatch-C-.: Use icicle-searching-p as the condition, not *-file-name-input-p.
;; 2008/09/14 dadams
;;     icicle-(minibuffer|apropos)-complete-and-exit: Set icicle-last-input to current input.
;;     icicle-minibuffer-complete-and-exit: Use apropos completion if that's the current mode.
;; 2008/09/13 dadams
;;     icicle-candidate-set-save-1: Save to fileset if zero prefix arg.
;;     icicle-candidate-set-retrieve: Retrieve also from a fileset.
;;                                    No default value for completing-read.
;;     Added: icicle-add-file-to-fileset.
;;     Renamed:
;;       icicle-candidate-set-save-to-cache-file to icicle-candidate-set-save-persistently,
;;       icicle-candidate-set-retrieve-from-cache-file to *-candidate-set-retrieve-persistent.
;;     icicle-candidate-set-save-persistently: Added arg FILESETP.
;;     icicle-add/update-saved-completion-set: No default value for completing-read.
;;     icicle-retrieve-candidates-from-set:
;;       Factored out code as icicle-get-candidates-from-saved-set - use it.  Don't return name.
;;     Moved to icicles-fn.el: icicle-readable-to-markers.
;; 2008/09/09 dadams
;;     icicle-candidate-set-save(-selected-1): Added NO-ERROR-P arg.
;;     icicle-candidate-set-save-selected: Call *-save-selected-1 with NO-ERROR-P arg.
;;     icicle-candidate-set-save-1: Raise error if set to save is empty and not NO-ERROR-P.
;; 2008/09/08 dadams
;;     icicle-apropos-complete-and-narrow:
;;       If currently prefix completing, escape current input before apropos completing.
;; 2008/09/07 dadams
;;     icicle-minibuffer-complete-and-exit:
;;       Use *-prefix-complete-no-display and *-display-candidates-in-Completions when needed.
;; 2008/09/06 dadams
;;     icicle-minibuffer-complete-and-exit: Rewrote, based on icicle-apropos-complete-and-exit.
;;     icicle-prefix-complete-1:
;;       Wrap most of single-candidate case in (boundp 'icicle-prefix-complete-and-exit-p).
;; 2008/09/04 dadams
;;     icicle-minibuffer-complete-and-exit: Temporary bug workaround.
;; 2008/08/31 dadams
;;     icicle-completion-help, icicle-pp-eval-expression-in-minibuffer,
;;       icicle-delete-candidate-object-1, icicle-apply-to-saved-candidate,
;;       icicle-toggle-highlight-all-current:
;;         Select window before call select-frame-set-input-focus.
;; 2008/08/29 dadams
;;     icicle-minibuffer-complete-and-exit: Update icicle-last-input to minibuffer contents.
;; 2008/08/28 dadams
;;     icicle-(apropos|prefix)-complete-1, icicle-narrow-candidates(-with-predicate):
;;       Update minibuffer-history-variable before throw result.
;;     icicle-help-on-candidate: Renamed alacarte-menu-items-alist to lacarte-menu-items-alist.
;; 2008/08/27 dadams
;;     icicle-kill-failed-input: Reverted mistaken change to use start of *Completions* (duh).
;; 2008/08/25 dadams
;;     icicle-minibuffer-complete-and-exit: Call icicle-prefix-complete-no-display with no-msg
;;       arg, instead of binding minibuffer-message-timeout to 0.
;;     icicle-(apropos|prefix)-complete-no-display: Added optional NO-MSG-P arg.
;; 2008/08/24 dadams
;;     icicle-minibuffer-complete-and-exit: Rewrote to not call original Emacs version.
;;     Use today's renamings from icicles-fn.el.
;;     icicle-raise-Completions-frame: Don't do anything unless one-window-p and option = t.
;;     icicle-choose-completion, *-kill-failed-input, *-current-completion-in-Completions:
;;       Check point wrt candidates start position, not bobp.
;;     icicle-current-completion-in-Completions:
;;       "No completion here" error after, not before, we set beg wrt mouse-face change.
;;     icicle-mouse-candidate-action-1: Call icicle-update-and-next if there are still cands.
;; 2008/08/21 dadams
;;     icicle-candidate-set-retrieve: If completing files, remove directory from candidates.
;; 2008/08/20 dadams
;;     icicle-mouse-candidate-action-1: Remove mouse-face property from choice.
;; 2008/08/19 dadams
;;     icicle-mouse-remove-candidate:
;;       Set icicle-last-completion-candidate.  Needed for *-remove-candidate-display-others.
;;     icicle-mouse-candidate-action-1: Removed unused vars: buffer, base-size.
;;     Added: icicle-delete-candidate-object-1, with explicit CAND arg and NO-DISPLAY-P option.
;;     icicle-delete-candidate-object: Added optional ALLP arg.
;;     icicle-delete-current-candidate-object: Added optional CAND arg.
;;     icicle-update-and-next: Don't try to move to cand in *Completions* if number not known.
;; 2008/08/18 dadams
;;     Added: icicle-remove-cand-from-lists, icicle-update-and-next.
;;     icicle-narrow-candidates-with-predicate: Update predicate correctly, with lexical-let.
;;     icicle-remove-candidate, icicle-delete(-current)-candidate-object:
;;       Removed stuff from doc string about removing all that match.
;;     icicle-delete-candidate-object, icicle-remove-candidate-display-others:
;;       Use icicle-remove-cand-from-lists (with mctized candidate) and icicle-update-and-next.
;;     icicle-delete-current-candidate-object: Changed first condition:
;;       (and icicle-candidates-alist (consp (car val))) to
;;       (or icicle-whole-candidate-as-text-prop-p icicle-candidates-alist)
;;     icicle-remove-candidate-display-others:
;;       Added optional ALLP arg.
;;       Reinitialize icicle-last-completion-candidate properly (as elsewhere).
;;     icicle(-mouse)-candidate-action-1: Use ALLP arg for *-remove-candidate-display-others.
;;     icicle-history: Don't set minibuffer-completion-predicate if it is nil.
;;     Use renamings from icicles-fn.el:
;;       icicle-complete-again-update, icicle-remove-if, icicle-put-whole-cand-prop.
;;     icicle-narrow-candidates: Do not bind icicle-whole-candidate-as-text-prop-p to nil.
;;     icicle-Completions-mouse-3-menu: Removed icicle-scroll-Completions from menu.
;; 2008/08/17 dadams
;;     icicle-narrow-candidates-with-predicate:
;;       (read-file-name|minibuffer-completion)-predicate: Removed `, in front.
;;     icicle-scroll-Completions: Added optional arg (for mouse wheel reversal).
;;     Added: icicle-scroll-Completions-up.
;; 2008/08/03 dadams
;;     Added: icicle-all-candidates-list(-alt)-action.
;;     icicle-all-candidates(-alt)-action:
;;       Act on saved candidates, if any.  Use list function, if normal is nil.
;;     icicle-all-candidates-action-1:
;;       Use icicle-candidate-action-1 (with cand arg (new)), not funcall, to apply fn-var.
;;       Act on saved candidates, if any.
;;       Do not call icicle-abort-recursive-edit at the end.
;;     icicle-candidate-action-1:
;;       Added optional CAND arg.
;;       Save stuff before funcall, in case FN-VAR does its own completion.
;;     icicle-mouse-candidate-action-1: Save stuff before funcall, in case FN-VAR completes.
;;     icicle-remove-candidate-display-others:
;;       When no candidates left, just call icicle-abort-recursive-edit.
;;     icicle-help-on-candidate: Added optional CAND arg.
;;     Renamed: icicle-candidate-alternative-action-fn to icicle-candidate-alt-action-fn.
;; 2008/07/27 dadams
;;     Added: icicle-sort-by-2nd-parts-alphabetically, icicle-maybe-multi-completion-*.
;;     icicle-current-sort-functions: Treat icicle-multi-completion-sort-predicate property.
;;     icicle-dispatch-C-comma: Call icicle-change-sort-order interactively (bug fix).
;;     icicle-reverse-sort-order: Display candidates, possibly in reverse order (bug fix).
;;     Moved from here to icicles-fn.el: icicle-transform-multi-completion.
;; 2008/07/16 dadams
;;     icicle-mouse-choose-completion: No error if minibuffer is not active.
;;     icicle-completion-help and top-level: Don't require help-mode.el unless Emacs 22+.
;;     eval-when-compile require's of fit-frame.el, linkd.el.
;; 2008/06/24 dadams
;;     icicle-narrow-candidates: Emacs < 22: Set minibuffer-completing-file-name to nil.
;; 2008/06/21 dadams
;;     icicle(-mouse)-choose-completion, icicle-current-completion-in-Completions,
;;       icicle-mouse-candidate-action-1:
;;         buffer-substring-no-properties -> buffer-substring.
;;         *NOTE*: THIS REVERSES A FIX made on 2008/02/03.  Dunno why that fix was made.
;; 2008/06/03 dadams
;;     Added: icicle-toggle-C-for-actions.
;;     icicle-Completions-mouse-3-menu: Added icicle-toggle-C-for-actions.
;; 2008/06/01 dadams
;;     Added: icicle-current-sort-functions.  Predicate tests various contexts using properties.
;;     icicle-change-sort-order: Use icicle-current-sort-functions.
;;     Added new sort order, defining function icicle-sort-special-candidates-first.
;; 2008/05/27 dadams
;;     icicle-isearch-complete-1: Use ring symbols, not their values.
;; 2008/05/25 dadams
;;     icicle-isearch-complete: Rewrote.  Handle minibuffer search.
;;     Added: icicle-isearch-complete-1, from stuff in icicle-isearch-complete.
;;            Treat string case. Allow recursive minibuffers.  Don't use icicle-isearch-resume.
;;     Removed: icicle-isearch-resume.
;; 2008/05/22 dadams
;;     icicle-toggle-case-sensitivity: Added prefix arg and update lighter.
;; 2008/05/03 dadams
;;     icicle-history: Bind minibuffer-completing-file-name to nil.
;; 2008/05/02 dadams
;;     (put 'icicle(-mouse)-yank-secondary 'delete-selection 'yank)
;; 2008/04/25 dadams
;;     Added: icicle-toggle-remote-file-testing.
;;     icicle-Completions-mouse-3-menu:
;;       Added icicle-toggle-remote-file-testing (C-^).
;;       icicle-toggle-ignored-space-prefix is now bound to M-_, not C-^.
;;     icicle-dispatch-C-^:
;;       icicle-toggle-remote-file-testing, not icicle-toggle-ignored-space-prefix.
;; 2008/04/18 dadams
;;     Renamed icicle-init-value-flag to icicle-default-value.
;; 2008/04/13 dadams
;;     icicle-pp-eval-expression-in-minibuffer: Treat prefix arg (added optional arg).
;;     icicle-apply-to-saved-candidate: Use icicle-pp-eval-expression, not pp-eval-expression.
;; 2008/04/02 dadams
;;     icicle-apropos-complete-and-narrow: No longer bind icicle-top-level-when-*-flag to t.
;; 2008/03/31 dadams
;;     icicle-apropos-complete-1: Allow for no completion type, for No completion msg.
;; 2008/03/29 dadams
;;     icicle-pp-eval-expression-in-minibuffer, icicle-narrow-candidates(-with-predicate):
;;       Removed binding of icicle-reminder-prompt-flag.
;; 2008/03/25 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       When not regexp-matching, use substitute-in-file-name on input, to convert \ to /.
;; 2008/03/23 dadams
;;     icicle-scroll-Completions: Respect and update icicle-scroll-Completions-backward-p.
;; 2008/03/19 dadams
;;     Added: icicle-insert-newline-in-minibuffer.
;;     Renamed i*-pp-eval-expression to *-in-minibuffer.  Calls new icicle-pp-eval-expression.
;; 2008/03/12 dadams
;;     icicle-completion-help: Add buttons for Commentary (icicles-doc*.el).
;; 2008/03/11 dadams
;;     icicle-add/update-saved-completion-set: Clarify error msg for unwritable file.
;; 2008/03/07 dadams
;;     Renamed icicle-abort-minibuffer-input to icicle-abort-recursive-edit.
;; 2008/03/02 dadams
;;     icicle-describe-file:
;;       Use default dir if arg is nil.  Error if no readable file. Removed save-excursion.
;; 2008/02/24 dadams
;;     icicle-apropos-complete-1: Use icicle-apropos-match-fns-alist lookup for message.
;;     icicle-Completions-mouse-3-menu: Added item for icicle-next-apropos-match-function.
;;     Added: icicle-next-apropos-match-function.
;; 2008/02/22 dadams
;;     icicle-retrieve-candidates-from-set:
;;       Like logic for alist in icicle-completing-read:
;;         Do icicle-readable-to-markers, then copy car and replace cdr with whole candidate.
;;       Removed RAW arg from call to find-file-noselect.
;;       Wrap Lisp read in condition-case.
;;     Renamed, added un: icicle(-mouse)-save-candidate to icicle(-mouse)-save/unsave-candidate.
;;     icicle-add/update-saved-completion-set: Changed default name to just add .icy.
;;     icicle-candidate-set-save-1, icicle-retrieve-candidates-from-set:
;;       Wrap write/read to/from cache file in condition-case.
;; 2008/02/16 dadams
;;     icicle-retrieve-candidates-from-set:
;;       Convert alist cands to propertized strings, using i*-put-alist-* and i*-readable-to-*.
;;       Set icicle-candidates-alist to reconstituted retrieved candidates.
;;     icicle-candidate-set-save-1:
;;       Convert to readable alist from propertized text, using i*-markers-to-*, i*-get-alist-*.
;;     Added: icicle-readable-to-markers, icicle-markers-to-readable.
;; 2008/02/15 dadams
;;     icicle-delete-windows-on:
;;       Enable recursive minibuffers for interactive use.  Thx to Simon Marshall.
;; 2008/02/14 dadams
;;     icicle-add/update-saved-completion-set: Remove properties from completion-set string.
;;     icicle-change-sort-order, icicle-retrieve-previous-input,
;;       icicle-insert-string-from-variable, icicle(-mouse)-candidate-read-fn-invoke,
;;       icicle-narrow-candidates, icicle-save-predicate-to-variable,
;;       icicle-candidate-set-retrieve, icicle-candidate-set-save-1,
;;       icicle-add/update-saved-completion-set, icicle-isearch-complete: 
;;         Bind icicle-whole-candidate-as-text-prop-p to nil.
;; 2008/02/07 dadams
;;     icicle-delete-windows-on:
;;       Delete frame if one-window-p and not a standalone minibuffer.  Thx to Simon Marshall.
;; 2008/02/03 dadams
;;     icicle-mouse-choose-completion:
;;       Use absolute file name for choice and prepend dir in minibuffer.  Set base-size to 0.
;;       Don't remove *Completions* window.
;;     icicle-mouse-candidate-action-1: Use absolute file name for choice.
;;     icicle-mouse-choose-completion, icicle-mouse-candidate-action-1,
;;       icicle-current-completion-in-Completions: buffer-substring -> *-no-properties.
;;     icicle-nb-of-candidate-in-Completions: Update last-nb before the test, in loop.
;;     icicle-prefix-complete-1: If input matches empty dir, use that dir as sole completion.
;;                               Don't remove *Completions* window until after minibuffer msg.
;;     icicle-move-to-next-completion: Use icicle-show-Completions-help-flag, not hard-coded 3.
;;     icicle-candidate-action-1: For require-match case also, remove cand and display others.
;;     Added: icicle-choose-completion.
;; 2008/01/30 dadams
;;     Added: icicle-yank-secondary, icicle-mouse-yank-secondary.
;; 2008/01/29 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       If icicle-incremental-completion-flag is explicit*, treat like incremental completion.
;;     icicle-apropos-complete-1:
;;       Distinguish non-existent dir from empty dir: icicle-apropos-any-file-name-candidates-p.
;;     icicle-highlight-input-noncompletion takes no args now.
;; 2008/01/13 dadams
;;     icicle-mouse-choose-completion, icicle-insert-completion,
;;       icicle-mouse-candidate-action-1, icicle-mouse-save-candidate:
;;         Do not use icicle-transform-multi-completion.
;;     icicle-retrieve-last-input, icicle-(apropos|prefix)-complete-1:
;;       Do not treat handle-switch-frame.
;;     icicle-mouse-candidate-action-1: Add back \n only if it has property icicle-keep-newline.
;;     icicle-mouse-remove-candidate: Removed cruft.
;;     icicle-remove-candidate-display-others: save-selected-window around Completions display.
;;     icicle-help-on-candidate: Rewrote.
;;       Do not use icicle-transform-multi-completion except where appropriate.
;;       Always use icicle-candidate-help-fn as first priority, if defined.
;;       Give help for prefix keys too during key completion.
;;     icicle-help-on-candidate-symbol: No call to icicle-candidate-help-fn here.
;; 2008/01/04 dadams
;;     icicle-mouse-choose-completion, icicle-current-completion-in-Completions:
;;       Add candidate's final \n only if it has property icicle-keep-newline.
;; 2007/01/01 dadams
;;     icicle-narrow-candidates: For Emacs < 22, don't tack dir onto file name if absolute.
;;     icicle-candidate-set-save-1: Only redisplay candidates if *Completions* was displayed.
;; 2007/12/31 dadams
;;     icicle-mouse-choose-completion, icicle-mouse-candidate-action-1:
;;       Add back candidate's final \n that is missing mouse-face.
;;       Return icicle-candidate-nb, as doc string says.
;;     icicle-mouse-candidate-action-1: Delete current input from minibuffer before acting.
;;     Added: icicle-insert-list-join-string.
;;     Don't mention that C-o is bound to icicle-candidate-action.
;; 2007/12/26 dadams
;;     icicle-transform-multi-completion: Empty input after join string means empty part.
;;     icicle-help-on-candidate: Don't call icicle-raise-Completions-frame.
;; 2007/12/11 dadams
;;     icicle-change-sort-order:
;;       Don't include icicle-proxy-candidate-first-p unless icicle-add-proxy-candidates-flag.
;; 2007/12/10 dadams
;;     icicle-exit-minibuffer, icicle-kill-failed-input:
;;       Face icicle-input-completion-fail-lax also.
;; 2007/12/09 dadams
;;     icicle-exit-minibuffer: Remove icicle-input-completion-fail face from input.
;;     icicle-kill-failed-input: Rehighlight after deleting highlighted part.
;; 2007/12/08 dadams
;;     icicle-(next|previous)-line: Rewrote for variable number of columns.
;;     Added: , icicle-(beginning|end)-of-line+.
;; 2007/12/03 dadams
;;     Renamed longest common match (lcm) to expanded common match (ecm).
;; 2007/11/30 dadams
;;     icicle-help-on-candidate-symbol:
;;       Use fboundp, not functionp, to get describe-function for macros too.
;; 2007/11/28 dadams
;;     Renamed describe-bindings-in-map to describe-keymap.
;;     icicle-toggle-proxy-candidates: Swap values for saved and unsaved.
;; 2007/11/25 dadams
;;     Added: icicle-sort-by-abbrev-frequency.
;;     icicle-help-on-candidate-symbol: Treat command abbrevs via apropos for their commands.
;; 2007/11/24 dadams
;;     Added: icicle-sort-proxy-candidates-first.
;; 2007/11/22 dadams
;;     icicle-help-on-candidate-symbol: Use describe-bindings-in-map for a keymap.
;; 2007/11/17 dadams
;;     Added: icicle-toggle-proxy-candidates.  Added to icicle-Completions-mouse-3-menu also.
;; 2007/11/04 dadams
;;     Require subr-21 if replace-regexp-in-string is not defined.
;;     Require icicles-mac (don't wrap in eval-when-compile).
;;     icicle-Completions-mouse-3-menu: Added the latest toggle commands.
;; 2007/10/28 dadams
;;     Added: icicle-toggle-expand-to-common-match, icicle-toggle-search-replace-common-match.
;;     icicle-retrieve-last-input:
;;       Treat prefix completion like non-nil expand-input-to-common-match-flag.
;;     icicle-search-define-replacement: Bind icicle-update-input-hook to nil.
;;     icicle-toggle-highlight-all-current:
;;       Save icicle-candidate-nb around rehighlighting.
;;       Call icicle-search-action to get back to current candidate and highlight it.
;; 2007/10/27 dadams
;;     icicle-search-define-replacement:
;;       Bind candidates, input, and cand #, to restore after read replacement string.
;; 2007/10/26 dadams
;;     icicle-toggle-highlight-all-current: select-frame-set-input-focus to minibuffer.
;; 2007/10/22 dadams
;;     icicle-doremi-*: Use 4 arrows - one command hands off to the other.
;; 2007/10/21 dadams
;;     Added: icicle-doremi-inter-candidates-min-spaces, icicle-doremi-candidate-width-factor.
;; 2007/10/14 dadams
;;     Updated doc strings to reflect icicle-act-before-cycle-flag.
;; 2007/10/13 dadams
;;     icicle-candidate-action-1:
;;       Don't set icicle-last-completion-candidate if string.  Used for repeated C-next.
;;     icicle-remove-candidate-display-others, icicle-history:
;;       Treat also the case where cand is a string, not a consp.
;; 2007/10/07 dadams
;;     icicle-delete-candidate-object: Respect icicle-deletion-action-flag.
;; 2007/10/02 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Apply abbreviate-file-name to file-name input.  Thx to Joonhwan Lee.
;;     icicle-toggle-fuzzy-completion: Removed soft require of fuzzy-match+.el.
;; 2007/09/29 dadams
;;     Added: icicle-toggle-fuzzy-completion.
;;     icicle-Completions-mouse-3-menu: Added icicle-toggle-fuzzy-completion.
;;     icicle-prefix-complete-1: Adjust feedback messages for fuzzy completion.
;;     icicle-(apropos|prefix)-complete-1:
;;       Only set icicle-default-directory if (icicle-file-name-input-p).
;; 2007/09/25 dadams
;;     icicle-narrow-candidates: Treat icicle-whole-candidate-as-text-prop-p case.
;;     icicle-kill-failed-input: Rewrote.
;; 2007/09/21 dadams
;;     icicle-narrow-candidates:
;;       Emacs<22, file-name completion: Append directory to each candidate.  Thx Ian Perryman.
;; 2007/09/14 dadams
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete:
;;       Wrapped condition-case around candidates computation.
;; 2007/08/25 dadams
;;     icicle-mouse-candidate-action-1: Use buffer-substring, not buffer-*-no-properties.
;; 2007/08/21 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Reset icicle-input-fail-pos.  Call icicle-highlight-input-noncompletion when no match.
;; 2007/08/19 dadams
;;     Added: icicle-kill-failed-input.
;; 2007/08/18 dadams
;;     icicle-previous-apropos-candidate-alt-action: Fixed typo.  Thx to Hadron Quark.
;; 2007/07/29 dadams
;;     icicle-apply-to-saved-candidate:
;;       Added use-icicle-candidates-alist-p arg.  Use icicle-get-alist-candidate.
;;       Report original error message also.
;;     icicle-candidate-action-1: Do nothing if icicle-last-completion-candidate not a string.
;; 2007/07/27 dadams
;;     icicle-successive-action:
;;       icicle-act-first-then-navigate-p -> icicle-act-before-cycle-flag.
;; 2007/07/08 dadams
;;     icicle-all-candidates(-alt)-action:
;;       Use icicle-all-candidates(-alternative)-action-fn if defined.
;;     icicle-all-candidates-action-1: Added listp arg.
;;     icicle-mouse-save-candidate:
;;       Deactivate mark and redisplay completions, to show save highlight.
;; 2007/07/07 dadams
;;     Added: icicle-candidate-set-save(-more)-selected(-1),
;;            icicle-mouse-candidate-set-save(-more), icicle-mouse-save-then-kill.
;;     icicle-insert-completion: If no current completion, return to minibuffer anyway.
;;                               Update icicle-current-input with inserted candidate.
;;     icicle-Completions-mouse-3-menu:
;;       Added icicle-candidate-set-save-(more(-selected)|-selected).
;;     icicle-save-candidate: If no defined icicle-candidate-nb, then just display message.
;;     icicle-candidate-set-save(-more):
;;       Use icicle-candidate-set-save-1: Intern variable in standard obarray also.  Redisplay
;;       candidates and reselect minibuffer after reading file/var name.  Put eof error in
;;       minibuf.  Deactivate mark and redisplay completions.  Separate msg if reset.
;;     icicle-candidate-set-retrieve: If nothing to restore, don't restore nothing.
;;                                    If single candidate to restore, no *Completions* display.
;;                                    Else, update candidate display.
;; 2007/07/04 dadams
;;     icicle-Completions-mouse-3-menu: Added icicle-retrieve-(next|\previous)-input.
;; 2007/07/03 dadams
;;     Added: icicle-insert-history-element, icicle-retrieve-(next|previous)-input.
;;     icicle-history, icicle-keep-only-past-inputs:
;;       Don't retrieve last input unless following a cycling command.
;;     icicle-history:
;;       Do an initial icicle-apropos-complete unless icicle-last-completion-command.
;;       If not following a cycling command, call icicle-last-completion-command (don't set it
;;         to empty string) and reset icicle-last-input to nil.
;;     icicle-Completions-mouse-3-menu:
;;       icicle-retrieve-(next|previous)-input, not icicle-retrieve-last-input.
;;     Redefined next-history-element, instead of using defadvice.
;; 2007/06/23 dadams
;;     icicle-search-define-replacement: Use icicle-completing-read-history, not read-string.
;;                                       Use icicle-search-replacement-history.
;; 2007/06/17 dadams
;;     Added: icicle-toggle-WYSIWYG-Completions.
;;     icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Added priority in call to icicle-place-overlay.
;; 2007/06/13 dadams
;;     Added: icicle-candidate-set-save-more.
;;     icicle-candidate-set-save: Unify messages.
;; 2007/06/12 dadams
;;     Added: icicle(-mouse)-save-candidate.
;;     icicle-candidate-set-retrieve: Insert candidate if there is only one retrieved.
;;     icicle-insert-completion: Added optional completion arg for non-interactive insertion.
;; 2007/06/10 dadams
;;     icicle-candidate-action-1: Treat icicle-require-match-p.
;; 2007/06/09 dadams
;;     icicle-candidate-action-1, icicle-mouse-candidate-action-1:
;;       Remove candidate if icicle-use-candidates-only-once-flag.
;;     icicle-candidate-action-1:
;;       Let users act on non-candidate too (arbitrary input).
;; 2007/06/07 dadams
;;     Renamed: icicle-function-history to icicle-function-name-history,
;;              icicle-variable-history to  icicle-variable-name-history.
;;     Use standard history variable if bound, else use Icicles history variable:
;;       function-name-history, variable-name-history
;; 2007/06/01 dadams
;;     icicle-erase-minibuffer-or-history-element, icicle-history:
;;       Ensure value of minibuffer-history-variable is bound.
;;     icicle-keep-only-past-inputs: If value of minibuffer-history-variable unbound, set nil.
;;     icicle-keep-only-past-inputs, icicle-history:
;;       Assume value of minibuffer-history-variable is a symbol - don't test that.
;; 2007/05/29 dadams
;;     icicle-insert-thing: Added optional arg no-replace-p.  Make sure end points are defined.
;;     icicle-insert-string-from-variable: Call icicle-insert-thing with no-replace-p arg.
;;     icicle-minibuffer-complete-and-exit: Set window-point to end of minibuffer.
;; 2007/05/15 dadams
;;     icicle-completion-help and top level:
;;       Soft require help-mode, not (featurep 'help-mode) and (fboundp 'define-button-type).
;; 2007/05/08 dadams
;;     Added: icicle-save-predicate-to-variable.
;;     icicle-Completions-mouse-3-menu: Added icicle-save-predicate-to-variable to menu.
;;     icicle-narrow-candidates-with-predicate: Quoted the predicate that is read.
;; 2007/05/07 dadams
;;     Added: icicle-narrow-candidates-with-predicate.
;;     icicle-Completions-mouse-3-menu: Added icicle-narrow-candidates-with-predicate (M-&).
;; 2007/05/06 dadams
;;     icicle-completion-help: Updated text at top of help buffer.
;;     icicle-customize-button: Capitalized group Icicles.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/04 dadams
;;     icicle-candidate-read-fn-invoke, icicle-keep-only-*-inputs, icicle-retrieve-last-input,
;;     icicle-candidate-set-(retrieve|save|swap|difference|union|intersection|complement),
;;     icicle-all-candidates(-alt)-action, icicle-pp-eval-expression,
;;     icicle-insert-string-from-variable:
;;       Can call from *Completions* too, so can choose from mouse-3 menu during multi-command.
;;     icicle-candidate-set-save, icicle-retrieve-last-input, icicle-insert-*-from-variable:
;;       Select minibuffer window.
;;     icicle-toggle-case-sensitivity: Use setq-default for case-fold-search.
;;     icicle-switch-to-Completions-buf:
;;       Use read-file-name-completion-ignore-case, if completing file name.
;;     Added empty defvars for Emacs 22 standard vars, to quiet byte compiler.
;; 2007/05/02 dadams
;;     Added: icicle-dispatch-M-q, icicle-toggle-search-whole-word.
;;     Removed: icicle-dispatch-C-backquote.
;; 2007/04/29 dadams
;;     Added: icicle-sort-by-last-file-modification-time (sort order).
;; 2007/04/19 dadams
;;     icicle-successive-action: No longer interactive.  Moved barfing to calling commands.
;; 2007/04/17 dadams
;;     Added: icicle-dispatch-M-comma, icicle-search-define-replacement,
;;            icicle-dispatch-C-backquote, icicle-toggle-literal-replacement.
;; 2007/04/08 dadams
;;     Added: icicle-all-candidates-alt-action, icicle-all-candidates-action-1.
;;     icicle-candidate-action-1, icicle-delete-candidate-object, icicle-help-on-candidate,
;;     icicle-candidate-read-fn-invoke:
;;       Use negative test for prefix mode, not positive test for apropos.
;; 2007/04/07 dadams
;;     Added: icicle-successive-action, icicle-toggle-search-replace-whole,
;;            icicle-dispatch-C-comma.
;;     Defined navigating action and help functions using icicle-successive-action.
;; 2007/03/31 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       Accept sole completion if icicle-top-level-when-sole-completion-flag.
;;     icicle-narrow-candidates:
;;       Use read-file-name only for Emacs 22 or later.
;;       Accept sole completion only if icicle-top-level-when-sole-completion-flag.
;;     icicle-apropos-complete-and-narrow: Bind icicle-top-level-when-*-flag to t.
;; 2007/03/30 dadams
;;     icicle-narrow-candidates: Suppress sole-completion minibuffer-message.
;; 2007/03/23 dadams
;;     Added: icicle-apropos-complete-and-narrow.  Thx to Marian Schubert for the suggestion.
;;     icicle-narrow-candidates: Use icicle-require-match-p as REQUIRE-MATCH arg.
;; 2007/03/09 dadams
;;     Changed require to eval-when-compile require for icicles-mac.el.
;; 2007/03/08 dadams
;;     icicle-delete-current-candidate-object: Rewrote.
;;       Value of var can be an arbitrary alist, a list of strings, or a list of symbols.
;;     icicle-remove-candidate-display-others: Rewrote.
;;       Set icicle-last-completion-candidate based on icicle-candidate-nb or 0.
;;       Delete icicle-last-completion-candidate completely from icicle-completion-candidates.
;;       Update minibuffer-completion-predicate or read-file-name-predicate to remove for
;;         completion.
;;       Use with-current-buffer, not save-window-excursion, to visit *Completions*.
;;     icicle-remove-candidate:
;;       Updated doc string to mention Emacs < 22 limitation for file-name candidates.
;;     icicle-retrieve-last-input: Don't reset icicle-last-completion-command if interactive.
;; 2007/03/07 dadams
;;     icicle-switch-to-Completions-buf, icicle-remove-candidate-display-others,
;;     icicle-help-on-candidate, icicle-delete-windows-on:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/06 dadams
;;     icicle-remove-candidate: Don't reset to first cand matching input if no last cand.
;;     icicle-change(alternative)-sort-order, icicle-reverse-sort-order,
;;       icicle-keep-only-past-inputs, icicle-toggle-sorting: Respect icicle-inhibit-sort-p.
;;     Renamed icicle-get-current-candidate to icicle-get-alist-candidate.
;; 2007/03/04 dadams
;;     icicle-remove-candidate-display-others:
;;       Use local var for cand-nb, because icicle-candidate-nb can change.
;;       If no last candidate, reset to first candidate matching input.
;;       Allow for icicle-candidate-nb not being defined here:
;;         Use icicle-get-current-candidate.  Move to next completion only if cand-nb defined.
;;       Use mapconcat only when delete multi-completion.
;;       Move to next completion in *Completions* only if icicle-candidate-nb was defined.
;;       Insert default-directory too, if icicle-file-name-input-p.
;;     icicle-insert-completion: Insert default-directory too, if icicle-file-name-input-p.
;;     icicle-(apropos|prefix)-complete-1, icicle-keep-only-past-inputs:
;;       Don't include directory when set icicle-last-completion-candidate.
;;     icicle-(apropos|prefix)-complete-1:
;;       Don't include directory when testing input membership in icicle-completion-candidates.
;; 2007/03/02 dadams
;;     icicle-delete-candidate-object:
;;       Corrected message target (object).  Added sit-for.
;;       Use local var for cand-nb, because icicle-candidate-nb can change.
;; 2007/02/27 dadams
;;     icicle-delete-candidate-object: Added message.
;;     icicle-delete-current-candidate-object: Don't erase minibuffer or update completions.
;; 2007/02/24 dadams
;;     Added: icicle(-mouse)-candidate-alt-action, icicle(-mouse)-candidate-action-1,
;;            icicle-(previous|next)-(apropos|prefix)-candidate-alt-action,
;;            icicle(-mouse)-remove-candidate, icicle-remove-candidate-display-others,
;;            icicle-delete-candidate-object, icicle-delete-current-candidate-object.
;;     icicle-insert-completion:
;;       Invoke icicle-transform-multi-completion.  Use with-current-buffer (window-buffer).
;;     icicle(-mouse)-candidate-action: Use icicle(-mouse)-candidate-action-1.
;; 2007/02/06 dadams
;;     icicle-completion-help: Added extra help if completing and if multi-command.
;; 2007/02/03 dadams
;;     Renamed icicle-icompleting-p to icicle-edit-update-p.
;; 2007/02/02 dadams
;;     Updated doc strings of toggle commands to mention the minibuffer bindings.
;; 2007/01/29 dadams
;;     icicle-change-sort-order: Don't sort icicle-sort-functions-alist entries for use.
;;     Define alphabetical sort order using icicle-case-string-less-p, not string-lessp.
;; 2007/01/23 dadams
;;     Added: icicle-toggle-highlight-historical-candidates.
;;     icicle-Completions-mouse-3-menu: Updated wrt toggles.
;; 2007/01/21 dadams
;;     icicle-narrow-candidates:
;;       Use minibuffer-history-variable, not regexp-history.  Thx to Jost for bug report.
;; 2007/01/20 dadams
;;     icicle-mouse-(choose-completion|candidate-action):
;;       Use icicle-transform-multi-completion.
;; 2007/01/15 dadams
;;     Added: icicle-change(-alternative)-sort-order, icicle-reverse-sort-order,
;;            icicle-current-sort-order, icicle-sort-*.
;;     icicle-transform-sole-candidate: Set icicle-last-*-candidate to transformed cand.
;;     icicle-help-on-candidate: Use icicle-transform-multi-completion.
;;     icicle-Completions-mouse-3-menu: Updated with new sort-order bindings.
;;     icicle-toggle-alternative-sorting: Better message.
;;     Require icicles-mac.el.
;; 2007/01/14 dadams
;;     Added: icicle-transform-multi-completion, icicle-transform-sole-candidate.
;;     icicle-(apropos|prefix)-complete-1: Use icicle-transform-sole-candidate.  Thx Rubikitch.
;;     icicle-help-on-candidate(-symbol):
;;       Use with-current-buffer to describe mode in Emacs 20 also.
;; 2007/01/13 dadams
;;     Added: icicle-describe-file, icicle-help-on-candidate-symbol.
;;     icicle-help-on-candidate:
;;       If existing symbol, describe it.  Else if buffer or file, describe it.  Else, convert
;;         string to symbol and describe it.  Use icicle-help-on-candidate-symbol.
;; 2007/01/10 dadams
;;     icicle-switch-to/from-minibuffer: Error message if minibuffer is not active.
;; 2007/01/06 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       expand-file-name -> icicle-abbreviate-or-expand-file-name.
;;     Added: icicle-toggle-~-for-home-dir.
;;     icicle-prefix-complete-1:
;;       Set icicle-default-directory only if also icicle-file-name-input-p.
;; 2007/01/01 dadams
;;     icicle-add/update-saved-completion-set: Use icicle-assoc-delete-all, not delete of assoc.
;;     Runtime, not compile-time, require of icicles-var.el, icicles-opt.el.
;; 2006/12/29 dadams
;;     icicle-insert-string-at-point:
;;       Treat nil return of alternative text-grabbing function.
;;       Echo the text-grabbing function when icicle-default-thing-insertion = alternatives.
;;     icicle-ensure-overriding-map-is-bound: Separate treatment for diff Emacs versions.
;; 2006/12/25 dadams
;;     icicle-keep-only-past-inputs:
;;       Added optional recent-first arg: Use icicle-most-recent-first-p as sort function.
;;       Update cands list if repeat.  Do not scroll Completions; update it unconditionally.
;;     Added: icicle-candidate-set-truncate.
;;     Uncommented describe-mode code, since RMS fixed Emacs bug that caused infinite recursion.
;; 2006/12/24 dadams
;;     Added: icicle-Completions-mouse-3-menu.
;; 2006/12/23 dadams
;;     icicle-narrow-candidates: Bug fix: Treat file-name completion with read-file-name.
;;     icicle-help-on-candidate: Call non-nil icicle-candidate-help-fn on candidate.
;; 2006/12/18 dadams
;;     icicle-apply-to-saved-candidate: Remove print arg and use current-prefix-arg instead.
;;     icicle-ensure-overriding-map-is-bound: Protect overriding-map-is-bound with boundp.
;;     Bug fix for Emacs 21: protect help-xref with get type button-category-symbol.
;; 2006/12/17 dadams
;;     Added: icicle(-mouse)-candidate-read-fn-invoke, icicle-apply-to-saved-candidate.
;; 2006/12/10 dadams
;;     Created from minibuffer and *Completions* commands in icicles-cmd.el.
 
;;;(@* "CHANGE LOG FOR `icicles-mode.el'")
;;
;; 2010/06/11 dadams
;;     icicle-define-minibuffer-maps: Bind/restore C-c + to icicle-make-directory in file-name maps.
;; 2010/06/04 dadams
;;     icicle-mode doc string: Mention missing doremi commands.
;;     icicle-define-icicle-maps: Added Swank items and Max # of Completions, :visible for separator.
;;     icicle-(bind|restore)-completion-keys: Bind icicle-doremi-increment-max-candidates+ to C-x #.
;;     Apply renamings of icicle-doremi* (added +).
;; 2010/05/26 dadams
;;     Add to command-history only if an interned symbol.  Thx to Michael Heerdegen.
;; 2010/05/22 dadams
;;     icicle-(bind|restore)-completion-keys: Bind icicle-candidate-read-fn-invoke to ESC C-m also.
;; 2010/05/17 dadams
;;     icicle-define-icicle-maps: Changed :enable conditions for *-goto*-marker.  Thx to M. Heerdegen.
;; 2010/05/15 dadams
;;     icicle-mode: Updated doc string for bookmark commands.
;; 2010/05/09 dadams
;;     Key-binding changes: icicle-change-sort-order is C-, icicle-dispatch-M-_ is M-_.
;; 2010/04/21 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Added icicle-sit-for for Emacs 23.
;; 2010/04/02 dadams
;;     icicle-mode: Updated doc string: list of commands.
;; 2010/04/02 dadams
;;     icicle-mode: Update doc string for change from regions to bookmarks.
;;     icicle-define-icicle-maps:
;;       Remove Icicles region stuff from menus.
;;       Added to menus: icicle-search-bookmarks-together, icicle-search-bookmark,
;;                       icicle-select-bookmarked-region.
;; 2010/03/28 dadams
;;     Applied renaming: icicle-search-all-regions to icicle-search-region.
;;     Use icicle-search-region-bookmark in menus.
;; 2010/03/14 dadams
;;     icicle-define-minibuffer-maps: Use featurep, not soft-require, for bookmark+.el.
;;     Added bookmark+ to final dolist for eval-after-load.
;; 2010/03/13 dadams
;;     icicle-define-icicle-maps:
;;       Bound icicle-toggle-show-multi-completion to M-m in minibuffer, and added to menus.
;;     Applied renaming of icicle-add-buffer-name-flag to icicle-show-multi-completion-flag.
;; 2010/02/17 dadams
;;     Applied rename of icicl-redefined-functions to icicle-inhibit-advice-functions.
;; 2010/02/13 dadams
;;     icicle-mode:
;;       Fill icicle-advice-info-list from advised fns among icicle-redefined-functions.
;;       Reactivate advised fns (in icicle-advice-info-list) when turn mode off.
;;       Added to doc string: icicle-bookmark-(dired|desktop|bookmark-list|man)-other-window.
;;     icicle-define-icicle-maps: Use (featurep 'recentf) instead of soft-requiring it.
;;     icicle(-bookmark)-menu-map: Added type-specific bookmark jump commands.
;;     eval-after-load's: Add (when (featurep 'icicles-mode)...) to ensure this file was loaded.
;; 2010/01/28 dadams
;;     icicle-define-minibuffer-maps, icicle-restore-completion-keys:
;;       Restore C-g correctly if delete-selection-mode.
;; 2009/12/25 dadams
;;     icicle-mode: Call completion-ignored-build-disable to disable the advice.
;; 2009/12/21 dadams
;;     Final dolist: Move loaded-library test outside of eval-after-load.
;;                   Update the test for Emacs 22+ (not just assoc).  Thx to Kevin Ryde.
;;     Combine eval-after-load's for dired-x.  Remove eval-after-load for simple.el (preloaded).
;;     fset -> defalias.
;; 2009/12/13 dadams
;;     icicle-define-minibuffer-maps:
;;       Bind C-x m to icicle-bookmark-file-other-window in file-name completion maps.
;; 2009/11/29 dadams
;;     Don't reference minibuffer-local-must-match-filename-map unless bound (obsolete in 23.2).
;; 2009/11/27 dadams
;;     icicle-(bind|restore)-completion-keys: Bind/restore C-x 1, C-x 2.
;; 2009/11/07 dadams
;;     Applied doremi cmd renamings (added +).
;; 2009/10/25 dadams
;;     icicle-mode, icicle-define-icicle-maps, icicle-bind-completion-keys:
;;       Updated doc string, menus, keys for completion-method command renamings.
;; 2009/09/26 dadams
;;     icicle-minibuffer-setup: Don't complete if icicle-progressive-completing-p.
;; 2009/09/16 dadams
;;     icy-mode: Add icicle-insert-buffer to doc string.
;;     icicle-define-icicle-maps: Added icicle-insert-buffer to icicle-menu-map.
;; 2009/09/10 dadams
;;     icicle-bind-key-completion-keys-in-keymaps-from: Don't exclude menu maps.
;; 2009/09/03 dadams
;;     icicle-add-menu-item-to-cmd-history: Wrap in condition-case, since on pre-command-hook.
;; 2009/08/18 dadams
;;     icicle-add-menu-item-to-cmd-history:
;;       Ensure this-command-keys-vector is not empty.  Thx to Kai Tetzlaff and Lennart Borgman.
;; 2009/08/09 dadams
;;     icicle-minibuffer-setup: Set region background for recursive minibuffers too.
;;     icicle-restore-region-face: Don't restore unless going back to top level.
;; 2009/08/01 dadams
;;     Added: icicle-add-menu-item-to-cmd-history.  Thx to Lennart Borgman.
;;     icy-mode: add/remove pre-command-hook, respecting icicle-menu-items-to-history-flag.
;;     call-interactively defadvice: Do not save let savehist save icicle-interactive-history.
;; 2009/07/29 dadams
;;     Change advice for call-interactively:
;;       Use it only for Emacs 23+.  Disable it to begin with.  
;;       Add only (non-mouse command) symbols to history.
;;     icy-mode (Emacs 23+):
;;       Enable/disable advice icicle-save-to-history when mode is turned on/off.
;;       Enable the advics only if non-nil icicle-populate-interactive-history-flag.
;;     icy-mode (Emacs 20-21): Remove advice icicle-save-to-history.
;; 2009/07/26 dadams
;;     Advise call-interactively to save command to icicle-interactive-history.
;;     icicle-(bind|restore)-completion-keys: Bind/restore C-M-pause as icicle-other-history.
;; 2009/07/13 dadams
;;     Emacs 22+:
;;       Added describe-face defadvice (icicle-respect-WYSIWYG).  icicle-mode: (de)activate it.
;; 2009/06/18 dadams
;;     icicle-mode: Added icicle-doremi-zoom-Completions to doc string.
;; 2009/06/17 dadams
;;     icicle-(bind|restore)-completion-keys: Bind icicle-doremi-zoom-Completions to C-x -.
;;     icicle-define-icicle-maps:
;;       Add icicle-doremi-zoom-Completions to Options menu.
;;       Change visible condition for all doremi stuff to ensure *Completions* is showing.
;; 2009/05/27 dadams
;;     icicle-retrieve-(next|previous)-input: Removed unused (always nil) arg DONT-COMPLETE-P.
;; 2009/05/22 dadams
;;     icicle-define-icicle-maps: Added icicle-Info-virtual-book to menu.
;;     Require icicles-cmd[12].el.
;; 2009/05/17 dadams
;;     dolist eval-after-load at end: Use icicle-toggle-icicle-mode-twice, not icy-mode calls.
;; 2009/05/09 dadams
;;     icicle-define-icicle-maps: Added icicle-toggle-dot to options menu.  Updated C-x . key.
;;     icicle-(bind|restore)-completion-keys:
;;       Bind *-toggle-dot to C-M-., *-toggle-hiding-common-match to C-x .,
;;            *-insert-dot-command to ..
;;     icy-mode: Mention icicle-toggle-dot in doc string.
;; 2009/05/02 dadams
;;     icicle-minibuffer-setup: Set icicle-cmd-reading-input to this-command.
;; 2009/04/30 dadams
;;     icicle-minibuffer-setup: Reset icicle-next-(prefix|apropos)-complete-cycles-p to nil.
;; 2009/04/20 dadams
;;     icicle-bind-completion-keys: Don't bind C-S-(up|down|next|prior) explicitly.
;;     icicle-restore-completion-keys:
;;       Restore: icicle-(prefix|apropos)-cycle-(previous|next)-alt-action-keys,
;;                icicle-modal-cycle-(up|down)-alt-action-keys.
;;       Don't restore C-S-(up|down|next|prior) explicitly.
;;     icicle-define-cycling-keys:
;;       Bind/Restore: icicle-(prefix|apropos)-cycle-(previous|next)-alt-action-keys,
;;                     icicle-modal-cycle-(up|down)-alt-action-keys.
;; 2009/04/19 dadams
;;     icicle-redefine-standard-commands: Added defalias for customize-apropos-options-of-type.
;;     eval-after-loads: Use when/unless instead of and/or for fset's. (cosmetic)
;; 2009/04/18 dadams
;;     icicle-mode: Mention in doc string: you might want to customize keys if no window mgr.
;; 2009/04/16 dadams
;;     icicle-restore-completion-keys, icicle-define-cycling-keys:
;;       Use icicle-(prefix|apropos)-cycle-(previous|next)-help-keys.
;; 2009/04/15 dadams
;;     icicle-bind-completion-keys: Removed bindings for C-M-(up|down|prior|next).
;;     icicle-restore-completion-keys: Unbind icicle-modal-cycle-(up|down)-help-keys.
;;     icicle-define-cycling-keys: Bind/unbind all help keys (including modal ones).
;; 2009/03/27 dadams
;;     icicle-(redefine|restore)-standard-commands:
;;       Added icicle-minibuffer-default-add-completions.
;; 2009/03/16 dadams
;;     icicle-define-icicle-maps: Use :visible for Icicle submenus themselves.
;;     icicle-(redefine|restore)-standard-commands: Added icicle-recentf-make-menu-items.
;;     Added eval-after load for recentf.el.
;; 2009/03/15 dadams
;;     icicle-mode: Added to doc string: icicle-recompute-shell-command-candidates,
;;                                       icicle-remove-file-from-recentf-list.
;;     icicle-define-icicle-maps: Added icicle-remove-file-from-recentf-list to menus.
;; 2009/03/10 dadams
;;     icicle-mode: Don't reset icicle-shell-*-cache if icicle-guess-commands-in-path is load.
;;     Applied renaming: icicle-shell-command-candidates to *-cache
;; 2009/03/01 dadams
;;     icicle-define-minibuffer-maps:
;;       Bind icicle-completing-read+insert, icicle-read+insert-file-name.  Add to Minibuf menu.
;; 2009/02/28 dadams
;;     fset old-dired-smart-shell-command after load Dired-X.
;; 2009/02/20 dadams
;;     icicle-mode: Reset icicle-shell-command-candidates to nil.
;;     icicle-minibuffer-setup:
;;       Use function icicle-require-match-p, not var.
;;       Do not reset icicle-completing-p to nil (reset by icicle-require-match-p).
;;     icicle-(redefine|restore)-standard-commands:
;;       Redefine dired-read-shell-command, not dired-guess-shell-command.
;;       Redefine: dired-smart-shell-command, shell-command(-on-region).
;;     Added eval-after load for dired-read-shell-command for dired-aux.el (and for dired-x.el).
;;     eval-after-load for read-shell-command: Don't do it only when mailcap can be loaded.
;; 2009/02/01 dadams
;;     icicle-define-minibuffer-maps: Bind C-backspace to icicle-up-directory.
;; 2009/01/25 dadams
;;     icicle-(redefine|restore)-standard-commands:
;;       Aliases for dired-guess-shell-command, read-shell-command.  Also eval-after-load's.
;; 2009/01/23 dadams
;;     icicle-(bind|restore)-other-keymap-keys:
;;       For sh-mode-map, remap comint-dynamic-complete, don't (un)bind TAB.  Thx to Seb Luque.
;; 2009/01/18 dadams
;;     Renamed Open Dired for Saved Completion Candidates to Open Dired for Chosen Files.
;; 2009/01/06 dadams
;;     Added to dolist of eval-after-load's: net-utils, rlogin, idlw-shell.
;; 2009/01/05 dadams
;;     icicle-(bind|restore)-other-keymap-keys: Treat keys for Shell Script, Ielm, Tcl, and GUD.
;;     icicle-(redefine|restore)-standard-commands:
;;       Handle: comint-dynamic-complete-filename, gud-gdb-complete-command.
;;     Added eval-after-load for gud.
;;     Added to dolist of eval-after-load's: ielm, gud, sh-script, tcl.
;; 2009/01/04 dadams
;;     Added ESS support:
;;       icicle-(redefine|restore)-standard-commands:
;;         Added: (icicle|old)-comint-replace-by-expanded-filename,
;;                (icicle|old)-ess-complete-object-name.
;;       Added eval-after-load for ess-site and for old-comint-replace-by-expanded-filename.
;;       Thx to Sebastian Luque.
;; 2008/12/30 dadams
;;     icicle-mode: Don't add/remove hook icicle-shell-hook-fn.
;;                  Don't call icicle-(un)bind-isearch-keys.
;;     Renamed: icicle-rebind-other-keymap-keys to icicle-bind-other-keymap-keys.
;;     icicle-(bind|restore)-other-keymap-keys:
;;       Call icicle-(un)bind-isearch-keys here.
;;       Bind/restore icicle-comint-command here.
;;       Don't use eval-after-load.  Instead, define keys if the maps are defined.
;;       Bind/restore shell keys only if icicle-redefine-standard-commands-flag is true.
;;     icicle-minibuffer-setup:
;;       Remove redundant code that calls icicle-define-cycling-keys for each minibuffer map.
;;     icicle-define-minibuffer-maps:
;;       Restore C-g to abort-recursive-edit in minibuffer-local-must-match-map.
;;     icicle-(redefine|restore)-standard-commands:
;;       Test fboundp of old-*, not *, for bbdb and comint.
;;       Do not defalias comint-dynamic-complete-filename or shell-dynamic-complete-*.
;;     icicle-remap: Moved to icicles-opt.el.
;;     At end of file:
;;       eval-after-load comint and bbdb: Turn off icy-mode before fset old-*.
;;       eval-after-load each of the other keymap libraries: Toggle icy-mode.
;; 2008/12/26 dadams
;;     Bind icicle-widen-candidates to M-+, and add it to Minibuf menu.
;;     Bind icicle-apropos-complete-and-widen to S-backspace.
;; 2008/12/22 dadams
;;     Forgot to add/remove icicle-shell-hook-fn to shell-mode-hook for Emacs 20.
;;     icicle-bind-completion-keys, icicle-define-icicle-maps: Added icicle-regexp-quote-input.
;; 2008/12/21 dadams
;;     icicle-(redefine|restore)-standard-commands: Added comint-*, shell-*.
;;     Renamed *-(rebind|restore)-non-completion-keys to *-(rebind|restore)-other-keymap-keys.
;;     icy-mode: Add/remove icicle-shell-hook-fn to shell-mode-hook.
;;     icicle--(rebind|restore)-other-keymap-keys:
;;       Don't rebind Info keys unless icicle-redefine-standard-commands-flag.
;;       Wrap Dired key bindings in eval-after-load.
;;     icicle-restore-other-keymap-keys: Corrected (updated) Dired keys.
;; 2008/12/07 dadams
;;     icicle-minibuffer-setup:
;;       Add completing prompt prefix here, using icicle-completion-prompt-overlay.
;;       Removed icicle-prompt.
;; 2008/12/05 dadams
;;     icicle-(bind|restore)-completion-keys: Bind C-v, M-v to scroll *Completions* window.
;; 2008/11/14 dadams
;;     icicle-toggle-hiding-common-match:
;;       icy-mode: Mention it in doc string, icicle-define-icicle-maps: Add it to menus.
;;       icicle-bind-completion-keys: Bind it to C-M-.
;;     icicle-minibuffer-setup:
;;       Use (cadr (buffer-list)), not other-buffer, for icicle-pre-minibuffer-buffer
;; 2008/11/04 dadams
;;     icicle-define-icicle-maps: No longer bind icicle-generic-S-tab keys (obsolete).
;;     Renamed:
;;       *-(un)bind-S-TAB-in-keymaps-from to *-(un)bind-key-completion-keys-in-keymaps-from,
;;       *-(un)bind-S-TAB-for-map-variable to *-(un)bind-key-completion-keys-for-map-var.
;;     icicle-(un)bind-key-completion-keys-in-keymaps-from:
;;       icicle-generic-S-tab-keys -> icicle-key-complete-keys.
;;       Respect icicle-complete-key-anyway-flag.
;;     icicle-define-minibuffer-maps: *-generic-S-tab-keys -> *-previous-candidate-keys.
;;     icicle-(bind|restore)-completion-keys: *-generic-S-tab-keys -> *-apropos-complete-keys.
;;     icicle-(un)bind-isearch-keys: *-generic-S-tab-keys -> *-search-from-isearch-keys.
;; 2008/11/03 dadams
;;     Applied renamings from icicles-cmd.el.
;; 2008/11/01 dadams
;;     Require cl.el at compile time for all Emacs versions, not just 20.
;; 2008/10/14 dadams
;;     icy-mode: No longer call icicle-update-help-string.
;;     Renamed: *-update-help-string to *-help-string-completion and moved to icicles-mcmd.el.
;;     Renamed: icicle-rebind-completion-maps to icicle-define-minibuffer-maps.
;;     Applied renaming from icicles-mcmd.el: icicle-completion-help to icicle-minibuffer-help.
;;     icicle-define-minibuffer-maps:
;;       Bound icicle-minibuffer-help also in non-completion minibuffer maps and *Completions*.
;;       Bound icicle-yank-secondary in minibuffer-local-isearch-map also.
;;     icicle-(bind|restore)-completion-keys: Only bind icicle-minibuffer-help if not inherited.
;; 2008/10/12 dadams
;;     icicle-rebind-non-completion-keys (and undo in icicle-restore-non-completion-keys):
;;       Ibuffer mode: Bind M-s i to icicle-search-ibuffer-marked and add to Operate menu.
;;       Buffer Menu mode: Bind M-s i to icicle-search-buff-menu-marked.
;;       Dired mode: Change binding of icicle-search-dired-marked from M-s to M-s i.
;;                   Removed vestigial binding to C-M-r.
;;       Make eval-after-load for "info" unconditional.
;; 2008/10/11 dadams
;;     icicle-update-help-string, icicle-(bind|restore)-completion-keys:
;;       icicle-kill-failed-input -> icicle-goto/kill-failed-input.
;;     icicle-define-cycling-keys: Typo: icicle-apropos-cycle-previous-keys -> *-action*.
;; 2008/10/10 dadams
;;     icicle-minibuffer-setup:
;;       Make icicle-current-completion-mode respect *-cycling-respects-completion-mode-flag.
;;       Initial *Completions* display respects icicle-cycling-respects-completion-mode-flag.
;;     icicle-minibuffer-setup, icicle-bind-completion-keys: Use icicle-define-cycling-keys.
;;     icicle-restore-completion-keys: Restore (C-)up, (C-)down, (C-)next, (C-)prior.
;;     Added: icicle-define-cycling-keys.
;; 2008/10/08 dadams
;;     icicle-bind-completion-keys:
;;       Bind icicle-candidate-set-retrieve-more to C-<.  Add to Minibuf menu.
;;     icicle-define-icicle-maps, icicle-update-help-string: Remove C-< for angle brackets.
;;     icicle-minibuffer-setup:
;;       Set icicle-pre-minibuffer-buffer to (other-buffer nil t), not (cadr (buffer-list).
;; 2008/10/04 dadams
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Substitute Icicles version of completing-read-multiple and maps.  Thx to Per Nordlow.
;;     icy-mode doc string: Typo - thx to Richard Kim.
;; 2008/10/01 dadams
;;     icicle-update-help-string: Added current values for toggles.  Moved toggles near top.
;;     Added: icicle-S-iso-lefttab-to-S-TAB.
;; 2008/09/30 dadams
;;     icicle-bind-isearch-keys: For icicle-generic-S-tab-keys, read the search string using
;;       icicle-isearch-complete-past-string.
;; 2008/09/13 dadams
;;     Use renamings from icicles-mcmd.el:
;;       icicle-candidate-set-save-to-cache-file to icicle-candidate-set-save-persistently,
;;       icicle-candidate-set-retrieve-from-cache-file to *-candidate-set-retrieve-persistent.
;; 2008/09/11 dadams
;;     icicle-define-icicle-maps: Added icicle-grep-saved-file-candidates to menus.
;; 2008/09/09 dadams
;;     Use renamings from icicles-cmd.el:
;;       icicle-candidate-set-dired-marked-save-* to icicle-dired-save-marked-*.  
;;     Bind: icicle-dired-save-marked-* to C(-M)->, not C(-M)-),
;;           icicle-dired-saved-file-candidates-other-window to C-M-<, not C-M-r,
;;           icicle-dired-save-marked-(to-variable|as-project) to C-M-}, C-},
;;           icicle-dired-project-other-window to C-{.
;;     Add to Dired > Multiple (or Operate) > Icicles menu: icicle-dired-save-marked-as-project.
;;     Add to Dired > Dir and File > Icicles menus: icicle-dired-project-other-window.
;;     Removed from File > Icicles menu: icicle-dired-saved-file-candidates.
;; 2008/08/22 dadams
;;     icicle-update-help-string: Removed mention of icicle-Completions-window-default-width.
;; 2008/08/21 dadams
;;     icicle-define-icicle-maps: Replace icicle-find-file(-*) with icicle-file.
;;     icicle-mode doc string, icicle-update-help-string: Updated for new cmds icicle-file etc.
;;     Define BBDB aliase only if BBDB is loaded. 
;; 2008/08/17 dadams
;;     icicle-rebind-completion-maps: Bind mouse wheel for completion-list-mode-map.
;; 2008/08/12 dadams
;;     icicle-define-icicle-maps: Add :keys for icicle-goto(-global)-marker.
;; 2008/08/08 dadams
;;     icicle-define-icicle-maps:
;;       Added icicle-goto(-global)-marker to icicle-(bookmark|search)-menu-map.
;;     Soft require of menu-bar+.el, instead of just eval-when-compile.
;; 2008/08/07 dadams
;;     icicle-(redefine|restore)-*: Don't use symbol-function for target of defalias.
;; 2008/08/04 dadams
;;     Use condition-case when require mb-depth+.el.
;;     icicle-(rebind|restore)-completion-maps, icicle-bind-completion-keys:
;;       Updated Minibuf menu to add icicle-clear-current-history.
;;     icicle-restore-completion-keys:
;;       Added: (alt-)action(-list)-all, icicle-save-predicate-to-variable.
;; 2008/08/03 dadams
;;     icicle-mode: Updated doc string.
;;     icicle-update-help-string: Added clear-history stuff and changed bindings.
;;     icicle-rebind-completion-maps: Added binding: icicle-clear-current-history (M-i).
;;     icicle-bind-completion-keys:
;;       Added to Minibuf menu: icicle-all-candidates(-list)(-alt)-action.
;;       Added bindings: icicle-all-candidates-list-action (M-!),
;;                       icicle-all-candidates-list-alt-action (M-|),
;;                       icicle-clear-current-history (M-i).
;;       Changed bindings:
;;         icicle-all-candidates-alt-action (C-S-insert to C-|)
;;         icicle-toggle-expand-to-common-match (C-| to C-;),
;;         icicle-toggle-search-replace-common-match (C-M-| to M-;),
;;       
;; 2008/08/01 dadams
;;     icicle-mode: Std mb-depth.el renamed *-indicate-depth-mode  to *-depth-indicate-mode.
;; 2008/07/30 dadams
;;     icicle-update-help-string: Make C-M-) more obvious for clearing saved candidates.
;; 2008/07/23 dadams
;;     Renamed: icicle-map to icicle-apply.
;; 2008/07/21 dadams
;;     icicle-(redefine|restore)-standard-commands: Added icicle-bbdb-complete.
;; 2008/06/03 dadams
;;     icy-mode: Added icicle-toggle-C-for-actions to doc string.
;;     icicle-define-icicle-maps: Added icicle-toggle-C-for-actions to menus.
;;     icicle-update-help-string:
;;       Mention icicle-insert-history-element, icicle-toggle-C-for-actions.
;;     icicle-bind-completion-keys: Bindings according to icicle-use-C-for-actions-flag.
;;     icicle-(bind|restore)-completion-keys: Bind/unbind M-g.
;; 2008/05/27 dadams
;;     Renamed: icicle-(un)bind-isearch-completion-keys to icicle-(un)bind-isearch-keys.
;;     icicle-bind-isearch-keys: Bind S-TAB to icicle-search, C-o to isearch-(m)occur.
;;                               Likewise, icicle-unbind-isearch-keys.
;; 2008/05/25 dadams
;;     Renamed: icicle-bind-isearch-keys to icicle-bind-isearch-completion-keys.
;;     Added: icicle-unbind-isearch-completion-keys.
;;     icy-mode: icicle-(un)bind-isearch-completion-keys instead of updating isearch-mode-hook.
;;     icicle-bind-isearch-completion-keys:
;;       Don't bind anything in minibuffer-local-isearch-map (overridden by icicle-mode-map).
;; 2008/05/22 dadams
;;     icicle-update-help-string: Mention C-u for read-file-name-completion-ignore-case.
;; 2008/05/11 dadams
;;     Moved icicle-bind-top-level-commands to icicles-opt.el (and added optional arg).
;;     Renamed icicle-fit-Completions-window to icicle-fit-completions-window.
;; 2008/05/10 dadams
;;     Renamed: icicle-bind-top-level-commands-alist to icicle-top-level-key-bindings.
;;     icicle-bind-top-level-commands: Don't eval the key (binding).
;; 2008/05/07 dadams
;;     icicle-define-icicle-maps: Use icicle-bind-top-level-commands instead of hard-coding.
;;     icicle-update-help-string: icicle-bind-*-flag -> icicle-bind-*-alist.
;; 2008/04/25 dadams
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Never set icicle-old-read-file-name-fn to icicle-read-file-name.
;;         Thx to Alexey Romanov and Per Nordlow.
;;     icicle-define-icicle-maps, icicle-(bind|restore)-completion-keys:
;;       Bound icicle-toggle-remote-file-testing (C-^).
;;       icicle-toggle-ignored-space-prefix is now bound to M-_, not C-^.
;;     icicle-update-help-string: Updated bindings accordingly.
;; 2008/04/18 dadams
;;     Renamed icicle-init-value-flag to icicle-default-value.
;; 2008/03/30 dadams
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Set and swap read-file-name-function and icicle-old-read-file-name-fn for Emacs 22+.
;;     Top-level:
;;       Moved fset old-read-file-name here, renamed it orig-read-file-name.  Not for Emacs 22.
;; 2008/03/29 dadams
;;     icicle-mode: No longer use icicle-control-reminder-prompt on kill-emacs-hook.
;;     icicle-update-help-string:
;;       Removed: icicle-completing(-mustmatch)-prompt-prefix, icicle-reminder-prompt-flag.
;;     icicle-rebind-completion-maps: Remove code setting icicle-prompt-suffix (removed).
;; 2008/03/26 dadams
;;     Added icicle-Info-menu to icicle-mode doc string and icicle-update-help-string.
;;     icicle-define-icicle-maps: Added icicle-Info-menu to menus.
;;     icicle-(rebind|restore)-non-completion-keys: Bind/restore icicle-Info-menu-cmd.
;; 2008/03/23 dadams
;;     Added: icicle-handle-switch-frame.
;;     icicle-(rebind|restore)-non-completion-keys:
;;       Bind switch-frame to icicle-handle-switch-frame globally, and restore.
;; 2008/03/19 dadams
;;     Remap (pp-)eval-expression to new icicle-pp-eval-expression.
;;     Replace lambdas by icicle-pp-eval-expression-in-minibuffer.
;;     Use icicle-pp-eval-expression-in-minibuffer (new) in help string.
;;     icicle-rebind-completion-maps: Bind M-: to icicle-pp-eval-expression-in-minibuffer.
;;       Bind icicle-insert-newline-in-minibuffer in all minibuffer maps (likewise, non-icy).
;;       Bind C-g in minibuffer-local-must-match-map, even if inherit.
;;     icicle-bind-completion-keys: Don't bind in must-match if it inherits:
;;       C-a, C-e, C-=, M-k, M-o, M-., M-:, C-M-y, M-S-(backspace|delete)
;; 2008/03/09 dadams
;;     icicle-mode: Add icicle-unhighlight-lighter to minibuffer-exit-hook.
;; 2008/03/07 dadams
;;     icicle-abort-minibuffer-input:  Thx to Damon Permezel.
;;       Make it always call abort-recursive-edit.  Renamed to icicle-abort-recursive-edit.
;; 2008/02/28 dadams
;;     icicle-define-icicle-maps: Don't bind pop-tag-mark (M-*) in icicle-mode-map for Emacs 21.
;; 2008/02/26 dadams
;;     Remapped where-is to icicle-where-is.
;;     Added to Describe > Icicles menu: icicle-describe-option-of-type, icicle-where-is.
;;     Bound icicle-select-frame to C-x 5 o in Icicle mode.
;; 2008/02/24 dadams
;;     icicle-define-icicle-maps: Bound icicle-next-apropos-match-function to M-(.
;;     Add icicle-next-apropos-match-function to icicle-update-help-string and mode doc string.
;; 2008/02/23 dadams
;;     Renamed: icicle-search-tag to icicle-find-tag,
;;              icicle-find-tag(-other-window) to icicle-find-first-tag(-other-window).
;;     icicle-define-icicle-maps: Added icicle-tags-search to Tags menu and Search menu.
;; 2008/02/22 dadams
;;     Renamed: icicle(-mouse)-save-candidate to icicle(-mouse)-save/unsave-candidate.
;; 2008/02/13 dadams
;;     Remapped: find-tag (M-.) to icicle-search-tag, instead of icicle-find-tag
;;               pop-tag-mark (M-*) to icicle-pop-tag-mark.
;;     icicle-define-icicle-maps:
;;       Added icicle-search-tag and icicle-pop-tag-mark to menus.
;;       Removed icicle-find-tag from menu.  Renamed icicle-find-tag-other-window menu item.
;; 2008/02/07 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Protect read-number with fboundp.
;; 2008/02/03 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Added (icicle|old)-choose-completion.
;; 2008/01/30 dadams
;;     Bound icicle-yank-secondary and icicle-mouse-yank-secondary.
;; 2008/01/15 dadams
;;     Require dired.el.  Soft-require dired+.el. Thx to Fabrice Knevez.
;; 2008/01/13 dadams
;;     icicle-(rebind|restore)-non-completion-keys:
;;       Bound [handle-switch-frame] to icicle-skip-this-command globally.
;;     Added: icicle-skip-this-command.
;; 2008/01/02 dadams
;;     icicle-(rebind|restore)-non-completion-keys: Bound icicle-search-dired-marked to M-s.
;;     icicle-define-icicle-maps: Added icicle-search-dired-marked to Dired>Multiple>Icicles.
;; 2008/01/01 dadams
;;     icicle-*-non-completion-keys: Bound icicle-candidate-set-dired-marked-save(-more).
;;     icicle-define-icicle-maps: Added Icicles submenu for Dired > Multiple (Operate) menu.
;; 2007/12/31 dadams
;;     Bound icicle-insert-list-join-string to C-M-j.  Update icicle-update-help-string.
;; 2007/12/24 dadams
;;     icicle-rebind-completion-maps: Bound C-j to also refit the minibuffer frame.
;; 2007/12/20 dadams
;;     Bound icicle-dired-saved-file-candidates-other-window in Dired to C-M-r.
;;     Bound icicle-describe-option-of-type to C-h C-o, not C-h M-o.
;; 2007/12/18 dadams
;;     icicle-define-icicle-maps: Bind icicle-describe-option-of-type to C-h M-o.
;; 2007/12/14 dadams
;;     icicle-mode: Only add to kill-emacs-hook if icicle-customize-save-flag is non-nil.
;; 2007/12/13 dadams
;;     icicle-update-help-string: spell out options, don't abbreviate using *.
;; 2007/12/08 dadams
;;     Bound icicle-(beginning|end)-of-line+.
;; 2007/12/07 dadams
;;     icicle-rebind-completion-maps: Removed obsolete stuff from doc string.
;; 2007/12/03 dadams
;;     Renamed longest common match (lcm) to expanded common match (ecm).
;; 2007/11/29 dadams
;;     icicle-minibuffer-setup: If icicle-add-proxy-candidates-flag is nil, swap candidate sets.
;;                              Reset icicle-saved-proxy-candidates to nil.
;; 2007/11/27 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Added read-number.
;; 2007/11/25 dadams
;;     icicle-define-icicle-maps: Bound icicle-command-abbrev to C-x SPC.
;;     icicle-mode: Use icicle-command-abbrev-save on kill-emacs-hook.
;;     Changed binding of icicle-doremi-inter-candidates-min-spaces from C-x SPC to C-x |.
;; 2007/11/23 dadams
;;     icicle-rebind-completion-maps:
;;       Use icicle-prefix-cycle-(next|previous)-keys, instead of hardcoding.
;;     icicle-(bind|restore)-completion-keys:
;;       Use icicle-(apropos|prefix)-cycle-(next|previous)-keys, instead of hardcoding.
;;       Restore explicit vanilla bindings last.
;; 2007/11/22 dadams
;;     icicle-(bind|restore)-completion-keys:
;;       Explicitly bind/restore (C-)up, (C-)down, (C-)next, (C-)(C-)prior, instead of
;;         remapping next-line etc.
;; 2007/11/21 dadams
;;     icicle-rebind-completion-maps: Explicitly bind C-j to icicle-self-insert.
;;     icicle-(bind|restore)-completion-keys, icicle-update-help-string:
;;       Removed C-o binding for icicle-candidate-action.
;; 2007/11/17 dadams
;;     Added doc, menus, bindings (C-M-_): icicle-toggle--proxy-candidates.
;; 2007/11/05 dadams
;;     icicle-define-icicle-maps: Moved [Icy] items to Icicles submenus.
;;     Added: icicle-(bookmark|custom|describe|edit|file|frames|info|search(-tags))-menu-map.
;; 2007/11/03 dadams
;;     icicle-define-icicle-maps, icicle-bind-S-TAB-in-keymaps-from,
;;       icicle-unbind-S-TAB-in-keymaps-from, icicle-rebind-completion-maps,
;;       icicle-bind-completion-keys:
;;         Bind icicle-generic-S-tab-keys instead of hard-coded S-(iso-left)tab.
;;     icicle-(bind|restore)-completion-keys:
;;       Bind/restore icicle-prefix-complete-keys,
;;         icicle-(apropos|prefix)-complete-keys-no-display, not hard-coded keys.
;;     icicle-bind-isearch-keys: Bind icicle-isearch-complete-keys, not hard-coded.
;;     Renamed icicle-modal-cycle-(up|down)-key to icicle-modal-cycle-(up|down)-keys,
;;             icicle-word-completion-key to icicle-word-completion-keys.
;; 2007/10/31 dadams
;;     icicle-define-icicle-maps:
;;       Moved options to new Icicles submenu of Options menu, and removed [Icy].  Added :keys.
;;     Added: icicle-options-menu-map.
;; 2007/10/28 dadams
;;     Added doc, menus, bindings (C-|, C-M-|):
;;       icicle-toggle-(expand-to-common-match|search-replace-common-match).
;; 2007/10/21 dadams
;;     icicle-(bind|restore)-completion-keys: Bind C-x w and C-x SPC.
;;     icicle-define-icicle-maps: Add Do Re Mi items.
;;     icicle-mode, icicle-update-help-string: Mention C-x w and C-x SPC.
;; 2007/09/29 dadams
;;     icicle-mode, icicle-define-icicle-maps, icicle-update-help-string:
;;       Added icicle-toggle-fuzzy-completion (and icicle-fuzzy-completion-flag to help string).
;;     icicle-(bind|restore)-completion-keys: Bind/unbind icicle-toggle-fuzzy-completion to C-(.
;; 2007/09/20 dadams
;;     icicle-(bind|restore)-completion-keys: Bind C-j to icicle-self-insert / exit-minibuffer.
;; 2007/09/18 dadams
;;     Added: icicle-update-help-string.  Use in icy-mode, not in icicle-rebind-completion-maps.
;;            Removed icicle-toggle-WYSIWYG-Completions (it has no minibuffer binding).
;; 2007/08/25 dadams
;;     icy-mode, icicle-completion-help-string:
;;       icicle-clear-option -> clear-option.  Added toggle alias.
;; 2007/08/21 dadams
;;     icicle-completion-help-string: Mention C-M-l.
;; 2007/08/19 dadams
;;     icicle-minibuffer-setup: Reset icicle-input-fail-pos.
;;     icicle-(bind|restore)-completion-keys:
;;       (Re|un)map reposition-window to icicle-kill-failed-input.
;; 2007/08/03 dadams
;;     icicle-mode: Remove icicle* hooks from local, not global, hooks.
;; 2007/07/22 dadams
;;     icicle-(redefine|restore)-standard-commands: Added customize-face(-other-window).
;;     Moved icicle-completing-p to icicles-fn.el.
;;     Require icicles-cmd.el.
;; 2007/07/06 dadams
;;     icicle-rebind-completion-maps:
;;       Moved icicle-Completions-mouse-3-menu to C-mouse-3.
;;       Added icicle(-mouse)-candidate-set-save(-more)-selected, icicle-candidate-set-retrieve,
;;             icicle-retrieve-previous-input.
;;     icicle-completion-help-string: Added icicle-candidate-set-save(-more)-selected.
;;     icicle-bind-completion-maps:
;;       Removed icicle-insert-history-element (inherited).
;;       Added: icicle-candidate-set-save-more(-selected), icicle-mouse-save-then-kill.
;; 2007/07/04 dadams
;;     icicle-rebind-completion-maps, icicle-(bind|restore)-completion-keys:
;;       Added icicle-insert-history-element to Minibuf menu.
;;     icicle-(bind|restore)-completion-keys:
;;       Added icicle-retrieve-(next|previous)-input to Minibuf menu.
;; 2007/07/03 dadams
;;     icicle-rebind-completion-maps, icicle-bind-completion-keys:
;;       Bind icicle-insert-history-element to M-o in all minibuffer maps.
;;     icicle-bind-completion-keys, icicle-completion-help-string:
;;       icicle-retrieve-(next|previous)-input, not icicle-retrieve-last-input.
;;     icicle-(redefine|restore)-std-completion-fns:
;;       defalias next-history-element to icicle-next-history-element.
;;     Removed defadvice for next-history-element.  Redefine in icicles-mcmd.el instead.
;; 2007/06/22 dadams
;;     Bound icicle-search-keywords and added to menus and help strings.
;; 2007/06/20 dadams
;;     Removed M-o binding for icicle-toggle-WYSIWYG-Completions.
;; 2007/06/19 dadams
;;     icicle-bind-completion-keys: Add icicle-save-predicate-to-variable to menus.
;;     icicle-completion-help-string:
;;       Mention icicle-save-predicate-to-variable and icicle-insert-string-from-variable.
;; 2007/06/18 dadams
;;     icy-mode doc string, icicle-completion-help-string: Added icicle-customize-face.
;;     icicle-define-icicle-maps: Added icicle-customize-face to menus.
;; 2007/06/17 dadams
;;     Bound icicle-toggle-WYSIWYG-Completions to M-o.
;;     icicle-minibuffer-setup: Reinitialize icicle-saved-candidate-overlays.
;; 2007/06/16 dadams
;;     icicle-(bind|restore)-completion-keys: Bound C-M-(help|f1).
;; 2007/06/15 dadams
;;     icicle-completion-help-string: Added and cleaned up set stuff.
;;     icicle-(bind|restore)-completion-keys: Cleanup.  Added menu items.
;; 2007/06/14 dadams
;;     Swap bindings for C-insert and insert.
;; 2007/06/13 dadams
;;     Bound C-insert and C-> to icicle-save-candidate and icicle-candidate-set-save-more.
;; 2007/06/12 dadams
;;     icicle-rebind-completion-maps: Bound icicle-mouse-save-candidate to M-S-mouse-2.
;; 2007/06/10 dadams
;;     icicle-mode: comint-mode-hook, compilation(-minor)-mode-hook, temp-buffer-show-hook.
;; 2007/06/08 dadams
;;     icy-mode: Added icicle-find-tag* to doc string.
;;     icicle-define-icicle-maps:
;;       Added icicle-find-tag*.  Remap find-tag* to icicle-find-tag*.
;;       Corrected Info menu.
;;     icicle-completion-help-string: Added icicle-find-tag*.
;; 2007/05/28 dadams
;;     icicle-restore-non-completion-keys: Unbind S-tab.
;;     Added: icicle-unbind-S-TAB-for-map-variable, icicle-unbind-S-TAB-in-keymaps-from.
;;     icicle-bind-S-TAB-in-keymaps-from: Treat S-tab and S-iso-lefftab separately.
;;     icicle-define-icicle-maps: Added icicle-imenu-* to Icicles/Search menus.
;; 2007/05/22 dadams
;;     Make [Icy] menu items invisible when not in Icicle mode.  Add :keys where appropriate.
;;     icicle-define-icicle-maps, icicle-rebind-completion-maps,
;;     icicle-(bind|restore)-completion-keys:
;;       icicle-menu-item-any-version -> menu-item.  Explicit put of enable property -> :enable.
;;     Don't require icicles-mac.el.
;;     icicle-bind-completion-keys: Added icicle-narrow-candidates, and corrected :enable forms.
;; 2007/05/21 dadams
;;     icicle-define-icicle-maps:
;;       Remap minibuffer-keyboard-quit to icicle-abort-minibuffer-input.  Needed, even though
;;         local-must-match inherits from local-completion in Emacs 22, because delsel.el binds
;;         C-g to minibuffer-keyboard-quit in minibuffer maps.
;;     menu-item-any-version -> icicle-menu-item-any-version.
;;     Added Icicles/Search menu items:
;;       Search (Buffer|File|Saved Region|All Saved Regions|Definition|Text Property).
;;     Renamed: Search a Region -> Search Saved Region, Choose a Region -> Choose Saved Region,
;;              Add Current Region to List -> Save Current Region.
;; 2007/05/20 dadams
;;     Enable menu-bar Minibuf:
;;       icicle-rebind-completion-maps:
;;         Use menu-item-any-version.
;;         Don't define menu for maps if it is defined by parent map.
;;         Add Enter and Help items for *-local-map.
;;         Add Enter, Help and Quit items for *-local-(ns|isearch)-map.
;;       icicle-bind-completion-keys:
;;         Use menu-item-any-version.
;;         Add Enter item for *-local-completion-map, unless defined by parent map.
;;       icicle-restore-completion-keys:
;;         Use menu-item-any-version.
;;         Add Enter and Quit items for *-local-completion-map, unless defined by parent map.
;;         Do not unmap kill-region(-wimpy).
;;         Bind [(control pause)] to nil.
;; 2007/05/13 dadams
;;     icicle-restore-completion-keys: Restore some forgotten minibuf menu items.
;; 2007/05/08 dadams
;;     Bound icicle-save-predicate-to-variable to C-M-&.
;; 2007/05/06 dadams
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string.
;;     Added defvars to quiet byte compiler.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/03 dadams
;;     Remap icicle-yank-function, not yank.
;;     icicle-define-icicle-maps: Bind icicle-search-word.
;;     icicle-mode, icicle-completion-help-string: Add icicle-search-word to doc.
;; 2007/05/02 dadams
;;     Bound M-q to icicle-dispatch-M-q, not to icicle-insert-key-description.
;;     Bound C-` to icicle-toggle-regexp-quote, not to icicle-dispatch-C-backquote.
;;     Bound C-M-` to icicle-toggle-literal-replacement.
;;     Update icicle-completion-help-string.
;; 2007/04/20 dadams
;;     icicle-minibuffer-setup: Don't reset icicle-search-context-level here.
;; 2007/04/17 dadams
;;     Bound M-, to icicle-dispatch-M-comma, not to icicle-change-alternative-sort-order.
;;     Bound C-` to icicle-dispatch-C-backquote, not to icicle-toggle-regexp-quote.
;; 2007/04/10 dadams
;;     icicle-minibuffer-setup: Initialize icicle-search-context-level.
;; 2007/04/09 dadams
;;     Bound icicle-imenu to C-c =.
;; 2007/04/08 dadams
;;     Bound icicle-all-candidates-alt-action to C-S-insert.
;; 2007/04/07 dadams
;;     icicle-completion-help-string: Updated.
;;     Bound icicle-dispatch-C-comma to C-,.
;;     Bound in menu: icicle-toggle-search-replace-whole.
;;     Bound icicle-(next|previous)-(apropos|prefix)-candidate-alt-action (forgot).
;; 2007/04/02 dadams
;;     Bound icicle-search-text-property to C-c ".  Added it to icicle-completion-help-string.
;; 2007/03/23 dadams
;;     Bound icicle-apropos-complete-and-narrow to S-SPC.  Mention in *-completion-help-string.
;; 2007/03/14 dadams
;;     Added: icicle-top-level-prep.
;;     Removed: icicle-reset-candidates-alist.
;;     Do top-level stuff in icicle-minibuffer-setup, not in icicle-mode.
;;     icicle-minibuffer-setup: Add icicle-top-level-prep to pre-command-hook.
;;     icicle-mode: Remove icicle-top-level-prep from pre-command-hook.
;; 2007/03/07 dadams
;;     icicle-cancel-Help-redirection: Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/02 dadams
;;     icicle-bind-S-TAB-in-keymaps-from: Bound S-iso-lefttab also.
;; 2007/03/02 dadams
;;     icicle-define-icicle-maps:
;;       Bound S-iso-lefttab also to icicle-generic-S-tab.  Thx to Shreevatsa R.
;; 2007/02/28 dadams
;;     Added: icicle-reset-candidates-alist.
;;     icicle-mode: Use icicle-reset-candidates-alist.
;; 2007/02/27 dadams
;;     icicle-minibuffer-setup: Wait icicle-incremental-completion-delay before initial display.
;; 2007/02/24 dadams
;;     Bound: delete, S-mouse-2 to icicle(-mouse)-remove-candidate,
;;            C-S-RET, C-S-mouse-2 to icicle(-mouse)-candidate-alt-action,
;;            S-delete to icicle-delete-candidate-object.
;;     Don't remap icicle-kill-region(-wimpy) to delete key.
;; 2007/02/17 dadams
;;     Added: icicle-bind-S-TAB-in-keymaps-from, icicle-bind-S-TAB-for-map-variable.
;;     icicle-rebind-non-completion-keys:
;;       Bind S-TAB to keymaps in icicle-keymaps-for-key-completion.
;; 2007/02/02 dadams
;;     icicle-completing-p: Cache the value in variable icicle-completing-p.
;;     icicle-minibuffer-setup: Reset icicle-completing-p to nil.
;;     icicle-activate-mark: Use var, not function, icicle-completing-p, but after minibuf test.
;; 2007/01/23 dadams
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Added icicle-read-face-name, icicle-face-valid-attribute-values.
;;     icicle-define-icicle-maps, icicle-rebind-completion-maps:
;;       Updated wrt toggles.  Added icicle*-highlight-historical-candidates*.
;;     icicle-bind-completion-keys: Added icicle-toggle-highlight-historical-candidates.
;; 2007/01/22 dadams
;;     Renamed icicle-regions to icicle-region-alist (forgot occurrences here).
;; 2007/01/20 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Added icicle-display-completion-list.
;; 2007/01/15 dadams
;;     Moved C-, binding from icicle-toggle-sorting to icicle-change-sort-order.
;;     Moved icicle-toggle-alternative-sorting from M-, to C-M-,.
;;     Bound icicle-change-alternative-sort-order to M-,.
;;     Updated list of options in icicle-completion-help-string.
;; 2007/01/12 dadams
;;     Removed: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map.
;;              Not used in minibuffer hooks.
;;     Removed [pause] bindings from minibuffer maps.
;;     Removed remap of yank in minibuffer maps.
;;     No longer bind icicle-remove-Completions-window in minibuffer maps.
;; 2007/01/11 dadams
;;     Renamed: icicle-define-icicle-mode-map to icicle-define-icicle-maps.
;;     icicle-define-icicle-maps: Use icicle-menu-map.  Don't recreate it.
;;     Bound [pause] to icicle-switch-to/from-minibuffer in all minibuffer maps.
;; 2007/01/10 dadams
;;     Added: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map,
;;            icicle-(rebind|restore)-non-completion-keys.
;;     Added: icicle-rebind-global: This used to be called icicle-remap.
;;     icicle-(remap|unmap): Different purpose and use now.  Redefined to use remapping when
;;        available (as was done before for self-insert-command).
;;     icicle-mode:
;;       Add, remove as minibuffer setup and exit hooks: icicle-override-maps-w-minibuffer-map,
;;                                                       icicle-restore-overriding-local-map.
;;       Call icicle-(rebind|restore)-non-completion-keys.
;;     icicle-define-icicle-mode-map:
;;       Use icicle-remap where previously used substitute-key-definition for top-level cmds.
;;       Moved to icicle-(rebind|restore)-non-completion-keys:
;;         binding of Info commands in Info map and S-tab in all keymaps (to *-rebind-* only).
;;     icicle-(bind|restore)-completion-keys: Use new icicle-(remap|unmap) where possible.
;;       Use icicle-rebind-global and substitute-key-definition for keys defined in vanilla
;;         completion maps.
;; 2007/01/06 dadams
;;     icicle-mode: Update doc and bind icicle-toggle-~-for-home-dir to M-~.
;; 2007/01/01 dadams
;;     Moved assq-delete-all to icicles-fn.el.
;;     Require at runtime, not compile-time: icicles-var.el, icicles-fn.el.
;; 2006-12-31 dadams
;;     icicle-define-icicle-mode-map: Delete icicle-mode entry from minor-mode-map-alist.
;;     icicle-mode: Unbind icicle-mode-map when the mode is turned off.
;;     Added assq-delete-all for Emacs 20.
;;     Use current-global-map function, not global-map variable.
;; 2006/12/25 dadams
;;     Bound icicle-candidate-set-truncate to M-$.
;; 2006/12/24 dadams
;;     icicle-bind-completion-keys: transpose-yank(-pop) -> yank(-pop): typo.
;;     Bound mouse-3 to icicle-Completions-mouse-3-menu in completion-list-mode-map.
;; 2006/12/22 dadams
;;     Bound icicle-exchange-point-and-mark.
;;     :group 'icicles -> :group 'Icicles-Miscellaneous.
;; 2006/12/17 dadams
;;     Bound icicle(-mouse)-candidate-read-fn-invoke.
;; 2006/12/16 dadams
;;     icicle-define-icicle-mode-map: Protect icicle-kmacro with fboundp.
;; 2006/12/12 dadams
;;     Added icicle-customize-*-group, icicle-kill-buffer, icicle-delete-windows to I. menu.
;;     Added + to multi-command menu items.
;; 2006/12/11 dadams
;;     Added icicle-customize-apropos* and icicle-Info-* to menu-bar menus.
;; 2006/12/10 dadams
;;     Updated user options list in icicle-completion-help-string.
;;     Updated list of icicle-opt stuff used here.
;; 2006/12/06
;;     icicle-select-minibuffer-contents:
;;       Use icicle-minibuffer-prompt-end, not point-min.  Thx to Erik Postma.
;; 2006/11/26 dadams
;;     Added icicle-regions stuff.
;; 2006/11/24 dadams
;;     icicle-redefine-standard-options: Treat icicle-kmacro-ring-max.
;;     Bind icicle-kmacro to f5
;;     Replaced icicle-select-window-or-frame by icicle-other-window-or-frame.
;;     Removed binding of icicle-select-frame.
;;     Do not require mb-depth+.el for Emacs 21 (do it only for Emacs 22).
;; 2006/11/23 dadams
;;     Bound icicle-execute-named-keyboard-macro to C-x M-e.
;; 2006/11/18 dadams
;;     Soft require mb-depth+.el instead of minibuf-depth.el.
;; 2006/11/17 dadams
;;     Bind icicle-select-window-or-frame to whatever other-window(-or-frame) is bound to.
;;     Bind icicle-select-frame to whatever other-frame is bound to.
;; 2006/11/09 dadams
;;     Bind icicle-dispatch-C-^, not icicle-toggle-ignored-space-prefix, to C-^.
;;     icicle-rebind-completion-maps: Updated doc string for icicle-dispatch-C-^.
;; 2006/11/05 dadams
;;     Bound icicle-occur to C-c '.  Added it to menu-bar menus.
;; 2006/10/18 dadams
;;     icy-mode: Invoke icicle-define-icicle-mode-map unconditionally, not just first time.
;; 2006/10/16 dadams
;;     icicle-define-icicle-mode-map: Try to avoid binding S-TAB to menu maps.
;; 2006/10/15 dadams
;;     icicle-define-icicle-mode-map: Simplified, corrected binding of S-TAB for key completion.
;;                                    Use a separate map for the menu bar.
;;     Moved here from icicles-fn.el:
;;       icicle-bind-isearch-keys, icicle-rebind-completion-maps,
;;       icicle-(redefine|restore)-standard-(commands|options),
;;       icicle-(redefine|restore)-std-completion-fns, icicle-(re|un)map,
;;       icicle-(bind|restore)-completion-keys, icicle-minibuffer-setup,
;;       icicle-cancel-*Help*-redirection, icicle-activate-mark,
;;       icicle-run-icicle-(pre|post)-command-hook, icicle-set-calling-cmd,
;;       icicle-undo-std-completion-faces icicle-update-ignored-extensions-regexp,
;;       icicle-completing-p, icicle-restore-region-face.
;;     Renamed: icicle-cancel-*Help*-redirection to icicle-cancel-Help-redirection.
;;     Moved here from icicles-cmd.el: icicle-select-minibuffer-contents, next-history-element.
;;     Moved to icicles-cmd.el: icicle-generic-S-tab.
;;     Require icicles-opt.el.
;;     Added eval-when-compile's and defvars to quite byte compiler.
;; 2006/09/23 dadams
;;     icicle-define-icicle-mode-map: Corrected binding of icicle-yank-insert.
;; 2006/09/22 dadams
;;     icicle-minibuffer-setup: Set this-command and last-command, for scrolling *Completions*.
;; 2006/09/18 dadams
;;     icicle-mode: Picked up all global prefixes for S-TAB.
;; 2006/09/17 dadams
;;     Added: icicle-generic-S-tab.  Bound to S-TAB.
;;     icicle-mode:
;;       Bound icicle-complete-keys to prefix keys followed by S-TAB.
;;       Added run-hooks for Emacs 22 version.
;; 2006/09/12 dadams
;;     Bound icicle-switch-to/from-minibuffer to [pause].
;; 2006/08/27 dadams
;;     Bound icicle-abort-minibuffer-input to what abort-recursive-edit is normally bound to.
;;       And add it to Icicle menu.
;; 2006/08/23 dadams
;;     Bound icicle-delete-window to what delete-window and delete-windows-for are normally
;;       bound to.
;;     Put use of Info-mode-map inside an eval-after-load.
;; 2006/08/18 dadams
;;     Added icicle-Info-goto-node-cmd to icicle-mode doc string.
;;       Substitute it for Info-goto-node binding.
;; 2006/08/13 dadams
;;     Added icicle-Info-index-cmd to icicle-mode doc string.
;;       Substitute it for Info-index binding.
;; 2006/08/04 dadams
;;     Added icicle-plist to menus.
;;     icicle-doc treats faces too now.
;; 2006/08/03 dadams
;;     Bound icicle-insert-yank to what yank is normally bound to.
;;     icicle-mode: Updated doc string.
;; 2006/07/29 dadams
;;     icy-mode, icicle-define-icicle-mode-map: Added missing toggle commands.
;; 2006/07/22 dadams
;;     Changed binding of C-c C-s for icicle-search to C-c ` for icicle-search-generic.
;;     Removed: add-hooks for icicle-compilation-search - see icicles-cmd.el.
;; 2006/06/08 dadams
;;     Converted global bindings in icicles-keys.el to icicle-mode-map bindings here.
;;     Added f10 binding for icicle-execute-menu-command.
;; 2006/05/19 dadams
;;     icicle-mode: (add-hook 'kill-emacs-hook 'icicle-control-reminder-prompt).
;; 2006/05/18 dadams
;;     Change :init-value to nil, per new Emacs convention.
;; 2006/05/13 dadams
;;     icicle-mode: Updated doc string.
;; 2006/05/10 dadams
;;     icicle-define-icicle-mode-map: Added menu item Send Bug Report.
;; 2006/04/03 dadams
;;     icicle-define-icicle-mode-map: Added icicle-toggle-(regexp-quote|incremental-completion).
;; 2006/03/16 dadams
;;     icicle-mode: Turn on minibuffer-indicate-depth-mode (Emacs 22 only).
;;     Added soft require of minibuf-depth.el for Emacs 22.
;; 2006/03/14 dadams
;;     Do not use icicle-reset-icicle-completing-p as minibuffer-exit-hook.
;; 2006/03/07 dadams
;;     Corrected menu items for icicle-doc (no name regexp input, just doc regexp).
;; 2006/03/05 dadams
;;     Moved here from icicle-opt.el: icicle-mode, icicle-mode-hook.
;;     Moved here from icicle-fn.el: icicle-mode-map.
;;     Added: icicle-define-icicle-mode-map.
 
;;;(@* "CHANGE LOG FOR `icicles-opt.el'")
;;
;; 2010/06/18 dadams
;;     Renamed: bookmarkp-* to bmkp-*.
;; 2010/06/09 dadams
;;     icicle-isearch-complete-keys:
;;       Added [escape tab] to default binding (isearch bizarrie).  Added C-M-TAB for all platforms.
;; 2010/06/08 dadams
;;     icicle-bookmark-refresh-cache-flag: Changed the default value to t to avoid confusion.
;; 2010/06/05 dadams
;;     Set icicle-top-level-when-sole-completion-delay to 0.7 from 0.0.
;; 2010/06/04 dadams
;;     Added: icicle-(buffers|files)-ido-like-flag, icicle-max-candidates.
;;     icicle-ignored-directories: Protect default value with boundp.
;;     icicle-type-actions-alist: Added (maximize|restore|toggle-max)-frame(-horizontally|-vertically).
;; 2010/05/30 dadams
;;     Added: icicle-ignored-directories.
;; 2010/05/15 dadams
;;     icicle-top-level-key-bindings: Updated Icicles versions of bookmark jump commands.
;;     icicle-keymaps-for-key-completion: Added bookmarkp-jump-map bookmarkp-jump-other-window-map.
;; 2010/04/30 dadams
;;     Added: icicle-no-match-hook.
;; 2010/04/09 dadams
;;     Remap find-file-read-only(-other-window) to icicle-find-file-read-only(-other-window).
;; 2010/04/02 dadams
;;     Removed: icicle-region-alist, icicle-region-auto-open-files-flag,
;;              icicle-region-bookmarks-flag, icicle-regions-name-length-max.
;;     icicle-top-level-key-bindings: Removed bookmarkp-bookmark-list-jump-other-window.
;;       bookmarkp*: Use condition (featurep 'bookmark+).
;; 2010/03/31 dadams
;;     Removed extra code redefining some bookmark commands.
;; 2010/03/28 dadams
;;     Removed: icicle-region-alist, icicle-region-auto-open-files-flag,
;;              icicle-region-bookmarks-flag, icicle-regions-name-length-max.
;; 2010/03/14 dadams
;;     Added: icicle-bookmark-refresh-cache-flag.
;; 2010/03/13 sadams
;;     Renamed icicle-add-buffer-name-flag to icicle-show-multi-completion-flag.  Doc string.
;; 2010/03/09 dadams
;;     icicle-color-themes: Initialize to ().  Do real init in cmd icicle-color-theme.
;; 2010/03/03 dadams
;;     Renamed: icicle-sort-function to icicle-sort-comparer,
;;              icicle-sort-functions-alist to icicle-sort-orders-alist
;;              icicle-alternative-sort-function to icicle-alternative-sort-comparer.
;;     Redefined to allow multi-sorting: icicle-sort-comparer, icicle-sort-orders-alist.
;; 2010/02/17 dadams
;;     Moved icicle-redefined-functions here from icicles-var.el
;;       and renamed to icicle-inhibit-advice-functions.
;;     Moved to icicles-face.el: icicle-increment-color-(hue|saturation).
;;     So now require icicles-face.el.
;;     Corrected alphabetical order.
;; 2010/02/13 dadams
;;     icicle-top-level-key-bindings: Bound icicle-bookmark-*-jump-other-window.
;; 2010/01/24 dadams
;;     icicle-thing-at-point-functions:
;;       Use region-or-word-nearest-point, not word-nearest-point.  Change order.
;; 2010/01/17 dadams
;;     icicle-top-level-key-bindings: Added Icicles remappings for bookmarkp-*-jump-other-*.
;; 2009/12/25 dadams
;;     icicle-top-level-key-bindings: Bind ESC-M-x to lacarte-execute-command.
;; 2009/12/13 dadams
;;     icicle-top-level-key-bindings: Map dired(-other-window) to icicle-dired(-other-window).
;; 2009/12/07 dadams
;;     icicle-guess-commands-in-path: Changed default value to nil.
;; 2009/11/27 dadams
;;     Added: icicle-swank-prefix-length, icicle-swank-timeout.
;;     icicle-TAB-completion-methods: Treat swank method also.
;; 2009/11/24 dadams
;;     Added: icicle-completions-format.
;;     icicle-color-themes: Fix: delete singleton list with string, not symbol bury-buffer.
;; 2009/11/22 dadams
;;     icicle-color-themes: Use color-theme-initialize instead of load-library, to load themes.
;; 2009/11/21 dadams
;;     icicle-color-themes: (load-library "color-theme-library"), for color theme version 6.6.0.
;; 2009/10/25 dadams
;;     Added: icicle-TAB-completion-methods.
;;     Removed: icicle-fuzzy-completion-flag, icicle-prefix-completion-is-basic-flag.
;;     Renamed: icicle-apropos-match-fns-alist to icicle-S-TAB-completion-methods-alist.
;; 2009/10/12 dadams
;;     icicle-top-level-key-bindings: Added bindings for icicle-bookmark(-other-window).
;;     icicle-prefix-completion-is-basic-flag: Make it a constant for pre-Emacs 23.
;; 2009/10/06 dadams
;;     icicle-keymaps-for-key-completion: Added bookmark-bmenu-mode-map to default value.
;;     icicle-sort-functions-alist: Use a separate defcustom for Emacs 20, since no :type alist.
;; 2009/09/25 dadams
;;     Added: icicle-prefix-completion-is-basic-flag.
;;     Changed default value of:
;;       icicle-candidate-width-factor          from 70  to 80
;;       icicle-Completions-text-scale-decrease from 0.8 to 0.66.
;; 2009/09/16 dadams
;;     icicle-top-level-key-bindings: Added remap for icicle-insert-buffer.
;; 2009/09/05 dadams
;;     icicle-keymaps-for-key-completion: Added facemenu-keymap.
;;     icicle-search-replace-common-match-flag: Fixed doc string: C-M-| -> M-;.
;;     icicle-expand-input-to-common-match-flag: Fixed doc string: C-| -> C-;.
;; 2009/09/02 dadams
;;     Added: icicle-region-bookmarks-flag, icicle-top-level-when-sole-completion-delay.
;;     icicle-region-alist: Updated doc string. Added :group Icicles-Searching.
;; 2009/08/19 dadams
;;     icicle-bookmark-name-length-max:
;;       Changed default value from 40 to 70.  It is now the total name length.  Updated doc.
;; 2009/08/01 dadams
;;     Added: icicle-menu-items-to-history-flag.
;; 2009/07/29 dadams
;;     Added: icicle-populate-interactive-history-flag.
;; 2009/06/17 dadams
;;     Added: icicle-Completions-text-scale-decrease.
;; 2009/05/20 dadams
;;     icicle-keymaps-for-key-completion: Added prefix-key maps senator-mode-map,
;;       srecode-mode-map, jde-mode-map, jde-jdb-mode-map.  Thx to Daniel Clemente.
;; 2009/05/17 dadams
;;     icicle-type-actions-alist: Updated to reflect thumb-frm.el name changes.
;; 2009/05/10 dadams
;;     icicle-list-join-string: Don't set display property for Emacs 21 (Emacs bug).
;; 2009/05/09 dadams
;;     Added: icicle-dot-string, icicle-dot-show-regexp-flag, icicle-anychar-regexp, 
;; 2009/05/07 dadams
;;     icicle-list-join-string: Use copy-sequence in default value, so users see original sexp.
;; 2009/05/02 dadams
;;     Added: icicle-alternative-actions-alist.
;; 2009/05/01 dadams
;;     Added function to icicle-type-actions-alist: icicle-shell-command-on-file.
;; 2009/04/26 dadams
;;     Added: icicle-type-actions-alist, icicle-use-anything-candidates-flag.
;; 2009/04/20 dadams
;;     Added: icicle-(prefix|apropos)-cycle-(next|previous)-alt-action-keys,
;;            icicle-modal-cycle-(down|up)-alt-action-keys.
;; 2009/04/18 dadams
;;     Soft-require hexrgb.el unconditionally, not just when there is a window-system.
;;     icicle-region-background: If frame background is unspecified, consider it white.
;;                               Don't use frame background unless it is a valid color name.
;;                               Use region background if cannot compute saturation.
;; 2009/04/16 dadams
;;     Added: icicle-(prefix|apropos)-cycle-(next|previous)-help-keys.
;; 2009/04/15 dadams
;;     Added: icicle-modal-cycle-(up|down)-help-keys.
;; 2009/04/05 dadams
;;     Added: icicle-help-in-mode-line-flag.
;; 2009/03/10 dadams
;;     Moved here from icicles-fn.el: icicle-shell-command-candidates.
;;     Moved here from icicles-var.el: icicle-shell-command-candidates.
;;     Renamed: icicle-shell-command-candidates (fn) to icicle-compute-shell-command-candidates,
;;              icicle-shell-command-candidates (var) to icicle-shell-command-candidates-cache,
;;              icicle-guess-cmds-in-path-flag to icicle-guess-commands-in-path.
;;     icicle-guess-commands-in-path: Boolean -> choice.  Default is now first-use.
;;     icicle-compute-shell-command-candidates: Don't update cache variable here.
;;     icicle-shell-command-candidates-cache: Initialize per icicle-guess-commands-in-path.
;; 2009/03/08 dadams
;;     Added: icicle-quote-shell-file-name-flag.
;; 2009/03/01 dadams
;;     Added: icicle-completing-read+insert-keys, icicle-read+insert-file-name-keys.
;;     Removed: icicle-complete-on-demand-keys.
;; 2009/02/20 dadams
;;     Added: icicle-complete-on-demand-keys (not yet used).
;;     Renamed: icicle-dired-guess-all-shell-cmds-flag to icicle-guess-cmds-in-path-flag.
;; 2009/01/25 dadams
;;     Added: icicle-dired-guess-all-shell-cmds-flag.
;; 2009/01/17 dadams
;;     Added: icicle-levenshtein-distance.
;;     icicle-apropos-match-fns-alist: Added icicle-levenshtein(-strict)-match.
;; 2009/01/05 dadams
;;     Added ess-complete-* to icicle-comint-dynamic-complete-replacements.
;; 2009/01/01 dadams
;;     Added: icicle-comint-dynamic-complete-replacements.
;; 2008/12/30 dadams
;;     Moved icicle-remap here from icicles-mode.el.
;; 2008/12/06 dadams
;;     icicle-incremental-completion-flag: Changed :type to choice from boolean.
;; 2008/12/02 dadams
;;     Removed: icicle-file-ignore-space-prefix-flag.
;; 2008/11/28 dadams
;;     Moved here from icicles-var.el: icicle-apropos-match-fns-alist.
;; 2008/11/14 dadams
;;     Added: icicle-hide-common-match-in-Completions-flag.
;; 2008/11/04 dadams
;;     Removed: icicle-generic-S-tab-keys.
;;     Added (split icicle-generic-S-tab-keys in 4):
;;       icicle-apropos-complete-keys, icicle-key-complete-keys, icicle-previous-candidate-keys,
;;       icicle-search-from-isearch-keys.
;;     Added: icicle-complete-key-anyway-flag.
;; 2008/11/03 dadams
;;     Applied renamings from icicles-cmd.el.
;; 2008/10/26 dadams
;;     Added: icicle-file-*.
;; 2008/10/18 dadams
;;     Added: icicle-customize-save-variable-function.
;; 2008/10/17 dadams
;;     Added: icicle-bookmark-name-length-max.
;;     icicle-top-level-key-bindings: Remapped bookmark-set to icicle-bookmark-cmd.
;; 2008/10/11 dadams
;;     icicle-highlight-input-completion-failure: Updated doc string.
;; 2008/10/10 dadams
;;     Added: icicle-(apropos|prefix)-cycle-(next|previous)-action-keys,
;;            icicle-modal-cycle-(down|up)-action-keys.
;;     icicle-cycling-respects-completion-mode-flag:
;;       Renamed to icicle-cycling-respects-completion-mode.
;;       It's now a choice of nil, t, apropos, prefix.  Thx to Andrey Zhdanov.
;; 2008/09/30 dadams
;;     icicle-highlight-input-completion-failure: Changed default value to implicit-strict.
;; 2008/09/13 dadams
;;     Added: icicle-filesets-as-saved-completion-sets-flag.
;; 2008/08/28 dadams
;;     icicle-top-level-key-bindings:
;;       Renamed alacarte-execute-menu-command to lacarte-execute-menu-command.
;; 2008/08/22 dadams
;;     Removed: icicle-Completions-window-default-width (obsolete).
;; 2008/08/21 dadams
;;     icicle-top-level-key-bindings:
;;       Replace icicle-find-file(-other-window) with icicle-file(-other-window).
;;     icicle-generic-S-tab-keys: Mention bug workaround in doc string.  Thx to Kevin Rodgers.
;; 2008/08/20 dadams
;;     Added: icicle-inhibit-ding-flag.
;; 2008/08/12 dadams
;;     icicle-top-level-key-bindings:
;;       Remap set-mark-command (C-SPC) and pop-global-mark (C-x C-SPC).
;; 2008/08/11 dadams
;;     icicle-key-definition, icicle-top-level-key-bindings:
;;       Change :match-alternatives to (symbolp) from (commandp).
;;     icicle-top-level-key-bindings: Added condition for minibuffer-keyboard-quit.
;; 2008/08/06 dadams
;;     Bind icicle-kmacro to S-f4, not f5.  Thx to Andrew Hyatt.
;; 2008/06/22 dadams
;;     Added: icicle-unpropertize-completion-result-flag.
;; 2008/06/03 dadams
;;     Added: icicle-use-C-for-actions-flag.
;; 2008/05/27 dadams
;;     icicle-isearch-complete-keys: Removed S-TAB, added M-o.
;; 2008/05/25 dadams
;;     icicle-isearch-complete-keys: Added M-TAB and C-M-TAB (if Windows).
;;     Change [] notation to `' notation for keys in doc strings.
;; 2008/05/24 dadams
;;     icicle-isearch-complete-keys: Use explicit ([S-tab] [S-iso-lefttab]) for Emacs < 22.
;; 2008/05/19 dadams
;;     icicle-generic-S-tab-keys: Use explicit ([S-tab] [S-iso-lefttab]) for Emacs < 22.
;; 2008/05/11 dadams
;;     icicle-top-level-key-bindings: Added :set and :initialize.
;;     Moved icicle-bind-top-level-commands here from icicles-mode.el, and added optional arg.
;; 2008/05/10 dadams
;;     Added widget icicle-key-definition (new custom type).
;;     Renamed: icicle-bind-top-level-commands-alist to icicle-top-level-key-bindings.
;;     icicle-top-level-key-bindings:
;;       Redefined:  New :type, using icicle-key-definition or choice of restricted-sexp.
;;         Changed order of entry elements.  Use kbd.  No eval for command to be remapped.
;;         Don't bind f5 unless have kmacro.
;;     Moved icicle-yank-function before icicle-top-level-key-bindings (needed by it).
;; 2008/05/07 dadams
;;     icicle-region-alist, icicle-sort-functions-alist:
;;       Removed Emacs 20 version (alist works for Emacs 20 also).
;;     Replaced icicle-bind-top-level-commands-flag with icicle-bind-top-level-commands-alist.
;;     icicle-isearch-complete-keys:
;;       Changed default value from ([S-tab] [S-iso-lefttab]) to ([backtab]).
;; 2008/05/05 dadams
;;     icicle-generic-S-tab-keys:
;;       Changed default value from ([S-tab] [S-iso-lefttab]) to ([backtab]).
;; 2008/04/26 dadams
;;     Added: icicle-test-for-remote-files-flag.  Updated C-^ to M-_
;;     icicle-ignore-space-prefix-flag: Update doc string: M-_, not C-^.
;; 2008/04/18 dadams
;;     Renamed icicle-init-value-flag to icicle-default-value.
;;     icicle-default-value: Updated to reflect t value.
;; 2008/04/13 dadams
;;     Added: icicle-pp-eval-expression-print-(length|level).
;; 2008/03/30 dadams
;;     Added: icicle-highlight-lighter-flag.
;; 2008/03/29 dadams
;;     Removed: icicle-completing(-mustmatch)-prompt-prefix, icicle-reminder-prompt-flag.
;;     Updated doc string of icicle-customize-save-flag.
;; 2008/03/21 dadams
;;     icicle-reminder-prompt-flag: Changed default value to nil.
;; 2008/02/23 dadams
;;     icicle-init-value-flag: Added insert-start value.  Renamed insert value to insert-end.
;; 2008/02/06 dadams
;;     Added: icicle-highlight-input-completion-failure-(delay|threshold).
;; 2008/01/29 dadams
;;     Removed: icicle-max-chars-noncompletion-highlight.
;;     Renamed: icicle-*-input-completion-failure-flag to icicle-*-input-completion-failure.
;;     icicle-*-input-completion-failure: Added values (implicit*, always) and renamed others.
;; 2007/12/31 dadams
;;     icicle-list-join-string: Add display property to hide the ^G.
;; 2007/12/26 dadams
;;     icicle-region-background:
;;       Put initialization back inside defcustom.
;;       Change test for color from Emacs version to widget test.
;; 2007/12/24 dadams
;;     Added: icicle-option-type-prefix-arg-list.
;; 2007/12/14 dadams
;;     Added: icicle-customize-save-flag.
;; 2007/12/09 dadams
;;     Added: icicle-max-chars-noncompletion-highlight.
;; 2007/12/03 dadams
;;     Renamed longest common match (lcm) to expanded common match (ecm).
;; 2007/11/25 dadams
;;     Added: icicle-command-abbrev-(alist|match-all-parts-flag|priority-flag).
;; 2007/11/23 dadams
;;     Added: icicle-(apropos|prefix)-cycle-(next|previous)-keys.
;; 2007/11/17 dadams
;;     Added: icicle-add-proxy-candidates-flag.
;; 2007/11/02 dadams
;;     Added: icicle-generic-S-tab-keys, icicle-prefix-complete-keys,
;;            icicle-(apropos|prefix)-complete-no-display-keys, icicle-isearch-complete-keys.
;;     Renamed icicle-modal-cycle-(up|down)-key to icicle-modal-cycle-(up|down)-keys,
;;             icicle-word-completion-key to icicle-word-completion-keys.
;; 2007/10/29 dadams
;;     icicle-define-alias-commands-flag, icicle-deletion-action-flag,
;;       icicle-list-nth-parts-join-string: Added type and group.
;; 2007/10/28 dadams
;;     Added: icicle-search-replace-common-match-flag.
;; 2007/10/23 dadams
;;     icicle-highlight-input-initial-whitespace-flag:
;;       Mention icicle-highlight-input-completion-failure-flag in doc string.
;; 2007/10/21 dadams
;;     icicle-candidate-width-factor, icicle-inter-candidates-min-spaces: Mention Do Re Mi.
;; 2007/10/14 dadams
;;     icicle-act-before-cycle-flag: Default value is now nil.
;; 2007/10/07 dadams
;;     Added: icicle-deletion-action-flag.
;; 2007/09/28 dadams
;;     Added: icicle-fuzzy-completion-flag.
;; 2007/08/25 dadams
;;     Added: icicle-anything-transform-candidates-flag, icicle-define-alias-commands-flag.
;; 2007/08/19 dadams
;;     Added: icicle-highlight-input-completion-failure-flag.
;; 2007/08/16 dadams
;;     icicle-region-alist, icicle-sort-functions-alist: Use alist as :type for recent Emacs.
;; 2007/07/27 dadams
;;     Added: Moved icicle-act-first-then-navigate-p here from icicles-var.el, as option *-flag.
;;            Thx to Juri Linkov for suggestion.
;; 2007/07/03 dadams
;;     Added: icicle-completion-history-max-length, icicle-C-l-uses-completion-flag.
;;     icicle-expand-input-to-common-match-flag: Updated doc string accordingly.
;; 2007/06/20 dadams
;;     icicle-WYSIWYG-Completions-flag: Use string value, not whole number, for sample text.
;; 2007/06/19 dadams
;;     icicle-WYSIWYG-Completions-flag: Allow also a whole-number value, for separate swatch.
;; 2007/06/17 dadams
;;     Added: icicle-WYSIWYG-Completions-flag.
;; 2007/06/12 dadams
;;     icicle-region-background: Use different value for dark-background frames.
;; 2007/06/09 dadams
;;     Added: icicle-use-candidates-only-once-flag.
;; 2007/06/05 dadams
;;     Don't require hexrgb.el if no window system.
;;     icicle-increment-color-*: Protected with featurep hexrgb and error message.
;; 2007/05/06 dadams
;;     Added: icicle-search-context-match-predicate.
;; 2007/05/02 dadams
;;     Added: icicle-search-whole-word-flag, icicle-yank-function.
;;     icicle-regexp-quote-flag: Updated doc string to mention search.
;; 2007/04/20 dadams
;;     Added: icicle-search-highlight-context-levels-flag, icicle-increment-color-saturation.
;; 2007/04/17 dadams
;;     Added: icicle-search-replace-literally-flag.
;; 2007/04/15 dadams
;;     icicle-search-replace-whole-candidate-flag: Changed default value to t.
;; 2007/04/07 dadams
;;     Added: icicle-search-replace-whole-candidate-flag.
;; 2007/03/31 dadams
;;     Added: icicle-top-level-when-sole-completion-flag.
;;     icicle(-regexp)-search-ring-max: Default is 1/10th what it was.
;; 2007/03/30 dadams
;;     Added: icicle-candidate-width-factor, icicle-inter-candidates-min-spaces.
;; 2007/03/12 dadams
;;     Added: icicle-Completions-window-max-height.
;;     icicle-show-Completions-help-flag: defvaralias it to completion-show-help (if bound).
;; 2007/03/10 dadams
;;     Added: icicle-Completions-window-default-width.
;; 2007/02/27 dadams
;;     Changed default value of icicle-incremental-completion-delay to 0.7.
;; 2007/02/22 dadams
;;     icicle-buffer-sort: Use icicle-buffer-sort-*...*-last, not nil, as default value.
;; 2007/02/20 dadams
;;     Changed icicle-region-auto-open-files-flag default value to nil.
;; 2007/02/19 dadams
;;     icicle-region-alist: Added buffer's file to alist entry.
;;     Added: icicle-region-auto-open-files-flag.
;; 2007/02/17 dadams
;;     Added: icicle-keymaps-for-key-completion.
;; 2007/02/06 dadams
;;     Added: icicle-add-buffer-name-flag.
;; 2007/01/29 dadams
;;     icicle-sort-function: Use icicle-case-string-less-p as value, not string-lessp.
;; 2007/01/23 dadams
;;     Added: icicle-highlight-historical-candidates-flag.
;;     Updated doc strings of *-flag to mention toggles.
;; 2007/01/18 dadams
;;     Renamed: icicle-regions to icicle-region-alist.
;; 2007/01/15 dadams
;;     Added: icicle-change-sort-order-completion-flag, icicle-sort-functions-alist.
;;     icicle-cycle-into-subdirs-flag, icicle-sort-function: Updated doc string.
;; 2007/01/14 dadams
;;     Added: icicle-list-nth-parts-join-string.
;; 2007/01/08 dadams
;;     icicle-reminder-prompt-flag: Reduced default value from 20 to 7 Emacs sessions.
;; 2007/01/06 dadams
;;     Added: icicle-use-~-for-home-dir-flag.  Thanks to Timothy Stotts for the suggestion.
;; 2006/12/29 dadams
;;     icicle-thing-at-point-functions: Added ffap-guesser as first alternative text grabber.
;;     icicle-default-thing-insertion: Changed default value to icicle-default-thing-insertion.
;; 2006/12/25 dadams
;;     Moved to icicles-fn.el: icicle-historical-alphabetic-p.
;; 2006/12/22 dadams
;;     Assigned Icicles subgroups, instead of group Icicles.
;; 2006/12/10 dadams
;;     icicle-regions: Corrected (forgot repeat).
;; 2006/11/26 dadams
;;     Added: icicle-regions(-name-length-max).
;; 2006/11/24 dadams
;;     Added: icicle-kmacro-ring-max.
;; 2006/11/23 dadams
;;     Added: icicle-TAB-shows-candidates-flag.  Thx to Tamas Patrovics for the suggestion.
;; 2006/11/09 dadams
;;     icicle-search-highlight-all-flag -> icicle-search-highlight-threshold.
;;     Added: icicle-search-highlight-all-current-flag.
;; 2006/10/28 dadams
;;     icicle-region-background: Changed :type to 'color for Emacs 21+.
;;     icicle(-alternative)-sort-function, icicle-buffer-sort, icicle-transform-function:
;;       function -> choice of nil or function.
;;     icicle-buffer-configs: Added :tag's.
;;     icicle-saved-completion-sets: Corrected doc string.
;; 2006/10/21 dadams
;;     Added: icicle-complete-keys-self-insert-flag.
;; 2006/10/14 dadams
;;     icicle-list-end-string: Added :type and :group.
;;     Moved conditional eval-when-compile to top level.
;; 2006/10/04 dadams
;;     Added: icicle-special-candidate-regexp.
;; 2006/10/03 dadams
;;     icicle-list-join-string: Replaced ^G^J by \007\012, to be able to upload to Emacs Wiki.
;; 2006/10/01 dadams
;;     icicle-alternative-sort-function: Updated doc string - it's now a general toggle.
;; 2006/09/30 dadams
;;     Added: icicle-key-descriptions-use-*-flag.
;; 2006/09/16 dadams
;;     Added: icicle-list-end-string.
;; 2006/09/03 dadams
;;     Renamed icicle-show-Completions-help to icicle-show-Completions-help-flag.
;; 2006/08/13 dadams
;;     Added: icicle-completing(-mustmatch)-prompt-prefix.
;; 2006/07/28 dadams
;;     icicle-change-region-background-flag:
;;       Default value takes no account of delete-selection mode.  Improved doc string.
;;     icicle-region-background:
;;       Don't make region invisible if hexrgb.el was not loaded.
;;       Change value, not hue, if grayscale frame background.  Improved doc string.
;; 2006/07/23 dadams
;;     Added: icicle-transform-function.
;;     icicle-sort-function: Added Note to doc string.
;; 2006/07/20 dadams
;;     Added: icicle-modal-cycle-(up|down)-key.
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-show-completions-help.  Renamed it to icicle-show-Completions-help.
;; 2006/07/18 dadams
;;     Added: icicle-Completions-display-min-input-chars.  Thx to Damien Elmes.
;; 2006/07/10 dadams
;;     icicle-historical-alphabetic-p: Fallback directory if no previous input.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort-function, icicle-historical-alphabetic-p.
;; 2006/07/04 dadams
;;     icicle-expand-input-to-common-match-flag: Updated doc string.
;; 2006/06/09 dadams
;;     icicle-region-background: Use nil in defcustom.  Initialize separately.
;; 2006/06/08 dadams
;;     icicle-bind-top-level-commands-flag: Updated doc string.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;       Changed its functionality to use a countdown.
;; 2006/05/16 dadams
;;     Added: icicle-bind-top-level-commands-flag.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     Added: icicle-buffer-ignore-space-prefix-flag.
;;     icicle-ignore-space-prefix-flag: Changed default value to nil.
;; 2006/05/09 dadams
;;     icicle-incremental-completion-threshold: Updated doc string (msg "Displaying...").
;; 2006/04/28 dadams
;;     Added: icicle-highlight-input-initial-whitespace-flag.
;; 2006/04/14 dadams
;;     Added: icicle-input-string, icicle-search-cleanup-flag, icicle-update-input-hook.
;;     icicle-list-join-string: Added :type and :group.
;; 2006/04/09 dadams
;;     Added: icicle-arrows-respect-completion-type-flag.
;; 2006/04/07 dadams
;;     Added: icicle-search-highlight-all-flag.
;; 2006/04/02 dadams
;;     Added: icicle-regexp-quote-flag.
;; 2006/03/24 dadams
;;     Added: icicle-incremental-completion-(delay|threshold).
;; 2006/03/20 dadams
;;     icicle-expand-input-to-common-match-flag: Changed default value to t.
;; 2006/03/19 dadams
;;     Added: icicle-expand-input-to-common-match-flag.
;; 2006/03/17 dadams
;;     Removed: icicle-cache-file.
;;     Added: icicle-saved-completion-sets.
;; 2006/03/13 dadams
;;     Added: icicle-cache-file.
;; 2006/03/08 dadams
;;     icicle-default-thing-insertion: Use substitute-command-keys in :tag.
;; 2006/03/05 dadams
;;     Moved from here to icicle-mode.el: icicle-mode, icicle-mode-hook.
;;     Added: icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     icicle-list-join-string: Changed value to ^G^J.  Clarified doc string.
 
;;;(@* "CHANGE LOG FOR `icicles-var.el'")
;;
;; 2010/06/18 dadams
;;     Added: icicle-completions-format-internal.
;; 2010/06/08 dadams
;;     Added: icicle-nb-candidates-before-truncation.
;; 2010/06/04 dadams
;;     Added: icicle-lighter-truncation.
;;     icicle-general-help-string: Mention missing doremi commands.
;; 2010/04/29 dadams
;;     Added: icicle-remove-icicles-props-p.
;; 2010/04/02 dadams
;;     icicle-general-help-string: Updated.
;; 2010/04/02 dadams
;;     Added: icicle-bookmark-types.
;;     icicle-general-help-string: Updated to reflect move from saved regions to bookmarks.
;; 2010/03/13 dadams
;;     Added: icicle-transform-before-sort-p.
;;     Removed: icicle-sorted-bookmark-alist.
;;     Applied renaming of icicle-add-buffer-name-flag to icicle-show-multi-completion-flag.
;; 2010/03/03 dadams
;;     Added: icicle-sorted-bookmark-alist, icicle-reverse-multi-sort-p.
;;     Renamed: icicle-last-sort-function to icicle-last-sort-comparer.
;;     icicle-general-help-string: Applied renaming to icicle-sort-orders-alist.
;;     Applied renamings: icicle-sort-function to icicle-sort-comparer,
;;                        icicle-sort-functions-alist to icicle-sort-orders-alist,
;;                        icicle-alternative-sort-function to icicle-alternative-sort-comparer.
;; 2010/02/17 dadams
;;     Moved icicle-redefined-functions to icicles-opt.el
;;       and renamed to icicle-inhibit-advice-functions
;; 2010/02/14 dadams
;;     Added: icicle-advice-info-list, icicle-redefined-functions.
;; 2009/11/07 dadams
;;     Applied doremi cmd renamings (added +) to help text.
;; 2009/10/25 dadams
;;     Added: icicle-current-TAB-method.
;;     Updated icicle-general-help-string with new command names.
;; 2009/09/26 dadams
;;     Added: icicle-progressive-completing-p.
;; 2009/09/25 dadams
;;     Removed: icicle-respect-completion-styles-p.  See option *-TAB-respects-*-styles-flag.
;; 2009/09/05 dadams
;;     Added: icicle-minibuffer-message-ok-p.
;;     Renamed icicle-acting-on-next/prev-p to icicle-acting-on-next/prev.
;; 2009/07/26 dadams
;;     Added: icicle-command-abbrev-history (belated), icicle-interactive-history.
;; 2009/05/17 dadams
;;     icicle-predicate-types-alist: Updated to reflect thumb-frm.el name changes.
;; 2009/05/11 dadams
;;     Added: icicle-hist-cands-no-highlight.
;; 2009/05/09 dadams
;;     Added: *-input-completion-fail-overlay, *-anychar-regexp, *-dot-string-internal.
;; 2009/05/02 dadams
;;     Added: icicle-cmd-reading-input.
;; 2009/04/28 dadams
;;     Renamed: icicle-object-predicate-types to icicle-predicate-types-alist.
;;     icicle-predicate-types-alist:
;;       Converted to alist, with cdr's from icicle-type-actions-alist.  Added more entries.
;;     Removed: icicle-object-named-types.
;; 2009/04/03 dadams
;;     Added: icicle-filtered-default-value.
;; 2009-03/16 dadams
;;     Added: icicle-use-candidates-only-once-alt-p.
;; 2009/03/15 dadams
;;     icicle-general-help-string: Added: icicle-recompute-shell-command-candidates,
;;                                        icicle-remove-file-from-recentf-list.
;; 2009/03/10 dadams
;;     Moved icicle-shell-command-candidates to icicles-opt.el and renamed: *-cache.
;; 2009/03/01 dadams
;;     Added: icicle-completing-read+insert-candidates.
;;     Removed: icicle-complete-on-demand-cmd.
;; 2009/02/23 dadams
;;     Added: icicle-extra-candidates-dir-insert-p.
;; 2009/02/20 dadams
;;     Added: icicle-shell-command-candidates, icicle-complete-on-demand-cmd.
;; 2009/02/04 dadams
;;     Added: icicle-next-prefix-complete-cycles-p, icicle-next-apropos-complete-cycles-p.
;; 2009/01/24 dadams
;;     Added: icicle-last-apropos-complete-match-fn.
;; 2009/01/13 dadams
;;     Added: icicle-respect-completion-styles-p (internal var, for now).
;; 2008/12/25 dadams
;;     Added: icicle-cycling-p.
;; 2008/12/07 dadams
;;     Added: icicle-completion-prompt-overlay.  Removed: icicle-prompt.
;; 2008/12/02 dadams
;;     Added: icicle-confirm-exit-commands.
;; 2008/11/28 dadams
;;     Moved to icicles-opt.el: icicle-apropos-match-fns-alist.
;; 2008/11/22 dadams
;;     Added: icicle-fancy-candidates-p, icicle-fancy-cands-internal-p.
;; 2008/11/03 dadams
;;     Applied renamings from icicles-cmd.el.
;; 2008/11/02 dadams
;;     Added: icicle-(doc|vardoc|fundoc|plist)-last-initial(-option)-cand-set.
;; 2008/10/14 dadams
;;     Added: icicle-general-help-string.
;;     Removed: icicle-completion-help-string.
;; 2008/10/06 dadams
;;     icicle-ms-windows-drive-hash: Set it to nil if make-hash-table is undefined.
;; 2008/09/30 dadams
;;     Added: icicle-ms-windows-drive-hash.
;; 2008/09/20 dadams
;;     icicle-ignored-extensions-regexp: Append $ to each extension.
;;     Added: icicle-abs-file-candidates.
;; 2008/08/28 dadams
;;     Renamed: alacarte-menu-items-alist to lacarte-menu-items-alist.
;; 2008/08/18 dadams
;;     Added: icicle-inhibit-try-switch-buffer.
;; 2008/08/12 dadams
;;     Added: icicle-apply-nomsg.
;; 2008/08/10 dadams
;;     Added: icicle-explore-final-choice, icicle-explore-final-choice-full.
;; 2008/08/03 dadams
;;     Added: icicle-all-candidates-list-alt-action-fn.
;;     Renamed: icicle-all-candidates-action-fn to icicle-all-candidates-list-action-fn,
;;              icicle-candidate-alternative-action-fn to icicle-candidate-alt-action-fn.
;; 2008/03/30 dadams
;;     Added: icicle-old-read-file-name-fn.
;; 2008/03/29 dadams
;;     Removed: icicle-prompt-suffix.
;; 2008/03/23 dadams
;;     Added: icicle-scroll-Completions-backward-p.
;; 2008/03/19 dadams
;;     Added: icicle-read-expression-map.
;; 2008/03/10 dadams
;;     Added: icicle-frame-alist.
;; 2008/02/24 dadams
;;     Added: icicle-apropos-match-fns-alist.
;; 2008/02/21 dadams
;;     Added: icicle-Info-only-rest-of-book-p.
;; 2008/02/03 dadams
;;     Added: icicle-comp-base-is-default-dir-p, icicle-dir-candidate-can-exit-p.
;; 2008/01/18 dadams
;;     Moved icicle-complete-keys-alist here from icicles-cmd.el
;; 2008/01/04 dadams
;;     Added: icicle-doc-history.
;; 2007/12/27 dadams
;;     Added: icicle-apropos-complete-match-fn.
;; 2007/12/05 dadams
;;     icicle-proxy-candidate-regexp: Removed * doc-string prefix.
;; 2007/11/25 dadams
;;     Added: icicle-commands-for-abbrev.
;; 2007/11/17 dadams
;;     Added: icicle(saved)-proxy-candidates, icicle-proxy-candidate-regexp.
;; 2007/10/06 dadams
;;     icicle-object-named-types: Added file type.
;; 2007/08/19 dadams
;;     Added: icicle-input-fail-pos.
;; 2007/08/18 dadams
;;     Added: icicle-whole-candidate-as-text-prop-p.
;; 2007/07/29 dadams
;;     Added: icicle-object-named-types, icicle-object-predicate-types.
;; 2007/07/27 dadams
;;     Moved icicle-act-first-then-navigate-p to icicles-opt.el as icicle-act-before-cycle-flag.
;; 2007/07/08 dadams
;;     Added: icicle-all-candidates(-alternative)-action-fn.
;; 2007/07/03 dadams
;;     Added: icicle-previous-raw(-non)-file-name-inputs.
;; 2007/06/23 dadams
;;     Added: icicle-search-replacement-history.
;; 2007/06/17 dadams
;;     Added: icicle-saved-candidate-overlays.
;; 2007/06/07 dadams
;;     Added: icicle-face-name-history.
;;     Renamed: frame-name-history to icicle-frame-name-history,
;;              icicle-font-history to icicle-font-name-history,
;;              icicle-function-history to icicle-function-name-history,
;;              icicle-variable-history to icicle-variable-name-history.
;; 2007/05/29 dadams
;;     icicle-insert-string-at-pt-*: Initialize to nil, not 0.
;; 2007/05/25 dadams
;;     Added: icicle-char-property-value-history.
;; 2007/05/06 dadams
;;     Added defvars to quiet byte compiler.
;; 2007/04/28 dadams
;;     Added: icicle-search-in-context-fn.
;; 2007/04/20 dadams
;;     Added: icicle-search-level-overlays.
;; 2007/04/15 dadams
;;     Added: icicle-search-context-regexp.
;; 2007/04/10 dadams
;;     Added: icicle-search-context-level.
;; 2007/04/08 dadams
;;     Added: icicle-all-candidates-action-p.
;;     icicle-candidate-action-fn: Corrected doc string: reversed success and failure values.
;; 2007/04/07 dadams
;;     Added: icicle-search-replacement, icicle-searching-p, icicle-act-first-then-navigate-p.
;; 2007/04/02 dadams
;;     Added: icicle-text-property-value-history.
;;     Added: icicle-text-properties-alist (commented out).
;; 2007/03/23 dadams
;;     Added: icicle-require-match-p.
;; 2007/03/14 dadams
;;     Added: icicle-last-top-level-command.
;; 2007/03/06 dadams
;;     Added: icicle-inhibit-sort-p.
;;     icicle-candidates-alist: Improved doc string.
;; 2007/02/20 dadams
;;     Added: icicle-delete-candidate-object, icicle-candidate-alternative-action-fn.
;; 2007/02/03 dadams
;;     Renamed icicle-icompleting-p to icicle-edit-update-p.
;; 2007/02/02 dadams
;;     Added: icicle-completing-p.
;; 2007/01/29 dadams
;;     icicle-last-sort-function: Use icicle-case-string-less-p, not string-lessp.
;; 2007/01/19 dadams
;;     Added: icicle-candidate-properties-alist.
;; 2007/01/15 dadams
;;     Added: icicle-reverse-sort-p.
;; 2007/01/14 dadams
;;     icicle-list-use-nth-parts: Updated doc string for new icicle-list-nth-parts-join-string.
;; 2007/01/12 dadams
;;     Added: icicle-list-use-nth-parts.
;;     Removed icicle-saved-overriding-local-map.
;; 2007/01/11 dadams
;;     Added: icicle-menu-map, icicle-minor-mode-map-entry.
;; 2007/01/10 dadams
;;     Added: icicle-saved-overriding-local-map.
;; 2007/01/05 dadams
;;     icicle-initial-value: Updated doc string to mention you can bind it.
;; 2006/12/25 dadams
;;     Added: icicle-saved-completion-candidates-internal.
;; 2006/12/23 dadams
;;     Added: icicle-candidate-help-fn.
;; 2006/12/17 dadams
;;     Added: icicle-saved-completion-candidate.
;; 2006/11/24 dadams
;;     Added: icicle-universal-argument-map, icicle-kmacro-alist, icicle-saved-kmacro-ring-max,
;;            icicle-kmacro-history.
;; 2006/11/18 dadams
;;     Added: frame-name-history, icicle-bookmark-history, icicle-buffer-config-history,
;;            icicle-color-history, icicle-color-theme-history, icicle-completion-set-history,
;;            icicle-dictionary-history, icicle-font-history, icicle-function-history,
;;            icicle-kill-history, icicle-search-history, icicle-variable-history,
;; 2006/11/09 dadams
;;     icicle-search-refined-overlays: Updated doc string: icicle-search-highlight-threshold.
;; 2006/10/14 dadams
;;     Moved conditional eval-when-compile to top level.
;; 2006/09/24 dadams
;;     icicle-last-transform-function: Corrected default value.
;; 2006/09/12 dadams
;;     Added: icicle-pre-minibuffer-buffer.
;; 2006/08/20 dadams
;;     icicle-current-completion-mode: Updated doc string.
;; 2006/08/04 dadams
;;     Removed icicle-apropos-completing-p (not used).
;; 2006/07/23 dadams
;;     Added: icicle-last-transform-function.
;; 2006/07/22 dadams
;;     Added: icicle-search-command, icicle-search-final-choice.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>: Added: icicle-current-completion-type.
;;     Renamed: icicle-current-completion-type to icicle-current-completion-mode.
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;; 2006/06/18 dadams
;;     Added: icicle-apropos-completing-p.
;; 2006/04/30 dadams
;;     Added: icicle-candidate-entry-fn.
;;     Renamed: icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/14 dadams
;;     Renamed icicle-search-refined-overlay to icicle-search-refined-overlays.
;;     Added: icicle-search-candidates.
;; 2006/04/07 dadams
;;     Added: icicle-search-overlays.
;;     Renamed icicle-search-overlay to icicle-search-current-overlay.
;; 2006/03/27 dadams
;;     Added: icicle-search-refined-overlay.
;; 2006/03/26 dadams
;;     Added: icicle-search-overlay.
;; 2006/03/25 dadams
;;     Added: icicle-saved-candidates-variables-obarray.
;; 2006/03/20 dadams
;;     Added: icicle-common-match-string, icicle-current-regexp-input.
;; 2006/03/14 dadams
;;     Removed: icicle-icicle-completing-p.
;; 2006/03/13 dadams
;;     Added: icicle-re-no-dot.
;; 2006/03/05 dadams
;;     Moved to icicles-mode.el: icicle-mode-map.
;; 2006/03/04 dadams
;;     Moved options stuff to Options menu, when available.
;;     Moved apropos stuff to Apropos menu, when available.
;;     Moved describe stuff to Describe menu, when available.
;; 2006/03/03 dadams
;;     Added to Icicles menu: icicle-complete-thesaurus-entry, icicle-apropos*,
;;       option-setting cmds, buffer-config cmds icicle-(var|fun)doc.
;;     Require apropos-fn+var.el.
 
;;;(@* "CHANGE LOG FOR `icicles.el'")
;;
;; 2009/05/22 dadams
;;     Require icicles-cmd[12].
;; 2009/04/13 dadams
;;     Removed the part of Thierry's text that mentioned emacsclient (no relation to Icicles).
;; 2008/12/02 dadams
;;     Don't warn, if daemonp.  Thx to Thierry Volpiatto.
;; 2007/07/22 dadams
;;     Require icicles-cmd.el before icicles-mode.el.
;; 2007/06/07 dadams
;;     Moved all doc to new files icicles-doc1.el and icicles-doc2.el.
;; 2007/05/12 dadams
;;     Moved Search Enhancements subsections to top level: Isearch Completion,
;;       Icicles Search Commands, Overview, Search and Replace.
;; 2007/05/06 dadams
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/04/20 dadams
;;     Require icicles-face.el after icicles-opt.el.
;; 2007/03/09 dadams
;;     Renamed sections .*Removal of Duplicates to .*Removing Duplicates and
;;                      More on Multi-Commands to More About Multi-Commands.
;; 2007/02/24 dadams
;;     Added section More on Multi-Commands.
;;     Added subsection Chipping Away the Non-Elephant to Nutshell View.
;; 2007/02/03 dadams
;;     Updated section Sorting Candidates and Removal of Duplicates.
;; 2007/01/28 dadams
;;     Added: subsection Using Progressive ... Process of Elimination.
;; 2007/01/21 dadams
;;     Added: section Text Properties in *Completions*.
;; 2007/01/19 dadams
;;     Added: section Programming Multi-Completions.
;; 2007/01/16 dadams
;;     Added linkd links.  Cleanup.
;; 2007/01/15 dadams
;;     Added: section Sorting Candidates and Removal of Duplicates.
;;     Renamed:
;;      icicle-sort-and-strip-ignored to icicle-strip-ignored-files-and-sort,
;;      icicle-dirs-last-p to icicle-dirs-last-p,
;;      icicle-sort-case-insensitively to *-case-insensitive-string-lessp.
;; 2007/01/12 dadams
;;     Updated section Multi-Completions for icicle-list-use-nth-parts.
;; 2007/01/06 dadams
;;     File-Name and Directory-Name Completion Tips: Mention icicle-use-~-for-home-dir-flag.
;; 2006/11/23 dadams
;;     Added icicle-TAB-shows-candidates-flag.
;; 2006/11/10 dadams
;;     Multi-Commands: Mention prompt prefix +.
;; 2006/11/05 dadams
;;     icicle-occur is bound to C-c '.  Search commands use multiple buffers.
;;     Added Nutshell subsection Perform Multiple Operations In One Command.
;; 2006/10/19 dadams
;;     Added Goggle Matching section.
;; 2006/10/16 dadams
;;     Added key completion to Nutshell View.
;; 2006/10/14 dadams
;;     Renamed: icicle-cancel-*Help*-* to icicle-cancel-Help-*.
;;     Moved conditional eval-when-compile to top level.
;; 2006/10/01 dadams
;;     Updated for new alternative-sort toggle:
;;       History Enhancements, Key Completion, Customization *, Key Bindings.
;; 2006/09/30 dadams
;;     Changed bindings of icicle-candidate-set-(save|retrieve) from C-<, C-> to C-M-<, C-M->.
;;     Added icicle-key-descriptions-use-<>-flag in Customization section.
;; 2006/09/17 dadams
;;     Added section Key Completion.
;; 2006/09/12 dadams
;;     Added section Moving Between the Minibuffer and Other Buffers.
;; 2006/08/23 dadams
;;     Added sections Icicles Mult M-x and Defining Icicles Multi M-x.
;; 2006/08/18 dadams
;;     Added section Icicles Info Enhancements.
;; 2006/08/13 dadams
;;     Documented icicle-completing(-mustmatch)-prompt-prefix.
;; 2006/06/17 dadams
;;     Rewrote Multi-Commands, Defining Icicles Commands (Including Multi-Commands), and
;;             Defining Multi-Commands the Hard Way.
;;     Renamed: Defining Icicles Commands: + (Including Multi-Commands).
;;              Defining Multi-Commands: + the Hard Way.
;;     Added: Defining Multiple-Choice Menus.
;; 2006/06/08 dadams
;;     Removed require of icicle-keys.el (obsolete).
;; 2006/05/26 dadams
;;     Mention M-k as icicle-erase-minibuffer-or-history-element.
;;     Don't mention M-S-backspace and M-S-delete any more.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;       Updated its doc to reflect new functionality.
;; 2006/05/18 dadams
;;     Change install instructions to include turning on Icicle mode.
;; 2006/05/16 dadams
;;     Require icicles-keys.el when icicle-bind-top-level-commands-flag.
;;     Updated doc to reflect new library icicles-keys.el.
;; 2006/05/15 dadams
;;     Renamed: ici*-nospace-flag to icicle-ignore-space-prefix-flag.
;;     Updated doc of icicle-ignore-space-prefix-flag.
;;     Added doc of icicle-buffer-ignore-space-prefix-flag.
;; 2006/04/14 dadams
;;     Added section Inserting a Regexp from a Variable.
;; 2006/04/09 dadams
;;     Added descriptions of icicle-arrows-respect-completion-type-flag.
;; 2006/03/19 dadams
;;     Added description of icicle-expand-input-to-common-match-flag.
;; 2006/03/07 dadams
;;     Correct the description of icicle-doc - match against only the doc, not the symbol name.
;; 2006/03/06 dadams
;;     Reordered Commentary sections, putting Emacs-Lisp stuff later.
;; 2006/03/05 dadams
;;     Mention icicle-touche-pas-aux-menus-flag.
;; 2006/03/03 dadams
;;     Clarified Multi-Completions description.
;; 2006/03/01 dadams
;;     Added: icicle-(complete|insert)-thesaurus-entry.
;; 2006/02/27 dadams
;;     Split into multiple libraries: *-cmd, *-face, *-fn, *-mac, *-mode, *-opt, *-var.
;; 2006/02/25 dadams
;;     Added: icicle-narrow-candidates (bound to M-*), icicle-icicle-completing-p,
;;            icicle-set-calling-cmd, icicle-reset-icicle-completing-p,
;;            icicle-run-icicle-(pre|post)-command-hook.
;;     Add all hooks in icicle-mode only, except for minibuffer-local hooks (pre- and
;;       post-command).
;;     Remove all hooks when exit Icicle mode.
;;     icicle-completing-read, icicle-read-file-name:
;;       Add catch icicle-read-top.  Set icicle-icicle-completing-p.
;;       Separate case of not Icicle mode from other no-prompt cases.
;;     Reordered some groups of functions.
;; 2006/02/24 dadams
;;     icicle-candidate-set-1: Treat empty set.
;; 2006/02/21 dadams
;;     icicle-prefix-complete: Implemented icompletion here, like icicle-apropos-complete-1.
;;     icicle-call-then-update-Completions:
;;       Use icicle-last-completion-command, not icicle-apropos-complete.
;;     Renamed icicle-apropos-icompleting-p to icicle-icompleting-p.
;;     Added: icicle-(kill|delete)(-backward)-*, icicle-yank etc.  Bound them.
;;     Added: icicle-call-then-update-Completions.
;;     Added: icicle-incremental-completion-p.
;;       Use instead of icicle-incremental-completion-flag everywhere.
;;       Upgrade from t in icicle-display-candidates-in-Completions.
;;       Reset in icicle-minibuffer-setup.
;;     icicle-isearch-complete: Use search-ring symbol as history arg to completing-read.
;;     icicle-display-candidates-in-Completions, icicle-keep-only-past-inputs, icicle-history:
;;       Ensure that minibuffer-history-variable is a list.
;;     Fixed typos: icicle-keep-past-inputs -> icicle-keep-only-past-inputs.
;; 2006/02/20 dadams
;;     icicle-insert-string-at-point: Treat negative prefix arg.
;;     Added: icicle-signum.
;;     icicle-insert-thing: Remove text properties of string to insert.
;; 2006/02/19 dadams
;;     icicle-thing-at-point-functions: Added function to grab successive text.
;;     icicle-insert-string-at-point: Treat successive-grab fn and prefix arg.
;;     Added: icicle-default-thing-insertion, icicle-default-thing-insertion-flipped-p,
;;            icicle-insert-string-at-pt-(start|end), icicle-successive-grab-count,
;;            icicle-insert-thing.
;;     Renamed: icicle-insert-string-near-point to icicle-insert-string-at-point.
;; 2006/02/18 dadams
;;     icicle-retrieve-last-input: Don't reset icicle-last-completion-command if not interactive
;;     icicle-candidate-set-complement, icicle-keep-only-past-inputs:
;;       Use icicle-retrieve-last-input.
;;     icicle-keep-only-past-inputs:
;;       Rewrote modeled on icicle-apropos-complete:
;;        Take into account singleton and empty candidate set.
;;        Provide input to icicle-display-ca*.
;;        Set icicle-last-completion-command.
;;     icicle-history: Force redisplay of *Completions*.  Don't set this-command.
;;     icicle-completing-read: Ensure icicle-initial-value is not nil.
;;     icicle-save-or-restore-input: Don't restore empty input.
;;     icicle-recompute-candidates:
;;       Don't recompute if last completion cmd was icicle-keep-only-past-inputs.
;;     Added: icicle-historical-candidate, icicle-keep-only-past-inputs.
;;     icicle-display-candidates-in-Completions: Use icicle-historical-candidate.
;;     Bind icicle-keep-only-past-inputs to M-pause in minibuffer completion maps.
;; 2006/02/17 dadams
;;     Added: icicle-complete-input-overlay, icicle-highlight-complete-input,
;;            icicle-complete-input.
;;     icicle-(prefix|apropos)-complete(-1): Use icicle-highlight-complete-input.
;;     Added icicle-inhibit-reminder-prompt-flag.  Thx to Jonathan Simms for the suggestion.
;;     icicle-completing-read, icicle-read-file-name: Use icicle-inhibit-reminder-prompt-flag.
;; 2006/02/12 dadams
;;     icicle-read-string: Finished bug fix of 2/11.  Thx to Andrey Zhdanov.
;; 2006/02/11 dadams
;;     icicle-insert-string-near-point: Always start with first function.
;;     read-from-minibuffer: Bug fix: don't use def if init is consp.  Thx to Andrey Zhdanov.
;; 2006/02/09 dadams
;;     Added: icicle-insert-string-near-point, icicle-thing-at-point-functions,
;;            icicle-thing-at-pt-fns-pointer.
;;     Bound icicle-insert-string-near-point.
;;     Added Commentary section "Inserting Text Found Near the Cursor"
;;     Require: thingatpt+.el, thingatpt.el.
;;     Bug fix: icicle-execute-extended-command(-1): Take care of last-command and this-command.
;; 2006/02/08 dadams
;;     icicle-completing-read: Treat consp case of initial-input.
;;     icicle-read-file-name: Fixed bug introduced 02/02:
;;       Don't ensure initial-input is not null.
;; 2006/02/07 dadams
;;     Bug fix: Files menu find-file stuff was bound to *recent-file*.
;; 2006/02/03 dadams
;;     icicle-init-value-flag: Use nil as the default value.
;;     Added: icicle-read-from-minibuffer, icicle-read-string.
;;              Use in icicle-(redefine|restore)-standard-commands.
;; 2006/02/02 dadams
;;     icicle-completing-read, read-file-name:
;;       Respect icicle-init-value-flag only if default value not nil.
;;     read-file-name: Ensure initial-value is not null.  Initialize icicle-initial-value.
;;                     Respect icicle-init-value-flag.
;; 2006/01/29 dadams
;;     icicle-completing-read, icicle-read-file-name: Remove binding of ESC-TAB.
;;     icicle-lisp-complete-symbol: Enable recursive minibuffers if in minibuffer.
;;     Commentary: Combine lisp-complete-symbol with dabbrev.
;;     Updated bindings listed in icicle-completion-help-string.
;; 2006/01/28 dadams
;;     New feature: icicle-lisp-complete-symbol (added).  Added to Commentary and moved section.
;;     Corrected fix of 2005/12/14:
;;       icicle-minibuffer-setup: Save region background at recursion level 1.
;;       icicle-saved-region-background: defvar to nil.
;;     Added: icicle-increment-color-hue.  Use in icicle-region-background.
;;     Added: icicle-(re)set-option-to-(nil|t), icicle-clear-option, icicle-toggle-option,
;;            icicle-binary-option-p.
;; 2006/01/26 dadams
;;     Added: icicle(-saved)(-regexp)-search-ring-max,
;;            icicle-(redefine|restore)-standard-options.
;;     icicle-mode: Use icicle-(redefine|restore)-standard-options.
;;                  Use icicle-(redefine|restore)-standard-commands for Emacs 21+ also (forgot?)
;;     icicle-(redefine|restore)-*: Use defalias, not fset.
;; 2006/01/24 dadams
;;     New feature: icicle-isearch-complete.
;;       Added: icicle-isearch-complete, icicle-isearch-resume, icicle-bind-isearch-keys.
;;       icicle-mode: add/remove isearch-mode-hook.
;;     Minor bug fix: initial value was treated as icicle-last-completion-candidate.
;;       Added: icicle-initial-value.
;;       icicle-completing-read, icicle-read-file-name:
;;         Set icicle-initial-value, not icicle-last-completion-candidate.
;;       icicle-next-candidate:
;;         Initialize icicle-last-completion-candidate to icicle-initial-value.
;;       icicle-save-or-restore-input:
;;         Don't change icicle-current-input if = icicle-initial-value
;;       Renamed: icicle-init-value to icicle-init-value-flag.
;; 2006/01/23 dadams
;;     Use command remapping for self-insert-command in Emacs 22.
;;     Changed icicle-(re|un)map to defsubst.
;;     Removed Commentary section on icicle-execute-extended-command.
;;     icicle-apropos-complete-and-exit, icicle-apropos-complete-1:
;;       Use flag icicle-apropos-complete-and-exit-p to suppress minibuffer-message.
;; 2006/01/22 dadams
;;     Added: icicle-execute-extended-command*.
;;     completing-read, icicle-read-file-name:
;;       Corrected nil case for icicle-require-match-flag (bug fix).
;;       Hard-code bindings, instead of using \\[...], so the simpler bindings are shown.
;;     Changed C-o to C-RET for consistency (C-o still works too).
;;       icicle-(bind|restore)-completion-keys: Added C-RET binding.
;; 2006/01/21 dadams
;;     icicle-mouse-choose-completion:
;;       Don't save selected window if it's *Completions*.
;;     Added more Commentary about icicle-retrieve-last-input.
;; 2006/01/20 dadams
;;     icicle-sort-and-strip-ignored: Don't ignore names if only ignored extensions match.
;;     Added: icicle-apropos-complete-and-exit.  Bound it in icicle-rebind-completion-maps.
;;     icicle-minibuffer-setup: Don't reset icicle-require-match-flag.
;;     icicle-apropos-complete: Return the list of candidates.
;; 2006/01/19 dadams
;;     Added: icicle(-buffer)-require-match-flag.  Thanks to Mathias Dahl for feedback.
;;            Use in completing-read, read-file-name, and icicle-minibuffer-setup.
;;     Re-alphabetized defcustoms.
;; 2006/01/07 dadams
;;     Added :link.
;; 2005/12/31 dadams
;;     Added: icicle-fix-default-directory.
;;     icicle-read-file-name: Use icicle-fix-default-directory hack to fix bug.
;; 2005/12/26 dadams
;;     Added icicle-sort-case-insensitively.
;;     Added more parent groups for icicles group.
;; 2005/12/14 dadams
;;     icicle-minibuffer-setup: Only save region background when at top level.
;;     Added: icicle-Completions-frame-at-right-flag.  Use in icicle-candidate-action.
;;     Added: defvars for font-lock-keyword-face, font-lock-function-name-face.
;; 2005/12/09 dadams
;;     Fontify icicle-define* in emacs-lisp-mode.
;; 2005/12/02 dadams
;;     Added: icicle-customize-apropos*.  Use in icicle-(redefine|restore)-standard-commands.
;; 2005/12/01 dadams
;;     Added: icicle-repeat-complex-command, icicle-redefine-standard-commands-flag,
;;            icicle-(redefine|restore)-standard-commands.
;; 2005/11/30 dadams
;;     Added: icicle-apropos-zippy.
;;     icicle-apropos-command, icicle-apropos-variable:
;;       Corrected completing-read for do-all arg.
;;     icicle-apropos-command, *-apropos-option: My version must not respect apropos-do-all.
;; 2005/11/29 dadams
;;     Added: icicle-apropos*.
;;     icicle-help-on-candidate: Treat plists.  Message "No help" is the default.
;; 2005/11/25 dadams
;;     Added: icicle-dabbrev-completion.
;;     Renamed names with "*Completions*" to use "Completions", for coherence with XEmacs port.
;; 2005/11/24 dadams
;;     icicle-mouse-choose-completion: Delete *Completions* window systematically.
;; 2005/11/21 dadams
;;     icicle-delete-windows-on: Avoid error Attempt to delete minibuffer or sole ... window.
;;     icicle-prefix-complete, icicle-apropos-complete-1, icicle-next-candidate:
;;       Use icicle-delete-windows-on, not delete-window.
;;     icicle-candidate-set-save: Use map in doc string.
;;     icicle-compilation-search: Tidied up doc string.
;;     Use #' for clarity.
;; 2005/11/20 dadams
;;     icicle-completing-read: Added treatment of completions that are lists of strings.
;;     Updated Commentary: new section on completions that are lists.
;;     Added: icicle-list-join-string, icicle-doc, icicle-fundoc, icicle-vardoc.
;; 2005/11/15 dadams
;;     Temporarily removed defadvice of next-history-element for Emacs 22.  Bug reported.
;;     icicle-minibuffer-prompt-end: Changed from defsubst to defun.
;; 2005/11/13 dadams
;;     icicle-mouse-candidate-action: buffer-substring -> buffer-substring-no-properties.
;;     icicle-completing-read: Bind, don't set, minibuffer-completion-table.
;;     icicle-buffer*: Use other buffer for DEF, not INIT-VALUE.
;;     Added: icicle-preselect-init-value-flag, icicle-(add|remove)-buffer-*,
;;            icicle-read-from-minibuf-nil-default, icicle-buffer-list,
;;            icicle-select-minibuffer-contents, icicle-completing-p.
;;     icicle-minibuffer-setup:
;;       Select minibuf contents if icicle-preselect-init-value-flag.
;;       Only display *Completions* if icicle-completing-p.
;;     Advised next-history-element.
;; 2005/11/11 dadams
;;     Added: icicle-show-*Completions*-initially-flag, icicle-display-*Completions*.
;;     icicle-minibuffer-setup: If icicle-show-*Completions*-initially-flag, display it.
;; 2005/11/09 dadams
;;     Added: icicle-mouse-candidate-action.  Bind in icicle-rebind-completion-maps.
;;     icicle-buffer(-other-window): Use buffer-name-history as HIST arg to completing-read.
;; 2005/11/08 dadams
;;     Add/remove hook icicle-cancel-*Help*-redirection in icicle-mode, not at top level.
;;     Removed icicle-reset-icicle-menu-items-alist.
;;     Reset icicle-menu-items-alist in icicle-execute-menu-command of icicles-menu.el.
;; 2005/11/06 dadams
;;     Include minibuffer-local-filename-completion-map.
;; 2005/11/05 dadams
;;     icicle-display-candidates-in-*Completions*: Don't try to highlight root if it is "".
;;     icicle-help-on-candidate:
;;       Test null, not boundp icicle-menu-items-alist.
;;       If menu item's command is a lambda, set cand-symb to nil.
;;     icicle-mode: Use icicle-reset-icicle-menu-items-alist on minibuffer-exit-hook.
;;     Added: icicle-reset-icicle-menu-items-alist.
;;     Added defvar for icicle-menu-items-alist.
;;     Added byte-compiler comments and defvars to quiet byte-compile.
;; 2005/11/04 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Bug fix - use (functionp minibuffer-completion-table), not (icicle-file-name-input-p).
;; 2005/11/03 dadams
;;     Added: icicle-filter-wo-input and vars icicle-must-*, icicle-extra*, icicle-buffer-*,
;;            icicle-buffer-config*, icicle-buffer-sort*.
;;     icicle-unsorted-*: Use icicle-filter-wo-input and icicle-extra-candidates.
;;     Added Commentary section Global Filters.
;;     icicle-buffer* commands: Added filter bindings.
;;     icicle-define(-file)-command: Minor bug fix: Ensure buffer is live before switching back.
;; 2005/11/01 dadams
;;     Added: icicle-must(-not)-match-regexp.  Use in icicle-unsorted-*-candidates.
;; 2005/10/31 dadams
;;     Added: icicle-use-default-as-init-value-flag.  Use in completing-read.
;;     icicle-find-file*: Minor bug fix - REQUIRE-MATCH should be nil.
;; 2005/10/29 dadams
;;     icicle-display-candidates-in-*Completions: Minor bug fix - wrap in save-window-excursion.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Minor bug fix - do nothing if file & user erased minibuffer.
;;     Menu-bar menus:
;;       Enable Icicles menu items only in Icicle mode.  Put search
;;       stuff on Search menu, if available.   Use "[Icy]" prefix for
;;       Icicles items in menus other than "Icicles".
;; 2005/10/28 dadams
;;     Added: icicle-define-file-command.
;;            Use it to define icicle-delete-file, icicle-find-file*.
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Do action before moving to next|prev.
;;     icicle-candidate-action: Raise *Completions* frame, to keep it on top.
;; 2005/10/27 dadams
;;     Added: icicle-define-command, icicle-find-file*, select-frame-set-input-focus.
;;     Redefined using icicle-define-command:
;;       icicle-bookmark, icicle-buffer*, icicle-color-theme, icicle-delete-file,
;;       icicle-find-file*, icicle-font, icicle-frame-*, icicle-recent-file*.
;;     icicle-all-candidates-action: Report failures, not successes.  Use error msg.
;;     Added Commentary sections: Special-Character Conflicts, Defining Icicles Commands.
;;     Commentary section Act on All Candidates: Added delete-one-or-more-files example.
;;     Added icicle-find-file* to menu-bar menus.
;;     Inactivated top-level menu items when minibuffer is active.
;;     Renamed: icicle-delete-file-1 to icicle-delete-file-or-directory.
;; 2005/10/25 dadams
;;     Thx to Lennart Borgman for suggestion about select-frame-set-input-focus.
;; 2005/10/24 dadams
;;     icicle-search:
;;       1) Bug fix - need to have mouse-choose-completion set icicle-candidate-nb.
;;       2) Show error message.
;;     Default value of icicle-candidate-nb is now nil, not -1.
;;     Added: icicle-mouse-choose-completion, icicle-nb-of-candidate-in-*Completions*.
;;     icicle-move-to-(next|previous)-completion, icicle-increment-cand-nb+signal-end:
;;       Reset candidate number to 0 if nil.
;;     icicle-(redefine|restore)-std-completion-fns: Use icicle-mouse-choose-completion.
;; 2005/10/23 dadams
;;     Added: icicle-mode-map.
;;     icicle-(bind|restore)-completion-keys: Updated menu-bar menu.
;;     icicle-compilation-search: Error if not in a compilation buffer.
;; 2005/10/21 dadams
;;     icicle-remove-duplicates: redefined.
;; 2005/10/18 dadams
;;     icicle-file-name-input-p doc string:
;;       Mention why don't use minibuffer-completing-file-name.
;; 2005/10/16 dadams
;;     Added: icicle-compilation-search, icicle-search-hook.
;;     icicle-search: Run icicle-search-hook.  Added optional sit-for-period arg.
;;     icicle-mode: Added list of top-level commands to doc string.
;;     icicle-scroll-or-update-*Completions*: Added msg arg - only display msg if don't scroll.
;; 2005/10/14 dadams
;;     Allow for multisets of candidates.
;;     Added: icicle-search, icicle-completion-nospace-flag, icicle-candidate-nb,
;;            icicle-filter-alist, icicle-increment-cand-nb+signal-end.
;;     Commentary: Updated for icicle-search.
;;     icicle-next-candidate: Major rewrite.
;;       Uses icicle-candidate-nb, icicle-increment-cand-nb+signal-end,
;;            icicle-move-to-next-completion.
;;     Use icicle-completion-nospace-flag in calls to all-completions.
;;     icicle-previous-(apropos|prefix)-candidate,
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action: Added optional arg.
;;     icicle-apropos-complete-1, icicle-next-candidate, icicle-recompute-candidates:
;;       Added *-action commands to memq test.
;;     icicle-move-to-next-completion: Added optional no-minibuffer-follow-p arg.
;;     icicle-scroll-or-update-*Completions*: Update display even if handle-switch-frame.
;; 2005/10/12 dadams
;;     Added: icicle-redefine-std-completion-fns,
;;            icicle-restore-std-completion-fns,
;;            icicle-delete-windows-on, icicle-frames-on.
;;     icicle-mode: Use icicle-redefine-std-completion-fns,
;;                  icicle-restore-std-completion-fns.
;;     Renamed to use icicle- prefix: choose-completion-string,
;;       completing-read, completion-setup-function, exit-minibuffer,
;;       minibuffer-complete-and-exit, read-file-name,
;;       switch-to-completions.  Added these and also old- versions.
;;     icicle-history: Treat file names also.
;;     remove-windows-on -> icicle-delete-windows-on.
;; 2005/10/11 dadams
;;     Added: icicle-history, icicle-scroll-or-update-*Completions*,
;;            icicle-undo-std-completion-faces.
;;     Minor bug fixes:
;;       icicle-remove-dots: Also match against "." and ".." (lack of slash in Emacs 21+).
;;       icicle-save-or-*: Don't reset to last input if icicle-last-completion-candidate is "".
;;                         Update icicle-last-completion-candidate also to use current input.
;;       Reset icicle-last-input in icicle-minibuffer-setup, not in
;;         completing-read and read-file-name.
;;       icicle-display-candidates-in-*Completions*, icicle-next-candidate:
;;         Put candidate in consp before applying predicate.
;;       icicle-recompute-candidates: Don't recompute unless icicle-last-completion-command.
;;       icicle-retrieve-last-input: Use icicle-current-input, not icicle-last-input.
;;       icicle-self-insert: Update icicle-current-input and set this-command to
;;                           icicle-apropos-complete.
;;       icicle-apropos-complete: Use error-message-string.
;;       icicle-apropos-complete-1:
;;         Protect icicle-file-directory-p with
;;         icicle-file-name-input-p.  Unconditionally update
;;         icicle-last-completion-command.
;;     Moved undoing of std completion faces to icicle-mode.
;;     Use icicle-scroll-or-update-*Completions* in icicle-candidate-set-1.
;; 2005/10/06 dadams
;;     icicle-prefix-complete, icicle-apropos-complete-1:
;;       Removed vestigial slash cruft - should have been removed in 2005/09/01 fix.
;;     Added: icicle-remove-dots.  Use in icicle-save-or-restore-input.
;; 2005/10/05 dadams
;;     icicle-msg-maybe-in-minibuffer: use format-string arg.
;; 2005/10/04 dadams
;;     Replace use of minibuffer-completion-help by
;;       icicle-apropos-complete.
;;     Added: icicle-recent-file*, icicle-toggle-ignored-extensions,
;;            icicle-update-completions, icicle-msg-maybe-in-minibuffer,
;;            icicle-saved-ignored-extensions.
;;     Bound icicle-toggle-*.
;;     icicle-toggle-sorting: Use icicle-update-completions, icicle-msg-maybe-in-minibuffer.
;;     icicle-sort-and-strip-ignored:
;;       icicle-ignored-extensions-regexp nil => nothing is ignored.
;;     Reorder key bindings, so prompt shows S-tab, not S-iso-lefttab.
;;     icicle-next-candidate: Fixed code to highlight candidate in *Completions*: restriction.
;; 2005/10/03 dadams
;;     Regexps can now use backslash (it is no longer a directory separator on MS Windows).
;;       icicle-minibuffer-contents-from-minibuffer, icicle-file-name-directory-w-default:
;;         Escape backslash, so not used as directory separator on MS Windows.
;;       Added: icicle-apropos-complete-1, icicle-file-name-nondirectory.
;;       icicle-apropos-complete: Use icicle-apropos-complete-1.
;;                                Treat regexp error via message.
;;       Use icicle-file-name-nondirectory everywhere, instead of file-name-nondirectory.
;;     Can now use "?" for regexps; it no longer shows completion list.
;;     Do icicle-update-ignored-extensions-regexp inside icicle-minibuffer-setup.
;;     Added and bound: icicle-retrieve-last-input.
;;     Updated icicle-completion-help-string with recent bindings.
;;     Renamed: icicle-last-command to icicle-last-completion-command.
;;              icicle-candidate-set-restore to icicle-candidate-set-retrieve.
;; 2005/10/01 dadams
;;     Added: icicle-candidate-set-(define|restore|swap).
;;     Changed binding of icicle-candidate-set-save to C->.
;;     Bound new commands.
;; 2005/10/01 dadams
;;     Major rewrite to add set operations: complement, difference, union, intersection.
;;       Added: icicle-completion-candidates, icicle-current-input, icicle-candidate-set-*,
;;              icicle-set-*, icicle-save-or-restore-input, icicle-recompute-candidates.
;;       Bound icicle-candidate-set*.
;;       Added Commentary for Sets of Completion Candidates.
;;       icicle-(apropos|prefix)-complete: Update icicle-completion-candidates, only as needed.
;;       icicle-next-candidate:
;;         Reverse candidates only if switched to opposite-direction command of same type.
;;         Likewise, for refresh of *Completions*.
;;         Protect put-text-property for root (e.g. no match for complement).
;;       icicle-(apropos|prefix)-complete,
;;       icicle-prefix-word-complete, icicle-next-candidate: Use icicle-completion-candidates.
;;       icicle-all-candidates-action: Use icicle-completion-candidates, not *-apropos-complete.
;;       icicle-display-candidates-in-*Completions*:
;;         Removed first arg (candidates).  Update icicle-completion-candidates.
;;    icicle-all-candidates-action:
;;      Use icicle-completion-candidates, so act on completions of either kind.
;; 2005/09/30 dadams
;;     Commented out resetting of minibuffer-completion-table to nil for icompletion.
;;     Thx to Andrey for bug report on M-x M-r problem.
;; 2005/09/27 dadams
;;     icicle-(bind|restore)-completion-keys: Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/26 dadams
;;     Bug fix: Changed "\C-!"  to [(control ?!)] (others similarly).
;;     Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/16 dadams
;;     Added: icicle-all-candidates-action, icicle-delete-file*,
;;     icicle-rebind-completion-maps: Bound icicle-all-candidates-action to C-!.
;;     icicle-(apropos|prefix)-complete: Return candidates list.
;;     icicle-bookmark, icicle-buffer*, icicle-color-theme,
;;     icicle-font, icicle-frame*: Return t for success, nil for failure.
;;     Commentary: Added section Choose All Completion Candidates.
;; 2005/09/14 dadams
;;     icicle-rebind-completion-maps: Bound TAB and S-TAB for navigation.
;;     icicle-move-to-(next|previous)-completion, icicle-(next|previous)-line: Wrap around.
;; 2005/09/13 dadams
;;     Major rewrite of file treatment, to treat directory candidates similarly to files.
;;     Added: icicle-default-directory, icicle-file-directory-p, icicle-sort-function,
;;            icicle-toggle-sorting, toggle-icicle-sorting.
;;     Use icicle-file-directory-p everywhere, except choose-completion-string.
;;     Removed: icicle-nondirectory-*.
;;     icicle-next-candidate: If not icicle-cycle-into-subdirs-flag, then use relative
;;                            file/dir name, not nondirectory part.
;;     icicle-(apropos|prefix)-complete:
;;       Set icicle-default-directory if sole completion is a subdirectory.
;;     icicle-sort-and-strip-ignored: Removed optional arg and treatment of subdirs.
;;     icicle-next-(apropos|prefix)-candidate, icicle-(apropos|prefix)-complete:
;;       Don't treat icicle-cycle-into-subdirs-flag here.
;;     icicle-(apropos|prefix)-complete, icicle-next-candidate:
;;       Set icicle-default-directory, if directory candidate.
;;     icicle-minibuffer-setup: Set icicle-default-directory.
;;     icicle-apropos-complete: Different message if icicle-apropos-icompleting-p.
;;     icicle-sort-dirs-last: Treat other kinds of candidates, besides files and dirs.
;;     Commentary and doc strings: Updated for icicle-sort-function, icicle-cycle-into-subdirs.
;;     Let delete-selection mode work with icicle-self-insert.
;;     icicle-display-candidates-in-*Completions*: Make *Completions* read-only.
;; 2005/09/09 dadams
;;     choose-completion-string: bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;; 2005/09/08 dadams
;;     completion-setup-function: bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;;     Added: icicle-remap, icicle-unmap, icicle-(bind|restore)-completion-keys.
;;     completing-read: Do not append suffix if not in Icicle mode.
;;     icicle-rebind-completion-maps:
;;       Clean-up.  Use icicle-(bind|restore)-completion-keys.
;;       Don't (suppress-keymap completion-list-mode-map).
;; 2005/09/06 dadams
;;     Provided apropos icompletion.
;;     Added: icicle-self-insert, icicle-incremental-completion-flag,
;;            icicle-apropos-icompleting-p.
;;     icicle-apropos-complete: Respect icicle-apropos-icompleting-p.
;;     Commentary: Updated Icompletion and Customization sections.
;;                 Added Apropos Icompletion.
;;     Changed default value of icicle-word-completion-key to M-SPC.
;;     icicle-rebind-completion-maps:
;;       Bind icicle-self-insert. Use self-insert for SPC.
;;       Updated icicle-completion-help-string.  Treat menu-bar menu for the minibuffer.
;;     completion-setup-function: Add instruction2 only when icicle-mode.
;;     icicle-display-candidates-in-*Completions*: Use save-restriction.
;;     icicle-minibuffer-contents-from-minibuffer:
;;       Allow for mixing $ of environment vars with $ of regexps.
;; 2005/09/02 dadams
;;     Added: icicle-bookmark, icicle-buffer(-other-window), icicle-candidate-action,
;;            icicle-candidate-action-fn, icicle-color-theme(s), icicle-font,
;;            icicle-frame-(b|f)g.
;;     Renamed: icicle-(next|previous)-(apropos|prefix)-*-help to
;;              icicle-(next|previous)-(apropos|prefix)-*-action.
;;     icicle-(apropos|prefix)-complete: Set icicle-last-completion-candidate.
;;     In renamed functions:  Use icicle-candidate-action, not icicle-help-on-candidate.
;;     icicle-rebind-completion-maps: Bound C-o to icicle-candidate-action.
;;     Added Commentary section on actions on candidates.
;;     icicle-move-to-next-completion: Test line num, not char position (fix).
;;     icicle-previous-line: 3 or 4, not 4 or 5 (fix).
;; 2005/09/01 dadams
;;     Fixed major bug: file-name completion did not work at all in non-MS Windows!
;;       icicle-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;       icicle-nondirectory-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".  Bind default-directory.
;;       icicle-(apropos|prefix)-complete: Treat case when icicle-cycle-into-subdirs-flag = nil.
;;     icicle-next-candidate: Took out code that moved point when line is too long.
;;     icicle-minibuffer-setup: Reset icicle-prompt.
;; 2005/08/23 dadams
;;     Added: icicle-help-on-candidate, icicle-cancel-*Help*-redirection,
;;            icicle-(previous|next)-(prefix|apropos)-candidate-help.  Bound them all.
;;     icicle-rebind-completion-maps:
;;       Bound icicle-help-on-candidate, icicle-(previous|next)-(prefix|apropos)-candidate-help.
;; 2005/08/22 dadams
;;     Unconditionally require cl.el when compile (because of case).
;; 2005/08/19 dadams
;;     Renamed icicle-cursor-position-in-candidate to icicle-point-position-in-candidate.
;;     Added: icicle-mark-position-in-candidate, icicle-minibuffer-prompt-end.
;;     icicle-place-cursor: Position both point and mark.
;;     icicle-point-position-in-candidate: Change values from bob, eob to input-start/end.
;;     Removed: icicle-select-rest-of-completion-flag.
;;     Use inequality test on point and mark.
;; 2005/08/16 dadams
;;     Minbuffer messages: Differentiate prefix from apropos completion.
;;     completing-read, read-file-name: Append icicle-prompt-suffix for Emacs 20 (oversight).
;; 2005/08/15 dadams
;;     Bug fix: Only use face-spec-reset-face if target faces defined.
;;     read-file-name: bug fix:
;;       Use condition-case to get the correct number of args for
;;       old-read-file-name. Thx to Mathias Dahl for both bug reports.
;; 2005/08/14 dadams
;;     icicle-place-cursor: Narrow region to exclude minibuffer-prompt
;; 2005/08/13 dadams
;;     Add regexp support (removed it when introduced highlighting).
;;       icicle-next-candidate: Added regexp-p arg.  Use in icicle-next-apropos-candidate.
;;       icicle-place-cursor: Use regexp search.  For root-start, go to match-beginning.
;;       icicle-unsorted-file-name-apropos-candidates: Don't use regexp-quote.
;;     icicle-switch-to-*Completions*:
;;       Search in restriction of mouse-face zone; repeat.
;;       Treat file case (use nondirectory part).  Bind case-fold-search.
;;     Protect (aref <input> 0) against empty string.
;;     member -> memq, for symbols.
;; 2005/08/12 dadams
;;     Added: icicle-word-completion-key, icy-mode, icicle-insert-a-space.
;;     icicle-rebind-completion-maps: Use icicle-word-completion-key and icicle-insert-a-space.
;;     completing-read, icicle-rebind-completion-maps: Corrected bindings in doc string.
;; 2005/07/29 dadams
;;     Added: icicle-change-region-background-flag, icicle-increment-color-value,
;;            icicle-region-background, icicle-saved-region-background,
;;            icicle-restore-region-face.
;;     Added icicle-restore-region-face to minibuffer-exit-hook.
;;     Require hexrgb.el.
;;     Removed: icicle-select-rest-of-completion.
;;     icicle-minibuffer-setup:
;;       Save icicle-saved-region-background and use icicle-region-background.
;; 2005/07/28 dadams
;;     Added: icicle-*Completions*-instruction-*.
;;     completion-setup-function:
;;       Use icicle-*Completions*-instruction-*.
;;       Remove ? from instruction2.  Put both instr on same line.
;;       Use put-text-property, not *-w-face*.
;;     ------
;;     Move all completion stuff here, from simple+.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;;     Wrap *Completions* window deletion in save-selected-window.
;;     Added icicle-prefix-word-complete, and bound it to SPC.
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/27 dadams
;;     Renamed: icicle-completing-read-prompt* to icicle-prompt*.
;;     Added: read-file-name, face
;;            icicle-completing-read-prompt-suffix, icicle-remove-property,
;;            icicle-select-rest-of-completion (simple, for now).
;;     completing-read: Apply faces to prompt.
;;     icicle-place-cursor: Use icicle-select-rest-of-completion.
;;     Added (if icicle-mode (icicle-mode 1)) at end.
;;     Reworded Commentary in terms of "input completion", not just completing-read.
;; 2005/07/26 dadams
;;     rebind-minibuffer-completion-maps: Minor bug fix.
;;     icicle-mode: Added " Icy" to mode line.
;;     Wrapped Emacs 21 version of icicle-mode (with define-minor-mode) in (eval (quote...)),
;;       so byte-compiling in Emacs 20 will produce a *.elc that works in Emacs 21.
;; 2005/07/25 dadams
;;     Added: icicle-mode, icicle-*-hook, icicle-minibuffer-setup, icicle-activate-mark.
;;     rebind-minibuffer-completion-maps:
;;       Restore bindings when exit Icicle mode.
;;       Added argument.  Pick up everything bound to help-command.
;;       Message only when mode is turned on.
;; 2005/07/24 dadams
;;     Now leave region from end of root to end of completion, so you can easily replace it,
;;       especially if you use delete-selection mode.  (Suggestion by Lennart Borgman.)
;;     Added: icicle-select-rest-of-completion-flag.
;;     icicle-place-cursor: Create active region if icicle-select-rest-of-completion-flag
;;     icicle-completion-help: Removed icicle-abort-minibuffer-input.
;;     icicle-abort-minibuffer-input: Removed obsolete code & comment on icomplete-inhibit.
;; 2005/07/22 dadams
;;     Major fixup: Treat file and directory names well, respect standard user options, more.
;;     Renamed:
;;       icicle-(next|previous)?-completion-candidate to icicle-*-prefix-candidate(s),
;;       icicle*filename* to icicle*file-name*,
;;       icicle-descend-into-subdirs to icicle-cycle-into-subdirs-flag.
;;     Added: icicle-file-name-apropos-candidates, icicle-file-name-directory-w-default,
;;            icicle-file-name-input-p, icicle-file-name-prefix-candidates,
;;            icicle-nondirectory-file-name-apropos-candidates,
;;            icicle-nondirectory-file-name-prefix-candidates,
;;            icicle-sort-dirs-last, icicle-unsorted-apropos-candidates,
;;            icicle-unsorted-file-name-apropos-candidates,
;;            icicle-unsorted-file-name-prefix-candidates, icicle-unsorted-prefix-candidates,
;;            icicle-last-command.
;;     Respect insert-default-directory and completion-auto-help.
;;     Use minibuffer-message instead of message.
;;     Commentary: Added Customization & Tips section.
;;     completing-read: Save icicle-last-input.  Reset icicle-nb-of-other-cycle-candidates.
;;     icicle-next-*-candidate: Branch to file-specific functions.
;;     icicle-*-candidates: Use icicle-unsorted-*-candidates.
;;     icicle-next-candidate:
;;       Delete *Completions* window if no candidates.
;;       Use icicle-file-name-directory, not file-name-directory.
;;     icicle-minibuffer-contents-from-minibuffer: Use substitute-in-file-name.
;;     icicle-*-complete:
;;       Treat slashed file names (e.g. "/foo").
;;       Use icicle-file-name-*-candidates, icicle-file-name-directory-w-default for files.
;;       Added messages [No completion], [Sole completion], [Complete, but not unique].
;;       Use icicle-last-command for repetition test. And set it.
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string and message.
;; 2005/07/21 dadams
;;     icicle-apropos-candidates: Use, not apropos, but delete-if-not on string-match.
;;                                Treat files too.
;;     Removed icicle-intersection.
;;     Added: icicle-descend-into-subdirs.
;;     icicle-sort-and-strip-ignored: Use icicle-descend-into-subdirs.  Don't use "." and "..".
;;     icicle-next-candidate:
;;       File names w/o dir.
;;       Use regexp-quote on root for underlining file-name root.
;;       Insert directory name for file.
;;     icicle-place-cursor: Search past dir, then search for file-name w/o dir.
;;     icicle-prefix-complete, icicle-apropos-complete,
;;     icicle-switch-to-*Completions*: Use icicle-minibuffer-contents-from-minibuffer.
;;     icicle-prefix-complete, icicle-apropos-complete: Insert dir when single candidate.
;;     icicle-display-candidates-in-*Completions*: Underline file-name w/o dir.
;; 2005/07/20 dadams
;;     icicle-next-candidate, *-display-candidates-in-*Completions*: Use set-buffer-modified-p.
;;     icicle-next-candidate: Use ding when hit end of cycle.
;;     Added: icicle-cursor-position-in-candidate, icicle-place-cursor.
;;            Use in icicle-next-candidate to position cursor.
;;     Added: defgroup icicles.
;; 2005/07/19 dadams
;;     Initialize icicle-ignored-*.
;;     Added: icicle-nb-of-other-cycle-candidates, icicle-minibuffer-contents-from-minibuffer.
;;     completing-read: Reset icicle-last-completion-candidate to nil.
;;     icicle-next-candidate:
;;       Use icicle-minibuffer-contents-from-minibuffer.
;;       Save icicle-nb-of-other-cycle-candidates for icomplete-completions (icomplete+).
;;       Use copy of "next" string since change its text properties.
;;       Use regexp-quote for underlined root.
;;       Use put-text-property, so works in Emacs 20.
;;       Update *Completions*, even if last command is repeated.
;;     icicle-*-complete: Complete rewrite.
;;     icicle-display-candidates-in-*Completions*: Do even if last command is repeated.
;; 2005/07/18 dadams
;;     icicle-display-*: Highlight only first occurrence in each candidate.
;;     icicle-next-candidate: Use completion-ignore-case.
;; 2005/07/17 dadams
;;     Treat file names also.
;;     Added: icicle-delete-if*, and use instead of delete-if-*.  Removed require cl.el.
;;     Added: icicle-ignored-extensions*, icicle-sort-and-strip-ignored,
;;            icicle-filename-input-p, icicle-update-ignored-extensions-regexp,
;;            icicle-prefix-complete.  Bound icicle-prefix-complete.
;;     Use icicle-update-ignored-extensions-regexp as minibuffer-setup-hook.
;;     icicle-*-candidates: Use icicle-sort-and-strip-ignored.
;;     icicle-next-candidate, icicle-display-candidates-in-*Completions*:
;;       Don't use predicate on file-name candidates (icicle-filename-input-p).
;;     icicle-next-candidate:
;;       Use read-file-name-completion-ignore-case (Emacs 22) and file-name-nondirectory.
;;     icicle-apropos-complete: Return t/nil. Treat single candidate as no-op.
;;     Reset std completions-* faces, so they don't interfere with apropos highlighting.
;; 2005/07/16 dadams
;;     Added: icicle-display-*, icicle-apropos-complete.
;;     Use icicle-display-* in icicle-next-candidate and icicle-apropos-complete.
;;     Bound icicle-apropos-complete to S-tab in completion maps.
;;     icicle-switch-to-*Completions*:
;;       Move to start of candidate.  Highlight candidate, not regexp.
;;     icicle-next-candidate: Underline the root that was completed.
;;     Added: faces icicle-root-highlight-*.
;;     Removed: faces: icicle-completion-help*.
;;     Removed (not used): require of strings.el.
;;     Commentary: Added Nutshell View.
;; 2005/07/15 dadams
;;     Renamed: icicle-completion-help+ to icicle-completion-help.
;;     Replaced: icicle-delete-lines by icicle-erase-minibuffer.
;;     icicle-next-candidate:
;;       Wrapped display-* and re-search-forward in condition-case.  Use icicle-place-overlay.
;;     Changed icicle-completion-help bindings to [f1].
;;     Added: icicle-*-line, icicle-switch-to-*, icicle-move-to-*-completion,
;;            icicle-current-completion-in-*Completions*, icicle-place-overlay.
;;     Added bindings for icicle-*-line, icicle-switch-to-*, icicle-move-to-*.
;;     Bound q to icicle-abort-minibuffer-input in completion-list-mode-map.
;;     icicle-completing-read-prompt-suffix: Mention both [f1] and ?.
;;     Removed: icicle-fit-frame.
;;     Commentary: Added How...Improves...(5).  Updated Key Bindings.
;; 2005/07/14 dadams
;;     icicle-next-candidate:
;;       Update *Completions*, if displayed, to reflect current
;;       candidates, but don't do it if this-command = last-command.
;;       Reverse list as needed, to keep same order.   Ensure current
;;       *Completions* choice shows in window (recenter as needed).
;;       For highlighting: Search with re-search-forward to be sure to get the right one.
;;       Took test for presence of predicate out of loop.
;;     Commentary: Added Note on pop-up-frames = t.
;; 2005/07/13 dadams
;;     Rewrote icicle-apropos-candidates.
;;     Added: icicle-intersection.
;; 2005/07/12 dadams
;;     Added: icicle-(next|previous)-apropos-candidate, icicle-next-candidate,
;;            icicle-apropos-candidates, icicle-completion-candidates.
;;     Bound: icicle-(next|previous)-apropos-candidate.
;;     Renamed: icicle-completion-help-(title-)face: Removed "-face".
;;     icicle-next-completion-candidate: Redefined to use icicle-next-candidate.
;;     icicle-rebind-completion-maps: Updated text to mention apropos completion.
;;     icicle-completion-help+: Use icicle-abort-minibuffer-input, not abort-recursive-edit.
;; 2005/07/10 dadams
;;     First version of icicles.el (previously called elect-mbuf.el).
;;     Renamed: minibuffer-completion-help-string to icicle-completion-help-string,
;;              completing-read-prompt to icicle-completing-read-prompt,
;;              completing-read-prompt-suffix to icicle-completing-read-prompt-suffix,
;;              mbuf-completion-help-face to icicle-completion-help-face,
;;              mbuf-completion-help-title-face to icicle-completion-help-title-face,
;;              minibuffer-last-default to icicle-last-completion-candidate,
;;              command-calling-for-completion to icicle-cmd-calling-for-completion,
;;              minibuffer-completion-help+ to icicle-completion-help+,
;;              abort-minibuffer-input to icicle-abort-minibuffer-input,
;;              next-default-input to icicle-next-completion-candidate,
;;              previous-default-input to icicle-previous-completion-candidate,
;;              rebind-minibuffer-completion-maps to icicle-rebind-completion-maps,
;;     Added: minibuffer-complete-and-exit, icicle-fit-frame, icicle-last-input.
;;     Moved delete-lines here from and renamed to icicle-delete-lines.
;;     Removed: mod+ (unused).
;;     icicle-completion-help+: Use *Help*, not *Completions*.  Don't show completions.
;;     icicle-next-completion-candidate: Use insert, not insert-string.
;;     icicle-rebind-completion-maps: Made it interactive.
;; 2005/07/09 dadams
;;     Removed: buffer-alist (not used).
;; 2005/05/15 dadams
;;     Renamed: flash-ding-minibuffer-frame to 1on1-flash-ding-minibuffer-frame.
;; 2005/05/10 dadams
;;     Hacked completing-read to remove *Completions* window at end if require-match is non-nil.
;;       (Don't know why/when this became a problem.)
;; 2004/09/21 dadams
;;     Updated to work in Emacs 21 (and 20):
;;       next-default-input uses delete-minibuffer-contents for 21, but erase-buffer for 20.
;;       minibuffer-completion-help+: bind inhibit-read-only to t around erase-buffer.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/09/03 dadams
;;     Added: mbuf-completion-help-face, mbuf-completion-help-title-face.
;;     minibuffer-completion-help+: Use mbuf-*-face's instead of hard-coding.
;;     minibuffer-completion-help-string, completing-read-prompt-suffix: defconst -> defvar.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/13 dadams
;;     Bound delete-lines to M-S-DEL and M-S-backspace.
;; 1999/03/17 dadams
;;     protect calls with test fboundp.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/03/26 dadams
;;     minibuffer-completion-help+: concat -> concat-w-faces (color).
;; 1995/12/20 dadams
;;     exit-minibuffer: Iconify *Completion* frame.
;; 1995/12/15 dadams
;;     abort-minibuffer-input: Reset minibuffer-completion-table to avoid icompletion.
;;     Defined replacement exit-minibuffer to do the same as #1.
;; 1995/12/01 dadams
;;     abort-minibuffer-input: Incorporated delete-selection-mode code
;;     rebind-minibuffer-completion-maps:
;;       Added C-g bindings for minibuffer-local-map, minibuffer-local-ns-map,
;;         minibuffer-local-isearch-map.
;; 1995/10/25 dadams
;;     Put defvar of minibuffer-completion-help-string after do
;;       rebind-minibuffer-completion-maps, so its doc string gives bindings.
;; 1995/10/24 dadams
;;     Mention ESC-TAB completion in completing-read.
;; 1995/10/17 dadams
;;     Let minibuffer use ESC-TAB for completion (Lisp symbols etc.)
;;     completing-read: Minibuffer adopts current buffer's ESC-TAB binding.
;;     Added command-calling-for-completion to memorize current command (done in
;;       completion-setup-hook).
;; 1995/09/12 dadams
;;     Added abort-minibuffer-input.
;;     Define C-g as abort-minibuffer-input in completion-list-mode-map and
;;       minibuffer-local-* maps.
;;     No self-insertion for completion-list-mode-map.
;; 1995/08/16 dadams
;;     next-default-input: Fixed bug - skip repeated alist entries.
;; 1995/08/10 dadams
;;     Rewrote minibuffer-completion-help+: Provide help even if no completions.
;;     So, added minibuffer-completion-help-string.
;;     `?' defined correctly for minibuffer-local-must-match-map.
;; 1995/08/08 dadams
;;     next-default-input: error msg: no hard coding of key seq.
;; 1995/08/02 dadams
;;     Major rewrite.
;;       No reminders in prompts.  Added minibuffer-completion-help+ to provide help info for
;;         *Completions*.
;;     Log for functions that were previously in simple+.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;; 2005/07/28 dadams
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function: Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion to icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     choose-completion-string:
;;       Added doc string.  Updated to correspond to Emacs 34.1.
;;     completion-setup-function: diff prompt setups.  face1 & face2 tests.
;;     Added: switch-to-completions.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
 
;;;(@* "CHANGE LOG FOR `icicles-cmd.el'" - Deprecated file)
;;
;; 2009/05/22 dadams
;;     Split in two: icicles-cmd1.el, icicles-cmd2.el.  File too large to upload to Emacs Wiki.
;; 2009/05/20 dadams
;;     Renamed: *-Info-build-node-completions-fix-* to *-Info-build-node-completions-1.
;;     icicle-Info-goto-node: Plain C-u now prepends Info file name to candidates.
;;     icicle-Info-goto-node-1: swapped arg order in call to Info-goto-node nodename (bug fix).
;;     icicle-Info-read-node-name, icicle-Info-build-node-completions(-1):
;;       Added optional arg INCLUDE-FILE-P.
;; 2009/05/11 dadams
;;     icicle-complete-keys: Use icicle-local-keys-first-p as icicle-sort-function.
;;                           Bind icicle-hist-cands-no-highlight to ("..").
;;     icicle-keys+cmds-w-prefix: Propertize candidate "..".
;;     icicle-dabbrev-completion: Use icicle-upcase.
;; 2009/05/10 dadams
;;     Added: icicle-search-(defs|lines|paragraphs|sentences|pages).
;;     icicle-plist: Pretty-print, unless negative prefix arg (or Emacs 20).
;;     icicle-search-regexp-scan: Fix to pick up candidate at eob: removed (not (eobp)) test.
;; 2009/05/05 dadams
;;     icicle-find-file-(absolute|in-tags-table)(-other-window),
;;       icicle-(recent|locate)-file(-other-window), icicle-describe-option-of-type,
;;       icicle-(vardoc|fundoc|doc|plist), icicle-goto-global-marker:
;;         Remove quote before face name, for icicle-candidate-properties-alist value.
;;     icicle-buffer(-other-window|-list), icicle-kill-buffer, icicle-choose-candidate-of-type:
;;       Bind completion-ignore-case, based on read-buffer-completion-ignore-case.
;; 2009/05/01 dadams
;;     Added: icicle-shell-command-on-file.
;;     Bind both icicle-all-candidates-list-alt-action-fn and icicle-candidate-alt-action-fn
;;       to icicle-alt-act-fn-for-type for appropriate type - same commands as for 2009/04/26,
;;       except removed these for *recent* (mistaken).
;;     Use renaming from icicles-fn.el: *-choose-action-for-type -> *-alt-act-fn-for-type.
;; 2009/04/28 dadams
;;     icicle-object-action: 
;;       At end, just apply the fn returned by icicle-choose-action-for-type to chosen object.
;;       Use icicle-type-actions-alist, not icicle-object-named-types.
;;       Update to use with icicle-predicate-types-alist (not flat list of preds).
;;     icicle-choose-candidate-of-type: Handle color type too.
;;     icicle-get-anything-actions-for-type:
;;       Highlight Anything actions using face icicle-special-candidate.
;; 2009/04/26 dadams
;;     icicle-marker+text, icicle-compilation-search-in-context-fn:
;;       Bind inhibit-field-text-motion to t, to ensure real end-of-line.
;;     Load icicles-mac (at compile time) using load-library, to ensure latest .elc.
;;     Bind icicle-candidate-alt-action-fn, where appropriate, to icicle-choose-action-for-type:
;;       *-lisp-complete-symbol, *-customize-face(-other-window), *-execute-extended-command,
;;       *-command-abbrev, *-command-abbrev-command, *-execute-named-keyboard-macro,
;;       *-set-option-to-t, *-reset-option-to-nil, *-toggle-option, *-select-frame,
;;       *-delete-windows, *-kill-buffer, icicle-buffer(-other-window),
;;       *-(add|remove)-buffer-candidate, *-(buffer|face|file|directory)-list,
;;       *-find-file(-absolute|-in-tags-table)(-other-window),
;;       *-(recent|locate)-file(-other-window), *-frame-(bg|fg), *-where-is,
;;       *-apropos(-variable|option|function|command), *-apply, *-save-string-to-variable,
;;       *--choose-candidate-of-type, *-read-var-value-satisfying, *-read-color. 
;;     icicle-execute-extended-command-1, icicle-command-abbrev-action:
;;       Rebind icicle-candidate-alt-action-fn to nil so we don't override it for invoked cmd.
;;     icicle-object-action: Respect icicle-use-anything-candidates-flag.
;; 2009/04/25 dadams
;;     icicle-find-file-in-tags-table(-other-window): Fixed copy+paste typo.
;; 2009/04/24 dadams
;;     Added: icicle-find-file-in-tags-table(-other-window).
;; 2009/04/19 dadams
;;     icicle-customize-apropos-options-of-type, icicle-dabbrev-completion:
;;       Don't fset unless standard function is defined.
;;     Use when/unless instead of or for fset's. (cosmetic)
;; 2009/04/18 dadams
;;     Soft-require hexrgb.el unconditionally, not just when there is a window-system.
;;     icicle-read-color: Error if hexrgb is not loaded.
;; 2009/04/11 dadams
;;     Added: icicle-region-add-short-help.
;;     icicle-font-w-orig-size: Construct short help only if user will see it.
;;     icicle-describe-opt-of-type-complete, icicle-marker+text,
;;       icicle-(select|search|remove)-region, icicle-search-(regexp|char-property)-scan:
;;         Add short help for mode-line and tooltip.
;;     icicle-region-help: Corrected start/end order.
;;     icicle-region-help, icicle-search-help: Removed unused help: No help.
;; 2009/04/10 dadams
;;     icicle-font-w-orig-size: Provide help string.  Set font to use orig-pointsize also.
;;     icicle-font: Modify frame - don't use menu-bar-mode.  Get orig menu-bar info from frame.
;;                  Save orig-pointsize also.
;;     icicle-frame-(bg|fg):
;;       Incorporate behavior of icicle-read-color: multi-completion etc.  Added short help.
;;       (Use icicle-make-color-candidate, icicle-color-completion-setup.)
;;     icicle-read-color:
;;       Factored out stuff as new function icicle-color-completion-setup.
;;       Corrected handling of ARG > 2.
;;     Moved to icicles-fn.el:
;;       icicle-color-help, icicle-make-color-candidate, icicle-remove-color-duplicates.
;; 2009/04/09 dadams
;;     Added: icicle-font-w-orig-size.
;;     icicle-font: Use icicle-font-w-orig-size.  No menu bar, to avoid Emacs frame-sizing bug.
;; 2009/04/04 dadams
;;     icicle-kill-buffer, icicle-buffer(-other-window|-list), icicle-choose-candidate-of-type:
;;       Bind icicle-sort-functions-alist for buffer names.
;; 2009/03/29 dadams
;;     icicle-make-color-candidate: Use white or black foreground, depending on color value.
;; 2009/03/27 dadams
;;     Renamed: icicle-default-buffer-name to icicle-default-buffer-names.
;;     icicle-buffer(-other-window):
;;       Bind bufflist, to make it available also to icicle-default-buffer-names.
;;     icicle-default-buffer-names: Emacs 23: Provide first 4 buffers from bufflist.
;; 2009/03/26 dadams
;;     Added: icicle-default-buffer-name.
;;     icicle-buffer(-other-window), icicle-add-buffer-candidate:
;;       Use icicle-default-buffer-name.
;; 2009/03/16 dadams
;;     icicle-buffer(-other-window), icicle-buffer-list, icicle-kill-buffer,
;;       icicle-add-buffer-candidate: Treat prefix arg buffers visiting files or frame-local.
;;     icicle-recent-file(-other-window): Bind icicle-use-candidates-only-once-alt-p to t.
;; 2009/03/15 dadams
;;     Added: icicle-remove-file-from-recentf-list, icicle-remove-from-recentf-candidate-action.
;;     icicle-recent-file(-other-window):
;;       Bind icicle-candidate-alt-action-fn  to icicle-remove-from-recentf-candidate-action.
;; 2009/03/10 dadams
;;     Added: icicle-recompute-shell-command-candidates.
;; 2009/02/28 dadams
;;     icicle-find-file-absolute(-other-window):
;;       Treat directory candidates as special candidates.
;;     icicle-apropos: Treat functions as special candidates.
;;     icicle-apropos-variable: Treat user options as special candidates.
;;     icicle-apropos-function: Treat commands as special candidates.
;;     icicle-apropos-command (non-apropos-fn+var version): Treat cmds as special candidates.
;; 2009/02/23 dadams
;;     icicle-Info-goto-node-action:
;;       Protect dir insertion with icicle-extra-candidates-dir-insert-p.
;; 2009/02/17 dadams
;;     icicle-lisp-complete-symbol:
;;       Added predicate arg.  Updated wrt Emacs 23 (better var completion).
;; 2009/02/01 dadams
;;     icicle-(find-file-absolute|(recent|locate)-file)(-other-window):
;;       Bind C-backspace to icicle-up-directory.
;; 2009/01/18 dadams
;;     icicle-dired-saved-file-candidates(-other-window):
;;       If there is no saved set, then use icicle-file-list to let user choose a set.
;;     Added: icicle-dired-chosen-files(-other-window), as aliases.
;; 2009/01/14 dadams
;;     Same commands as yesterday: icicle-remove-duplicates -> icicle-remove-dups-if-extras.
;; 2009/01/13 dadams
;;     icicle-kill-buffer, icicle-buffer(-other-window), icicle-(buffer|file|directory)-list,
;;       icicle-find-file(-absolute)(-other-window), icicle-(recent|locate)-file(-other-window),
;;       icicle-choose-candidate-of-type:
;;         Bind icicle-transform-function to icicle-remove-duplicates.
;; 2009/01/05 dadams
;;     Added: icicle-gud-gdb-complete-command.
;;     icicle-shell-dynamic-complete(-as)-environment-variable: (require 'shell)
;;     icicle-shell-dynamic-complete-as-command: Added candidate help for shell commands.
;; 2009/01/04 dadams
;;     Added ESS support:
;;       Added: icicle-comint-replace-by-expanded-filename, icicle-ess-complete-filename,
;;              icicle-ess(-internal)-complete-object-name, icicle-ess-R-complete-object-name.
;;       Thx to Sebastian Luque.
;;     Improved some doc strings.
;; 2009/01/02 dadams
;;     icicle-comint-dynamic-complete-as-filename, icicle-comint-dynamic-simple-complete:
;;       Fixed for comint-completion-autolist, absolute directories.  Thx to Sebastian Luque.
;; 2009/01/01 dadams
;;     icicle-comint-replace-orig-completion-fns:
;;       Use icicle-comint-dynamic-complete-replacements instead of hard-coded list.
;;     icicle-comint-dynamic-simple-complete: Fix for Emacs 20: map #'list over completions.
;; 2008/12/30 dadams
;;     Removed: shell-hook-fn.
;;     Removed fset here for: old-comint-dynamic-*, old-shell-dynamic-*, old-bbdb-complete-name.
;;     icicle-comint-dynamic-simple-complete: Map #'list over candidates (for Emacs 20).
;;     icicle-comint-hook-fn:
;;       Don't bind icicle-comint-command here - do it in icicle-bind-other-keymap-keys.
;; 2008/12/27 dadams
;;     Removed: icicle-find-file(-other-window)-w-wildcards.
;;     Removed soft require of dired+.el.
;;     icicle-find-file(-other-window):
;;       Use just find-file* with wildcards arg, not *-w-wildcards, as action fn.
;;       Bind icicle-default-value so that it won't insert the default value.
;;     icicle-find-file(-other-window), icicle-choose-candidate-of-type:
;;       Use nil as 3rd read-file-name arg, if not in Dired on a file name.
;; 2008/12/23 dadams
;;     icicle-comint-dynamic-complete-as-filename: Bind use-dialog-box to nil.
;; 2008/12/22 dadams
;;     icicle-comint-dynamic-complete-as-filename: Use INIT and PRED in read-file-name call.
;; 2008/12/21 dadams
;;     Added: icicle-shell-hook-fn, (icicle|old)-comint-dynamic-complete(-filename),
;;            (icicle|old)-shell-dynamic-complete-(command|filename|environment-variable), 
;;            icicle-comint-replace-orig-completion-fns, icicle-comint-dynamic-simple-complete,
;;            icicle-comint-dynamic-complete-as-filename,
;;            icicle-shell-dynamic-complete-as-(command|environment-variable).
;;     Renamed: icicle-comint-send-input to icicle-comint-search-send-input,
;;              icicle-comint-get-final-choice to icicle-comint-search-get-final-choice,
;;              icicle-comint-get-minibuffer-input to icicle-comint-search-get-minibuffer-input.
;;     Require shell.el at compile time.
;;     Use fn, not var, confirm-nonexistent-file-or-buffer.  Thx to Daniel Clemente.
;; 2008/12/02 dadams
;;     Removed all use of icicle-file-ignore-space-prefix-flag (removed).
;; 2008/11/23 dadams
;;     Updated wrt Emacs 23:
;;       find-file-confirm-nonexistent-file -> confirm-nonexistent-file-or-buffer.
;;       icicle-buffer*, icicle-add-buffer-candidate, icicle-choose-candidate-of-type:
;;         Added confirm-nonexistent-file-or-buffer.
;; 2008/11/22 dadams
;;     icicle-command-abbrev, icicle-remove-buffer-config-action, icicle-find-file-absolute*,
;;       icicle-(recent|locate)-file*, icicle-describe-option-of-type,
;;       icicle-(vardoc|fundoc|plist|doc), icicle-complete-keys-1, icicle-read-color:
;;         Put icicle-fancy-candidates property on prompt when appropriate.
;;     icicle-(find|recent|locate)-file(-absolute)(-other-window):
;;       Support find-file-confirm-nonexistent-file (Emacs 23).
;; 2008/11/17 dadams
;;     Added: icicle-find-first-tag-action, icicle-find-first-tag-other-window-action.
;;     icicle-find(-first)-tag(-other-window), icicle-find-tag-action: Crosshairs highlighting.
;;     icicle-find-first-tag(-other-window): Use icicle-find-first-tag(-other-window)-action.
;;     Mention crosshairs.el in doc strings of commands that use it.
;; 2008/11/14 dadams
;;     icicle-locate-file*: Use face icicle-candidate-part only for negative prefix arg.
;; 2008/11/09 dadams
;;     icicle-explore: When only 1 candidate, no completion.  Thx to Hannes Janetzek.
;; 2008/11/04 dadams
;;     Removed compile-time require of palette.el or eyedropper.el.  Thx to Andrey Zhdanov.
;;     icicle-read-color: Moved soft require of palette.el or eyedropper.el to beginning.
;;     Removed: icicle-generic-S-tab.
;;     icicle-add-key+cmd: Removed test for icicle-generic-S-tab.
;; 2008/11/03 dadams
;;     icicle-insert-for-yank: push-mark first.  Thx to Fabrice Knevez.
;;     Renamed: icicle-insert-kill to icicle-completing-yank,
;;              icicle-yank-insert to icicle-yank-maybe-completing.
;;     icicle-search: Restored binding of icicle-searching-p (somehow, accidentally removed it).
;; 2008/11/02 dadams
;;     icicle-(doc|vardoc|fundoc|plist):
;;       Prefix arg uses candidates cache.  Tighten scope of condition-case.
;;     icicle-doc:
;;       Show all of fn, var, and face doc that are available.  Thx to Fabrice Knevez.
;;       Use multi-completion: added symbol name and type.
;;     icicle-doc-action: (intern (icicle-transform-multi-completion...)), not assoc.
;; 2008/10/27 dadams
;;     icicle-find(-first)-tag*: Bind case-fold-search too.
;; 2008/10/26 dadams
;;     icicle-(file|directory)-list, icicle-find-file(-absolute)(-other-window),
;;       icicle-(locate|recent)-file(-other-window): Bind to icicle-file-* options.
;;     icicle-locate-file*, icicle-recent-file*:
;;       Bind icicle-abs-file-candidates, icicle-delete-candidate-object.
;;       Move init code into bindings.
;;     icicle-find-first-tag*: Move init code into bindings.
;; 2008/10/24 dadams
;;     Find-tag fixes.  Thx to Per Nordlow.
;;       icicle-find-tag: Bind completion-ignore-case also for call to icicle-explore.
;;       icicle-find-tag-define-candidates-1:
;;         Look for regexp followed by DEL, through eol.  Move to bol, then next line.
;;     icicle-explore: Call icicle-get-alist-candidate with NO-ERROR-P arg.
;; 2008/10/23 dadams
;;     icicle-find-tag: Use nil for REQUIRE-MATCH in call to icicle-explore.
;; 2008/10/21 dadams
;;     icicle-search-read-context-regexp: Bound icicle-candidate-(action|help)-fn to nil.
;;     icicle-search-define-candidates-1: Added error message - better than generic explore msg.
;;     Moved enable-recursive-minibuffers from *-search-region to *-search-region-action.
;;     icicle-search-region-action: Added message if no such file.
;; 2008/10/18 dadams
;;     Replaced customize-save-variable by funcall icicle-customize-save-variable-function.
;;     icicle-bookmark(-other-window): Use icicle-bookmark-cleanup.  Removed completion default.
;;     Added: icicle-bookmark-cleanup: Select orig-window at end.  Thx to Andrey Zhdanov.
;; 2008/10/17 dadams
;;     Added: icicle-bookmark-cmd.
;; 2008/10/16 dadams
;;     icicle-bookmark-jump-1: Fixed for Emacs 23.  Thx to Andrey Zhdanov.
;; 2008/10/12 dadams
;;     Added: icicle-search-ibuffer-marked, icicle-search-buff-menu-marked.
;;     icicle-search-dired-marked: Removed unused let binding.
;; 2008/10/11 dadams
;;     icicle-Info-index-20: Use symbol at point as default index topic.
;; 2008/10/05 dadams
;;     icicle-Info-read-node-name: Use lax completion.  Thx to Richard Kim.
;; 2008/09/22 dadams
;;     icicle-dired-project(-other-window):
;;       Map expand-file-name over absolute file names, to expand ~ in fileset entries.
;; 2008/09/20 dadams
;;     icicle-find-file-absolute(-other-window): abs-file-list -> icicle-abs-file-list.
;; 2008/09/19 dadams
;;     icicle-add-entry-to-saved-completion-set:
;;       Use only the fileset name, not the whole fileset, in the (:fileset...) entry.
;;     icicle-remove-entry-from-saved-completion-set: Rewrote.
;;       Either the entry to remove or the set to remove it from can now be a fileset.
;;     icicle-dired-save-marked-persistently: Added FILESETP arg, so you can save in a fileset.
;;     icicle-dired-project(-other-window): Include filesets as project candidates.
;;     Renamed: icicle-dired-save-marked-to-cache-file to icicle-dired-save-marked-persistently.
;;     Moved icicle-kill-a-buffer to icicles-fn.el.
;; 2008/09/15 dadams
;;     icicle-(add|remove)-candidate-(to|from)-saved-completion-set: Use :fileset keyword.
;;     icicle-search-where-arg, icicle-search-where-arg:
;;       Wrap icicle-(file|buffer)-list with save-selected-window.
;; 2008/09/14 dadams
;;     Renamed: icicle-(add|remove)-candidate-(to|from)-saved-completion-set to
;;              icicle-(add|remove)-entry-(to|from)-saved-completion-set.
;;     icicle-add-entry-to-saved-completion-set: Treat fileset entries, not just strings.
;;     icicle-(add|remove)-entry-(to|from)-saved-completion-set,
;;       icicle-remove-saved-completion-set, icicle-dired-project(-other-window):
;;         No default value for completing-read.
;; 2008/09/13 dadams
;;     Use renamings from icicles-mcmd.el:
;;       icicle-candidate-set-save-to-cache-file to icicle-candidate-set-save-persistently,
;;       icicle-candidate-set-retrieve-from-cache-file to *-candidate-set-retrieve-persistent.
;; 2008/09/11 dadams
;;     Added: icicle-grep-saved-file-candidates.
;; 2008/09/09 dadams
;;     Added: icicle-remove-saved-set-action: No longer set minibuffer-completion-table or call
;;                                            icicle-complete-again-update.
;;            icicle-dired-save-marked-(to-(variable|cache-file)|as-project),
;;            icicle-dired-project(-other-window).
;;     Renamed: icicle-candidate-set-dired-marked-save(-more) to icicle-dired-save-marked(-more)
;;     icicle-remove-saved-completion-set:
;;       Prompt and delete cache file.  Use icicle-remove-saved-set-action as action function.
;;       Bind: icicle-whole-candidate-as-text-prop-p, icicle-use-candidates-only-once-flag.
;;       Remove *Completions* window at end.
;; 2008/09/08 dadams
;;     icicle-dired-saved-file-candidates(-other-window):
;;       Don't change to relative file names - use whichever form (abs, rel) is saved.
;;     icicle-find-tag-define-candidates-1: Make sure tag is not empty before (aref tag 0).
;;     icicle-find-file-absolute(-other-window):
;;       Use default-directory as init value and default value.
;; 2008/09/03 dadams
;;     icicle-bookmark(-other-window), icicle-goto-marker-1:
;;       Use crosshairs-unhighlight, not icicle-unhighlight-crosshairs+cleanup.
;;     icicle-goto-marker-1: Bind orig-buff.
;;     *-bookmark-jump-1, *-goto-marker-1-action, *-compilation-search-in-context-fn:
;;       Use crosshairs-highlight, not icicle-highlight-crosshairs.
;; 2008/09/02 dadams
;;     Added: icicle-dabbrev--abbrev-at-point.
;;     icicle-dabbrev-completion: Use icicle-dabbrev--abbrev-at-point.  Thx to Tomer Levin.
;; 2008/09/01 dadams
;;     Added: icicle-bookmark-jump(-other-window), icicle-bookmark-jump-1.
;;     Removed: icicle-bookmark-other-window-action.
;;     icicle-bookmark(-other-window): Use icicle-bookmark-jump(-other-window) as action fn.
;;                                     Clean up crosshairs highlighting at end.
;;     icicle-goto-marker-1:
;;       Don't bind minibuffer-exit-hook.  Don't require crosshairs.el or hl-line+.el.
;;       Use icicle-unhighlight-crosshairs+cleanup for the cleanup.
;;     icicle-compilation-search: Don't require hl-line+.el.
;;     icicle-compilation-search-in-context-fn: Use icicle-highlight-crosshairs.
;; 2008/08/31 dadams
;;     icicle-goto-marker-1-action:
;;       Use col-highlight-(un)highlight, not crosshairs-flash.  Force redisplay.
;;       Add col-highlight-unhighlight to pre-command-hook.
;;       Use hl-line-highlight-now, not hl-line-flash.
;;     icicle-goto-marker-1: Remove col-highlight-unhighlight from pre-command-hook at end.
;;     icicle-customize-face(-other-window, icicle-execute-extended-command-1,
;;       icicle-find-tag-action, icicle-insert-thesaurus-entry, icicle-apply(-list)-action,
;;       icicle-search-action, icicle-tags-search, icicle-choose-anything-candidate:
;;         Select window before call select-frame-set-input-focus.
;; 2008/08/25 dadams
;;     icicle-find-file-absolute(-other-window), icicle-(recent|locate)-file(-other-window):
;;       Use lax completion and provide WILDCARDS arg to find-file.
;; 2008/08/24 dadams
;;     Use renaming from icicles-fn.el: icicle-minibuffer-contents to icicle-minibuf-input.
;; 2008/08/23 dadams
;;     icicle-clear-history: Wrap default value, minibuffer-history-variable, with symbol-name.
;; 2008/08/21 dadams
;;     Added: icicle-file(-other-*), icicle-find-file-absolute(-other-*).  Thx to Glauber M4.
;;     Soft-require bbdb-com.el at compile time.  Don't require bbdb.el at load time - define
;;       old-bbdb-* only if original is already defined.
;;     icicle-bbdb-complete-name: Raise error if BBDB not loaded.
;; 2008/08/20 dadams
;;     icicle-bbdb-complete-name, icicle-clear-current-history: Replaced ding by icicle-ding.
;; 2008/08/18 dadams
;;     icicle-clear-history:
;;       Moved icicle-use-candidates-only-once-flag binding to icicle-clear-history-1.
;;     icicle-delete-windows: Bind icicle-inhibit-try-switch-buffer.
;;     icicle-insert-kill: Bind icicle-sort-function to nil.
;;     icicle-recent-file(-other-window):
;;       Bind *-candidate-properties-alist and *-list-use-nth-parts to nil if no prefix arg.
;;     icicle-goto(-global)-marker:
;;       Bind icicle-sort-function(s-alist) differently in each - don't bind in *-goto-marker-1.
;;     icicle-remove-region: Use just icicle-delete-region-from-alist as action function.
;;     icicle-delete-region-from-alist:
;;       Use the propertized region name, not just reg-name in the cons to delete.
;;     Moved icicle-cdr-less-p to icicles-fn.el and renamed to icicle-cdr-lessp.
;;     Moved icicle-remove-candidate-display-others to icicles-mcmd.el.
;;     Use renamings from icicles-fn.el: icicle-complete-again-update, icicle-remove-if-not.
;;     icicle-explore, icicle-find-tag, icicle-complete-keys:
;;       Renamed local orig-* vars.  Use explore's in *-search-(quit-or-error|cleanup|action).
;; 2008/08/14 dadams
;;     icicle-apply: Handle multi-completion alist entries.
;;     icicle-goto-global-marker: Treat icicle-add-buffer-name-flag: Use multi-completions.
;;     icicle-marker+text: Added optional arg.  Annotate text with buffer name if appropriate.
;;     icicle-goto-marker-1: Pass global-ring-p to icicle-marker+text, so it can annotate.
;; 2008/08/13 dadams
;;     icicle-goto-marker-1: Don't add marker to ring markers if global ring.
;;     icicle-goto-marker-1-action: select-frame-set-input-focus.
;;     icicle-bbdb-complete-name: let -> let* (duh!) for completing-read.
;; 2008/08/12 dadams
;;     Added: icicle-goto(-global)-marker-or-(set-mark-command|pop-global-mark),
;;            icicle-goto-marker-1-action (factored out from icicle-goto-marker-1).
;;     icicle-goto-marker-1:
;;       Add mark-marker to list of markers in ring.
;;       Use icicle-goto-marker-1-action.
;;       Go directly to marker if only one.  Message if same as point.
;;       Better test for type of ring, for error msg.
;;     icicle-apply: Use icicle-explore-final-choice-full in final result (not void var).
;;                   Added optional arg NOMSG; bind icicle-apply-nomsg.
;;     icicle-apply-action: Respect icicle-apply-nomsg.  Removed vestigial cruft.
;;     icicle-bbdb-complete-name: Bind icicle-bbdb-complete-name to t and use strict completion.
;; 2008/08/11 dadams
;;     icicle-search-highlight-context-levels: Increase overlay priority for each context level.
;;     icicle-search-highlight-all-input-matches, icicle-search-highlight-input-matches-here:
;;       Made of current-input overlay higher than any context-level overlay: 204 -> 220.
;; 2008/08/10 dadams
;;     Added: icicle-explore, icicle-find-tag-define-candidates (renamed old to *-1),
;;            icicle-search-define-candidates (renamed old to *-1), icicle-search-cleanup,
;;            icicle-(find-tag|search)-(final-act|quit-or-error).
;;     Renamed: icicle-find-tag-define-candidates to icicle-find-tag-define-candidates-1,
;;              icicle-search-define-candidates to icicle-search-define-candidates-1.
;;     Redefined to use icicle-explore: icicle-find-tag, icicle-apply, icicle-search.
;;     icicle-comint-get-final-choice: Use icicle-explore-final-choice (not icicle-search-*).
;; 2008/08/08 dadams
;;     Added: icicle-goto-marker-1 (from icicle-goto-marker):
;;       Use unwind-protect and minibuffer-exit-hook to unhighlight crosshairs.
;;       Provide S-delete: bind icicle-delete-candidate-object.
;;       Require crosshairs.el, hl-line+.el.  Use crosshairs-flash.
;;     icicle-goto(-global)-marker: Use icicle-goto-marker-1.
;; 2008/08/06 dadams
;;     icicle-kmacro(-action):
;;       Better prefix arg treatment.
;;       kmacro-end-or-call-macro if still defining.
;;       No-exit require-match.
;;       Default value if only one.
;;       Minor bug fix (extra candidate).
;;       Name macros just 1, 2..., not macro #1, macro #2...
;; 2008/08/03 dadams
;;     Added: icicle-clear(-current)-history(-1|-entry), icicle-apply-list-action.
;;     icicle-apply: Bind icicle-all-candidates-list-action-fn to icicle-apply-list-action.
;;     Renamed: icicle-all-candidates-action-fn to icicle-all-candidates-list-action-fn,
;;              icicle-candidate-alternative-action-fn to icicle-candidate-alt-action-fn.
;; 2008/07/27 dadams
;;     icicle-recent-file*, icicle-locate-file*: Added date multi-completion, with prefix arg.
;;     Added: icicle-make-file+date-candidate.
;; 2008/07/23 dadams
;;     Renamed: icicle-map(-action) to icicle-apply(-action).  Added defalias for icicle-map.
;; 2008/07/21 dadams
;;     Added: icicle-bbdb-complete-name.  Thx to Ian Swainson.  require of bbdb.el for compile.
;; 2008/07/19 dadams
;;     icicle-Info-menu: Corrected regexp to pick up menu-item default, for Emacs 20.
;; 2008/07/15 dadams
;;     eval-when-compile require of: anything, cookie1, etags, eyedropper/palette, yow.
;;     icicle-customize-apropos-options-of-type: Typo: help-remove-duplicates -> icicle-*.
;;     icicle-send-bug-report: Include emacs-version.
;; 2008/05/16 dadams
;;     icicle-find-first-tag*, icicle-find-tag: Use tags-lazy-completion-table for Emacs 23.
;; 2008/04/25 dadams
;;     icicle-execute-extended-command-1:
;;       Add msg about binding if suggest-key-bindings.  Thx to Jonathan Arkell.
;; 2008/04/14 dadams
;;     icicle-pp-eval-expression: Treat negative prefix arg.
;; 2008/04/13 dadams
;;     icicle-pp-eval-expression:
;;       Treat prefix arg (added optional arg).
;;       Respect eval-expression-debug-on-error and icicle-pp-eval-*.
;; 2008/04/09 dadams
;;     icicle-apropos-(function|command), icicle-imenu-non-interactive-function-p,
;;       icicle-complete-keys-help: Use fboundp, not functionp, to also pick up macros.
;; 2008/04/04 dadams
;;     icicle-bookmark: Bind completion-ignore-case to bookmark-completion-ignore-case.
;;     Added: icicle-bookmark-other-window(-action).
;; 2008/04/01 dadams
;;     icicle-describe-option-of-type: Bind icicle-apropos-complete-match-fn to nil.
;;     icicle-describe-opt-of-type-complete:
;;       Moved icicle-current-completion-mode test out of icicle-delete-if-not (iterates).
;; 2008/03/31 dadams
;;     icicle-locate-file(-other-window), icicle-plist, icicle-describe-option-of-type,
;;       icicle-(var|fun)doc: icicle-highlight-lighter, before gather candidates.
;;     icicle-plist, icicle-describe-option-of-type, icicle-vardoc, icicle-fundoc:
;;       Simplified doc string and prompt wrt C-M-j.
;; 2008/03/30 dadams
;;     icicle-Info-menu: Treat also pseudo-menu at Top.  Thx to William Xu.
;; 2008/03/26 dadams
;;     Added: icicle-Info-menu(-cmd).
;; 2008/03/22 dadams
;;     icicle-other-window-or-frame: Rewrote to use icicle-select-window for all frames or one.
;;     icicle-select-window:
;;       Rewrote: Similar to icicle-select-frame (unique window names).  All frames or one.
;;     Added: icicle-select-window-by-name, icicle-make-window-alist.
;; 2008/03/19 dadams
;;     Added: icicle-pp-eval-expression (old version in icicles-mcmd.el was renamed).
;; 2008/03/11 dadams
;;     icicle-repeat-complex-command: Rewrote doc string.
;; 2008/03/10 dadams
;;     icicle-select-frame: Rewrote to use icicle-select-frame-by-name (unique frame names).
;;     icicle-choose-candidate-of-type: Use icicle-make-frame-alist, not make-frame-alist.
;;     Added: icicle-select-frame-by-name, icicle-make-frame-alist.
;; 2008/03/09 dadams
;;     icicle-dabbrev-completion, icicle-find-tag-define-candidates,
;;       icicle-Info-build-node-completions, icicle-search, icicle-read-color:
;;         Call icicle-highlight-lighter.
;; 2008/03/05 dadams
;;     icicle-map: Don't inhibit sorting.
;;     icicle-goto(-global)-marker: Provide sorting choices.  Highlight target line.
;;     Added: icicle-cdr-less-p.
;; 2008/02/26 dadams
;;     Added: icicle-where-is.
;; 2008/02/23 dadams
;;     icicle-lisp-complete-symbol:
;;       Wrap completing-read in save-excursion, because of icicle-top-level-*-completion-flag.
;;     icicle-search: Use NOWARN arg with find-file-noselect.
;;     Added: icicle-tags-search.
;;     Renamed: icicle-search-tag* to icicle-find-tag*,
;;              icicle-find-tag(-other-window) to icicle-find-first-tag(-other-window).
;; 2008/02/21 dadams
;;     icicle-Info-goto-node: Added .. node for book order.
;;       Use global var icicle-Info-only-rest-of-book-p.  Call icicle-Info-goto-node-1.
;;     icicle-Info-read-node-name: Remove REST-P arg.
;;     icicle-Info-goto-node-action: When rest-of-book-p:
;;       Update node completion table and then completions, and set current candidate number. 
;;     Added: icicle-Info-goto-node-1, icicle-Info-build-node-completions(-fix-*).
;; 2008/02/17 dadams
;;     icicle-Info-goto-node:
;;       Moved let to interactive spec, and added two args.  Use icicle-Info-read-node-name.
;;       Negative prefix arg means completions are limited to rest of book.
;;       Add icicle-Info-book-order-p to icicle-sort-functions-alist.
;;       New doc string, to explain everything.
;;     Added: icicle-Info-book-order-p, icicle-Info-read-node-name.
;; 2008/02/16 dadams
;;     icicle-(select|remove|search)-region:
;;       Removed extraneous (setq icicle-candidates-alist) in let binding.
;; 2008/02/13 dadams
;;     Added: icicle-search-tag, icicle-search-tag-define-candidates, icicle-search-tag-action,
;;       icicle-search-tag-help, icicle-pop-tag-mark.
;;     Updated doc string of icicle-find-tag(-other-window).
;; 2008/02/08 dadams
;;     icicle-compilation-search-in-context-fn:
;;       Simplified and corrected.  Dropped let, with-current-buffer.  Narrow to whole line.
;;       Different code for Emacs 22+: Set current error; call compilation-next-error-function.
;;     Removed: icicle-compilation-search-hl-line.
;;     icicle-compilation-search: Soft require hl-line+.
;;                                Don't bind next-error-hook to icicle-*-search-hl-line.
;;     Renamed: icicle-directories-list to icicle-directory-list.
;; 2008/02/07 dadams
;;     icicle-search-action:
;;       Moved icicle-highlight-candidate-* to after icicle-search-in-context-fn call.
;;     icicle-compilation-search-in-context-fn:
;;       Use buffer of marker.  Removed code using overlay buffer.
;;     icicle-compilation-search: Bound icicle-compilation-search-hl-line as next-error-hook.
;;     Added: icicle-compilation-search-hl-line.
;; 2008/02/03 dadams
;;     icicle-file-list: Rewrote using icicle-define-file-command.
;;     icicle-(re)set-option-*: Added boundp before user-variable-p and icicle-binary-option-p.
;;     Added: icicle-directories-list.
;; 2008/01/18 dadams
;;     Moved icicle-complete-keys-alist to icicles-var.el
;; 2008/01/13 dadams
;;     icicle-customize-face(-other-window), icicle-face-list, icicle-read-color:
;;       Use icicle-transform-multi-completion.
;;     icicle-(vardoc|fundoc): Use icicle-funvardoc-action as action fn and as help fn.
;;     icicle-doc: Use icicle-doc-action as action fn and as help fn.
;;     Added: icicle-funvardoc-action, icicle-doc-action.
;;     icicle-search-replace-search-hit: Simplified bind/restore of Icicles completion vars.
;; 2008/01/11 dadams
;;     icicle-search-highlight-and-maybe-replace:
;;       Don't call icicle-update-completions or icicle-display-candidates-in-Completions here.
;;     icicle-search-in-context-default-fn, icicle-compilation-search-in-context-fn:
;;       Call icicle-update-completions (at the end), since not done in i*-and-maybe-replace.
;; 2008/01/08 dadams
;;     icicle-describe-opt-of-type-complete: Treat 3rd arg.  Treat prefix completion.
;; 2008/01/06 dadams
;;     icicle-read-color: Provide color help for C-M-.  Added: icicle-color-help.
;; 2008/01/04 dadams
;;     icicle-doc, icicle-fundoc, icicle-vardoc: Use history variable icicle-doc-history.
;; 2008/01/02 dadams
;;     Added: icicle-search-dired-marked.
;; 2008/01/01 dadams
;;     icicle-(buffer|face|file)-list: Reverse list, so C-! keeps order.
;;     icicle-dired-saved-file-candidates(-other-window):
;;       Convert candidates to relative file names before opening Dired using them.
;;       Error if any candidate is not a file in directory.
;;     Added: icicle-candidate-set-dired-marked-save(-more).
;; 2007/12/31 dadams
;;     Added: icicle-fn-doc-minus-sig.
;;     icicle-fundoc, icicle-doc: Use icicle-fn-doc-minus-sig.
;;     icicle-doc: Bind icicle-transform-function, not icicle-whole-candidate-as-text-prop-p.
;; 2007/12/27 dadams
;;     icicle-describe-option-of-type:
;;       Bind icicle-apropos-complete-match-fn to nil to prevent automatic input match.
;; 2007/12/26 dadams
;;     icicle-describe-opt-of-type-complete: Pretty-print types.  Bind icicle-candidate-help-fn.
;;     Added: icicle-describe-opt-action.
;; 2007/12/24 dadams
;;     icicle-describe-option-of-type:
;;       Rewrote for different kinds of type matching.  TYPE can be a regexp or type sexp.
;;         Diff prefix args for diff behaviors. Handle type inheritance and value-checking.
;;     Added: icicle-describe-opt-of-type-complete.
;; 2007/12/21 dadams
;;     icicle-customize-apropos-options-of-type: help-var-is-* -> icicle-var-is-*.
;; 2007/12/20 dadams
;;     icicle-dired-saved-file-candidates(-*):
;;       Use substitute-command-keys in error msg.  Use generate-new-buffer name, not y-or-n-p.
;; 2007/12/15 dadams
;;     Added: icicle-customize-apropos-options-of-type.
;; 2007/12/11 dadams
;;     icicle-read-color: Don't leave out variable proxies if don't have eyedropper.
;; 2007/12/07 dadams
;;     Added: icicle-describe-option-of-type.
;;     icicle-doc:
;;       Choose kind of doc, instead of showing all (overwriting).
;;       Removed binding of icicle-candidate-properties-alist.
;;     icicle-read-color: Single-quote proxies, don't wrap with `_'.
;; 2007/12/06 dadams
;;     icicle-doc: Forgot to include face doc.
;; 2007/12/03 dadams
;;     Renamed icicle-longest-common-match to icicle-expanded-common-match.
;; 2007/12/02 dadams
;;     icicle-read-color: Include color-valued variables as proxy candidates.
;; 2007/11/30 dadams
;;     icicle-command-abbrev:
;;       Use only membership in icicle-proxy-candidates, not icicle-proxy-candidate property.
;;       So predicate is just commandp, since proxies are not part of regular candidates.
;;     icicle-command-abbrev-action:
;;       Bind to save/restore: icicle-add-proxy-candidates-flag, icicle-proxy-candidates.
;;     icicle-command-abbrev-command: Bind icicle-add-proxy-candidates-flag to nil, to reset.
;; 2007/11/29 dadams
;;     icicle-command-abbrev: Treat icicle-add-proxy-candidates-flag, icicle-proxy-candidates.
;;     icicle-read-color: Don't test icicle-add-proxy-candidates-flag.
;; 2007/11/27 dadams
;;     icicle-command-abbrev: Remove icicle-proxy-candidate property in unwind-protect.
;; 2007/11/26 dadams
;;     icicle-get-anything-candidates: Fixed for try-completion and test-completion cases.
;;     icicle-choose-anything-candidate: Bind icicle-candidates-alist to actions (two places).
;; 2007/11/25 dadams
;;     Added: icicle-command-abbrev(-action|-command|-matching-commands|-record|regexp).
;; 2007/11/24 dadams
;;     icicle-execute-extended-command: Bind use-file-dialog to nil.
;; 2007/11/17 dadams
;;     icicle-read-color:
;;       Respect icicle-add-proxy-candidates-flag.  Convert proxy multi-completions to strings.
;; 2007/11/03 dadams
;;     icicle-generic-S-tab: Doc string - mention icicle-generic-S-tab-keys.
;; 2007/10/28 dadams
;;     icicle-search: Don't bind icicle-expand-input-to-common-match-flag.  Updated doc string.
;;     icicle-search-highlight-all-input-matches:
;;       Don't set icicle-search-lcm unless icicle-expand-input-to-common-match-flag.
;;     icicle-search-highlight-and-maybe-replace:
;;       Match the lcm if icicle-search-replace-common-match-flag is non-nil.
;;       Save icicle-candidate-nb around icicle-update-completions.
;;       Set icicle-candidate-nb to 0 if nil.
;;     icicle-search-highlight-input-matches-here:
;;       Don't delete icicle-search-refined-overlays if icicle-*-all-current-flag.
;; 2007/10/26 dadams
;;     icicle-search-highlight-all-input-matches: Loop through overlays twice, to get the lcm.
;;     icicle-search-highlight-input-matches-here: Highlight the lcm, if available.
;;     icicle-search: Bind icicle-search-lcm.
;; 2007/10/22 dadams
;;    icicle-read-color: Use special-candidate face for pseudo color candidates.
;;    icicle-add-key+cmd: Don't include keys bound to undefined.
;; 2007/10/20 dadams
;;    icicle-read-color, icicle-remove-color-duplicates:
;;      Treat pseudo colors too (e.g. *point foreground*).
;;    icicle-make-color-candidate: Added optional HEX-RGB arg.
;; 2007/10/16 dadams
;;     icicle-vardoc: Prefix arg means use only user options.
;; 2007/10/14 dadams
;;     Updated doc strings to reflect new icicle-act-before-cycle-flag change.
;; 2007/10/13 dadams
;;     icicle-get-anything-candidates:
;;       If candidates is a fn, return fn that's useful for all-completions.  Filter by input.
;;     icicle-choose-anything-candidate:
;;       Don't sort or transform cands.  Respect Anything's require-pattern and delayed
;;       settings.  Bind icicle-candidates-alist to candidates, if that's a function.
;;     icicle-get-anything-candidates-of-type: Don't use mapcar if candidates is a function.
;;     Added: icicle-get-anything-req-pat-chars, icicle-get-anything-input-delay.
;; 2007/10/06 dadams
;;     icicle-choose-candidate-of-type: Added entry for file type.
;; 2007/09/25 dadams
;;     icicle-doc: Bind icicle-whole-candidate-as-text-prop-p to treat full candidates.
;; 2007/09/04 dadams
;;     icicle-read-color: Added optional PROMPT arg.
;; 2007/08/31 dadams
;;     icicle-buffer-list: Prefix arg means only buffers visiting files are candidates.
;;     icicle-search-where-arg, icicle-occur:
;;       Prefix arg 99 means only buffers visiting files are candidates.
;;     Added: icicle-search-choose-buffers.
;; 2007/08/27 dadams
;;     icicle-search-action: Fixed for return value.  Display the error message.
;; 2007/08/25 dadams
;;     icicle-choose-candidate-of-type: Removed (> emacs-major-version 21) restriction.
;; 2007/08/25 dadams
;;     Added: a, any, buffer, file, icicle-get-anything-(types|(default-)actions-for-type|
;;            candidates-of-type|(cached-)candidates), icicle-anything(-candidate-value),
;;            what-which-how, icicle-choose-anything-candidate.
;;     Renamed icicle-clear-option to clear-option.
;;     icicle-object-action: Treat Anything stuff.
;;       Added optional type arg.  Use vars icicle-object-(named|predicate)-types.
;;       Move icicle-read-var-value-satisfying call here from icicle-choose-candidate-of-type.
;;     icicle-choose-candidate-of-type: Create buffer if needed.
;;     Protected alias definitions with icicle-define-alias-commands-flag.
;;     icicle-map(-action):
;;       Use icicle-whole-candidate-as-text-prop-p and new icicle-get-alist-candidate.
;;     icicle-(select|remove|search)-region, icicle-search:
;;       Bind/use icicle-whole-candidate-as-text-prop-p.
;;     icicle-occur: Require buffer names to match existing buffers.  Thx to Hadron Quark.
;; 2007/08/15 dadams
;;     Added command toggle as alias for icicle-toggle-option.
;; 2007/08/09 dadams
;;     Soft require kmacro.  Define icicle-kmacro* if kmacro gets loaded.
;; 2007/07/27 dadams
;;     icicle-search: Renamed icicle-act-first-then-navigate-p to icicle-act-before-cycle-flag.
;; 2007/07/22 dadams
;;     icicle-customize-face: Added prefix arg to give vanilla completion.
;;     Added: icicle-customize-face-other-window.
;;     Replace standard customize-face(-other-window) by icicle-customize-face(-other-window).
;;     No longer require icicles-mode.el.
;;     Require cus-edit.el (not just at compile time).
;; 2007/07/08 dadams
;;     Added: icicle-customize-faces.
;;     icicle-customize-face: Bind icicle-all-candidates-action-fn to icicle-customize-faces.
;; 2007/06/23 dadams
;;     icicle-search-read-context-regexp, icicle-search-read-word, icicle-search-property-args,
;;      icicle-add-region, icicle-save-string-to-variable:
;;        Use icicle-completing-read-history, not read-from-minibuffer or read-string.
;;     icicle-face-list: Bound icicle-list-nth-parts-join-string etc.
;;     Moved to icicles-fn.el: icicle-read-from-minibuf-nil-default.
;; 2007/06/22 dadams
;;     Added: icicle-group-regexp, icicle-keyword-list, icicle-search-keywords.
;; 2007/06/21 dadams
;;     icicle-plist, icicle-(var|fun)doc, icicle-region-add-buffers, icicle-search-regexp-scan,
;;      icicle-search-char-property-scan:
;;        Use face icicle-candidate-part, not icicle-special-candidate.
;;     icicle-add-key+cmd: Highlight key side of the pair (key  =  binding).
;; 2007/06/18 dadams
;;     Added: icicle-customize-face.
;; 2007/06/17 dadams
;;     icicle-make-color-candidate: Respect icicle-WYSIWYG-Completions-flag.
;;     icicle-search-action: Added priority in call to icicle-place-overlay.
;; 2007/06/12 dadams
;;     icicle-execute-extended-command(-1):
;;       i-e-e-c-1 prepares the new last command, but i-e-e-c sets it, at end, to this-command.
;; 2007/06/10 dadams
;;     Added: icicle-comint-hook-fn, icicle-compilation-hook-fn.
;;     Removed unconditional add-hooks for comint-mode-hook and compilation(minor)-mode-hook.
;; 2007/06/09 dadams
;;     icicle-set-option-to-t, icicle-reset-option-to-nil, icicle-delete-windows,
;;      icicle-add-buffer-candidate, icicle-remove-buffer-candidate, icicle-buffer-list,
;;      icicle-remove-buffer-config, icicle-face-list, icicle-file-list,
;;      icicle-remove-all-regions-in-buffer, icicle-delete-file:
;;        Bind icicle-use-candidates-only-once-flag to t.
;;     icicle-set-option-to-t, icicle-clear-option: Candidate vars must have value nil/non-nil.
;;     icicle-search-regexp-scan: Added eobp test and empty hit-string test.
;;     icicle-select-(frame|window), icicle-delete-windows:
;;       Removed extra t arg to icicle-define-command.
;; 2007/06/08 dadams
;;     Added icicle-find-tag(-other-window).
;; 2007/06/07 dadams
;;     Renamed: icicle-font-history to icicle-font-name-history,
;;              icicle-function-history to icicle-function-name-history,
;;              icicle-variable-history to  icicle-variable-name-history.
;;     Use standard history variable if bound, else use Icicles history variable:
;;       bookmark-history, color-history, color-theme-history, face-name-history,
;;       font-name-history, frame-name-history, function-name-history, variable-name-history
;; 2007/06/05 dadams
;;     Don't require hexrgb.el if no window system.
;;     icicle-read-color: Use featurep, not require, for hexrgb.
;;     icicle-make-color-candidate: Error if hexrgb is not loaded.
;; 2007/05/31 dadams
;;     icicle-execute-extended-command-1: Set, don't bind this-command.
;;     icicle-execute-extended-command, icicle-execute-named-keyboard-macro:
;;       Simpler save and restore of last-command.
;; 2007/05/28 dadams
;;     icicle-imenu: Use same prefix-arg behavior as icicle-search (search multiple...).
;;     Added: icicle-imenu-(command|non-interactive-function(-p)).
;;     icicle-search-highlight-context-levels:
;;       Wrapped loop in condition-case.  Took max-levels determination out of loop.
;; 2007/05/25 dadams
;;     Added: icicle-face-list, icicle-next-single-char-property-change,
;;            icicle-search-(where-arg|property-args|char-property(-scan)|overlay-property),
;;            icicle-char-properties-in-buffer(s).
;;     Removed: icicle-text-properties-in-buffer(s), icicle-search-text-property-scan.
;;     icicle-search(-word): Use icicle-search-where-arg.
;; 2007/05/21 dadams
;;     Added: icicle-search-(buffer|file|all-regions).
;;     icicle-imenu: Fixed treatment of prefix arg.
;;     icicle-imenu-command-p: Fix to work also with Emacs 22 and  21.
;; 2007/05/14 dadams
;;     icicle-execute-extended-command-1: Error if empty command name.
;; 2007/05/12 dadams
;;     icicle-imenu: Added prefix arg for search for commands and non-cmds in Emacs-Lisp mode.
;;     Added: icicle-imenu-command-p.
;; 2007/05/11 dadams
;;     icicle-search-define-candidates:
;;       Move scan-fn-or-regexp to 4th arg from 2nd.  Removed &optional.
;;       Apply icicle-search-regexp-scan to args also.
;;     icicle-search-regexp-scan: Treat predicate.
;;       Move regexp to 3rd arg from 2nd, and add predicate arg.  Removed &optional.
;;     icicle-search-regexp-scan, icicle-search-text-property-scan:
;;       Add lengths of temp-list and icicle-candidates-alist.
;;     icicle-search-text-property(-scan): Added predicate argument.
;;     icicle-search-region-action: Bind icicle-candidate-action-fn to icicle-search-action.
;;     icicle-(select|search|remove)-region: Removed temp var bound to (icicle-region-sorted).
;;     icicle-search: Mention predicate in no-match message.
;; 2007/05/07 dadams
;;     icicle-add-buffer-config: Added history args for icicle-read-from-minibuf-nil-default.
;; 2007/05/06 dadams
;;     icicle-search: Bind icicle-must-pass-predicate to icicle-search-context-match-predicate.
;;     icicle-search-read-context-regexp: Don't accept an empty regexp.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/04 dadams
;;     icicle-(re)set-option-to-(nil|t), icicle-toggle-option: Enable recursive minibuffers.
;; 2007/05/02 dadams
;;     icicle-search-read-word: Updated doc string.
;;     icicle-search: Respect icicle-search-whole-word-flag and icicle-regexp-quote-flag.
;;                    Single message for non-existent buffers.
;;     icicle-select-region-action: Message for non-existent file.
;;     Added empty defvars for Emacs 22 standard vars, to quiet byte compiler.
;; 2007/05/01 dadams
;;     icicle-execute-extended-command-1:
;;       Don't bind icicle-candidate-action-fn if we have already read command name.
;;     icicle-search-*-scan: Fill, reverse temp list, then append to icicle-candidates-alist.
;;     Added: icicle-search-word, icicle-search-read-word.
;; 2007/04/29 dadams
;;     icicle-search, icicle-search-text-property, icicle-text-properties-in-buffers:
;;       Allow search of multiple files.  Change prefix arg accordingly.
;; 2007/04/28 dadams
;;     Added: icicle-compilation-search-in-context-fn, icicle-search-in-context-default-fn,
;;            icicle-search-highlight-and-maybe-replace.
;;     icicle-search-action: Move code to the new functions.  Use icicle-search-in-context-fn.
;;     icicle-compilation-search: Rewrote.
;;       No longer use compile-goto-error as icicle-search-hook.
;;       Use icicle-compilation-search-in-context-fn.
;;       Cannot-replace error msg if not Emacs 22, since no compilation-highlight-overlay.
;;       Provide .* regexp and do not highlight - as in icicle-occur.
;; 2007/04/22 dadams
;;     icicle-search: Allow for pre-bound icicle-candidate(-alternative)-action-fn.
;;     icicle-search-replace-search-hit:
;;       Call icicle-candidate-action-fn, not necessarily icicle-search-action.
;; 2007/04/20 dadams
;;     Added: icicle-search-highlight-context-levels.  Use in icicle-search-action.
;;     icicle-search-highlight-cleanup: Delete icicle-search-level-overlays.
;;     icicle-search: Set icicle-search-context-regexp nil if scan-fn-or-regexp is not string.
;;     icicle-search-replace-fixed-case-p: Return nil if arg is nil.
;;     icicle-search-read-context-regexp: Use default with read-number; protect with fboundp.
;;     Added soft require of strings.el.
;; 2007/04/18 dadams
;;     Added: icicle-search-replace-fixed-case-p.  Use in icicle-search-action.
;;     icicle-search-action:
;;       Set match data to region when icicle-search-replace-whole-candidate-flag is t.
;;       Don't search for icicle-search-context-regexp.
;;     icicle-search-replace-match: Added fixedcase arg.
;;                                  Use icicle-search-replace-literally-flag.
;;     Use replace-match-maybe-edit (Emacs 22), not replace-match, and save and restore stuff.
;;     icicle-search-replace-search-hit:
;;       Use regexp-history in read-string.  Use icicle-search-define-replacement.
;; 2007/04/17 dadams
;;     Added: icicle-search-replace-match.  Treat Emacs<22 also.  Use in icicle-search-action.
;;     icicle-search-replace-search-hit: Use regexp-history in read-string.
;; 2007/04/16 dadams
;;     icicle-search-action: Use replace-count.
;;     icicle-search: Initialize replace-count to 0.
;; 2007/04/15 dadams
;;     icicle-search-action: Allow \,... etc. replacement.
;;       Use query-replace-compile-replacement and replace-match, with error treatment.
;;       Removed unwind-protect.  Removed %s from error-msg return.
;;     icicle-search: Save search string as icicle-search-context-regexp.
;;                    Use "%s" for error message in failure error call.
;;                    Updated doc string for lisp-eval replacement etc.
;;     *(-all)-input-matches(-here): save-match-data around input matching.
;;     icicle-search-highlight-input-matches-here: Added not eobp to loop test.
;; 2007/04/13 dadams
;;     icicle-search-highlight-input-matches-here: Bound free var ov.
;; 2007/04/10 dadams
;;     icicle-search-regexp-scan:
;;       Use match indicated by icicle-search-context-level as context.
;;     Added: icicle-search-read-context-regexp.
;;     icicle-search(-region|-regexp-scan): Use icicle-search-read-context-regexp.
;; 2007/04/08 dadams
;;     icicle-map-action, icicle-search-action:
;;       Return nil for success, error msg for failure (it was reversed).
;;       Use icicle-highlight-candidate-in-Completions.
;;     icicle-map-action: Moved minibuffer frame selection to unwind-protect final clause.
;;     icicle-search: Give focus to original frame, in unwinding clause (C-g).
;;     Added: icicle-search-highlight-input-matches-here.
;;     icicle-search-highlight-all-input-matches: Highlight all input matches in candidate.
;;     icicle-search-replace-search-hit:
;;       Use 0 if icicle-candidate-nb is nil.  Display completions.
;;       Save and restore: icicle-last-completion-command, icicle-last-input.
;;       Prevent using same string as candidate for replacement.
;;     icicle-search-action: Rewrote.  Enable repeated replacment of input matches.
;;       Save and restore icicle-candidate-nb.  Wrap it around if at end.
;;       Warn about empty input for whole-candidate replacement.
;;       Update icicle-last-completion-candidate.  Display completions.
;; 2007/04/07 dadams
;;     icicle-search: Added query-replace functionality.
;;       Navigate first, then act (C-next).
;;       Give focus to last frame searched.
;;       Bind: icicle-candidate-alternative-action-fn, icicle-inhibit-sort-p,
;;             icicle-searching-p, icicle-expand-input-to-common-match-flag.
;;       Bind icicle-buffer-require-match-flag to partial-match-ok in interactive spec.
;;     icicle-search-action:
;;       Added optional replace arg: if non-nil, then fn becomes a replace action.
;;     Added: icicle-search-replace-search-hit, icicle-search-replace-candidate,
;;     icicle-buffer-list: Bind all the stuff that is bound in icicle-buffer.
;; 2007/04/06 dadams
;;     icicle-execute-extended-command-1:
;;       Bind this-command, but only around call to cmd - don't set it.
;; 2007/04/02 dadams
;;     Added: icicle-search-text-property, icicle-search-text-property-scan,
;;            icicle-text-properties-in-buffer(s).
;;     icicle-search:
;;       Added &rest args.  Updated doc string.  Don't read regexp or reverse alist here.
;;       Use icicle-search-define-candidates, not icicle-search-regexp-scan.
;;     icicle-search-regexp-scan: Read regexp here.  Reverse alist here.
;;     Moved to icicles-fn.el: icicle-filter-alist, icicle-first-matching-candidate.
;;     Renamed: icicle-search-region-beg-end to icicle-region-or-buffer-limits.
;; 2007/04/01 dadams
;;     icicle-repeat-complex-command:
;;       Remove duplicates and sort entries, but only for reading command to eval.
;; 2007/03/31 dadams
;;     icicle-lisp-complete-symbol: Bind icicle-top-level-when-sole-completion-flag to t.
;; 2007/03/27 dadams
;;     icicle-search: Hard-code C-next key in message.
;;     icicle-search-regexp-scan: Initialize last-beg for first time.
;; 2007/03/23 dadams
;;     icicle-execute-extended-command-1:
;;       Don't restore last-command.  Run (pre|post)-command-hook.  Set, don't bind,
;;       this-command.  enable-recursive-minibuffers for interactive call of cmd.
;;     icicle-execute-extended-command, icicle-execute-named-keyboard-macro:
;;       Restore last-command at end.
;; 2007/03/20 dadams
;;     icicle-execute-extended-command-1: When wrong-number-of-args, icicle-help-on-candidate.
;; 2007/03/14 dadams
;;     icicle-dired-saved-file-candidates, icicle-buffer, icicle-(find|locate|recent)-file:
;;       Put 200 as value of property icicle-Completions-window-max-height.
;;     Added ;;;###autoload for icicle-define*.
;; 2007/03/09 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region, icicle-imenu,
;;     icicle-occur, icicle-compilation-search:
;;       Bound icicle-transform-function to nil if interactive-p.
;;     icicle-comint-search: Updated doc string to mention false positives in command matching.
;;     Removed eval-when-compile from requires of Icicles libraries (except icicle-mac.el).
;; 2007/03/08 dadams
;;     icicle-insert-kill: Bound icicle-delete-candidate-object.
;; 2007/03/07 dadams
;;     icicle-delete-windows, icicle-map-action, icicle-search-action,
;;     icicle-choose-candidate-of-type, icicle-complete-keys-help:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/06 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region:
;;       Bind icicle-inhibit-sort-p to non-nil to prevent user sorting.
;;       Update doc string to mention that you cannot sort candidates.
;;     icicle-(remove|search|select)-region: Sort candidates by buffer and then by tag.
;;     icicle-search-region: Bound icicle-delete-candidate-object.
;;     icicle-search, icicle-map: Don't add or subtract one from candidate # if use action fn.
;;     icicle-search:
;;       If require-match, set icicle-completion-candidates and marker to reflect final choice.
;;     Renamed: icicle-get-current-candidate to icicle-get-alist-candidate.
;;     Added: icicle-region-sorted, icicle-region-add-buffers.
;; 2007/03/04 dadams
;;     icicle-get-current-candidate: Return nil if icicle-candidates-alist is nil.
;; 2007/03/02 dadams
;;     icicle-remove-buffer-config, icicle-remove-buffer-candidate: Removed extraneous quote.
;;     icicle-(find|recent)-file(-other-window): Bound icicle-delete-candidate-object.
;; 2007/02/28 dadams
;;     icicle-buffer-list, icicle-color-theme: Bound icicle-delete-candidate-object.
;; 2007/02/27 dadams
;;     icicle-recent-file(-other-window): Changed INITIAL-INPUT completing-read arg to nil.
;; 2007/02/25 dadams
;;     Added: icicle-remove-buffer-candidate-action, icicle-remove-buffer-config-action.
;;     icicle-add-buffer-candidate, icicle-bookmark, icicle-buffer-config:
;;       Bound icicle-delete-candidate-object.
;; 2007/02/24 dadams
;;     Added: icicle-kill-a-buffer-and-update-completions (was kill-a-buffer),
;;            icicle-delete-region-from-alist.
;;     icicle-delete-window: Use icicle-kill-a-buffer...-completions, not icicle-kill-a-buffer.
;;     icicle-buffer(-other-window), icicle-choose-candidate-of-type:
;;       Bind icicle-delete-candidate-object to icicle-kill-a-buffer.
;;       Bind icicle-sort-function to self if icicle-buffer-sort-function is nil.
;;     icicle-select-region:
;;       Bind icicle-delete-candidate-object to icicle-delete-region-from-alist.
;;     icicle-remove-region: Rewrote.
;;       Use icicle-delete-region-from-alist.
;;       Use icicle-remove-candidate-display-others, but don't redisplay unless completing.
;;       Respect icicle-add-buffer-name-flag (append buffer names).
;; 2007/02/20 dadams
;;     Added: icicle-search-region-action.  Open file associated with candidate.
;;     icicle-search-region:
;;       Use icicle-search-region-action.  Updated doc string.  Bind icicle-list-*.
;;       Fix default completing-read value.  Respect icicle-add-buffer-name-flag.
;;     icicle-select-region-action: Open file associated with candidate.
;;     icicle-region-open-all-files: Ignore files that are not readable.
;;     icicle-regions: Remove only non-existent buffers that are not associated with files.
;; 2007/02/19 dadams
;;     Added: icicle-region-open-all-files.
;;     icicle-(search|select)-region, icicle-search: Use icicle-region-open-all-files.
;;     icicle-add-region: Add file name to region entry in alist.
;;     icicle-select-region-action, icicle-region-help, icicle-search:
;;       Updated selectors for point and mark, because of addition of file name.
;;     icicle-region-help: Add file name to help message.
;; 2007/02/18 dadams
;;     Added: icicle-first-matching-candidate.
;;     icicle-get-current-candidate: Use icicle-first-matching-candidate, not assoc.
;; 2007/02/07 dadams
;;     icicle-search-action: Make *Completions* window dedicated.  Thx to Peter Povinec.
;; 2007/02/07 dadams
;;     icicle-search: pop-to-buffer and raise frame. Don't select orig-window.
;; 2007/02/06 dadams
;;     icicle-search, icicle-select-region, icicle-search-regexp-scan:
;;       Respect icicle-add-buffer-name-flag.
;;     icicle-search: Bound icicle-show-Completions-initially-flag to t.
;;                    Bound icicle-candidate-help-fn to icicle-search-help.
;;     icicle-search-regexp-scan: nil BUFFER arg means current buffer now.
;;     icicle-search-action, icicle-filter-alist: Treat multi-completion case also.
;;     Added: icicle-search-help.
;;     icicle-map-action, icicle-search-action: Removed unused local var, curr-cand-string.
;;     icicle-select-region, icicle-remove-region, icicle-search-region:
;;       Use icicle-candidates-alist, not regions-w-buffers (but bind it locally too).
;;     Renamed: icicle-get-current-region-candidate to icicle-get-current-candidate.
;;     icicle-region-help: Provide region limits also.
;;     Added note to some doc strings that the command is for Icicle mode only.
;; 2007/02/02 dadams
;;     icicle-search: Test for buffers, not buffer names, in WHERE.  Thx to Peter Povinec.
;;     icicle-buffer-list: Return the list of buffers.
;; 2007/01/28 dadams
;;     icicle-complete-keys:
;;       Bind icicle-sort-functions-alist, using icicle-command-names-alphabetic-p.
;; 2007/01/22 dadams
;;     icicle-read-color:
;;       Removed free var NTH-PARTS.
;;       Bound icicle-list-nth-parts-join-string the same as icicle-list-join-string.
;;       Remove duplicates: AliceBlue and alice blue etc.: downcase and remove whitespace.
;;     Added: icicle-remove-color-duplicates.
;;     Added soft require of hexrgb.el.
;;     icicle-*doc, icicle-plist: Bound icicle-candidate-properties-alist.
;; 2007/01/21 dadams
;;     Added: icicle-read-color, icicle-make-color-candidate.
;; 2007/01/18 dadams
;;     Added: icicle-get-current-region-candidate.
;;       Use current cand if cycled or `mouse-2'.  Else if single match, use that.  Else error.
;;     icicle-remove-region, icicle-search-region, icicle-region-help:
;;       Use icicle-get-current-region-candidate.
;;     icicle-add-region: Added optional TAG arg and prefix arg treatment.
;;     icicle-remove-region: Update completions list.  Bind regions-w-buffers.
;;     icicle-remove-all-regions-in-buffer: Use current buffer name as default.
;;     Added: icicle-select-region-action.
;;     icicle-select-region: Use icicle-select-region-action.
;;     Renamed: option icicle-regions to icicle-region-alist.
;;     icicle-regions: Sort entries.
;; 2007/01/17 dadams
;;    icicle-filter-alist: Reversed arg order.  Return alist arg if filter-keys arg is empty.
;; 2007/01/12 dadams
;;    icicle-delete-window: Do icicle-remove-Completions-window if in minibuffer.
;;    icicle-yank-insert: Do icicle-yank if in minibuffer.
;;    icicle-(fundoc|vardoc|doc|plist): Added condition-case: protect symbols that error.
;; 2007/01/01 dadams
;;    Added: icicle-(add|remove)-candidate-(to|from)-saved-completion-set.
;;    icicle-add-buffer-config: Use nil, not "nil" as default, if icicle-buffer-sort is nil.
;;                              Use icicle-define-add-to-alist-command to define it.
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set:
;;      Use icicle-assoc-delete-all, not delete of assoc.
;;    icicle-remove-saved-completion-set: Update display after removal.
;;    Reformatted icicle-define(-file)-command, based on setup.el's lisp-indentation-hack.
;; 2006/12/25 dadams
;;    Bug fix: icicle-search-action: Use icicle-filter-alist on icicle-candidates-alist.
;;    icicle-(select|search)-region: Use pop-to-buffer and raise-frame, not set-buffer.
;;    icicle-select-region: Activate the region.
;; 2006/12/23 dadams
;;    Added: icicle-region-help.  Use in icicle-*-region.
;;    Added: icicle-remove-all-regions-in-buffer, icicle-remove-all-regions-action.
;;    icicle-(select|search)-region: Ignore regions in non-existent buffers.
;;    icicle-remove-region: Update the persistent value of icicle-regions.
;; 2006/12/22 dadams
;;    Added: icicle-exchange-point-and-mark.
;;    icicle-customize-icicles-group: icicles -> Icicles (group name).
;; 2006/12/18 dadams
;;    icicle-object-action: Remove print arg.  icicle-apply-to-* uses current-prefix-arg now.
;; 2006/12/17 dadams
;;    Added: icicle-object-action, icicle-choose-candidate-of-type,
;;           icicle-read-var-value-satisfying.
;; 2006/12/16 dadams
;;    icicle-map-action: Bug fix: Use icicle-candidate-nb, not assoc.
;;    Added: icicle-goto(-global)-marker, icicle-marker+text, icicle-markers.
;; 2006/12/10 dadams
;;    Moved minibuffer and *Completions* commands to new file, icicles-mcmd.el.
;;    Require icicles-opt.el.
;;    icicle-buffer-list: Added final message.
;; 2006/11/26 dadams
;;    icicle-search-action: Bug fix: Use icicle-candidate-nb, not assoc, to get cand+mrker.
;;    icicle-*-complete-1: Bug fix: Don't set icicle-current-input to icicle-last-input if nil.
;;    Renamed: icicle-search-region to icicle-search-region-beg-end.
;;    Added: icicle-(add|remove|select|search)-region.
;;    icicle-search: Use icicle-regions for numeric prefix arg.  Updated doc string.
;;    Added: icicle-Info-index-20 - thx to Tamas Patrovics.  Use it in icicle-Info-index.
;; 2006/11/25 dadams
;;    icicle-search: After final selection, select orig-window and give its frame input focus.
;; 2006/11/24 dadams
;;    Added: icicle-ensure-overriding-map-is-bound, icicle-universal-argument,
;;           icicle-universal-argument-more, icicle-negative-argument, icicle-digit-argument,
;;           icicle-universal-argument-other-key, icicle-universal-argument-minus,
;;           icicle-kmacro(-action).
;;    icicle-dabbrev-completion: Don't stop at common root, and use lax completion.
;;    Replaced icicle-select-window-or-frame by icicle-other-window-or-frame (respects C-u 0).
;; 2006/11/23 dadams
;;    icicle-prefix-complete-1: Respect icicle-TAB-shows-candidates-flag.
;;    icicle-execute-extended-command-1: Treat named keyboard macros too.
;;    Added: icicle-execute-named-keyboard-macro.
;; 2006/11/18 dadams
;;    icicle-add/update-saved-completion-set, icicle-apropos*, icicle-bookmark,
;;    icicle-buffer-config, icicle-candidate-set-retrieve, icicle-candidate-set-save,
;;    icicle-color-theme, icicle-comint-command, icicle-complete-thesaurus-entry,
;;    icicle-customize-apropos*, icicle-delete-windows, icicle-font, icicle-frame-bg,
;;    icicle-frame-fg, icicle-insert-kill, icicle-insert-string-from-variable,
;;    icicle-insert-thesaurus-entry, icicle-locate-file*, icicle-map, icicle-narrow-candidates,
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set,
;;    icicle-reset-option-to-nil, icicle-save-string-to-variable, icicle-search,
;;    icicle-select-window, icicle-set-option-to-t, icicle-toggle-option:
;;      Use a specific history variable.
;; 2006/11/17 dadams
;;    Added: icicle-select-(frame|window), icicle-select-window-or-frame.
;; 2006/11/10 dadams
;;    icicle-mouse-candidate-action: read-event to swallow mouse up event.
;;    icicle-map-action:
;;      Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;      Unwind-protect to reselect minibuffer frame.  Don't bind case-fold-search.
;;    icicle-map: enable-recursive-minibuffers.
;; 2006/11/09 dadams
;;    icicle-nb-of-candidate-in-Completions: Redefined using binary search, for performance.
;;    icicle-toggle-ignored-space-prefix: Update doc string to use icicle-dispatch-C-^.
;;    icicle-search:
;;      Bind icicle-update-input-hook unconditionally, after icicle-search-regexp-scan.
;;    icicle-search-regexp-scan: Highlight up to icicle-search-highlight-threshold.
;;    icicle-search-highlight-all-input-matches:
;;      Only update input and highlight if icicle-search-highlight-all-current-flag.
;;    icicle-search-action:
;;      Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;    icicle-search-highlight-cleanup: Bind inhibit-quit to t.
;; 2006/11/07 dadams
;;    Added: icicle-toggle-highlight-all-current.
;; 2006/11/06 dadams
;;    icicle-search-action:
;;      Highlight icicle-current-input, not icicle-current-raw-input (not updated).
;;    Renamed icicle-search-highlight-all-flag to icicle-search-highlight-threshold.
;; 2006/11/05 dadams
;;    Added: icicle-search-regexp-scan.
;;    icicle-search:
;;      Added buffers arg.  Updated doc string.
;;      Use icicle-search-regexp-scan:  Scan each buffer in buffers.
;;                                      Add marker, not position, to icicle-candidates-alist.
;;      Go to candidate in its buffer.
;;      Added progress message.
;;    icicle-search-action: Pop to buffer of candidate (marker) and raise frame.
;;    icicle-occur:
;;      Added buffers arg.  Updated doc string.  Call icicle-search-highlight-cleanup.
;;    icicle-search-highlight-all-input-matches: set-buffer for each ov in dolist (minor opt.).
;;    icicle-search-highlight-cleanup: Added progress messages.  Minor optimization.
;; 2006/10/22 dadams
;;    icicle-complete-keys-action:
;;      Set last-nonmenu-event to non-mouse info, to ignore *Completions* click.
;;    icicle-complete-keys-1: Don't use a default value for completing-read.
;; 2006/10/21 dadams
;;    Added: icicle-insert-char.
;;    icicle-add-key+cmd: Respect icicle-complete-keys-self-insert-flag.
;; 2006/10/20 dadams
;;    icicle-map, icicle-delete-window: Corrected doc string.
;; 2006/10/16 dadams
;;    icicle-add-key+cmd: Protect :enable's eval with condition-case.
;;    icicle-complete-keys-1:
;;      No longer use icicle-extra-candidates.
;;      Use default value of .. for completing-read (except at top level).
;;    icicle-complete-keys-action: Correct no-match case: must match whole and part.
;;    icicle-keys+cmds-w-prefix: Add .. to icicle-complete-keys-alist unless at top level.
;; 2006/10/15 dadams
;;    icicle-complete-keys:
;;      Bind icicle-complete-keys-action, not icicle-complete-keys-help, to icicle-*-action-fn.
;;      Bind orig-buff, orig-window, and icicle-completing-keys-p, for use elsewhere.
;;    Added: icicle-complete-keys-action using code from icicle-complete-keys.
;;    icicle-complete-keys-action:
;;      Use orig-buff and orig-window; restore to originally selected window.
;;      Error if candidate doesn't match template xxx  =  yyy.
;;    icicle-complete-keys-1:
;;      Call icicle-complete-keys-action on chosen candidate.
;;    icicle-help-on-candidate: Treat key completion also.
;;    Added from cus-edit+.el: custom-variable-p.
;;    Moved to icicles-mode.el: icicle-select-minibuffer-contents, next-history-element.
;;    Moved here from icicles-mode.el: icicle-generic-S-tab.
;;    icicle-generic-S-tab (bug fix): Do not set last-command to icicle-apropos-complete.
;;    Added: eval-when-compile's.
;; 2006/10/13 dadams
;;    icicle-add-key+cmd:
;;      Add actual key to icicle-complete-keys-alist.  Thx to Stefan Monnier.
;;      Don't filter out index (Imenu) keymap.
;;      Treat :enable condition.
;;    icicle-complete-keys-1:
;;      Use actual key recorded in icicle-*-keys-alist. Don't convert to key description.
;;      Treat digit-argument and negative-argument.
;;    icicle-complete-keys-alist: Updated doc string for new structure.
;; 2006/10/08 dadams
;;    Added: icicle-add-key+cmd, icicle-read-single-key-description.
;;    Added: icicle-complete-keys-alist.
;;           Use in icicle-complete-keys-1, icicle-keys+cmds-w-prefix, icicle-add-key+cmd.
;;    icicle-add-key+cmd: Update binding, depending on its type (menu item etc.).
;;      Push (cons candidate binding), not just candidate, onto icicle-complete-keys-alist.
;;    icicle-complete-keys-1:
;;      Use binding, not just command name.  Call it and put it in (this|last)-command.
;;      Flipped (corrected) use of icicle-key-descriptions-use-<>-flag.
;;      Use icicle-read-single-key-description.
;;    icicle-prefix-keys-first-p, icicle-complete-keys-1, icicle-complete-keys-help,
;;      icicle-keys+cmds-w-prefix: Don't use icicle-list-*-string.
;; 2006/10/05 dadams
;;    icicle-complete-keys-1: Remove icicle-special-candidate property from all candidates.
;;    icicle-keys+cmds-w-prefix:
;;      Intern candidate and, if local binding, put icicle-special-candidate property on it.
;;      Use single string for candidate (don't use multi-completion).
;; 2006/10/03 dadams
;;     icicle-complete-keys-1: Treat "..".
;;     icicle-complete-keys: Updated doc string accordingly.
;;     icicle-prefix-keys-first-p: ".." is less than all other strings.  Don't hard-code "= ".
;;     icicle-keys+cmds-w-prefix:
;;       Filtered out shadowed bindings, icicle-generic-S-tab, and icicle-complete-keys.
;;       Use only map-keymap & lookup-key, not accessible-keymaps, current-active-maps,
;;         map-keymap.
;; 2006/10/01 dadams
;;     icicle-complete-keys: Bind sort functions, to put prefix keys first, by default.
;;                           Set last-command, before recursing.
;;     Replaced icicle-alternative-sort with icicle-toggle-alternative-sorting (new).
;;     icicle-(apropos|prefix)-complete-1:
;;       Ensure icicle-(current|last)-input are strings, before compare.
;;     icicle-keys+cmds-w-prefix: Tolerate empty local and global maps.
;; 2006/09/30 dadams
;;     Added: icicle-read-kbd-macro, icicle-edmacro-parse-keys, icicle-toggle-angle-brackets.
;;     icicle-complete-keys-1, icicle-dabbrev-completion:
;;       key-description -> icicle-key-description, with icicle-key-descriptions-use-<>-flag.
;;     icicle-complete-keys-1:
;;       read-kbd-macro -> icicle-read-kbd-macro, with icicle-key-descriptions-use-<>-flag.
;;       Got rid of extra space in prompt before colon, when no prefix.
;;     icicle-keys+cmds-w-prefix: Use single-key-description with icicle-*-use-<>-flag.
;;     icicle-insert-key-description:
;;       Change arg to a toggle, and use icicle-key-descriptions-use-<>-flag.
;;     Bind icicle-candidate-set-(retrieve|save) to C-M-<, C-M->, not C-<, C->.
;;     icicle-dired-saved-file-candidates*:
;;       Changed doc strings and messages to use dynamic binding of icicle-candidate-set-save.
;; 2006/09/24 dadams
;;     Added: icicle-complete-keys-help.
;;     icicle-complete-keys:
;;       Bind icicle-*-action-fn to icicle-complete-keys-help.  Mention help keys in docstring.
;;     icicle-complete-keys-1:
;;       Set last-command to command, so completion doesn't think candidate was last-command.
;;     icicle-keys+cmds-w-prefix: Provide placeholder for future use of generic characters.
;; 2006/09/23 dadams
;;     icicle-complete-keys-1:
;;       Error if there are no keys for the prefix.
;;       Error, not self-insert-command, for key read-kbd-macro can't convert. condition-case.
;;       Report error if calling cmd fails.
;;       Use vconcat for recursive call.
;;       Read cmd, don't intern it - it might be a lambda or byte-compiled function.
;;       Remove duplicates.
;;       Provide KEYS arg to call-interactively, for error reporting.
;;       Don't bind icicle-must-not-match-regexp to "^Character set .*=  self-insert-command".
;;     icicle-keys+cmds-w-prefix:
;;       Treat also local keymap and current minor maps.
;;       Do nothing if keys+maps is nil.
;;       Only map-keymap if the target is a keymap.
;;       Use keymapp, not functionp, as the binding test.
;;       Only add binding if it is a command or keymap.
;;       Only add self-insert-command binding if the key is char-valid-p.
;;       Use format %S, not %s for a command binding.
;;     icicle-insert-key-description: Added no-angle-brackets-p arg.
;; 2006/09/22 dadams
;;     icicle-complete-keys-1:
;;       Filter out keys described "Character set ...= self-insert-command".
;; 2006/09/20 dadams
;;     icicle-complete-keys-1: Treat self-insert-command specially.
;; 2006/09/17 dadams
;;     Added: icicle-complete-keys(-1), icicle-*-keys-prefix, icicle-keys+cmds-w-prefix,
;;     icicle-doc: Removed one \n from each candidate.
;; 2006/09/12 dadams
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;;     Added: icicle-switch-to/from-minibuffer.
;;     icicle-completion-help: Keep focus in the minibuffer after displaying help.
;; 2006/09/02 dadams
;;     icicle-help-on-(next|previous)-(apropos|prefix)-candidate,
;;       icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Use save-selected-window, not save-window-excursion.
;;     icicle-find-file*: In Dired, ignore errors picking up current-line's file name.
;;     icicle-mouse-choose-completion: Error if minibuffer is not active.
;; 2006/08/27 dadams
;;     icicle-abort-minibuffer-input: If minibuffer not active, just kill buffer *Completions*.
;;     icicle-execute-extended-command-1, icicle-insert-thesaurus-entry, icicle-search-action:
;;       Ensure orig-window is live before using it.
;; 2006/08/23 dadams
;;     Added: icicle-delete-window(s).
;;     Added soft require of frame-cmds.el.
;; 2006/08/22 dadams
;;     icicle-execute-extended-command-1: Bind this-command, don't set it (fixes C-next).
;;     icicle-help-on-candidate: If no last candidate, then reset to first matching candidate.
;;     icicle-*-candidate-action, icicle-help-on-*-candidate: save-window-excursion.
;; 2006/08/20 dadams
;;     icicle-find-file*: Use diredp-find-a-file* in Dired mode (Emacs 22 or later).
;;     Bug fix: icicle-candidate-action: Use icicle-*-candidates, not icicle-next-*-candidate.
;;              icicle-next-*-candidate(-action): Set icicle-current-completion-mode.
;; 2006/08/18 dadams
;;     Added: icicle-Info-goto-node(-(action|cmd)).
;;     icicle-candidate-action: If no icicle-last-completion-candidate, use first candidate.
;; 2006/08/15 dadams
;;     Added: icicle-help-on-*-*-candidate,icicle-mouse-help-on-candidate.
;;     No longer put icicle-candidate-action-command property on symbols (not used).
;;     Added: icicle-raise-Completions-frame.
;;     icicle*-candidate-action, icicle-help-on-candidate: Use icicle-raise-Completions-frame.
;;     icicle-help-on-candidate: Can use it from *Completions* too now.
;;                               Use icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     Added: icicle-Info-index(-(action|cmd)).
;; 2006/08/04 dadams
;;     icicle-*-complete-1, icicle-prefix-word-complete, icicle-keep-only-past-inputs:
;;       Set icicle-last-completion-command to the explicit command, not this-command.
;;     icicle-history: Call icicle-last-completion-command, not icicle-apropos-complete.
;;     icicle-apropos-complete-1, icicle-narrow-candidates:
;;       Removed binding of icicle-apropos-completing-p (not used).
;;     Added: icicle-plist, icicle-remove-Completions-window, icicle-pp-eval-expression.
;;     Added soft require of pp+.el.
;;     icicle-exit-minibuffer, icicle-minibuffer-complete-and-exit,
;;       icicle-mouse-choose-completion, icicle-abort-minibuffer-input,
;;       icicle-(apropos|prefix)-complete-1, icicle-keep-only-past-inputs,
;;       icicle-insert-thesaurus-entry-cand-fn: Use icicle-remove-Completions-window.
;;     icicle-doc: Treat doc of faces also.
;;     icicle-non-whitespace-string-p: Added doc string.
;; 2006/08/03 dadams
;;     Added:
;;       icicle-comint-command, icicle-insert-kill, icicle-insert-for-yank,icicle-yank-insert.
;;     Bound icicle-comint-command to C-c TAB in comint-mode.
;;     icicle-search, icicle-comint-search: Cleaned up doc string.
;; 2006/08/02 dadams
;;     icicle-comint-search: Mention *-prompt-pattern.  Thx to Kevin Rodgers.
;;     icicle-insert-string-from-variable: Added more variables to the completing-read alist.
;; 2006/07/29 dadams
;;     Added: icicle-dispatch-C-., toggle-icicle-search-cleanup, icicle-toggle-search-cleanup.
;; 2006/07/23 dadams
;;     Added: icicle-toggle-transforming.
;;     icicle-comint-search: Bind icicle-transform-function to icicle-remove-duplicates.
;; 2006/07/22 dadams
;;     Added: icicle-comint-search, icicle-comint-send-input, icicle-comint-get-*-input,
;;            icicle-comint-get-final-choice, icicle-search-generic.
;;     icicle-search: Added require-match arg for non-interactive calls.
;;                    Run the hooks if no match and no match required, and if we didn't cycle.
;;                    Return final choice as value (not used yet).
;;     icicle-insert-string-from-variable: Use buffer-local value of variable, if there is one.
;;     icicle-insert-string-from-variable:
;;       Make sure we use the buffer-local value of the variable, if there is one
;;       Added comint-prompt-regexp to regexp list.
;;     Added mode hooks for icicle-compilation-search and icicle-comint-send-input.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-(next|previous)-context-candidate, icicle-scroll-completions.
;;       icicle-switch-to-completions, icicle-switch-to-Completions-buf,
;;         icicle-move-to-next-completion, icicle-map-action, icicle-search-action:
;;           Use icicle-start-of-completions.
;;       icicle-(apropos|prefix)-complete-1:
;;         Set icicle-current-completion-type vs use icicle-arrows-respect-*-flag.
;;         Use icicle-scroll-completions.
;;       icicle-current-completion-in-Completions: Use point-min if no previous prop change.
;;       icicle-keep-only-past-inputs: Use icicle-scroll-completions.
;;     Renamed icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;             icicle-current-completion-type to icicle-current-completion-mode,
;;             icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;             icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-(next|previous)-context-candidate: Use icicle-barf-if-outside-minibuffer.
;;     icicle-scroll-Completions: Changed with-selected-window to Emacs 20 equivalent.
;; 2006/07/18 dadams
;;     icicle-search: Bind completion-ignore-case to case-fold-search.
;;     icicle-search-highlight-all-input-matches, icicle-search-action:
;;       Put search inside condition-case, for bad regexp.
;;     Added: icicle-toggle-case-sensitivity, toggle-icicle-case-sensitivity.
;; 2006/07/10 dadams
;;     Added: icicle-search-region.  Use in search functions.  Thx to Le Wang.
;; 2006/07/08 dadams
;;     icicle-search-highlight-all-input-matches: Use *-current-*, not *-current-raw-*.
;;     icicle-execute-extended-command-1:
;;       First try a string candidate as arg, then read it to convert it to symbol or number.
;;       Reset focus back to the minibuffer, in action function.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort.
;;     icicle-imenu: Show *Completions* initially for submenu choice (only).
;;     icicle-execute-extended-command:
;;       Echo prefix arg in prompt.  Thx: *.dhcp.mdsn.wi.charter.com
;; 2006/07/06 dadams
;;     Added (eval-when-compile (require 'icicles-mac)).
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;;     icicle-prefix-complete-1: Don't set icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-prefix-complete-1: No longer calculate common prefix and set current input to it.
;;     Added plist entries to categorize commands:
;;       icicle-(cycling|completing|candidate-action)-command.
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete,
;;     icicle-switch-to-Completions-buf, icicle-keep-only-past-inputs, icicle-history:
;;       Use icicle-cycling-command property.
;;     icicle-apropos-complete-1: Removed regexp-p arg in call to icicle-save-or-restore-input.
;; 2006/07/03 dadams
;;     icicle-(apropos|prefix)-complete-1: deactivate mark after inserting current input.
;; 2006/06/18 dadams
;;     icicle-apropos-complete-1, icicle-narrow-candidates: Bind icicle-apropos-completing-p.
;; 2006/06/09 dadams
;;     Bug fixes: Picked up matching subdir as default dir, even if there other files match.
;;                  Thx to Andrey Zhdanov.
;;                Empty directory not treated as a match.
;;     icicle-(apropos|prefix)-complete-1:
;;       If input matches an empty directory, then use that directory as the sole completion.
;;       Do not expand file-name input before call icicle-file-name-*-candidates.
;;     icicle-retrieve-last-input: Use insert, not icicle-insert-input (no longer used).
;;                                 (Input backslashes reverted to slashes.)
;; 2006/06/08 dadams
;;     Bug fix: Could not complete after cycling file names.  Thx to Andrey Zhdanov.
;;     icicle-insert-input: Use icicle-expand-file-name.
;;     icicle-prefix-complete-1:
;;       Expand file-name input before call icicle-file-name-prefix-candidates.
;;       Expand icicle-last-completion-candidate if it is a directory name.
;; 2006/05/30 dadams
;;     icicle-erase-minibuffer-or-history-element: Fix for consecutive deletions.
;; 2006/05/26 dadams
;;     Added: icicle-erase-minibuffer-or-history-element.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;     icicle-narrow-candidates: Bind icicle-reminder-prompt-flag to nil, not t.
;; 2006/05/16 dadams
;;     Added: icicle-kill(-a)-buffer.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-candidate-set-complement: Put back icicle-ignore-space-prefix-flag.
;;     icicle-buffer(-other-window): Bind icicle-buffer-ignore-space-prefix-flag.
;;     Added: icicle-toggle-ignored-space-prefix, toggle-icicle-ignored-space-prefix.
;; 2006/05/13 dadams
;;     icicle-occur: Make icicle-search-main-regexp-others unnoticeable instead of
;;                   setting icicle-search-highlight-all-flag to nil.
;;     icicle-candidate-set-complement: Use nil, not icicle-completion-nospace-flag.
;;     Renamed: icicle-search-imenu to icicle-imenu,
;;              icicle-search-imenu-in-buffer-p to icicle-imenu-in-buffer-p.
;; 2006/05/12 dadams
;;     icicle-search-imenu: Remove unmatched submenus.  Error if no imenu for the buffer.
;;     Added: icicle-search-imenu-in-buffer-p.
;;     icicle-insert-string-at-point: Use icicle-barf-if-outside-minibuffer.
;;     Moved to icicles-fn.el: icicle-barf-if-outside-*.
;;     Moved some commands to minibuffer-cmds section from top-level cmds section.
;; 2006/05/09 dadams
;;     Added: icicle-customize-icicles-group, icicle-send-bug-report, icicle-customize-button.
;; 2006/04/30 dadams
;;     Added: icicle-map, icicle-map-action.
;;     icicle-filter-alist: Corrected and simplified.
;;     icicle-search: Corrected cand-nb adjustment when cycle with action fns.
;;     Renamed: icicle-search-action-fn to icicle-search-action,
;;              icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/28 dadams
;;     icicle-retrieve-last-input, icicle-(apropos|prefix)-complete-1:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/25 dadams
;;     icicle-completion-help: Emacs 21.3's help-insert-xref-button signature is different.
;; 2006/04/16 dadams
;;     Added: icicle-search-imenu.
;;     icicle-search: Bug fixes:
;;       Treat completion without cycling: error or singleton go-to.
;;       Only subtract one from candidate number for C- cycling, not regular cycling.
;; 2006/04/14 dadams
;;     icicle-search:
;;       Bug fix: Position was off by one.
;;       Highlight input match inside each main regexp match (or not).
;;         Bind icicle-update-input-hook and icicle-incremental-completion-flag.
;;       Extract code to define icicle-search-action-fn.
;;       Use icicle-search-candidates instead of local variable search-candidates.
;;       Respect icicle-search-cleanup-flag.
;;     Added: icicle-search-highlight-*, icicle-search-action-fn,
;;            icicle-(insert|save)-text-(from|to)-variable.
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/09 dadams
;;     icicle-(apropos|prefix)-complete-1: Deal with icicle-arrows-respect-*-flag.
;;     Moved here from icicles-fn.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/07 dadams
;;     icicle-search: Highlight all occurrences at once (like isearch highlight, but not lazy).
;;                    Error if no match for initial regexp.
;;     icicle-occur: Bind icicle-search-highlight-all-flag to nil: don't highlight each line.
;; 2006/04/02 dadms
;;     Added: icicle-toggle-regexp-quote, icicle-find-file*-w-wildcards.
;;     icicle-find-file*: Use icicle-find-file*-w-wildcards.
;; 2006/03/31 dadams
;;     icicle-search: Wrap action function with unwind-protect to select minibuffer frame.
;;                    Use completion-ignore-case when highlighting search hits.
;;                    Protect delete-overlay with overlayp.
;;                    Turn off region highlighting (so can see highlighting done here).
;;                    Removed sit-for-period argument.
;;     icicle-candidate-set-save: Use prin1 instead of pp.
;; 2006/03/27 dadams
;;     Added: icicle-occur.
;;     icicle-search: Highlight also match of current regexp, inside that of regexp arg.
;;                    Use new faces icicle-search-*-regexp.
;;     icicle-search, icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Use new, generic icicle-place-overlay.
;;     Removed icicle-place-search-overlay.
;; 2006/03/26 dadams
;;     icicle-search: Use icicle-search-overlay.  Ensure don't match twice at same position.
;;                    Added regexp arg.  Use 0 as sit-for default.
;;     Added: icicle-place-search-overlay.
;; 2006/03/25 dadams
;;     icicle-prefix-complete: Minor bug fix: Don't save try-completion if not a string.
;;     icicle-candidate-set-(save|retrieve): Allow use of a variable to save/retrieve.
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-variable, icicle-*-no-display,
;;            icicle-prefix-complete-1.
;;     icicle-apropos-complete-1: Added no-display-p optional arg.
;;     Use no-display-p arg in calls to icicle-display-candidates-in-Completions.
;;     icicle-candidate-set-(retrieve-from|save-to)-cache-file: Pass a consp, not t.
;;     icicle-candidate-set-retrieve: Don't display *Completions*.
;; 2006/03/24 dadams
;;     Added icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-candidate-set-define: Rewrote.  Can also use at top level.
;;       Error if wrong result type.  Sort result.  Use display-completion-list and
;;       icicle-narrow-candidates (unless at top level).
;;     icicle-narrow-candidates: Can call from top-level (but not interactively).
;;     icicle-candidate-set-complement: Use icicle-maybe-sort-and-strip-candidates.
;;     Mention in doc strings of minibuffer and *Completions* functions: where, key.
;; 2006/03/22 dadams
;;     icicle-find-file*: Use default-directory as default, so opens directory on empty input.
;;     icicle-prefix-complete:
;;       Save icicle-current-regexp-input.
;;       Set icicle-current-input to common prefix.  Use it everywhere here.
;;     Calls to icicle-display-candidates-in-Completions: no root arg now.
;; 2006/03/21 dadams
;;     icicle-insert-input: Bug fix: Use directory of input, not default-directory.
;;                                   Append a slash if input itself is a directory.
;; 2006/03/20 dadams
;;     icicle-retrieve-last-input: Insert icicle-current-regexp-input if repeat C-l.
;;     Added: icicle-insert-input.
;; 2006/03/19 dadams
;;     icicle-apropos-complete-1: Call icicle-save-or-restore-input with non-nil regexp-p arg.
;; 2006/03/17 dadams
;;     Added: icicle-add/update-saved-completion-set, icicle-remove-saved-completion-set,
;;            icicle-retrieve-candidates-from-set.
;;     Removed: icicle-cache-file.
;;     icicle-candidate-set-retrieve: Read candidates set and use its cache file.
;;                                    Enable recursive minibuffers.
;;     icicle-candidate-set-save: Read candidates set and cache-file names.
;;                                Use icicle-add/update-saved-completion-set.
;;     icicle-barf-if-outside-minibuffer: Move interactive test to calling functions.
;;     icicle-files-within: Moved to icicle-fn.el.
;; 2006/03/16 dadams
;;     Added: icicle*-saved-completion-set.
;; 2006/03/14 dadams
;;     icicle-narrow-candidates: Handle no-catch error.  Don't use icicle-completing-p.
;;     icicle-candidate-set-complement:
;;       Do what we do in icicle-candidate-set-retrieve: call icicle-narrow-candidates.
;;     icicle-candidate-set-(retrieve|complement): Msg when display.
;;     icicle-(apropos|prefix)-complete-1:
;;       Removed test for last-command = icicle-candidate-set-complement.
;; 2006/03/13 dadams
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-cache-file.
;;     icicle-candidate-set-(retrieve|save): C-u uses cache file.
;; 2006/03/12 dadams
;;     Added: icicle-dired-saved-file-candidates(-other-window), icicle-locate-file*,
;;            icicle-files-within.
;; 2006/03/11 dadams
;;     icicle-find-file*, icicle-delete-file*:
;;       Reverted to simple form (moved directory control to icicles-mac.el).
;;     icicle-keep-only-past-inputs: Expand file name relative to directory of last input.
;; 2006/03/10 dadams
;;     icicle-find-file*, icicle-delete-file*: Expand file name relative to dir of last input.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;; 2006/03/09 dadams
;;     icicle-barf-if-outside-*: Removed argument - use this-command instead.
;; 2006/03/08 dadams
;;     icicle-bookmark: Use default value, not init value, for completing-read.
;; 2006/03/07 dadams
;;     icicle-doc: Save table in minibuffer-completion-table, so can access via C-RET too.
;;     icicle-insert-thesaurus-entry, icicle-*doc:
;;       Removed binding of icicle-incremental-completion-flag to nil.
;;     Added: icicle-barf-if-outside-(minibuffer|Completions).  Use in appropriate commands.
;;     Added: icicle-non-whitespace-string-p.  Use in icicle-*doc.
;; 2006/03/06 dadams
;;     Update doc strings of *-thesaurus*.
;; 2006/03/05 dadams
;;     Added: icicle-toggle-incremental-completion, toggle-icicle-incremental-completion.
;; 2006/03/03 dadams
;;     icicle-*doc: Clarified doc strings.  Updated prompts.
;;     Added: icicle-help-button.  Use in icicle-completion-help.
;; 2006/03/02 dadams
;;     icicle-insert-thesaurus-entry, icicle-complete-thesaurus-entry:
;;       Use synonyms-ensure-synonyms-read-from-cache.  Clarified doc strings.
;;     icicle-complete-thesaurus-entry: Error if no word at point.  Correct looking-at regexp.
;; 2006/03/01 dadams
;;     Added: icicle-insert-thesaurus-entry, icicle-insert-thesaurus-entry-cand-fn,
;;            icicle-complete-thesaurus-entry.
;;     icicle-(previous|next)-(apropos|prefix)-candidate-action: Wrap in save-excursion.
;;     Use icicle-clear-minibuffer instead of icicle-erase-minibuffer non-interactively.
;;     icicle-erase-minibuffer: Use icicle-call-then-update-Completions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; You need not load this file.  It contains only documentation.

(provide 'icicles-chg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-chg.el ends here

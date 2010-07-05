;;; columnify.el --- arrange lines into columns
;;;; Copyright 2007, 2008 Jim Blandy
;;;;
;;;; Author: Jim Blandy <jimb@red-bean.com>
;;;; Version: 1.0
;;;; Keywords: convenience, files, wp
;;;;
;;;; This file is not (yet) part of GNU Emacs.
;;;; However, it is distributed under the same license.
;;;;
;;;; GNU Emacs is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Emacs is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301, USA.
;;;;
;;;; Commentary:
;;;;
;;;; This library provides the 'columnify' command, which arranges the
;;;; lines in a region into columns.  It chooses a column width based
;;;; on the length of the longest line, and tries to fit as many
;;;; columns as possible into the current line width (as established
;;;; by `fill-column'), with some reasonable amount of space between
;;;; columns.

;;;; Code:

;;;###autoload
(defun columnify (start end)
  "Reformat the lines in the current region into a columnar list.

Choose the number of columns based on the width of the longest
line and the current value of `fill-column'.  Try to get at least
a half-column-width of space between each column.

The items in the region are taken to be in \"column-major\"
order: reading down each column, starting with the leftmost
column, yields the items in the order they originally appeared.

Ignore whitespace at the beginning and end of each line.

This command assumes that all characters have a width that is an
even multiple of the width of a space."
  ;; It might be nice to take the number of columns as a prefix
  ;; argument; if the user doesn't like what we choose, they can play
  ;; with it themselves.
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((max-width 0)
            (num-lines 0))

        ;; Remove leading and trailing horizontal space.
        ;; Find the length of the longest line, and count the lines.
        (goto-char (point-min))
        (while (< (point) (point-max))
          (setq num-lines (1+ num-lines))
          (delete-horizontal-space)
          (end-of-line)
          (delete-horizontal-space)
          (if (> (current-column) max-width)
              (setq max-width (current-column)))
          (forward-line 1))

        (unless (> num-lines 0)
          (error "region has no lines to columnify"))
        (unless (> max-width 0)
          (error "all lines in region are empty; cannot columnify"))

        (let* ((spacing (/ (1+ max-width) 2))
               (num-cols (/ (+ fill-column spacing)
                            (+ max-width spacing)))
               (num-rows (/ (+ num-lines num-cols -1) num-cols)))
          
          ;; Extend each line out to a full column width.
          (goto-char (point-min))
          (while (< (point) (point-max))
            (end-of-line)
            (move-to-column (+ max-width spacing) t)
            (forward-line 1))

          ;; A marker for the line the next entry should go at
          ;; the end of.
          (let ((dest (point-min-marker))
                (n 0))

            ;; Skip to the end of the first column, and then copy
            ;; entries to their proper places, starting at the top
            ;; again every num-rows lines.
            (goto-char (point-min))
            (forward-line num-rows)
            (while (< (point) (point-max))
              (let* ((line-start (point))
                     (line (buffer-substring line-start
                                             (progn (end-of-line) (point)))))
                (delete-region line-start (progn (forward-line 1) (point)))
                (save-excursion
                  (goto-char dest)
                  (end-of-line)
                  (insert line)
                  (setq n (1+ n))
                  (if (< n num-rows)
                      (forward-line 1)
                    (goto-char (point-min))
                    (setq n 0))
                  (set-marker dest (point))))))))

      ;; Trim all trailing whitespace, introduced in the final column.
      (goto-char (point-min))
      (while (< (point) (point-max))
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1)))))

(provide 'columnify)

;;; columnify.el ends here

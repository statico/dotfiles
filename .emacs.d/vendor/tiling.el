;;; tiling.el --- changing window layout

;; Copyright (C) 2010  Fang lungang

;; Author: Fang lungang <fanglungang at 163.com>
;; Created: Fang lungang 11/14/2010
;; Modified: Fang lungang 12/01/2010 13:18>
;; Keywords: convenience, frames
;; Version: 0.0.4

;; This file is NOT part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; As the display geting larger and larger, "Tiling" is getting more and more
;; popular. Emacs is tiling windows within the frame for sure. However, by
;; default, it does not provide an easy way to change a set of preset layouts
;; like what Xmonad and tmux did. This package is trying to provide one solution

;; To install: download this file into to you load-path and "(require 'tiling)"
;; in your init file.

;; Strongly recommand you use this package together with windmove, winner-mode
;; and buffermove. The last one is not part of Emacs yet. Beneath is my
;; configuration:

;; ;;; Windows related operations
;; ;; Split & Resize
;; (define-key global-map (kbd "C-x |") 'split-window-horizontally)
;; (define-key global-map (kbd "C-x _") 'split-window-vertically)
;; (define-key global-map (kbd "C-{") 'shrink-window-horizontally)
;; (define-key global-map (kbd "C-}") 'enlarge-window-horizontally)
;; (define-key global-map (kbd "C-^") 'enlarge-window)
;; ;; Navgating: Windmove uses C-<up> etc.
;; (define-key global-map (kbd "C-<up>"   ) 'windmove-up)
;; (define-key global-map (kbd "C-<down>" ) 'windmove-down)
;; (define-key global-map (kbd "C-<right>") 'windmove-right)
;; (define-key global-map (kbd "C-<left>" ) 'windmove-left)
;; ;; Swap buffers: M-<up> etc.
;; (define-key global-map (kbd "M-<up>"   ) 'buf-move-up)
;; (define-key global-map (kbd "M-<down>" ) 'buf-move-down)
;; (define-key global-map (kbd "M-<right>") 'buf-move-right)
;; (define-key global-map (kbd "M-<left>" ) 'buf-move-left)
;; ;; Tile
;; (define-key global-map (kbd "C-\\") 'tiling-cycle) ; accepts prefix number
;; (define-key global-map (kbd "C-M-<up>") 'tiling-tile-up)
;; (define-key global-map (kbd "C-M-<down>") 'tiling-tile-down)
;; (define-key global-map (kbd "C-M-<right>") 'tiling-tile-right)
;; (define-key global-map (kbd "C-M-<left>") 'tiling-tile-left)
;; ;; Another type of representation of same keys, in case your terminal doesn't
;; ;; recognize above key-binding. Tip: C-h k C-up etc. to see into what your
;; ;; terminal tranlated the key sequence.
;; (define-key global-map (kbd "M-[ a"     ) 'windmove-up)
;; (define-key global-map (kbd "M-[ b"     ) 'windmove-down)
;; (define-key global-map (kbd "M-[ c"     ) 'windmove-right)
;; (define-key global-map (kbd "M-[ d"     ) 'windmove-left)
;; (define-key global-map (kbd "ESC <up>"   ) 'buf-move-up)
;; (define-key global-map (kbd "ESC <down>" ) 'buf-move-down)
;; (define-key global-map (kbd "ESC <right>") 'buf-move-right)
;; (define-key global-map (kbd "ESC <left>" ) 'buf-move-left)
;; (define-key global-map (kbd "ESC M-[ a" ) 'tiling-tile-up)
;; (define-key global-map (kbd "ESC M-[ b" ) 'tiling-tile-down)
;; (define-key global-map (kbd "ESC M-[ c" ) 'tiling-tile-right)
;; (define-key global-map (kbd "ESC M-[ d" ) 'tiling-tile-left)

;;; Code:

(defvar tiling-layouts (list 'tiling-master-left
                             'tiling-master-top
                             'tiling-even-horizontal
                             'tiling-even-vertical
                             'tiling-tile-4)
  "List of supported layout. Maybe ring is better")

(defvar tiling-current-layout (car tiling-layouts)
  "Current layout, maynot reflect the actual layout. But that is
  not a big deal")


;; Layout for 4-window only
(defun tiling-tile-4 (bufs)
  "The layout only applies to 4-window frame"
  (when (= 4 (length bufs))
    (delete-other-windows)
    (set-window-buffer nil (nth 0 bufs))

    (split-window-horizontally)
    (other-window 1)
    (set-window-buffer nil (nth 1 bufs))

    (split-window-vertically)
    (other-window 1)
    (set-window-buffer nil (nth 2 bufs))

    (other-window 1)

    (split-window-vertically)
    (other-window 1)
    (set-window-buffer nil (nth 3 bufs))
    (other-window 1)))

;; master
(defun tiling-master(bufs horizontal)
  (delete-other-windows)
  (funcall (if horizontal
               'split-window-horizontally
             'split-window-vertically))
  (set-window-buffer nil (car bufs))
  (other-window 1)
  (set-window-buffer nil (car (cdr bufs)))
  (condition-case nil                ; in case too many windows
      (mapcar
       (lambda (buf)
         "split and set other buffers"
         (funcall (if horizontal
                      ;; if master horizontal, others vertical
                      'split-window-vertically
                    'split-window-horizontally))
         (other-window 1)
         (set-window-buffer nil buf))
       (cdr (cdr bufs)))
    (error nil))
  (balance-windows)
  (other-window 1))

(defun tiling-master-left(bufs)
  "No matter how many windows, left half for one master buffer,
  all others split the right half evenly"
  (tiling-master bufs t))

(defun tiling-master-top(bufs)
  "Selected window as the master, put it on top; others share the
  bottom half"
  (tiling-master bufs nil))

;; even
(defun tiling-even (bufs horizontal)
  (delete-other-windows)
  (set-window-buffer nil (car bufs))
  (condition-case nil                ; in case too many windows
      (mapcar
       (lambda (buf)
         "split and set other buffers"
         (funcall (if horizontal
                      'split-window-horizontally
                    'split-window-vertically))
         (other-window 1)
         (set-window-buffer nil buf))
       (cdr bufs))
    (error nil))
  (balance-windows)
  (other-window 1)) ; back to the first window


(defun tiling-even-horizontal (bufs) (tiling-even bufs t))

(defun tiling-even-vertical (bufs) (tiling-even bufs nil))

;; cycling
(defun tiling-cycle (&optional numOfWins)
  "cycling the among the preset layouts"
  (interactive "p")

  (let ((bufs (mapcar 'window-buffer (window-list nil -1 nil)))
        (found nil) (new-layout nil))

    (when (> numOfWins 1)                   ; with number prefix
      ;; get buffers to be showed, which also determins number of windows. By
      ;; default, re-arrange current windows
      (when (< (length bufs) numOfWins)
        ;;also show some recently viewed buffers except mini-buffer etc.
        (mapcar (lambda (x)
                  (unless
                      (and (string= " " (substring (buffer-name x) 0 1))
                           (null (buffer-file-name x)))
                    (add-to-list 'bufs x t)))
                (buffer-list))
        ;; Still no enough bufs, repeat current buffer
        (while (< (length bufs) numOfWins)
          (setq bufs (append bufs (list (car bufs)))))

        ;; show first n buffers being shown
        (nbutlast bufs (- (length bufs) numOfWins))))

    (when (> (length bufs) 1)
      ;; find next layout
      (dolist (layout tiling-layouts)
        (if (eq layout tiling-current-layout)
            (setq found t) ; and continue with next iteration
          (if (and found (not new-layout))
              ;; previous layout is current layout
              (setq new-layout layout))))
      (if (not new-layout) ; current-layout must be the last in the list
          (setq new-layout (car tiling-layouts))) ; wrap
      (setq tiling-current-layout new-layout)
      (funcall tiling-current-layout bufs))))

;; tweak layout

(defun tiling-tile-move (direction)
  (let ((other-win (windmove-find-other-window direction))
        (this-buf (window-buffer (selected-window))))
    (if (and other-win (not (eq (minibuffer-window) other-win)))
        (progn
          (delete-window (selected-window))
          (balance-windows)             ; balance from
          (select-window other-win)
          (if (or (eq direction 'up) (eq direction 'down))
              (split-window-horizontally)
            (split-window-vertically))
          (balance-windows)             ; balance to
          (other-window 1)
          (set-window-buffer nil this-buf)))))

(defun tiling-tile-up () (interactive) (tiling-tile-move 'up))

(defun tiling-tile-down () (interactive) (tiling-tile-move 'down))

(defun tiling-tile-left () (interactive) (tiling-tile-move 'left))

(defun tiling-tile-right () (interactive) (tiling-tile-move 'right))

(provide 'tiling)
;;; tiling.el ends here

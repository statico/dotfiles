;;; hfyview.el --- View current buffer as html in web browser

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman
;; Created: Fri Oct 21 2005
(defconst hfyview:version "0.63") ;; Version:
;; Last-Updated: 2010-04-16 Fri
;; Keywords: printing
;; URL: http://OurComments.org/Emacs/DL/elisp/hfyview.el
;; Compatibility:
;;
;;
;; Features that might be required by this library:
;;
  ;; `easymenu'.
;;
;;
;; htmlfontify.el is part of Emacs.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This file shows the current buffer in your web browser with all
;;  the colors it has. The purpose is mainly to make it possible to
;;  easily print what you see in Emacs in colors on different
;;  platforms.
;;
;;  Put this file in your load-path and in your .emacs this:
;;
;;      (require 'hfyview)
;;
;;  This defines the commands `hfyview-buffer', `hfyview-region' and
;;  `hfyview-window' which will show the whole or a part of the buffer
;;  in your web browser.
;;
;;  You can add those commands to the menus by customizing
;;  `hfyview-quick-print-in-files-menu' to t. This will add an entry
;;  "Quick Print (Using Web Browser)" to the files menu.
;;
;;
;;  There is also a command `hfyview-frame' to take a "screen shot" of
;;  your current frame and produce an html look-alike page. If you
;;  turn on `hfyview-frame-mode' you get this function on the <apps>
;;  key in most situations.
;;
;;
;;  You can see an example of the output here:
;;
;;    http://ourcomments.org/Emacs/nXhtml/doc/htmlfontify-example.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'htmlfontify))
(require 'easymenu)

(defvar hfyview-selected-window)

(defvar hfyview-frame-mode-emulation-map
  (let ((m (make-sparse-keymap)))
    ;;(define-key m [apps] 'hfyview-frame)
    m))

(defvar hfyview-frame-mode-emulation-maps
  (list (cons 'hfyview-frame-mode hfyview-frame-mode-emulation-map)))

;; Fix-me: which are needed? Probably only viper, but have to test.
(defconst hfyview-frame-mode-other-maps
  '(
    hfyview-frame-mode-emulation-map
    minibuffer-local-completion-map
    minibuffer-local-filename-completion-map
    minibuffer-local-isearch-map
    minibuffer-local-map
    ;; minibuffer-local-must-match-filename-map
    minibuffer-local-must-match-map
    minibuffer-local-ns-map
    viper-minibuffer-map
    isearch-mode-map))

(define-minor-mode hfyview-frame-mode
  "Define some useful things for `hfyview-frame'.
The <apps> key is bound to `hfyview-frame' in this mode. When
this mode is on you can push <apps> to get all of what you see on
the screen. Without it the minibuffer/echo area will not be
shown."
  :global t
  :group 'htmlfontify
  (if hfyview-frame-mode
      (progn
        (add-hook 'pre-command-hook 'hfy-grab-minibuffer-content)
        (add-hook 'post-command-hook 'hfy-grab-echo-content)
        (add-to-list 'emulation-mode-map-alists 'hfyview-frame-mode-emulation-maps)
        (dolist (map hfyview-frame-mode-other-maps)
          (define-key (symbol-value map) [(apps)] 'hfyview-frame)
          )
        )
    (remove-hook 'pre-command-hook 'hfy-grab-minibuffer-content)
    (remove-hook 'post-command-hook 'hfy-grab-echo-content)
    (setq emulation-mode-map-alists (delq 'hfyview-frame-mode-emulation-maps emulation-mode-map-alists))
    (dolist (map hfyview-frame-mode-other-maps)
      (define-key (symbol-value map) [(apps)] nil))))

(defun hfyview-fontify-region (start end)
  "Fontify region between START and END the htmlfontify way."
  ;; If the last command in mumamo resulted in a change of major-mode
  ;; the big bug watcher in mumamo will get us if we do not tell that
  ;; we know what we are doing:
  (let ((mumamo-just-changed-major nil))
    (if start
        (save-restriction
          (widen)
          (narrow-to-region start end)
          (assert (= end (point-max)))
          (assert (= start (point-min)))
          (htmlfontify-buffer))
      (htmlfontify-buffer))))

(defun hfyview-buffer-1(start end show-source)
  "Convert current buffer between START and END to html.
If SHOW-SOURCE is non-nil then also show produced html in other
window."
  (let ((hbuf (hfyview-fontify-region start end)))
    (with-current-buffer hbuf
      (setq buffer-file-name nil)
      (browse-url-of-buffer))
    (when show-source (switch-to-buffer-other-window hbuf))
    hbuf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Menus

(defvar hfyview-print-menu (make-sparse-keymap "QP"))
(defvar hfyview-print-region-menu (make-sparse-keymap "QPR"))
(defvar hfyview-print-window-menu (make-sparse-keymap "QPW"))
(defun hfyview-add-to-files-menu ()
  "Add \"Quick Print\" entry to file menu."
  ;; Why did I redo this???
  (setq hfyview-print-menu (make-sparse-keymap "QP"))
  (setq hfyview-print-region-menu (make-sparse-keymap "QPR"))
  (setq hfyview-print-window-menu (make-sparse-keymap "QPW"))
  ;; Main
  (define-key-after menu-bar-file-menu [hfyview-print]
    (list 'menu-item
          "Quick Print (Using Web Browser)"
          hfyview-print-menu
          :visible 'hfyview-print-visible)
    'separator-print)
  ;; Main submenu
  (define-key hfyview-print-menu [hfyview-browser-frame-pre]
    '(menu-item "Print Preview Frame" hfyview-frame
                :help "Print preview frame with web browser"))
  (define-key hfyview-print-menu [hfyview-browser-window-pre]
    '(menu-item "Print Preview Window" hfyview-window
                :help "Print preview window with web browser"))
  (define-key hfyview-print-menu [hfyview-browser-region-pre]
    (list 'menu-item "Print Preview Region" 'hfyview-region
          :help "Print preview region with web browser"
          :enable 'mark-active))
  (define-key hfyview-print-menu [hfyview-separator-pre]
    '(menu-item "--"))
  (define-key hfyview-print-menu [hfyview-browser-pre]
    '(menu-item "Print Preview Buffer" hfyview-buffer
                :help "Print preview buffer with web browser"
                :visible t))
  )

;;;###autoload
(defcustom hfyview-quick-print-in-files-menu nil
  "Add Quick print entries to File menu if non-nil.
If you set this to nil you have to restart Emacs to get rid of
the Quick Print entry."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (hfyview-add-to-files-menu)))
  :group 'hfy-view)

(defvar hfyview-print-visible t
  "Non-nil means show Quick Print entry on the file menu.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Interactive commands

;;;###autoload
(defun hfyview-buffer (arg)
  "Convert buffer to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window."
  (interactive "P")
  (hfyview-buffer-1 nil nil arg))

;;;###autoload
(defun hfyview-region (arg)
  "Convert region to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window."
  (interactive "P")
  (hfyview-buffer-1 (region-beginning) (region-end) arg))

;;;###autoload
(defun hfyview-window (arg)
  "Convert window to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window."
  (interactive "P")
  (hfyview-buffer-1 (window-start) (window-end) arg))

;;;###autoload
(defun hfyview-frame (whole-buffers)
  "Convert frame to html preserving faces and show in web browser.
Make an XHTML view of the current Emacs frame. Put it in a buffer
named *hfyview-frame* and show that buffer in a web browser.

If WHOLE-BUFFERS is non-nil then the whole content of the buffers
is shown in the XHTML page, otherwise just the part that is
visible currently on the frame.

If you turn on the minor mode `hfyview-frame-mode' you can also
get the minibuffer/echo area in the output. See this mode for
details.

With command prefix also show html source in other window."
  (interactive (list (y-or-n-p "Enter y for whole buffers, n for only visible part? ")))
  (let ((title "Emacs - Frame Dump")
        buf)
    (setq title (frame-parameter (selected-frame) 'name))
    (setq buf (hfyview-frame-1 whole-buffers title))
    (when current-prefix-arg
      (switch-to-buffer-other-window buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Internal commands

(defconst hfyview-modline-format
  ;; There seems to be a bug in Firefox that prevents this from
  ;; displaying correctly.  Anyway this is just a quick and reasonable
  ;; approximation.
  (concat "<div style=\"width:%sem; color:%s; background:%s; white-space:pre; overflow:hidden; font-family:monospace;\">"
          ;; Using <pre> gives empty line above and below
          ;;"<pre>"
          "-- (Unix)%s   <b>%s</b>    (%s%s) "
          (make-string 6 ?-)
          "%s" ;; Viper
          (make-string 200 ?-)
          ;;"</pre>"
          "</div>"))

(defun hfyview-get-minors ()
  "Return string with active minor mode highlighters."
  (let ((minors ""))
    (dolist (mr minor-mode-alist)
      (let ((mm (car mr))
            (ml (cadr mr)))
        (when (symbol-value mm)
          (when (stringp ml)
            (setq minors (concat minors ml))))))
    minors))

;; (hfyview-dekludge-string "<i> ")
(defun hfyview-dekludge-string (str)
  "Return html quoted string STR."
  (mapconcat (lambda (c)
               (hfy-html-quote
                (char-to-string c)))
             (append str)
             ""))

(defvar viper-mode-string) ;; Silence compiler

(defun hfyview-fontify-win-to (win tag whole-buffer)
  "Return html code for window WIN.
Sorround the code with the html tag <TAG>.
WHOLE-BUFFER corresponds to the similar argument for
`hfyview-frame-1'."
  (let* ((bstart (unless whole-buffer (window-start win)))
         (bend   (unless whole-buffer (window-end win)))
         (hbuf (hfyview-fontify-region bstart bend))
         (edges (window-edges win))
         (width  (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (border-color (or (hfy-triplet "SystemActiveBorder")
                          "gray"))
         start
         end
         css-start
         css-end
         mod-fgcolor
         mod-bgcolor
         mod-width
         mod
         bu-name
         ma-name
         minors
         (window-start-line (point-min))
         (window-end-line   (point-max))
         (is-selected-window (eq win hfyview-selected-window))
         (mark-viper "")
         )
    ;; Fix-me: fetch style too
    (with-current-buffer (window-buffer win)
      (unless whole-buffer
        (save-restriction
          (widen)
          (setq window-start-line (line-number-at-pos bstart))
          (setq window-end-line   (line-number-at-pos bend))
          (unless (or (< (line-number-at-pos (point-min)) window-start-line)
                      (> (line-number-at-pos (point-max)) window-end-line))
            (setq whole-buffer t))
          )
        )
      (setq mod-fgcolor (face-attribute (if is-selected-window 'mode-line 'mode-line-inactive) :foreground))
      (setq mod-bgcolor (face-attribute (if is-selected-window 'mode-line 'mode-line-inactive) :background))
      (setq mod-fgcolor (hfy-triplet mod-fgcolor))
      (setq mod-bgcolor (hfy-triplet mod-bgcolor))
      (setq mod (if (buffer-modified-p) "**" "--"))
      (when buffer-read-only
        (setq mod "%%"))
      (setq bu-name (buffer-name))
      (setq ma-name mode-name)
      (setq minors (hfyview-get-minors))
      (when (and (local-variable-p 'viper-mode-string) viper-mode-string)
        (setq mark-viper viper-mode-string))
      )
    ;; Compensate for scroll-bars
    (setq mod-width (+ width 1))
    (with-current-buffer hbuf
      (setq width (- width 2.5))
      (setq width (* 0.57 width))
      (setq height (+ height 2)) ;; For pre
      ;;(setq height (+ height 1.2)) ;; For horisontal scrollbar
      (setq height (* 1.16 height))
      (goto-char (point-min))
      (re-search-forward "<body.*?>")
      (setq start (point))
      (insert
       (format "<%s style=\"width:%sem; height:%sem; border: 1px solid %s; overflow:%s; padding:4px;\">\n"
               tag width height border-color
               (if whole-buffer "auto" "hidden") ;; overflow
               ))
      (goto-char (point-max))
      (setq end (search-backward "</body>"))
      (unless whole-buffer
        (insert
         (format "\n<div style=\"margin-top:2em; color: red; text-align: center; \"> Truncated to line %s - %s! </div>\n"
                 window-start-line window-end-line)))
      (insert "</" tag ">\n")
      ;;(lwarn t :warning "%s" mark-viper)
      (insert (format hfyview-modline-format
                      width
                      mod-fgcolor mod-bgcolor mod
                      (hfyview-dekludge-string bu-name)
                      (hfyview-dekludge-string ma-name)
                      (hfyview-dekludge-string minors)
                      (hfyview-dekludge-string mark-viper)))
      (setq end (point))
      (goto-char (point-min))
      (search-forward "<style type=\"text/css\"><!--")
      (beginning-of-line)
      (setq css-start (point))
      (search-forward "--></style>")
      (setq css-end (point))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))
    (list hbuf start end css-start css-end)))

;; (defun hfyview-window-framed ()
;;   "Just a test"
;;   (interactive)
;;   (let* ((res (hfyview-fontify-win-to (selected-window) "div" nil))
;;          (hbuf (nth 0 res)))
;;     (with-current-buffer hbuf
;;       (browse-url-of-buffer))))

(defun hfyview-fontify-tree-win (win whole-buffer)
  "Return html code for window WIN.
WHOLE-BUFFER corresponds to the similar argument for
`hfyview-frame-1'."
  (with-selected-window win
    (let* ((start (window-start))
           (end (window-end))
           (res (hfyview-fontify-win-to win "div" whole-buffer))
           (hbuf (nth 0 res)))
      (with-current-buffer hbuf
        (rename-buffer (generate-new-buffer-name (format "%s %s-%s" win start end))))
      ;;(lwarn t :warning "win=%s, hbuf=%s" win hbuf)
      res)))

(defun hfyview-fontify-tree (wt whole-buffers)
  "Return list of html code for all windows in tree WT.
WT should be the result of function `window-tree' or a subtree of
this. For WHOLE-BUFFERS see `hfyview-frame-1'."
  (if (not (listp wt))
      (hfyview-fontify-tree-win wt whole-buffers)
    (let ((ret))
      (dolist (w (cddr wt))
        (setq ret (cons (hfyview-fontify-tree w whole-buffers) ret)))
      (list (car wt) ret))))

(defun hfyview-frame-to-html (res)
  "Return list with css and html code for frame.
RES is the collected result from `hfyview-fontify-tree'."
  (let ((html "")
        (css "")
        (first (car res))
        (td "<td style=\"vertical-align:top;\">")
        h)
    (cond
     ((memq first '(nil t))
      (dolist (sub (reverse (cadr res)))
        (let* ((fres (hfyview-frame-to-html sub))
               (h    (nth 0 fres))
               (c    (nth 1 fres)))
          (when first (setq h (concat "<tr>\n" h "</tr>\n")))
          (setq html (concat html h))
          (setq css  (concat css c))))
      (unless first
        (setq html (concat "<tr>" html "</tr>\n")))
      (setq html (concat "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\">\n" html "</table>\n"))
      (setq html (concat td html "</td>\n"))
      )
     ((bufferp first)
      ;; (buf start end)
      (let* ((buf (nth 0 res))
             (sta (nth 1 res))
             (end (nth 2 res))
             (cst (nth 3 res))
             (cnd (nth 4 res))
             (h
              ;;(concat "<td>" "temp" "</td>\n")
              (with-current-buffer buf (buffer-substring-no-properties sta end)))
             (c
              ;;(concat "<td>" "temp" "</td>\n")
              (with-current-buffer buf (buffer-substring-no-properties cst cnd))))
        (setq h (concat td h
                        "</td>\n"))
        (setq html (concat html h))
        (setq css c)
        (kill-buffer buf)))
     (t
      (error "Uh?")))
    (list html css)))

(defconst hfyview-xhtml-header
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>%s</title>
<style type=\"text/css\"><!--
body { font-family: outline-courier new;  font-stretch: normal;  font-weight: 500;  font-style: normal;  color: rgb(0, 0, 0);  font-size: 10pt;  text-decoration: none; }
 --></style>
%s
  </head>
  <body>\n")

(defvar hfyview-xhtml-footer "</body>\n</html>\n")

(defun hfyview-wm-border-color ()
  "Return CSS code for color to use in window borders."
  (or (hfy-triplet "SystemActiveTitle")
      (hfy-triplet "blue")))

(defvar hfy-grabbed-echo-content nil)
(defvar hfy-grabbed-minibuffer-content nil)
(defvar hfyview-prompt-face nil)

(defun hfyview-frame-minibuff (use-grabbed)
  "Return html code for minibuffer.
If USE-GRABBED is non-nil use what has been grabbed by
`hfy-grab-echo-content' or `hfy-grab-minibuffer-content'.
Otherwise make a default content for the minibuffer."
  (if (and use-grabbed
           (or hfy-grabbed-echo-content
               hfy-grabbed-minibuffer-content))
      (let* ((str (if hfy-grabbed-echo-content
                      hfy-grabbed-echo-content
                    hfy-grabbed-minibuffer-content))
             (tmpbuf (get-buffer-create "*hfy-minibuff-temp*"))
             (hbuf (with-current-buffer tmpbuf
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       ;; Fix-me: move the propertize to a new
                       ;; copy-buffer in hfy-fontify-buffer. Explained
                       ;; in mail to Vivek.
                       (insert (propertize str
                                           'read-only nil
                                           'intangible nil
                                           'field nil
                                           'modification-hooks nil
                                           'insert-in-front-hooks nil
                                           'insert-behind-hooks nil
                                           'point-entered nil
                                           'point-left nil
                                           'font-sticky nil
                                           'rear-nonsticky nil
                                           ))
                       (htmlfontify-buffer))))
             bdy-start
             bdy-end
             bdy-txt
             css-start
             css-end
             css-txt)
        (with-current-buffer hbuf
          (goto-char (point-min))
          (search-forward "<style type=\"text/css\"><!--")
          (beginning-of-line)
          (setq css-start (point))
          (search-forward "--></style>")
          (setq css-end (point))
          (goto-char (point-min))
          (search-forward "<pre>")
          (setq bdy-start (point))
          (goto-char (point-max))
          (search-backward "</pre>")
          (setq bdy-end (point))
          (list (buffer-substring css-start css-end)
                (buffer-substring bdy-start bdy-end))))
    (let ((mini-bg (face-attribute hfyview-prompt-face :background))
          (mini-fg (face-attribute hfyview-prompt-face :foreground)))
      (if (eq mini-fg 'unspecified)
          (setq mini-fg "")
        (setq mini-fg (concat "color:" (hfy-triplet mini-fg) "; ")))
      (if (eq mini-bg 'unspecified)
          (setq mini-bg "")
        (setq mini-bg (concat "background:" (hfy-triplet mini-bg) "; ")))
      (list nil
            (concat
             "<span style=\"" mini-fg mini-bg "\">"
             "&nbsp;M-x "
             "</span>"
             "&nbsp;"
             "hfyview-frame"
             )))))

(defun hfyview-frame-1(whole-buffers frame-title)
  "Return buffer with html code for current frame.
If WHOLE-BUFFERS is non-nil then make scrollable buffers in the
html output. Otherwise just make html code for the currently
visible part of the buffers.

FRAME-TITLE is the title to show on the resulting html page."
  (let* ((wt (window-tree))
         (hfyview-selected-window (selected-window))
         (res (hfyview-fontify-tree (car wt) whole-buffers))
         (title-bg-color (hfyview-wm-border-color))
         (title-color (or (hfy-triplet "SystemHilightText")
                               "white"))
         (title-style (concat (format "background-color:%s; color:%s;" title-bg-color title-color)
                              "border: none; padding:4px; vertical-align: middle;"))
         (outbuf (get-buffer-create "frame"))
         html
         css
         ;; (face-attribute 'minibuffer-prompt :foreground)
         (hfyview-prompt-face (plist-get minibuffer-prompt-properties 'face))
         minibuf
         (frame-width (* 0.56 (frame-width)))
         table-style
         (icon-file (expand-file-name "../etc/images/icons/emacs_16.png" exec-directory))
         (img-tag (if (file-exists-p icon-file)
                      (concat "<img src=\"file://" icon-file "\" height=\"16\" width=\"16\" />")))
	 mini-css
	 mini-html
         )
    (setq table-style
          (format "border: solid %s; width:%sem;"
                  (hfyview-wm-border-color)
                  frame-width
                  ))
    (setq minibuf (hfyview-frame-minibuff hfyview-frame-mode))
    (setq mini-css  (nth 0 minibuf))
    (setq mini-html (nth 1 minibuf))
    (when (string= mini-html "") (setq mini-html "&nbsp;"))
    (setq res (hfyview-frame-to-html res))
    (setq html (nth 0 res))
    (setq css  (nth 1 res))
    (with-current-buffer outbuf
      ;;(lwarn t :warning "outbuf=%s" outbuf)
      (erase-buffer)
      (insert (format hfyview-xhtml-header
                      (concat "Emacs frame dump - " frame-title)
                      css)
              (if mini-css mini-css "")
              (format "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"%s\">\n" table-style)
              "<tr>\n"
              (format "<td style=\"%s\">%s&nbsp;&nbsp;%s</td>\n" title-style img-tag
                      (hfyview-dekludge-string frame-title))
              "</tr>\n"
              "<tr>\n"
              html
              "</tr>\n"
              "<tr>\n"
              "<td style=\"padding:1px;\">\n"
              mini-html
              "</td>\n"
              "</tr>\n"
              "</table>\n"
              hfyview-xhtml-footer)
      (browse-url-of-buffer)
      outbuf)))

(defun hfy-grab-echo-content ()
  "Return echo area content."
  (setq hfy-grabbed-echo-content (current-message)))

(defun hfy-grab-minibuffer-content ()
  "Return minibuffer content."
  ;;(interactive)
  (let* ((mw (minibuffer-window))
         (mb (window-buffer mw)))
    (setq hfy-grabbed-minibuffer-content
          (with-current-buffer mb
              (buffer-substring
               (point-min) (point-max)))
            )))

;;(add-hook 'pre-command-hook 'grab-minibuffer-content nil t)
;;(remove-hook 'pre-command-hook 'grab-minibuffer-content) t)

(provide 'hfyview)
;;; hfyview.el ends here

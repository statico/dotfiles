;;; html-write.el --- Hide some tags for writing text in XHTML
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-10-03T01:29:44+0200 Thu
(defconst html-write:version "0.6") ;; Version:
;; Last-Updated: 2009-08-11 Tue
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
;; The minor mode `html-write-mode' displays simple tags like <i>,
;; <b>, <em>, <strong> or <a> with appropriate faces (for example bold
;; and italic) instead of displaying the tags.
;;
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
;;
;;; Code:

;; Silence byte compiler
(defvar jit-lock-start)
(defvar jit-lock-end)

(eval-when-compile (require 'mumamo)) ;; Just for the defmacro ...
(eval-when-compile (require 'mlinks nil t))

;;;###autoload
(defgroup html-write nil
  "Customization group for html-write."
  :group 'nxhtml
  :group 'convenience)

(defface html-write-base
  '((t (:inherit font-lock-type-face)))
  "Face from which other faces inherits."
  :group 'html-write)

(defface html-write-em
  '((t (:inherit html-write-base :slant italic)))
  "Face used for <em> tags."
  :group 'html-write)

(defface html-write-strong
  '((t (:inherit html-write-base :weight bold)))
  "Face used for <strong> tags."
  :group 'html-write)

(defface html-write-link
  '((t (:inherit html-write-base :underline t)))
  "Face used for <a> tags."
  :group 'html-write)

(defconst html-write-tag-list
  '(("i"      html-write-em-tag-actions)
    ("b"      html-write-strong-tag-actions)
    ("em"     html-write-em-tag-actions)
    ("strong" html-write-strong-tag-actions)
    ("a"      html-write-a-tag-actions)
    ;;("img"    html-write-img-tag-actions t)
    )
  "List of tags that should be hidden.
A record in the list has the format

  \(TAG HANDLE [SINGLE])

where
- TAG is the tag name string.

- HANDLE is a function to call when hiding the tag. It takes
  three parameters, TAG-BEGIN, TAG-END and OVERLAY.  TAG-BEGIN
  and TAG-END are start and end of the start tag.  OVERLAY is an
  overlay used for faces, keymaps etc that covers the whole tag."
  )

(defun html-write-em-tag-actions (tag-begin tag-end overlay)
  "Do actions for <em> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-mode' for this tag."
  (overlay-put overlay 'face 'html-write-em))

(defun html-write-strong-tag-actions (tag-begin tag-end overlay)
  "Do actions for <strong> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-mode' for this tag."
  (overlay-put overlay 'face 'html-write-strong))

;; Fix-me
(defun html-write-img-tag-actions (tag-begin tag-end overlay)
  "Do actions for <img> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-mode' for this tag."
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "src=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'display (concat "image " href))
        (overlay-put overlay 'html-write-url href))
      (goto-char (point)))))

(defun html-write-point-entered-echo (left entered)
  (let ((msg (get-char-property entered 'help-echo)))
    (when msg (message "%s" msg))))

(defun html-write-a-tag-actions (tag-begin tag-end overlay)
  "Do actions for <a> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-mode' for this tag."
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "href=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'face 'html-write-link)
        (overlay-put overlay 'help-echo href)
        ;; Fix-me: Seems like point-entered must be a text prop
        (overlay-put overlay 'point-entered 'html-write-point-entered-echo)
        (overlay-put overlay 'mouse-face 'highlight)
        (if (eq ?# (string-to-char href))
            (setq href (concat "file:///" buffer-file-name href))
          (when (file-exists-p href)
            (setq href (expand-file-name href))))
        (overlay-put overlay 'html-write-url href))
      (goto-char (point)))))

(defun html-write-get-tag-ovl ()
  "Get tag overlay at current point."
  (catch 'ranges
    (dolist (ovl (overlays-at (point)))
      (let ((ranges (overlay-get ovl 'html-write)))
        (when ranges
          (throw 'ranges ovl))))))

(defun html-write-toggle-current-tag ()
  "Toggle display of tag at current point."
  (interactive)
  (let* ((ovl (html-write-get-tag-ovl))
         (hiding-ranges (overlay-get ovl 'html-write))
         (invis (get-text-property (caar hiding-ranges) 'invisible))
         (ovl-start (overlay-start ovl))
         (ovl-end (overlay-end ovl)))
    (if invis
        (progn
          (overlay-put ovl 'html-face (overlay-get ovl 'face))
          (overlay-put ovl 'face 'highlight)
          (dolist (range hiding-ranges)
            (let ((start (car range))
                  (end   (cdr range)))
              (mumamo-with-buffer-prepared-for-jit-lock
               (put-text-property start end 'invisible nil)))))
      (delete-overlay ovl)
      (html-write-hide-tags ovl-start ovl-end))))

(defun html-write-browse-link ()
  "Browse link in current tag."
  (interactive)
  (let* ((ovl (html-write-get-tag-ovl))
         (url (overlay-get ovl 'html-write-url)))
    (unless url
      (error "No link in this tag"))
    (browse-url url)
    ))

(defvar html-write-keymap
  (let ((map (make-sparse-keymap))
        keys)
    (define-key map [(control ?c) ?+] 'html-write-toggle-current-tag)
    (define-key map [(control ?c) ?!] 'html-write-browse-link)
    (define-key map [mouse-1] 'html-write-browse-link)
    (when (featurep 'mlinks)
      (setq keys (where-is-internal 'mlinks-goto mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto))
      (setq keys (where-is-internal 'mlinks-goto-other-window mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto-other-window))
      (setq keys (where-is-internal 'mlinks-goto-other-frame mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto-other-frame))
      )
    map))

(defun html-write-mlinks-goto ()
  "Goto link."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto))

(defun html-write-mlinks-goto-other-window ()
  "Goto link in other window."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto-other-window))

(defun html-write-mlinks-goto-other-frame ()
  "Goto link in other frame."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto-other-frame))

(defun html-write-mlinks-goto-1 (goto-fun)
  (let* ((ovl (html-write-get-tag-ovl))
         (ovl-start (overlay-start ovl))
         (ovl-end (overlay-end ovl))
         (here (point-marker)))
    (goto-char ovl-start)
    (skip-chars-forward "^\"" ovl-end)
    (forward-char)
    (unless (funcall goto-fun) (goto-char here))
    ))

;;(html-write-make-hide-tags-regexp)
(defun html-write-make-hide-tags-regexp ()
  "Make regexp used for finding tags to hide."
  ;; fix-me: single tags. Fix-me: what did I mean??? Maybe &lt; etc...
  (let ((tags-re
         (mapconcat 'identity
                    (mapcar (lambda (elt)
                              (if (stringp elt)
                                  elt
                                (car elt)))
                            html-write-tag-list)
                    "\\|")))
    (concat
     "<\\(?1:"
     "\\(?:" tags-re "\\)"
     "\\)[^>]*>\\(?3:[^<]*\\)\\(?2:</\\1>\\)"
     )))

(defvar html-write-pending-changes nil)
(make-variable-buffer-local 'html-write-pending-changes)
(put 'html-write-pending-changes 'permanent-local t)


(defun html-write-hide-tags (start end)
  "Hide tags matching `html-write-tag-list' between START and END."
  ;;(message "html-write-hide-tags %s %s" start end)
  (let ((here (point-marker))
        (buffer-name (buffer-file-name))
        (dbg nil))
    (save-restriction
      (widen)
      (goto-char start)
      (save-match-data
        (let ((hide-tags-regexp (html-write-make-hide-tags-regexp)))
          (when dbg (message "before search start=%s end=%s, point=%s" start end (point)))
          (while (re-search-forward hide-tags-regexp end t)
            (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)
                                      nil t nil))
                   (tag-fun (cadr (assoc (match-string-no-properties 1)
                                         html-write-tag-list)))
                   hiding-ranges)
              ;;(overlay-put ovl 'face 'font-lock-variable-name-face)
              (overlay-put ovl 'keymap html-write-keymap)
              (setq hiding-ranges
                    (list (cons (1- (match-beginning 1)) (match-beginning 3))
                          (cons (match-beginning 2) (match-end 2))))
              (overlay-put ovl 'html-write hiding-ranges)
              (mumamo-with-buffer-prepared-for-jit-lock
               (dolist (range hiding-ranges)
                 (let ((start (car range))
                       (end   (cdr range)))
                   (put-text-property start end 'invisible 'html-write)
                   ;; Fix-me: more careful rear-nonsticky?
                   (put-text-property (1- end) end
                                      'rear-nonsticky '(invisible)))))
              ;; Let tag-fun override
              (when tag-fun
                (funcall tag-fun (match-end 1) (match-beginning 3) ovl))
              )))))
    (goto-char here)))

(defun html-write-reveal-tags (start end)
  "Reveal tags between START and END."
  (let ((here (point-marker)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
        (mumamo-with-buffer-prepared-for-jit-lock
         (remove-text-properties start
                                 end
                                 '(invisible html-write))
         (dolist (ovl (overlays-in start end))
           (when (overlay-get ovl 'html-write)
             (let ((end (overlay-end ovl)))
               (remove-list-of-text-properties (1- end) end '(rear-nonsticky))
               (delete-overlay ovl)))))))
    (goto-char here)))

;;;###autoload
(define-minor-mode html-write-mode
  "Minor mode for convenient display of some HTML tags.
When this mode is on a tag in `html-write-tag-list' is displayed as
the inner text of the tag with a face corresponding to the tag.
By default for example <i>...</i> is displayed as italic and
<a>...</a> is displayed as an underlined clickable link.

Only non-nested tags are hidden.  The idea is just that it should
be easier to read and write, not that it should look as html
rendered text.

See the customization group `html-write' for more information about
faces.

The following keys are defined when you are on a tag handled by
this minor mode:

\\{html-write-keymap}

IMPORTANT: Most commands you use works also on the text that is
hidden.  The movement commands is an exception, but as soon as
you edit the buffer you may also change the hidden parts.

Hint: Together with `wrap-to-fill-column-mode' this can make it
easier to see what text you are actually writing in html parts of
a web file."
  :group 'html-write
  (if t
      (if html-write-mode
          (html-write-font-lock t)
        (html-write-font-lock nil)
        (save-restriction
          (widen)
          (html-write-reveal-tags (point-min) (point-max))))))
(put html-write-mode 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font lock

(defun html-write-jit-extend-after-change (start end old-len)
  "For JIT lock extending.
Should be on `jit-lock-after-change-extend-region-functions'.

START, END and OLD-LEN are the parameters from after change."
  (let ((our-ovls nil))
    (dolist (ovl (append (overlays-in start end)
                        (overlays-at start)
                        nil))
      ;; Leave the overlays until re-fontification time, but note their extent.
      (when (overlay-get ovl 'html-write)
        (setq jit-lock-start (min jit-lock-start (overlay-start ovl)))
        (setq jit-lock-end   (max jit-lock-end   (overlay-end   ovl)))))))


(defun html-write-fontify (bound)
  ;;(message "html-write-fontify %s" bound)
  (let (tag-ovl)
    ;;(save-match-data
      (let* ((hide-tags-regexp (html-write-make-hide-tags-regexp))
             (next-tag (re-search-forward hide-tags-regexp bound t))
             (tag-beg (when next-tag (match-beginning 0)))
             (tag-end (when next-tag (match-end 0)))
             (tag-nam (when next-tag (match-string-no-properties 1)))
             (tag-fun (when next-tag (cadr (assoc tag-nam html-write-tag-list))))
             tag-hid
             (old-start (next-single-char-property-change (max (point-min) (1- (point))) 'html-write nil bound)))
        ;;(message "here a old-start=%s, tag-beg/end=%s/%s" old-start tag-beg tag-end)
        (setq tag-ovl (when next-tag (make-overlay tag-beg tag-end)))
        (when old-start
          ;; Fix-me: maybe valid, perhaps better keep it then?
          (let ((ovl (catch 'ovl
                       (dolist (o (append (overlays-at old-start)
                                          (overlays-in old-start (1+ old-start))
                                          nil))
                         (when (overlay-get o 'html-write)
                           (throw 'ovl o))))))
            (when ovl ;; fix-me: there should be one...
              ;;(message "here b")
              (mumamo-with-buffer-prepared-for-jit-lock
               (remove-list-of-text-properties (overlay-start ovl) (overlay-end ovl) '(invisible html-write)))
              (delete-overlay ovl))))
        ;;(html-write-hide-tags start end)
        ;;(message "here d, tag-ovl=%s" tag-ovl)
        (when tag-ovl
          (overlay-put tag-ovl 'face 'font-lock-variable-name-face)
          (overlay-put tag-ovl 'keymap html-write-keymap)
          (setq tag-hid
                (list (cons (1- (match-beginning 1)) (match-beginning 3))
                      (cons (match-beginning 2) (match-end 2))))
          (overlay-put tag-ovl 'html-write tag-hid)
          (when tag-fun
            (funcall tag-fun (match-end 1) (match-beginning 3) tag-ovl))
          (mumamo-with-buffer-prepared-for-jit-lock
           (dolist (range tag-hid)
             (let ((start (car range))
                   (end   (cdr range)))
               (put-text-property start end 'invisible 'html-write)
               ;;(put-text-property start end 'html-write t)
               ;; Fix-me: more careful rear-nonsticky?
               (put-text-property (1- end) end
                                  'rear-nonsticky '(invisible)))))))
      ;;)
    (when tag-ovl
      (set-match-data (list (copy-marker (overlay-start tag-ovl))
                            (copy-marker (overlay-end tag-ovl))))
      (goto-char (1+ (overlay-end tag-ovl)))
      t)))

(defun html-write-font-lock (on)
  ;; See mlinks.el
  (let* ((add-or-remove (if on 'font-lock-add-keywords 'font-lock-remove-keywords))
         (fontify-fun 'html-write-fontify)
         (args (list nil `(( ,fontify-fun ( 0 'html-write-base t ))))))
    (when fontify-fun
      (when on (setq args (append args (list t))))
      (apply add-or-remove args)
      (font-lock-mode -1)
      (font-lock-mode 1)
      )))

(provide 'html-write)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-write.el ends here

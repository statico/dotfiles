;;; mumamo-fun.el --- Multi major mode functions
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-09T01:35:21+0100 Sun
;; Version: 0.51
;; Last-Updated: 2008-08-04T17:54:29+0200 Mon
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl', `flyspell', `ispell', `mumamo',
;;   `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines some "multi major modes" functions.  See mumamo.el for more
;; information.
;;
;;;; Usage:
;;
;;  See mumamo.el for how to use the multi major mode functions
;;  defined here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (add-to-list 'load-path default-directory))
(eval-when-compile (require 'mumamo))
(eval-when-compile (require 'sgml-mode))
;;(mumamo-require)

;;;#autoload
;;(defun mumamo-fun-require ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File wide key bindings

(defun mumamo-multi-mode-map ()
  "Return mumamo multi mode keymap."
  (symbol-value
   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-map"))))

;; (defun mumamo-multi-mode-hook-symbol ()
;;   "Return mumamo multi mode hook symbol."
;;   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-hook")))

;;;###autoload
(defun mumamo-define-html-file-wide-keys ()
  "Define keys in multi major mode keymap for html files."
  (let ((map (mumamo-multi-mode-map)))
    (define-key map [(control ?c) (control ?h) ?b] 'nxhtml-browse-file)
    ))
;; (defun mumamo-add-html-file-wide-keys (hook)
;;   (add-hook hook 'mumamo-define-html-file-wide-keys)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk search routines for XHTML things

(defun mumamo-chunk-attr= (pos min max attr= attr=is-regex attr-regex submode)
  "This should work similar to `mumamo-find-possible-chunk'.
See `mumamo-chunk-style=' for an example of use.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-chunk-attr=-new pos max attr= attr=is-regex attr-regex submode))

(defun mumamo-chunk-attr=-new-fw-exc-fun (pos max)
  ;;(msgtrc "(mumamo-chunk-attr=-new-fw-exc-fun %s %s)" pos max)
  (save-match-data
    (let ((here (point))
          first-dq
          next-dq
          (this-chunk (mumamo-get-existing-new-chunk-at pos)))
      (if this-chunk
          (goto-char (overlay-end this-chunk))
        (goto-char (overlay-end mumamo-last-chunk)))
      (setq first-dq (search-forward "\"" max t))
      (unless (bobp)
        (backward-char)
        (condition-case err
            (with-syntax-table (standard-syntax-table)
              (setq next-dq (scan-sexps (point) 1)))
          (error nil)))
      (prog1
          next-dq
        (goto-char here)))))

(defun mumamo-chunk-attr=-new-find-borders-fun (start-border end-border dummy)
  ;;(setq borders (funcall find-borders-fun start-border end-border exc-mode))
  (save-match-data
    (let ((here (point))
          (end2 (when end-border (1- end-border)))
          start2)
      (goto-char start-border)
      (save-match-data
        (setq start2 (search-forward "\"" (+ start-border 200) t)))
      (goto-char here)
      (list start2 end2))))

(defun mumamo-chunk-attr=-new (pos
                               ;;min
                               max
                               attr=
                               attr=is-regex
                               attr-regex
                               submode)
  ;;(message "\n(mumamo-chunk-attr=-new %s %s %s %s %s %s)" pos max attr= attr=is-regex attr-regex submode)
  ;;(mumamo-condition-case err
  (condition-case err
      (save-match-data
        (let ((here (point))
              (next-attr= (progn
                            ;; fix-me:
                            (if (not attr=is-regex)
                                (goto-char (+ pos (length attr=)))
                              (goto-char pos)
                              (skip-chars-forward "a-zA-Z="))
                            (goto-char pos)
                            (if attr=is-regex
                                (re-search-forward attr= max t)
                              (search-forward attr= max t))))
              next-attr-sure
              ;;next-attr=
              start start-border
              end   end-border
              exc-mode
              borders
              exc-start-next
              exc-end-next
              exc-start-next
              exc-end-next
              (tries 0)
              (min (1- pos))
	      look-max
              )
          ;; make sure if we have find prev-attr= or not
          (unless (eq (char-after) ?\")
            (setq next-attr= nil))
          (when next-attr=
            (forward-char)
            (skip-chars-forward "^\"")
            (setq look-max (+ (point) 2)))
          (while (and next-attr=
                      (< min (point))
                      (not next-attr-sure)
                      (< tries 5))
            ;;(msgtrc "attr=-new: min=%s, point=%s" min (point))
            (setq tries (1+ tries))
            ;;(if (not (re-search-backward "<[^?]" (- min 300) t))
            (if (not (re-search-backward "<[^?]\\|\?>" (- min 300) t))
                (setq next-attr= nil)
              ;;(if (looking-at attr-regex)
              (if (let ((here (point)))
                    (prog1
                        (re-search-forward attr-regex look-max t)
                      (goto-char here)))
              ;;(if (mumamo-end-in-code (point) next-attr= 'php-mode)
                  (setq next-attr-sure 'found)
                (unless (bobp)
                  (backward-char)
                  ;;(msgtrc "attr=-new 1: min=%s, point=%s" min (point))
                  (setq next-attr= (if attr=is-regex
                                       (re-search-backward attr= (- min 300) t)
                                     (search-backward attr= (- min 300) t)))))))
          (unless next-attr-sure (setq next-attr= nil))


          ;; find prev change and if inside style= the next change
          (when next-attr=
              (setq exc-start-next (match-beginning 1))
              (setq exc-end-next   (match-end 2))
              (when (>= exc-start-next pos)
                (if (> pos exc-end-next)
                    (progn
                      (setq start (+ (match-end 2) 1))
                      ;;(setq start-border (+ (match-end 2) 2))
                      )
                  (setq exc-mode submode)
                  (setq start (match-beginning 1))
                  (setq start-border (match-beginning 2))
                  (setq end (1+ (match-end 2)))
                  (setq end-border (1- end)))
                ))
          ;; find next change
          (unless end
            (if start
                (goto-char start)
              (goto-char pos)
              (search-backward "<" min t))
            ;;(msgtrc "attr=-new 2: min=%s, point=%s" min (point))
            (setq next-attr= (if attr=is-regex
                                 (re-search-forward attr= max t)
                               (search-forward attr= max t)))
            (when (and next-attr=
                       (search-backward "<" min t))
              (when (looking-at attr-regex)
                (setq end (match-beginning 1)))))
          (when start (assert (>= start pos) t))
          (when end   (assert (<= pos end) t))
          ;;(message "start-border=%s end-border=%s" start-border end-border)
          (when (or start-border end-border)
            (setq borders (list start-border end-border nil)))
          ;; (message "mumamo-chunk-attr=-new: %s"
          ;;          (list start
          ;;                end
          ;;                exc-mode
          ;;                borders
          ;;                nil ;; parseable-by
          ;;                'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
          ;;                'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
          ;;                ))
          (goto-char here)
          (setq end nil)
          (when (or start end)
            (list start
                  end
                  exc-mode
                  borders
                  nil ;; parseable-by
                  'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
                  'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
                  ))))
    (error (mumamo-display-error 'mumamo-chunk-attr=-new "%s" (error-message-string err)))
    ))

;;;; xml pi

(defvar mumamo-xml-pi-mode-alist
  '(("php"    . php-mode)
    ("python" . python-mode))
  "Alist used by `mumamo-chunk-xml-pi' to get exception mode." )

;; Fix-me: make it possible to make the borders part of the php chunk
;; so that parsing of them by nxml may be skipped. Or, rather if the
;; borders are not part of the chunk then assume nxml can not parse
;; the chunk and the borders.
;; (defun mumamo-search-bw-exc-start-xml-pi-1 (pos min lt-chars)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop.
;; LT-CHARS is just <?.

;; Actual use is in `mumamo-search-bw-exc-start-xml-pi'."
;;   (let ((exc-start (mumamo-chunk-start-bw-str (+ pos 2) min lt-chars))
;;         spec
;;         exc-mode
;;         hit)
;;     (when exc-start
;;       (goto-char exc-start)
;;       (when (and (not (looking-at "xml"))
;;                  (looking-at (rx (0+ (any "a-z")))))
;;         ;; (setq exc-start (match-end 0)) include it in sub chunk instead
;;         (setq exc-start (- exc-start 2))
;;         (setq spec (match-string-no-properties 0))
;;         (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
;;         (when exc-mode (setq exc-mode (cdr exc-mode)))
;;         (setq hit t)
;;         )
;;       (when hit
;;         (unless exc-mode
;;           ;;(setq exc-mode 'fundamental-mode)
;;           ;; Fix-me: Better assume php-mode
;;           (setq exc-mode 'php-mode))
;;         (when (<= exc-start pos)
;;           ;;(cons exc-start exc-mode)
;;           (list exc-start exc-mode nil)
;;           )))))

;; (defun mumamo-search-bw-exc-start-xml-pi (pos min)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-search-bw-exc-start-xml-pi-1 pos min "<?"))

(defun mumamo-search-fw-exc-start-xml-pi-new (pos max)
  (let ((here (point))
        start
        spec
        exc-mode
        ret)
    (setq start (search-forward "<?" max t))
    (when (and start
               (looking-at (rx (0+ (any "a-z")))))
      (setq spec (match-string-no-properties 0))
      (unless (string= spec "xml")
        (when (= 0 (length spec))
          (setq spec "php"))
        (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
        (if exc-mode
            (setq exc-mode (cdr exc-mode))
          (setq exc-mode 'mumamo-bad-mode))
        (setq ret (list (- start 2) exc-mode nil))))
    (goto-char here)
    ret))

(defun mumamo-xml-pi-end-is-xml-end (pos)
  "Return t if the ?> at pos is end of <?xml."
  (when (> 1000 pos)
;;;     (assert (and (= (char-after pos) ??)
;;;                  (= (char-after (1+ pos)) ?>)))
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (when (search-backward "<" (- pos 150) t)
            (when (looking-at (rx line-start "<\?xml" (1+ space)))
              (mumamo-msgfntfy "mumamo-xml-pi-end-is-xml-end %s => t" pos)
              t)))))))

;; (defun mumamo-search-bw-exc-end-xml-pi (pos min)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop."
;;   ;; Fix me: merge xml header
;;   (mumamo-msgfntfy "mumamo-search-bw-exc-end-xml-pi %s %s" pos min)
;;   ;;(let ((end-pos (mumamo-chunk-end-bw-str pos min "?>")))
;;   (let ((end-pos (mumamo-chunk-end-bw-str-inc pos min "?>")))
;;     (mumamo-msgfntfy "  end-pos=%s" end-pos)
;;     (when end-pos
;;       (unless (or (mumamo-xml-pi-end-is-xml-end end-pos)
;;                   (= (save-restriction
;;                        (widen)
;;                        (char-after (- end-pos 1)))
;;                      ?<))
;;         (mumamo-msgfntfy "  returning end-pos")
;;         end-pos))))

(defun mumamo-search-fw-exc-end-xml-pi (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  ;; Fix me: merge xml header
  ;;(let ((end-pos (mumamo-chunk-end-fw-str pos max "?>")))
  (save-match-data
    (let ((end-pos (mumamo-chunk-end-fw-str-inc pos max "?>")))
      (when end-pos
        (unless (mumamo-xml-pi-end-is-xml-end end-pos)
          end-pos)))))

(defun mumamo-search-fw-exc-start-xml-pi-1 (pos max lt-chars)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop.

Used in `mumamo-search-fw-exc-start-xml-pi'.  For an explanation
of LT-CHARS see `mumamo-search-bw-exc-start-xml-pi-1'."
  (goto-char pos)
  (skip-chars-backward "a-zA-Z")
  ;;(let ((end-out (mumamo-chunk-start-fw-str (point) max lt-chars)))
  (let ((end-out (mumamo-chunk-start-fw-str-inc (point) max lt-chars))
        spec
        exc-mode
        hit)
    (when (looking-at "xml")
      (if t ;(= 1 pos)
          (setq end-out (mumamo-chunk-start-fw-str-inc (1+ (point)) max lt-chars))
        (setq end-out nil)))
    (when end-out
      ;; Get end-out:
      (if (looking-at (rx (0+ (any "a-z"))))
          (progn
            ;;(setq end-out (match-end 0))
            (setq end-out (- (match-beginning 0) 2))
            (setq spec (match-string-no-properties 0))
            (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
            (if exc-mode
                (setq exc-mode (cdr exc-mode))
              (setq exc-mode 'php-mode))
            (setq end-out (list end-out exc-mode nil))
            )
        (setq end-out nil))
      end-out)))

(defun mumamo-search-fw-exc-start-xml-pi-old (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  (mumamo-search-fw-exc-start-xml-pi-1 pos max "<?"))

;; Add a find-borders-fun here so that for example src="<?php some
;; code ?>" can be handled.
;;
;; Fix-me: Maybe generalize for other values than <?php
(defun mumamo-find-borders-xml-pi (start end exc-mode)
  (let (start-border
        end-border
        (inc t)
        ;;(begin-mark "<?php")
        (begin-mark "<?")
        (end-mark "?>")
        (here (point)))
    (if (and inc) ;; exc-mode)
        (progn
          (when start
            ;;(setq start-border (+ start (length begin-mark)))
            (goto-char (+ start (length begin-mark)))
            (skip-chars-forward "=a-zA-Z")
            (setq start-border (point))
            )
          (when end
            (setq end-border
                  (- end (length end-mark)))))
      (if (and (not inc) (not exc-mode))
          (progn
            (when start
              (setq start-border
                    (+ start (length end-mark))))
            (when end
              (setq end-border (- end (length begin-mark)))
              ;;(goto-char end)
              ;;(skip-chars-forward "=a-zA-Z")
              ;;(setq end-border (point))
              ))))
    (goto-char here)
    (when (or start-border end-border)
      (list start-border end-border))))

(defun mumamo-chunk-xml-pi (pos min max)
  "Find process instruction, <? ... ?>.  Return range and wanted mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-xml-pi
  ;;                             'mumamo-search-bw-exc-end-xml-pi
  ;;                             'mumamo-search-fw-exc-start-xml-pi-old
  ;;                             'mumamo-search-fw-exc-end-xml-pi
  ;;                             'mumamo-find-borders-xml-pi)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-xml-pi-new
                                 'mumamo-search-fw-exc-end-xml-pi
                                 'mumamo-find-borders-xml-pi))


;;;; <style ...>

(defconst mumamo-style-tag-start-regex
  (rx "<style"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      "text/css"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[")
      ))

;; (defun mumamo-search-bw-exc-start-inlined-style (pos min)
;;   "Helper for `mumamo-chunk-inlined-style'.
;; POS is where to start search and MIN is where to stop."
;;   (goto-char (+ pos 6))
;;   (let ((marker-start (search-backward "<style" min t))
;;         exc-mode
;;         exc-start)
;;     (when marker-start
;;       (when (looking-at mumamo-style-tag-start-regex)
;;         (setq exc-start (match-end 0))
;;         (goto-char exc-start)
;;         (when (<= exc-start pos)
;;           ;;(cons (point) 'css-mode)
;;           ;;(list (point) 'css-mode '(nxml-mode))
;;           ;; Fix-me: Kubica looping problem
;;           (list (point) 'css-mode)
;;           )
;;         ))))

;; (defun mumamo-search-bw-exc-end-inlined-style (pos min)
;;   "Helper for `mumamo-chunk-inlined-style'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</style>"))

;; (defun mumamo-search-fw-exc-start-inlined-style-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-style'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<style" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 6))
;;       (when (looking-at mumamo-style-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-search-fw-exc-end-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</style>")))

;; (defun mumamo-chunk-inlined-style-old (pos min max)
;;   "Find <style>...</style>.  Return range and 'css-mode.
;; See `mumamo-find-possible-chunk' for POS, MIN and MAX."
;;   (mumamo-find-possible-chunk pos min max
;;                               'mumamo-search-bw-exc-start-inlined-style
;;                               'mumamo-search-bw-exc-end-inlined-style
;;                               'mumamo-search-fw-exc-start-inlined-style-old
;;                               'mumamo-search-fw-exc-end-inlined-style))

(defun mumamo-search-fw-exc-start-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<style" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 6))
      (when (looking-at mumamo-style-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'css-mode nil)
        ))))

(defun mumamo-chunk-inlined-style (pos min max)
  "Find <style>...</style>.  Return range and 'css-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-style
                                 'mumamo-search-fw-exc-end-inlined-style))

;;;; <script ...>

(defconst mumamo-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      "text/javascript"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

;; (defun mumamo-search-bw-exc-start-inlined-script (pos min)
;;   "Helper for `mumamo-chunk-inlined-script'.
;; POS is where to start search and MIN is where to stop."
;;   (goto-char (+ pos 7))
;;   (let ((marker-start (when (< min (point)) (search-backward "<script" min t)))
;;         exc-mode
;;         exc-start)
;;     (when marker-start
;;       (when (looking-at mumamo-script-tag-start-regex)
;;         (setq exc-start (match-end 0))
;;         (goto-char exc-start)
;;         (when (<= exc-start pos)
;;           ;;(cons (point) 'javascript-mode)
;;           (list (point) 'javascript-mode '(nxml-mode))
;;           )
;;         ))))

;; (defun mumamo-search-bw-exc-end-inlined-script (pos min)
;;   "Helper for `mumamo-chunk-inlined-script'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</script>"))

;; (defun mumamo-search-fw-exc-start-inlined-script-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-script'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<script" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 7))
;;       (when (looking-at mumamo-script-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-search-fw-exc-end-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</script>")))

;; (defun mumamo-chunk-inlined-script-old (pos min max)
;;   "Find <script>...</script>.  Return range and 'javascript-mode.
;; See `mumamo-find-possible-chunk' for POS, MIN and MAX."
;;   (mumamo-find-possible-chunk pos min max
;;                               'mumamo-search-bw-exc-start-inlined-script
;;                               'mumamo-search-bw-exc-end-inlined-script
;;                               'mumamo-search-fw-exc-start-inlined-script-old
;;                               'mumamo-search-fw-exc-end-inlined-script))

(defun mumamo-search-fw-exc-start-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-script-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode '(nxml-mode))
        ))))

(defun mumamo-chunk-inlined-script (pos min max)
  "Find <script>...</script>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;; on[a-z]+=\"javascript:"

(defconst mumamo-onjs=-attr=
  (rx
   ;;"on[a-z]+="
   (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
   "="))

(defconst mumamo-onjs=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       ;;"on" (1+ (any "a-za-z"))
       (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
       "=")
      (0+ space)
      ?\"
      (submatch
       (opt "javascript:")
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-onjs=(pos min max)
  "Find javascript on...=\"...\".  Return range and 'javascript-mode."
  (mumamo-chunk-attr= pos min max mumamo-onjs=-attr= t mumamo-onjs=-attr-regex
                      'javascript-mode))

;;;; py:somthing=\"python\"

(defconst mumamo-py:=-attr= "py:[a-z]+=")

(defconst mumamo-py:=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       "py:" (1+ (any "a-za-z"))
       "=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-py:=(pos min max)
  "Find python py:...=\"...\".  Return range and 'python-mode."
  (mumamo-chunk-attr= pos min max mumamo-py:=-attr= t mumamo-py:=-attr-regex
                      'python-mode))

(defun mumamo-chunk-py:match (pos min max)
  (save-match-data
    (let ((here (point))
          (py:match (progn
                      (goto-char pos)
                      (re-search-forward (rx "py:match"
                                             (1+ space)
                                             (0+ (not (any ">")))
                                             word-start
                                             (submatch "path=")
                                             (0+ space)
                                             ?\"
                                             (submatch
                                              (0+
                                               (not (any "\"")))))
                                         max t)))
          start end borders
          )
      (when py:match
        (setq start (match-beginning 1))
        (setq end   (match-end 2))
        (setq borders (list (match-end 1) (1- end)))
        )
      (goto-char here)
      (when start
        (list start
              end
              'python-mode
              borders
              nil ;; parseable-by
              'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
              'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
            )))))

;;;; style=

(defconst mumamo-style=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      (submatch "style=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-style=(pos min max)
  "Find style=\"...\".  Return range and 'css-mode."
  (mumamo-chunk-attr= pos min max "style=" nil mumamo-style=start-regex
                      'css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HTML w html-mode

(put 'mumamo-alt-php-tags-mode 'permanent-local t)
(define-minor-mode mumamo-alt-php-tags-mode
  "Minor mode for using '(?php' instead of '<?php' in buffer.
When turning on this mode <?php is replace with (?php in the buffer.
If you write the buffer to file (?php is however written as <?php.

When turning off this mode (?php is replace with <?php in the buffer.

The purpose of this minor mode is to work around problems with
using the `nxml-mode' parser in php files.  `nxml-mode' knows
damned well that you can not have the character < in strings and
I can't make it forget that.  For PHP programmers it is however
very convient to use <?php ... ?> in strings.

There is no reason to use this minor mode unless you want XML
validation and/or completion in your php file.  If you do not
want that then you can simply use a multi major mode based on
`html-mode' instead of `nxml-mode'/`nxhtml-mode'.  Or, of course,
just `php-mode' if there is no html code in the file."
  :lighter "<?php "
  (if mumamo-alt-php-tags-mode
      (progn
        ;;(unless mumamo-multi-major-mode (error "Only for mumamo multi major modes"))
        (unless (let ((major-mode (mumamo-main-major-mode)))
                  (derived-mode-p 'nxml-mode))
          ;;(error "Mumamo multi major mode must be based on nxml-mode")
          )
        (unless (memq 'mumamo-chunk-alt-php (caddr mumamo-current-chunk-family))
          (error "Mumamo multi major must have chunk function mumamo-chunk-alt-php"))

        ;; Be paranoid about the file/content write hooks
        (when (<= emacs-major-version 22)
          (with-no-warnings
            (when local-write-file-hooks ;; obsolete, but check!
              (error "Will not do this because local-write-file-hooks is non-nil"))))
        (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)
        (when write-contents-functions
          (error "Will not do this because write-contents-functions is non-nil"))
        (when (delq 'recentf-track-opened-file (copy-sequence write-file-functions))
          (error "Will not do this because write-file-functions is non-nil"))

        (add-hook 'write-contents-functions 'mumamo-alt-php-write-contents t t)
        (put 'write-contents-functions 'permanent-local t)
        (save-restriction
          (let ((here (point)))
            (widen)
            (goto-char (point-min))
            (while (search-forward "<?php" nil t)
              (replace-match "(?php"))
            (goto-char (point-min))
            (while (search-forward "<?=" nil t)
              (replace-match "(?="))
            (goto-char (point-min))
            (while (search-forward "?>" nil t)
                (replace-match "?)"))
            (goto-char here))))
    (save-restriction
      (let ((here (point)))
        (widen)
        (goto-char (point-min))
        (while (search-forward "(?php" nil t)
          (replace-match "<?php"))
        (goto-char (point-min))
        (while (search-forward "(?=" nil t)
          (replace-match "<?="))
        (goto-char (point-min))
        (while (search-forward "?)" nil t)
          (replace-match "?>"))
        (goto-char here)))
    (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)))

(defun mumamo-chunk-alt-php (pos min max)
  "Find (?php ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-static-chunk pos min max "(?php" "?)" t 'php-mode t)))

(defun mumamo-chunk-alt-php= (pos min max)
  "Find (?= ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-static-chunk pos min max "(?=" "?)" t 'php-mode t)))

;;;###autoload
(define-mumamo-multi-major-mode html-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP."
  ("HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-alt-php=
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'html-mumamo-mode)

;; (define-mumamo-multi-major-mode xml-pi-only-mumamo-mode
;;   "Test"
;;   ("HTML Family" html-mode
;;    (mumamo-chunk-xml-pi
;;     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XHTML w nxml-mode

(defun mumamo-alt-php-write-contents ()
  "For `write-contents-functions' when `mumamo-chunk-alt-php' is used."
  (let ((here (point)))
    (save-match-data
      (save-restriction
        (widen)
        (condition-case nil
            (atomic-change-group
              (progn
                (goto-char (point-min))
                (while (search-forward "(?php" nil t)
                  (replace-match "<?php"))
                (goto-char (point-min))
                (while (search-forward "(?=" nil t)
                  (replace-match "<?="))
                (goto-char (point-min))
                (while (search-forward "?)" nil t)
                  (replace-match "?>"))
                (basic-save-buffer-1)
                (signal 'mumamo-error-ind-0 nil)))
          (mumamo-error-ind-0)))
      (set-buffer-modified-p nil))
    (goto-char here))
  ;; saved, return t
  t)

;;;###autoload
(define-mumamo-multi-major-mode nxml-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'."
  ("nXml Family" nxml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'nxml-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mason (not ready)
;; http://www.masonhq.com/docs/manual/Devel.html#examples_and_recommended_usage

(defun mumamo-chunk-mason-perl-line (pos min max)
  (mumamo-whole-line-chunk pos min max "%" 'perl-mode))

(defun mumamo-chunk-mason-perl-single (pos min max)
  (mumamo-quick-static-chunk pos min max "<% " " %>" t 'perl-mode t))

(defun mumamo-chunk-mason-perl-block (pos min max)
  (mumamo-quick-static-chunk pos min max "<%perl>" "</%perl>" t 'perl-mode t))

(defun mumamo-chunk-mason-perl-init (pos min max)
  (mumamo-quick-static-chunk pos min max "<%init>" "</%init>" t 'perl-mode t))

(defun mumamo-chunk-mason-perl-once (pos min max)
  (mumamo-quick-static-chunk pos min max "<%once>" "</%once>" t 'perl-mode t))

(defun mumamo-chunk-mason-perl-cleanup (pos min max)
  (mumamo-quick-static-chunk pos min max "<%cleanup>" "</%cleanup>" t 'perl-mode t))

(defun mumamo-chunk-mason-perl-shared (pos min max)
  (mumamo-quick-static-chunk pos min max "<%shared>" "</%shared>" t 'perl-mode t))

(defun mumamo-chunk-mason-simple-comp (pos min max)
  (mumamo-quick-static-chunk pos min max "<& " " &>" t 'text-mode t))

(defun mumamo-chunk-mason-args (pos min max)
  ;; Fix-me: perl-mode is maybe not the best here?
  (mumamo-quick-static-chunk pos min max "<%args>" "</%args>" t 'perl-mode t))

(defun mumamo-chunk-mason-doc (pos min max)
  (mumamo-quick-static-chunk pos min max "<%doc>" "</%doc>" t 'mumamo-comment-mode t))

(defun mumamo-chunk-mason-text (pos min max)
  (mumamo-quick-static-chunk pos min max "<%text>" "</%text>" t 'text-mode t))

;; component calls with content

;; (defun mumamo-chunk-mason-compcont-bw-exc-start-fun (pos min)
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<&| ")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'html-mode))))

;; (defun mumamo-chunk-mason-compcont-fw-exc-start-fun-old (pos max)
;;   (mumamo-chunk-start-fw-str-inc pos max "<&| "))

(defun mumamo-chunk-mason-compcont-fw-exc-end-fun (pos max)
  (mumamo-chunk-end-fw-str-inc pos max "</&>"))

(defun mumamo-chunk-mason-compcont-find-borders-fun (start end dummy)
  (when dummy
    (list
     (when start
       (save-match-data
         (let ((here (point))
               ret)
           (goto-char start)
           (when (re-search-forward "[^>]* &>" end t)
             (setq ret (point))
             (goto-char here)
             ret))
         ))
     (when end (- end 4))
     dummy)))

;; (defun mumamo-chunk-mason-compcont-old (pos min max)
;;   (mumamo-find-possible-chunk-new pos
;;                                   max
;;                                   'mumamo-chunk-mason-compcont-bw-exc-start-fun
;;                                   'mumamo-chunk-mason-compcont-fw-exc-start-fun-old
;;                                   'mumamo-chunk-mason-compcont-fw-exc-end-fun
;;                                   'mumamo-chunk-mason-compcont-find-borders-fun))

(defun mumamo-chunk-mason-compcont-fw-exc-start-fun (pos max)
  (let ((where (mumamo-chunk-start-fw-str-inc pos max "<&| ")))
    (when where
      (list where 'html-mode nil))))

(defun mumamo-chunk-mason-compcont (pos min max)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-chunk-mason-compcont-fw-exc-start-fun
                                 'mumamo-chunk-mason-compcont-fw-exc-end-fun
                                 'mumamo-chunk-mason-compcont-find-borders-fun))

;;;###autoload
(define-mumamo-multi-major-mode mason-html-mumamo-mode
  "Turn on multiple major modes for Mason using main mode `html-mode'.
This covers inlined style and javascript."
  ("Mason html Family" html-mode
   (
    mumamo-chunk-mason-perl-line
    mumamo-chunk-mason-perl-single
    mumamo-chunk-mason-perl-block
    mumamo-chunk-mason-perl-init
    mumamo-chunk-mason-perl-once
    mumamo-chunk-mason-perl-cleanup
    mumamo-chunk-mason-perl-shared
    mumamo-chunk-mason-simple-comp
    mumamo-chunk-mason-compcont
    mumamo-chunk-mason-args
    mumamo-chunk-mason-doc
    mumamo-chunk-mason-text
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'mason-html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family-locally 'mason-html-mumamo-mode 'mason-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Embperl

(defun mumamo-chunk-embperl-<- (pos min max)
  "Find [- ... -], return range and `perl-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "[-" "-]" t 'perl-mode t))

(defun mumamo-chunk-embperl-<+ (pos min max)
  "Find [+ ... +], return range and `perl-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "[+" "+]" t 'perl-mode nil))

(defun mumamo-chunk-embperl-<! (pos min max)
  "Find [! ... !], return range and `perl-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "[!" "!]" t 'perl-mode t))

(defun mumamo-chunk-embperl-<$ (pos min max)
  "Find [$ ... $], return range and `perl-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; This is a bit tricky since [$var] etc must be avoided.
  (let* ((begin-mark "[$")
         (end-mark "$]")
         (good-chars '(32 ;space
                       10 ;line feed
                       9  ;tab
                       ))
         ;; (search-bw-exc-start (lambda (pos min)
         ;;                        (let ((not-found t)
         ;;                              (next-char nil)
         ;;                              (exc-start (mumamo-chunk-start-bw-str
         ;;                                          pos min begin-mark))
         ;;                              (here (point)))
         ;;                          (while (and not-found
         ;;                                      exc-start)
         ;;                            (setq next-char (char-after (+ (point) 2)))
         ;;                            (if (memq next-char good-chars)
         ;;                                (setq not-found nil)
         ;;                              (setq exc-start
         ;;                                    (search-backward begin-mark
         ;;                                                     min t))))
         ;;                          (when (and exc-start
         ;;                                     (<= exc-start pos))
         ;;                            (cons exc-start 'perl-mode)))))
         ;; (search-bw-exc-end (lambda (pos min)
         ;;                      (mumamo-chunk-end-bw-str pos min end-mark)))
         ;; (search-fw-exc-start-old (lambda (pos max)
         ;;                            (let ((not-found t)
         ;;                                  (next-char nil)
         ;;                                  (exc-start (mumamo-chunk-start-fw-str
         ;;                                              pos max begin-mark))
         ;;                                  (here (point)))
         ;;                              (while (and not-found
         ;;                                          exc-start)
         ;;                                (setq next-char (char-after))
         ;;                                (if (memq next-char good-chars)
         ;;                                    (setq not-found nil)
         ;;                                  (setq exc-start
         ;;                                    (search-forward begin-mark
         ;;                                                    max t))))
         ;;                              exc-start)))
         (search-fw-exc-start (lambda (pos max)
                                (let ((not-found t)
                                      (next-char nil)
                                      (exc-start (mumamo-chunk-start-fw-str
                                                  pos max begin-mark))
                                      (here (point)))
                                  (while (and not-found
                                              exc-start)
                                    (setq next-char (char-after))
                                    (if (memq next-char good-chars)
                                        (setq not-found nil)
                                      (setq exc-start
                                            (search-forward begin-mark
                                                            max t))))
                                  (list exc-start 'perl-mode))))
         (search-fw-exc-end (lambda (pos max)
                              (save-match-data
                                (mumamo-chunk-end-fw-str pos max end-mark))))
         )
    ;; (mumamo-find-possible-chunk pos min max
    ;;                             search-bw-exc-start
    ;;                             search-bw-exc-end
    ;;                             search-fw-exc-start-old
    ;;                             search-fw-exc-end)
    (mumamo-possible-chunk-forward pos max
                                search-fw-exc-start
                                search-fw-exc-end)
    ))

;;;###autoload
(define-mumamo-multi-major-mode embperl-html-mumamo-mode
  "Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript."
    ("Embperl HTML Family" html-mode
     (mumamo-chunk-embperl-<-
      mumamo-chunk-embperl-<+
      mumamo-chunk-embperl-<!
      mumamo-chunk-embperl-<$
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; django

(defun mumamo-chunk-django4(pos min max)
  "Find {% comment %}.  Return range and `django-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{% comment %}" "{% endcomment %}" t 'mumamo-comment-mode t))

(defun mumamo-chunk-django3(pos min max)
  "Find {# ... #}.  Return range and `django-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{#" "#}" t 'mumamo-comment-mode t))

(defun mumamo-chunk-django2(pos min max)
  "Find {{ ... }}.  Return range and `django-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{{" "}}" t 'django-variable-mode t))

(defun mumamo-chunk-django (pos min max)
  "Find {% ... %}.  Return range and `django-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let ((chunk (mumamo-quick-static-chunk pos min max "{%" "%}" t 'django-mode t)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

;; (defun mumamo-search-bw-exc-start-django (pos min)
;;   "Helper for `mumamo-chunk-django'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{%")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'django-mode))))

;; (defun mumamo-search-bw-exc-start-django2(pos min)
;;   "Helper for `mumamo-chunk-django2'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{{")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'django-mode))))

;; (defun mumamo-search-bw-exc-start-django3(pos min)
;;   "Helper for `mumamo-chunk-django3'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{#")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'mumamo-comment-mode))))

;; (defun mumamo-search-bw-exc-start-django4(pos min)
;;   "Helper for `mumamo-chunk-django4'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min
;;                                                        "{% comment %}")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'mumamo-comment-mode))))

;; (defun mumamo-search-bw-exc-end-django (pos min)
;;   "Helper for `mumamo-chunk-django'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str-inc pos min "%}"))

;; (defun mumamo-search-bw-exc-end-django2(pos min)
;;   "Helper for `mumamo-chunk-django2'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str-inc pos min "}}"))

;; (defun mumamo-search-bw-exc-end-django3(pos min)
;;   "Helper for `mumamo-chunk-django3'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str-inc pos min "#}"))

;; (defun mumamo-search-bw-exc-end-django4(pos min)
;;   "Helper for `mumamo-chunk-django4'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str-inc pos min "{% endcomment %}"))

(defun mumamo-search-fw-exc-start-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{%"))

(defun mumamo-search-fw-exc-start-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{{"))

(defun mumamo-search-fw-exc-start-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{#"))

(defun mumamo-search-fw-exc-start-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{% comment %}"))

(defun mumamo-search-fw-exc-end-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "%}"))

(defun mumamo-search-fw-exc-end-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "}}"))

(defun mumamo-search-fw-exc-end-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#}"))

(defun mumamo-search-fw-exc-end-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "{% endcomment %}"))

;;;###autoload
(define-mumamo-multi-major-mode django-html-mumamo-mode
  "Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Django HTML Family" html-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genshi / kid

;; {% python ... %}
(defun mumamo-chunk-genshi%(pos min max)
  "Find {% python ... %}.  Return range and `genshi-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{% python" "%}" t 'python-mode t))

;; ${expr}
(defun mumamo-chunk-genshi$(pos min max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let ((chunk
         (mumamo-quick-static-chunk pos min max "${" "}" t 'python-mode t)))
    (when chunk
      ;; Test for clash with %}
      (let ((sub-mode (nth 2 chunk))
            (start (nth 0 chunk)))
        (if sub-mode
            chunk
          ;;(message "point.1=%s" (point))
          (when (and start
                     (eq ?% (char-before start)))
            ;;(message "point.2=%s" (point))
            ;;(message "clash with %%}, chunk=%s" chunk)
            ;;(setq chunk nil)
            (setcar chunk (1- start))
            )
          ;;(message "chunk.return=%s" chunk)
          chunk)))))

;; Fix-me: Because of the way chunks currently are searched for there
;; is an error when a python chunk is used. This is because mumamo
;; gets confused by the %} ending and the } ending.  This can be
;; solved by running a separate phase to get the chunks first and
;; during that phase match start and end of the chunk.


;; Note: You will currently get fontification errors if you use
;; python chunks

;;   {% python ... %}

;; The reason is that the chunk routines currently do not know when
;; to just look for the } or %} endings.  However this should not
;; affect your editing normally.

;;;###autoload
(define-mumamo-multi-major-mode genshi-html-mumamo-mode
  "Turn on multiple major modes for Genshi with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Genshi HTML Family" html-mode
   (
    ;;mumamo-chunk-genshi%
    mumamo-chunk-genshi$
    mumamo-chunk-py:=
    mumamo-chunk-py:match
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MJT

;; ${expr}
(defun mumamo-chunk-mjt$(pos min max)
  "Find ${ ... }, return range and `javascript-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "${" "}" t 'javascript-mode t))

;;;###autoload
(define-mumamo-multi-major-mode mjt-html-mumamo-mode
  "Turn on multiple major modes for MJT with main mode `html-mode'.
This also covers inlined style and javascript."
  ("MJT HTML Family" html-mode
   (
    mumamo-chunk-mjt$
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smarty

(defun mumamo-chunk-smarty-literal (pos min max)
  "Find {literal} ... {/literal}.  Return range and 'html-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{literal}" "{/literal}" t 'html-mode t))

(defun mumamo-chunk-smarty-t (pos min max)
  "Find {t} ... {/t}.  Return range and 'html-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{t}" "{/t}" t 'text-mode t))

(defun mumamo-chunk-smarty-comment (pos min max)
  "Find {* ... *}.  Return range and 'mumamo-comment-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{*" "*}" t 'mumamo-comment-mode nil))

(defun mumamo-chunk-smarty (pos min max)
  "Find { ... }.  Return range and 'smarty-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "{" "}" t 'smarty-mode nil))

;;;###autoload
(define-mumamo-multi-major-mode smarty-html-mumamo-mode
  "Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Smarty HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    ;;mumamo-chunk-inlined-style
    ;;mumamo-chunk-inlined-script
    mumamo-chunk-smarty-literal
    mumamo-chunk-smarty-t
    mumamo-chunk-smarty-comment
    mumamo-chunk-smarty
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ssjs - server side javascript

;; http://www.sitepoint.com/blogs/2009/03/10/server-side-javascript-will-be-as-common-as-php/
;;
;; It looks like there are different syntaxes, both
;;
;;  <script runat="server">...</script> and <% ... %>.

(defun mumamo-chunk-ssjs-% (pos min max)
  "Find <% ... %>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "<%" "%>" t 'javascript-mode t))

(defconst mumamo-ssjs-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "runat"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      (or "server" "both" "server-proxy")
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

;; (defun mumamo-search-bw-exc-start-inlined-ssjs (pos min)
;;   "Helper for `mumamo-chunk-inlined-ssjs'.
;; POS is where to start search and MIN is where to stop."
;;   (goto-char (+ pos 7))
;;   (let ((marker-start (when (< min (point)) (search-backward "<script" min t)))
;;         exc-mode
;;         exc-start)
;;     (when marker-start
;;       (when (looking-at mumamo-ssjs-tag-start-regex)
;;         (setq exc-start (match-end 0))
;;         (goto-char exc-start)
;;         (when (<= exc-start pos)
;;           ;;(cons (point) 'javascript-mode)
;;           (list (point) 'javascript-mode '(nxml-mode))
;;           )
;;         ))))

;; (defun mumamo-search-fw-exc-start-inlined-ssjs-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-ssjs'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<script" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 7))
;;       (when (looking-at mumamo-ssjs-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-search-fw-exc-start-inlined-ssjs (pos max)
  "Helper for `mumamo-chunk-inlined-ssjs'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-ssjs-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-chunk-inlined-ssjs (pos min max)
  "Find <script runat=...>...</script>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-inlined-ssjs
  ;;                             'mumamo-search-bw-exc-end-inlined-script
  ;;                             'mumamo-search-fw-exc-start-inlined-ssjs-old
  ;;                             'mumamo-search-fw-exc-end-inlined-script)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-ssjs
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;###autoload
(define-mumamo-multi-major-mode ssjs-html-mumamo-mode
  "Turn on multiple major modes for SSJS with main mode `html-mode'.
This covers inlined style and javascript."
  ("HTML Family" html-mode
   (mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-inlined-ssjs
    mumamo-chunk-ssjs-%
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'ssjs-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gsp

(defun mumamo-chunk-gsp (pos min max)
  "Find <% ... %>.  Return range and 'groovy-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "<%" "%>" t 'groovy-mode t))

;;;###autoload
(define-mumamo-multi-major-mode gsp-html-mumamo-mode
  "Turn on multiple major modes for GSP with main mode `html-mode'.
This also covers inlined style and javascript."
    ("GSP HTML Family" html-mode
     (mumamo-chunk-gsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jsp - Java Server Pages

(defun mumamo-chunk-jsp (pos min max)
  "Find <% ... %>.  Return range and 'java-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "<%" "%>" t 'java-mode t))

;;;###autoload
(define-mumamo-multi-major-mode jsp-html-mumamo-mode
  "Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript."
    ("JSP HTML Family" html-mode
     (mumamo-chunk-jsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eruby

;; Fix-me: Maybe take care of <%= and <%- and -%>, but first ask the
;; ruby people if this is worth doing.
;;
;; See also http://wiki.rubyonrails.org/rails/pages/UnderstandingViews
(defun mumamo-chunk-eruby (pos min max)
  "Find <% ... %>.  Return range and 'ruby-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let ((chunk (mumamo-quick-static-chunk pos min max "<%" "%>" t 'ruby-mode t)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      ;; Fix-me: use this!
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby-quoted (pos min max)
  "Find \"<%= ... %>\".  Return range and 'ruby-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX.

This is a workaround for problems with strings."
  (let ((chunk (mumamo-quick-static-chunk pos min max "\"<%=" "%>\"" t 'ruby-mode t)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      ;; Fix-me: use this!
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby-comment (pos min max)
  "Find <%# ... %>.  Return range and 'ruby-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX.

This is needed since otherwise the end marker is thought to be
part of a comment."
  (mumamo-quick-static-chunk pos min max "<%#" "%>" t 'mumamo-comment-mode t))

;; (defun mumamo-search-bw-exc-start-ruby (pos min)
;;   "Helper for `mumamo-chunk-ruby'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "<%")))
;;     (when (and exc-start
;;                (<= exc-start pos))
;;       (cons exc-start 'ruby-mode))))

;;;###autoload
(define-mumamo-multi-major-mode eruby-mumamo-mode
  "Turn on multiple major mode for eRuby with unspecified main mode.
Current major-mode will be used as the main major mode."
  ("eRuby Family" nil
   (mumamo-chunk-eruby-comment
    mumamo-chunk-eruby
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-html-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript."
  ("eRuby Html Family" html-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-javascript-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `javascript-mode'."
  ("eRuby Html Family" javascript-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby-quoted
    mumamo-chunk-eruby
    ;;mumamo-chunk-inlined-style
    ;;mumamo-chunk-inlined-script
    ;;mumamo-chunk-style=
    ;;mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heredoc

(defcustom mumamo-heredoc-modes
  '(
    ("HTML" html-mode)
    ("CSS" css-mode)
    ("JAVASCRIPT" javascript-mode)
    ("JAVA" java-mode)
    ("GROOVY" groovy-mode)
    ("SQL" sql-mode)
    )
  "Matches for heredoc modes.
The entries in this list have the form

  (REGEXP MAJOR-MODE-SPEC)

where REGEXP is a regular expression that should match the
heredoc marker line and MAJOR-MODE-SPEC is the major mode spec to
use in the heredoc part.

The major mode spec is translated to a major mode using
`mumamo-major-mode-from-modespec'."
  :type '(repeat
          (list
           regexp
           (function :tag "Major mode")))
  :group 'mumamo-modes)

(defun mumamo-mode-for-heredoc (marker)
  "Return major mode associated with MARKER.
Use first match in `mumamo-heredoc-modes'.
If no match use `text-mode'."
  (let ((mode (catch 'mode
                (save-match-data
                  (dolist (rec mumamo-heredoc-modes)
                    (let ((regexp (nth 0 rec))
                          (mode   (nth 1 rec)))
                      (when (string-match regexp marker)
                        (throw 'mode mode))))))))
    (if mode
        (mumamo-major-mode-from-modespec mode)
      'text-mode)))

(defun mumamo-chunk-heredoc (pos min max lang)
  "This should work similar to `mumamo-find-possible-chunk'.
POS, MIN and MAX have the same meaning as there.

LANG is the programming language.
Supported values are 'perl."
  ;; Fix-me: LANG
  ;; Fix-me: use mumamo-end-in-code
  (mumamo-condition-case err
      (let ((old-point (point)))
        (goto-char pos)
        (beginning-of-line)
        (let (next-<<
              (want-<< t)
              heredoc-mark
              end-mark-len
              heredoc-line
              delimiter
              skipped
              (skip-b "")
              start-inner
              end
              exc-mode
              fw-exc-fun
              border-fun
              allow-code-after
              start-outer
              ps
              )
          (goto-char pos)
          (beginning-of-line)
          (case lang
            ('sh
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "\t*")
                 (unless (eolp) (forward-char)))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "")
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n<>;]+\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank line-end))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('w32-ps (error "No support for windows power shell yet"))
            ('php
             (while want-<<
               (setq next-<< (search-forward "<<<" max t))
               ;; Check inside string or comment.
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 0)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 3))
               (skip-chars-forward " \t")
               (when (looking-at (concat "\\([^\n;]*\\)[[:blank:]]*\n"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 1)
                                      (match-end 1)))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;; fix-me: nowdoc
                 (when (and (= ?\' (string-to-char heredoc-mark))
                            (= ?\' (string-to-char (substring heredoc-mark (1- (length heredoc-mark))))))
                   (setq heredoc-mark (substring heredoc-mark 1 (- (length heredoc-mark) 1))))
                 (setq end-mark-len (1+ (length heredoc-mark)))
                 (setq start-inner (match-end 0)))))
            ('perl
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "") ;; blank line
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n;]*\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank ";"))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;;(setq start-inner (1+ (match-end 0)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('python
             (unless (eobp) (forward-char))
             (while want-<<
               (setq next-<< (re-search-forward "\"\"\"\\|'''" max t))
               (setq start-outer (- (point) 3))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 3)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil)))))
            ('ruby
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "[ \t]*")
                 (forward-char))
               (when (looking-at (concat "[^\n[:blank:]]*"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 0)
                                      (match-end 0)))
                 (setq end-mark-len (length heredoc-mark))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (match-end 0)))))
            (t (error "next-<< not implemented for lang %s" lang)))
          (when start-inner (assert (<= pos start-inner) t))
          (goto-char old-point)
          (when (or start-inner end)
            (let ((endmark-regexp
                   (case lang
                     ('sh (concat "^" skip-b heredoc-mark "$"))
                     ('php (concat "^" heredoc-mark ";?$"))
                     ('perl (concat "^" heredoc-mark "$"))
                     ('python (concat "^" heredoc-mark "[[:space:]]*"))
                     ('ruby (concat "^" skip-b heredoc-mark "$"))
                     (t (error "mark-regexp not implemented for %s" lang)))))
              ;; Fix-me: rename start-inner <=> start-outer...
              (setq border-fun `(lambda (start end exc-mode)
                                  ;; Fix-me: use lengths...
                                  (list
                                   (if ,allow-code-after nil (+ start (- ,start-inner ,start-outer 1)))
                                   (when end (- end ,end-mark-len)))))
              (setq fw-exc-fun `(lambda (pos max)
                                  (save-match-data
                                    (let ((here (point)))
                                      (goto-char pos)
                                      (prog1
                                          (when (re-search-forward ,endmark-regexp max t)
                                            (- (point) 1 ,(length heredoc-mark))
                                            (- (point) 0)
                                            )
                                        (goto-char here)))))))
            (setq exc-mode (mumamo-mode-for-heredoc heredoc-line))
            (list start-inner end exc-mode nil nil fw-exc-fun nil)
            ;; Fix me: Add overriding for inner chunks (see
            ;; http://www.emacswiki.org/emacs/NxhtmlMode#toc13). Maybe
            ;; make fw-exc-fun a list (or a cons, since overriding is
            ;; probably all that I want to add)? And make the
            ;; corresponding chunk property a list too?
            ;;(list start-outer end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            (list (if allow-code-after start-inner start-outer)
                  end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            )))
    (error (mumamo-display-error 'mumamo-chunk-heredoc
                                 "%s" (error-message-string err)))))


;;;; Unix style sh heredoc

(defun mumamo-chunk-sh-heredoc (pos min max)
  "Find sh here docs.
See `mumamo-find-possible-chunk' for POS, MIN
and MAX."
  (let ((r (mumamo-chunk-heredoc pos min max 'sh)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode sh-heredoc-mumamo-mode
  "Turn on multiple major modes for sh heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("SH HereDoc" sh-mode
   (mumamo-chunk-sh-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'sh-heredoc-mumamo-mode)


;;;; PHP heredoc

(defun mumamo-chunk-php-heredoc (pos min max)
  "Find PHP here docs.
See `mumamo-find-possible-chunk' for POS, MIN
and MAX."
  (let ((r (mumamo-chunk-heredoc pos min max 'php)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode php-heredoc-mumamo-mode
  "Turn on multiple major modes for PHP heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("PHP HereDoc" php-mode
   (mumamo-chunk-php-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'php-heredoc-mumamo-mode)
(mumamo-inherit-sub-chunk-family-locally 'php-heredoc-mumamo-mode 'html-mumamo-mode)


;;;; Perl heredoc

(defun mumamo-chunk-perl-heredoc (pos min max)
  "Find perl here docs.
See `mumamo-find-possible-chunk' for POS, MIN
and MAX."
  (let ((r (mumamo-chunk-heredoc pos min max 'perl)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode perl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" perl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'perl-heredoc-mumamo-mode)

;;;###autoload
(define-mumamo-multi-major-mode cperl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" cperl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'cperl-heredoc-mumamo-mode)


;;;; Python heredoc

(defun mumamo-chunk-python-heredoc (pos min max)
  "Find python here docs.
See `mumamo-find-possible-chunk' for POS, MIN
and MAX."
  (let ((r (mumamo-chunk-heredoc pos min max 'python)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode python-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Python HereDoc" python-mode
   (mumamo-chunk-python-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'python-heredoc-mumamo-mode)


;;;; Ruby heredoc

(defun mumamo-chunk-ruby-heredoc (pos min max)
  "Find Ruby here docs.
See `mumamo-find-possible-chunk' for POS, MIN
and MAX."
  (let ((r (mumamo-chunk-heredoc pos min max 'ruby)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode ruby-heredoc-mumamo-mode
  "Turn on multiple major modes for Ruby heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Ruby HereDoc" ruby-mode
   (mumamo-chunk-ruby-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'ruby-heredoc-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tex meta

;; (defun mumamo-search-bw-textext-start (pos min)
;;   "Helper for `mumamo-chunk-textext'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "textext(\""))
;;         (exc-mode 'plain-tex-mode))
;;     (when exc-start
;;       (when (<= exc-start pos)
;;         (cons exc-start exc-mode)))))

(defconst mumamo-textext-end-regex
  (rx "textext("
      (0+
       (0+ (not (any "\"()")))
       ?\"
       (0+ (not (any "\"")))
       ?\"
       )
      (0+ (not (any "\"()")))
      ")"))

(defun mumamo-textext-test-is-end (pos)
  "Helper for `mumamo-chunk-textext'.
Return POS if POS is at the end of textext chunk."
  (when pos
    (let ((here (point))
          hit)
      (goto-char (+ 2 pos))
      (when (looking-back mumamo-textext-end-regex)
        (setq hit t))
      (goto-char here)
      (when hit pos))))

;; (defun mumamo-search-bw-textext-end (pos min)
;;   "Helper for `mumamo-chunk-textext'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((end (mumamo-chunk-end-bw-str pos min "\")"))
;;         res)
;;     (while (and end
;;                 (not (setq res (mumamo-textext-test-is-end end))))
;;       (setq end (mumamo-chunk-end-bw-str (1- end) min "\")")))
;;     res))

;; (defun mumamo-search-fw-textext-start-old (pos max)
;;   "Helper for `mumamo-chunk-textext'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "textext(\""))

(defun mumamo-search-fw-textext-start (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "textext(\"")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-textext-end (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (let ((end (mumamo-chunk-end-fw-str pos max "\")")))
      (mumamo-textext-test-is-end end))))

(defun mumamo-chunk-textext (pos min max)
  "Find textext or TEX chunks.  Return range and 'plain-tex-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-textext-start
  ;;                             'mumamo-search-bw-textext-end
  ;;                             'mumamo-search-fw-textext-start-old
  ;;                             'mumamo-search-fw-textext-end)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-textext-start
                                 'mumamo-search-fw-textext-end))

;; (defun mumamo-search-bw-verbatimtex-start (pos min)
;;   "Helper for `mumamo-chunk-verbatimtextext'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "\nverbatimtex"))
;;         (exc-mode 'plain-tex-mode))
;;     (when exc-start
;;       (when (<= exc-start pos)
;;         (cons exc-start exc-mode)))))

;; (defun mumamo-search-bw-verbatimtex-end (pos min)
;;   "Helper for `mumamo-chunk-verbatimtextext'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "\netex"))

;; (defun mumamo-search-fw-verbatimtex-start-old (pos max)
;;   "Helper for `mumamo-chunk-verbatimtextext'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "\nverbatimtex"))

(defun mumamo-search-fw-verbatimtex-start (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-verbatimtex-end (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-verbatimtex (pos min max)
  "Find verbatimtex - etex chunks.  Return range and 'plain-tex-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-verbatimtex-start
  ;;                             'mumamo-search-bw-verbatimtex-end
  ;;                             'mumamo-search-fw-verbatimtex-start-old
  ;;                             'mumamo-search-fw-verbatimtex-end)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-verbatimtex-start
                                 'mumamo-search-fw-verbatimtex-end))

;; (defun mumamo-search-bw-btex-start (pos min)
;;   "Helper for `mumamo-chunk-btex'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "\nverbatimtex"))
;;         (exc-mode 'plain-tex-mode))
;;     (when exc-start
;;       (when (<= exc-start pos)
;;         (cons exc-start exc-mode)))))

;; (defun mumamo-search-bw-btex-end (pos min)
;;   "Helper for `mumamo-chunk-btex'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "\netex"))

;; (defun mumamo-search-fw-btex-start-old (pos max)
;;   "Helper for `mumamo-chunk-btex'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "\nverbatimtex"))

(defun mumamo-search-fw-btex-start (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-btex-end (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-btex (pos min max)
  "Find btex - etex chunks.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-btex-start
  ;;                             'mumamo-search-bw-btex-end
  ;;                             'mumamo-search-fw-btex-start-old
  ;;                             'mumamo-search-fw-btex-end)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-btex-start
                                 'mumamo-search-fw-btex-end))

;;;###autoload
(define-mumamo-multi-major-mode metapost-mumamo-mode
  "Turn on multiple major modes for MetaPost."
  ("MetaPost TeX Family" metapost-mode
   (mumamo-chunk-textext
    mumamo-chunk-verbatimtex
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenLaszlo

(defconst mumamo-lzx-method-tag-start-regex
  (rx "<method"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-method (pos min)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MIN is where to stop."
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<method" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))

;; (defun mumamo-search-bw-exc-end-inlined-lzx-method (pos min)
;;   "Helper for `mumamo-chunk-inlined-lzx-method'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</method>"))

;; (defun mumamo-search-fw-exc-start-inlined-lzx-method-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-lzx-method'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<method" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 7))
;;       (when (looking-at mumamo-lzx-method-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-search-fw-exc-start-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<method" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</method>")))

(defun mumamo-chunk-inlined-lzx-method (pos min max)
  "Find <method>...</method>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-inlined-lzx-method
  ;;                             'mumamo-search-bw-exc-end-inlined-lzx-method
  ;;                             'mumamo-search-fw-exc-start-inlined-lzx-method-old
  ;;                             'mumamo-search-fw-exc-end-inlined-lzx-method)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-method
                                 'mumamo-search-fw-exc-end-inlined-lzx-method))

(defconst mumamo-lzx-handler-tag-start-regex
  (rx "<handler"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

;; (defun mumamo-search-bw-exc-start-inlined-lzx-handler (pos min)
;;   "Helper for `mumamo-chunk-inlined-lzx-handler'.
;; POS is where to start search and MIN is where to stop."
;;   (goto-char (+ pos 8))
;;   (let ((marker-start (search-backward "<handler" min t))
;;         exc-mode
;;         exc-start)
;;     (when marker-start
;;       (when (looking-at mumamo-lzx-handler-tag-start-regex)
;;         (setq exc-start (match-end 0))
;;         (goto-char exc-start)
;;         (when (<= exc-start pos)
;;           (cons (point) 'javascript-mode))
;;         ))))

;; (defun mumamo-search-bw-exc-end-inlined-lzx-handler (pos min)
;;   "Helper for `mumamo-chunk-inlined-lzx-handler'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</handler>"))

;; (defun mumamo-search-fw-exc-start-inlined-lzx-handler-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-lzx-handler'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<handler" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 8))
;;       (when (looking-at mumamo-lzx-handler-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-search-fw-exc-start-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<handler" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 8))
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</handler>")))

(defun mumamo-chunk-inlined-lzx-handler (pos min max)
  "Find <handler>...</handler>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-inlined-lzx-handler
  ;;                             'mumamo-search-bw-exc-end-inlined-lzx-handler
  ;;                             'mumamo-search-fw-exc-start-inlined-lzx-handler-old
  ;;                             'mumamo-search-fw-exc-end-inlined-lzx-handler)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-handler
                                 'mumamo-search-fw-exc-end-inlined-lzx-handler))


;;;###autoload
(define-mumamo-multi-major-mode laszlo-nxml-mumamo-mode
  "Turn on multiple major modes for OpenLaszlo."
  ("OpenLaszlo Family" nxml-mode
   (mumamo-chunk-inlined-script
    mumamo-chunk-inlined-lzx-method
    mumamo-chunk-inlined-lzx-handler
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound

;; (defun mumamo-search-bw-exc-start-csound-orc (pos min)
;;   "Helper for `mumamo-chunk-csound-orc'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "<csinstruments>")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'csound-orc-mode))))

;; (defun mumamo-search-bw-exc-end-csound-orc (pos min)
;;   "Helper for `mumamo-chunk-csound-orc'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</csinstruments>"))

;; (defun mumamo-search-fw-exc-start-csound-orc-old (pos max)
;;   "Helper for `mumamo-chunk-csound-orc'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "<csinstruments>"))

(defun mumamo-search-fw-exc-start-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csinstruments>")))
    (when where
      (list where 'csound-orc-mode))))

(defun mumamo-search-fw-exc-end-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csinstruments>")))

(defun mumamo-chunk-csound-orc (pos min max)
  "Find <csinstruments>...</...>.  Return range and 'csound-orc-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-csound-orc
  ;;                             'mumamo-search-bw-exc-end-csound-orc
  ;;                             'mumamo-search-fw-exc-start-csound-orc-old
  ;;                             'mumamo-search-fw-exc-end-csound-orc)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-orc
                                 'mumamo-search-fw-exc-end-csound-orc))

;; (defun mumamo-search-bw-exc-start-csound-sco (pos min)
;;   "Helper for `mumamo-chunk-csound-sco'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-str pos min "<csscore>")))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'csound-sco-mode))))

;; (defun mumamo-search-bw-exc-end-csound-sco (pos min)
;;   "Helper for `mumamo-chunk-csound-sco'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "</csscore>"))

;; (defun mumamo-search-fw-exc-start-csound-sco-old (pos max)
;;   "Helper for `mumamo-chunk-csound-sco'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "<csscore>"))

(defun mumamo-search-fw-exc-start-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csscore>")))
    (when where
      (list where 'csound-sco-mode))))

(defun mumamo-search-fw-exc-end-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csscore>")))

(defun mumamo-chunk-csound-sco (pos min max)
  "Found <csscore>...</csscore>.  Return range and 'csound-sco-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-csound-sco
  ;;                             'mumamo-search-bw-exc-end-csound-sco
  ;;                             'mumamo-search-fw-exc-start-csound-sco-old
  ;;                             'mumamo-search-fw-exc-end-csound-sco)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-sco
                                 'mumamo-search-fw-exc-end-csound-sco))

;;;###autoload
(define-mumamo-multi-major-mode csound-sgml-mumamo-mode
  "Turn on mutiple major modes for CSound orc/sco Modes."
  ("CSound orc/sco Modes" sgml-mode
   (mumamo-chunk-csound-sco
    mumamo-chunk-csound-orc
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; noweb

;;;###autoload
(defgroup mumamo-noweb2 nil
  "Customization group for `noweb2-mumamo-mode'."
  :group 'mumamo-modes)

(defcustom mumamo-noweb2-mode-from-ext
  '(
    ("php" . php-mode)
    ("c" . c-mode)
    )
  "File extension regexp to major mode mapping.
Used by `noweb2-mumamo-mode'."
  :type '(repeat
          (cons regexp major-mode-function))
  :group 'mumamo-noweb2)

(defvar mumamo-noweb2-found-mode-from-ext nil
  "Major modes determined from file names.  Internal use.")

(defun mumamo-noweb2-chunk-start-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-re pos max "^<<\\(.*?\\)>>="))
        (exc-mode 'text-mode))
    (when where
      (let* ((file-name (match-string-no-properties 1))
             (file-ext (when file-name (file-name-extension file-name))))
        (when file-ext
          (setq exc-mode (catch 'major
                           (dolist (rec mumamo-noweb2-mode-from-ext)
                             (when (string-match (car rec) file-ext)
                               (throw 'major (cdr rec))))
                           nil))))
      (list where exc-mode))))

;; (defun mumamo-noweb2-chunk-start-bw (pos min)
;;   "Helper for `mumamo-noweb2-chunk'.
;; POS is where to start search and MIN is where to stop."
;;   (let ((exc-start (mumamo-chunk-start-bw-re pos min "^<<\\(.*?\\)>>="))
;;         (exc-mode 'text-mode))
;;     (when exc-start
;;       (let* ((file-name (match-string 1))
;;              (file-ext  (when file-name (file-name-extension file-name))))
;;         (when file-ext
;;           (setq exc-mode (catch 'major
;;                            (dolist (rec mumamo-noweb2-mode-from-ext)
;;                              (when (string-match (car rec) file-ext)
;;                                (throw 'major (cdr rec))))
;;                            nil))
;;           (unless exc-mode
;;             (setq exc-mode
;;                   (cdr (assoc file-ext mumamo-noweb2-found-mode-from-ext)))
;;             (unless exc-mode
;;               ;; Get the major mode from file name
;;               (with-temp-buffer
;;                 (setq buffer-file-name file-name)
;;                 (condition-case err
;;                     (normal-mode)
;;                   (error (message "error (normal-mode): %s"
;;                                   (error-message-string err))))
;;                 (setq exc-mode (or major-mode
;;                                    'text-mode))
;;                 (add-to-list 'mumamo-noweb2-found-mode-from-ext
;;                              (cons file-ext exc-mode)))
;;               ))))
;;       (cons exc-start exc-mode))))

(defun mumamo-noweb2-chunk-end-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-re pos max "^@")))

;; (defun mumamo-noweb2-chunk-end-bw (pos min)
;;   "Helper for `mumamo-noweb2-chunk'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-re pos min "^@"))

(defun mumamo-noweb2-code-chunk (pos min max)
  "Find noweb chunks.  Return range and found mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (save-match-data
    ;; (mumamo-find-possible-chunk pos min max
    ;;                             'mumamo-noweb2-chunk-start-bw
    ;;                             'mumamo-noweb2-chunk-end-bw
    ;;                             'mumamo-noweb2-chunk-start-fw-old
    ;;                             'mumamo-noweb2-chunk-end-fw)
    (mumamo-possible-chunk-forward pos max
                                   'mumamo-noweb2-chunk-start-fw
                                   'mumamo-noweb2-chunk-end-fw)))


;;;###autoload
(define-mumamo-multi-major-mode noweb2-mumamo-mode
  "Multi major mode for noweb files."
  ("noweb Family" latex-mode
   (mumamo-noweb2-code-chunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Template-Toolkit



;; (setq auto-mode-alist
;;       (append '(("\\.tt2?$" . tt-mode))  auto-mode-alist ))

;;(require 'tt-mode)
(defun mumamo-chunk-tt (pos min max)
  "Find [% ... %], return range and `tt-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX.

This is for Template Toolkit.
See URL `http://dave.org.uk/emacs/' for `tt-mode'."
  (mumamo-quick-static-chunk pos min max "[%" "%]" t 'tt-mode nil))

(define-mumamo-multi-major-mode tt-html-mumamo-mode
  "Turn on multiple major modes for TT files with main mode `nxhtml-mode'.
TT = Template-Toolkit.

This also covers inlined style and javascript."
    ("TT HTML Family" html-mode
     (mumamo-chunk-tt
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Asp

;;;; asp <%@language="javscript"%>

(defvar mumamo-asp-default-major 'asp-js-mode)
(make-variable-buffer-local 'mumamo-asp-default-major)
(put 'mumamo-asp-default-major 'permanent-local t)

(defconst mumamo-asp-lang-marker
  (rx "<%@"
      (0+ space)
      "language"
      (0+ space)
      "="
      (0+ space)
      "\""
      (submatch (1+ (not (any "\""))))
      "\""
      (0+ space)))

(defun mumamo-search-fw-exc-start-jsp (pos min max)
  ;; fix-me
  )
(defun mumamo-chunk-asp (pos min max)
  "Find <% ... %>.  Return range and 'asp-js-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; Fix-me: this is broken!
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-asp
  ;;                             'mumamo-search-bw-exc-end-jsp
  ;;                             'mumamo-search-fw-exc-start-jsp-old
  ;;                             'mumamo-search-fw-exc-end-jsp)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-asp
                                 'mumamo-search-fw-exc-end-jsp))


;;;; asp <% ...>

(defun mumamo-chunk-asp% (pos min max)
  "Find <% ... %>.  Return range and 'asp-js-mode or 'asp-vb-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let* ((chunk (mumamo-quick-static-chunk pos min max "<%" "%>" t 'java-mode t))
         (beg (nth 0 chunk))
         (here (point))
         glang)
    (when chunk
      (goto-char beg)
      (if (looking-at mumamo-asp-lang-marker)
          (progn
            (setq glang (downcase (match-string 1)))
            (cond
             ((string= glang "javascript")
              (setq mumamo-asp-default-major 'asp-js-mode))
             ((string= glang "vbscript")
              (setq mumamo-asp-default-major 'asp-vb-mode))
             )
            (setcar (nthcdr 2 chunk) 'mumamo-comment-mode))
        (setcar (nthcdr 2 chunk) mumamo-asp-default-major))
      chunk)))

;;;; asp <script ...>

(defconst mumamo-asp-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "language"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      ;; "text/javascript"
      (submatch
       (or "javascript" "vbscript"))
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

;; (defun mumamo-asp-search-bw-exc-start-inlined-script (pos min)
;;   "Helper function for `mumamo-asp-chunk-inlined-script'.
;; POS is where to start search and MIN is where to stop."
;;   (goto-char (+ pos 7))
;;   (let ((marker-start (search-backward "<script" min t))
;;         (exc-mode 'asp-vb-mode)
;;         exc-start
;;         lang)
;;     (when marker-start
;;       (when (looking-at mumamo-asp-script-tag-start-regex)
;;         (setq lang (downcase (match-string-no-properties 1)))
;;         (cond
;;          ((string= lang "javascript")
;;           (setq exc-mode 'asp-js-mode))
;;          ((string= lang "vbscript")
;;           (setq exc-mode 'asp-vb-mode))))
;;       (setq exc-start (match-end 0))
;;       (goto-char exc-start)
;;       (when (<= exc-start pos)
;;         (cons (point) exc-mode))
;;       )))

;; (defun mumamo-asp-search-fw-exc-start-inlined-script-old (pos max)
;;   "Helper for `mumamo-chunk-inlined-script'.
;; POS is where to start search and MAX is where to stop."
;;   (goto-char (1+ pos))
;;   (skip-chars-backward "^<")
;;   ;; Handle <![CDATA[
;;   (when (and
;;          (eq ?< (char-before))
;;          (eq ?! (char-after))
;;          (not (bobp)))
;;     (backward-char)
;;     (skip-chars-backward "^<"))
;;   (unless (bobp)
;;     (backward-char 1))
;;   (let ((exc-start (search-forward "<script" max t))
;;         exc-mode)
;;     (when exc-start
;;       (goto-char (- exc-start 7))
;;       (when (looking-at mumamo-asp-script-tag-start-regex)
;;         (goto-char (match-end 0))
;;         (point)
;;         ))))

(defun mumamo-asp-search-fw-exc-start-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        (exc-mode 'asp-vb-mode)
        (lang "vbscript"))
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-asp-script-tag-start-regex)
        (goto-char (match-end 0))
        (setq lang (downcase (match-string-no-properties 1)))
        (cond
         ((string= lang "javascript")
          (setq exc-mode 'asp-js-mode))
         ((string= lang "vbscript")
          (setq exc-mode 'asp-vb-mode)))
        (list (point) exc-mode)
        ))))

(defun mumamo-asp-chunk-inlined-script (pos min max)
  "Find <script language=...  runat=...>...</script>.  Return 'asp-js-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-asp-search-bw-exc-start-inlined-script
  ;;                             'mumamo-search-bw-exc-end-inlined-script
  ;;                             'mumamo-asp-search-fw-exc-start-inlined-script-old
  ;;                             'mumamo-search-fw-exc-end-inlined-script)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-asp-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;###autoload
(define-mumamo-multi-major-mode asp-html-mumamo-mode
  "Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("ASP Html Family" html-mode
   (mumamo-chunk-asp%
    mumamo-asp-chunk-inlined-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org-mode

(defcustom mumamo-org-submodes
  '(
    (emacs-lisp emacs-lisp-mode)
    (ruby ruby-mode)
    (python python-mode)
    (sh sh-mode)
    (R R-mode)
    (ditaa picture-mode)
    )
  "Alist for conversion of org #+BEGIN_SRC specifier to major mode.
Works kind of like `mumamo-major-modes'.

This may be used for example for org-babel \(see URL
`http://orgmode.org/worg/org-contrib/babel/')."
  :type '(alist
          :key-type (symbol :tag "Symbol in #BEGIN_SRC specifier")
          :value-type (repeat (choice
                               (command :tag "Major mode")
                               (symbol :tag "Major mode (not yet loaded)")))
          )
  :group 'mumamo-modes)

(defun mumamo-org-mode-from-spec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
Translate MAJOR-SPEC used in #BEGIN_SRC to a major mode.

See `mumamo-org-submodes' for an explanation."
  (mumamo-major-mode-from-spec major-spec mumamo-org-submodes))

(defun mumamo-chunk-org-html (pos min max)
  "Find #+BEGIN_HTML ... #+END_HTML, return range and `html-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "#+BEGIN_HTML" "#+END_HTML" nil 'html-mode nil))

;; (defun mumamo-search-bw-org-src-start (pos min)
;;   "Helper for `mumamo-chunk-org-src'.
;; POS is where to start search and MIN is where to stop."
;;   (let* ((exc-start (mumamo-chunk-start-bw-str pos min "#+BEGIN_SRC"))
;;          (exc-mode (when exc-start
;;                      (let ((here (point)))
;;                        (goto-char exc-start)
;;                        (prog1
;;                            (read (current-buffer))
;;                          (goto-char here))))))
;;     (setq exc-mode (mumamo-org-mode-from-spec exc-mode))
;;     ;;(setq exc-mode (eval exc-mode))
;;     ;;(setq exc-mode 'text-mode)
;;     ;;(when exc-mode (setq exc-mode (quote exc-mode)))
;;     ;;(assert (eq exc-mode 'emacs-lisp-mode) t)
;;     (when exc-start
;;       (when (<= exc-start pos)
;;         (cons exc-start exc-mode)))))

;; (defun mumamo-search-bw-org-src-end (pos min)
;;   "Helper for `mumamo-chunk-org-src'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-chunk-end-bw-str pos min "#+END_SRC"))

;; (defun mumamo-search-fw-org-src-start-old (pos max)
;;   "Helper for `mumamo-chunk-org-src'.
;; POS is where to start search and MAX is where to stop."
;;   (mumamo-chunk-start-fw-str pos max "#+BEGIN_SRC"))

(defun mumamo-search-fw-org-src-start (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "#+BEGIN_SRC")))
    (when where
      (let ((exc-mode (let ((here (point)))
                        (goto-char where)
                        (prog1
                            (read (current-buffer))
                          (goto-char here)))))
        (setq exc-mode (mumamo-org-mode-from-spec exc-mode))
        (list where exc-mode)))))

(defun mumamo-search-fw-org-src-end (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "#+END_SRC")))

(defun mumamo-chunk-org-src (pos min max)
  "Find #+BEGIN_SRC ... #+END_SRC, return range and choosen major mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX.

See Info node `(org) Literal Examples' for how to specify major
mode."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-org-src-start
  ;;                             'mumamo-search-bw-org-src-end
  ;;                             'mumamo-search-fw-org-src-start-old
  ;;                             'mumamo-search-fw-org-src-end)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-org-src-start
                                 'mumamo-search-fw-org-src-end))

;;;###autoload
(define-mumamo-multi-major-mode org-mumamo-mode
  "Turn on multiple major modes for `org-mode' files with main mode `org-mode'.
** Note about HTML subchunks:
Unfortunately this only allows `html-mode' (not `nxhtml-mode') in
sub chunks."
    ("Org Mode + Html" org-mode
     (mumamo-chunk-org-html
      mumamo-chunk-org-src
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mako

;; See http://www.makotemplates.org/docs/syntax.html

;;; Comments mode
;; Fix-me: move to mumamo.el
(defconst mumamo-comment-font-lock-keywords
  (list
   (cons "\\(.*\\)" (list 1 font-lock-comment-face))
   ))
(defvar mumamo-comment-font-lock-defaults
  '(mumamo-comment-font-lock-keywords t t))

(define-derived-mode mumamo-comment-mode nil "Comment chunk"
  "For comment blocks."
  (set (make-local-variable 'font-lock-defaults) mumamo-comment-font-lock-defaults))



(defun mumamo-chunk-mako-<% (pos min max)
  "Find <% ... %> and <%! ... %>. Return range and `python-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-mako-<%-bw-start
  ;;                             'mumamo-mako-<%-bw-end
  ;;                             'mumamo-mako-<%-fw-start-old
  ;;                             'mumamo-mako-<%-fw-end
  ;;                             'mumamo-mako-<%-find-borders)
  (let ((chunk (mumamo-possible-chunk-forward pos max
                                              'mumamo-mako-<%-fw-start
                                              'mumamo-mako-<%-fw-end
                                              'mumamo-mako-<%-find-borders
                                              )))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-mako-<%-find-borders (start end exc-mode)
  (when exc-mode
    (list
     (when start
       (+ start
          (if (eq ?! (char-after (+ start 2)))
              3
            2)))
     (when end (- end 2))
     exc-mode)))

;; (defun mumamo-mako-<%-bw-start (pos min)
;;   (let ((here (point))
;;         start
;;         ret
;;         )
;;     (goto-char (+ pos 3))
;;     (setq start (re-search-backward "<%!?\\(?:[ \t]\\|$\\)" min t))
;;     (when (and start (<= start pos))
;;       (setq ret (list start 'python-mode)))
;;     (goto-char here)
;;     ret))

;; (defun mumamo-mako-<%-bw-end (pos min)
;;   (mumamo-chunk-end-bw-str-inc pos min "%>")) ;; ok

;; (defun mumamo-mako-<%-fw-start-old (pos max)
;;   (let ((here (point))
;;         start
;;         ret)
;;     (goto-char pos)
;;     (setq start
;;           (re-search-forward "<%!?\\(?:[ \t]\\|$\\)" max t))
;;     (when start
;;       (setq ret (match-beginning 0)))
;;     (goto-char here)
;;     ret))

(defun mumamo-mako-<%-fw-start (pos max)
  (let ((here (point))
        start
        ret)
    (goto-char pos)
    (setq start
          (re-search-forward "<%!?\\(?:[ \t]\\|$\\)" max t))
    (when start
      (setq ret (match-beginning 0)))
    (goto-char here)
    (when ret
      (list ret 'python-mode))))

(defun mumamo-mako-<%-fw-end (pos max)
  (save-match-data
    (mumamo-chunk-end-fw-str-inc pos max "%>"))) ;; ok



(defun mumamo-chunk-mako-% (pos min max)
  "Find % python EOL.  Return range and `python-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let ((chunk (mumamo-whole-line-chunk pos min max "%" 'python-mode)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-mako-one-line-comment (pos min max)
  "Find ## comment EOL.  Return range and `python-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-whole-line-chunk pos min max "##" 'mumamo-comment-mode))

;; Fix-me: Move this to mumamo.el
;; Fix-me: does not work with new chunk div
(defun mumamo-whole-line-chunk-fw-exc-end-fun (pos max)
  (let ((here (point)))
    (goto-char pos)
    (prog1
        (line-end-position)
      (goto-char here))))

(defun mumamo-whole-line-chunk (pos min max marker mode)
  (let* ((here (point))
         (len-marker (length marker))
         (pattern (rx-to-string `(and bol (0+ blank) ,marker blank) t))
         (whole-line-chunk-borders-fun
          `(lambda (start end dummy)
             (let ((start-border (+ start ,len-marker)))
               (list start-border nil))))
         beg
         end
         ret)
    (goto-char pos)
    (setq beg (re-search-forward pattern max t))
    (when beg
      (setq beg (- beg len-marker 1))
      (setq end (line-end-position))
      (setq ret (list beg
                      end
                      mode
                      (let ((start-border (+ beg len-marker)))
                        (list start-border nil))
                      nil
                      'mumamo-whole-line-chunk-fw-exc-end-fun
                      whole-line-chunk-borders-fun
                      )))
    (goto-char here)
    ret))

;; (defun mumamo-single-regexp-chunk (pos min max begin-mark end-mark mode)
;;   "Not ready yet. `mumamo-quick-static-chunk'"
;;   (let ((here (point))
;;         (len-marker (length marker))
;;         beg
;;         end
;;         ret)
;;     (goto-char pos)
;;     (setq beg (line-beginning-position))
;;     (setq end (line-end-position))
;;     (unless (or (when min (< beg min))
;;                 (when max (> end max))
;;                 (= pos end))
;;       (goto-char beg)
;;       (skip-chars-forward " \t")
;;       (when (and
;;              (string= marker (buffer-substring-no-properties (point) (+ (point) len-marker)))
;;              (memq (char-after (+ (point) len-marker))
;;                    '(?\  ?\t ?\n))
;;              (>= pos (point)))
;;         (setq ret
;;               (list (point)
;;                     end
;;                     mode
;;                     (let ((start-border (+ (point) len-marker)))
;;                       (list start-border nil))))))
;;     (unless ret
;;       (let ((range-regexp
;;              (concat "^[ \t]*"
;;                      "\\("
;;                      (regexp-quote marker)
;;                      "[ \t\n].*\\)$")))
;;         ;; Backward
;;         (goto-char pos)
;;         (unless (= pos (line-end-position))
;;           (goto-char (line-beginning-position)))
;;         (setq beg (re-search-backward range-regexp min t))
;;         (when beg (setq beg (match-end 1)))
;;         ;; Forward, take care of indentation part
;;         (goto-char pos)
;;         (unless (= pos (line-end-position))
;;           (goto-char (line-beginning-position)))
;;         (setq end (re-search-forward range-regexp max t))
;;         (when end (setq end (match-beginning 1))))
;;       (setq ret (list beg end)))
;;     (goto-char here)
;;     ;;(setq ret nil)
;;     ret))


(defun mumamo-chunk-mako-<%doc (pos min max)
  (mumamo-quick-static-chunk pos min max "<%doc>" "</%doc>" t 'mumamo-comment-mode t))

(defun mumamo-chunk-mako-<%include (pos min max)
  (mumamo-quick-static-chunk pos min max "<%include" "/>" t 'html-mode t))

(defun mumamo-chunk-mako-<%inherit (pos min max)
  (mumamo-quick-static-chunk pos min max "<%inherit" "/>" t 'html-mode t))

(defun mumamo-chunk-mako-<%namespace (pos min max)
  (mumamo-quick-static-chunk pos min max "<%namespace" "/>" t 'html-mode t))

(defun mumamo-chunk-mako-<%page (pos min max)
  (mumamo-quick-static-chunk pos min max "<%page" "/>" t 'html-mode t))

;; Fix-me: this is not correct
(defun mumamo-chunk-mako-<%def (pos min max)
  (mumamo-quick-static-chunk pos min max "<%def" "</%def>" t 'html-mode t))

(defun mumamo-chunk-mako$(pos min max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "${" "}" t 'python-mode t))

;;;###autoload
(define-mumamo-multi-major-mode mako-html-mumamo-mode
  "Turn on multiple major modes for Mako with main mode `html-mode'.
This also covers inlined style and javascript."
;; Fix-me: test case
;;
;; Fix-me: Add chunks for the tags, but make sure these are made
;; invisible to nxml-mode parser.
;;
;; Fix-me: Maybe finally add that indentation support for one-line chunks?
  ("Mako HTML Family" html-mode
   (
    mumamo-chunk-mako-one-line-comment
    mumamo-chunk-mako-<%doc
    mumamo-chunk-mako-<%include
    mumamo-chunk-mako-<%inherit
    mumamo-chunk-mako-<%namespace
    mumamo-chunk-mako-<%page

    mumamo-chunk-mako-<%def
    ;;mumamo-chunk-mako-<%namesp:name
    ;;mumamo-chunk-mako-<%call
    ;;mumamo-chunk-mako-<%text

    mumamo-chunk-mako-<%
    mumamo-chunk-mako-%
    mumamo-chunk-mako$

    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(mumamo-inherit-sub-chunk-family-locally 'mako-html-mumamo-mode 'mako-html-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XSL

;;;###autoload
(define-mumamo-multi-major-mode xsl-nxml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `nxml-mode'.
This covers inlined style and javascript."
  ("XSL nXtml Family" nxml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;###autoload
(define-mumamo-multi-major-mode xsl-sgml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `sgml-mode'.
This covers inlined style and javascript."
  ("XSL SGML Family" sgml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown

(defun mumamo-chunk-markdown-html-1 (pos min max)
  (save-restriction
    (goto-char pos)
    (narrow-to-region (or min (point)) (or max (point-max)))
    (save-match-data
      (let ((here (point)))
        (when (re-search-forward (rx (* space)
                                     (submatch "<")
                                     (* (any "a-z"))
                                     (or ">" (any " \t\n")))
                                 nil t)
          (let ((beg (match-beginning 1))
                (end))
            (goto-char beg)
            (condition-case err
                (progn
                  (while (not (sgml-skip-tag-forward 1)))
                  (setq end (point)))
              (error (message "mumamo-chunk-markdown-html-1: %s" err)))
            (goto-char here)
            (when (and beg end)
              (cons beg end))))))))

(defun mumamo-chunk-markdown-html-fw-exc-fun (pos max)
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos nil max)))
    (cdr beg-end)))

(defun mumamo-chunk-markdown-html (pos min max)
  "Find a chunk of html code in `markdown-mode'.
Return range and `html-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos nil max)))
    (when beg-end
      (let ((beg (car beg-end))
            (end (cdr beg-end)))
        (list beg end 'html-mode
              nil ;; borders
              nil ;; parseable y
              'mumamo-chunk-markdown-html-fw-exc-fun
              nil ;; find-borders fun
              )))))

;;;###autoload
(define-mumamo-multi-major-mode markdown-html-mumamo-mode
  "Turn on multi major markdown mode in buffer.
Main major mode will be `markdown-mode'.
Inlined html will be in `html-mode'.

You need `markdown-mode' which you can download from URL
`http://jblevins.org/projects/markdown-mode/'."
  ("Markdown HTML Family" markdown-mode
   (
    mumamo-chunk-markdown-html
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Latex related

(defun mumamo-latex-closure-chunk (pos min max)
  (mumamo-quick-static-chunk pos min max "\\begin{clojure}" "\\end{clojure}" t 'clojure-mode t))

;;;###autoload
(define-mumamo-multi-major-mode latex-clojure-mumamo-mode
  "Turn on multi major mode latex+clojure.
Main major mode will be `latex-mode'.
Subchunks will be in `clojure-mode'.

You will need `clojure-mode' which you can download from URL
`http://github.com/jochu/clojure-mode/tree'."
  ("Latex+clojur Family" latex-mode
   (
    mumamo-latex-closure-chunk
    )))

(add-to-list 'auto-mode-alist '("\\.lclj\\'" . latex-clojure-mumamo-mode))


(defun mumamo-latex-haskell-chunk (pos min max)
  (mumamo-quick-static-chunk pos min max "\\begin{code}" "\\end{code}" t 'haskell-mode t))

;;;###autoload
(define-mumamo-multi-major-mode latex-haskell-mumamo-mode
  "Turn on multi major mode latex+haskell.
Main major mode will be `latex-mode'.
Subchunks will be in `haskell-mode'.

You will need `haskell-mode' which you can download from URL
`http://projects.haskell.org/haskellmode-emacs/'."
  ("Latex+haskell Family" latex-mode
   (
    mumamo-latex-haskell-chunk
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python + ReST

;; From Martin Soto

(defun python-rst-long-string-chunk (pos min max)
 "Find Python long strings.  Return range and 'mumamo-comment-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
 ;;(mumamo-quick-static-chunk pos min max "\"\"\"((" "))\"\"\"" nil 'rst-mode nil))
 (mumamo-quick-static-chunk pos min max "\"\"\"" "\"\"\"" t 'rst-mode t))

;;;###autoload
(define-mumamo-multi-major-mode python-rst-mumamo-mode
 "Turn on multiple major modes for Python with RestructuredText docstrings."
 ("Python ReST Family" python-mode
  (
   python-rst-long-string-chunk
   )))


(provide 'mumamo-fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-fun.el ends here

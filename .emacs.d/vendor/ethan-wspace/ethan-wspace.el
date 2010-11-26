;;; ethan-wspace.el
;;; Whitespace customizations.
;;
;; For more information on the design of this package, please see the
;; README that came with it.
;;
;; Editing whitespace. I don't like to forcibly clean[1] whitespace
;; because when doing diffs/checkins, it pollutes changesets. However,
;; I prefer clean whitespace -- often I've deleted whitespace by
;; accident and been unable to "put it back" for purposes of diffing.
;;
;; Therefore, my approach is hybrid -- maintain clean whitespace where
;; possible, and avoid disturbing messy whitespce when I come across
;; it. I add a find-file-hook to track whether the whitespace of a
;; given file was clean to begin with, and add a before-save-hook that
;; uses this information to forcibly clean whitespace only if it was
;; clean at start.
;;
;; Viewing whitespace. Originally I had a font-lock-mode hook that
;; always called show-trailing-whitespace and show-tabs, but this
;; turns out to be annoying for a lot of reasons. Emacs internal
;; buffers like *Completions* and *Shell* would get highlit. So I
;; decided that a more sophisticated approach was called for.
;;
;; 1) If a buffer does not correspond to a file, I almost certainly do
;; not care whether the whitespace is clean. I'm not going to be
;; saving it, after all.
;;
;; 2) If a buffer corresponds to a file that was originally clean, I
;; still don't really care about the whitespace, since my hook (above)
;; would ensure that it was clean going forward.
;;
;; 3) If a buffer corresponds to a file that was not originally clean,
;; I do care about seeing whitespace, because I do not want to edit it
;; by accident.
;;
;; There are a few exceptions -- if I'm looking at a patch, or hacking
;; a Makefile, whitespace is different. More about those later.
;;
;; This file adds hooks to make this stuff happen -- check whether the
;; whitespace is clean when a file is first found, and preserve that
;; cleanliness if so, and highlight that dirtiness if not. It does
;; this by setting show-trailing-whitespace when necessary.
;;
;; NOTE: take out any customizations like this:
;; '(show-trailing-whitespace t)
;; show-trailing-whitespace will be turned on by ethan-wspace.
;;
;; Also disable '(require-final-newlines t); ethan-wspace will handle
;; the final newlines.
;;
;; FIXME: Coding conventions suggest using (define-* thing-name) for generated stuff.
;;
;; FIXME: fancy lighter
;;
;; FIXME: buffer-local-ize ethan-wspace-errors
;;
;; FIXME: coding conventions suggest adding a ethan-wspace-unload-hook to unhook

;; We store the whitespace types here.
;; Currently each whitespace type is represented as an association list
;; with keys :check, :clean, and :highlight, and values symbols of functions
;; or whatever.
(require 'assoc)

(defvar ethan-wspace-types nil
  "The list of all known whitespace types.")

;; Define the format/structure for the each wspace type. Right now it's
;; (name . (:foo bar :baz quux)), aka (name :foo bar :baz :quux).
(defun ethan-wspace-add-type (name args)
  (aput 'ethan-wspace-types name args))

(defun ethan-wspace-get-type (name)
  (or (assq name ethan-wspace-types)
      (error "Ethan Wspace Type '%s' does not exist" name)))

(defun ethan-wspace-type-get-field (type field)
  (plist-get (cdr type) field))

(defun ethan-wspace-all-error-types ()
  "The list of all currently-defined types as symbol names."
  ;; Repeated loads could define types multiple times, so we define
  ;; this slightly ugly mechanism that stores symbols uniquely in an
  ;; association list, and then pulls out the names.
  (let ((type-names nil))
    (mapc '(lambda (type) (aput 'type-names (car type) t)) ethan-wspace-types)
    (mapcar 'car type-names)))

(defun ethan-wspace-buffer-errors ()
  (let ((errors nil))
    (dolist (type (ethan-wspace-all-error-types))
      (when (eval (ethan-wspace-type-clean-mode-symbol type))
        (setq errors (cons type errors))))))
;; This variable isn't used. FIXME: provide a function that returns
;; this as a list, for use in customizations.

;; (defvar ethan-wspace-buffer-errors nil
;; "Keep track, per-buffer, which whitespace errors a file has.

;; For possible values, see ethan-wspace-errors.")
;; (make-variable-buffer-local 'ethan-wspace-buffer-errors)

;; Need to set up something like yas/global-mode, which turns on/off
;; ethan-wspace globally

;; Each type of whitespace has:
;; - a find function, which when run, returns (begin end) of the next instance
;;   of this whitespace (or nil if none)
;; - a clean function, which is run to eliminate this kind of whitespace.
;; - a highlight function, which is run with one argument -- 1 to turn
;;   it on, -1 to turn it off. This is probably be a minor mode.
;; FIXME: change to define-ethan-wspace-type?
(defmacro ethan-wspace-declare-type (name &rest args)
  "Declare a whitespace type.

Options recognized: :find :clean :highlight :description
:description is used in docstrings and should be plural."

   (let* ((name-str (symbol-name name))
        (clean-mode-name (concat "ethan-wspace-clean-" name-str "-mode"))
        (clean-mode (intern clean-mode-name))
        (highlight-mode-name (concat "ethan-wspace-highlight-" name-str "-mode"))
        (highlight-mode (intern highlight-mode-name))
        (description (or (plist-get args :description)
                         (and (plist-put args :description name-str)
                              name-str)))
        (letter (or (plist-get args :letter)
                    (and (plist-put args :letter (substring (symbol-name name) 0 1))
                         (plist-get args :letter)))))
  `(progn
     (ethan-wspace-add-type ',name ',args)
     (define-minor-mode ,clean-mode
       ,(format "Verify that %s are clean in this buffer.

Works as a save-file hook that cleans %s.
Turning this mode on turns off highlighting %s, and vice versa." description description description)
       :init-value nil :lighter nil :keymap nil
       ;; FIXME: Body goes here
)


     ,(let ((disable-highlight (intern (concat clean-mode-name "-disable-highlight")))
            (disable-clean     (intern (concat highlight-mode-name "-disable-clean"))))
        ;; Defining these as symbols so that multiple loads don't
        ;; add multiple functions
        `(progn
           (defun ,disable-highlight ()
             (when ,clean-mode (,highlight-mode -1)))
           (defun ,disable-clean ()
             (when ,highlight-mode (,clean-mode -1)))
           (add-hook ',(intern (concat clean-mode-name "-hook"))
                     ',disable-highlight)
           (add-hook ',(intern (concat highlight-mode-name "-hook"))
                     ',disable-clean)))
   )
))

(defun ethan-wspace-type-find (type-or-name)
  "Call the find function for wspace type `type-or-name'.

Type can be either the symbol name of the type, or the type
object itself."
  (let* ((type (if (symbolp type-or-name)
                   (ethan-wspace-get-type type-or-name)
                 type-or-name))
         (find-func (ethan-wspace-type-get-field type :find)))
    (apply find-func nil)))

(defun ethan-wspace-type-check (type-or-name)
  "Check for any instance of this wspace type at all.

Returns t or nil."
  (when (ethan-wspace-type-find type-or-name) t))

(defun ethan-wspace-type-clean (type-or-name &optional begin end)
  "Calls the clean function for wspace type `type-or-name'.

Optional `begin' and `end' are the range to clean.
If absent, use the whole buffer."
  (let* ((type (if (symbolp type-or-name)
                  (ethan-wspace-get-type type-or-name)
                type-or-name))
         (clean-func (ethan-wspace-type-get-field type :clean))
         (real-begin (or begin (point-min)))
         (real-end (or end (point-max))))
    (apply clean-func (list real-begin real-end))))

(defun ethan-wspace-type-clean-mode-symbol (type-name)
  "Return the symbol-name of the variable for the clean-mode for `type-name'.

For example, (ethan-wspace-type-clean-mode-symbol 'tabs) => 'ethan-wspace-type-tabs-clean-mode."
  (let* ((name-str (if (symbolp type-name) (symbol-name type-name) type-name))
         (clean-mode-name (concat "ethan-wspace-clean-" name-str "-mode")))
    (intern clean-mode-name)))

(defun ethan-wspace-type-activate (type-name)
  "Turn \"on\" the whitespace type given by `type-name'.

If the whitespace for this type is clean, turn on the clean-mode
for this type. Otherwise, turn on the highlight mode for this
type."
  (if (ethan-wspace-type-check type-name)
     (ethan-wspace-type-activate-highlight type-name)
    (ethan-wspace-type-activate-clean type-name)))

(defun ethan-wspace-type-deactivate (type-name)
  "Turn \"off\" the whitespace type given by `type-name'.

This turns off both highlighting and cleaning for this type. You're on your own."
  (ethan-wspace-type-activate-highlight type-name -1)
  (ethan-wspace-type-activate-clean type-name -1))

(defun ethan-wspace-type-activate-clean (type-name &optional value)
  "Used to activate and deactivate clean-modes for a type.

Set `value' to -1 to deactivate a clean-mode."
  (let ((clean-mode (ethan-wspace-type-clean-mode-symbol type-name)))
    (apply clean-mode (list (or value 1)))))

(defun ethan-wspace-type-clean-mode-active (type-name)
  (let ((clean-mode (ethan-wspace-type-clean-mode-symbol type-name)))
    (eval clean-mode)))

(defun ethan-wspace-type-activate-highlight (type-name &optional value)
  "Used to activate and deactivate highlight-modes for a type.

Set `value' to -1 to deactivate a highlight-mode."
  (let* ((type (ethan-wspace-get-type type-name))
         (highlight-mode (ethan-wspace-type-get-field type :highlight)))
    (apply highlight-mode (list (or value 1)))))

(defun ethan-wspace-type-name-from-indicator (indicator)
  (let ((found nil))
    (dolist (elt (ethan-wspace-all-error-types))
      (when (equal (ethan-wspace-type-get-field (ethan-wspace-get-type elt) :letter) (downcase indicator))
        (setq found elt)))
    (unless found (error "Type not found for indicator %s" indicator))
    found))

;;; TABS
(defvar ethan-wspace-type-tabs-regexp "\t+"
  "The regexp to use when searching for tabs.")

(defun ethan-wspace-type-tabs-find ()
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward ethan-wspace-type-tabs-regexp nil t)))
      (when pos (cons (match-beginning 0) (match-end 0))))))

(defun ethan-wspace-untabify (start end)
  "Like `untabify', but be more respectful of point.
The standard `untabify' does not keep track of point when
deleting tabs and replacing with spaces. This means point can
jump to the beginning of a tab if it was at the end of a tab when
untabify was called.

Since we clean tabs implicitly, we don't want point to jump around.
For details on how we accomplish this, see the source."
  ;; Stock untabify does three things that cause this problem together:
  ;;
  ;; 1. untabify deletes tabs before inserting spaces, which collapses
  ;;    the point-before-tabs and point-after-tabs cases.
  ;;
  ;; 2. save-excursion saves position of point using a marker, but
  ;;    since the insertion type of that marker is nil, inserting
  ;;    spaces before the marker don't move the marker. This is fine
  ;;    when point-before-tabs but wrong when point-after-tabs.  (We
  ;;    use insert-before-markers instead of indent-to-column to
  ;;    handle this.)
  ;;
  ;; 3. Point could be between two tabs, but untabify replaces
  ;;    multiple tabs at once, which is guaranteed to give you the
  ;;    wrong result.
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char start)
      (while (search-forward "\t" nil t)        ; faster than re-search
        (forward-char -1)
        (let ((tab-start (point))
              (column-start (current-column)))
          (forward-char 1)
          (let ((column-end (current-column)))
            ;; Insert text after tabs, but before markers, so that
            ;; point-marker (if after tab) will be advanced.
            (insert-before-markers (make-string (- column-end column-start) ?\ ))
            ;; Delete tabs, so we can handle before-tabs and after-tabs
            ;; separately.
            (delete-region tab-start (1+ tab-start))))))))

(defvar ethan-wspace-type-tabs-keyword
  (list ethan-wspace-type-tabs-regexp 0 (list 'quote 'ethan-wspace-face) t)
  "The font-lock keyword used to highlight tabs.")

(define-minor-mode ethan-wspace-highlight-tabs-mode
  "Minor mode to highlight tabs.

With arg, turn tab-highlighting on if arg is positive, off otherwise.
This supercedes (require 'show-wspace) and show-ws-highlight-tabs."
  :init-value nil :lighter nil :keymap nil
  (if ethan-wspace-highlight-tabs-mode
      (font-lock-add-keywords nil (list ethan-wspace-type-tabs-keyword))
    (font-lock-remove-keywords nil (list ethan-wspace-type-tabs-keyword)))
  (font-lock-fontify-buffer))

(ethan-wspace-declare-type tabs :find ethan-wspace-type-tabs-find
                           :clean ethan-wspace-untabify :highlight ethan-wspace-highlight-tabs-mode
                           )


;; Trailing whitespace on each line: symbol `eol', lighter L.  Named
;; "eol" to distinguish from final-newline whitespace (ensuring that
;; there is exactly one newline at EOF).

(defvar ethan-wspace-blank-chars (list ?\t ?\ (decode-char 'ucs #x3000))
  "The list of blank (non-newline whitespace) characters we know about.

This list is used to highlight and clean invisible whitespace at
end of line.

Right now this includes tab (0x09), blank (x20), and fullwidth
space (x3000). If there is a whitespace character used more
commonly in your language, email me.")

(defvar ethan-wspace-type-eol-regexp (concat "[" ethan-wspace-blank-chars "]+$")
  "The regexp to use to find end-of-line whitespace.

We use a hard-coded list of whitespace `ethan-wspace-blank-chars'
rather than \\s- because \"\\s-+$\" matches \"\\n\\n\" -- which
leads us to treat multiple newlines as trailing end-of-line
whitespace. If there is a whitespace character used more commonly
in your language, email me.")

(defun ethan-wspace-type-eol-find ()
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward ethan-wspace-type-eol-regexp nil t)))
      (when pos (cons (match-beginning 0) (match-end 0))))))

;;; Implemented using show-trailing-whitespace.
;;;
;;; emacs core handles show-trailing-whitespace specially. It's
;;; impossible (AFAICT) to use font-lock keywords to highlight
;;; trailing whitespace except when point is right afterwards, so we
;;; just use show-trailing-whitespace.
(define-minor-mode ethan-wspace-highlight-eol-mode
  "Minor mode to highlight trailing whitespace.

With arg, turn highlighting on if arg is positive, off otherwise.
This internally uses `show-trailing-whitespace'."
  :init-value nil :lighter nil :keymap nil
  (setq show-trailing-whitespace ethan-wspace-highlight-eol-mode))

(defun ethan-wspace-type-eol-clean (begin end)
  (save-restriction
    (narrow-to-region begin end)
    (delete-trailing-whitespace)))

(ethan-wspace-declare-type eol :find ethan-wspace-type-eol-find
                           :clean ethan-wspace-type-eol-clean
                           :highlight ethan-wspace-highlight-eol-mode
                           :letter "l" )   ; for "lines"


;;; "No newline at end of file": symbol `no-nl-eof'
(defun ethan-wspace-count-trailing-nls ()
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (- (point-max) (point))))

(defun ethan-wspace-type-no-nl-eof-find ()
  (let* ((trailing-newlines (ethan-wspace-count-trailing-nls))
         (max (point-max)))
    (if (and (= trailing-newlines 0)
             (not (= (buffer-size) 0)))  ; don't count empty files as dirty
        (cons max max)
      nil)))

(defun ethan-wspace-type-no-nl-eof-clean (begin end)
  ;; FIXME: could respect begin, end
  (save-match-data
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (if (and (not (looking-at "\n"))
               (not (= (buffer-size) 0)))   ; don't need to clean empty files
          (insert "\n")))))



;;; This is a failed attempt at using a font-lock keyword to look for
;;; trailing newlines.  It doesn't work because there's no regexp that
;;; matches EOF. I'm keeping it here in case I want to use the
;;; define-ethan-wspace-highlight-mode macro.

;; (defvar ethan-wspace-type-trailing-newline-keyword
;;   (list ethan-wspace-type-trailing-newline-regexp 0 (list 'quote 'ethan-wspace-face) t)
;;   "The font-lock keyword used to highlight trailing newlines.")

;; (defmacro define-ethan-wspace-highlight-mode (name doc &rest args)
;;   (let ((font-lock-keyword (plist-get args :font-lock-keyword)))
;;     (unless font-lock-keyword
;;       (error "Font-lock keyword sucked somehow: %s" args))

;;     `(define-minor-mode ,name ,doc
;;        :init-value nil :lighter nil :keymap nil
;;        (if ,name
;;            (font-lock-add-keywords nil (list ,font-lock-keyword))
;;          (font-lock-remove-keywords nil (list ,font-lock-keyword)))
;;        (font-lock-fontify-buffer))))

;; (define-ethan-wspace-highlight-mode ethan-wspace-highlight-trailing-newlines-mode
;;   "Minor mode to highlight trailing newlines if absent or if more than 1.

;; With arg, turn highlighting on if arg is positive, off otherwise."
;;   :font-lock-keyword ethan-wspace-type-trailing-newline-keyword)


;; Implemented using overlays to mark a fake "eof" string, or
;; highlight newlines at end of file.  I couldn't figure out a way to
;; make the eof have a trailing newline AND keep the cursor on the
;; first character -- putting a \n in the overlay seems to force the
;; cursor to the newline afterwards -- so I just extend the overlay to
;; edge-of-frame manually. This kind of sucks but it'll hopefully do
;; for now.
(defvar ethan-wspace-highlight-no-nl-eof-overlay nil
  "An overlay used to indicate that trailing newlines are missing.")

(make-variable-buffer-local 'ethan-wspace-highlight-no-nl-eof-overlay)

(defun ethan-wspace-highlight-no-nl-eof-make-overlay ()
  (setq ethan-wspace-highlight-no-nl-eof-overlay
        (or ethan-wspace-highlight-no-nl-eof-overlay (make-overlay 0 0))))

(defun ethan-wspace-highlight-no-nl-eof-overlay-off ()
  (and ethan-wspace-highlight-no-nl-eof-overlay
       (overlay-put ethan-wspace-highlight-no-nl-eof-overlay 'after-string nil)))

(define-minor-mode ethan-wspace-highlight-no-nl-eof-mode
  "Minor mode to highlight trailing newlines if absent or if more than 1.

With arg, turn highlighting on if arg is positive, off otherwise."
  :init-value nil :lighter nil :keymap nil
  (if ethan-wspace-highlight-no-nl-eof-mode
      (ethan-wspace-highlight-no-nl-eof-make-overlay)
    (ethan-wspace-highlight-no-nl-eof-overlay-off)))

(defun ethan-wspace-highlight-no-nl-eof-overlay-update ()
  "Update the overlay to show that there is no trailing newline at end of file."
  (ethan-wspace-highlight-no-nl-eof-overlay-off)
  (when (= (ethan-wspace-count-trailing-nls) 0)
    (save-excursion
      (move-overlay ethan-wspace-highlight-no-nl-eof-overlay (point-max) (point-max))
      (goto-char (point-max))
      (let* ((str-len (- (frame-width) (current-column)))
             (str (concat "eof" (make-string (- str-len 3) ?\ ))))
        (set-text-properties 0 str-len '(face ethan-wspace-face) str)
        (set-text-properties 0 1 '(cursor t face ethan-wspace-face) str)
        (overlay-put ethan-wspace-highlight-no-nl-eof-overlay 'after-string str)))))

(ethan-wspace-declare-type no-nl-eof :find ethan-wspace-type-no-nl-eof-find
                           :clean ethan-wspace-type-no-nl-eof-clean
                           :highlight ethan-wspace-highlight-no-nl-eof-mode
                           :description "trailing newlines")


;;; More than one newline at end of file: symbol `many-nls-eof'
(defun ethan-wspace-type-many-nls-eof-find ()
  (let* ((trailing-newlines (ethan-wspace-count-trailing-nls))
         (max (point-max)))
    (if (> trailing-newlines 1)
        (cons (- max (1- trailing-newlines)) max)
      nil)))

(defun ethan-wspace-type-many-nls-eof-clean (begin end)
  (save-match-data
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (when (looking-at "\n")                  ; If no \n, nothing to clean.
        (forward-char)                         ; skip a newline
        (delete-region (point) (point-max)))))) ; delete others

(defvar ethan-wspace-highlight-many-nls-eof-overlay nil
  "The overlay to use when highlighting too-many-newlines.")

(make-variable-buffer-local 'ethan-wspace-highlight-many-nls-eof-overlay)

(defun ethan-wspace-highlight-many-nls-eof-make-overlay ()
  (setq ethan-wspace-highlight-many-nls-eof-overlay
        (or ethan-wspace-highlight-many-nls-eof-overlay (make-overlay 0 0))))

(defun ethan-wspace-highlight-many-nls-eof-overlay-off ()
  (and ethan-wspace-highlight-many-nls-eof-overlay
       (overlay-put ethan-wspace-highlight-many-nls-eof-overlay 'face nil)))

(define-minor-mode ethan-wspace-highlight-many-nls-eof-mode
  "Minor mode to highlight trailing newlines if absent or if more than 1.

With arg, turn highlighting on if arg is positive, off otherwise."
  :init-value nil :lighter nil :keymap nil
  (if ethan-wspace-highlight-many-nls-eof-mode
      (ethan-wspace-highlight-many-nls-eof-make-overlay)
    (ethan-wspace-highlight-many-nls-eof-overlay-off)))

(defun ethan-wspace-highlight-many-nls-eof-overlay-update ()
  "Update the overlay to highlight the newlines at EOF."
  (ethan-wspace-highlight-many-nls-eof-overlay-off)
  (when (< 1 (ethan-wspace-count-trailing-nls))
    (save-excursion
      (goto-char (point-max))             ; end of file
      (skip-chars-backward "\n")          ; backwards through all of them
      (forward-char 1)                      ; leave one, which is the correct amount
      (move-overlay ethan-wspace-highlight-many-nls-eof-overlay (point) (point-max))
      (overlay-put ethan-wspace-highlight-many-nls-eof-overlay 'face 'ethan-wspace-face))))

(ethan-wspace-declare-type many-nls-eof :find ethan-wspace-type-many-nls-eof-find
                           :clean ethan-wspace-type-many-nls-eof-clean
                           :highlight ethan-wspace-highlight-many-nls-eof-mode
                           :description "trailing newlines")


;;; Mode-Line stuff
(defun ethan-wspace-type-get-mode-line-clean-help (type-name)
  "Generate a help-echo text for clean-mode letters for `type-name'."
  (let* ((type (ethan-wspace-get-type type-name))
         (friendly-name (ethan-wspace-type-get-field type :description)))
    (format "Cleaning %s\nmouse-1: Switch to highlighting %s\nmouse-2: Show help for minor mode\nmouse-3: Toggle minor modes" friendly-name friendly-name)))

(defun ethan-wspace-type-get-mode-line-highlight-help (type-name)
  "Generate a help-echo text for clean-mode letters for `type-name'."
  (let* ((type (ethan-wspace-get-type type-name))
         (friendly-name (ethan-wspace-type-get-field type :description)))
    (format "Highlighting %s\nmouse-1: Switch to cleaning %s\nmouse-2: Show help for minor mode\nmouse-3: Toggle minor modes" friendly-name friendly-name)))

(defun ethan-wspace-menu-activate-clean (event)
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (ethan-wspace-type-activate-clean (ethan-wspace-type-name-from-indicator indicator))))

(defun ethan-wspace-menu-activate-highlight (event)
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (ethan-wspace-type-activate-highlight (ethan-wspace-type-name-from-indicator indicator))))

(defun ethan-wspace-type-get-mode-line-element (type-name)
  (let* ((type (ethan-wspace-get-type type-name))
         (letter (ethan-wspace-type-get-field type :letter))
         (cap-letter (capitalize letter))
         (clean-mode (ethan-wspace-type-clean-mode-symbol type-name))
         (highlight  (ethan-wspace-type-get-field (ethan-wspace-get-type type-name) :highlight)))
    (list
     (list clean-mode
           (list :propertize letter
                       'help-echo (ethan-wspace-type-get-mode-line-clean-help type-name)
                       'local-map '(keymap
                                    (mode-line keymap
                                               (down-mouse-1 . ethan-wspace-menu-activate-highlight)))
                       'mouse-face 'mode-line-highlight))
     (list highlight
           (list :propertize cap-letter
                       'help-echo (ethan-wspace-type-get-mode-line-highlight-help type-name)
                       'local-map '(keymap
                                    (mode-line keymap
                                               (down-mouse-1 . ethan-wspace-menu-activate-clean)))
                       'mouse-face 'mode-line-highlight)))))

(defvar ethan-wspace-mode-line-element
  (let ((mode-line '(" ew:")))
    (mapc (lambda (type)
              (setq mode-line
                    (cons (ethan-wspace-type-get-mode-line-element type) mode-line)))
            (ethan-wspace-all-error-types))
    (list (reverse mode-line)))
  "The mode-line element for ethan-wspace.

Typically looks like: \"ew:tLNm\".")
(put 'ethan-wspace-mode-line-element 'risky-local-variable t)


;;; ethan-wspace-mode: doing stuff for all types.
(defgroup ethan-wspace nil
  "Be extremely OCD about whitespace in files."
  :link '(emacs-library-link :tag "Lisp Source" "ethan-wspace.el")
  :group 'programming)

(defcustom ethan-wspace-errors (ethan-wspace-all-error-types)
  "The list of errors that a user wants recognized."
  :group 'ethan-wspace)

;; Let this get changed per-buffer
(make-variable-buffer-local 'ethan-wspace-errors)

(defun ethan-wspace-alpha-blend (bg fg alpha)
  (let ((newcolor nil))
    (dolist (i '(2 1 0))
      (let* ((bgi (nth i bg))
             (fgi (nth i fg))
             (newi (+ (* fgi alpha)
                      (* bgi (- 1 alpha)))))
        (setq newcolor (cons newi newcolor))))
    newcolor))

(defun ethan-wspace-appropriate-face (&optional frame)
  (let* ((bg (let
                 ((frameprop (aget (frame-parameters frame) 'background-color)))
               (if (or (null frameprop) ; try to handle undefined bgs by using white
                       (equal "unspecified" frameprop)
                       (equal "unspecified-bg" frameprop))
                   "white"
                 frameprop)))
         (rgb (color-values bg))
         (new-color (ethan-wspace-alpha-blend rgb '(65535 0 0) 0.2))
         (hex (apply 'format "#%04x%04x%04x" new-color)))
    (setq ethan-wspace-against-background bg)
    (list :background hex)))

;; FIXME: some way to keep trailing-whitespace in sync with this?
(defvar ethan-wspace-against-background nil
  "The last background we used to compute ethan-wspace-face.")

(defface ethan-wspace-face
  `((t ,(ethan-wspace-appropriate-face)))
  "Face used to highlight whitespace.

Ideally it is visible without being obtrusive.

This face gets updated automatically whenever the
background-color of the frame is changed. If you want to
customize this face yourself, please set ethan-wspace-face-customized."
  :group 'ethan-wspace)

(defcustom ethan-wspace-face-customized nil
  "Whether the whitespace face has been customized.

If this is `nil', we recompute an appropriate face by
alpha-blending some red onto the frame background color every
time the frame background color changes. If `t', we assume the
user knows best."
  :group 'ethan-wspace)

;; show-trailing-whitespace uses the face `trailing-whitespace', so we
;; make this mirror `ethan-wspace-face'.
;(copy-face 'ethan-wspace-face 'trailing-whitespace)
(defun ethan-wspace-face-updated ()
  (let ((attribs (list (list t (face-attr-construct 'ethan-wspace-face)))))
    (face-spec-set 'trailing-whitespace attribs nil)))

(ethan-wspace-face-updated)

(defun ethan-wspace-update-face (&optional frame)
  ;(message "Computing face %S %s %S" frame (frame-parameter frame 'name) (frame-parameters frame))
  (unless (or ethan-wspace-face-customized
              ; Don't compute face for tooltips; causes breakage with "Invalid face"
              (equal (frame-parameter frame 'name) "tooltip")
              (eq ethan-wspace-against-background
                  (aget (frame-parameters frame) 'background-color)))
    ;(message "updating face for frame %s : was %S, now %S" frame ethan-wspace-against-background (aget (frame-parameters frame) 'background-color))
    (face-spec-set 'ethan-wspace-face (list (list t (ethan-wspace-appropriate-face frame))))
    (ethan-wspace-face-updated)))

(add-hook 'window-configuration-change-hook 'ethan-wspace-update-face)

;; Some pre/post command hooks for dealing with overlays
(defun ethan-wspace-pre-command-hook ()
  ;; For some reason vline uses this to turn off the overlay.
  ;; Probably for ease of creating new overlays without having to
  ;; delete old ones?  No idea, but I'm just copying what they do,
  ;; so..
  ())

(defun ethan-wspace-post-command-hook ()
  ;; FIXME: needs to update in each buffer!
  (when ethan-wspace-highlight-no-nl-eof-mode
    (ethan-wspace-highlight-no-nl-eof-overlay-update))
  (when ethan-wspace-highlight-many-nls-eof-mode
    (ethan-wspace-highlight-many-nls-eof-overlay-update)))

(defvar ethan-wspace-errors-in-buffer-hook nil
  "Hook run by `ethan-wspace-mode' to allow customizing the errors to look for.

A useful hook might be:

  (lambda () (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))")

;;;###autoload
(define-minor-mode ethan-wspace-mode
  "Minor mode for coping with whitespace.

This just activates each whitespace type in this buffer."
  :init-value nil :lighter ethan-wspace-mode-line-element :keymap nil
  ;(message "Turning on ethan-wspace mode for %s" (buffer-file-name))
  (if ethan-wspace-mode
      (progn
        (run-hooks 'ethan-wspace-errors-in-buffer-hook)
        (ethan-wspace-update-buffer)
        (add-hook 'pre-command-hook 'ethan-wspace-pre-command-hook nil t)
        (add-hook 'post-command-hook 'ethan-wspace-post-command-hook nil t)
        (add-hook 'hack-local-variables-hook
                  (function ethan-wspace-hack-local-variables-hook) t t))
    (remove-hook 'pre-command-hook  'ethan-wspace-pre-command-hook t)
    (remove-hook 'post-command-hook 'ethan-wspace-command-hook t)
    (remove-hook 'hack-local-variables-hook
                 (function ethan-wspace-hack-local-variables-hook) t)))

(defun ethan-wspace-update-buffer ()
  (interactive)
  (dolist (type (ethan-wspace-all-error-types))
    (if (not (memq type ethan-wspace-errors))
        (ethan-wspace-type-deactivate type)
      (ethan-wspace-type-activate type))))

(defun ethan-wspace-clean-all-modes ()
  "*Turn on clean mode for all whitespace modes."
  (interactive)
  (dolist (type ethan-wspace-errors)
    (ethan-wspace-type-activate-clean type)))

(defun ethan-wspace-is-buffer-appropriate ()
  ;;; FIXME: not OK for diff mode, non-file modes, etc?
  (when (buffer-file-name)
    (ethan-wspace-mode 1)))

(define-global-minor-mode global-ethan-wspace-mode
  ethan-wspace-mode ethan-wspace-is-buffer-appropriate
  :init-value t)

(defun ethan-wspace-clean-before-save-hook ()
  (when ethan-wspace-mode
    (dolist (type ethan-wspace-errors)
      (when (ethan-wspace-type-clean-mode-active type)
          (ethan-wspace-type-clean type)))))

(add-hook 'before-save-hook 'ethan-wspace-clean-before-save-hook)

(defun ethan-wspace-hack-local-variables-hook ()
        (ethan-wspace-update-buffer))

;; Diff mode.
;;
;; Every line in a diff starts with a plus, a minus, or a space -- so
;; empty lines in common with both files will show up as lines with
;; just a space in the diff. As a result, we only highlight trailing
;; whitespace for non-empty lines.
;;
;; The regex matches whitespace that only comes at
;; the end of a line with non-space in it.
;;
; FIXME: This also makes the diff-mode font lock break a little --
; changes text color on lines that match.
;
; FIXME: "bzr diff" format contains tabs?
;; (add-hook 'diff-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(("\\S-\\([\240\040\t]+\\)$"
;;                                        (1 'show-ws-trailing-whitespace t))))))

(provide 'ethan-wspace)

;;; icicles-face.el --- Faces for Icicles
;;
;; Filename: icicles-face.el
;; Description: Faces for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2010, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:19:43 2006
;; Version: 22.0
;; Last-Updated: Thu Aug 12 13:57:45 2010 (-0700)
;;           By: dradams
;;     Update #: 519
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-face.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
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
;;  This is a helper library for library `icicles.el'.  It defines
;;  customization groups and faces.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Groups defined here:
;;
;;    `Icicles', `Icicles-Buffers', `Icicles-Completions-Display',
;;    `Icicles-Files', `Icicles-Key-Bindings',
;;    `Icicles-Key-Completion', `Icicles-Matching',
;;    `Icicles-Minibuffer-Display', `Icicles-Miscellaneous',
;;    `Icicles-Searching'.
;;
;;  Faces defined here:
;;
;;    `icicle-candidate-part',
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input', `icicle-completion',
;;    `icicle-Completions-instruction-1',
;;    `icicle-Completions-instruction-2',
;;    `icicle-current-candidate-highlight', `icicle-extra-candidate',
;;    `icicle-historical-candidate', `icicle-input-completion-fail',
;;    `icicle-input-completion-fail-lax',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-mode-line-help',
;;    `icicle-multi-command-completion',
;;    `icicle-mustmatch-completion', `icicle-proxy-candidate',
;;    `icicle-saved-candidate', `icicle-search-context-level-1',
;;    `icicle-search-context-level-2',
;;    `icicle-search-context-level-3',
;;    `icicle-search-context-level-4',
;;    `icicle-search-context-level-5',
;;    `icicle-search-context-level-6',
;;    `icicle-search-context-level-7',
;;    `icicle-search-context-level-8', `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others', `icicle-special-candidate',
;;    `icicle-whitespace-highlight', `minibuffer-prompt'.
;;
;;  Functions defined here:
;;
;;    `icicle-increment-color-hue',
;;    `icicle-increment-color-saturation'.
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
;;  (@> "Groups, organized alphabetically")
;;  (@> "Faces, organized alphabetically")
 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Groups, organized alphabetically")

;;; Groups, organized alphabetically ---------------------------------

;;;###autoload
(defgroup Icicles nil
  "Minibuffer input completion and cycling of completion candidates."
  :prefix "icicle-"
  :group 'completion :group 'convenience :group 'help :group 'apropos
  :group 'dabbrev :group 'matching :group 'minibuffer :group 'recentf
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Buffers nil
  "Icicles preferences related to buffers."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Files nil
  "Icicles preferences related to files."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Completions-Display nil
  "Icicles preferences related to display of completion candidates."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Key-Bindings nil
  "Icicles preferences related to key bindings."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Key-Completion nil
  "Icicles preferences related to key completion (`icicle-complete-keys')."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Matching nil
  "Icicles preferences related to matching input for completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Minibuffer-Display nil
  "Icicles preferences related to minibuffer display during completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Miscellaneous nil
  "Miscellaneous Icicles preferences."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

;;;###autoload
(defgroup Icicles-Searching nil
  "Icicles preferences related to searching."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )
 
;;(@* "Faces, organized alphabetically")

;;; Faces, organized alphabetically ----------------------------------

;;;###autoload
(defface icicle-candidate-part
    '((((background dark)) (:background "#451700143197")) ; a very dark magenta
      (t (:background "#EF84FFEAF427"))) ; A light green.
  "*Face used to highlight part(s) of a candidate in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-common-match-highlight-Completions
    '((((background dark)) (:foreground "#2017A71F2017")) ; a dark green
      (t (:foreground "magenta3")))
  "*Face used to highlight candidates common match, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-complete-input
  '((((background dark)) (:foreground "#B19E6A64B19E")) ; a dark magenta
    (t (:foreground "DarkGreen")))
  "*Face used to highlight input when it is complete."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;;;###autoload
(defface icicle-completion
    '((((background dark)) (:foreground "#0000D53CD53C")) ; a dark cyan
      (t (:foreground "Red")))                            ; red
  "*Face used to indicate minibuffer completion.
It highlights the minibuffer indicator and the `Icy' minor-mode
lighter during completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

;;;###autoload
(defface icicle-Completions-instruction-1
  '((((background dark)) (:foreground "#AC4AAC4A0000")) ; a dark yellow
    (t (:foreground "Blue")))
  "*Face used to highlight first line of *Completions* buffer."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-Completions-instruction-2
    '((((background dark)) (:foreground "#0000D53CD53C")) ; a dark cyan
      (t (:foreground "Red")))
  "*Face used to highlight second line of *Completions* buffer."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-current-candidate-highlight
  '((((background dark)) (:background "#69D40A460000")) ; a red brown
    (t (:background "CadetBlue1")))
  "*Face used to highlight the current candidate, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-extra-candidate
    '((((background dark)) (:background "#4517305D0000")) ; a dark brown
      (t (:background "#C847D8FEFFFF"))) ; a light blue
  "*Face used to highlight *Completions* candidates that are extra.
This means that they belong to list `icicle-extra-candidates'."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-historical-candidate
  '((((background dark)) (:foreground "#DBD599DF0000")) ; a dark orange
    (t (:foreground "Blue")))
  "*Face used to highlight *Completions* candidates that have been used."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-input-completion-fail
    '((((background dark)) (:background "#22225F5F2222")) ; a dark green
      (t (:foreground "Black" :background "Plum")))
  "*Face for highlighting failed part of input during strict completion."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;;;###autoload
(defface icicle-input-completion-fail-lax
    '((((background dark)) (:background "#00005E3B5A8D")) ; a dark cyan
      (t (:foreground "Black" :background "#FFFFB8C4BB87")))
  "*Face for highlighting failed part of input during lax completion."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;;;###autoload
(defface icicle-match-highlight-Completions
    '((((background dark)) (:foreground "#1F1FA21CA21C")) ; a very dark cyan
      (t (:foreground "Red3")))
  "*Face used to highlight root that was completed, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-match-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;;;###autoload
(defface icicle-mode-line-help
  '((((background dark)) (:foreground "#AC4AAC4A0000")) ; a dark yellow
    (t (:foreground "Blue")))
  "*Face used to highlight help shown in the mode-line."
  :group 'Icicles-Completions-Display :group 'Icicles-Miscellaneous :group 'faces)

;;;###autoload
(defface icicle-multi-command-completion
    '((((background dark)) ; a dark cyan on a dark magenta
       (:foreground "#0000D53CD53C" :background "#8B3500007533"))
      (t (:foreground "Red" :background "#78F6FFFF8E4F"))) ; red on a light green
  "*Face used to indicate Icicles multi-command completion.
It highlights the minibuffer indicator and the `Icy+' minor-mode
lighter during multi-command completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

;;;###autoload
(defface icicle-mustmatch-completion
    '((((type x w32 mac graphic) (class color))
       (:box (:line-width -2 :color "Blue"))) ; blue box
      (t (:inverse-video t)))
  "*Face used to indicate strict minibuffer completion.
It highlights the minibuffer indicator and the `Icy' or `Icy+'
minor-mode lighter during strict completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

;;;###autoload
(defface icicle-proxy-candidate
    '((((background dark)) (:background "#316B22970000")) ; a very dark brown
      (t (:background "#E1E1EAEAFFFF"   ; A light blue.
          :box (:line-width 2 :color "White" :style released-button))))
  "*Face used to highlight proxy candidates in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-saved-candidate
    '((((background dark)) (:background "gray20"))   ; a dark gray
      (t (:background "gray80"))) ; a light gray
  "*Face used to highlight *Completions* candidates that have been saved."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-search-main-regexp-current
  '((((background dark)) (:background "#00004AA652F1")) ; a dark cyan
    (t (:background "misty rose")))
  "*Face used to highlight current match of your search context regexp.
This highlighting is done during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

;; This is essentially a version of `doremi-increment-color-component' for hue only.
;; Must be before `icicle-search-context-level-1'.
(defun icicle-increment-color-hue (color increment)
  "Increase hue component of COLOR by INCREMENT."
  (unless (featurep 'hexrgb) (error "`icicle-increment-color-hue' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color  (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb         (x-color-values color))
         (red         (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green       (/ (float (nth 1 rgb)) 65535.0))
         (blue        (/ (float (nth 2 rgb)) 65535.0))
         (hsv         (hexrgb-rgb-to-hsv red green blue))
         (hue         (nth 0 hsv))
         (saturation  (nth 1 hsv))
         (value       (nth 2 hsv)))
    (setq hue  (+ hue (/ increment 100.0)))
    (when (> hue 1.0) (setq hue  (1- hue)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;; This is essentially a version of `doremi-increment-color-component' for saturation only.
;; Must be before `icicle-search-context-level-1'.
(defun icicle-increment-color-saturation (color increment)
  "Increase saturation component of COLOR by INCREMENT."
  (unless (featurep 'hexrgb)
    (error "`icicle-increment-color-saturation' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color  (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb         (x-color-values color))
         (red         (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green       (/ (float (nth 1 rgb)) 65535.0))
         (blue        (/ (float (nth 2 rgb)) 65535.0))
         (hsv         (hexrgb-rgb-to-hsv red green blue))
         (hue         (nth 0 hsv))
         (saturation  (nth 1 hsv))
         (value       (nth 2 hsv)))
    (setq saturation  (+ saturation (/ increment 100.0)))
    (when (> saturation 1.0) (setq saturation  (1- saturation)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;;;###autoload
(defface icicle-search-context-level-1
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-saturation
                            (icicle-increment-color-hue context-bg 80) 10)
                           "#071F473A0000"))) ; a dark green
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-saturation
                              (icicle-increment-color-hue context-bg 80) 10)
                             "#FA6CC847FFFF"))))) ; a light magenta
  "*Face used to highlight level (subgroup match) 1 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-2
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-saturation
                            (icicle-increment-color-hue context-bg 40) 10)
                           "#507400002839"))) ; a dark red
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-saturation
                              (icicle-increment-color-hue context-bg 40) 10)
                             "#C847FFFFE423"))))) ; a light cyan
  "*Face used to highlight level (subgroup match) 2 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-3
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-saturation
                            (icicle-increment-color-hue context-bg 60) 10)
                           "#4517305D0000"))) ; a dark brown
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-saturation
                              (icicle-increment-color-hue context-bg 60) 10)
                             "#C847D8FEFFFF"))))) ; a light blue
  "*Face used to highlight level (subgroup match) 3 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-4
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-saturation
                            (icicle-increment-color-hue context-bg 20) 10)
                           "#176900004E0A"))) ; a dark blue
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-saturation
                              (icicle-increment-color-hue context-bg 20) 10)
                             "#EF47FFFFC847"))))) ; a light yellow
  "*Face used to highlight level (subgroup match) 4 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-5
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-hue context-bg 80)
                           "#04602BC00000"))) ; a very dark green
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-hue context-bg 80)
                             "#FCFCE1E1FFFF"))))) ; a light magenta
  "*Face used to highlight level (subgroup match) 5 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-6
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-hue context-bg 40)
                           "#32F200001979"))) ; a very dark red
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-hue context-bg 40)
                             "#E1E1FFFFF0F0"))))) ; a light cyan
  "*Face used to highlight level (subgroup match) 6 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-7
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-hue context-bg 60)
                           "#316B22970000"))) ; a very dark brown
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-hue context-bg 60)
                             "#E1E1EAEAFFFF"))))) ; a light blue
  "*Face used to highlight level (subgroup match) 7 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-context-level-8
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (featurep 'hexrgb)
                           (icicle-increment-color-hue context-bg 20)
                           "#12EC00003F0E"))) ; a very dark blue
        (t (:background ,(if (featurep 'hexrgb)
                             (icicle-increment-color-hue context-bg 20)
                             "#F6F5FFFFE1E1"))))) ; a light yellow
  "*Face used to highlight level (subgroup match) 8 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-current-input
    '((((background dark))
       (:foreground "White" :background "#7F0D00007F0D")) ; a dark magenta
      (t (:foreground "Black" :background "Green")))
  "*Face used to highlight what your current input matches.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-search-main-regexp-others
  '((((background dark)) (:background "#348608690000")) ; a very dark brown
    (t (:background "CadetBlue1")))
  "*Face used to highlight other matches of your search context regexp.
If user option `icicle-search-highlight-threshold' is less than one,
then this face is not used.
This highlighting is done during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

;;;###autoload
(defface icicle-special-candidate
    '((((background dark)) (:background "#176900004E0A")) ; a dark blue
      (t (:background "#EF47FFFFC847")))   ; A light yellow.
  "*Face used to highlight *Completions* candidates that are special.
The meaning of special is that their names match
`icicle-special-candidate-regexp'."
  :group 'Icicles-Completions-Display :group 'faces)

;;;###autoload
(defface icicle-whitespace-highlight
    '((((background dark)) (:background "#000093F402A2")) ; a medium green
      (t (:background "Magenta")))
  "*Face used to highlight initial whitespace in minibuffer input."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This is for Emacs < 22.  This is used
;; only for versions of Emacs that have `propertize' but don't have this face.
;;;###autoload
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-face.el ends here

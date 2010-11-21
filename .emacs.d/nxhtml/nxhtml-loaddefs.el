;; Autoloads for nXthml
;;
;; This file should be updated by `nxhtmlmaint-get-file-autoloads',
;; `nxhtmlmaint-get-dir-autoloads' or `nxhtmlmaint-get-all-autoloads'.
(eval-when-compile (require 'nxhtml-base))
(eval-when-compile (require 'web-vcs))

;;;### (autoloads (cancel-secondary-selection set-secondary-selection
;;;;;;  anchored-transpose) "anchored-transpose" "util/anchored-transpose.el"
;;;;;;  (19333 54924))
;;; Generated autoloads from util/anchored-transpose.el
(web-autoload-require 'anchored-transpose 'lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'anchored-transpose `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Transpose portions of the region around an anchor phrase.

`this phrase but not that word'    can be transposed into
`that word but not this phrase'

I want this phrase but not that word.
       |----------------------------|. .This is the entire phrase.
                  |-------|. . . . . . .This is the anchor phrase.

First select the entire phrase and type \\[anchored-transpose].
This set the secondary selection.

Then select the anchor phrase and type \\[anchored-transpose]
again.  Alternatively you can do the selections like this:

I want this phrase but not that word.
       |----------|       |---------|   Separate phrase selection.

By default the anchor phrase will automatically include
any surrounding whitespace even if you don't explicitly select
it.  Also, it won't include certain trailing punctuation.  See
`anchored-transpose-do-fuzzy' for details.  A prefix arg prior to
either selection means `no fuzzy logic, use selections
literally'.

You can select the regions to be swapped separately in any
order.

After swapping both primary and secondary selection are still
active.  They will be canceled after second next command if you
do not swap regions again.  (Second because this allow you to
adjust the regions and try again.)

You can also swap text between different buffers this way.

Typing \\[anchored-transpose] with nothing selected clears any
prior selection, ie secondary selection.

\(fn BEG1 END1 FLG1 &optional BEG2 END2 FLG2 WIN2)" t nil)

(nxhtml-autoload 'set-secondary-selection `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Set the secondary selection to the current region.
This must be bound to a mouse drag event.

\(fn BEG END)" t nil)

(nxhtml-autoload 'cancel-secondary-selection `(lp '(nxhtml-download-root-url nil) "util/anchored-transpose" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (appmenu-mode appmenu-add appmenu) "appmenu" "util/appmenu.el"
;;;;;;  (19275 63380))
;;; Generated autoloads from util/appmenu.el
(web-autoload-require 'appmenu 'lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'appmenu 'custom-loads))) (if (member '"appmenu" loads) nil (put 'appmenu 'custom-loads (cons '"appmenu" loads))))

(nxhtml-autoload 'appmenu-add `(lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir) "\
Add entry to `appmenu-alist'.
Add an entry to this list with ID, PRIORITY, TEST, TITLE and
DEFINITION as explained there.

\(fn ID PRIORITY TEST TITLE DEFINITION)" nil nil)

(defvar appmenu-mode nil "\
Non-nil if Appmenu mode is enabled.
See the command `appmenu-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `appmenu-mode'.")

(nxhtml-custom-autoload 'appmenu-mode 'appmenu nil)

(nxhtml-autoload 'appmenu-mode `(lp '(nxhtml-download-root-url nil) "util/appmenu" nxhtml-install-dir) "\
Use a context sensitive popup menu.
AppMenu (appmenu.el) is a framework for creating cooperative
context sensitive popup menus with commands from different major
and minor modes. Using this different modes may cooperate about
the use of popup menus.

There is also the command `appmenu-as-help' that shows the key
bindings at current point in the help buffer.

The popup menu and the help buffer version are on these keys:

\\{appmenu-mode-map}

The variable `appmenu-alist' is where the popup menu entries
comes from.

If there is a `keymap' property at point then relevant bindings
from this is also shown in the popup menu.

You can write functions that use whatever information you want in
Emacs to construct these entries. Since this information is only
collected when the popup menu is shown you do not have to care as
much about computation time as for entries in the menu bar.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (as-external-mode as-external-for-wiki as-external-for-mail-mode
;;;;;;  as-external-for-xhtml as-external) "as-external" "util/as-external.el"
;;;;;;  (19292 49706))
;;; Generated autoloads from util/as-external.el
(web-autoload-require 'as-external 'lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'as-external 'custom-loads))) (if (member '"as-external" loads) nil (put 'as-external 'custom-loads (cons '"as-external" loads))))

(nxhtml-autoload 'as-external-for-xhtml `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit XHTML.
It's All Text is a Firefox add-on for editing textareas with an
external editor.
See URL `https://addons.mozilla.org/en-US/firefox/addon/4125'.

In this case Emacs is used to edit textarea fields on a web page.
The text will most often be part of a web page later, like on a
blog.  Therefore turn on these:

- `nxhtml-mode' since some XHTML tags may be allowed.
- `nxhtml-validation-header-mode' since it is not a full page.
- `wrap-to-fill-column-mode' to see what you are writing.
- `html-write-mode' to see it even better.

Also bypass the question for line end conversion when using
emacsw32-eol.

\(fn)" t nil)

(nxhtml-autoload 'as-external-for-mail-mode `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit mail.
Set normal mail comment markers in column 1 (ie >).

Set `fill-column' to 90 and enable `wrap-to-fill-column-mode' so
that it will look similar to how it will look in the sent plain
text mail.

See also `as-external-mode'.

\(fn)" t nil)

(nxhtml-autoload 'as-external-for-wiki `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
Setup for Firefox addon It's All Text to edit MediaWikis.

\(fn)" t nil)

(defvar as-external-mode nil "\
Non-nil if As-External mode is enabled.
See the command `as-external-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `as-external-mode'.")

(nxhtml-custom-autoload 'as-external-mode 'as-external nil)

(nxhtml-autoload 'as-external-mode `(lp '(nxhtml-download-root-url nil) "util/as-external" nxhtml-install-dir) "\
If non-nil check if Emacs is called as external editor.
When Emacs is called as an external editor for example to edit
text areas on a web page viewed with Firefox this library tries
to help to setup the buffer in a useful way. It may for example
set major and minor modes for the buffer.

This can for example be useful when blogging or writing comments
on blogs.

See `as-external-alist' for more information.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (buffer-bg-set-color) "buffer-bg" "util/buffer-bg.el"
;;;;;;  (19254 64104))
;;; Generated autoloads from util/buffer-bg.el
(web-autoload-require 'buffer-bg 'lp '(nxhtml-download-root-url nil) "util/buffer-bg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'buffer-bg-set-color `(lp '(nxhtml-download-root-url nil) "util/buffer-bg" nxhtml-install-dir) "\
Add an overlay with background color COLOR to buffer BUFFER.
If COLOR is nil remove previously added overlay.

\(fn COLOR BUFFER)" t nil)

;;;***

;;;### (autoloads (chartg-make-chart chartg-complete) "chartg" "util/chartg.el"
;;;;;;  (19278 15746))
;;; Generated autoloads from util/chartg.el
(web-autoload-require 'chartg 'lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'chartg-complete `(lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'chartg-make-chart `(lp '(nxhtml-download-root-url nil) "util/chartg" nxhtml-install-dir) "\
Try to make a new chart.
If region is active then make a new chart from data in the
selected region.

Else if current buffer is in `chartg-mode' then do it from the
chart specifications in this buffer.  Otherwise create a new
buffer and initialize it with `chartg-mode'.

If the chart specifications are complete enough to make a chart
then do it and show the resulting chart image.  If not then tell
user what is missing.

NOTE: This is beta, no alpha code. It is not ready.

Below are some examples.  To test them mark an example and do

  M-x chartg-make-chart

* Example, simple x-y chart:

  Output-file: \"~/temp-chart.png\"
  Size: 200 200
  Data: 3 8 5 | 10 20 30
  Type: line-chartg-xy

* Example, pie:

  Output-file: \"~/temp-depression.png\"
  Size: 400 200
  Data:
  2,160,000
  3,110,000
  1,510,000
  73,600
  775,000
  726,000
  8,180,000
  419,000
  Type: pie-3-dimensional
  Chartg-title: \"Depression hits on Google\"
  Legends:
  \"SSRI\"
  | \"Psychotherapy\"
  | \"CBT\"
  | \"IPT\"
  | \"Psychoanalysis\"
  | \"Mindfulness\"
  | \"Meditation\"
  | \"Exercise\"


* Example, pie:

  Output-file: \"~/temp-panic.png\"
  Size: 400 200
  Data:
  979,000
  969,000
  500,000
  71,900
  193,000
  154,000
  2,500,000
  9,310,000
  Type: pie-3-dimensional
  Chartg-title: \"Depression hits on Google\"
  Legends:
  \"SSRI\"
  | \"Psychotherapy\"
  | \"CBT\"
  | \"IPT\"
  | \"Psychoanalysis\"
  | \"Mindfulness\"
  | \"Meditation\"
  | \"Exercise\"


* Example using raw:

  Output-file: \"~/temp-chartg-slipsen-kostar.png\"
  Size: 400 130
  Data: 300 1000 30000
  Type: bar-chartg-horizontal
  Chartg-title: \"Vad killen i slips tjänar jämfört med dig och mig\"
  Google-chartg-raw: \"&chds=0,30000&chco=00cd00|ff4500|483d8b&chxt=y,x&chxl=0:|Killen+i+slips|Partiledarna|Du+och+jag&chf=bg,s,ffd700\"


\(fn)" t nil)

;;;***

;;;### (autoloads (css-color-test css-color-global-mode css-color-mode
;;;;;;  css-color) "css-color" "util/css-color.el" (19386 34254))
;;; Generated autoloads from util/css-color.el
(web-autoload-require 'css-color 'lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'css-color 'custom-loads))) (if (member '"css-color" loads) nil (put 'css-color 'custom-loads (cons '"css-color" loads))))

(nxhtml-autoload 'css-color-mode `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Show hex color literals with the given color as background.
In this mode hexadecimal colour specifications like #6600ff are
displayed with the specified colour as background.

Certain keys are bound to special colour editing commands when
point is at a hexadecimal colour:

\\{css-color-map}

\(fn &optional ARG)" t nil)

(defvar css-color-global-mode nil "\
Non-nil if Css-Color-Global mode is enabled.
See the command `css-color-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `css-color-global-mode'.")

(nxhtml-custom-autoload 'css-color-global-mode 'css-color nil)

(nxhtml-autoload 'css-color-global-mode `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Toggle Css-Color mode in every possible buffer.
With prefix ARG, turn Css-Color-Global mode on if and only if
ARG is positive.
Css-Color mode is enabled in all buffers where
`css-color-turn-on-in-buffer' would do it.
See `css-color-mode' for more information on Css-Color mode.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'css-color-test `(lp '(nxhtml-download-root-url nil) "util/css-color" nxhtml-install-dir) "\
Test colors interactively.
The colors are displayed in the echo area. You can specify the
colors as any viable css color.  Example:

  red
  #f00
  #0C0
  #b0ff00
  hsla(100, 50%, 25%)
  rgb(255,100,120)

\(fn FG-COLOR BG-COLOR)" t nil)

;;;***

;;;### (autoloads (css-palette-global-mode css-palette css-palette-mode)
;;;;;;  "css-palette" "util/css-palette.el" (19235 1650))
;;; Generated autoloads from util/css-palette.el
(web-autoload-require 'css-palette 'lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'css-palette-mode `(lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir) "\
Minor mode for palettes in CSS.

The mode `css-palette-mode' acts on the first COLORS declaration in your
  file of the form:

COLORS:
\(
c0 \"#6f5d25\"	;tainted sand
c1 \"#000000\"	;Black
c2 \"#cca42b\"	;goldenslumber
c3 \"#6889cb\"	;far off sky
c4 \"#fff\"	;strange aeons
)

Such declarations should appear inside a block comment, in order
  to be parsed properly by the LISP reader.

Type \\[css-palette-update-all], and any occurence of

  color: #f55; /*[c3]*/

will be updated with

  color: #6899cb; /*[c3]*/

The following commands are available to insert key-value pairs
  and palette declarations:
  \\{css-palette-mode-map}

You can extend or redefine the types of palettes by defining a
  new palette specification of the form (PATTERN REGEXP
  REF-FOLLOWS-VALUE), named according to the naming scheme
  css-palette:my-type, where

PATTERN is a pattern containing two (%s) format directives which
  will be filled in with the variable and its value,

REGEXP is a regular expression to match a value - variable
  pattern,

and REF-FOLLOWS-VALUE defined whether or not the reference comes
  after the value. This allows for more flexibility.

Note that, although the w3c spec at URL
  `http://www.w3.org/TR/CSS2/syndata.html#comments' says that
  comments \" may occur anywhere between tokens, and their
  contents have no influence on the rendering\", Internet
  Explorer does not think so. Better keep all your comments after
  a \"statement\", as per the default. This means `css-palette'
  is ill-suited for use within shorthands.

See variable `css-palette:colors' for an example of a palette
  type.

The extension mechanism means that palette types can be used to
  contain arbitrary key-value mappings.

Besides the colors palette, css-palette defines the palette
  definition variables `css-palette:colors-outside' and
  `css-palette:files', for colors with the reference outside and
  for file url()'s respectively.

You can fine-control which palette types css-palette should look
  at via the variable `css-palette-types'.

\(fn &optional ARG)" t nil)

(let ((loads (get 'css-palette 'custom-loads))) (if (member '"css-palette" loads) nil (put 'css-palette 'custom-loads (cons '"css-palette" loads))))

(defvar css-palette-global-mode nil "\
Non-nil if Css-Palette-Global mode is enabled.
See the command `css-palette-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `css-palette-global-mode'.")

(nxhtml-custom-autoload 'css-palette-global-mode 'css-palette nil)

(nxhtml-autoload 'css-palette-global-mode `(lp '(nxhtml-download-root-url nil) "util/css-palette" nxhtml-install-dir) "\
Toggle Css-Palette mode in every possible buffer.
With prefix ARG, turn Css-Palette-Global mode on if and only if
ARG is positive.
Css-Palette mode is enabled in all buffers where
`css-palette-turn-on-in-buffer' would do it.
See `css-palette-mode' for more information on Css-Palette mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cusnu-export-my-skin-options customize-for-new-user)
;;;;;;  "cus-new-user" "util/cus-new-user.el" (19173 56140))
;;; Generated autoloads from util/cus-new-user.el
(web-autoload-require 'cus-new-user 'lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'customize-for-new-user `(lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir) "\
Show special customization page for new user.

\(fn &optional NAME)" t nil)

(nxhtml-autoload 'cusnu-export-my-skin-options `(lp '(nxhtml-download-root-url nil) "util/cus-new-user" nxhtml-install-dir) "\
Export to file FILE custom options in `cusnu-my-skin-options'.
The options is exported to elisp code that other users can run to
set the options that you have added to `cusnu-my-skin-options'.

For more information about this see `cusnu-export-cust-group'.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (ediff-url) "ediff-url" "util/ediff-url.el" (19362
;;;;;;  34258))
;;; Generated autoloads from util/ediff-url.el
(web-autoload-require 'ediff-url 'lp '(nxhtml-download-root-url nil) "util/ediff-url" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ediff-url `(lp '(nxhtml-download-root-url nil) "util/ediff-url" nxhtml-install-dir) "\
Compare current buffer to a web URL using `ediff-buffers'.
Check URL using `ediff-url-redirects' before fetching the file.

This is for checking downloaded file.  A the file may have a comment
telling the download URL of thise form in the header:

   ;; URL: http://the-server.net/the-path/the-file.el

If not the user is asked for the URL.

\(fn URL)" t nil)

;;;***

;;;### (autoloads (ffip-find-file-in-dirtree ffip-set-current-project)
;;;;;;  "ffip" "util/ffip.el" (19257 25432))
;;; Generated autoloads from util/ffip.el
(web-autoload-require 'ffip 'lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ffip-set-current-project `(lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir) "\
Setup ffip project NAME with top directory ROOT of type TYPE.
ROOT can either be just a directory or a list of directory where
the first used just for prompting purposes and the files in the
rest are read into the ffip project.

Type is a type in `ffip-project-file-types'.

\(fn NAME ROOT TYPE)" nil nil)

(nxhtml-autoload 'ffip-find-file-in-dirtree `(lp '(nxhtml-download-root-url nil) "util/ffip" nxhtml-install-dir) "\
Find files in directory tree ROOT.

\(fn ROOT)" t nil)

;;;***

;;;### (autoloads (fold-dwim-turn-on-outline-and-hide-all fold-dwim-turn-on-hs-and-hide
;;;;;;  fold-dwim-unhide-hs-and-outline fold-dwim-mode fold-dwim-toggle
;;;;;;  fold-dwim) "fold-dwim" "util/fold-dwim.el" (19218 42180))
;;; Generated autoloads from util/fold-dwim.el
(web-autoload-require 'fold-dwim 'lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'fold-dwim 'custom-loads))) (if (member '"fold-dwim" loads) nil (put 'fold-dwim 'custom-loads (cons '"fold-dwim" loads))))

(nxhtml-autoload 'fold-dwim-toggle `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Toggle visibility or some other visual things.
Try toggling different visual things in this order:

- Images shown at point with `inlimg-mode'
- Text at point prettified by `html-write-mode'.

For the rest it unhides if possible, otherwise hides in this
order:

- `org-mode' header or something else using that outlines.
- Maybe `fold-dwim-toggle-selective-display'.
- `Tex-fold-mode' things.
- In html if `outline-minor-mode' and after heading hide content.
- `hs-minor-mode' things.
- `outline-minor-mode' things. (Turns maybe on this.)

It uses `fold-dwim-show' to show any hidden text at point; if no
hidden fold is found, try `fold-dwim-hide' to hide the
construction at the cursor.

Note: Also first turn on `fold-dwim-mode' to get the keybinding
for this function from it.

\(fn)" t nil)

(defvar fold-dwim-mode nil "\
Non-nil if Fold-Dwim mode is enabled.
See the command `fold-dwim-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fold-dwim-mode'.")

(nxhtml-custom-autoload 'fold-dwim-mode 'fold-dwim nil)

(nxhtml-autoload 'fold-dwim-mode `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Key binding for `fold-dwim-toggle'.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'fold-dwim-unhide-hs-and-outline `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Unhide everything hidden by Hide/Show and Outline.
Ie everything hidden by `hs-minor-mode' and
`outline-minor-mode'.

\(fn)" t nil)

(nxhtml-autoload 'fold-dwim-turn-on-hs-and-hide `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Turn on minor mode `hs-minor-mode' and hide.
If major mode is derived from `nxml-mode' call `hs-hide-block'
else call `hs-hide-all'.

\(fn)" t nil)

(nxhtml-autoload 'fold-dwim-turn-on-outline-and-hide-all `(lp '(nxhtml-download-root-url nil) "util/fold-dwim" nxhtml-install-dir) "\
Turn on `outline-minor-mode' and call `hide-body'.

\(fn)" t nil)

;;;***

;;;### (autoloads (foldit-global-mode foldit-mode foldit) "foldit"
;;;;;;  "util/foldit.el" (19275 63380))
;;; Generated autoloads from util/foldit.el
(web-autoload-require 'foldit 'lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'foldit 'custom-loads))) (if (member '"foldit" loads) nil (put 'foldit 'custom-loads (cons '"foldit" loads))))

(nxhtml-autoload 'foldit-mode `(lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir) "\
Minor mode providing visual aids for folding.
Shows some hints about what you have hidden and how to reveal it.

Supports `hs-minor-mode', `outline-minor-mode' and major modes
derived from `outline-mode'.

\(fn &optional ARG)" t nil)

(defvar foldit-global-mode nil "\
Non-nil if Foldit-Global mode is enabled.
See the command `foldit-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `foldit-global-mode'.")

(nxhtml-custom-autoload 'foldit-global-mode 'foldit nil)

(nxhtml-autoload 'foldit-global-mode `(lp '(nxhtml-download-root-url nil) "util/foldit" nxhtml-install-dir) "\
Toggle Foldit mode in every possible buffer.
With prefix ARG, turn Foldit-Global mode on if and only if
ARG is positive.
Foldit mode is enabled in all buffers where
`(lambda nil (foldit-mode 1))' would do it.
See `foldit-mode' for more information on Foldit mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (gimpedit-can-edit gimpedit-edit-buffer gimpedit-edit-file
;;;;;;  gimpedit) "gimpedit" "util/gimpedit.el" (19275 63380))
;;; Generated autoloads from util/gimpedit.el
(web-autoload-require 'gimpedit 'lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'gimpedit 'custom-loads))) (if (member '"gimpedit" loads) nil (put 'gimpedit 'custom-loads (cons '"gimpedit" loads))))

(nxhtml-autoload 'gimpedit-edit-file `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Edit IMAGE-FILE with GIMP.
See also `gimpedit-edit-file'.

\(fn IMAGE-FILE &optional EXTRA-ARGS)" t nil)

(nxhtml-autoload 'gimpedit-edit-buffer `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Edit image file in current buffer with GIMP.
See also `gimpedit-edit-file'.

You may also be interested in gimpedit-mode with which you can edit
gimp files from within Emacs using GIMP's scripting
possibilities. See

  URL `http://www.emacswiki.org/emacs/GimpMode'

\(fn)" t nil)

(nxhtml-autoload 'gimpedit-can-edit `(lp '(nxhtml-download-root-url nil) "util/gimpedit" nxhtml-install-dir) "\
Not documented

\(fn FILE-NAME)" nil nil)

;;;***

;;;### (autoloads (gpl-mode) "gpl" "util/gpl.el" (18795 27308))
;;; Generated autoloads from util/gpl.el
(web-autoload-require 'gpl 'lp '(nxhtml-download-root-url nil) "util/gpl" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'gpl-mode `(lp '(nxhtml-download-root-url nil) "util/gpl" nxhtml-install-dir) "\
Mode for font-locking and editing color palettes of the GPL format.

Such palettes are used and produced by free software applications
such as the GIMP, Inkscape, Scribus, Agave and on-line tools such
as http://colourlovers.com.

You can also use
URL `http://niels.kicks-ass.org/public/elisp/css-palette.el' to import
such palette into a css-file as hexadecimal color palette.

\(fn)" t nil)

;;;***

;;;### (autoloads (hfyview-frame hfyview-window hfyview-region hfyview-buffer
;;;;;;  hfyview-quick-print-in-files-menu) "hfyview" "util/hfyview.el"
;;;;;;  (19400 17335))
;;; Generated autoloads from util/hfyview.el
(web-autoload-require 'hfyview 'lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir 'nxhtml-byte-compile-file)


(defvar hfyview-quick-print-in-files-menu nil "\
Add Quick print entries to File menu if non-nil.
If you set this to nil you have to restart Emacs to get rid of
the Quick Print entry.")

(nxhtml-custom-autoload 'hfyview-quick-print-in-files-menu 'hfyview nil)

(nxhtml-autoload 'hfyview-buffer `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert buffer to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window.

\(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-region `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert region to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window.

\(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-window `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert window to html preserving faces and show in web browser.
With command prefix ARG also show html source in other window.

\(fn ARG)" t nil)

(nxhtml-autoload 'hfyview-frame `(lp '(nxhtml-download-root-url nil) "util/hfyview" nxhtml-install-dir) "\
Convert frame to html preserving faces and show in web browser.
Make an XHTML view of the current Emacs frame. Put it in a buffer
named *hfyview-frame* and show that buffer in a web browser.

If WHOLE-BUFFERS is non-nil then the whole content of the buffers
is shown in the XHTML page, otherwise just the part that is
visible currently on the frame.

If you turn on the minor mode `hfyview-frame-mode' you can also
get the minibuffer/echo area in the output. See this mode for
details.

With command prefix also show html source in other window.

\(fn WHOLE-BUFFERS)" t nil)

;;;***

;;;### (autoloads (hl-needed-mode hl-needed) "hl-needed" "util/hl-needed.el"
;;;;;;  (19394 16942))
;;; Generated autoloads from util/hl-needed.el
(web-autoload-require 'hl-needed 'lp '(nxhtml-download-root-url nil) "util/hl-needed" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'hl-needed 'custom-loads))) (if (member '"hl-needed" loads) nil (put 'hl-needed 'custom-loads (cons '"hl-needed" loads))))

(defvar hl-needed-mode nil "\
Non-nil if Hl-Needed mode is enabled.
See the command `hl-needed-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hl-needed-mode'.")

(nxhtml-custom-autoload 'hl-needed-mode 'hl-needed nil)

(nxhtml-autoload 'hl-needed-mode `(lp '(nxhtml-download-root-url nil) "util/hl-needed" nxhtml-install-dir) "\
Try to highlight current line and column when needed.
This is a global minor mode.  It can operate in some different
ways:

- Highlighting can be on always, see `hl-needed-always'.

Or, it can be turned on depending on some conditions.  In this
case highlighting is turned off after each command and turned on
again in the current window when either:

- A new window was selected, see `hl-needed-on-new-window'.
- A new buffer was selected, see `hl-needed-on-new-buffer'.
- Window configuration was changed, see `hl-needed-on-config-change'.
- Buffer was scrolled see `hl-needed-on-scrolling'.
- A window was clicked with the mouse, see `hl-needed-on-mouse'.

After this highlighting may be turned off again, normally after a
short delay, see `hl-needed-flash'.

If either highlighting was not turned on or was turned off again
it will be turned on when

- Emacs has been idle for `hl-needed-idle-time' seconds.

See also `hl-needed-not-in-modes' and `hl-needed-currently-fun'.

Note 1: For columns to be highlighted vline.el must be available.

Note 2: This mode depends on `hl-line-mode' and `vline-mode' and
tries to cooperate with them. If you turn on either of these that
overrides the variables for turning on the respective
highlighting here.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (html-write-mode html-write) "html-write" "util/html-write.el"
;;;;;;  (19275 63380))
;;; Generated autoloads from util/html-write.el
(web-autoload-require 'html-write 'lp '(nxhtml-download-root-url nil) "util/html-write" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-write 'custom-loads))) (if (member '"html-write" loads) nil (put 'html-write 'custom-loads (cons '"html-write" loads))))

(nxhtml-autoload 'html-write-mode `(lp '(nxhtml-download-root-url nil) "util/html-write" nxhtml-install-dir) "\
Minor mode for convenient display of some HTML tags.
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
a web file.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (inlimg-toggle-slicing inlimg-toggle-display inlimg-global-mode
;;;;;;  inlimg-mode inlimg) "inlimg" "util/inlimg.el" (19269 33008))
;;; Generated autoloads from util/inlimg.el
(web-autoload-require 'inlimg 'lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'inlimg 'custom-loads))) (if (member '"inlimg" loads) nil (put 'inlimg 'custom-loads (cons '"inlimg" loads))))

(nxhtml-autoload 'inlimg-mode `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Display images inline.
Search buffer for image tags.  Display found images.

Image tags are setup per major mode in `inlimg-mode-specs'.

Images are displayed on a line below the tag referencing them.
The whole image or a slice of it may be displayed, see
`inlimg-slice'.  Margins relative text are specified in
`inlimg-margins'.

See also the commands `inlimg-toggle-display' and
`inlimg-toggle-slicing'.

Note: This minor mode uses `font-lock-mode'.

\(fn &optional ARG)" t nil)

(defvar inlimg-global-mode nil "\
Non-nil if Inlimg-Global mode is enabled.
See the command `inlimg-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `inlimg-global-mode'.")

(nxhtml-custom-autoload 'inlimg-global-mode 'inlimg nil)

(nxhtml-autoload 'inlimg-global-mode `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle Inlimg mode in every possible buffer.
With prefix ARG, turn Inlimg-Global mode on if and only if
ARG is positive.
Inlimg mode is enabled in all buffers where
`inlimg--global-turn-on' would do it.
See `inlimg-mode' for more information on Inlimg mode.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'inlimg-toggle-display `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle display of image at point POINT.
See also the command `inlimg-mode'.

\(fn POINT)" t nil)

(nxhtml-autoload 'inlimg-toggle-slicing `(lp '(nxhtml-download-root-url nil) "util/inlimg" nxhtml-install-dir) "\
Toggle slicing of image at point POINT.
See also the command `inlimg-mode'.

\(fn POINT)" t nil)

;;;***

;;;### (autoloads (majmodpri majmodpri-apply-priorities majmodpri-apply
;;;;;;  majmodpri-sort-lists) "majmodpri" "util/majmodpri.el" (19407
;;;;;;  18407))
;;; Generated autoloads from util/majmodpri.el
(web-autoload-require 'majmodpri 'lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'majmodpri-sort-lists `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Sort the list used when selecting major mode.
Only sort those lists choosen in `majmodpri-lists-to-sort'.
Sort according to priorities in `majmodpri-mode-priorities'.
Keep the old order in the list otherwise.

The lists can be sorted when loading elisp libraries, see
`majmodpri-sort-after-load'.

See also `majmodpri-apply-priorities'.

\(fn)" t nil)

(nxhtml-autoload 'majmodpri-apply `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Sort major mode lists and apply to existing buffers.
Note: This function is suitable to add to
`desktop-after-read-hook'. It will restore the multi major modes
in buffers.

\(fn)" nil nil)

(nxhtml-autoload 'majmodpri-apply-priorities `(lp '(nxhtml-download-root-url nil) "util/majmodpri" nxhtml-install-dir) "\
Apply major mode priorities.
First run `majmodpri-sort-lists' and then if CHANGE-MODES is
non-nil apply to existing file buffers.  If interactive ask
before applying.

\(fn CHANGE-MODES)" t nil)

(let ((loads (get 'majmodpri 'custom-loads))) (if (member '"majmodpri" loads) nil (put 'majmodpri 'custom-loads (cons '"majmodpri" loads))))

;;;***

;;;### (autoloads (markchars-global-mode markchars-mode markchars)
;;;;;;  "markchars" "util/markchars.el" (19372 5886))
;;; Generated autoloads from util/markchars.el
(web-autoload-require 'markchars 'lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'markchars 'custom-loads))) (if (member '"markchars" loads) nil (put 'markchars 'custom-loads (cons '"markchars" loads))))

(nxhtml-autoload 'markchars-mode `(lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir) "\
Mark special characters.
Which characters to mark are defined by `markchars-keywords'.

The default is to mark non-IDN, non-ascii chars with a magenta
underline.

For information about IDN chars see `idn-is-recommended'.

If you change anything in the customization group `markchars' you
must restart this minor mode for the changes to take effect.

\(fn &optional ARG)" t nil)

(defvar markchars-global-mode nil "\
Non-nil if Markchars-Global mode is enabled.
See the command `markchars-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `markchars-global-mode'.")

(nxhtml-custom-autoload 'markchars-global-mode 'markchars nil)

(nxhtml-autoload 'markchars-global-mode `(lp '(nxhtml-download-root-url nil) "util/markchars" nxhtml-install-dir) "\
Toggle Markchars mode in every possible buffer.
With prefix ARG, turn Markchars-Global mode on if and only if
ARG is positive.
Markchars mode is enabled in all buffers where
`(lambda nil (markchars-mode 1))' would do it.
See `markchars-mode' for more information on Markchars mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mlinks-global-mode mlinks-mode mlinks) "mlinks"
;;;;;;  "util/mlinks.el" (19364 56214))
;;; Generated autoloads from util/mlinks.el
(web-autoload-require 'mlinks 'lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'mlinks 'custom-loads))) (if (member '"mlinks" loads) nil (put 'mlinks 'custom-loads (cons '"mlinks" loads))))

(nxhtml-autoload 'mlinks-mode `(lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir) "\
Recognizes certain parts of a buffer as hyperlinks.
The hyperlinks are created in different ways for different major
modes with the help of the functions in the list
`mlinks-mode-functions'.

The hyperlinks can be hilighted when point is over them.  Use
`mlinks-toggle-hilight' to toggle this feature for the current
buffer.

All keybindings in this mode are by default done under the prefi§x
key

  C-c RET

which is supposed to be a kind of mnemonic for link (alluding to
the RET key commonly used in web browser to follow a link).
\(Unfortunately this breaks the rules in info node `Key Binding
Conventions'.) Below are the key bindings defined by this mode:

\\{mlinks-mode-map}

For some major modes `mlinks-backward-link' and
`mlinks-forward-link' will take you to the previous/next link.
By default the link moved to will be active, see
`mlinks-active-links'.

\(fn &optional ARG)" t nil)

(defvar mlinks-global-mode nil "\
Non-nil if Mlinks-Global mode is enabled.
See the command `mlinks-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mlinks-global-mode'.")

(nxhtml-custom-autoload 'mlinks-global-mode 'mlinks nil)

(nxhtml-autoload 'mlinks-global-mode `(lp '(nxhtml-download-root-url nil) "util/mlinks" nxhtml-install-dir) "\
Toggle Mlinks mode in every possible buffer.
With prefix ARG, turn Mlinks-Global mode on if and only if
ARG is positive.
Mlinks mode is enabled in all buffers where
`mlinks-turn-on-in-buffer' would do it.
See `mlinks-mode' for more information on Mlinks mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mumamo-multi-major-modep mumamo-list-defined-multi-major-modes
;;;;;;  mumamo-mark-for-refontification mumamo-hi-lock-faces mumamo
;;;;;;  mumamo-add-to-defined-multi-major-modes define-mumamo-multi-major-mode)
;;;;;;  "mumamo" "util/mumamo.el" (19412 26290))
;;; Generated autoloads from util/mumamo.el
(web-autoload-require 'mumamo 'lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'define-mumamo-multi-major-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Define a function that turn on support for multiple major modes.
Define a function FUN-SYM that set up to divide the current
buffer into chunks with different major modes.

The documentation string for FUN-SYM should contain the special
documentation in the string SPEC-DOC, general documentation for
functions of this type and information about chunks.

The new function will use the definitions in CHUNKS (which is
called a \"chunk family\") to make the dividing of the buffer.

The function FUN-SYM can be used to setup a buffer instead of a
major mode function:

- The function FUN-SYM can be called instead of calling a major
  mode function when you want to use multiple major modes in a
  buffer.

- The defined function can be used instead of a major mode
  function in for example `auto-mode-alist'.

- As the very last thing FUN-SYM will run the hook FUN-SYM-hook,
  just as major modes do.

- There is also a general hook, `mumamo-turn-on-hook', which is
  run when turning on mumamo with any of these functions.  This
  is run right before the hook specific to any of the functions
  above that turns on the multiple major mode support.

- The multi major mode FUN-SYM has a keymap named FUN-SYM-map.
  This overrides the major modes' keymaps since it is handled as
  a minor mode keymap.

- There is also a special mumamo keymap, `mumamo-map' that is
  active in every buffer with a multi major mode.  This is also
  handled as a minor mode keymap and therefor overrides the major
  modes' keymaps.

- However when this support for multiple major mode is on the
  buffer is divided into chunks, each with its own major mode.

- The chunks are fontified according the major mode assigned to
  them for that.

- Indenting is also done according to the major mode assigned to
  them for that.

- The actual major mode used in the buffer is changed to the one
  in the chunk when moving point between these chunks.

- When major mode is changed the hooks for the new major mode,
  `after-change-major-mode-hook' and `change-major-mode-hook' are
  run.

- There will be an alias for FUN-SYM called mumamo-alias-FUN-SYM.
  This can be used to check whic multi major modes have been
  defined.

** A little bit more technical description:

The dividing of a buffer into chunks is done during fontification
by `mumamo-get-chunk-at'.

The name of the function is saved in in the buffer local variable
`mumamo-multi-major-mode' when the function is called.

All functions defined by this macro is added to the list
`mumamo-defined-multi-major-modes'.

Basically Mumamo handles only major modes that uses jit-lock.
However as a special effort also `nxml-mode' and derivatives
thereof are handled.  Since it seems impossible to me to restrict
those major modes fontification to only a chunk without changing
`nxml-mode' the fontification is instead done by
`html-mode'/`sgml-mode' for chunks using `nxml-mode' and its
derivates.

CHUNKS is a list where each entry have the format

  (CHUNK-DEF-NAME MAIN-MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)

CHUNK-DEF-NAME is the key name by which the entry is recognized.
MAIN-MAJOR-MODE is the major mode used when there is no chunks.
If this is nil then `major-mode' before turning on this mode will
be used.

SUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the
chunk division of the buffer.  They are tried in the order they
appear here during the chunk division process.

If you want to write new functions for chunk divisions then
please see `mumamo-find-possible-chunk'.  You can perhaps also
use `mumamo-quick-static-chunk' which is more easy-to-use
alternative.  See also the file mumamo-fun.el where there are
many routines for chunk division.

When you write those new functions you may want to use some of
the functions for testing chunks:

 `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'
 `mumamo-test-easy-make'        `mumamo-test-fontify-region'

These are in the file mumamo-test.el.

\(fn FUN-SYM SPEC-DOC CHUNKS)" nil (quote macro))

(nxhtml-autoload 'mumamo-add-to-defined-multi-major-modes `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Not documented

\(fn ENTRY)" nil nil)

(let ((loads (get 'mumamo 'custom-loads))) (if (member '"mumamo" loads) nil (put 'mumamo 'custom-loads (cons '"mumamo" loads))))

(let ((loads (get 'mumamo-hi-lock-faces 'custom-loads))) (if (member '"mumamo" loads) nil (put 'mumamo-hi-lock-faces 'custom-loads (cons '"mumamo" loads))))

(nxhtml-autoload 'mumamo-mark-for-refontification `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Mark region between MIN and MAX for refontification.

\(fn MIN MAX)" nil nil)

(nxhtml-autoload 'mumamo-list-defined-multi-major-modes `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
List currently defined multi major modes.
If SHOW-DOC is non-nil show the doc strings added when defining
them. (This is not the full doc string. To show the full doc
string you can click on the multi major mode in the list.)

If SHOW-CHUNKS is non-nil show the names of the chunk dividing
functions each multi major mode uses.

If MATCH then show only multi major modes whos names matches.

\(fn SHOW-DOC SHOW-CHUNKS MATCH)" t nil)

(nxhtml-autoload 'mumamo-multi-major-modep `(lp '(nxhtml-download-root-url nil) "util/mumamo" nxhtml-install-dir) "\
Return t if VALUE is a multi major mode function.

\(fn VALUE)" nil nil)

;;;***

;;;### (autoloads (python-rst-mumamo-mode latex-haskell-mumamo-mode
;;;;;;  latex-clojure-mumamo-mode markdown-html-mumamo-mode xsl-sgml-mumamo-mode
;;;;;;  xsl-nxml-mumamo-mode mako-html-mumamo-mode org-mumamo-mode
;;;;;;  asp-html-mumamo-mode noweb2-mumamo-mode mumamo-noweb2 csound-sgml-mumamo-mode
;;;;;;  laszlo-nxml-mumamo-mode metapost-mumamo-mode ruby-heredoc-mumamo-mode
;;;;;;  python-heredoc-mumamo-mode cperl-heredoc-mumamo-mode perl-heredoc-mumamo-mode
;;;;;;  php-heredoc-mumamo-mode sh-heredoc-mumamo-mode eruby-javascript-mumamo-mode
;;;;;;  eruby-html-mumamo-mode eruby-mumamo-mode jsp-html-mumamo-mode
;;;;;;  gsp-html-mumamo-mode ssjs-html-mumamo-mode smarty-html-mumamo-mode
;;;;;;  mjt-html-mumamo-mode genshi-html-mumamo-mode django-html-mumamo-mode
;;;;;;  embperl-html-mumamo-mode mason-html-mumamo-mode nxml-mumamo-mode
;;;;;;  html-mumamo-mode mumamo-define-html-file-wide-keys) "mumamo-fun"
;;;;;;  "util/mumamo-fun.el" (19410 22971))
;;; Generated autoloads from util/mumamo-fun.el
(web-autoload-require 'mumamo-fun 'lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mumamo-define-html-file-wide-keys `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Define keys in multi major mode keymap for html files.

\(fn)" nil nil)

(nxhtml-autoload 'html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP." t)

(nxhtml-autoload 'nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'." t)

(nxhtml-autoload 'mason-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Mason using main mode `html-mode'.
This covers inlined style and javascript." t)

(nxhtml-autoload 'embperl-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'django-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'genshi-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Genshi with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'mjt-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for MJT with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'smarty-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'ssjs-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for SSJS with main mode `html-mode'.
This covers inlined style and javascript." t)

(nxhtml-autoload 'gsp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for GSP with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'jsp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major mode for eRuby with unspecified main mode.
Current major-mode will be used as the main major mode." t)

(nxhtml-autoload 'eruby-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-javascript-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `javascript-mode'." t)

(nxhtml-autoload 'sh-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for sh heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'php-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for PHP heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'perl-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'cperl-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'python-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'ruby-heredoc-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Ruby heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes." t)

(nxhtml-autoload 'metapost-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for MetaPost." t)

(nxhtml-autoload 'laszlo-nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for OpenLaszlo." t)

(nxhtml-autoload 'csound-sgml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on mutiple major modes for CSound orc/sco Modes." t)

(let ((loads (get 'mumamo-noweb2 'custom-loads))) (if (member '"mumamo-fun" loads) nil (put 'mumamo-noweb2 'custom-loads (cons '"mumamo-fun" loads))))

(nxhtml-autoload 'noweb2-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Multi major mode for noweb files." t)

(nxhtml-autoload 'asp-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'org-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for `org-mode' files with main mode `org-mode'.
** Note about HTML subchunks:
Unfortunately this only allows `html-mode' (not `nxhtml-mode') in
sub chunks." t)

(nxhtml-autoload 'mako-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Mako with main mode `html-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'xsl-nxml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode for XSL with main mode `nxml-mode'.
This covers inlined style and javascript." t)

(nxhtml-autoload 'xsl-sgml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode for XSL with main mode `sgml-mode'.
This covers inlined style and javascript." t)

(nxhtml-autoload 'markdown-html-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major markdown mode in buffer.
Main major mode will be `markdown-mode'.
Inlined html will be in `html-mode'.

You need `markdown-mode' which you can download from URL
`http://jblevins.org/projects/markdown-mode/'." t)

(nxhtml-autoload 'latex-clojure-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode latex+clojure.
Main major mode will be `latex-mode'.
Subchunks will be in `clojure-mode'.

You will need `clojure-mode' which you can download from URL
`http://github.com/jochu/clojure-mode/tree'." t)

(nxhtml-autoload 'latex-haskell-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multi major mode latex+haskell.
Main major mode will be `latex-mode'.
Subchunks will be in `haskell-mode'.

You will need `haskell-mode' which you can download from URL
`http://projects.haskell.org/haskellmode-emacs/'." t)

(nxhtml-autoload 'python-rst-mumamo-mode `(lp '(nxhtml-download-root-url nil) "util/mumamo-fun" nxhtml-install-dir) "\
Turn on multiple major modes for Python with RestructuredText docstrings." t)

;;;***

;;;### (autoloads (mumamo-add-region-from-string mumamo-add-region)
;;;;;;  "mumamo-regions" "util/mumamo-regions.el" (19275 63380))
;;; Generated autoloads from util/mumamo-regions.el
(web-autoload-require 'mumamo-regions 'lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mumamo-add-region `(lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir) "\
Add a mumamo region from selection.
Mumamo regions are like another layer of chunks above the normal chunks.
They does not affect the normal chunks, but they overrides them.

To create a mumamo region first select a visible region and then
call this function.

If the buffer is not in a multi major mode a temporary multi
major mode will be created applied to the buffer first.
To get out of this and get back to a single major mode just use

  M-x normal-mode

\(fn)" t nil)

(nxhtml-autoload 'mumamo-add-region-from-string `(lp '(nxhtml-download-root-url nil) "util/mumamo-regions" nxhtml-install-dir) "\
Add a mumamo region from string at point.
Works as `mumamo-add-region' but for string or comment at point.

Buffer must be fontified.

\(fn)" t nil)

;;;***

;;;### (autoloads (n-back-game n-back) "n-back" "util/n-back.el"
;;;;;;  (19278 15746))
;;; Generated autoloads from util/n-back.el
(web-autoload-require 'n-back 'lp '(nxhtml-download-root-url nil) "util/n-back" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'n-back 'custom-loads))) (if (member '"n-back" loads) nil (put 'n-back 'custom-loads (cons '"n-back" loads))))

(nxhtml-autoload 'n-back-game `(lp '(nxhtml-download-root-url nil) "util/n-back" nxhtml-install-dir) "\
Emacs n-Back game.
This game is supposed to increase your working memory and fluid
intelligence.

In this game something is shown for half a second on the screen
and maybe a sound is played.  You should then answer if parts of
it is the same as you have seen or heard before.  This is
repeated for about 20 trials.

You answer with the keys shown in the bottom window.

In the easiest version of the game you should answer if you have
just seen or heard what is shown now.  By default the game gets
harder as you play it with success.  Then first the number of
items presented in a trial grows.  After that it gets harder by
that you have to somehow remember not the last item, but the item
before that (or even earlier). That is what \"n-Back\" stands
for.

Note that remember does not really mean remember clearly.  The
game is for training your brain getting used to keep those things
in the working memory, maybe as a cross-modal unit.  You are
supposed to just nearly be able to do what you do in the game.
And you are supposed to have fun, that is what your brain like.

You should probably not overdue this. Half an hour a day playing
might be an optimal time according to some people.

The game is shamelessly modeled after Brain Workshop, see URL
`http://brainworkshop.sourceforge.net/' just for the fun of
getting it into Emacs.  The game resembles but it not the same as
that used in the report by Jaeggi mentioned at the above URL.

Not all features in Brain Workshop are implemented here, but some
new are maybe ... - and you have it available here in Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run nxhtmltest-run-indent) "nxhtmltest-suites"
;;;;;;  "tests/nxhtmltest-suites.el" (19360 6294))
;;; Generated autoloads from tests/nxhtmltest-suites.el
(web-autoload-require 'nxhtmltest-suites 'lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmltest-run-indent `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir) "\
Run indentation tests.

\(fn)" t nil)

(nxhtml-autoload 'nxhtmltest-run `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-suites" nxhtml-install-dir) "\
Run all tests defined for nXhtml.
Currently there are only tests using ert.el defined.

Note that it is currently expected that the following tests will
fail (they corresponds to known errors in nXhtml/Emacs):

  `nxhtml-ert-nxhtml-changes-jump-back-10549'
  `nxhtml-ert-nxhtml-changes-jump-back-7014'

\(fn)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run-Q) "nxhtmltest-Q" "tests/nxhtmltest-Q.el"
;;;;;;  (19264 36684))
;;; Generated autoloads from tests/nxhtmltest-Q.el
(web-autoload-require 'nxhtmltest-Q 'lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-Q" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmltest-run-Q `(lp '(nxhtml-download-root-url nil) "tests/nxhtmltest-Q" nxhtml-install-dir) "\
Run all tests defined for nXhtml in fresh Emacs.
See `nxhtmltest-run' for more information about the tests.

\(fn)" t nil)

;;;***

;;;### (autoloads (ert-run-tests-interactively ert-deftest) "ert"
;;;;;;  "tests/ert.el" (19173 56140))
;;; Generated autoloads from tests/ert.el
(web-autoload-require 'ert 'lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ert-deftest `(lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir) "\
Define NAME (a symbol) as a test.

\(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)" nil (quote macro))

(nxhtml-autoload 'ert-run-tests-interactively `(lp '(nxhtml-download-root-url nil) "tests/ert" nxhtml-install-dir) "\
Run the tests specified by SELECTOR and display the results in a buffer.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

;;;***

;;;### (autoloads (ocr-user-mode) "ocr-user" "util/ocr-user.el" (19290
;;;;;;  21626))
;;; Generated autoloads from util/ocr-user.el
(web-autoload-require 'ocr-user 'lp '(nxhtml-download-root-url nil) "util/ocr-user" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ocr-user-mode `(lp '(nxhtml-download-root-url nil) "util/ocr-user" nxhtml-install-dir) "\
Color up digits three by three.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ourcomments-warning ourcomments-M-x-menu-mode
;;;;;;  ourcomments-paste-with-convert-mode use-custom-style info-open-file
;;;;;;  replace-read-files rdir-query-replace ldir-query-replace
;;;;;;  grep-query-replace emacs-Q-nxhtml emacs-Q emacs--no-desktop
;;;;;;  emacs--debug-init emacs-buffer-file emacs emacs-restart ourcomments-ido-ctrl-tab
;;;;;;  ourcomments-ido-buffer-raise-frame ourcomments-ido-buffer-other-frame
;;;;;;  ourcomments-ido-buffer-other-window describe-symbol describe-defstruct
;;;;;;  describe-custom-group narrow-to-comment buffer-narrowed-p
;;;;;;  describe-command ourcomments-ediff-files find-emacs-other-file
;;;;;;  ourcomments-insert-date-and-time describe-timers ourcomments-copy+paste-set-point
;;;;;;  better-fringes-mode describe-key-and-map-briefly ourcomments-move-end-of-line
;;;;;;  ourcomments-move-beginning-of-line ourcomments-mark-whole-buffer-or-field
;;;;;;  fill-dwim unfill-individual-paragraphs unfill-region unfill-paragraph
;;;;;;  define-toggle-old define-toggle popup-menu-at-point ourcomments-indirect-fun)
;;;;;;  "ourcomments-util" "util/ourcomments-util.el" (19411 29548))
;;; Generated autoloads from util/ourcomments-util.el
(web-autoload-require 'ourcomments-util 'lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'ourcomments-indirect-fun `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Get the alias symbol for function FUN if any.

\(fn FUN)" nil nil)

(nxhtml-autoload 'popup-menu-at-point `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is.

\(fn MENU &optional PREFIX)" nil nil)

(nxhtml-autoload 'define-toggle `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu.

The arguments have the same meaning as for `defcustom' with these
restrictions:

- The :type keyword cannot be used.  Type is always 'boolean.
- VALUE must be t or nil.

DOC and ARGS are just passed to `defcustom'.

A `defcustom' named SYMBOL with doc-string DOC and a function
named SYMBOL-toggle is defined.  The function toggles the value
of SYMBOL.  It takes no parameters.

To create a menu item something similar to this can be used:

    (define-key map [SYMBOL]
      (list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))

\(fn SYMBOL VALUE DOC &rest ARGS)" nil (quote macro))

(nxhtml-autoload 'define-toggle-old `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented

\(fn SYMBOL VALUE DOC &rest ARGS)" nil (quote macro))

(nxhtml-autoload 'unfill-paragraph `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill the current paragraph.

\(fn)" t nil)

(nxhtml-autoload 'unfill-region `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill the current region.

\(fn)" t nil)

(nxhtml-autoload 'unfill-individual-paragraphs `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Unfill individual paragraphs in the current region.

\(fn)" t nil)

(nxhtml-autoload 'fill-dwim `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Fill or unfill paragraph or region.
With prefix ARG fill only current line.

\(fn ARG)" t nil)

(nxhtml-autoload 'ourcomments-mark-whole-buffer-or-field `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Mark whole buffer or editable field at point.

\(fn)" t nil)

(nxhtml-autoload 'ourcomments-move-beginning-of-line `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Move point to beginning of line or indentation.
See `beginning-of-line' for ARG.

If `line-move-visual' is non-nil then the visual line beginning
is first tried.

If in a widget field stay in that.

\(fn ARG)" t nil)

(nxhtml-autoload 'ourcomments-move-end-of-line `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Move point to end of line or after last non blank char.
See `end-of-line' for ARG.

Similar to `ourcomments-move-beginning-of-line' but for end of
line.

\(fn ARG)" t nil)

(nxhtml-autoload 'describe-key-and-map-briefly `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Try to print names of keymap from which KEY fetch its definition.
Look in current active keymaps and find keymap variables with the
same value as the keymap where KEY is bound.  Print a message
with those keymap variable names.  Return a list with the keymap
variable symbols.

When called interactively prompt for KEY.

INSERT and UNTRANSLATED should normall be nil (and I am not sure
what they will do ;-).

\(fn &optional KEY INSERT UNTRANSLATED)" t nil)

(defvar better-fringes-mode nil "\
Non-nil if Better-Fringes mode is enabled.
See the command `better-fringes-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `better-fringes-mode'.")

(nxhtml-custom-autoload 'better-fringes-mode 'ourcomments-util nil)

(nxhtml-autoload 'better-fringes-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Choose another fringe bitmap color and bottom angle.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'ourcomments-copy+paste-set-point `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Set point for copy+paste here.
Enable temporary minor mode `ourcomments-copy+paste-mode'.
However if point for copy+paste already is set then cancel it and
disable the minor mode.

The purpose of this command is to make it easy to grab a piece of
text and paste it at current position.  After this command you
should select a piece of text to copy and then call the command
`ourcomments-copy+paste'.

\(fn)" t nil)

(nxhtml-autoload 'describe-timers `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show timers with readable time format.

\(fn)" t nil)

(nxhtml-autoload 'ourcomments-insert-date-and-time `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Insert date and time.
See option `ourcomments-insert-date-and-time' for how to
customize it.

\(fn)" t nil)

(nxhtml-autoload 'find-emacs-other-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Find corresponding file to source or installed elisp file.
If you have checked out and compiled Emacs yourself you may have
Emacs lisp files in two places, the checked out source tree and
the installed Emacs tree.  If buffer contains an Emacs elisp file
in one of these places then find the corresponding elisp file in
the other place. Return the file name of this file.

Rename current buffer using your `uniquify-buffer-name-style' if
it is set.

When DISPLAY-FILE is non-nil display this file in other window
and go to the same line number as in the current buffer.

\(fn DISPLAY-FILE)" t nil)

(nxhtml-autoload 'ourcomments-ediff-files `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
In directory DEF-DIR run `ediff-files' on files FILE-A and FILE-B.
The purpose of this function is to make it eaiser to start
`ediff-files' from a shell through Emacs Client.

This is used in EmacsW32 in the file ediff.cmd where Emacs Client
is called like this:

  @%emacs_client% -e \"(setq default-directory \\\"%emacs_cd%\\\")\"
  @%emacs_client% -n  -e \"(ediff-files \\\"%f1%\\\" \\\"%f2%\\\")\"

It can of course be done in a similar way with other shells.

\(fn DEF-DIR FILE-A FILE-B)" nil nil)

(nxhtml-autoload 'describe-command `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Like `describe-function', but prompts only for interactive commands.

\(fn COMMAND)" t nil)

(nxhtml-autoload 'buffer-narrowed-p `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Return non-nil if the current buffer is narrowed.

\(fn)" nil nil)

(nxhtml-autoload 'narrow-to-comment `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'describe-custom-group `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Describe customization group SYMBOL.

\(fn SYMBOL)" t nil)

(nxhtml-autoload 'describe-defstruct `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented

\(fn SYMBOL)" t nil)

(nxhtml-autoload 'describe-symbol `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show information about SYMBOL.
Show SYMBOL plist and whether is is a variable or/and a
function.

\(fn SYMBOL)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-other-window `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show buffer in other window.

\(fn)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-other-frame `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Show buffer in other frame.

\(fn)" t nil)

(nxhtml-autoload 'ourcomments-ido-buffer-raise-frame `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Raise frame showing buffer.

\(fn)" t nil)

(defvar ourcomments-ido-ctrl-tab nil "\
Non-nil if Ourcomments-Ido-Ctrl-Tab mode is enabled.
See the command `ourcomments-ido-ctrl-tab' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ourcomments-ido-ctrl-tab'.")

(nxhtml-custom-autoload 'ourcomments-ido-ctrl-tab 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-ido-ctrl-tab `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Enable buffer switching using C-Tab with function `ido-mode'.
This changes buffer switching with function `ido-mode' the
following way:

- You can use C-Tab.

- You can show the selected buffer in three ways independent of
  how you entered function `ido-mode' buffer switching:

  * S-return: other window
  * C-return: other frame
  * M-return: raise frame

Those keys are selected to at least be a little bit reminiscent
of those in for example common web browsers.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'emacs-restart `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Restart Emacs and start `server-mode' if on before.

\(fn)" t nil)

(nxhtml-autoload 'emacs `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with default parameters.
Additional ARGS are passed to the new Emacs.

See also `ourcomments-started-emacs-use-output-buffer'.

\(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-buffer-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs showing current buffer file.
Go to the current line and column in that file.
If there is no buffer file then instead start with `dired'.

This calls the function `emacs' with argument --no-desktop and
the file or a call to dired.

\(fn)" t nil)

(nxhtml-autoload 'emacs--debug-init `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with --debug-init parameter.
This calls the function `emacs' with added arguments ARGS.

\(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs--no-desktop `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with --no-desktop parameter.
This calls the function `emacs' with added arguments ARGS.

\(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-Q `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start a new Emacs with -Q parameter.
Start new Emacs without any customization whatsoever.
This calls the function `emacs' with added arguments ARGS.

\(fn &rest ARGS)" t nil)

(nxhtml-autoload 'emacs-Q-nxhtml `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Start new Emacs with -Q and load nXhtml.
This calls the function `emacs' with added arguments ARGS.

\(fn &rest ARGS)" t nil)

(nxhtml-autoload 'grep-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Do `query-replace-regexp' of FROM with TO, on all files in *grep*.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

\(fn FROM TO &optional DELIMITED)" t nil)

(nxhtml-autoload 'ldir-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Replace FROM with TO in FILES in directory DIR.
This runs `query-replace-regexp' in files matching FILES in
directory DIR.

See `tags-query-replace' for DELIMETED and more information.

\(fn FROM TO FILES DIR &optional DELIMITED)" t nil)

(nxhtml-autoload 'rdir-query-replace `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Replace FROM with TO in FILES in directory tree ROOT.
This runs `query-replace-regexp' in files matching FILES in
directory tree ROOT.

See `tags-query-replace' for DELIMETED and more information.

\(fn FROM TO FILE-REGEXP ROOT &optional DELIMITED)" t nil)

(nxhtml-autoload 'replace-read-files `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Read files arg for replace.

\(fn REGEXP &optional REPLACE)" nil nil)

(nxhtml-autoload 'info-open-file `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Open an info file in `Info-mode'.

\(fn INFO-FILE)" t nil)

(nxhtml-autoload 'use-custom-style `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Setup like in `Custom-mode', but without things specific to Custom.

\(fn)" nil nil)

(defvar ourcomments-paste-with-convert-mode nil "\
Non-nil if Ourcomments-Paste-With-Convert mode is enabled.
See the command `ourcomments-paste-with-convert-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ourcomments-paste-with-convert-mode'.")

(nxhtml-custom-autoload 'ourcomments-paste-with-convert-mode 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-paste-with-convert-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Pasted text may be automatically converted in this mode.
The functions in `ourcomments-paste-with-convert-hook' are run
after commands in `ourcomments-paste-with-convert-commands' if any
of the functions returns non-nil that text is inserted instead of
the original text.

For exampel when this mode is on and you paste an html link in an
`org-mode' buffer it will be directly converted to an org style
link. (This is the default behaviour.)

Tip: The Firefox plugin Copy as HTML Link is handy, see URL
     `https://addons.mozilla.org/en-US/firefox/addon/2617'.

Note: This minor mode will defadvice the paste commands.

\(fn &optional ARG)" t nil)

(defvar ourcomments-M-x-menu-mode nil "\
Non-nil if Ourcomments-M-X-Menu mode is enabled.
See the command `ourcomments-M-x-menu-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ourcomments-M-x-menu-mode'.")

(nxhtml-custom-autoload 'ourcomments-M-x-menu-mode 'ourcomments-util nil)

(nxhtml-autoload 'ourcomments-M-x-menu-mode `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Add commands started from Emacs menus to M-x history.
The purpose of this is to make it easier to redo them and easier
to learn how to do them from the command line (which is often
faster if you know how to do it).

Only commands that are not already in M-x history are added.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'ourcomments-warning `(lp '(nxhtml-download-root-url nil) "util/ourcomments-util" nxhtml-install-dir) "\
Not documented

\(fn FORMAT-STRING &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (major-modep major-or-multi-majorp) "ourcomments-widgets"
;;;;;;  "util/ourcomments-widgets.el" (19275 63380))
;;; Generated autoloads from util/ourcomments-widgets.el
(web-autoload-require 'ourcomments-widgets 'lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir 'nxhtml-byte-compile-file)

 (nxhtml-autoload 'command "ourcomments-widgets")

(nxhtml-autoload 'major-or-multi-majorp `(lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir) "\
Return t if VALUE is a major or multi major mode function.

\(fn VALUE)" nil nil)

(nxhtml-autoload 'major-modep `(lp '(nxhtml-download-root-url nil) "util/ourcomments-widgets" nxhtml-install-dir) "\
Return t if VALUE is a major mode function.

\(fn VALUE)" nil nil)
 (nxhtml-autoload 'major-mode-function "ourcomments-widgets")

;;;***

;;;### (autoloads (pause-start-in-new-emacs pause-mode pause) "pause"
;;;;;;  "util/pause.el" (19335 58922))
;;; Generated autoloads from util/pause.el
(web-autoload-require 'pause 'lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'pause 'custom-loads))) (if (member '"pause" loads) nil (put 'pause 'custom-loads (cons '"pause" loads))))

(defvar pause-mode nil "\
Non-nil if Pause mode is enabled.
See the command `pause-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pause-mode'.")

(nxhtml-custom-autoload 'pause-mode 'pause nil)

(nxhtml-autoload 'pause-mode `(lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir) "\
This minor mode tries to make you take a break.
It will jump up and temporary stop your work - even if you are
not in Emacs.  If you are in Emacs it will however try to be
gentle and wait until you have been idle with the keyboard for a
short while. (If you are not in Emacs it can't be gentle. How
could it?)

Then it will show you a special screen with a link to a yoga
exercise you can do when you pause.

After the pause you continue your work where you were
interrupted.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'pause-start-in-new-emacs `(lp '(nxhtml-download-root-url nil) "util/pause" nxhtml-install-dir) "\
Start pause with interval AFTER-MINUTES in a new Emacs instance.
The new Emacs instance will be started with -Q.  However if
`custom-file' is non-nil it will be loaded so you can still
customize pause.

One way of using this function may be to put in your .emacs
something like

  ;; for just one Emacs running pause
  (when server-mode (pause-start-in-new-emacs 15))

See `pause-start' for more info.

\(fn AFTER-MINUTES)" t nil)

;;;***

;;;### (autoloads (global-pointback-mode pointback-mode) "pointback"
;;;;;;  "util/pointback.el" (19023 47096))
;;; Generated autoloads from util/pointback.el
(web-autoload-require 'pointback 'lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'pointback-mode `(lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir) "\
Restore previous window point when switching back to a buffer.

\(fn &optional ARG)" t nil)

(defvar global-pointback-mode nil "\
Non-nil if Global-Pointback mode is enabled.
See the command `global-pointback-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pointback-mode'.")

(nxhtml-custom-autoload 'global-pointback-mode 'pointback nil)

(nxhtml-autoload 'global-pointback-mode `(lp '(nxhtml-download-root-url nil) "util/pointback" nxhtml-install-dir) "\
Toggle Pointback mode in every possible buffer.
With prefix ARG, turn Global-Pointback mode on if and only if
ARG is positive.
Pointback mode is enabled in all buffers where
`pointback-on' would do it.
See `pointback-mode' for more information on Pointback mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (popcmp-completing-read popcmp-completion-style
;;;;;;  popcmp) "popcmp" "util/popcmp.el" (19365 33760))
;;; Generated autoloads from util/popcmp.el
(web-autoload-require 'popcmp 'lp '(nxhtml-download-root-url nil) "util/popcmp" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'popcmp 'custom-loads))) (if (member '"popcmp" loads) nil (put 'popcmp 'custom-loads (cons '"popcmp" loads))))

(defvar popcmp-completion-style (cond (t 'popcmp-popup)) "\
Completion style.
The currently available completion styles are:

- popcmp-popup: Use OS popup menus (default).
- emacs-default: Emacs default completion.
- Company Mode completion.
- anything: The Anything elisp lib completion style.

The style of completion set here is not implemented for all
completions.  The scope varies however with which completion
style you have choosen.

For information about Company Mode and how to use it see URL
`http://www.emacswiki.org/emacs/CompanyMode'.

For information about Anything and how to use it see URL
`http://www.emacswiki.org/emacs/Anything'.

See also the options `popcmp-short-help-beside-alts' and
`popcmp-group-alternatives' which are also availabe when popup
completion is available.")

(nxhtml-custom-autoload 'popcmp-completion-style 'popcmp nil)

(nxhtml-autoload 'popcmp-completing-read `(lp '(nxhtml-download-root-url nil) "util/popcmp" nxhtml-install-dir) "\
Read a string in the minubuffer with completion, or popup a menu.
This function can be used instead `completing-read'. The main
purpose is to provide a popup style menu for completion when
completion is tighed to text at point in a buffer. If a popup
menu is used it will be shown at window point. Whether a popup
menu or minibuffer completion is used is governed by
`popcmp-completion-style'.

The variables PROMPT, TABLE, PREDICATE, REQUIRE-MATCH,
INITIAL-INPUT, POP-HIST, DEF and INHERIT-INPUT-METHOD all have the
same meaning is for `completing-read'.

ALT-HELP should be nil or a hash variable or an association list
with the completion alternative as key and a short help text as
value.  You do not need to supply help text for all alternatives.
The use of ALT-HELP is set by `popcmp-short-help-beside-alts'.

ALT-SETS should be nil or an association list that has as keys
groups and as second element an alternative that should go into
this group.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT POP-HIST DEF INHERIT-INPUT-METHOD ALT-HELP ALT-SETS)" nil nil)

;;;***

;;;### (autoloads (rebind-keys-mode rebind) "rebind" "util/rebind.el"
;;;;;;  (19292 11678))
;;; Generated autoloads from util/rebind.el
(web-autoload-require 'rebind 'lp '(nxhtml-download-root-url nil) "util/rebind" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'rebind 'custom-loads))) (if (member '"rebind" loads) nil (put 'rebind 'custom-loads (cons '"rebind" loads))))

(defvar rebind-keys-mode nil "\
Non-nil if Rebind-Keys mode is enabled.
See the command `rebind-keys-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `rebind-keys-mode'.")

(nxhtml-custom-autoload 'rebind-keys-mode 'rebind nil)

(nxhtml-autoload 'rebind-keys-mode `(lp '(nxhtml-download-root-url nil) "util/rebind" nxhtml-install-dir) "\
Rebind keys as defined in `rebind-keys'.
The key bindings will override almost all other key bindings
since it is put on emulation level, like for example ``cua-mode'
and `viper-mode'.

This is for using for example C-a to mark the whole buffer (or a
field). There are some predifined keybindings for this.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rnc-mode) "rnc-mode" "util/rnc-mode.el" (18775
;;;;;;  60004))
;;; Generated autoloads from util/rnc-mode.el
(web-autoload-require 'rnc-mode 'lp '(nxhtml-download-root-url nil) "util/rnc-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'rnc-mode `(lp '(nxhtml-download-root-url nil) "util/rnc-mode" nxhtml-install-dir) "\
Major mode for editing RELAX NG Compact Syntax schemas.
\\{rnc-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (search-form) "search-form" "util/search-form.el"
;;;;;;  (19275 63380))
;;; Generated autoloads from util/search-form.el
(web-autoload-require 'search-form 'lp '(nxhtml-download-root-url nil) "util/search-form" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'search-form `(lp '(nxhtml-download-root-url nil) "util/search-form" nxhtml-install-dir) "\
Display a form for search and replace.

\(fn)" t nil)

;;;***

;;;### (autoloads (sex-mode sex) "sex-mode" "util/sex-mode.el" (19218
;;;;;;  42182))
;;; Generated autoloads from util/sex-mode.el
(web-autoload-require 'sex-mode 'lp '(nxhtml-download-root-url nil) "util/sex-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'sex 'custom-loads))) (if (member '"sex-mode" loads) nil (put 'sex 'custom-loads (cons '"sex-mode" loads))))

(defvar sex-mode nil "\
Non-nil if Sex mode is enabled.
See the command `sex-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sex-mode'.")

(nxhtml-custom-autoload 'sex-mode 'sex-mode nil)

(nxhtml-autoload 'sex-mode `(lp '(nxhtml-download-root-url nil) "util/sex-mode" nxhtml-install-dir) "\
Open certain files in external programs.
See `sex-get-file-open-cmd' for how to determine which files to
open by external applications.  Note that this selection is
nearly the same as in `org-mode'.  The main difference is that
the fallback always is to open a file in Emacs. (This is
necessary to avoid to disturb many of Emacs operations.)

This affects all functions that opens files, like `find-file',
`find-file-noselect' etc.

However it does not affect files opened through Emacs client.

Urls can also be handled, see `sex-handle-urls'.

When opening a file with the shell a (temporary) dummy buffer is
created in Emacs with major mode `sex-file-mode' and an external
program is called to handle the file.  How this dummy buffer is
handled is governed by `sex-keep-dummy-buffer'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (sml-modeline-mode sml-modeline) "sml-modeline"
;;;;;;  "util/sml-modeline.el" (19362 49086))
;;; Generated autoloads from util/sml-modeline.el
(web-autoload-require 'sml-modeline 'lp '(nxhtml-download-root-url nil) "util/sml-modeline" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'sml-modeline 'custom-loads))) (if (member '"sml-modeline" loads) nil (put 'sml-modeline 'custom-loads (cons '"sml-modeline" loads))))

(defvar sml-modeline-mode nil "\
Non-nil if Sml-Modeline mode is enabled.
See the command `sml-modeline-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sml-modeline-mode'.")

(nxhtml-custom-autoload 'sml-modeline-mode 'sml-modeline nil)

(nxhtml-autoload 'sml-modeline-mode `(lp '(nxhtml-download-root-url nil) "util/sml-modeline" nxhtml-install-dir) "\
Show buffer size and position like scrollbar in mode line.
You can customize this minor mode, see option `sml-modeline-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tabkey2-emma-without-tabkey2 tabkey2-mode tabkey2)
;;;;;;  "tabkey2" "util/tabkey2.el" (19277 65356))
;;; Generated autoloads from util/tabkey2.el
(web-autoload-require 'tabkey2 'lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'tabkey2 'custom-loads))) (if (member '"tabkey2" loads) nil (put 'tabkey2 'custom-loads (cons '"tabkey2" loads))))

(defvar tabkey2-mode nil "\
Non-nil if Tabkey2 mode is enabled.
See the command `tabkey2-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabkey2-mode'.")

(nxhtml-custom-autoload 'tabkey2-mode 'tabkey2 nil)

(nxhtml-autoload 'tabkey2-mode `(lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir) "\
More fun with Tab key number two (completion etc).
This global minor mode by default binds Tab in a way that let you
do completion with Tab in all buffers (where it is possible).

The Tab key is easy to type on your keyboard.  Then why not use
it for completion, something that is very useful?  Shells usually
use Tab for completion so many are used to it.  This was the idea
of Smart Tabs and this is a generalization of that idea.

However in Emacs the Tab key is usually used for indentation.
The idea here is that if Tab has been pressed once for
indentation, then as long as point stays further Tab keys might
as well do completion.

So you kind of do Tab-Tab for first completion (and then just
Tab for further completions as long as point is not moved).

And there is even kind of Tab-Tab-Tab completion: If completion
fails the next completion function will be the one you try with
next Tab. (You get some notification of this, of course.)

See `tabkey2-first' for more information about usage.

Note: If you do not want the Tab-Tab behaviour above, but still
want an easy way to reach the available completion functions,
then you can instead of turning on tabkey2-mode enter this in
your .emacs:

 (global-set-key [f8] 'tabkey2-cycle-completion-functions)

After hitting f8 you will then be in the same state as after the
first in tabkey2-mode.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'tabkey2-emma-without-tabkey2 `(lp '(nxhtml-download-root-url nil) "util/tabkey2" nxhtml-install-dir) "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (tyda-mode) "tyda" "util/tyda.el" (19275 63380))
;;; Generated autoloads from util/tyda.el
(web-autoload-require 'tyda 'lp '(nxhtml-download-root-url nil) "util/tyda" nxhtml-install-dir 'nxhtml-byte-compile-file)


(defvar tyda-mode nil "\
Non-nil if Tyda mode is enabled.
See the command `tyda-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tyda-mode'.")

(nxhtml-custom-autoload 'tyda-mode 'tyda nil)

(nxhtml-autoload 'tyda-mode `(lp '(nxhtml-download-root-url nil) "util/tyda" nxhtml-install-dir) "\
Minor mode for key bindings for `tyda-lookup-word'.
It binds Alt-Mouse-1 just as the Tyda add-on does in Firefox.
Here are all key bindings

\\{tyda-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (udev-call-first-step) "udev" "util/udev.el" (19412
;;;;;;  25976))
;;; Generated autoloads from util/udev.el
(web-autoload-require 'udev 'lp '(nxhtml-download-root-url nil) "util/udev" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-call-first-step `(lp '(nxhtml-download-root-url nil) "util/udev" nxhtml-install-dir) "\
Set up and call first step.
Set up buffer LOG-BUFFER to be used for log messages and
controling of the execution of the functions in list STEPS which
are executed one after another.

Write HEADER at the end of LOG-BUFFER.

Call first step.

If FINISH-FUN non-nil it should be a function. This is called
after last step with LOG-BUFFER as parameter.

\(fn LOG-BUFFER STEPS HEADER FINISH-FUN)" nil nil)

;;;***

;;;### (autoloads (udev-ecb-customize-startup udev-ecb-update) "udev-ecb"
;;;;;;  "util/udev-ecb.el" (19256 5410))
;;; Generated autoloads from util/udev-ecb.el
(web-autoload-require 'udev-ecb 'lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-ecb-update `(lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir) "\
Fetch and install ECB from the devel sources.
To determine where to store the sources see `udev-ecb-dir'.
For how to start ECB see `udev-ecb-load-ecb'.

\(fn)" t nil)

(nxhtml-autoload 'udev-ecb-customize-startup `(lp '(nxhtml-download-root-url nil) "util/udev-ecb" nxhtml-install-dir) "\
Customize ECB dev nXhtml startup group.

\(fn)" t nil)

;;;***

;;;### (autoloads (udev-rinari-update) "udev-rinari" "util/udev-rinari.el"
;;;;;;  (19256 5410))
;;; Generated autoloads from util/udev-rinari.el
(web-autoload-require 'udev-rinari 'lp '(nxhtml-download-root-url nil) "util/udev-rinari" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'udev-rinari-update `(lp '(nxhtml-download-root-url nil) "util/udev-rinari" nxhtml-install-dir) "\
Fetch and install Rinari from the devel sources.
To determine where to store the sources and how to start rinari
see `udev-rinari-dir' and `udev-rinari-load-rinari'.

\(fn)" t nil)

;;;***

;;;### (autoloads (viper-tutorial) "viper-tut" "util/viper-tut.el"
;;;;;;  (19388 44990))
;;; Generated autoloads from util/viper-tut.el
(web-autoload-require 'viper-tut 'lp '(nxhtml-download-root-url nil) "util/viper-tut" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'viper-tutorial `(lp '(nxhtml-download-root-url nil) "util/viper-tut" nxhtml-install-dir) "\
Run a tutorial for Viper.

A simple classic tutorial in 5 parts that have been used by many
people starting to learn vi keys.  You may learn enough to start
using `viper-mode' in Emacs.

Some people find that vi keys helps against repetetive strain
injury, see URL

  `http://www.emacswiki.org/emacs/RepeatedStrainInjury'.

Note: There might be a few clashes between vi key binding and
Emacs standard key bindings.  You will be notified about those in
the tutorial.  Even more, if your own key bindings comes in
between you will be notified about that too.

\(fn PART &optional DONT-ASK-FOR-REVERT)" t nil)

;;;***

;;;### (autoloads (vline-global-mode vline-mode) "vline" "util/vline.el"
;;;;;;  (19157 2168))
;;; Generated autoloads from util/vline.el
(web-autoload-require 'vline 'lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'vline-mode `(lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir) "\
Display vertical line mode.

\(fn &optional ARG)" t nil)

(defvar vline-global-mode nil "\
Non-nil if Vline-Global mode is enabled.
See the command `vline-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vline-global-mode'.")

(nxhtml-custom-autoload 'vline-global-mode 'vline nil)

(nxhtml-autoload 'vline-global-mode `(lp '(nxhtml-download-root-url nil) "util/vline" nxhtml-install-dir) "\
Display vertical line mode as globally.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (whelp) "whelp" "util/whelp.el" (19277 65356))
;;; Generated autoloads from util/whelp.el
(web-autoload-require 'whelp 'lp '(nxhtml-download-root-url nil) "util/whelp" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'whelp 'custom-loads))) (if (member '"whelp" loads) nil (put 'whelp 'custom-loads (cons '"whelp" loads))))

;;;***

;;;### (autoloads (wikipedia-draft-buffer wikipedia-draft-page wikipedia-draft
;;;;;;  wikipedia-mode) "wikipedia-mode" "related/wikipedia-mode.el"
;;;;;;  (19277 65356))
;;; Generated autoloads from related/wikipedia-mode.el
(web-autoload-require 'wikipedia-mode 'lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'wikipedia-mode `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Major mode for editing wikimedia style wikis.
Major mode for editing articles written in the markup language
used by Wikipedia, the free on-line
encyclopedia (see URL `http://www.wikipedia.org').

There are several ways to use wikipedia-mode:

- You can simply cut and paste articles between Emacs and your
  web browser's text box.
- If you are using Firefox you can use the It's All Text add-on
  for Firefox.
- You can use MozEx, a Mozilla/Firefox web browser extension that
  allows you to call Emacs from a text
  box (see URL `http://mozex.mozdev.org/').
- Another way is to use the PERL script ee-helper, which allows
  you to up and download wiki texts.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikipedia-fill-article] fills the buffer.
\\[wikipedia-unfill-article] unfills the buffer.
Be warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.
\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph
\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.



The following commands put in markup structures.

\\[wikipedia-insert-bold-italic] bold+italic
\\[wikipedia-insert-bold] bold text
\\[wikipedia-insert-italics] italics
\\[wikipedia-insert-nowiki] no wiki markup
\\[wikipedia-insert-link-wiki] inserts a link

The following commands are also defined:
\\[wikipedia-insert-user] inserts user name
\\[wikipedia-insert-signature] inserts ~~~~
\\[wikipedia-insert-enumerate] inserts enumerate type structures
\\[wikipedia-insert-itemize] inserts itemize type structures
\\[wikipedia-insert-hline] inserts a hline

The draft functionality
\\[wikipedia-draft]
\\[wikipedia-draft-region]
\\[wikipedia-draft-view-draft]
\\[wikipedia-draft-page]
\\[wikipedia-draft-buffer]

Replying and sending functionality
\\[wikipedia-reply-at-point-simple]
\\[wikipedia-draft-reply]


The register functionality
\\[wikipedia-copy-page-to-register]
\\[defun wikipedia-insert-page-to-register]


Some simple editing commands.
\\[wikipedia-enhance-indent]
\\[wikipedia-yank-prefix]
\\[wikipedia-unfill-paragraph-or-region]



\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.

\(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Open a temporary buffer in wikipedia mode for editing an
 wikipedia draft, which an arbitrary piece of data. After
 finishing the editing either use \\[wikipedia-draft-buffer] to
 send the data into the wikipedia-draft-data-file, or send the
 buffer using `wikipedia-draft-send-to-mozex' and insert it later
 into a wikipedia article.

\(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft-page `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'wikipedia-draft-buffer `(lp '(nxhtml-download-root-url nil) "related/wikipedia-mode" nxhtml-install-dir) "\
Wikipedia-draft-buffer sends the contents of the current (temporary)
buffer to the wikipedia-draft-buffer, see the variable
wikipedia-draft-data-file.

\(fn)" t nil)

(defvar wikipedia-draft-send-archive t "\
*Archive the reply.")

;;;***

;;;### (autoloads (visual-basic-mode) "visual-basic-mode" "related/visual-basic-mode.el"
;;;;;;  (19235 1650))
;;; Generated autoloads from related/visual-basic-mode.el
(web-autoload-require 'visual-basic-mode 'lp '(nxhtml-download-root-url nil) "related/visual-basic-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'visual-basic-mode `(lp '(nxhtml-download-root-url nil) "related/visual-basic-mode" nxhtml-install-dir) "\
A mode for editing Microsoft Visual Basic programs.
Features automatic indentation, font locking, keyword capitalization,
and some minor convenience functions.
Commands:
\\{visual-basic-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (tt-mode) "tt-mode" "related/tt-mode.el" (18603
;;;;;;  15792))
;;; Generated autoloads from related/tt-mode.el
(web-autoload-require 'tt-mode 'lp '(nxhtml-download-root-url nil) "related/tt-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'tt-mode `(lp '(nxhtml-download-root-url nil) "related/tt-mode" nxhtml-install-dir) "\
Major mode for editing Template Toolkit files.

\(fn)" t nil)

;;;***

;;;### (autoloads (smarty-mode smarty) "smarty-mode" "related/smarty-mode.el"
;;;;;;  (19235 1650))
;;; Generated autoloads from related/smarty-mode.el
(web-autoload-require 'smarty-mode 'lp '(nxhtml-download-root-url nil) "related/smarty-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'smarty 'custom-loads))) (if (member '"smarty-mode" loads) nil (put 'smarty 'custom-loads (cons '"smarty-mode" loads))))

(nxhtml-autoload 'smarty-mode `(lp '(nxhtml-download-root-url nil) "related/smarty-mode" nxhtml-install-dir) "\
Smarty Mode
***********

Smarty Mode is a GNU XEmacs major mode for editing Smarty templates.

1 Introduction
**************

Smarty-Mode is a mode allowing easy edit of Smarty templates:
highlight, templates, navigation into source files...



Features (new features in bold) :

   * Completion

   * Customizable

   * Highlight

   * Menu

   * Stuttering

   * Templates
        - Built-in Functions

        - User Functions

        - Variable Modifiers

        - Plugin (Functions)
             * BlockRepeatPlugin

             * ClipCache

             * Smarty Formtool

             * Smarty Paginate

             * Smarty Validate

        - Plugin (Variable Modifiers)
             * AlternativeDateModifierPlugin

             * B2Smilies

             * BBCodePlugin

        - Fonctions Non-Smarty



This manual describes Smarty Mode version 0.0.5.

2 Installation
**************

2.1 Requirements
================

Smarty Mode is a XEmacs major mode that needs the following
software/packages:

   * XEmacs (http://www.xemacs.org/).

   * `font-lock' mode generaly installed with XEmacs.

   * `assoc' mode generaly installed with XEmacs.

   * `easymenu' mode generaly installed with XEmacs.

   * `hippie-exp' mode generaly installed with XEmacs.

Before continuing, you must be sure to have all this packages
installed.

2.2 Download
============

Two internet address to download Smarty Mode :

   * Principal: Smarty-Mode 0.0.5
     (http://deboutv.free.fr/lisp/smarty/download/smarty-0.0.5.tar.gz)
     (http://deboutv.free.fr/lisp/smarty/)

   * Secondary: Smarty-Mode 0.0.5
     (http://www.morinie.fr/lisp/smarty/download/smarty-0.0.5.tar.gz)
     (http://www.morinie.fr/lisp/smarty/)

   * Old releases: Smarty-Mode
     (http://deboutv.free.fr/lisp/smarty/download.php)
     (http://deboutv.free.fr/lisp/smarty/)

2.3 Installation
================

2.3.1 Installation
------------------

To install Smarty Mode you need to choose an installation directory
\(for example `/usr/local/share/lisp' or `c:lisp'). The administrator
must have the write rights on this directory.

With your favorite unzip software, unzip the archive in the
installation directory.

Example:
     cd /usr/local/share/lisp
     tar zxvf smarty-0.0.5.tar.gz
Now you have a `smarty' directory in the installation directory. This
directory contains 2 files `smarty-mode.el' and `smarty-mode.elc' and
another directory `docs' containing the documentation.

You need to configure XEmacs. open you initialization file `init.el'
\(open the file or start XEmacs then choose the Options menu and Edit
Init File). Add the following lines (the installation directory in
this example is `/usr/local/share/lisp') :

     (setq load-path
           (append (list \"/usr/local/share/lisp/\") load-path))
     (nxhtml-autoload 'smarty-mode \"smarty-mode\" \"Smarty Mode\" t)

2.3.2 Update
------------

The update is easy. You need to unzip the archive in the installation
directory to remove the old release.

Example:
     cd /usr/local/share/lisp
     rm -rf smarty
     tar zxvf smarty-0.0.5.tar.gz

2.4 Invoke Smarty-Mode
======================

You have two possibilities to invoke the Smarty Mode.

   - Manually: At each file opening you need to launch Smarty Mode
     with the following command:

     `M-x smarty-mode'

   - Automatically: Add the following linesin your initialization
     file `init.el' :

          (setq auto-mode-alist
                (append
                 '((\"\\.tpl$\" . smarty-mode))
          	 auto-mode-alist))


3 Customization
***************

This chapter describes the differents parameters and functions that
you can change to customize Smarty Mode.  To do that, open a Smarty
file, click on the Smarty menu and choose Options then Browse
Options....

3.1 Parameters
==============

3.1.1 Mode
----------

Smarty Mode has 2 modes allowing to simplify the writing of Smarty
templates. You can enable/disable each mode individually.

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

3.1.2 Menu
----------

Smarty Mode has also 1 menu that you can enable/disable. The menu
Sources is specific to each Smarty files opened.

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

3.1.3 Menu
----------

`smarty-highlight-plugin-functions'
     Type: boolean
     Default value: `t'
     Description: If `t'; the functions described in the smarty
     plugins are highlighted.

3.1.4 Templates
---------------

3.1.4.1 Header
..............

`smarty-file-header'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file header. If the
     string specifies an existing file name the contents of the file
     is inserted; otherwise the string itself is inserted as file
     header.
     Type `C-j' for newlines.
     The follonwing keywords are supported:
     <filename>: replaced by the file name.
     <author>: replaced by the user name and email address.
     <login>: replaced by `user-login-name'.
     <company>: replaced by `smarty-company-name' content.
     <date>: replaced by the current date.
     <year>: replaced by the current year.
     <copyright>: replaced by `smarty-copyright-string' content.
     <cursor>: final cursor position.

`smarty-file-footer'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file footer.  See
     `smarty-file-header'

`smarty-company-name'
     Type: string
     Default value: `\"\"'
     Description: Name of the company to insert in file header.

`smarty-copyright-string'
     Type: string
     Default value: `\"\"'
     Description: Coryright string to insert in file header.

`smarty-date-format'
     Type: string
     Default value: `\"%Y-%m-%d\"'
     Description: Date format.

`smarty-modify-date-prefix-string'
     Type: string
     Default value: `\"\"'
     Description: Prefix string of modification date in Smarty file
     header.

`smarty-modify-date-on-saving'
     Type: bool
     Default value: `nil'
     Description: If `t'; update the modification date when the
     buffer is saved.

3.1.5 Miscellaneous
-------------------

`smarty-left-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Left escaping delimiter for Smarty templates.

`smarty-right-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Right escaping delimiter for Smarty templates.

`smarty-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`smarty-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`smarty-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

3.2 Functions
=============

3.2.1 Mode
----------

`smarty-electric-mode'
     Menu: Smarty -> Options -> Mode -> Electric Mode
     Keybinding: `C-c C-m C-e'
     Description: This functions is used to enable/disable the
     electric mode.

`smarty-stutter-mode'
     Menu: Smarty -> Options -> Mode -> Stutter Mode
     Keybinding: `C-c C-m C-s'
     Description: This function is used to enable/disable the stutter
     mode.

4 Menus
*******

There are 2 menus: Smarty and Sources. All theses menus can be
accessed from the menubar or from the right click. This chapter
describes each menus.

4.1 Smarty
==========

This is the main menu of Smarty Mode. It allows an easy access to the
main features of the Smarty Mode: Templates (see *Note Templates::)
and Options (see *Note Customization::).

This menu contains also 3 functions that are discussed in the next
part.

4.1.1 Functions
---------------

`smarty-show-messages'
     Menu: Smarty -> Show Messages
     Keybinding: `C-c M-m'
     Description: This function opens the *Messages* buffer to
     display previous error messages.

`smarty-doc-mode'
     Menu: Smarty -> Smarty Mode Documentation
     Keybinding: `C-c C-h'
     Description: This function opens the *Help* buffer and prints in
     it the Smarty Mode documentation.

`smarty-version'
     Menu: Smarty -> Version
     Keybinding: `C-c C-v'
     Description: This function displays in the minibuffer the
     current Smarty Mode version with the timestamp.

4.2 Sources
===========

The Sources menu shows the Smarty files in the current directory. If
you add or delete a file in the current directory, you need to
refresh the menu.

4.2.1 Customization
-------------------

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

4.2.2 Functions
---------------

`smarty-add-source-files-menu'
     Menu: Sources -> *Rescan*
     Keybinding: `C-c C-s C-u'
     Description: This function is used to refresh the Sources menu.

5 Stuttering
************

The stutter mode is a mode that affects a function to a key. For
example, when you use the `ENTER' key, the associated function will
create a new line and indent it.

5.1 Customization
=================

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

5.2 Functions
=============

`SPACE'
     If in comment, indent the comment and add new line if necessary.
     In other case, add a space.

`('
     If the previous character is a `(', the `((' will be replaced by
     `['.
     If the previous character is a `[', the `[(' will be replaced by
     `{'.
     In other case, insert a `('.

`)'
     If the previous character is a `)', the `))' will be replaced by
     `]'.
     If the previous character is a `]', the `])' will be replaced by
     `}'.
     In other case, insert a `)'.

6 Templates
***********

In the Smarty Mode, the Smarty functions (like if, while, for, fopen,
fclose) are predefined in functions called \"Templates\".

Each template can be invoked by the function name or by using the
<SPACE> key after the Smarty function name in the buffer (Note, using
`M-<SPACE>' disable the template).

A template can be aborted by using the `C-g' or by lefting empty the
tempate prompt (in the minibuffer).

6.1 Customization
=================

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

For a complete description of the template customizable variables,
see *Note Cu01-Pa01-Template::

6.2 Functions
=============

6.2.1 Smarty Functions
----------------------

For Smarty functions, see PDF or HTML documentation.

6.2.2 Non-Smarty Functions
--------------------------

`smarty-template-header'
     Menu: Smarty -> Templates -> Insert Header
     Keybinding: `C-c C-t C-h'
     Description: This function is used to insert a header in the
     current buffer.

`smarty-template-footer'
     Menu: Smarty -> Templates -> Insert Footer
     Keybinding: `C-c C-t C-f'
     Description: This function is used to insert a footer in the
     current buffer.

`smarty-template-insert-date'
     Menu: Smarty -> Templates -> Insert Date
     Keybinding: `C-c C-t C-d i'
     Description: This function is used to insert the date in the
     current buffer.

`smarty-template-modify'
     Menu: Smarty -> Templates -> Modify Date
     Keybinding: `C-c C-t C-d m'
     Description: This function is used to modify the last
     modification date in the current buffer.

7 Bugs, Help
************

   * To report bugs: Bugtracker
     (http://bugtracker.morinie.fr/lisp/set_project.php?project_id=2)

   * To obtain help you can post on the dedicated forum: Forum
     (http://forum.morinie.fr/lisp/)

8 Key bindings
**************

\\{smarty-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns php) "php-mode" "related/php-mode.el"
;;;;;;  (19218 42180))
;;; Generated autoloads from related/php-mode.el
(web-autoload-require 'php-mode 'lp '(nxhtml-download-root-url nil) "related/php-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(nxhtml-custom-autoload 'php-file-patterns 'php-mode nil)

(nxhtml-autoload 'php-mode `(lp '(nxhtml-download-root-url nil) "related/php-mode" nxhtml-install-dir) "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (global-mozadd-mirror-mode mozadd-mirror-mode global-mozadd-refresh-edited-on-save-mode
;;;;;;  mozadd-refresh-edited-on-save-mode) "mozadd" "related/mozadd.el"
;;;;;;  (19235 1650))
;;; Generated autoloads from related/mozadd.el
(web-autoload-require 'mozadd 'lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'mozadd-refresh-edited-on-save-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Refresh mozadd edited file in Firefox when saving file.
The mozadd edited file is the file in the last buffer visited in
`mozadd-mirror-mode'.

You can use this for example when you edit CSS files.

The mozadd edited file must be shown in Firefox and visible.

\(fn &optional ARG)" t nil)

(defvar global-mozadd-refresh-edited-on-save-mode nil "\
Non-nil if Global-Mozadd-Refresh-Edited-On-Save mode is enabled.
See the command `global-mozadd-refresh-edited-on-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-mozadd-refresh-edited-on-save-mode'.")

(nxhtml-custom-autoload 'global-mozadd-refresh-edited-on-save-mode 'mozadd nil)

(nxhtml-autoload 'global-mozadd-refresh-edited-on-save-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Toggle Mozadd-Refresh-Edited-On-Save mode in every possible buffer.
With prefix ARG, turn Global-Mozadd-Refresh-Edited-On-Save mode on if and only if
ARG is positive.
Mozadd-Refresh-Edited-On-Save mode is enabled in all buffers where
`(lambda nil (when (or (derived-mode-p (quote css-mode)) (mozadd-html-buffer-file-p)) (mozadd-refresh-edited-on-save-mode 1)))' would do it.
See `mozadd-refresh-edited-on-save-mode' for more information on Mozadd-Refresh-Edited-On-Save mode.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'mozadd-mirror-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Mirror content of current file buffer immediately in Firefox.
When you turn on this mode the file will be opened in Firefox.
Every change you make in the buffer will trigger a redraw in
Firefox - regardless of if you save the file or not.

For the mirroring to work the edited file must be shown in
Firefox and visible.

If `nxml-where-mode' is on the marks will also be shown in
Firefox as CSS outline style.  You can customize the style
through the option `mozadd-xml-path-outline-style'.

See also `mozadd-refresh-edited-on-save-mode'.

\(fn &optional ARG)" t nil)

(defvar global-mozadd-mirror-mode nil "\
Non-nil if Global-Mozadd-Mirror mode is enabled.
See the command `global-mozadd-mirror-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-mozadd-mirror-mode'.")

(nxhtml-custom-autoload 'global-mozadd-mirror-mode 'mozadd nil)

(nxhtml-autoload 'global-mozadd-mirror-mode `(lp '(nxhtml-download-root-url nil) "related/mozadd" nxhtml-install-dir) "\
Toggle Mozadd-Mirror mode in every possible buffer.
With prefix ARG, turn Global-Mozadd-Mirror mode on if and only if
ARG is positive.
Mozadd-Mirror mode is enabled in all buffers where
`(lambda nil (when (mozadd-html-buffer-file-p) (mozadd-mirror-mode 1)))' would do it.
See `mozadd-mirror-mode' for more information on Mozadd-Mirror mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (inferior-moz-mode moz-minor-mode) "moz" "related/moz.el"
;;;;;;  (19048 2102))
;;; Generated autoloads from related/moz.el
(web-autoload-require 'moz 'lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'moz-minor-mode `(lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir) "\
MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area (as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'inferior-moz-mode `(lp '(nxhtml-download-root-url nil) "related/moz" nxhtml-install-dir) "\
Major mode for interacting with Firefox via MozRepl.

\(fn)" t nil)

;;;***

;;;### (autoloads (iss-mumamo-mode) "iss-mumamo" "related/iss-mumamo.el"
;;;;;;  (19294 54042))
;;; Generated autoloads from related/iss-mumamo.el
(web-autoload-require 'iss-mumamo 'lp '(nxhtml-download-root-url nil) "related/iss-mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'iss-mumamo-mode `(lp '(nxhtml-download-root-url nil) "related/iss-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes Inno Setup .iss files.
The main major mode will be `iss-mode'.
The [code] section, if any, will be in `pascal-mode'." t)

;;;***

;;;### (autoloads (iss-mode) "iss-mode" "related/iss-mode.el" (19294
;;;;;;  54042))
;;; Generated autoloads from related/iss-mode.el
(web-autoload-require 'iss-mode 'lp '(nxhtml-download-root-url nil) "related/iss-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'iss-mode `(lp '(nxhtml-download-root-url nil) "related/iss-mode" nxhtml-install-dir) "\
Major mode for editing InnoSetup script files. Upon startup iss-mode-hook is run.

\(fn)" t nil)

;;;***

;;;### (autoloads (flymake-js-load flymake-js) "flymake-js" "related/flymake-js.el"
;;;;;;  (19218 42180))
;;; Generated autoloads from related/flymake-js.el
(web-autoload-require 'flymake-js 'lp '(nxhtml-download-root-url nil) "related/flymake-js" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'flymake-js 'custom-loads))) (if (member '"flymake-js" loads) nil (put 'flymake-js 'custom-loads (cons '"flymake-js" loads))))

(nxhtml-autoload 'flymake-js-load `(lp '(nxhtml-download-root-url nil) "related/flymake-js" nxhtml-install-dir) "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (flymake-java-1-load) "flymake-java-1" "related/flymake-java-1.el"
;;;;;;  (19264 27004))
;;; Generated autoloads from related/flymake-java-1.el
(web-autoload-require 'flymake-java-1 'lp '(nxhtml-download-root-url nil) "related/flymake-java-1" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'flymake-java-1-load `(lp '(nxhtml-download-root-url nil) "related/flymake-java-1" nxhtml-install-dir) "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (flymake-css-load) "flymake-css" "related/flymake-css.el"
;;;;;;  (19292 11678))
;;; Generated autoloads from related/flymake-css.el
(web-autoload-require 'flymake-css 'lp '(nxhtml-download-root-url nil) "related/flymake-css" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'flymake-css-load `(lp '(nxhtml-download-root-url nil) "related/flymake-css" nxhtml-install-dir) "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (django-mode) "django" "related/django.el" (19411
;;;;;;  8520))
;;; Generated autoloads from related/django.el
(web-autoload-require 'django 'lp '(nxhtml-download-root-url nil) "related/django" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'django-mode `(lp '(nxhtml-download-root-url nil) "related/django" nxhtml-install-dir) "\
Simple Django mode for use with mumamo.
This mode only provides syntax highlighting.

\(fn)" t nil)

;;;***

;;;### (autoloads (csharp-mode csharp-mode-hook) "csharp-mode" "related/csharp-mode.el"
;;;;;;  (19410 9973))
;;; Generated autoloads from related/csharp-mode.el
(web-autoload-require 'csharp-mode 'lp '(nxhtml-download-root-url nil) "related/csharp-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(nxhtml-custom-autoload 'csharp-mode-hook 'csharp-mode t)

(nxhtml-autoload 'csharp-mode `(lp '(nxhtml-download-root-url nil) "related/csharp-mode" nxhtml-install-dir) "\
Major mode for editing C# code. This mode is derived from CC Mode to
support C#.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

This mode will automatically add a regexp for Csc.exe error and warning
messages to the `compilation-error-regexp-alist'.

Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (winring-rename-configuration winring-delete-configuration
;;;;;;  winring-jump-to-configuration winring-prev-configuration
;;;;;;  winring-next-configuration winring-duplicate-configuration
;;;;;;  winring-new-configuration) "winring" "util/winring.el" (19392
;;;;;;  30980))
;;; Generated autoloads from util/winring.el
(web-autoload-require 'winring 'lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'winring-new-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'winring-duplicate-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'winring-next-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Switch to the next window configuration for this frame.

\(fn)" t nil)

(nxhtml-autoload 'winring-prev-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Switch to the previous window configuration for this frame.

\(fn)" t nil)

(nxhtml-autoload 'winring-jump-to-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Go to the named window configuration.

\(fn)" t nil)

(nxhtml-autoload 'winring-delete-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'winring-rename-configuration `(lp '(nxhtml-download-root-url nil) "util/winring" nxhtml-install-dir) "\
Rename the current configuration to NAME.

\(fn)" t nil)

;;;***

;;;### (autoloads (winsav-switch-config winsav-save-full-config winsav-save-mode
;;;;;;  winsav winsav-put-window-tree) "winsav" "util/winsav.el"
;;;;;;  (19295 38082))
;;; Generated autoloads from util/winsav.el
(web-autoload-require 'winsav 'lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'winsav-put-window-tree `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Put window structure SAVED-TREE into WINDOW.
Restore a structure SAVED-TREE returned from
`winsav-get-window-tree' into window WINDOW.

If COPY-WIN-OVL is non-nil then overlays having a 'window
property pointing to one of the windows in SAVED-TREE where this
window still is shown will be copied to a new overlay with
'window property pointing to the corresponding new window.

If WIN-OVL-ALL-BUFS is non-nil then all buffers will be searched
for overlays with a 'window property of the kind above.

At the very end of this function the hook `winsav-after-put' is
run.

\(fn SAVED-TREE WINDOW &optional COPY-WIN-OVL WIN-OVL-ALL-BUFS)" nil nil)

(let ((loads (get 'winsav 'custom-loads))) (if (member '"winsav" loads) nil (put 'winsav 'custom-loads (cons '"winsav" loads))))

(defvar winsav-save-mode nil "\
Non-nil if Winsav-Save mode is enabled.
See the command `winsav-save-mode' for a description of this minor mode.")

(nxhtml-custom-autoload 'winsav-save-mode 'winsav nil)

(nxhtml-autoload 'winsav-save-mode `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Toggle winsav configuration saving mode.
With numeric ARG, turn winsav saving on if ARG is positive, off
otherwise.

When this mode is turned on, winsav configurations are saved from
one session to another.  A winsav configuration consists of
frames, windows and visible buffers configurations plus
optionally buffers and files managed by the functions used by
option `desktop-save-mode'

By default this is integrated with `desktop-save-mode'.  If
`desktop-save-mode' is on and `winsav-handle-also-desktop' is
non-nil then save and restore also desktop.

See the command `winsav-switch-config' for more information and
other possibilities.

Note: If you want to avoid saving when you exit just turn off
this minor mode.

For information about what is saved and restored and how to save
and restore additional information see the function
`winsav-save-configuration'.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'winsav-save-full-config `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Saved current winsav configuration in directory DIRNAME.
Then change to this configuration.

See also `winsav-switch-config'.

\(fn DIRNAME)" nil nil)

(nxhtml-autoload 'winsav-switch-config `(lp '(nxhtml-download-root-url nil) "util/winsav" nxhtml-install-dir) "\
Change to winsav configuration in directory DIRNAME.
If DIRNAME is the current winsav configuration directory then
offer to save it or restore it from saved values.

Otherwise, before switching offer to save the current winsav
configuration.  Then finally switch to the new winsav
configuration, creating it if it does not exist.

If option `desktop-save-mode' is on then buffers and files are also
restored and saved the same way.

See also option `winsav-save-mode' and command
`winsav-tell-configuration'.

\(fn DIRNAME)" t nil)

;;;***

;;;### (autoloads (winsav-rotate winsize-set-mode-line-colors winsize-save-window-configuration
;;;;;;  winsize-balance-siblings resize-windows) "winsize" "util/winsize.el"
;;;;;;  (19292 49706))
;;; Generated autoloads from util/winsize.el
(web-autoload-require 'winsize 'lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'resize-windows `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Start window resizing.
During resizing a window is selected.  You can move its
borders. In the default configuration the arrow keys moves the
right or bottom border if they are there. To move the opposite
border use S-arrowkeys.

You can also do other window operations, like splitting, deleting
and balancing the sizes.  The keybindings below describes the key
bindings during resizing:\\<winsize-keymap>

  `balance-windows'                      \\[balance-windows]
  `winsize-balance-siblings'             \\[winsize-balance-siblings]
  `fit-window-to-buffer'                 \\[fit-window-to-buffer]
  `shrink-window-if-larger-than-buffer'  \\[shrink-window-if-larger-than-buffer]

  `winsav-rotate'                        \\[winsav-rotate]

  `winsize-move-border-up'      \\[winsize-move-border-up]
  `winsize-move-border-down'    \\[winsize-move-border-down]
  `winsize-move-border-left'    \\[winsize-move-border-left]
  `winsize-move-border-right'   \\[winsize-move-border-right]

  `winsize-to-border-or-window-left'    \\[winsize-to-border-or-window-left]
  `winsize-to-border-or-window-up'      \\[winsize-to-border-or-window-up]
  `winsize-to-border-or-window-right'   \\[winsize-to-border-or-window-right]
  `winsize-to-border-or-window-down'    \\[winsize-to-border-or-window-down]

   Note that you can also use your normal keys for
   `forward-char', `backward-char', `next-line', `previous-line'
   and what you have on HOME and END to move in the windows. That
   might sometimes be necessary to directly select a
   window. (You may however also use `other-window' or click
   with the mouse, see below.)

  `delete-window'                \\[delete-window]
  `delete-other-windows'         \\[delete-other-windows]
  `split-window-vertically'      \\[split-window-vertically]
  `split-window-horizontally'    \\[split-window-horizontally]
  `other-window'                 \\[other-window]

  `winsize-save-window-configuration'       \\[winsize-save-window-configuration]
  `winsize-next-window-configuration'       \\[winsize-next-window-configuration]
  `winsize-previous-window-configuration'   \\[winsize-previous-window-configuration]

  `mouse-set-point'   \\[mouse-set-point]

  `winsize-quit'               \\[winsize-quit]
  `winsize-stop-go-back'       \\[winsize-stop-go-back]
  `winsize-stop'               \\[winsize-stop]
  `winsize-stop-and-execute'   \\[winsize-stop-and-execute]

  `winsize-help'          \\[winsize-help]
  `describe-key'          \\[describe-key]
  `describe-key-briefly'  \\[describe-key-briefly]
  (All the normal help keys work, and at least those above will
  play well with resizing.)

Nearly all other keys exits window resizing and they are also
executed.  However, the key sequences in `winsize-let-me-use' and
dito for commands there are also executed without exiting
resizing.

The colors of the modelines are changed to those given in
`winsize-mode-line-colors' to indicate that you are resizing
windows.  To make this indication more prominent the text in the
selected window is marked with the face hold in the variable
`winsize-selected-window-face'.

The option `winsize-juris-way' decides how the borders to move
are selected. If this option is non-nil then the right or bottom
border are the ones that are moved with the arrow keys and the
opposite border with shift arrow keys.

If `winsize-juris-way' is nil then the following apply:

As you select other borders or move to new a window the mouse
pointer is moved inside the selected window to show which borders
are beeing moved. The mouse jumps a little bit to make its
position more visible. You can turn this off by customizing
`winsize-make-mouse-prominent'.

Which borders initially are choosen are controlled by the
variable `winsize-autoselect-borders'.

** Example: Border selection, movements and windows.

  Suppose you have a frame divided into windows like in the
  figure below.  If window B is selected when you start resizing
  then (with default settings) the borders marked with 'v' and
  'h' will be the ones that the arrow keys moves. To indicate
  this the mouse pointer is placed in the right lower corner of
  the selected window B.

    +----------+-----------+--------+
    |          |           v        |
    |          |           v        |
    |    A     |    _B_    v        |
    |          |           v        |
    |          |           v        |
    |          |         x v        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Now if you press M-<left> then the picture below shows what has
  happened. Note that the selected vertical border is now the one
  between A and B. The mouse pointer has moved to the
  corresponding corner in the window B, which is still selected.

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |    A     v    _B_    |        |
    |          v           |        |
    |          v           |        |
    |          v x         |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Press M-<left> once again. This gives this picture:

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |   _A_    v     B     |        |
    |          v           |        |
    |          v           |        |
    |        x v           |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Note that the window A is now selected. However there is no
  border that could be moved to the left of this window (which
  would otherwise be chosen now) so the border between A and B is
  still the one that <left> and <right> moves. The mouse has
  moved to A.

  If we now delete window A the new situation will look like
  this:

    +----------+-----------+--------+
    |                      |        |
    |                      |        |
    |         _B_          |        |
    |                      |        |
    |                      |        |
    |                    x |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+



>>>> testing stuff >>>>
`help-mode-hook'
`temp-buffer-show-function'
`view-exit-action'
<<<<<<<<<<<<<<<<<<<<<<<

\(fn)" t nil)

(nxhtml-autoload 'winsize-balance-siblings `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Make current window siblings the same height or width.
It works the same way as `balance-windows', but only for the
current window and its siblings.

\(fn)" t nil)

(nxhtml-autoload 'winsize-save-window-configuration `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'winsize-set-mode-line-colors `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Turn mode line colors on if ON is non-nil, otherwise off.

\(fn ON)" nil nil)

(nxhtml-autoload 'winsav-rotate `(lp '(nxhtml-download-root-url nil) "util/winsize" nxhtml-install-dir) "\
Rotate window configuration on selected frame.
MIRROR should be either 'mirror-left-right, 'mirror-top-bottom or
nil.  In the first case the window configuration is mirrored
vertically and in the second case horizontally.  If MIRROR is nil
the configuration is not mirrored.

If TRANSPOSE is non-nil then the window structure is transposed
along the diagonal from top left to bottom right (in analogy with
matrix transosition).

If called interactively MIRROR will is 'mirror-left-right by
default, but 'mirror-top-bottom if called with prefix.  TRANSPOSE
is t. This mean that the window configuration will be turned one
quarter clockwise (or counter clockwise with prefix).

\(fn MIRROR TRANSPOSE)" t nil)

;;;***

;;;### (autoloads (wrap-to-fill-column-mode wrap-to-fill-left-marg-modes
;;;;;;  wrap-to-fill-left-marg wrap-to-fill) "wrap-to-fill" "util/wrap-to-fill.el"
;;;;;;  (19306 50510))
;;; Generated autoloads from util/wrap-to-fill.el
(web-autoload-require 'wrap-to-fill 'lp '(nxhtml-download-root-url nil) "util/wrap-to-fill" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'wrap-to-fill 'custom-loads))) (if (member '"wrap-to-fill" loads) nil (put 'wrap-to-fill 'custom-loads (cons '"wrap-to-fill" loads))))

(defvar wrap-to-fill-left-marg nil "\
Left margin handling for `wrap-to-fill-column-mode'.
Used by `wrap-to-fill-column-mode'. If nil then center the
display columns. Otherwise it should be a number which will be
the left margin.")

(nxhtml-custom-autoload 'wrap-to-fill-left-marg 'wrap-to-fill t)

(defvar wrap-to-fill-left-marg-modes '(text-mode fundamental-mode) "\
Major modes where `wrap-to-fill-left-margin' may be nil.")

(nxhtml-custom-autoload 'wrap-to-fill-left-marg-modes 'wrap-to-fill t)

(nxhtml-autoload 'wrap-to-fill-column-mode `(lp '(nxhtml-download-root-url nil) "util/wrap-to-fill" nxhtml-install-dir) "\
Use `fill-column' display columns in buffer windows.
By default the display columns are centered, but see the option
`wrap-to-fill-left-marg'.

Fix-me:
Note 1: When turning this on `visual-line-mode' is also turned on. This
is not reset when turning off this mode.

Note 2: The text properties 'wrap-prefix and 'wrap-to-fill-prefix
is set by this mode to indent continuation lines.

Key bindings added by this minor mode:

\\{wrap-to-fill-column-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (xhtml-help xhtml-help-show-tag-ref xhtml-help-tag-at-point
;;;;;;  xhtml-help-show-css-ref) "xhtml-help" "nxhtml/xhtml-help.el"
;;;;;;  (19364 56214))
;;; Generated autoloads from nxhtml/xhtml-help.el
(web-autoload-require 'xhtml-help 'lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'xhtml-help-show-css-ref `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Show CSS reference for CSS property name at point.

\(fn)" t nil)

(nxhtml-autoload 'xhtml-help-tag-at-point `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Get xhtml tag name at or before point.

\(fn)" nil nil)

(nxhtml-autoload 'xhtml-help-show-tag-ref `(lp '(nxhtml-download-root-url nil) "nxhtml/xhtml-help" nxhtml-install-dir) "\
Show xhtml reference for tag name at or before point.

\(fn)" t nil)

(let ((loads (get 'xhtml-help 'custom-loads))) (if (member '"xhtml-help" loads) nil (put 'xhtml-help 'custom-loads (cons '"xhtml-help" loads))))

;;;***

;;;### (autoloads (tidy-build-menu tidy) "tidy-xhtml" "nxhtml/tidy-xhtml.el"
;;;;;;  (19364 56214))
;;; Generated autoloads from nxhtml/tidy-xhtml.el
(web-autoload-require 'tidy-xhtml 'lp '(nxhtml-download-root-url nil) "nxhtml/tidy-xhtml" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'tidy 'custom-loads))) (if (member '"tidy-xhtml" loads) nil (put 'tidy 'custom-loads (cons '"tidy-xhtml" loads))))

(nxhtml-autoload 'tidy-build-menu `(lp '(nxhtml-download-root-url nil) "nxhtml/tidy-xhtml" nxhtml-install-dir) "\
Set up the tidy menu in MAP.
Used to set up a Tidy menu in your favourite mode.

\(fn &optional MAP)" t nil)

;;;***

;;;### (autoloads (rngalt-set-validation-header) "rngalt" "nxhtml/rngalt.el"
;;;;;;  (19365 33760))
;;; Generated autoloads from nxhtml/rngalt.el
(web-autoload-require 'rngalt 'lp '(nxhtml-download-root-url nil) "nxhtml/rngalt" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'rngalt-set-validation-header `(lp '(nxhtml-download-root-url nil) "nxhtml/rngalt" nxhtml-install-dir) "\
Not documented

\(fn START-OF-DOC)" nil nil)

;;;***

;;;### (autoloads (nxml-where-global-mode nxml-where-mode nxml-where)
;;;;;;  "nxml-where" "nxhtml/nxml-where.el" (19365 33760))
;;; Generated autoloads from nxhtml/nxml-where.el
(web-autoload-require 'nxml-where 'lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'nxml-where 'custom-loads))) (if (member '"nxml-where" loads) nil (put 'nxml-where 'custom-loads (cons '"nxml-where" loads))))

(nxhtml-autoload 'nxml-where-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir) "\
Shows path in mode line.

\(fn &optional ARG)" t nil)

(defvar nxml-where-global-mode nil "\
Non-nil if Nxml-Where-Global mode is enabled.
See the command `nxml-where-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nxml-where-global-mode'.")

(nxhtml-custom-autoload 'nxml-where-global-mode 'nxml-where nil)

(nxhtml-autoload 'nxml-where-global-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxml-where" nxhtml-install-dir) "\
Toggle Nxml-Where mode in every possible buffer.
With prefix ARG, turn Nxml-Where-Global mode on if and only if
ARG is positive.
Nxml-Where mode is enabled in all buffers where
`nxml-where-turn-on-in-nxml-child' would do it.
See `nxml-where-mode' for more information on Nxml-Where mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (nxhtml-features-check nxhtml-customize nxhtml)
;;;;;;  "nxhtml" "nxhtml/nxhtml.el" (19412 25954))
;;; Generated autoloads from nxhtml/nxhtml.el
(web-autoload-require 'nxhtml 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'nxhtml 'custom-loads))) (if (member '"nxhtml" loads) nil (put 'nxhtml 'custom-loads (cons '"nxhtml" loads))))

(nxhtml-autoload 'nxhtml-customize `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir) "\
Customize nXhtml.

\(fn)" t nil)

(nxhtml-autoload 'nxhtml-features-check `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml" nxhtml-install-dir) "\
Check if external modules used by nXhtml are found.

\(fn)" t nil)

;;;***

;;;### (autoloads (mako-nxhtml-mumamo-mode asp-nxhtml-mumamo-mode
;;;;;;  eruby-nxhtml-mumamo-mode jsp-nxhtml-mumamo-mode gsp-nxhtml-mumamo-mode
;;;;;;  smarty-nxhtml-mumamo-mode mjt-nxhtml-mumamo-mode genshi-nxhtml-mumamo-mode
;;;;;;  mason-nxhtml-mumamo-mode django-nxhtml-mumamo-mode embperl-nxhtml-mumamo-mode
;;;;;;  nxhtml-mumamo-mode) "nxhtml-mumamo" "nxhtml/nxhtml-mumamo.el"
;;;;;;  (19389 57826))
;;; Generated autoloads from nxhtml/nxhtml-mumamo.el
(web-autoload-require 'nxhtml-mumamo 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for (X)HTML with main mode `nxhtml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'." t)

(nxhtml-autoload 'embperl-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Embperl files with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'django-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Django with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'mason-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Mason using main mode `nxhtml-mode'.
This covers inlined style and javascript." t)

(nxhtml-autoload 'genshi-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Genshi with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'mjt-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for MJT with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'smarty-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Smarty with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'gsp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for GSP with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'jsp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for JSP with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'eruby-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for eRuby with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'asp-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for ASP with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(nxhtml-autoload 'mako-nxhtml-mumamo-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mumamo" nxhtml-install-dir) "\
Turn on multiple major modes for Mako with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

;;;***

;;;### (autoloads (nxhtml-validation-header-mode nxhtml-short-tag-help
;;;;;;  nxhtml-mode) "nxhtml-mode" "nxhtml/nxhtml-mode.el" (19412
;;;;;;  25947))
;;; Generated autoloads from nxhtml/nxhtml-mode.el
(web-autoload-require 'nxhtml-mode 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(when (fboundp 'nxml-mode)
(nxhtml-autoload 'nxhtml-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
Major mode for editing XHTML documents.
It is based on `nxml-mode' and adds some features that are useful
when editing XHTML files.\\<nxhtml-mode-map>

To see an overview in html format do \\[nxhtml-overview].

* Note: Please observe that when loading nXhtml some file
  associations are done, see `nxhtml-setup-file-assoc'.

The nXhtml menu is added by this mode (or actually the minor
mode `nxhtml-menu-mode') and gives quick access and an overview
of some other important features. These includes:

- multiple major modes, see `define-mumamo-multi-major-mode'
- easy uploading and viewing of files, see for example
  `html-upl-upload-file'
- validation in XHTML part for php etc, see
  `nxhtml-validation-header-mode' (you probably also want to know about
  `nxhtml-toggle-visible-warnings' for this!)
- converting of html to xhtml, see `tidy-buffer'

The XML menu contains functionality added by `nxml-mode' (on
which this major mode is based).  There is also a popup menu
added to the [apps] key.

The most important features are probably completion and
validation, which is inherited from `nxml-mode' with some small
addtions.  In very many situation you can use completion. To
access it type \\[nxml-complete]. Completion has been enhanced in
the following way:

- If region is active and visible then completion will surround the
  region with the chosen tag's start and end tag.  However only the
  starting point is checked for validity. If something is wrong after
  insertion you will however immediately see it if you have validation
  on.
- It can in some cases give assistance with attribute values.
- Completion can be customized, see the menus XHTML - Completion:
  * You can use a menu popup style completion.
  * You can have alternatives grouped.
  * You can get a short help text shown for each alternative.
- There does not have to be a '<' before point for tag name
  completion. (`nxml-mode' requires a '<' before point for tag name
  completion.)
- Completes xml version and encoding.
- Completes in an empty buffer, ie inserts a skeleton.

Some smaller, useful, but easy-to-miss features:

* Following links. The href and src attribute names are
  underlined and a special keymap is bound to
  them:\\<mlinks-mode-map>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes

    \\[mlinks-goto], Mouse-1 Follow link inside Emacs
        (if possible)

  It is even a little bit quicker when the links are in an active
  state (marked with the face `isearch'):\\<mlinks-active-hilight-keymap>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes
    \\[mlinks-goto], Mouse-1  Follow link inside Emacs (if possible)

  If the link is not into a file that you can edit (a mailto link
  for example) you will be prompted for an alternative action.

* Creating links. To make it easier to create links to id/name
  attribute in different files there are two special
  functions:\\<nxhtml-mode-map>

    \\[nxhtml-save-link-to-here] copy link to id/name (you must
        be in the tag to get the link)
    \\[nxhtml-paste-link-as-a-tag] paste this as an a-tag.

Here are all key bindings in nxhtml-mode itself:

\\{nxhtml-mode-map}

The minor mode `nxhtml-menu-mode' adds some bindings:

\\{nxhtml-menu-mode-map}

Notice that other minor mode key bindings may also be active, as
well as emulation modes. Do \\[describe-bindings] to get a list
of all active key bindings. Also, *VERY IMPORTANT*, if mumamo is
used in the buffer each mumamo chunk has a different major mode
with different key bindings. You can however still see all
bindings with \\[describe-bindings], but you have to do that with
point in the mumamo chunk you want to know the key bindings in.

---------
* Note: Some of the features supported by this mode are optional
  and available only if other Emacs modules are found.  Use
  \\[nxhtml-features-check] to get a list of these optional
  features and modules needed. You should however have no problem
  with this if you have followed the installation instructions
  for nXhtml.

\(fn)" t nil))

(nxhtml-autoload 'nxhtml-short-tag-help `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
Display description of tag TAG.  If TAG is omitted, try tag at point.

\(fn TAG)" t nil)

(when (fboundp 'nxml-mode)
(nxhtml-autoload 'nxhtml-validation-header-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-mode" nxhtml-install-dir) "\
If on use a Fictive XHTML Validation Header for the buffer.
See `nxhtml-set-validation-header' for information about Fictive XHTML Validation Headers.

This mode may be turned on automatically in two ways:
- If you try to do completion of a XHTML tag or attribute then
  `nxthml-mode' may ask you if you want to turn this mode on if
  needed.
- You can also choose to have it turned on automatically whenever
  a mumamo multi major mode is used, see
  `nxhtml-validation-header-if-mumamo' for further information.

\(fn &optional ARG)" t nil))

;;;***

;;;### (autoloads (nxhtml-overview nxhtml-menu-mode nxhtml-browse-region
;;;;;;  nxhtml-browse-file nxhtml-edit-with-gimp) "nxhtml-menu" "nxhtml/nxhtml-menu.el"
;;;;;;  (19412 26360))
;;; Generated autoloads from nxhtml/nxhtml-menu.el
(web-autoload-require 'nxhtml-menu 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-edit-with-gimp `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Edit with GIMP buffer or file at point.

\(fn)" t nil)

(nxhtml-autoload 'nxhtml-browse-file `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
View file in web browser.

\(fn FILE)" t nil)

(nxhtml-autoload 'nxhtml-browse-region `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
View region in web browser.

\(fn)" t nil)

(defvar nxhtml-menu-mode nil "\
Non-nil if Nxhtml-Menu mode is enabled.
See the command `nxhtml-menu-mode' for a description of this minor mode.")

(nxhtml-custom-autoload 'nxhtml-menu-mode 'nxhtml-menu nil)

(nxhtml-autoload 'nxhtml-menu-mode `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Minor mode to turn on some key and menu bindings.
See `nxhtml-mode' for more information.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'nxhtml-overview `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-menu" nxhtml-install-dir) "\
Show a HTML page with an overview of nXhtml.

\(fn)" t nil)

;;;***

;;;### (autoloads (nxhtml-report-bug) "nxhtml-bug" "nxhtml/nxhtml-bug.el"
;;;;;;  (19277 65354))
;;; Generated autoloads from nxhtml/nxhtml-bug.el
(web-autoload-require 'nxhtml-bug 'lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-bug" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-report-bug `(lp '(nxhtml-download-root-url nil) "nxhtml/nxhtml-bug" nxhtml-install-dir) "\
Report a bug in nXhtml.

\(fn)" t nil)

;;;***

;;;### (autoloads (html-wtoc) "html-wtoc" "nxhtml/html-wtoc.el" (19364
;;;;;;  56214))
;;; Generated autoloads from nxhtml/html-wtoc.el
(web-autoload-require 'html-wtoc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-wtoc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-wtoc 'custom-loads))) (if (member '"html-wtoc" loads) nil (put 'html-wtoc 'custom-loads (cons '"html-wtoc" loads))))

;;;***

;;;### (autoloads (html-upl-ediff-file html-upl-edit-remote-file-with-toc
;;;;;;  html-upl-edit-remote-file html-upl-upload-file html-upl-remote-dired
;;;;;;  html-upl-upload-site html-upl-upload-site-with-toc html-upl)
;;;;;;  "html-upl" "nxhtml/html-upl.el" (19364 56214))
;;; Generated autoloads from nxhtml/html-upl.el
(web-autoload-require 'html-upl 'lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-upl 'custom-loads))) (if (member '"html-upl" loads) nil (put 'html-upl 'custom-loads (cons '"html-upl" loads))))

(nxhtml-autoload 'html-upl-upload-site-with-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'html-upl-upload-site `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'html-upl-remote-dired `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Start dired for remote directory or its parent/ancestor.

\(fn DIRNAME)" t nil)

(nxhtml-autoload 'html-upl-upload-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Upload a single file in a site.
For the definition of a site see `html-site-current'.

\(fn FILENAME)" t nil)

(nxhtml-autoload 'html-upl-edit-remote-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'html-upl-edit-remote-file-with-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'html-upl-ediff-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-upl" nxhtml-install-dir) "\
Run ediff on local and remote file.
FILENAME could be either the remote or the local file.

\(fn FILENAME)" t nil)

;;;***

;;;### (autoloads (html-toc) "html-toc" "nxhtml/html-toc.el" (19364
;;;;;;  56214))
;;; Generated autoloads from nxhtml/html-toc.el
(web-autoload-require 'html-toc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-toc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-toc 'custom-loads))) (if (member '"html-toc" loads) nil (put 'html-toc 'custom-loads (cons '"html-toc" loads))))

(defconst html-toc-menu-map (let ((map (make-sparse-keymap))) (define-key map [html-toc-browse-frames-file] (list 'menu-item "Browse Frames File" 'html-toc-browse-frames-file)) (define-key map [html-toc-write-frames-file] (list 'menu-item "Write Frames File" 'html-toc-write-frames-file)) (define-key map [html-toc-write-toc-file] (list 'menu-item "Write TOC File for Frames" 'html-toc-write-toc-file)) (define-key map [html-toc-sep1] (list 'menu-item "--")) (define-key map [html-toc-edit-pages-file] (list 'menu-item "Edit List of Pages for TOC" 'html-site-edit-pages-file)) (define-key map [html-toc-create-pages-file] (list 'menu-item "Write List of Pages for TOC" 'html-toc-create-pages-file)) map))

;;;***

;;;### (autoloads (html-site-query-replace html-site-rgrep html-site-find-file
;;;;;;  html-site-dired-current html-site-set-site html-site-buffer-or-dired-file-name
;;;;;;  html-site) "html-site" "nxhtml/html-site.el" (19364 56214))
;;; Generated autoloads from nxhtml/html-site.el
(web-autoload-require 'html-site 'lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-site 'custom-loads))) (if (member '"html-site" loads) nil (put 'html-site 'custom-loads (cons '"html-site" loads))))

(nxhtml-autoload 'html-site-buffer-or-dired-file-name `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Return buffer file name or file pointed to in dired.

\(fn)" nil nil)

(nxhtml-autoload 'html-site-set-site `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Not documented

\(fn NAME)" t nil)

(nxhtml-autoload 'html-site-dired-current `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Open `dired' in current site top directory.

\(fn)" t nil)

(nxhtml-autoload 'html-site-find-file `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Find file in current site.

\(fn)" t nil)

(nxhtml-autoload 'html-site-rgrep `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Search current site's files with `rgrep'.
See `rgrep' for the arguments REGEXP and FILES.

\(fn REGEXP FILES)" t nil)

(nxhtml-autoload 'html-site-query-replace `(lp '(nxhtml-download-root-url nil) "nxhtml/html-site" nxhtml-install-dir) "\
Query replace in current site's files.

\(fn FROM TO FILE-REGEXP DELIMITED)" t nil)

;;;***

;;;### (autoloads (html-pagetoc-rebuild-toc html-pagetoc-insert-toc
;;;;;;  html-pagetoc) "html-pagetoc" "nxhtml/html-pagetoc.el" (19364
;;;;;;  56214))
;;; Generated autoloads from nxhtml/html-pagetoc.el
(web-autoload-require 'html-pagetoc 'lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-pagetoc 'custom-loads))) (if (member '"html-pagetoc" loads) nil (put 'html-pagetoc 'custom-loads (cons '"html-pagetoc" loads))))

(nxhtml-autoload 'html-pagetoc-insert-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir) "\
Inserts a table of contents for the current html file.
The html header tags h1-h6 found in the file are inserted into
this table.  MIN-LEVEL and MAX-LEVEL specifies the minimum and
maximum level of h1-h6 to include.  They should be integers.

\(fn &optional MIN-LEVEL MAX-LEVEL)" t nil)

(nxhtml-autoload 'html-pagetoc-rebuild-toc `(lp '(nxhtml-download-root-url nil) "nxhtml/html-pagetoc" nxhtml-install-dir) "\
Update the table of contents inserted by `html-pagetoc-insert-toc'.

\(fn)" t nil)

(defconst html-pagetoc-menu-map (let ((map (make-sparse-keymap))) (define-key map [html-pagetoc-rebuild-toc] (list 'menu-item "Update Page TOC" 'html-pagetoc-rebuild-toc)) (define-key map [html-pagetoc-insert-style-guide] (list 'menu-item "Insert CSS Style for Page TOC" 'html-pagetoc-insert-style-guide)) (define-key map [html-pagetoc-insert-toc] (list 'menu-item "Insert Page TOC" 'html-pagetoc-insert-toc)) map))

;;;***

;;;### (autoloads (html-chklnk) "html-chklnk" "nxhtml/html-chklnk.el"
;;;;;;  (19364 56214))
;;; Generated autoloads from nxhtml/html-chklnk.el
(web-autoload-require 'html-chklnk 'lp '(nxhtml-download-root-url nil) "nxhtml/html-chklnk" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'html-chklnk 'custom-loads))) (if (member '"html-chklnk" loads) nil (put 'html-chklnk 'custom-loads (cons '"html-chklnk" loads))))

;;;***

;;;### (autoloads (web-vcs-investigate-elisp-file web-vcs-byte-compile-file
;;;;;;  web-vcs-message-with-face web-vcs-get-files-from-root web-vcs-log-edit
;;;;;;  web-vcs-default-download-directory) "web-vcs" "web-vcs.el"
;;;;;;  (19412 16397))
;;; Generated autoloads from web-vcs.el
(web-autoload-require 'web-vcs 'lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'web-vcs-default-download-directory `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Try to find a suitable place.
Considers site-start.el, site-

\(fn)" nil nil)

(nxhtml-autoload 'web-vcs-log-edit `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Open log file.

\(fn)" t nil)

(nxhtml-autoload 'web-vcs-get-files-from-root `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Download a file tree from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Show URL first and offer to visit the page.  That page will give
you information about version control system (VCS) system used
etc.

\(fn WEB-VCS URL DL-DIR)" nil nil)

(nxhtml-autoload 'web-vcs-message-with-face `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Display a colored message at the bottom of the string.
FACE is the face to use for the message.
FORMAT-STRING and ARGS are the same as for `message'.

Also put FACE on the message in *Messages* buffer.

\(fn FACE FORMAT-STRING &rest ARGS)" nil nil)

(nxhtml-autoload 'web-vcs-byte-compile-file `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Byte compile FILE in a new Emacs sub process.
EXTRA-LOAD-PATH is added to the front of `load-path' during
compilation.

FILE is set to `buffer-file-name' when called interactively.
If LOAD

\(fn FILE &optional LOAD EXTRA-LOAD-PATH COMP-DIR)" t nil)

(nxhtml-autoload 'web-vcs-investigate-elisp-file `(lp '(nxhtml-download-root-url nil) "web-vcs" nxhtml-install-dir) "\
Not documented

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads (nxhtmlmaint-byte-uncompile-all nxhtmlmaint-byte-recompile
;;;;;;  nxhtmlmaint-start-byte-compilation) "nxhtmlmaint" "nxhtmlmaint.el"
;;;;;;  (19378 31293))
;;; Generated autoloads from nxhtmlmaint.el
(web-autoload-require 'nxhtmlmaint 'lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtmlmaint-start-byte-compilation `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Start byte compilation of nXhtml in new Emacs instance.
Byte compiling in general makes elisp code run 5-10 times faster
which is quite noticeable when you use nXhtml.

This will also update the file nxhtml-loaddefs.el.

You must restart Emacs to use the byte compiled files.

If for some reason the byte compiled files does not work you can
remove then with `nxhtmlmaint-byte-uncompile-all'.

\(fn)" t nil)

(nxhtml-autoload 'nxhtmlmaint-byte-recompile `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Recompile or compile all nXhtml files in current Emacs.

\(fn)" t nil)

(nxhtml-autoload 'nxhtmlmaint-byte-uncompile-all `(lp '(nxhtml-download-root-url nil) "nxhtmlmaint" nxhtml-install-dir) "\
Delete byte compiled files in nXhtml.
This will also update the file nxhtml-loaddefs.el.

See `nxhtmlmaint-start-byte-compilation' for byte compiling.

\(fn)" t nil)

;;;***

;;;### (autoloads (zencoding-preview zencoding-expand-yas zencoding-mode
;;;;;;  zencoding-expand-line zencoding) "zencoding-mode" "util/zencoding-mode.el"
;;;;;;  (19275 63380))
;;; Generated autoloads from util/zencoding-mode.el
(web-autoload-require 'zencoding-mode 'lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir 'nxhtml-byte-compile-file)


(let ((loads (get 'zencoding 'custom-loads))) (if (member '"zencoding-mode" loads) nil (put 'zencoding 'custom-loads (cons '"zencoding-mode" loads))))

(nxhtml-autoload 'zencoding-expand-line `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'.

\(fn ARG)" t nil)

(nxhtml-autoload 'zencoding-mode `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'.

\(fn &optional ARG)" t nil)

(nxhtml-autoload 'zencoding-expand-yas `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Not documented

\(fn)" t nil)

(nxhtml-autoload 'zencoding-preview `(lp '(nxhtml-download-root-url nil) "util/zencoding-mode" nxhtml-install-dir) "\
Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("autostart.el" "autostart22.el" "etc/schema/schema-path-patch.el"
;;;;;;  "nxhtml-base.el" "nxhtml/html-imenu.el" "nxhtml/html-move.el"
;;;;;;  "nxhtml/html-quote.el" "nxhtml/nxhtml-autoload.el" "nxhtml/nxhtml-strval.el"
;;;;;;  "nxhtml/nxhtmljs.el" "nxhtml/outline-magic.el" "nxhtml/wtest.el"
;;;;;;  "related/flymake-helpers.el" "related/flymakemsg.el" "related/flymu.el"
;;;;;;  "related/php-imenu.el" "tests/angus77-setup-jde.el" "tests/emacstest-suites.el"
;;;;;;  "tests/ert2.el" "tests/hfy-test.el" "tests/inemacs/bug1013.el"
;;;;;;  "tests/mumamo-test.el" "tests/nxhtmltest-helpers.el" "tests/temp-test.el"
;;;;;;  "util/appmenu-fold.el" "util/css-simple-completion.el" "util/custsets.el"
;;;;;;  "util/ecb-batch-compile.el" "util/fupd.el" "util/idn.el"
;;;;;;  "util/key-cat.el" "util/mumamo-aspnet.el" "util/mumamo-trace.el"
;;;;;;  "util/new-key-seq-widget.el" "util/nxml-mode-os-additions.el"
;;;;;;  "util/org-panel.el" "util/rxi.el" "util/useful-commands.el"
;;;;;;  "web-autoload.el") (19412 26385 593000))

;;;***

;;;### (autoloads (nxhtml-byte-recompile-file nxhtml-byte-compile-file
;;;;;;  nxhtml-get-missing-files nxhtml-update-existing-files nxhtml-setup-download-all
;;;;;;  nxhtml-setup-auto-download nxhtml-setup-install) "nxhtml-web-vcs"
;;;;;;  "nxhtml-web-vcs.el" (19412 16464))
;;; Generated autoloads from nxhtml-web-vcs.el
(web-autoload-require 'nxhtml-web-vcs 'lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir 'nxhtml-byte-compile-file)


(nxhtml-autoload 'nxhtml-setup-install `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Setup and start nXhtml installation.

This is for installation and updating directly from the nXhtml
development sources.

There are two different ways to install:

  (1) Download all at once: `nxhtml-setup-download-all'
  (2) Automatically download part by part: `nxhtml-setup-auto-download'

You can convert between those ways by calling this function again.
You can also do this by setting the option `nxhtml-autoload-web' yourself.

When you have nXhtml installed you can update it:

  (3) Update new files in nXhtml: `nxhtml-update-existing-files'

To learn more about nXhtml visit its home page at URL
`http://www.emacswiki.com/NxhtmlMode/'.

If you want to test auto download (but not use it further) there
is a special function for that, you answer T here:

   (T) Test automatic download part by part: `nxhtml-setup-test-auto-download'

======
*Note*
If you want to download a zip file with latest released version instead then
please see URL `http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html'.

\(fn WAY)" t nil)

(nxhtml-autoload 'nxhtml-setup-auto-download `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Set up to autoload nXhtml files from the web.

This function will download some initial files and then setup to
download the rest when you need them.

Files will be downloaded under the directory root you specify in
DL-DIR.

Note that files will not be upgraded automatically.  The auto
downloading is just for files you are missing. (This may change a
bit in the future.) If you want to upgrade those files that you
have downloaded you can just call `nxhtml-update-existing-files'.

You can easily switch between this mode of downloading or
downloading the whole of nXhtml by once.  To switch just call the
command `nxhtml-setup-install'.

See also the command `nxhtml-setup-download-all'.

Note: If your nXhtml is to old you can't use this function
      directly.  You have to upgrade first, se the function
      above. Version 2.07 or above is good for this.

\(fn DL-DIR)" t nil)

(nxhtml-autoload 'nxhtml-setup-download-all `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Download or update all of nXhtml.

You can download all if nXhtml with this command.

To update existing files use `nxhtml-update-existing-files'.

If you want to download only those files you are actually using
then call `nxhtml-setup-auto-download' instead.

See the command `nxhtml-setup-install' for a convenient way to
call these commands.

For more information about auto download of nXhtml files see
`nxhtml-setup-auto-download'.

\(fn DL-DIR)" t nil)

(nxhtml-autoload 'nxhtml-update-existing-files `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Update existing nXhtml files from the development sources.
Only files you already have will be updated.

Note that this works both if you have setup nXhtml to auto
download files as you need them or if you have downloaded all of
nXhtml at once.

For more information about installing and updating nXhtml see the
command `nxhtml-setup-install'.

\(fn)" t nil)

(nxhtml-autoload 'nxhtml-get-missing-files `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Not documented

\(fn SUB-DIR FILE-NAME-LIST)" nil nil)

(nxhtml-autoload 'nxhtml-byte-compile-file `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Not documented

\(fn FILE &optional LOAD)" nil nil)

(nxhtml-autoload 'nxhtml-byte-recompile-file `(lp '(nxhtml-download-root-url nil) "nxhtml-web-vcs" nxhtml-install-dir) "\
Byte recompile FILE file if necessary.
For more information see `nxhtml-byte-compile-file'.
Loading is done if recompiled and LOAD is t.

\(fn FILE &optional LOAD)" t nil)

;;;***

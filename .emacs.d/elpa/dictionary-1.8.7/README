
               Emacs package for talking to a dictionary server

                                 Introduction

   In December 1998 I installed the dictd server, which can be used to
   access  several  dictionaries  using  a  simple protocol as defined in
   RFC 2229 (Text Version).

   As  my primary working environment is XEmacs 21, I decided to write
   an  Emacs-Lisp package for accessing this dictionary server. The older
   webster.el  didn't  worked with the newer protocol. After starting the
   implementation  I  was  pointed to an already existing implementation,
   but this was basically a wrapper to the dict client program and didn't
   have  all  the features I wanted and have now been implemented in this
   dictionary client.

   If  you  didn't  received this file from its original location you can
   visit it at http://www.myrkr.in-berlin.de/dictionary.html

                                   Features

   The dictionary mode provides the following features:
     * looking up word definitions in all dictionaries
     * search for matching word
     * words/phrases  marked  with  { } in the dictionary definitions are
       recognized as hyper links and browseable
     * easy selection of dictionary and search strategy
     * backward moving through the visited definitions
     * in the latest versions of GNU Emacs and XEmacs you get support for
       popup menus
     * in  GNU Emacs 21 you can lookup words by simply pointing the mouse
       cursor to them (tool-tips)

   Here are three sample screenshot showing the dictionary mode in action
   within an Emacs 21 buffer:

   This buffer shows the result of searching the definition for
   distinguished. A lot of entries have been found, including translation
   into german and russian language. The blue words are hyper-links that
   points to the definitions of these words. The hyper-link with the
   green background will be activated when pressing the middle mouse
   button.
   
   This buffer shows the result of looking for matching words to
   possible. You can now select one of the found entries or ignore the
   search by pressing any mouse button outside the menu.
   
   This screenshot shows the tool-tips supported in GNU Emacs 21.
   Whenever the mouse cursor stands still for a certain time (about one
   second) the word at cursor is looked up in the configured dictionary.
   This example shows the german translation for originally, in this
   context not the right one, ursprünglich would be better.

                           Download and Requirements

   I  have  tested the package with a native GNU Emacs 19.34.1 and XEmacs
   20.4.  I  implemented  some  hacks  to  work  with  the  very very old
   custom.el  file  in  the above GNU Emacs. Please update to the current
   version  whenever possible. XEmacs 21 and Emacs 20 as well as Emacs 21
   have been tested too.
     * Current version (1.7.2) of this package
     * Version 1.5.1 of this package
     * Version 1.4.1 of this package
     * Version 1.3.3 of this package
     * Version 1.2.1 of this package
     * Version 1.1 of this package
     * Initial version (1.0) of this package

   I  you  want  to  know  more  about the differences please look at the
   ChangeLog.

   You will need the custom package to use this package. For full support
   please  check if your system knows the defface function, if not please
   download the current version.

   For  best  usability  I  suggest  using the mouse, but it provide good
   keyboard support as well.

                             Unpacking the archive

   The package is distributed as tar.gz file. You unpack it using:
gzip -dc dictionary-1.7.2.tar.gz | tar xf -

   or
tar -xzf dictionary-1.7.2.tar.gz

   (with  the  version number subject to change) depending on whether you
   are  using  GNU  tar  which  support the z flag for compression. After
   unpacking  the  archive  a directory dictionary-1.7.2 has been created
   containing the necessary files.

                                 Installation

Debian

   If  you  are using a current Debian distribution (one that support the
   emacsen  package  system) and have the dpkg-dev installed (for running
   dpkg-buildpackage) you can use the supplied debian support.
make debian

   This  will  create  a  package  named  dictionary-1.7.2-1_i386.deb  or
   similiar  in  the  parent  directory  of dictionary-1.7.2. You can now
   install  this  package  as  root,  it  will automatically byte-compile
   itself  for  all  installed  emacs versions and provide a startup-file
   which autoloads this package. In the configuration example given below
   you can omit the autoload lines.

   If you no longer want to use this package, you can remove it using:
dpkg -r dictionary

XEmacs 21

   The  XEmacs  version  21  support  so  called  xemacs  packages. These
   packages are also supported, you can create them using:
make EMACS=xemacs package

   The created package will be named dictionary-1.5-pkg.tar.gz and stored
   within  the  current  directory.  If  you  don't  want to install this
   package manually, you can use the following command, provided you have
   sufficient privileges (if unsure, login as super user):
make EMACS=xemacs package-install

   If  you  have  more  than  one XEmacs versions installed make sure the
   EMACS argument to make points to the current binary.

Manually

  Byte compiling

   For  faster  loading  and  executing of the package I strongly suggest
   that  you  byte-compile  the files. Emacs user please call make within
   the  create  subdirectory,  XEmacs  user has to specify there favorite
   tool using make EMACS=xemacs. If your custom package is not up-to-date
   expect some warnings about free variables.

  Installing the files

   To  install  the  files into your GNU Emacs/XEmacs installation please
   copy  the  *.elc  files  into  a  directory  being  in  your load-path
   variable.    On   most   installations   /usr/lib/emacs/site-lisp   or
   /usr/local/lib/emacs/site-lisp are suitable locations.

                              Loading the package

   You have to insert some instructions into your .emacs file to load the
   dictionary  package  whenever needed. If you installed this dictionary
   package  as  Debian  package  or  XEmacs  package  you  don't need the
   autoloads,  they are already supplied. Other users I suggest using the
   following lines:
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)

   In  addition, some key bindings for faster access can be useful. I use
   the following ones in my installation:
;; key bindings
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)

   I  will  describe  the  user-callable  functions  and the key bindings
   within dictionary mode later in this document.

                               Using the package

Glossary

   Before  I  start  describing  how  you use this package, please let me
   explain some words as they are used in this text:

   word
          This is a word you want to lookup in the dictionaries verbatim.

   pattern
          This is used for looking up matching words. A pattern can be as
          simple  as a single word but also as complex as a POSIX regular
          expression.  The  meaning  of a pattern depends on the strategy
          used for matching words.

   dictionary
          The  server  can  handle several distinct dictionaries. You can
          select specific dictionaries or ask the server to search in all
          dictionaries  or  until  matches  or  definitions are found. To
          search  in  all  dictionaries  the  special name * is used, the
          special  name ! requests to search until definitions are found.
          For   more   details  please  take  a  look  at  the  standards
          definition.

   definition
          A dictionary entry that can be the result of a word search.

   search
          The operation of looking up a word in the dictionaries.

   match
          The  operation  of  comparing  a  pattern  to  all words in the
          dictionary.

   strategy
          While  matching  in a dictionary serveral methods for comparing
          words  can  be  used.  These  methods  are named strategies and
          include  exact  match,  regular  expression  match, and soundex
          match.  The  available  strategies depends on the server, but a
          special name . can be used to denote a server-default strategy.

Invoking

   There are five different (documented) ways of invoking the package. By
   calling  dictionary  you can start a new dictionary buffer waiting for
   your  commands.  If  you want to create multiple buffers for searching
   the   dictionary,   you   can   run   this  function  multiple  times.
   dictionary-search  will ask for a word a search defaulting to the word
   at point and present all definitions found.

   If  you  want  to  lookup  the  word  near  the  point without further
   confirmation  use  the dictionary-lookup-definition function. The last
   one is dictionary-match-words which will ask for a pattern and display
   all matching words.

   You can also display a popup menu showing the definition for a certain
   word.  You  just  place  the  mouse  cursor above the word you want to
   lookup  and  press the assigned mouse button. The sample definition in
   section  Customizing  binds this command to the right mouse button
   in  GNU  Emacs  and  to ctrl + right mouse button in XEmacs. For mouse
   invocation use dictionary-mouse-popup-matching-words, for assigning to
   a  key  combination  use the dictionary-popup-matching-words function.
   The latter function search for the word located at point.

   I  have tested this feature in Emacs 21 and XEmacs 21. Emacs 20 do not
   support this kind of popup menus (you will get an error message if you
   try anyway), XEmacs 20 or prior has not been tested yet.

   A  new  feature of version 1.5 is the tool-tip support. This will only
   work  in  GNU  Emacs  21,  XEmacs  uses  a different mechanism (called
   balloon  help)  which  can  not  easily  be  adapted. Please note, the
   tool-tip mode may slow down your Emacs, especially if you prefer using
   the mouse.

   First  you  have  to  define  the  dictionary  for looking up words to
   display  in  the  tool-tip  window.  You  could use * to search in all
   dictionaries, however tool-tip window should be rather small. I use an
   english  to german dictionary for myself, which is called eng-deu (you
   can find out the name if you look into contents of the square brackets
   within  the  dictionary buffer; for example, the line From WordNet (r)
   1.6[wn]: tells you the name of this dictionary is wn).
(setq dictionary-tooltip-dictionary "eng-deu")

   Next  you  have  to decide if you want tool-tip support in the current
   buffer   only   or   in   all   buffers.   For   the   first  use  the
   dictionary-tooltip-mode       command,       for       the      latter
   global-dictionary-tooltip-mode.
;; choose on of the following lines
(global-dictionary-tooltip-mode 1)
(dictionary-tooltip-mode 1)

   To  turn the tool-tip support off, call these functions with a numeric
   argument of 0.

   If  you  get  an error message that the server could not be contacted,
   please  check  the  values  of  the  variables  dictionary-server  and
   dictionary-port.  The  port should usually be 2628, the default server
   as distributed is dict.org.

   You  can  have  multiple  independent  dictionary buffer. If the above
   functions  are  called  from  within  dictionary  mode  they reuse the
   existing buffer. Otherwise they create a new buffer.

Quitting

   Once  a dictionary buffer is created you can close it by simply typing
   q  (dictionary-close)  or  pressing  the  [Quit]  button  on  the top.
   Another,  more  cruel, way is to use the kill-buffer function which is
   handled correctly.

   The  dictionary  mode save the window configuration on startup and try
   to reestablish it when the buffer is being closed.

Using the buffer

   After a successful search the buffer is divided into two sections. The
   first  one  is  the  button area at the top, the other one is the text
   buffer  displaying  the result. By pressing the buttons you can select
   some functions that are otherwise inaccessible with the mouse.

   In  the  text  are  each  definition  is introduced by the name of the
   database  that  contains it. In the default configuration this text is
   in  italic  face.  The definition itself can contains hyper links that
   are  marked  using blue foreground and both sensitive to clicking with
   the mouse and pressing return while being within the link.

   Each  link  selection  or  otherwise selected new search or match will
   create  a  new  buffer  showing the new result. You can use the [Back]
   button  on  the  top  or the l key (dictionary-previous) to return the
   previous buffer contents.

   Pressing Meta while clicking on a link to start the search will extent
   the  search  to  all dictionaries (dictionary-default-dictionary to be
   more precisely).

   If you prefer using the keyboard it can be very frustrating to use the
   cursor  key  to position the point before pressing return to visit the
   link  is possible. Therefore, I defined the Tab and the n keys to jump
   to  the  next link (dictionary-next-link) and the Shift-Tab and p keys
   to  jump  to the previous one (dictionary-prev-link). Please note that
   the  Shift-Tab  key  may be the same as the Tab key when running Emacs
   within a tty (in contrast to a windowing system like X11). There is no
   way  for  a  application  to  differ  Shift-Tab from Tab in this case,
   please  use  the p key to visit the previous link if you run into this
   problem.

Searching

   The  search  operation can be invoked by using the [Search Definition]
   button  on  the  top or by pressing s (dictionary-search). It will ask
   for  a  word  to  search and will default to the word near point. This
   allows  you  to  edit  the  word before starting the search. The found
   definitions  will  be displayed inside the buffer. If no entries could
   not be found an error message will be displayed.

   If  you  want  to quickly lookup the word at the point without further
   confirmation  use the d key (dictionary-lookup-definition). Except for
   not  allowing  to  edit  the  search  word before asking the server it
   behaves the same way as the normal search operation.

Matching

   The  match  operation  is  started  upon pressing the [Matching Words]
   button or pressing the m key (dictionary-match-words). It will use the
   current  database and the current strategy and list matching words for
   the one you entered at the prompt. The output is grouped by dictionary
   and  each found word can be looked up by clicking the word or pressing
   return.  Please  note  that  in  some  cases  not  only  the requested
   definition  but some similiar definitions are shown. This behaviour is
   caused by the keyword lookup in the server. E.g., when you ask for the
   definition  of from in Webster you will presented with the definitions
   of  from  and Thrust, the latter includes the phrase To thrust away or
   from which causes the display.

Selecting dictionary

   By  default  all dictionaries (special name is "*") are searched for a
   word  definition or for matching words. You can select a specific word
   for  both  modi  by pressing the [Select Default Dictionary] button or
   the D key (dictionary-select-dictionary). You will get a message about
   successful selection.

   If  you  hold  Meta  while  selecting  a  dictionary you will get more
   information displayed instead.

   If you want to restore the original behaviour select All dictionaries.
   The  first  matching  dictionary  is  a special dictionary (named "!")
   where  the  search  will  stop  at  the  first  dictionary  with found
   definitions or matching words.

Selection search strategy

   While  searching  matching words to the pattern you entered the server
   can  use  different  comparison algorithm (aka search strategy). Every
   server provides a default strategy which is internally known as ".".

   After  pressing  the  [Select Match Strategy] button or pressing the M
   key  (dictionary-select-strategy)  all  available  strategies  on this
   server  are  presented.  As  in  the  "select dictionary" mode you can
   select it by pressing the mouse button 2 or typing return.

Going backward

   If  you visited a link and want to go back to the previous definition,
   simply    choose    the   [Back]   button   or   press   the   l   key
   (dictionary-previous). The buffer contents and cursor position will be
   restored.  If  you intented to go beyond the first definition an error
   message will appear.

Getting Help

   If  you  are  totally confused what all the keys do in your dictionary
   buffer  some  help will displayed by pressing the h key. Within XEmacs
   you  can  exit the help screen by pressing q, in GNU Emacs you have to
   switch  to the help window using C-x o (other-window) first. This help
   buffer  will  display  the default key bindings only as I had problems
   with  displaying  multiple  bindings for a single function (e.g., both
   button2 and return select a link). Any suggestions are welcome.

Proxy support

   Starting  from  version  1.6  (not officially released) the dictionary
   client  supports  connections  via  HTTP  proxies. It uses the CONNECT
   method which is usually used to relay SSL connections through a proxy.
   On  most  proxies  the dictionary port (2628) must be configured to be
   allowed  to  connect  through  the proxy server. You will get an error
   message like:
HTTP/1.0 500 WWWOFFLE Server Error

   if this is not the case.

   To enable proxy support, set the variable dictionary-use-http-proxy to
   true.  This  can  be  done through the customize interface, there is a
   sub-group  named  Dictionary  Proxy  of  the  Dictionary  group. Other
   settings   you   can   modify   are  the  name  of  the  proxy  server
   (dictionary-proxy-server,  Default  proxy)  and  the port of the proxy
   server (dictionary-proxy-port, Default 3128).

Support for different encodings in dictionaries

   Although the standard definition specifies UTF-8 as character encoding
   to  be  used  for  the  protocol there exists dictionaries encoding in
   other encodings like koi8-r for the russian language. To support these
   dictionaries  there  exists starting from version 1.7 a variable named
   dictionary-coding-systems-for-dictionaries.  This  is  a  list of cons
   cell  containing the dictionary name (as string) and the coding system
   to use (as symbol). For modifying the variable the customize interface
   should be used, it helps you selecting a valid charset and keeping the
   lisp structure intact.

   If a dictionary is not specified in this list its encoding defaults to
   UTF-8.

   The default setting
     '( ("mueller" . koi8-r))

   specifies   that   the   koi8-r   encoding  should  be  used  for  the
   english-russian  dictionary  internally  called  mueller. The internal
   dictionary  name  can  be  found  in  brackets within the dictionaries
   output  buffer after the long dictionary name (for example, the output
   From  WordNet  (r)  1.6[wn] specifies the internal name of the Wordnet
   dictionary being wn.

                                  Customizing

   If  you  have an sufficient recent custom version installed (e.g., the
   one  provided in XEmacs 20.4) you can use the customize-group with the
   dictionary  group  to  customize this package. For using the customize
   buffer please refer to its online help.

   Of  course  you  can  set  all the variables and hooks you want in the
   startup  file.  Here  is a little example that I use for selecting the
   server  on  my  local  machine  and  for  binding some function to the
   user-reserved  keys starting from C-c a to C-c z. It also shows how to
   invoke  popup menus (using the right button in GNU Emacs or ctrl+right
   button in XEmacs) and installs the global tooltip-mode.
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)
(setq dictionary-server "localhost")

;; Popup menu for GNU Emacs 21, and XEmacs 21
(if (boundp 'running-xemacs)
    (global-set-key [(control button3)] 'dictionary-mouse-popup-matching-words)
   (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words))

;; Tool-tip support for GNU Emacs 21
(setq dictionary-tooltip-dictionary "eng-deu")
(global-dictionary-tooltip-mode 1)

                                    Thanks

   I  want  to thank Sam Steingold, Baoqiu Cui, Bruce Ravel, Pavel Janík,
   Sergei  Pokrovsky,  Jeff  Mincy,  Serge  Boiko,  Enrico Scholz, Reuben
   Thomas,  Rui  Zhu, Eugene Morozov, and Robert Pluim for their valuable
   suggestions (including patches) for improving this package.

                                    License

   This  file  is free software; you can redistribute it and/or modify it
   under  the terms of the GNU General Public License as published by the
   Free  Software  Foundation;  either version 2, or (at your option) any
   later version.

   This  file  is  distributed  in  the  hope that it will be useful, but
   WITHOUT   ANY   WARRANTY;   without   even  the  implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR  PURPOSE. See the GNU
   General Public License for more details.

   You  should  have  received  a  copy of the GNU General Public License
   version 2 along with this package; see the file GPL.

                                  ChangeLogs

Version 1.7.1 - 1.7.2

     * link.el  (link-initialize-keymap):  fixed keybinding bug (reported
       by David A. Panariti)

Version 1.7 - 1.7.1

     * fixed  mule-detection in dictionary-coding-system (patch submitted
       by Robert Pluim)

Version 1.5.1 - 1.7

     * added HTTP proxy support (using the CONNECT method)
     * added  support for dictionaries not encoded in utf-8 (suggested by
       Eugene Morozov)

Version 1.5 - 1.5.1

     * fixed bug with non-working dictionary-previous (found by Rui Zhu)
     * fixed key bindings in link.el

Version 1.4.1 - 1.5

     * tool-tip support for GNU Emacs 21

Version 1.4 - 1.4.1

     * changed recognition of utf-8 support (suggested by Enrico Scholz)

Version 1.3.3 - 1.4

     * added popup menu for easier lookup of words

Version 1.3.2 - 1.3.3

     * added  support  for  XEmacs  21  packages to ease installation for
       those users (suggested and reviewed by Enrico Scholz)

Version 1.3.1 - 1.3.2

     * replaced   set-text-properties   by   remove-text-properties   and
       add-text-properties  because  this  function  is  not  recommended
       within XEmacs (reported by Serge Boiko)

Version 1.3 - 1.3.1

     * small  fix in dictionary function to check for availability of the
       utf-8  encoding  to  prevent  problems  in certain xemacs versions
       (reported by Jeff Mincy)
     * added debian support (use dpkg-buildpackage to build a package)

Version 1.2.1 - 1.3

     * Implemented an automatic detection for line ends CR/LF and LF. The
       variable  connection-broken-end-of-line is no longer necessary and
       its value ignored.
     * Added  utf-8  support,  the native character set of the dictionary
       protocol.  Using  ISO-8859-1  (aka  latin-1)  was just a necessary
       work-around.

Version 1.2 - 1.2.1

     * Corrected dictionary command to draw the button bar.
     * Improved documentation on dictionary to explicitly mention the use
       of multiple buffers.

Version 1.1.1 - 1.2

     * Some  users  reported  problems with GNU Emacs 20.3 and MULE. So I
       introduced  a  new  variable  connection-broken-end-of-line  which
       controls whether a line is ended by \n or by \r\n. You can use the
       customize-group command on dictionary to change the setting of the
       variable.

Version 1.1 - 1.1.1

     * dictionary-search now allows editing the word to search for
     * dictionary-search-word-near-point  has  been  removed, you can use
       dictionary-lookup-definition     instead.    It    behaves    like
       dictionary-search but don't allow the search word to be edited (to
       speed up looking up words).

Version 1.0 - 1.1

     * all dictionary buffers now share a single connection
     * added kill-all-local-variables
     * use cons instead of list where possible
     * dictionary-search now:
          + use     word    as    point    as    default    (implementing
            dictionary-search-word-near-point too)
          + asks for dictionary with prefix argument
     * added help-echo tags which are used in XEmacs
     * mark has been replaced by generic marker
     * added  messages  for  communications to the dictionary server that
       may take a while
     * fixed bug with going to the previous link
     * replaced word-at-point by current-word
     _________________________________________________________________

   E-Mail: dictionary@myrkr.in-berlin.de
   Last modified: Fri Dec 21 18:51:23 CET 2001

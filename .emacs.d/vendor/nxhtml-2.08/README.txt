To install nXhtml put this in your .emacs:

   (load "YOUR-PATH-TO/nxhtml/autostart.el")

where autostart.el is the file in the same directory as this
readme.txt file.

Note 1: If you are using Emacs+EmacsW32 then nXhtml is already
        installed.

Note 2: If you are using Emacs 22 then you need to install nXml
        separately. (It is included in Emacs 23.)

Note 3: You may optionally also byte compile nXhtml from the nXhtml
        menu (recommended).



Files that are now in Emacs' development (CVS/Bazaar) repository
================================================================

Some files that were previously distributed with nXhtml are now in
Emacs' development repository.  Distributing them also with nXhtml is
a bad idea since that can lead to that the wrong file is loaded.  They
are therefore not distributed with nXhtml anymore.

Instead you can (if you do not have the files in your Emacs) in many
cases use the version from the repository.  To do that you can
currently start from

  http://cvs.savannah.gnu.org/viewvc/emacs/emacs/lisp/

Files you can download and use this way are for example

  js.el (JavaScript, formerly called espresso.el)
  htmlfontify.el

If you do that I suggest that you put these files in a special
directory and add that to load-path in your .emacs and make that
adding to load-path depend on your Emacs version so that they will not
be loaded when you have upgraded your Emacs.

Note that if you want to use nxml-mode (and it is not in your Emacs)
you should not download it from Emacs' development directory. Instead go to

  http://www.thaiopensource.com/download/

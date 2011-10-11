jshint-mode
=======

Integrate [JSHint](http://jshint.com) into Emacs via a [node.js](http://nodejs.org) server.

![example](https://github.com/daleharvey/jshint-mode/raw/master/example.png)

jshint-mode was heavily inspired by Kevin Turners [lintnode](https://github.com/keturn/lintnode).

Dependencies
========

 * nodejs - http://nodejs.org
 * npmjs - http://npmjs.org
 * formidable

Install *formidable* via npm:

    $ npm install formidable

Building
========

Install via [npm](http://npmjs.org/)

    $ npm install jshint-mode

or you can [download](https://github.com/daleharvey/jshint-mode/tarball/master) or clone

    $ git clone git://github.com/daleharvey/jshint-mode.git

Usage
=====

Add the configuration to your .emacs config file:

    $jshint-emacs-path >> ~/.emacs  // or
	$npm explore jshint-mode
	Exploring /Users/yourname/node_modules/jshint-mode
	Type 'exit' or ^D when finished

	bash-3.2$ exit


or if you downloaded manually:

    (add-to-list 'load-path "~/path/to/jshint-mode")
    (require 'flymake-jshint)
    (add-hook 'javascript-mode-hook
         (lambda () (flymake-mode t)))

**Becareful**: If you emacs can't find node program, maybe you should add some config to .emacs like this:

	(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
	(setq exec-path
	      '(
		"/usr/local/bin"
		"/usr/bin"
		))

You can use M-x flymake-mode to turn flymake of and on, if you want to turn it on be fault, add the following to your .emacs

    ;; Turns on flymake for all files which have a flymake mode
    (add-hook 'find-file-hook 'flymake-find-file-hook)

Note to Emacs.app users
=======================

If use Emacs.app on OS X, you need to set the following environment variables:

  * PATH - Add path to your *node* executable
  * NODE_PATH - Add path to where your node_modules are located so node can find the  *formidable* module

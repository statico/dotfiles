#!/usr/bin/env node

var conf = '\n(add-to-list \'load-path "' + __dirname + '")\n'
  + '(require \'flymake-jshint)\n'
  + '(add-hook \'javascript-mode-hook\n'
  + '    (lambda () (flymake-mode t)))';

console.log(conf);
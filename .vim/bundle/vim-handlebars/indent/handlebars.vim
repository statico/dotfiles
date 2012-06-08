" Handlebars indent
" Taken from https://github.com/juvenn/mustache.vim/blob/master/indent/mustache.vim

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif

" Use HTML formatting rules.
runtime! indent/html.vim

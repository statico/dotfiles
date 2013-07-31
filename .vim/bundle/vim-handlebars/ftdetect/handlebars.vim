if has("autocmd")
  au BufNewFile,BufRead *.handlebars,*.hbs,*.hb,*.hbt set ft=html syntax=handlebars | runtime! ftplugin/handlebars.vim ftplugin/handlebars*.vim ftplugin/handlebars/*.vim
endif

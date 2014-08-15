php.vim
=======

This project is a fork of [php.vim--Garvin][garvin] which in turn is an update of the [php.vim][php-vim] script which in turn is an updated version of the php.vim syntax file distributed with Vim. Whew!

:point_right: Accepting [pull requests](https://github.com/StanAngeloff/php.vim/issues/15) for [PHP 5.6 new features](http://docs.php.net/manual/en/migration56.new-features.php). :point_left:

  [garvin]:  https://github.com/vim-scripts/php.vim--Garvin
  [php-vim]: http://www.vim.org/scripts/script.php?script_id=2874

Configuration
-------------

- `g:php_syntax_extensions_enabled`, `g:php_syntax_extensions_disabled`  
  `b:php_syntax_extensions_enabled`, `b:php_syntax_extensions_disabled`

  A list of extension names (lowercase) for which built-in functions, constants, classes and interfaces is enabled / disabled.

### Overriding Highlighting

Syntax highlighting can be configured to distinguish groups by overriding the defaults. For example, all code in PHP comments is highlighted as `phpComment`, however there are pieces you can tweak, e.g., how `@tags` appear.
There are [many groups you can choose from](https://github.com/StanAngeloff/php.vim/blob/48fc7311fa07c2b83888e7a31fae03118bae720b/syntax/php.vim#L754). Here is how you can override PHP `@tags` and `$parameters` in comments to appear in a different group:

```vim
" Put at the very end of your .vimrc file.

function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END
```

<center>![Overriding Highlighting](http://i.imgur.com/eAlB1eb.png)</center>

Updating
--------

The project comes with a Dockerfile which can be used to rebuild the syntax file.

```bash
docker build -t stanangeloff/php.vim .
docker run --rm -i -v "$PWD":/var/php -t stanangeloff/php.vim > /tmp/php.vim && cat /tmp/php.vim | sed 's/\x0D$//' > syntax/php.vim
docker rmi stanangeloff/php.vim
```

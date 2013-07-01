php.vim
=======

This project is a fork of [php.vim--Garvin][garvin] which in turn is an update
of the [php.vim][php-vim] script which in turn is an updated version of the
php.vim syntax file distributed with Vim. Whew!


  [garvin]:  https://github.com/vim-scripts/php.vim--Garvin
  [php-vim]: http://www.vim.org/scripts/script.php?script_id=2874

Improvements
------------

- Spell checking was enabled in strings and comments (both single and
  multi-line).
- Spell checking was enabled in heredocs, e.g., `<<<EOD`.
- The PHP built-in and extension-provided functions list was updated for
  PHP 5.3.
- Compatibility with [html5.vim][html5].
- Support for not-so-common extensions was turned off, e.g., `mssql_*`.
- Basic support for PHP 5.4 syntax was added.


  [html5]: https://github.com/othree/html5.vim

Customising
-----------

A script `update_syntax.php` is provided to re-generate the syntax file.
A single variable `$allowed_extensions` can be customised to
[turn on/off][defaults] certain extensions.

When re-generating the syntax file, you must have allowed extensions installed.


  [defaults]: https://github.com/StanAngeloff/php.vim/blob/master/update_syntax.php#L29-L101

### Original README

> This is an updated version of the php.vim syntax file distributed with VIM.
> The list of PHP constants, functions, and classes was updated to be current
> with PHP 5.3. Many new classes were added in the 5.2 branch and the
> distributed version only covers up to 5.1.4. In addition I simplified the
> file, removing several sections that are not often used (at least by me) such
> as automatic folding of all control structures and ASP tags support. I also
> removed several switches designed for b/c with VIM 5.X and 6.X. As an
> addition I have included the PHP file I used to generate the constant,
> function, class list. It uses reflection to mine out these items from your
> PHP installation and generate part of the php.vim script. Before running open
> up the file and adjust the output file location and the list of extensions to
> generate syntax for. Then run "php php_vimgen.php" from your shell.

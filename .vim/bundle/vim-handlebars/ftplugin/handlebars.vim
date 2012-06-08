" Taken from https://github.com/juvenn/mustache.vim/blob/master/ftplugin/mustache.vim

if exists("loaded_matchit")
  let b:match_ignorecase = 0

  let b:match_words = '{:},[:],(:),'
	\ . '\%({{\)\@<=[#^]\s*\([-0-9a-zA-Z_?!/.]\+\)\s*}}'
	\ . ':'
	\ . '\%({{\)\@<=/\s*\1\s*}}'
endif

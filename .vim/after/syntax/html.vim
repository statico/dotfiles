" Thanks! https://coderwall.com/p/vgk5-q/make-vim-play-nice-with-html-templates-inside-script-tags
unlet b:current_syntax
syn include @GLSL syntax/glsl.vim
syn region htmlTemplate start=+<script [^>]*type *=[^>]*x-shader[^>]*>+
\                       end=+</script>+me=s-1 keepend
\                       contains=@GLSL,htmlScriptTag

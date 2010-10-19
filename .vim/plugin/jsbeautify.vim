if &cp || exists("loaded_jsbeautify")
    finish
endif
let loaded_jsbeautify = 3



function! s:trim_output()
	while len(s:output) > 0 && (s:output[len(s:output)-1] == " " || s:output[len(s:output)-1] == s:indent_string)
		call remove(s:output, -1)
	endwhile
endfunction

function! s:print_newline(ignore_repeated) 
	let s:if_line_flag = 0
	call s:trim_output()
	if len(s:output)==0
		return
	endif
	if s:output[len(s:output)-1] != "\n" || !a:ignore_repeated
		call add(s:output, "\n")
	endif
	let index = 0
	while index < s:indent_level
		call add(s:output, s:indent_string)
		let index += 1
	endwhile
endfunction

function! s:print_space()
	let last_output = " "
	if len(s:output) > 0
		let last_output = s:output[len(s:output) - 1]
	endif
	if last_output != " " && last_output != "\n" && last_output != s:indent_string
		call add(s:output, " ")
	endif
endfunction

function! s:print_token()
	call add(s:output, s:token_text)
endfunctio

function! s:indent()
	let s:indent_level += 1
endfunction

function! s:unindent()
	if s:indent_level
		let s:indent_level -= 1
	endif
endfunction

function! s:remove_indent()
	if len(s:output)>0 && s:output[len(s:output) -1] == s:indent_string
		call remove(s:output, -1)
	endif
endfunction

function! s:set_mode(mode)
	call add(s:modes, s:current_mode)
	let s:current_mode = a:mode
endfunction

function! s:restore_mode()
	if s:current_mode == "DO_BLOCK"
		let s:do_block_just_closed = 1
	else
		let s:do_block_just_closed = 0
	endif
	let s:current_mode = remove(s:modes, -1)
endfunction

function! s:in_array(what, arr)
	return index(a:arr, a:what) != -1
endfunction

function! s:get_next_token()
	let n_newlines = 0

	if s:parser_pos >= len(s:input)
		return ["", "TK_EOF"]
	endif

	let c = s:input[s:parser_pos]
	let s:parser_pos += 1

	while s:in_array(c, s:whitespace) 
		if s:parser_pos >= len(s:input)
			return ["", "TK_EOF"]
		endif

		if c == "\n"
			let n_newlines += 1
		endif

		let c = s:input[s:parser_pos]
		let s:parser_pos += 1
	endwhile

	let wanted_newline = 0
	
	if s:opt_preserve_newlines
		if n_newlines > 1
			for i in [0, 1] 
				call s:print_newline(i==0)
			endfor
		endif
		let wanted_newline = n_newlines == 1
	endif

	if s:in_array(c, s:wordchar)
		if s:parser_pos < len(s:input)
			while s:in_array(s:input[s:parser_pos], s:wordchar)
				let c .= s:input[s:parser_pos]
				let s:parser_pos += 1
				if s:parser_pos == len(s:input)
					break
				endif
			endwhile
		endif

		"if s:parser_pos != len(s:input) && c =~ /^[0-9]+[Ee]$/ && (s:input[s:parser_pos] == "-" || s:input[s:parser_pos] == "+")
			"let sign = s:input[s:parser_pos]
			"let s:parser_pos += 1

			"let t = get_next_token(s:parser_pos)
			"let c .= sign . t[0]
			"return [c, "TK_WORD"]
	   " endif

		if c == "in"
			return [c, "TK_OPERATOR"]
		endif
		if wanted_newline && s:last_type != "TK_OPERATOR" && !s:if_line_flag
			call s:print_newline(1)
		endif
		return [c, "TK_WORD"]
	endif
	if c == "(" || c == "["
		return [c, "TK_START_EXPR"]
	endif

	if c == ")" || c == "]"
		return [c, "TK_END_EXPR"]
	endif

	if c == "{"
		return [c, "TK_START_BLOCK"]
	endif

	if c == "}"
		return [c, "TK_END_BLOCK"]
	endif

	if c == ";"
		return [c, "TK_SEMICOLON"]
	endif

	if c == "/"
		let comment = ""
		if s:input[s:parser_pos] == "*"
			let s:parser_pos += 1
			if s:parser_pos < len(s:input) 
				while !(s:input[s:parser_pos] == "*" && s:parser_pos + 1 < len(s:input) && s:input[s:parser_pos + 1] == "/" && s:parser_pos < len(s:input))
					let comment .= s:input[s:parser_pos]
					let s:parser_pos += 1
					if s:parser_pos >= len(s:input)
						break
					endif
				endwhile
			endif
			let s:parser_pos += 2
			return ['/*' . comment . '*/', 'TK_BLOCK_COMMENT']
		endif

		" peek for comment // ...
		if s:input[s:parser_pos] == "/"
			let comment = c
			while s:input[s:parser_pos] != "\r" && s:input[s:parser_pos] != "\n"
				let comment .= s:input[s:parser_pos]
				let s:parser_pos += 1
				if s:parser_pos >= len(s:input)
					break
				endif
			endwhile
			let s:parser_pos += 1
			if wanted_newline
				call s:print_newline(1)
			endif
			return [comment, "TK_COMMENT"]
		endif
	endif

	if c == "'" || c =='"' || (c == "/" && ((s:last_type == "TK_WORD" && s:last_text == "return") || (s:last_type == "TK_START_EXPR" || s:last_type == "TK_START_BLOCK" || s:last_type == "TK_END_BLOCK" || s:last_type == "TK_OPERATOR" || s:last_type == "TK_EOF" || s:last_type == "TK_SEMICOLON")))
		let sep = c
		let esc = 0
		let resulting_string = c

		if s:parser_pos < len(s:input)
			while esc || s:input[s:parser_pos] != sep
				let resulting_string .= s:input[s:parser_pos]
				if !esc
					let esc = s:input[s:parser_pos] == "\\"
				else
					let esc = 0
				endif
				let s:parser_pos += 1
				if s:parser_pos >= len(s:input)
					return [resulting_string, "TK_STRING"]
				endif
			endwhile
		endif

		let s:parser_pos += 1

		let resulting_string .= sep

		if sep == "/"
			
			while s:parser_pos < len(s:input) && s:in_array(s:input[s:parser_pos], s:wordchar)
				let resulting_string .= s:input[s:parser_pos]
				let s:parser_pos += 1
			endwhile
		endif
		return [resulting_string, "TK_STRING"]
	endif

	if c == "#"
		let sharp = "#"
		if s:parser_pos < len(s:input) && s:in_array(s:input[s:parser_pos], s:digits)
			let c = s:input[s:parser_pos]
			let sharp .= c
			let s:parser_pos += 1

			while s:parser_pos < len(s:input) && c != "#" && c !="="
				let c = s:input[s:parser_pos]
				let sharp .= c
				let s:parser_pos += 1
			endwhile

			if c == "#"
				return [sharp, "TK_WORD"]
			else
				return [sharp, "TK_OPERATOR"]
			endif
		endif
	endif

	if c == "<" && s:input[s:parser_pos-1 : s:parser_pos+3] == "<!--"				
		let s:parser_pos += 3
		return ["<!--", "TK_COMMENT"]
	endif

	if c == "-" && s:input[s:parser_pos-1 : s:parser_pos+2] == "-->"
		let s:parser_pos += 2
		if wanted_newline
			call s:print_newline(1)
		endif
		return ["-->", "TK_COMMENT"]
	endif

	if s:in_array(c, s:punct)
		while s:parser_pos < len(s:input) && s:in_array(c . s:input[s:parser_pos], s:punct)
			let c .= s:input[s:parser_pos]
			let s:parser_pos += 1
			if s:parser_pos >= len(s:input)
				break
			endif
		endwhile

		return [c, "TK_OPERATOR"]
	endif

		return [c, "TK_UNKNOWN"]
	endif

	

endfunction

function! s:is_js()
	return expand("%:e") == "js"
endfunction

"function! g:Jsbeautify(js_source_text, options)
function! g:Jsbeautify()
	if !s:is_js()
		echo "Not a JS file."
		return
	endif

	"let a:options = {}
	let s:opt_indent_size = 1
	let s:opt_indent_char = "\t"
	let s:opt_preserve_newlines = 1
	let s:opt_indent_level = 0

	let s:if_line_flag = 0
	"--------------------------------
	
	let s:indent_string = ""
	while s:opt_indent_size > 0
		let s:indent_string .= s:opt_indent_char
		let s:opt_indent_size -= 1
	endwhile

	let s:indent_level = s:opt_indent_level

	let lines = getline(1, "$")
	let s:input = join(lines, "\n")
	"let s:input = a:js_source_text
	
	let s:last_word = "" "last 'TK_WORD' passed
	let s:last_type = "TK_START_EXPR" "last token type
	let s:last_text = "" "last token text
	let s:output = []

	let s:do_block_just_closed = 0
	let s:var_line = 0
	let s:var_line_tainted = 0

	let s:whitespace = ["\n", "\r", "\t", " "]
	let s:wordchar = split("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$", '\zs')
	let s:digits = split("0123456789", '\zs')
	
	"<!-- is a special case (ok, it"s a minor hack actually)
	let s:punct = split("+ - * / % & ++ -- = += -= *= /= %= == === != !== > < >= <= >> << >>> >>>= >>= <<= && &= | || ! !! , : ? ^ ^= |= ::", " ")

	let s:line_starters = split("continue,try,throw,return,var,if,switch,case,default,for,while,break", ",")

	let s:current_mode = "BLOCK"
	let s:modes = [s:current_mode]

	let s:parser_pos = 0
	let s:in_case = 0
	while 1
		let t = s:get_next_token()
		let s:token_text = t[0]
		let s:token_type = t[1]
		if s:token_type == "TK_EOF"
			break
		endif
		
		try
			if s:token_type == "TK_START_EXPR"
				let s:var_line = 0
				call s:set_mode("EXPRESSION")
				if s:last_text == ";"
					call s:print_newline(1)
				elseif s:last_type == "TK_END_EXPR" || s:last_type == "TK_START_EXPR"
					" do nothing on (( and )( and ][ and ]( ..
				elseif s:last_type != "TK_WORD" && s:last_type != "TK_OPERATOR"
					call s:print_space()
				elseif s:in_array(s:last_word, s:line_starters)
					call s:print_space()
				endif

				call s:print_token()

			elseif s:token_type == "TK_END_EXPR"
				call s:print_token()
				call s:restore_mode()
			elseif s:token_type == "TK_START_BLOCK"

				if s:last_word == "do"
					call s:set_mode("DO_BLOCK")
				else
					call s:set_mode("BLOCK")
				endif
				if s:last_type != "TK_OPERATOR" && s:last_type != "TK_START_EXPR"
					if s:last_type == "TK_START_BLOCK"
						call s:print_newline(1)
					else
						call s:print_space()
					endif
				endif
				call s:print_token()
				call s:indent()
			elseif s:token_type == "TK_END_BLOCK"
				if s:last_type == "TK_START_BLOCK"
					call s:remove_indent()
					call s:unindent()
				else
					call s:unindent()
					call s:print_newline(1)
				endif
				call s:print_token()
				call s:restore_mode()

			elseif s:token_type == "TK_WORD"
				if s:do_block_just_closed
					" do {} ## while ()
					call s:print_space()
					call s:print_token()
					call s:print_space()
					let s:do_block_just_closed = 0
					throw "jump out"
				endif
				if s:token_text == "case" || s:token_text == "default"
					if s:last_text == ":"
						"switch cases following one another
						call s:remove_indent()
					else
						" case statement starts in the same line where switch
						call s:unindent()
						call s:print_newline(1)
						call s:indent()
					endif
					call s:print_token()
					let s:in_case = 1
					throw "jump out"
				endif

				let s:prefix = "NONE"

				if s:last_type == "TK_END_BLOCK"
					if !s:in_array(tolower(s:token_text), ["else", "catch", "finally"])
						let s:prefix = "NEWLINE"
					else 
						let s:prefix = "SPACE"
						call s:print_space()
					endif
				elseif s:last_type == "TK_SEMICOLON" && (s:current_mode == "BLOCK" || s:current_mode == "DO_BLOCK")
					let s:prefix = "NEWLINE"
				elseif s:last_type == "TK_SEMICOLON" && s:current_mode == "EXPRESSION"
					let s:prefix = "SPACE"
				elseif s:last_type == "TK_STRING"
					let s:prefix = "NEWLINE"
				elseif s:last_type == "TK_WORD"
					let s:prefix = "SPACE"
				elseif s:last_type == "TK_START_BLOCK"
					let s:prefix = "NEWLINE"
				elseif s:last_type == "TK_END_EXPR"
					call s:print_space()
					let s:prefix = "NEWLINE"
				endif

				if s:last_type != "TK_END_BLOCK" && s:in_array(tolower(s:token_text), ["else", "catch", "finally"])
					call s:print_newline(1)
				elseif s:in_array(s:token_text, s:line_starters) || s:prefix == "NEWLINE"
					if s:last_text == "else"
						call s:print_space()
					elseif (s:last_type == "TK_START_EXPR" || s:last_text == "=" || s:last_text == ",") && s:token_text == "function"
						" no need to force newline on "function":
						" DONOTHINT
					elseif s:last_type == "TK_WORD" && (s:last_text == "return" || s:last_text == "throw")
						" no newline between "return nnn"
						call s:print_space()
					elseif s:last_type != "TK_END_EXPR"
						if (s:last_type != "TK_START_EXPR" || s:token_text != "var") && s:last_text != ":"
							" no need to force newline on "var": for (var
							" x = 0...)
							if s:token_text == "if" && s:last_type == "TK_WORD" && s:last_word == "else"
								" no newline for } else if {
								call s:print_space()
							else
								call s:print_newline(1)
							endif
						endif
					else
						if s:in_array(s:token_text, s:line_starters) && s:last_text != ")"
							call s:print_newline(1)
						endif
					endif
				elseif s:prefix == "SPACE"
					call s:print_space()
				endif
				call s:print_token()
				let s:last_word = s:token_text

				if s:token_text == "var"
					let s:var_line = 1
					let s:var_line_tainted = 0
				endif

				if s:token_text == "if" || s:token_text == "else"
					let s:if_line_flag = 1
				endif

			elseif s:token_type == "TK_SEMICOLON"
				call s:print_token()
				let s:var_line = 0
			
			elseif s:token_type == "TK_STRING"
				if s:last_type == "TK_START_BLOCK" || s:last_type == "TK_END_BLOCK" || s:last_type == "TK_SEMICOLON"
					call s:print_newline(1)
				elseif s:last_type == "TK_WORD"
					call s:print_space()
				endif
				call s:print_token()

			elseif s:token_type == "TK_OPERATOR"

				let start_delim = 1
				let end_delim = 1
				if s:var_line && s:token_text != ","
					let s:var_line_tainted = 1
					if s:token_text == ":"
						let s:var_line = 0
					endif
				endif
				if s:var_line && s:token_text=="," && s:current_mode == "EXPRESSION"
					" do not break on comma, for(var a = 1, b = 2)
					let s:var_line_tainted = 0
				endif

				if s:token_text == ":" && s:in_case
					call s:print_token()
					call s:print_newline(1)
					throw "jump out"
				endif

				if s:token_text == "::"
					" no spaces around exotic namespacing syntax operator
					call s:print_token()
					throw "jump out"
				endif

				let s:in_case = 0

				if s:token_text == ","
					if s:var_line
						if s:var_line_tainted
							call s:print_token()
							call s:print_newline(1)
							let s:var_line_tainted = 0
						else
							call s:print_token()
							call s:print_space()
						endif
					elseif s:last_type == "TK_END_BLOCK"
						call s:print_token()
						call s:print_newline(1)
					else
						if s:current_mode == "BLOCK"
							call s:print_token()
							call s:print_newline(1)
						else
							" EXPR od DO_BLOCK
							call s:print_token()
							call s:print_space()
						endif
					endif
					throw "jump out"
				elseif s:token_text == "--" || s:token_text == "++" " unary operators special case
					if s:last_text == ";"
						" space for (;; ++i)
						let start_delim = 1
						let end_delim = 0
					else 
						let start_delim = 0
						let end_delim = 0
					endif
				elseif s:token_text == "!" && s:last_type == "TK_START_EXPR"
					" special case handling: if (!a)
					let start_delim = 0
					let end_delim = 0
				elseif s:last_type == "TK_OPERATOR"
					let s:start_delim = 0
					let s:end_delim = 0
				elseif s:last_type == "TK_END_EXPR"
					let s:start_delim = 1
					let s:end_delim = 1
				elseif s:token_text == "."
					" decimal digits or object.property
					let start_delim = 0
					let end_delim = 0
				elseif s:token_text == ":"
					" zz: xx
					" can"t differentiate ternary op, so for now it"s a ? b:
					" c;without space before colon
					if s:last_text =~ '/^\d+$/'
						" a little help for ternary a ? 1 : 0
						let start_delim = 1
					else
						let start_delim = 0
					endif
				endif
				if start_delim
					call s:print_space()
				endif

				call s:print_token()

				if end_delim
					call s:print_space()
				endif
				throw "jump out"

			elseif s:token_type == "TK_BLOCK_COMMENT"
				call s:print_newline(1)
				call s:print_token()
				call s:print_newline(1)
				
			elseif s:token_type == "TK_COMMENT"

				"call s:print_newline(1)
				call s:print_space()
				call s:print_token()
				call s:print_newline(1)

			elseif s:token_type == "TK_UNKNOWN"
				call s:print_token()
				throw "jump out"
			endif
		catch /.*/
			if v:exception != 'jump out'
				echo "exception caught: " v:exception 
			endif
		endtry

		let s:last_type = s:token_type
		let s:last_text = s:token_text
	endwhile
	
	let ret = join(s:output, "")
	:g/.*/d
	let @0 = ret
	:put!0
endfunction

nnoremap <silent> <leader>ff :call g:Jsbeautify()<cr>

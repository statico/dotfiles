" File: taglist.vim
" Author: Yegappan Lakshmanan (yegappan AT yahoo DOT com)
" Version: 3.3
" Last Modified: May 26, 2004
"
" The "Tag List" plugin is a source code browser plugin for Vim and
" provides an overview of the structure of source code files and allows
" you to efficiently browse through source code files for different
" programming languages.  You can visit the taglist plugin home page for
" more information:
"
"       http://www.geocities.com/yegappan/taglist
"
" You can subscribe to the taglist mailing list to post your questions
" or suggestions for improvement or to report bugs. Visit the following
" page for subscribing to the mailing list:
"
"       http://groups.yahoo.com/group/taglist/
"
" For more information about using this plugin, after installing the
" taglist plugin, use the ":help taglist" command.
"
" Installation
" ------------
" 1. Download the taglist.zip file and unzip the files to the $HOME/.vim
"    or the $HOME/vimfiles or the $VIM/vimfiles directory. This should
"    unzip the following two files (the directory structure should be
"    preserved):
"
"       plugin/taglist.vim - main taglist plugin file
"       doc/taglist.txt    - documentation (help) file
"
"    Refer to the 'add-plugin', 'add-global-plugin' and 'runtimepath'
"    Vim help pages for more details about installing Vim plugins.
" 2. Change to the $HOME/.vim/doc or $HOME/vimfiles/doc or
"    $VIM/doc/vimfiles directory, start Vim and run the ":helptags ."
"    command to process the taglist help file.
" 3. Set the Tlist_Ctags_Cmd variable to point to the location of the
"    exuberant ctags utility (not to the directory) in the .vimrc file.
" 4. If you are running a terminal/console version of Vim and the
"    terminal doesn't support changing the window width then set the
"    'Tlist_Inc_Winwidth' variable to 0 in the .vimrc file.
" 5. Restart Vim.
" 6. You can now use the ":Tlist" command to open/close the taglist
"    window. You can use the ":help taglist" command to get more
"    information about using the taglist plugin.
" 
" ****************** Do not modify after this line ************************
if exists('loaded_taglist') || &cp
    finish
endif
let loaded_taglist=1

" Location of the exuberant ctags tool
if !exists('Tlist_Ctags_Cmd')
    if executable('exuberant-ctags')
        let Tlist_Ctags_Cmd = 'exuberant-ctags'
    elseif executable('ctags')
        let Tlist_Ctags_Cmd = 'ctags'
    elseif executable('tags')
        let Tlist_Ctags_Cmd = 'tags'
    else
        echomsg 'Taglist: Exuberant ctags not found in PATH. ' .
                    \ 'Plugin is not loaded.'
        finish
    endif
endif

" Tag listing sort type - 'name' or 'order'
if !exists('Tlist_Sort_Type')
    let Tlist_Sort_Type = 'order'
endif

" Tag listing window split (horizontal/vertical) control
if !exists('Tlist_Use_Horiz_Window')
    let Tlist_Use_Horiz_Window = 0
endif

" Open the vertically split taglist window on the left or on the right side.
" This setting is relevant only if Tlist_Use_Horiz_Window is set to zero (i.e.
" only for vertically split windows)
if !exists('Tlist_Use_Right_Window')
    let Tlist_Use_Right_Window = 0
endif

" Increase Vim window width to display vertically split taglist window.  For
" MS-Windows version of Vim running in a MS-DOS window, this must be set to 0
" otherwise the system may hang due to a Vim limitation.
if !exists('Tlist_Inc_Winwidth')
    if (has('win16') || has('win95')) && !has('gui_running')
        let Tlist_Inc_Winwidth = 0
    else
        let Tlist_Inc_Winwidth = 1
    endif
endif

" Vertically split taglist window width setting
if !exists('Tlist_WinWidth')
    let Tlist_WinWidth = 30
endif

" Horizontally split taglist window height setting
if !exists('Tlist_WinHeight')
    let Tlist_WinHeight = 10
endif

" Automatically open the taglist window on Vim startup
if !exists('Tlist_Auto_Open')
    let Tlist_Auto_Open = 0
endif

" Display tag prototypes or tag names in the taglist window
if !exists('Tlist_Display_Prototype')
    let Tlist_Display_Prototype = 0
endif

" Display tag scopes in the taglist window
if !exists('Tlist_Display_Tag_Scope')
    let Tlist_Display_Tag_Scope = 1
endif

" Use single left mouse click to jump to a tag. By default this is disabled.
" Only double click using the mouse will be processed.
if !exists('Tlist_Use_SingleClick')
    let Tlist_Use_SingleClick = 0
endif

" Control whether additional help is displayed as part of the taglist or not.
" Also, controls whether empty lines are used to separate the tag tree.
if !exists('Tlist_Compact_Format')
    let Tlist_Compact_Format = 0
endif

" Exit Vim if only the taglist window is currently open. By default, this is
" set to zero.
if !exists('Tlist_Exit_OnlyWindow')
    let Tlist_Exit_OnlyWindow = 0
endif

" Automatically close the folds for the non-active files in the taglist window
if !exists('Tlist_File_Fold_Auto_Close')
    let Tlist_File_Fold_Auto_Close = 0
endif

" Automatically highlight the current tag
if !exists('Tlist_Auto_Highlight_Tag')
    let Tlist_Auto_Highlight_Tag = 1
endif

" Process files even when the taglist window is not open
if !exists('Tlist_Process_File_Always')
    let Tlist_Process_File_Always = 0
endif

" Enable fold column to display the folding for the tag tree
if !exists('Tlist_Enable_Fold_Column')
    let Tlist_Enable_Fold_Column = 1
endif

"------------------- end of user configurable options --------------------

" Initialize the taglist plugin local variables for the supported file types
" and tag types

" assembly language
let s:tlist_def_asm_settings = 'asm;d:define;l:label;m:macro;t:type'

" aspperl language
let s:tlist_def_aspperl_settings = 'asp;f:function;s:sub;v:variable'

" aspvbs language
let s:tlist_def_aspvbs_settings = 'asp;f:function;s:sub;v:variable'

" awk language
let s:tlist_def_awk_settings = 'awk;f:function'

" beta language
let s:tlist_def_beta_settings = 'beta;f:fragment;s:slot;v:pattern'

" c language
let s:tlist_def_c_settings = 'c;d:macro;g:enum;s:struct;u:union;t:typedef;' .
                           \ 'v:variable;f:function'

" c++ language
let s:tlist_def_cpp_settings = 'c++;v:variable;d:macro;t:typedef;c:class;' .
                             \ 'n:namespace;g:enum;s:struct;u:union;f:function'

" c# language
let s:tlist_def_cs_settings = 'c#;d:macro;t:typedef;n:namespace;c:class;' .
                             \ 'E:event;g:enum;s:struct;i:interface;' .
                             \ 'p:properties;m:method'

" cobol language
let s:tlist_def_cobol_settings = 'cobol;d:data;f:file;g:group;p:paragraph;' .
                               \ 'P:program;s:section'

" eiffel language
let s:tlist_def_eiffel_settings = 'eiffel;c:class;f:feature'

" erlang language
let s:tlist_def_erlang_settings = 'erlang;d:macro;r:record;m:module;f:function'

" expect (same as tcl) language
let s:tlist_def_expect_settings = 'tcl;c:class;f:method;p:procedure'

" fortran language
let s:tlist_def_fortran_settings = 'fortran;p:program;b:block data;' .
                    \ 'c:common;e:entry;i:interface;k:type;l:label;m:module;' .
                    \ 'n:namelist;t:derived;v:variable;f:function;s:subroutine'

" HTML language
let s:tlist_def_html_settings = 'html;a:anchor;f:javascript function'

" java language
let s:tlist_def_java_settings = 'java;p:package;c:class;i:interface;' .
                              \ 'f:field;m:method'

" javascript language
let s:tlist_def_javascript_settings = 'javascript;f:function'

" lisp language
let s:tlist_def_lisp_settings = 'lisp;f:function'

" lua language
let s:tlist_def_lua_settings = 'lua;f:function'

" makefiles
let s:tlist_def_make_settings = 'make;m:macro'

" pascal language
let s:tlist_def_pascal_settings = 'pascal;f:function;p:procedure'

" perl language
let s:tlist_def_perl_settings = 'perl;p:package;s:subroutine'

" php language
let s:tlist_def_php_settings = 'php;c:class;f:function'

" python language
let s:tlist_def_python_settings = 'python;c:class;m:member;f:function'

" rexx language
let s:tlist_def_rexx_settings = 'rexx;s:subroutine'

" ruby language
let s:tlist_def_ruby_settings = 'ruby;c:class;f:method;F:function;' .
                              \ 'm:singleton method'

" scheme language
let s:tlist_def_scheme_settings = 'scheme;s:set;f:function'

" shell language
let s:tlist_def_sh_settings = 'sh;f:function'

" C shell language
let s:tlist_def_csh_settings = 'sh;f:function'

" Z shell language
let s:tlist_def_zsh_settings = 'sh;f:function'

" slang language
let s:tlist_def_slang_settings = 'slang;n:namespace;f:function'

" sml language
let s:tlist_def_sml_settings = 'sml;e:exception;c:functor;s:signature;' .
                             \ 'r:structure;t:type;v:value;f:function'

" sql language
let s:tlist_def_sql_settings = 'sql;c:cursor;F:field;P:package;r:record;' .
            \ 's:subtype;t:table;T:trigger;v:variable;f:function;p:procedure'

" tcl language
let s:tlist_def_tcl_settings = 'tcl;c:class;f:method;p:procedure'

" vera language
let s:tlist_def_vera_settings = 'vera;c:class;d:macro;e:enumerator;' .
                                \ 'f:function;g:enum;m:member;p:program;' .
                                \ 'P:prototype;t:task;T:typedef;v:variable;' .
                                \ 'x:externvar'

"verilog language
let s:tlist_def_verilog_settings = 'verilog;m:module;P:parameter;r:register;' .
                                 \ 't:task;w:write;p:port;v:variable;f:function'

" vim language
let s:tlist_def_vim_settings = 'vim;a:autocmds;v:variable;f:function'

" yacc language
let s:tlist_def_yacc_settings = 'yacc;l:label'

"------------------- end of language specific options --------------------

" Vim window size is changed or not
let s:tlist_winsize_chgd = 0
" Taglist window is maximized or not
let s:tlist_win_maximized = 0
" Number of files displayed in the taglist window
let s:tlist_file_count = 0
" Number of filetypes supported by taglist
let s:tlist_ftype_count = 0
" Is taglist part of other plugins like winmanager or cream?
let s:tlist_app_name = "none"
" Are we displaying brief help text
let s:tlist_brief_help = 1
" Do not change the name of the taglist title variable. The winmanager plugin
" relies on this name to determine the title for the taglist plugin.
let TagList_title = "__Tag_List__"

" An autocommand is used to refresh the taglist window when entering any
" buffer. We don't want to refresh the taglist window if we are entering the
" file window from one of the taglist functions. The 'Tlist_Skip_Refresh'
" variable is used to skip the refresh of the taglist window and is set
" and cleared appropriately.
let s:Tlist_Skip_Refresh = 0

" Tlist_Display_Help()
function! s:Tlist_Display_Help()
    if s:tlist_app_name == "winmanager"
        " To handle a bug in the winmanager plugin, add a space at the
        " last line
        call setline('$', ' ')
    endif

    if s:tlist_brief_help
        " Add the brief help
        call append(0, '" Press ? to display help text')
    else
        " Add the extensive help
        call append(0, '" <enter> : Jump to tag definition')
        call append(1, '" o : Jump to tag definition in new window')
        call append(2, '" p : Preview the tag definition')
        call append(3, '" <space> : Display tag prototype')
        call append(4, '" u : Update tag list')
        call append(5, '" s : Select sort field')
        call append(6, '" d : Remove file from taglist')
        call append(7, '" x : Zoom-out/Zoom-in taglist window')
        call append(8, '" + : Open a fold')
        call append(9, '" - : Close a fold')
        call append(10, '" * : Open all folds')
        call append(11, '" = : Close all folds')
        call append(12, '" [[ : Move to the start of previous file')
        call append(13, '" ]] : Move to the start of next file')
        call append(14, '" q : Close the taglist window')
        call append(15, '" ? : Remove help text')
    endif
endfunction

" Tlist_Toggle_Help_Text()
" Toggle taglist plugin help text between the full version and the brief
" version
function! s:Tlist_Toggle_Help_Text()
    if g:Tlist_Compact_Format
        " In compact display mode, do not display help
        return
    endif

    " Include the empty line displayed after the help text
    let brief_help_size = 1
    let full_help_size = 16

    setlocal modifiable

    " Set report option to a huge value to prevent informational messages
    " while deleting the lines
    let old_report = &report
    set report=99999

    " Remove the currently highlighted tag. Otherwise, the help text
    " might be highlighted by mistake
    match none

    " Toggle between brief and full help text
    if s:tlist_brief_help
        let s:tlist_brief_help = 0

        " Remove the previous help
        exe '1,' . brief_help_size . ' delete _'

        " Adjust the start/end line numbers for the files
        call s:Tlist_Update_Line_Offsets(0, 1, full_help_size - brief_help_size)
    else
        let s:tlist_brief_help = 1

        " Remove the previous help
        exe '1,' . full_help_size . ' delete _'

        " Adjust the start/end line numbers for the files
        call s:Tlist_Update_Line_Offsets(0, 0, full_help_size - brief_help_size)
    endif

    call s:Tlist_Display_Help()

    " Restore the report option
    let &report = old_report

    setlocal nomodifiable
endfunction

" Tlist_Warning_Msg()
" Display a message using WarningMsg highlight group
function! s:Tlist_Warning_Msg(msg)
    echohl WarningMsg
    echomsg a:msg
    echohl None
endfunction

" Tlist_Get_File_Index()
" Return the index of the specified filename
function! s:Tlist_Get_File_Index(fname)
    let i = 0

    " Do a linear search
    while i < s:tlist_file_count
        if s:tlist_{i}_filename == a:fname
            return i
        endif
        let i = i + 1
    endwhile

    return -1
endfunction

" Tlist_Get_File_Index_By_Linenum()
" Return the index of the filename present in the specified line number
" Line number refers to the line number in the taglist window
function! s:Tlist_Get_File_Index_By_Linenum(lnum)
    let i = 0

    " TODO: Convert this to a binary search
    while i < s:tlist_file_count
        if a:lnum >= s:tlist_{i}_start && a:lnum <= s:tlist_{i}_end
            return i
        endif
        let i = i + 1
    endwhile

    return -1
endfunction

" Tlist_Skip_File()
" Check whether tag listing is supported for the specified file
function! s:Tlist_Skip_File(filename, ftype)
    " Skip buffers with filetype not set
    if a:ftype == ''
        return 1
    endif

    " Skip files which are not supported by exuberant ctags
    " First check whether default settings for this filetype are available.
    " If it is not available, then check whether user specified settings are
    " available. If both are not available, then don't list the tags for this
    " filetype
    let var = 's:tlist_def_' . a:ftype . '_settings'
    if !exists(var)
        let var = 'g:tlist_' . a:ftype . '_settings'
        if !exists(var)
            return 1
        endif
    endif

    " Skip buffers with no names
    if a:filename == ''
        return 1
    endif

    " Skip files which are not readable or files which are not yet stored
    " to the disk
    if !filereadable(a:filename)
        return 1
    endif

    return 0
endfunction

" Tlist_FileType_Init
" Initialize the ctags arguments and tag variable for the specified
" file type
function! s:Tlist_FileType_Init(ftype)
    " If the user didn't specify any settings, then use the default
    " ctags args. Otherwise, use the settings specified by the user
    let var = 'g:tlist_' . a:ftype . '_settings'
    if exists(var)
        " User specified ctags arguments
        let settings = {var} . ';'
    else
        " Default ctags arguments
        let var = 's:tlist_def_' . a:ftype . '_settings'
        if !exists(var)
            " No default settings for this file type. This filetype is
            " not supported
            return 0
        endif
        let settings = s:tlist_def_{a:ftype}_settings . ';'
    endif

    let msg = 'Taglist: Invalid ctags option setting - ' . settings

    " Format of the option that specifies the filetype and ctags arugments:
    "
    "       <language_name>;flag1:name1;flag2:name2;flag3:name3
    "

    " Extract the file type to pass to ctags. This may be different from the
    " file type detected by Vim
    let pos = stridx(settings, ';')
    if pos == -1
        call s:Tlist_Warning_Msg(msg)
        return 0
    endif
    let ctags_ftype = strpart(settings, 0, pos)
    if ctags_ftype == ''
        call s:Tlist_Warning_Msg(msg)
        return 0
    endif
    " Make sure a valid filetype is supplied. If the user didn't specify a
    " valid filetype, then the ctags option settings may be treated as the
    " filetype
    if ctags_ftype =~ ':'
        call s:Tlist_Warning_Msg(msg)
        return 0
    endif

    " Remove the file type from settings
    let settings = strpart(settings, pos + 1)
    if settings == ''
        call s:Tlist_Warning_Msg(msg)
        return 0
    endif

    " Process all the specified ctags flags. The format is
    " flag1:name1;flag2:name2;flag3:name3
    let ctags_flags = ''
    let cnt = 0
    while settings != ''
        " Extract the flag
        let pos = stridx(settings, ':')
        if pos == -1
            call s:Tlist_Warning_Msg(msg)
            return 0
        endif
        let flag = strpart(settings, 0, pos) 
        if flag == ''
            call s:Tlist_Warning_Msg(msg)
            return 0
        endif
        " Remove the flag from settings
        let settings = strpart(settings, pos + 1)

        " Extract the tag type name
        let pos = stridx(settings, ';')
        if pos == -1
            call s:Tlist_Warning_Msg(msg)
            return 0
        endif
        let name = strpart(settings, 0, pos)
        if name == ''
            call s:Tlist_Warning_Msg(msg)
            return 0
        endif
        let settings = strpart(settings, pos + 1)

        let cnt = cnt + 1

        let s:tlist_{a:ftype}_{cnt}_name = flag
        let s:tlist_{a:ftype}_{cnt}_fullname = name
        let ctags_flags = ctags_flags . flag
    endwhile

    let s:tlist_{a:ftype}_ctags_args = '--language-force=' . ctags_ftype .
                            \ ' --' . ctags_ftype . '-types=' . ctags_flags
    let s:tlist_{a:ftype}_count = cnt
    let s:tlist_{a:ftype}_ctags_flags = ctags_flags

    " Save the filetype name
    let s:tlist_ftype_{s:tlist_ftype_count}_name = a:ftype
    let s:tlist_ftype_count = s:tlist_ftype_count + 1

    return 1
endfunction

" Tlist_Discard_TagInfo
" Discard the stored tag information for a file
function! s:Tlist_Discard_TagInfo(fidx)
    let ftype = s:tlist_{a:fidx}_filetype

    " Discard information about the tags defined in the file
    let i = 1
    while i <= s:tlist_{a:fidx}_tag_count
        unlet! s:tlist_{a:fidx}_tag_{i}
        let i = i + 1
    endwhile

    let s:tlist_{a:fidx}_tag_count = 0

    " Discard information about tags groups by their type
    let i = 1
    while i <= s:tlist_{ftype}_count
        let ttype = s:tlist_{ftype}_{i}_name
        if s:tlist_{a:fidx}_{ttype} != ''
            let s:tlist_{a:fidx}_{ttype} = ''
            let s:tlist_{a:fidx}_{ttype}_start = 0
            let cnt = s:tlist_{a:fidx}_{ttype}_count
            let s:tlist_{a:fidx}_{ttype}_count = 0
            let j = 1
            while j <= cnt
                unlet! s:tlist_{a:fidx}_{ttype}_{j}
                let j = j + 1
            endwhile
        endif
        let i = i + 1
    endwhile
endfunction

" Tlist_Update_Line_Offsets
" Update the line offsets for tags for files starting from start_idx
" and displayed in the taglist window by the specified offset
function! s:Tlist_Update_Line_Offsets(start_idx, increment, offset)
    let i = a:start_idx

    while i < s:tlist_file_count
        if s:tlist_{i}_visible
            " Update the start/end line number only if the file is visible
            if a:increment
                let s:tlist_{i}_start = s:tlist_{i}_start + a:offset
                let s:tlist_{i}_end = s:tlist_{i}_end + a:offset
            else
                let s:tlist_{i}_start = s:tlist_{i}_start - a:offset
                let s:tlist_{i}_end = s:tlist_{i}_end - a:offset
            endif
        endif
        let i = i + 1
    endwhile
endfunction

" Tlist_Discard_FileInfo
" Discard the stored information for a file
function! s:Tlist_Discard_FileInfo(fidx)
    call s:Tlist_Discard_TagInfo(a:fidx)

    let ftype = s:tlist_{a:fidx}_filetype

    let i = 1
    while i <= s:tlist_{ftype}_count
        let ttype = s:tlist_{ftype}_{i}_name
        unlet! s:tlist_{a:fidx}_{ttype}
        unlet! s:tlist_{a:fidx}_{ttype}_start
        unlet! s:tlist_{a:fidx}_{ttype}_count
        let i = i + 1
    endwhile

    unlet! s:tlist_{a:fidx}_filename
    unlet! s:tlist_{a:fidx}_sort_type
    unlet! s:tlist_{a:fidx}_filetype
    unlet! s:tlist_{a:fidx}_mtime
    unlet! s:tlist_{a:fidx}_start
    unlet! s:tlist_{a:fidx}_end
    unlet! s:tlist_{a:fidx}_valid
    unlet! s:tlist_{a:fidx}_visible
    unlet! s:tlist_{a:fidx}_tag_count
endfunction

" Tlist_Remove_File_From_Display
" Remove the specified file from display
function! s:Tlist_Remove_File_From_Display(fidx)
    " Remove the tags displayed for the specified file from the window
    let start = s:tlist_{a:fidx}_start
    " Include the empty line after the last line also
    if g:Tlist_Compact_Format
        let end = s:tlist_{a:fidx}_end
    else
        let end = s:tlist_{a:fidx}_end + 1
    endif

    setlocal modifiable

    exe 'silent! ' . start . ',' . end . 'delete _'

    setlocal nomodifiable

    " Correct the start and end line offsets for all the files following
    " this file, as the tags for this file are removed
    call s:Tlist_Update_Line_Offsets(a:fidx + 1, 0, end - start + 1)
endfunction

" Tlist_Remove_File
" Remove the file under the cursor or the specified file index
function! s:Tlist_Remove_File(file_idx)
    let fidx = a:file_idx

    if fidx == -1
        let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
        if fidx == -1
            return
        endif
    endif

    call s:Tlist_Remove_File_From_Display(fidx)

    call s:Tlist_Discard_FileInfo(fidx)

    " Shift all the file variables by one index
    let i = fidx + 1

    while i < s:tlist_file_count
        let j = i - 1

        let s:tlist_{j}_filename = s:tlist_{i}_filename
        let s:tlist_{j}_sort_type = s:tlist_{i}_sort_type
        let s:tlist_{j}_filetype = s:tlist_{i}_filetype
        let s:tlist_{j}_mtime = s:tlist_{i}_mtime
        let s:tlist_{j}_start = s:tlist_{i}_start
        let s:tlist_{j}_end = s:tlist_{i}_end
        let s:tlist_{j}_valid = s:tlist_{i}_valid
        let s:tlist_{j}_visible = s:tlist_{i}_visible
        let s:tlist_{j}_tag_count = s:tlist_{i}_tag_count

        let k = 1
        while k <= s:tlist_{j}_tag_count
            let s:tlist_{j}_tag_{k} = s:tlist_{i}_tag_{k}
            let k = k + 1
        endwhile

        let ftype = s:tlist_{i}_filetype

        let k = 1
        while k <= s:tlist_{ftype}_count
            let ttype = s:tlist_{ftype}_{k}_name
            let s:tlist_{j}_{ttype} = s:tlist_{i}_{ttype}
            let s:tlist_{j}_{ttype}_start = s:tlist_{i}_{ttype}_start
            let s:tlist_{j}_{ttype}_count = s:tlist_{i}_{ttype}_count
            if s:tlist_{j}_{ttype} != ''
                let l = 1
                while l <= s:tlist_{j}_{ttype}_count
                    let s:tlist_{j}_{ttype}_{l} = s:tlist_{i}_{ttype}_{l}
                    let l = l + 1
                endwhile
            endif
            let k = k + 1
        endwhile

        " As the file and tag information is copied to the new index,
        " discard the previous information
        call s:Tlist_Discard_FileInfo(i)

        let i = i + 1
    endwhile

    " Reduce the number of files displayed
    let s:tlist_file_count = s:tlist_file_count - 1
endfunction


" Tlist_Open_Window
" Create a new taglist window. If it is already open, jump to it
function! s:Tlist_Open_Window()
    " If used with winmanager don't open windows. Winmanager will handle
    " the window/buffer management
    if s:tlist_app_name == "winmanager"
        return
    endif

    " If the window is open, jump to it
    let winnum = bufwinnr(g:TagList_title)
    if winnum != -1
        " Jump to the existing window
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
        return
    endif

    " Create a new window. If user prefers a horizontal window, then open
    " a horizontally split window. Otherwise open a vertically split
    " window
    if g:Tlist_Use_Horiz_Window
        " Open a horizontally split window
        let win_dir = 'botright'
        " Horizontal window height
        let win_size = g:Tlist_WinHeight
    else
        " Open a horizontally split window. Increase the window size, if
        " needed, to accomodate the new window
        if g:Tlist_Inc_Winwidth &&
                    \ &columns < (80 + g:Tlist_WinWidth)
            " one extra column is needed to include the vertical split
            let &columns= &columns + (g:Tlist_WinWidth + 1)
            let s:tlist_winsize_chgd = 1
        else
            let s:tlist_winsize_chgd = 0
        endif

        if g:Tlist_Use_Right_Window
            " Open the window at the rightmost place
            let win_dir = 'botright vertical'
        else
            " Open the window at the leftmost place
            let win_dir = 'topleft vertical'
        endif
        let win_size = g:Tlist_WinWidth
    endif

    " If the tag listing temporary buffer already exists, then reuse it.
    " Otherwise create a new buffer
    let bufnum = bufnr(g:TagList_title)
    if bufnum == -1
        " Create a new buffer
        let wcmd = g:TagList_title
    else
        " Edit the existing buffer
        let wcmd = '+buffer' . bufnum
    endif

    " Create the taglist window
    exe 'silent! ' . win_dir . ' ' . win_size . 'split ' . wcmd
endfunction

" Tlist_Zoom_Window
" Zoom (maximize/minimize) the taglist window
function! s:Tlist_Zoom_Window()
    if s:tlist_win_maximized
        " Restore the window back to the previous size
        if g:Tlist_Use_Horiz_Window
            exe 'resize ' . g:Tlist_WinHeight
        else
            exe 'vert resize ' . g:Tlist_WinWidth
        endif
        let s:tlist_win_maximized = 0
    else
        " Set the window size to the maximum possible without closing other
        " windows
        if g:Tlist_Use_Horiz_Window
            resize
        else
            vert resize
        endif
        let s:tlist_win_maximized = 1
    endif
endfunction

" Tlist_Init_Window
" Set the default options for the taglist window
function! s:Tlist_Init_Window()
    " Define taglist window element highlighting
    if has('syntax')
        syntax match TagListComment '^" .*'
        syntax match TagListFileName '^[^" ].*$'
        syntax match TagListTitle '^  \S.*$'
        syntax match TagListTagScope  '\s\[.\{-\}\]$'

        " Define the highlighting only if colors are supported
        if has('gui_running') || &t_Co > 2
            " Colors to highlight various taglist window elements
            " If user defined highlighting group exists, then use them.
            " Otherwise, use default highlight groups.
            if hlexists('MyTagListTagName')
                highlight link TagListTagName MyTagListTagName
            else
                highlight link TagListTagName Search
            endif
            " Colors to highlight comments and titles
            if hlexists('MyTagListComment')
                highlight link TagListComment MyTagListComment
            else
                highlight clear TagListComment
                highlight link TagListComment Comment
            endif
            if hlexists('MyTagListTitle')
                highlight link TagListTitle MyTagListTitle
            else
                highlight clear TagListTitle
                highlight link TagListTitle Title
            endif
            if hlexists('MyTagListFileName')
                highlight link TagListFileName MyTagListFileName
            else
                highlight clear TagListFileName
                highlight link TagListFileName LineNr
            endif
            if hlexists('MyTagListTagScope')
                highlight link TagListTagScope MyTagListTagScope
            else
                highlight clear TagListTagScope
                highlight link TagListTagScope Identifier
            endif
        else
            highlight TagListTagName term=reverse cterm=reverse
        endif
    endif

    " Folding related settings
    if has('folding')
        setlocal foldenable
        setlocal foldminlines=0
        setlocal foldmethod=manual
        if g:Tlist_Enable_Fold_Column
            setlocal foldcolumn=3
        else
            setlocal foldcolumn=0
        endif
        setlocal foldtext=v:folddashes.getline(v:foldstart)
    endif

    if s:tlist_app_name != "winmanager"
        " Mark buffer as scratch
        silent! setlocal buftype=nofile
        if s:tlist_app_name == "none"
            silent! setlocal bufhidden=delete
        endif
        silent! setlocal noswapfile
        " Due to a bug in Vim 6.0, the winbufnr() function fails for unlisted
        " buffers. So if the taglist buffer is unlisted, multiple taglist
        " windows will be opened. This bug is fixed in Vim 6.1 and above
        if v:version >= 601
            silent! setlocal nobuflisted
        endif
    endif

    silent! setlocal nowrap

    " If the 'number' option is set in the source window, it will affect the
    " taglist window. So forcefully disable 'number' option for the taglist
    " window
    silent! setlocal nonumber

    " Create buffer local mappings for jumping to the tags and sorting the list
    nnoremap <buffer> <silent> <CR> :call <SID>Tlist_Jump_To_Tag(0)<CR>
    nnoremap <buffer> <silent> o :call <SID>Tlist_Jump_To_Tag(1)<CR>
    nnoremap <buffer> <silent> p :call <SID>Tlist_Jump_To_Tag(2)<CR>
    nnoremap <buffer> <silent> <2-LeftMouse> :call <SID>Tlist_Jump_To_Tag(0)<CR>
    nnoremap <buffer> <silent> s :call <SID>Tlist_Change_Sort()<CR>
    nnoremap <buffer> <silent> + :silent! foldopen<CR>
    nnoremap <buffer> <silent> - :silent! foldclose<CR>
    nnoremap <buffer> <silent> * :silent! %foldopen!<CR>
    nnoremap <buffer> <silent> = :silent! %foldclose<CR>
    nnoremap <buffer> <silent> <kPlus> :silent! foldopen<CR>
    nnoremap <buffer> <silent> <kMinus> :silent! foldclose<CR>
    nnoremap <buffer> <silent> <kMultiply> :silent! %foldopen!<CR>
    nnoremap <buffer> <silent> <Space> :call <SID>Tlist_Show_Tag_Prototype()<CR>
    nnoremap <buffer> <silent> u :call <SID>Tlist_Update_Window()<CR>
    nnoremap <buffer> <silent> d :call <SID>Tlist_Remove_File(-1)<CR>
    nnoremap <buffer> <silent> x :call <SID>Tlist_Zoom_Window()<CR>
    nnoremap <buffer> <silent> [[ :call <SID>Tlist_Move_To_File(-1)<CR>
    nnoremap <buffer> <silent> ]] :call <SID>Tlist_Move_To_File(1)<CR>
    nnoremap <buffer> <silent> ? :call <SID>Tlist_Toggle_Help_Text()<CR>
    nnoremap <buffer> <silent> q :close<CR>

    " Insert mode mappings
    inoremap <buffer> <silent> <CR>    <C-o>:call <SID>Tlist_Jump_To_Tag(0)<CR>
    " Windows needs return
    inoremap <buffer> <silent> <Return> <C-o>:call <SID>Tlist_Jump_To_Tag(0)<CR>
    inoremap <buffer> <silent> o        <C-o>:call <SID>Tlist_Jump_To_Tag(1)<CR>
    inoremap <buffer> <silent> p        <C-o>:call <SID>Tlist_Jump_To_Tag(2)<CR>
    inoremap <buffer> <silent> <2-LeftMouse> <C-o>:call 
                                            \ <SID>Tlist_Jump_To_Tag(0)<CR>
    inoremap <buffer> <silent> s        <C-o>:call <SID>Tlist_Change_Sort()<CR>
    inoremap <buffer> <silent> +             <C-o>:silent! foldopen<CR>
    inoremap <buffer> <silent> -             <C-o>:silent! foldclose<CR>
    inoremap <buffer> <silent> *             <C-o>:silent! %foldopen!<CR>
    inoremap <buffer> <silent> =             <C-o>:silent! %foldclose<CR>
    inoremap <buffer> <silent> <kPlus>       <C-o>:silent! foldopen<CR>
    inoremap <buffer> <silent> <kMinus>      <C-o>:silent! foldclose<CR>
    inoremap <buffer> <silent> <kMultiply>   <C-o>:silent! %foldopen!<CR>
    inoremap <buffer> <silent> <Space>       <C-o>:call 
                                    \ <SID>Tlist_Show_Tag_Prototype()<CR>
    inoremap <buffer> <silent> u    <C-o>:call <SID>Tlist_Update_Window()<CR>
    inoremap <buffer> <silent> d    <C-o>:call <SID>Tlist_Remove_File(-1)<CR>
    inoremap <buffer> <silent> x    <C-o>:call <SID>Tlist_Zoom_Window()<CR>
    inoremap <buffer> <silent> [[   <C-o>:call <SID>Tlist_Move_To_File(-1)<CR>
    inoremap <buffer> <silent> ]]   <C-o>:call <SID>Tlist_Move_To_File(1)<CR>
    inoremap <buffer> <silent> ?    <C-o>:call <SID>Tlist_Toggle_Help_Text()<CR>
    inoremap <buffer> <silent> q    <C-o>:close<CR>

    " Map single left mouse click if the user wants this functionality
    if g:Tlist_Use_SingleClick
    nnoremap <silent> <LeftMouse> <LeftMouse>:if bufname("%") =~ "__Tag_List__"
                        \ <bar> call <SID>Tlist_Jump_To_Tag(0) <bar> endif <CR>
    endif

    " Define the taglist autocommands
    augroup TagListAutoCmds
        autocmd!
        " Display the tag prototype for the tag under the cursor.
        autocmd CursorHold __Tag_List__ call s:Tlist_Show_Tag_Prototype()
        " Highlight the current tag 
        autocmd CursorHold * silent call <SID>Tlist_Highlight_Tag(
                                \ fnamemodify(bufname('%'), ':p'), line('.'), 1)
        " Adjust the Vim window width when taglist window is closed
        autocmd BufUnload __Tag_List__ call <SID>Tlist_Post_Close_Cleanup()
        " Close the fold for this buffer when it's not visible in any window
        autocmd BufWinLeave * silent call <SID>Tlist_Update_File_Display(
                                \ fnamemodify(expand('<afile>'), ':p'), 1)
        " Remove the file from the list when it's buffer is deleted
        autocmd BufDelete * silent call <SID>Tlist_Update_File_Display(
                                \ fnamemodify(expand('<afile>'), ':p'), 2)
        " Exit Vim itself if only the taglist window is present (optional)
        autocmd BufEnter __Tag_List__ call <SID>Tlist_Check_Only_Window()
        if s:tlist_app_name != "winmanager" && !g:Tlist_Process_File_Always
            " Auto refresh the taglist window
            autocmd BufEnter * call <SID>Tlist_Refresh()
        endif
    augroup end
endfunction

" Tlist_Refresh_Window
" Display the tags for all the files in the taglist window
function! s:Tlist_Refresh_Window()
    " Set report option to a huge value to prevent informational messages
    " while deleting the lines
    let old_report = &report
    set report=99999

    " Mark the buffer as modifiable
    setlocal modifiable

    " Delete the contents of the buffer to the black-hole register
    silent! %delete _

    if g:Tlist_Compact_Format == 0
        " Display help in non-compact mode
        call s:Tlist_Display_Help()
    endif

    " Mark the buffer as not modifiable
    setlocal nomodifiable

    " Restore the report option
    let &report = old_report

    " List all the tags for the previously processed files
    let i = 0
    while i < s:tlist_file_count
        " Mark the file as not visible, so that Tlist_Explore_File() will
        " display the tags for this file and mark the file as visible
        let s:tlist_{i}_visible = 0
        call s:Tlist_Explore_File(s:tlist_{i}_filename, s:tlist_{i}_filetype)
        let i = i + 1
    endwhile

    " If Tlist_File_Fold_Auto_Close option is set, then close all the 
    " folds
    if g:Tlist_File_Fold_Auto_Close
        if has('folding')
            " Close all the folds
            silent! %foldclose
        endif
    endif
endfunction

" Tlist_Post_Close_Cleanup()
" Close the taglist window and adjust the Vim window width
function! s:Tlist_Post_Close_Cleanup()
    " Mark all the files as not visible
    let i = 0
    while i < s:tlist_file_count
        let s:tlist_{i}_visible = 0
        let i = i + 1
    endwhile

    " Remove the taglist autocommands
    silent! autocmd! TagListAutoCmds

    " Clear all the highlights
    match none

    if has('syntax')
        silent! syntax clear TagListTitle
        silent! syntax clear TagListComment
        silent! syntax clear TagListTagScope
    endif

    " Remove the left mouse click mapping if it was setup initially
    if g:Tlist_Use_SingleClick
        if hasmapto('<LeftMouse>')
            nunmap <LeftMouse>
        endif
    endif

    if s:tlist_app_name != "winmanager"
    if g:Tlist_Use_Horiz_Window || g:Tlist_Inc_Winwidth == 0 ||
                \ s:tlist_winsize_chgd == 0 ||
                \ &columns < (80 + g:Tlist_WinWidth)
        " No need to adjust window width if using horizontally split taglist
        " window or if columns is less than 101 or if the user chose not to
        " adjust the window width
    else
        " Adjust the Vim window width
        let &columns= &columns - (g:Tlist_WinWidth + 1)
    endif
    endif

    " Reset taglist state variables
    if s:tlist_app_name == "winmanager"
        let s:tlist_app_name = "none"
        let s:tlist_window_initialized = 0
    endif
endfunction

" Tlist_Check_Only_Window
" Check if only the taglist window is opened currently. If the
" Tlist_Exit_OnlyWindow variable is set, then close the taglist window
function! s:Tlist_Check_Only_Window()
    if g:Tlist_Exit_OnlyWindow
        if winbufnr(2) == -1 && bufname(winbufnr(1)) == g:TagList_title
            " If only the taglist window is currently open, then the buffer
            " number associated with window 2 will be -1.
            quit
        endif
    endif
endfunction

" Tlist_Explore_File()
" List the tags defined in the specified file in a Vim window
function! s:Tlist_Explore_File(filename, ftype)
    " First check whether the file already exists
    let fidx = s:Tlist_Get_File_Index(a:filename)
    if fidx != -1
        let file_exists = 1
    else
        let file_exists = 0
    endif

    if file_exists && s:tlist_{fidx}_visible
        " Check whether the file tags are currently valid
        if s:tlist_{fidx}_valid
            " Goto the first line in the file
            exe s:tlist_{fidx}_start

            " If the line is inside a fold, open the fold
            if has('folding')
                exe "silent! " . s:tlist_{fidx}_start . "," . 
                            \ s:tlist_{fidx}_end . "foldopen!"
            endif
            return
        endif

        " Discard and remove the tags for this file from display
        call s:Tlist_Discard_TagInfo(fidx)
        call s:Tlist_Remove_File_From_Display(fidx)
    endif

    " Process and generate a list of tags defined in the file
    if !file_exists || !s:tlist_{fidx}_valid
        let ret_fidx = s:Tlist_Process_File(a:filename, a:ftype)
        if ret_fidx == -1
            return
        endif
        let fidx = ret_fidx
    endif

    " Set report option to a huge value to prevent informational messages
    " while adding lines to the taglist window
    let old_report = &report
    set report=99999

    " Mark the buffer as modifiable
    setlocal modifiable

    " Add new files to the end of the window. For existing files, add them at
    " the same line where they were previously present. If the file is not
    " visible, then add it at the end
    if s:tlist_{fidx}_start == 0 || !s:tlist_{fidx}_visible
        if g:Tlist_Compact_Format
            let s:tlist_{fidx}_start = line('$')
        else
            let s:tlist_{fidx}_start = line('$') + 1
        endif
    endif

    let s:tlist_{fidx}_visible = 1

    " Goto the line where this file should be placed
    if g:Tlist_Compact_Format
        exe s:tlist_{fidx}_start
    else
        exe (s:tlist_{fidx}_start - 1)
    endif

    let txt = fnamemodify(s:tlist_{fidx}_filename, ':t') . ' (' .
                \ fnamemodify(s:tlist_{fidx}_filename, ':p:h') . ')'
    if g:Tlist_Compact_Format == 0
        silent! put =txt
    else
        silent! put! =txt
        " Move to the next line
        exe line('.') + 1
    endif
    let file_start = s:tlist_{fidx}_start

    " Add the tag names grouped by tag type to the buffer with a title
    let i = 1
    while i <= s:tlist_{a:ftype}_count
        let ttype = s:tlist_{a:ftype}_{i}_name
        " Add the tag type only if there are tags for that type
        if s:tlist_{fidx}_{ttype} != ''
            let txt = '  ' . s:tlist_{a:ftype}_{i}_fullname
            if g:Tlist_Compact_Format == 0
                let ttype_start_lnum = line('.') + 1
                silent! put =txt
            else
                let ttype_start_lnum = line('.')
                silent! put! =txt
            endif
            silent! put =s:tlist_{fidx}_{ttype}

            if g:Tlist_Compact_Format
                exe (line('.') + s:tlist_{fidx}_{ttype}_count)
            endif

            let s:tlist_{fidx}_{ttype}_start = ttype_start_lnum - file_start

            " create a fold for this tag type
            if has('folding')
                let fold_start = ttype_start_lnum
                let fold_end = fold_start + s:tlist_{fidx}_{ttype}_count
                exe fold_start . ',' . fold_end  . 'fold'
            endif

            if g:Tlist_Compact_Format == 0
                silent! put =''
            endif
        endif
        let i = i + 1
    endwhile

    if s:tlist_{fidx}_tag_count == 0
        put =''
    endif

    let s:tlist_{fidx}_end = line('.') - 1

    " Create a fold for the entire file
    if has('folding')
        exe s:tlist_{fidx}_start . ',' . s:tlist_{fidx}_end . 'fold'
        exe 'silent! ' . s:tlist_{fidx}_start . ',' . 
                                        \ s:tlist_{fidx}_end . 'foldopen!'
    endif

    " Goto the starting line for this file,
    exe s:tlist_{fidx}_start

    if s:tlist_app_name == "winmanager"
        " To handle a bug in the winmanager plugin, add a space at the
        " last line
        call setline('$', ' ')
    endif

    " Mark the buffer as not modifiable
    setlocal nomodifiable

    " Restore the report option
    let &report = old_report

    " Update the start and end line numbers for all the files following this
    " file
    let start = s:tlist_{fidx}_start
    " include the empty line after the last line
    if g:Tlist_Compact_Format
        let end = s:tlist_{fidx}_end
    else
        let end = s:tlist_{fidx}_end + 1
    endif
    call s:Tlist_Update_Line_Offsets(fidx + 1, 1, end - start + 1)

    return
endfunction

" Tlist_Init_File
" Initialize the variables for a new file
function! s:Tlist_Init_File(filename, ftype)
    " Add new files at the end of the list
    let fidx = s:tlist_file_count
    let s:tlist_file_count = s:tlist_file_count + 1

    " Initialize the file variables
    let s:tlist_{fidx}_filename = a:filename
    let s:tlist_{fidx}_sort_type = g:Tlist_Sort_Type
    let s:tlist_{fidx}_filetype = a:ftype
    let s:tlist_{fidx}_mtime = -1
    let s:tlist_{fidx}_start = 0
    let s:tlist_{fidx}_end = 0
    let s:tlist_{fidx}_valid = 0
    let s:tlist_{fidx}_visible = 0
    let s:tlist_{fidx}_tag_count = 0

    " Initialize the tag type variables
    let i = 1
    while i <= s:tlist_{a:ftype}_count
        let ttype = s:tlist_{a:ftype}_{i}_name
        let s:tlist_{fidx}_{ttype} = ''
        let s:tlist_{fidx}_{ttype}_start = 0
        let s:tlist_{fidx}_{ttype}_count = 0
        let i = i + 1
    endwhile

    return fidx
endfunction

" Tlist_Process_File
" Get the list of tags defined in the specified file and store them
" in Vim variables. Returns the file index where the tags are stored.
function! s:Tlist_Process_File(filename, ftype)
    " Check for valid filename and valid filetype
    if a:filename == '' || !filereadable(a:filename) || a:ftype == ''
        return -1
    endif

    " If the tag types for this filetype are not yet created, then create
    " them now
    let var = 's:tlist_' . a:ftype . '_count'
    if !exists(var)
        if s:Tlist_FileType_Init(a:ftype) == 0
            return -1
        endif
    endif

    " If this file is already processed, then use the cached values
    let fidx = s:Tlist_Get_File_Index(a:filename)
    if fidx == -1
        " First time, this file is loaded
        let fidx = s:Tlist_Init_File(a:filename, a:ftype)
    else
        " File was previously processed. Discard the tag information
        call s:Tlist_Discard_TagInfo(fidx)
    endif

    let s:tlist_{fidx}_valid = 1

    " Exuberant ctags arguments to generate a tag list
    let ctags_args = ' -f - --format=2 --excmd=pattern --fields=nks '

    " Form the ctags argument depending on the sort type 
    if s:tlist_{fidx}_sort_type == 'name'
        let ctags_args = ctags_args . ' --sort=yes '
    else
        let ctags_args = ctags_args . ' --sort=no '
    endif

    " Add the filetype specific arguments
    let ctags_args = ctags_args . ' ' . s:tlist_{a:ftype}_ctags_args

    " Ctags command to produce output with regexp for locating the tags
    let ctags_cmd = g:Tlist_Ctags_Cmd . ctags_args
    let ctags_cmd = ctags_cmd . ' "' . a:filename . '"'

    " In Windows 95, if not using cygwin, disable the 'shellslash'
    " option. Otherwise, this will cause problems when running the
    " ctags command.
    if has("win95") && !has("win32unix")
        let myshellslash = &shellslash
        set noshellslash
    endif

    " Run ctags and get the tag list
    let cmd_output = system(ctags_cmd)

    " Restore the value of the 'shellslash' option.
    if has("win95") && !has("win32unix")
        let &shellslash = myshellslash
    endif

    " Handle errors
    if v:shell_error && cmd_output != ''
        let msg = "Taglist: Failed to generate tags for " . a:filename
        call s:Tlist_Warning_Msg(msg)
        call s:Tlist_Warning_Msg(cmd_output)
        return fidx
    endif

    " No tags for current file
    if cmd_output == ''
        call s:Tlist_Warning_Msg('Taglist: No tags found for ' . a:filename)
        return fidx
    endif

    " Store the modification time for the file
    let s:tlist_{fidx}_mtime = getftime(a:filename)

    " Process the ctags output one line at a time. Separate the tag output
    " based on the tag type and store it in the tag type variable
    " The format of each line in the ctags output is:
    "
    "     tag_name<TAB>file_name<TAB>ex_cmd;"<TAB>extension_fields 
    "
    while cmd_output != ''
        " Extract one line at a time
        let idx = stridx(cmd_output, "\n")
        let one_line = strpart(cmd_output, 0, idx)
        " Remove the line from the tags output
        let cmd_output = strpart(cmd_output, idx + 1)

        if one_line == ''
            " Line is not in proper tags format
            continue
        endif

        " Extract the tag type
        let ttype = s:Tlist_Extract_Tagtype(one_line)

        if ttype == ''
            " Line is not in proper tags format
            continue
        endif

        " make sure the tag type is supported
        if s:tlist_{a:ftype}_ctags_flags !~# ttype
            " Tag type is not supported
            continue
        endif

        " Extract the tag name
        if g:Tlist_Display_Prototype == 0
            let ttxt = '    ' . strpart(one_line, 0, stridx(one_line, "\t"))

            " Add the tag scope, if it is available. Tag scope is the last
            " field after the 'line:<num>\t' field
            if g:Tlist_Display_Tag_Scope     " only if it is selected
                let tag_scope = s:Tlist_Extract_Tag_Scope(one_line)
                if tag_scope != ''
                    let ttxt = ttxt . ' [' . tag_scope . ']'
                endif
            endif
        else
            let ttxt = s:Tlist_Extract_Tag_Prototype(one_line)
        endif

        " Update the count of this tag type
        let cnt = s:tlist_{fidx}_{ttype}_count + 1
        let s:tlist_{fidx}_{ttype}_count = cnt

        " Add this tag to the tag type variable
        let s:tlist_{fidx}_{ttype} = s:tlist_{fidx}_{ttype} . ttxt . "\n"

        " Update the total tag count
        let s:tlist_{fidx}_tag_count = s:tlist_{fidx}_tag_count + 1
        " Store the ctags output line and the tagtype count
        let s:tlist_{fidx}_tag_{s:tlist_{fidx}_tag_count} = 
                                    \ cnt . ':' . one_line
        " Store the tag output index
        let s:tlist_{fidx}_{ttype}_{cnt} = s:tlist_{fidx}_tag_count
    endwhile

    return fidx
endfunction

" Tlist_Update_File_Tags
" Update the tags for a file (if needed)
function! Tlist_Update_File_Tags(filename, ftype)
    " If the file doesn't support tag listing, skip it
    if s:Tlist_Skip_File(a:filename, a:ftype)
        return
    endif

    " First check whether the file already exists
    let fidx = s:Tlist_Get_File_Index(a:filename)

    if fidx != -1 && s:tlist_{fidx}_valid
        " File exists and the tags are valid
        return
    endif

    " If the taglist window is opened, update it
    let winnum = bufwinnr(g:TagList_title)
    if winnum == -1
        " Taglist window is not present. Just update the taglist
        " and return
        call s:Tlist_Process_File(a:filename, a:ftype)
    else
        " Save the current window number
        let save_winnr = winnr()

        " Goto the taglist window
        call s:Tlist_Open_Window()

        " Update the taglist window
        call s:Tlist_Explore_File(a:filename, a:ftype)

        if winnr() != save_winnr
            " Go back to the original window
            exe save_winnr . 'wincmd w'
        endif
    endif
endfunction

" Tlist_Close_Window
" Close the taglist window
function! s:Tlist_Close_Window()
    " Make sure the taglist window exists
    let winnum = bufwinnr(g:TagList_title)
    if winnum == -1
        call s:Tlist_Warning_Msg('Error: Taglist window is not open')
        return
    endif

    if winnr() == winnum
        " Already in the taglist window. Close it and return
        if winbufnr(2) != -1
            " If a window other than the taglist window is open,
            " then only close the taglist window.
            close
        endif
    else
        " Goto the taglist window, close it and then come back to the
        " original window
        let curbufnr = bufnr('%')
        exe winnum . 'wincmd w'
        close
        " Need to jump back to the original window only if we are not
        " already in that window
        let winnum = bufwinnr(curbufnr)
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
    endif
endfunction

" Tlist_Toggle_Window()
" Open or close a taglist window
function! s:Tlist_Toggle_Window()
    let curline = line('.')

    " If taglist window is open then close it.
    let winnum = bufwinnr(g:TagList_title)
    if winnum != -1
        call s:Tlist_Close_Window()
        return
    endif

    if s:tlist_app_name == "winmanager"
        " Taglist plugin is no longer part of the winmanager app
        let s:tlist_app_name = "none"
    endif

    " Get the filename and filetype for the specified buffer
    let curbuf_name = fnamemodify(bufname('%'), ':p')
    let curbuf_ftype = getbufvar('%', '&filetype')

    " Mark the current window as the desired window to open a file
    " when a tag is selcted
    let w:tlist_file_window = "yes"

    " Open the taglist window
    call s:Tlist_Open_Window()

    " Initialize the taglist window
    call s:Tlist_Init_Window()
    call s:Tlist_Refresh_Window()

    " Add and list the tags for all the buffers in the bufferlist
    let i = 1
    while i < bufnr('$')
        let fname = fnamemodify(bufname(i), ':p')
        let ftype = getbufvar(i, '&filetype')
        call s:Tlist_Explore_File(fname, ftype)
        let i = i + 1
    endwhile

    " Highlight the current tag
    call s:Tlist_Highlight_Tag(curbuf_name, curline, 1)

    " Go back to the original window
    let prev_Tlist_Skip_Refresh = s:Tlist_Skip_Refresh
    let s:Tlist_Skip_Refresh = 1
    wincmd p
    let s:Tlist_Skip_Refresh = prev_Tlist_Skip_Refresh
endfunction

" Tlist_Extract_Tagtype
" Extract the tag type from the tag text
function! s:Tlist_Extract_Tagtype(tag_txt)
    " The tag type is after the tag prototype field. The prototype field
    " ends with the /;"\t string. We add 4 at the end to skip the characters
    " in this special string..
    let start = strridx(a:tag_txt, '/;"' . "\t") + 4
    let end = strridx(a:tag_txt, 'line:') - 1
    let ttype = strpart(a:tag_txt, start, end - start)

    return ttype
endfunction

" Tlist_Extract_Tag_Prototype
" Extract the tag protoype from the tag text
function! s:Tlist_Extract_Tag_Prototype(tag_txt)
    let start = stridx(a:tag_txt, '/^') + 2
    let end = strridx(a:tag_txt, '/;"' . "\t")
    " The search patterns for some tag types doesn't end with 
    " the ;" character
    if a:tag_txt[end - 1] == '$'
        let end = end -1
    endif
    let tag_pat = strpart(a:tag_txt, start, end - start)

    " Remove all the leading space characters
    let tag_pat = substitute(tag_pat, '\s*', '', '')

    return tag_pat
endfunction

" Tlist_Extract_Tag_Scope
" Extract the tag scope from the tag text
function! s:Tlist_Extract_Tag_Scope(tag_txt)
    let start = strridx(a:tag_txt, 'line:')
    let end = strridx(a:tag_txt, "\t")
    if end <= start
        return ''
    endif

    let tag_scope = strpart(a:tag_txt, end + 1)
    let tag_scope = strpart(tag_scope, stridx(tag_scope, ':') + 1)

    return tag_scope
endfunction

" Tlist_Refresh()
" Refresh the taglist
function! s:Tlist_Refresh()
    " If we are entering the buffer from one of the taglist functions, then
    " no need to refresh the taglist window again.
    if s:Tlist_Skip_Refresh || (s:tlist_app_name == "winmanager")
        return
    endif

    " Skip buffers with 'buftype' set to nofile, nowrite, quickfix or help
    if &buftype != ''
        return
    endif

    let filename = fnamemodify(bufname('%'), ':p')
    let ftype = &filetype

    " If the file doesn't support tag listing, skip it
    if s:Tlist_Skip_File(filename, ftype)
        return
    endif

    let curline = line('.')

    " Make sure the taglist window is open. Otherwise, no need to refresh
    let winnum = bufwinnr(g:TagList_title)
    if winnum == -1
        if g:Tlist_Process_File_Always
            call Tlist_Update_File_Tags(filename, ftype)
        endif
        return
    endif

    let fidx = s:Tlist_Get_File_Index(filename)
    if fidx != -1
        let mtime = getftime(filename)
        if s:tlist_{fidx}_mtime != mtime
            " Invalidate the tags listed for this file
            let s:tlist_{fidx}_valid = 0

            " Update the taglist window
            call Tlist_Update_File_Tags(s:tlist_{fidx}_filename, 
                        \ s:tlist_{fidx}_filetype)

            " Store the new file modification time
            let s:tlist_{fidx}_mtime = mtime
        endif

        " If the tag listing for the current window is already present, no
        " need to refresh it
        if !g:Tlist_Auto_Highlight_Tag
            return
        endif

        " Highlight the current tag
        call s:Tlist_Highlight_Tag(filename, curline, 1)

        return
    endif

    " Save the current window number
    let cur_winnr = winnr()

    " Goto the taglist window
    call s:Tlist_Open_Window()

    if !g:Tlist_Auto_Highlight_Tag
        " Save the cursor position
        let save_line = line('.')
        let save_col = col('.')
    endif

    " Update the taglist window
    call s:Tlist_Explore_File(filename, ftype)

    " Highlight the current tag
    call s:Tlist_Highlight_Tag(filename, curline, 1)

    if !g:Tlist_Auto_Highlight_Tag
        " Restore the cursor position
        call cursor(save_line, save_col) 
    endif 

    " Refresh the taglist window
    redraw

    if s:tlist_app_name != "winmanager"
    " Jump back to the original window
    exe cur_winnr . 'wincmd w'
    endif
endfunction

" Tlist_Change_Sort()
" Change the sort order of the tag listing
function! s:Tlist_Change_Sort()
    let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
    if fidx == -1
        return
    endif

    " Remove the previous highlighting
    match none

    let sort_type = s:tlist_{fidx}_sort_type

    " Toggle the sort order from 'name' to 'order' and vice versa
    if sort_type == 'name'
        let s:tlist_{fidx}_sort_type = 'order'
    else
        let s:tlist_{fidx}_sort_type = 'name'
    endif

    " Save the current line for later restoration
    let curline = '\V\^' . getline('.') . '\$'

    " Invalidate the tags listed for this file
    let s:tlist_{fidx}_valid = 0

    call s:Tlist_Explore_File(s:tlist_{fidx}_filename, s:tlist_{fidx}_filetype)

    " Go back to the cursor line before the tag list is sorted
    call search(curline, 'w')
endfunction

" Tlist_Update_Tags()
" Update taglist for the current buffer by regenerating the tag list
" Contributed by WEN Guopeng.
function! s:Tlist_Update_Tags()
    if winnr() == bufwinnr(g:TagList_title)
        " In the taglist window. Update the current file
        call s:Tlist_Update_Window()
        return
    else
        " Not in the taglist window. Update the current buffer
        let filename = fnamemodify(bufname('%'), ':p')
        let fidx = s:Tlist_Get_File_Index(filename)
        if fidx != -1
            let s:tlist_{fidx}_valid = 0
        endif
        call Tlist_Update_File_Tags(filename, &filetype)
    endif
endfunction

" Tlist_Update_Window()
" Update the window by regenerating the tag list
function! s:Tlist_Update_Window()
    let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
    if fidx == -1
        return
    endif

    " Remove the previous highlighting
    match none

    " Save the current line for later restoration
    let curline = '\V\^' . getline('.') . '\$'

    let s:tlist_{fidx}_valid = 0

    " Update the taglist window
    call s:Tlist_Explore_File(s:tlist_{fidx}_filename, s:tlist_{fidx}_filetype)

    " Go back to the tag line before the list is updated
    call search(curline, 'w')
endfunction

" Tlist_Get_Tag_Index()
" Return the tag index for the current line
function! s:Tlist_Get_Tag_Index(fidx)
    let lnum = line('.')
    let ftype = s:tlist_{a:fidx}_filetype

    " Determine to which tag type the current line number belongs to using the
    " tag type start line number and the number of tags in a tag type
    let i = 1
    while i <= s:tlist_{ftype}_count
        let ttype = s:tlist_{ftype}_{i}_name
        let start_lnum = s:tlist_{a:fidx}_start + s:tlist_{a:fidx}_{ttype}_start
        let end =  start_lnum + s:tlist_{a:fidx}_{ttype}_count
        if lnum >= start_lnum && lnum <= end
            break
        endif
        let i = i + 1
    endwhile

    " Current line doesn't belong to any of the displayed tag types
    if i > s:tlist_{ftype}_count
        return 0
    endif

    " Compute the index into the displayed tags for the tag type
    let tidx = lnum - start_lnum
    if tidx == 0
        return 0
    endif

    " Get the corresponding tag line and return it
    return s:tlist_{a:fidx}_{ttype}_{tidx}
endfunction

" Tlist_Highlight_Tagline
" Higlight the current tagline
function! s:Tlist_Highlight_Tagline()
    " Clear previously selected name
    match none

    " Highlight the current selected name
    if g:Tlist_Display_Prototype == 0
        exe 'match TagListTagName /\%' . line('.') . 'l\s\+\zs.*/'
    else
        exe 'match TagListTagName /\%' . line('.') . 'l.*/'
    endif
endfunction

" Tlist_Jump_To_Tag()
" Jump to the location of the current tag
" win_ctrl == 0 - Reuse the existing file window
" win_ctrl == 1 - Open a new window
" win_ctrl == 2 - Preview the tag
function! s:Tlist_Jump_To_Tag(win_ctrl)
    " Do not process comment lines and empty lines
    let curline = getline('.')
    if curline =~ '^\s*$' || curline[0] == '"'
        return
    endif

    " If inside a fold, then don't try to jump to the tag
    if foldclosed('.') != -1
        return
    endif

    let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
    if fidx == -1
        return
    endif

    " Get the tag output for the current tag
    let tidx = s:Tlist_Get_Tag_Index(fidx)
    if tidx != 0
        let mtxt = s:tlist_{fidx}_tag_{tidx}
        let start = stridx(mtxt, '/^') + 2
        let end = strridx(mtxt, '/;"' . "\t")
        if mtxt[end - 1] == '$'
            let end = end - 1
        endif
        let tagpat = '\V\^' . strpart(mtxt, start, end - start) .
                                            \ (mtxt[end] == '$' ? '\$' : '')

        " Highlight the tagline
        call s:Tlist_Highlight_Tagline()
    else
        " Selected a line which is not a tag name. Just edit the file
        let tagpat = ''
    endif

    call s:Tlist_Open_File(a:win_ctrl, s:tlist_{fidx}_filename, tagpat)
endfunction

" Tlist_Open_File
" Open the specified file in either a new window or an existing window
" and place the cursor at the specified tag pattern
function! s:Tlist_Open_File(win_ctrl, filename, tagpat)
    let prev_Tlist_Skip_Refresh = s:Tlist_Skip_Refresh
    let s:Tlist_Skip_Refresh = 1

    if s:tlist_app_name == "winmanager"
        " Let the winmanager edit the file
        call WinManagerFileEdit(a:filename, a:win_ctrl)
    else
    " Goto the window containing the file.  If the window is not there, open a
    " new window
    let winnum = bufwinnr(a:filename)
    if winnum == -1
        " Locate the previously used window for opening a file
        let fwin_num = 0

        let i = 1
        while winbufnr(i) != -1
            if getwinvar(i, 'tlist_file_window') == "yes"
                let fwin_num = i
                break
            endif
            let i = i + 1
        endwhile

        if fwin_num != 0
            " Jump to the file window
            exe fwin_num . "wincmd w"

            " If the user asked to jump to the tag in a new window, then split
            " the existing window into two.
            if a:win_ctrl == 1
                split
            endif
            exe "edit " . a:filename
        else
            " Open a new window
            if g:Tlist_Use_Horiz_Window
                exe 'leftabove split #' . bufnr(a:filename)
                " Go to the taglist window to change the window size to the user
                " configured value
                wincmd p
                exe 'resize ' . g:Tlist_WinHeight
                " Go back to the file window
                wincmd p
            else
                " Open the file in a window and skip refreshing the taglist
                " window
                exe 'rightbelow vertical split #' . bufnr(a:filename)
                " Go to the taglist window to change the window size to the user
                " configured value
                wincmd p
                exe 'vertical resize ' . g:Tlist_WinWidth
                " Go back to the file window
                wincmd p
            endif
            let w:tlist_file_window = "yes"
        endif
    else
        exe winnum . 'wincmd w'

        " If the user asked to jump to the tag in a new window, then split the
        " existing window into two.
        if a:win_ctrl == 1
            split
        endif
    endif
    endif

    " Jump to the tag
    if a:tagpat != ''
        silent call search(a:tagpat, 'w')
    endif

    " Bring the line to the middle of the window
    normal! z.

    " If the line is inside a fold, open the fold
    if has('folding')
        if foldlevel('.') != 0
            normal! zv
        endif
    endif

    " If the user selects to preview the tag then jump back to the
    " taglist window
    if a:win_ctrl == 2
        " Go back to the taglist window
        let winnum = bufwinnr(g:TagList_title)
        exe winnum . 'wincmd w'
    endif

    let s:Tlist_Skip_Refresh = prev_Tlist_Skip_Refresh
endfunction

" Tlist_Show_Tag_Prototype()
" Display the prototype of the tag under the cursor
function! s:Tlist_Show_Tag_Prototype()
    " If we have already display prototype in the tag window, no need to
    " display it in the status line
    if g:Tlist_Display_Prototype
        return
    endif

    " Clear the previously displayed line
    echo

    " Do not process comment lines and empty lines
    let curline = getline('.')
    if curline =~ '^\s*$' || curline[0] == '"'
        return
    endif

    " If inside a fold, then don't display the prototype
    if foldclosed('.') != -1
        return
    endif

    " Get the file index
    let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
    if fidx == -1
        return
    endif

    " Get the tag output line for the current tag
    let tidx = s:Tlist_Get_Tag_Index(fidx)
    if tidx == 0
        return
    endif

    let mtxt = s:tlist_{fidx}_tag_{tidx}

    " Get the tag search pattern and display it
    echo s:Tlist_Extract_Tag_Prototype(mtxt)
endfunction

" Tlist_Find_Tag_text
" Find the tag text given the line number in the source window
function! s:Tlist_Find_Tag_text(fidx, linenum)
    let sort_type = s:tlist_{a:fidx}_sort_type

    let left = 1
    let right = s:tlist_{a:fidx}_tag_count

    if sort_type == 'order'
        " Tag list sorted by order, do a binary search comparing the line
        " numbers and pick a tag entry that contains the current line and
        " highlight it.  The idea behind this function is taken from the
        " ctags.vim script (by Alexey Marinichev) available at the Vim online
        " website.

        " If the current line is the less than the first tag, then no need to
        " search
        let txt = s:tlist_{a:fidx}_tag_1
        let start = strridx(txt, 'line:') + strlen('line:')
        let end = strridx(txt, "\t")
        if end < start
            let first_lnum = strpart(txt, start) + 0
        else
            let first_lnum = strpart(txt, start, end - start) + 0
        endif

        if a:linenum < first_lnum
            return ""
        endif

        while left < right
            let middle = (right + left + 1) / 2
            let txt = s:tlist_{a:fidx}_tag_{middle}

            let start = strridx(txt, 'line:') + strlen('line:')
            let end = strridx(txt, "\t")
            if end < start
                let middle_lnum = strpart(txt, start) + 0
            else
                let middle_lnum = strpart(txt, start, end - start) + 0
            endif

            if middle_lnum == a:linenum
                let left = middle
                break
            endif

            if middle_lnum > a:linenum
                let right = middle - 1
            else
                let left = middle
            endif
        endwhile
    else
        " sorted by name, brute force method (Dave Eggum)
        let closest_lnum = 0
        let final_left = 0
        while left < right
            let txt = s:tlist_{a:fidx}_tag_{left}

            let start = strridx(txt, 'line:') + strlen('line:')
            let end = strridx(txt, "\t")
            if end < start
                let lnum = strpart(txt, start) + 0
            else
                let lnum = strpart(txt, start, end - start) + 0
            endif

            if lnum < a:linenum && lnum > closest_lnum
                let closest_lnum = lnum
                let final_left = left
            elseif lnum == a:linenum
                let closest_lnum = lnum
                break
            else
                let left = left + 1
            endif
        endwhile
        if closest_lnum == 0
            return ""
        endif
        if left == right
            let left = final_left
        endif
    endif

    return s:tlist_{a:fidx}_tag_{left}
endfunction

" Tlist_Highlight_Tag()
" Highlight the current tag
" cntx == 1, Called by the taglist plugin itself
" cntx == 2, Forced by the user through the TlistSync command
function! s:Tlist_Highlight_Tag(filename, curline, cntx)
    " Highlight the current tag only if the user configured the
    " taglist plugin to do so or if the user explictly invoked the
    " command to highlight the current tag.
    if !g:Tlist_Auto_Highlight_Tag && a:cntx == 1
        return
    endif

    if a:filename == ''
        return
    endif

    " Make sure the taglist window is present
    let winnum = bufwinnr(g:TagList_title)
    if winnum == -1
        call s:Tlist_Warning_Msg('Error: Taglist window is not open')
        return
    endif

    let fidx = s:Tlist_Get_File_Index(a:filename)
    if fidx == -1
        return
    endif

    " If there are no tags for this file, then no need to proceed further
    if s:tlist_{fidx}_tag_count == 0
        return
    endif

    " If part of winmanager then disable winmanager autocommands
    if s:tlist_app_name == "winmanager"
        call WinManagerSuspendAUs()
    endif

    " Save the original window number
    let org_winnr = winnr()

    if org_winnr == winnum
        let in_taglist_window = 1
    else
        let in_taglist_window = 0
    endif

    " Go to the taglist window
    if !in_taglist_window
        exe winnum . 'wincmd w'
    endif

    " Clear previously selected name
    match none

    let tag_txt = s:Tlist_Find_Tag_text(fidx, a:curline)
    if tag_txt == ""
        " Make sure the current tag line is visible in the taglist window.
        " Calling the winline() function makes the line visible.  Don't know
        " of a better way to achieve this.
        let cur_lnum = line('.')

        if cur_lnum < s:tlist_{fidx}_start || cur_lnum > s:tlist_{fidx}_end
            " Move the cursor to the beginning of the file
            exe s:tlist_{fidx}_start
        endif

        if has('folding')
            normal! zv
        endif

        call winline()

        if !in_taglist_window
            let prev_Tlist_Skip_Refresh = s:Tlist_Skip_Refresh
            let s:Tlist_Skip_Refresh = 1
            exe org_winnr . 'wincmd w'
            let s:Tlist_Skip_Refresh = prev_Tlist_Skip_Refresh
        endif
        if s:tlist_app_name == "winmanager"
            call WinManagerResumeAUs()
        endif
        return
    endif

    " Extract the tag type
    let ttype = s:Tlist_Extract_Tagtype(tag_txt)

    " Extract the tag offset
    let offset = strpart(tag_txt, 0, stridx(tag_txt, ':')) + 0

    " Compute the line number
    let lnum = s:tlist_{fidx}_start + s:tlist_{fidx}_{ttype}_start + offset

    " Goto the line containing the tag
    exe lnum

    " Open the fold
    if has('folding')
        normal! zv
    endif

    " Make sure the current tag line is visible in the taglist window.
    " Calling the winline() function makes the line visible.  Don't know
    " of a better way to achieve this.
    call winline()

    " Highlight the tag name
    call s:Tlist_Highlight_Tagline()

    " Go back to the original window
    if !in_taglist_window
        let prev_Tlist_Skip_Refresh = s:Tlist_Skip_Refresh
        let s:Tlist_Skip_Refresh = 1
        exe org_winnr . 'wincmd w'
        let s:Tlist_Skip_Refresh = prev_Tlist_Skip_Refresh
    endif

    if s:tlist_app_name == "winmanager"
        call WinManagerResumeAUs()
    endif

    return
endfunction

" Tlist_Get_Tag_Prototype_By_Line
" Get the prototype for the tag on or before the specified line number in the
" current buffer
function! Tlist_Get_Tag_Prototype_By_Line(...)
    if a:0 == 0
        " Arguments are not supplied. Use the current buffer name
        " and line number
        let filename = bufname('%')
        let linenr = line('.')
    elseif a:0 == 2
        " Filename and line number are specified
        let filename = a:1
        let linenr = a:2
        if linenr !~ '\d\+'
            " Invalid line number
            return ""
        endif
    else
        " Sufficient arguments are not supplied
        let msg =  'Usage: Tlist_Get_Tag_Prototype_By_Line <filename> ' .
                                \ '<line_number>'
        call s:Tlist_Warning_Msg(msg)
        return ""
    endif

    " Expand the file to a fully qualified name
    let filename = fnamemodify(filename, ':p')
    if filename == ''
        return ""
    endif

    let fidx = s:Tlist_Get_File_Index(filename)
    if fidx == -1
        return ""
    endif

    " If there are no tags for this file, then no need to proceed further
    if s:tlist_{fidx}_tag_count == 0
        return ""
    endif

    " Get the tag text using the line number
    let tag_txt = s:Tlist_Find_Tag_text(fidx, linenr)
    if tag_txt == ""
        return ""
    endif

    " Extract the tag search pattern and return it
    return s:Tlist_Extract_Tag_Prototype(tag_txt)
endfunction

" Tlist_Get_Tagname_By_Line
" Get the tag name on or before the specified line number in the
" current buffer
function! Tlist_Get_Tagname_By_Line(...)
    if a:0 == 0
        " Arguments are not supplied. Use the current buffer name
        " and line number
        let filename = bufname('%')
        let linenr = line('.')
    elseif a:0 == 2
        " Filename and line number are specified
        let filename = a:1
        let linenr = a:2
        if linenr !~ '\d\+'
            " Invalid line number
            return ""
        endif
    else
        " Sufficient arguments are not supplied
        let msg =  'Usage: Tlist_Get_Tagname_By_Line <filename> <line_number>'
        call s:Tlist_Warning_Msg(msg)
        return ""
    endif

    " Make sure the current file has a name
    let filename = fnamemodify(filename, ':p')
    if filename == ''
        return ""
    endif

    let fidx = s:Tlist_Get_File_Index(filename)
    if fidx == -1
        return ""
    endif

    " If there are no tags for this file, then no need to proceed further
    if s:tlist_{fidx}_tag_count == 0
        return ""
    endif

    " Get the tag name using the line number
    let tag_txt = s:Tlist_Find_Tag_text(fidx, linenr)
    if tag_txt == ""
        return ""
    endif

    " Remove the line number at the beginning
    let start = stridx(tag_txt, ':')  + 1

    " Extract the tag name and return it
    return strpart(tag_txt, start, stridx(tag_txt, "\t") - start)
endfunction

" Tlist_Move_To_File
" Move the cursor to the beginning of the current file or the next file
" or the previous file in the taglist window
" dir == -1, move to start of current or previous function
" dir == 1, move to start of next function
function! s:Tlist_Move_To_File(dir)
    if foldlevel('.') == 0
        " Cursor is on a non-folded line (it is not in any of the files)
        " Move it to a folded line
        if a:dir == -1
            normal! zk
        else
            " While moving down to the start of the next fold,
            " no need to do go to the start of the next file.
            normal! zj
            return
        endif
    endif

    let fidx = s:Tlist_Get_File_Index_By_Linenum(line('.'))
    if fidx == -1
        return
    endif

    let cur_lnum = line('.')

    if a:dir == -1
        if cur_lnum > s:tlist_{fidx}_start
            " Move to the beginning of the current file
            exe s:tlist_{fidx}_start
            return
        endif

        if fidx == 0
            " At the first file, can't move to previous file
            return
        endif

        " Otherwise, move to the beginning of the previous file
        let fidx = fidx - 1
        exe s:tlist_{fidx}_start
        return
    else
        let fidx = fidx + 1

        if fidx == s:tlist_file_count
            " At the last file, can't move to the next file
            return
        endif

        " Otherwise, move to the beginning of the next file
        exe s:tlist_{fidx}_start
        return
    endif
endfunction

" Tlist_Session_Load
" Load a taglist session (information about all the displayed files
" and the tags) from the specified file
function! s:Tlist_Session_Load(...)
    if a:0 == 0 || a:1 == ''
        call s:Tlist_Warning_Msg('Usage: TlistSessionLoad <filename>')
        return
    endif

    let sessionfile = a:1

    if !filereadable(sessionfile)
        let msg = 'Taglist: Error - Unable to open file ' . sessionfile
        call s:Tlist_Warning_Msg(msg)
        return
    endif

    " Mark the current window as the file window
    if bufname('%') !~ g:TagList_title
        let w:tlist_file_window = "yes"
    endif

    " Open to the taglist window
    call s:Tlist_Open_Window()

    " Source the session file
    exe 'source ' . sessionfile

    let new_file_count = g:tlist_file_count
    unlet! g:tlist_file_count

    let i = 0
    while i < new_file_count
        let ftype = g:tlist_{i}_filetype
        unlet! g:tlist_{i}_filetype

        if !exists("s:tlist_" . ftype . "_count")
            if s:Tlist_FileType_Init(ftype) == 0
                let i = i + 1
                continue
            endif
        endif

        let fname = g:tlist_{i}_filename
        unlet! g:tlist_{i}_filename

        let fidx = s:Tlist_Get_File_Index(fname)
        if fidx != -1
            let s:tlist_{fidx}_visible = 0
            let i = i + 1
            continue
        endif

        let fidx = s:Tlist_Init_File(fname, ftype)

        let s:tlist_{fidx}_filename = fname

        let s:tlist_{fidx}_sort_type = g:tlist_{i}_sort_type
        unlet! g:tlist_{i}_sort_type

        let s:tlist_{fidx}_filetype = ftype
        let s:tlist_{fidx}_mtime = getftime(fname)

        let s:tlist_{fidx}_start = 0
        let s:tlist_{fidx}_end = 0

        let s:tlist_{fidx}_valid = 1
        " Mark the file as not visible, so that Tlist_Init_Window() function
        " will display the tags for this file
        let s:tlist_{fidx}_visible = 0

        let s:tlist_{fidx}_tag_count = g:tlist_{i}_tag_count
        unlet! g:tlist_{i}_tag_count

        let j = 1
        while j <= s:tlist_{fidx}_tag_count
            let s:tlist_{fidx}_tag_{j} = g:tlist_{i}_tag_{j}
            unlet! g:tlist_{i}_tag_{j}
            let j = j + 1
        endwhile

        let j = 1
        while j <= s:tlist_{ftype}_count
            let ttype = s:tlist_{ftype}_{j}_name

            if exists('g:tlist_' . i . '_' . ttype)
                let s:tlist_{fidx}_{ttype} = g:tlist_{i}_{ttype}
                unlet! g:tlist_{i}_{ttype}
                let s:tlist_{fidx}_{ttype}_start = 0
                let s:tlist_{fidx}_{ttype}_count = g:tlist_{i}_{ttype}_count
                unlet! g:tlist_{i}_{ttype}_count

                let k = 1
                while k <= s:tlist_{fidx}_{ttype}_count
                    let s:tlist_{fidx}_{ttype}_{k} = g:tlist_{i}_{ttype}_{k}
                    unlet! g:tlist_{i}_{ttype}_{k}
                    let k = k + 1
                endwhile
            else
                let s:tlist_{fidx}_{ttype} = ''
                let s:tlist_{fidx}_{ttype}_start = 0
                let s:tlist_{fidx}_{ttype}_count = 0
            endif

            let j = j + 1
        endwhile

        let i = i + 1
    endwhile

    " Initialize the taglist window
    call s:Tlist_Init_Window()
    call s:Tlist_Refresh_Window()

    if s:tlist_file_count > 0
        " Jump to the beginning of the first file
        call cursor(s:tlist_0_start, 1)
    endif
endfunction

" Tlist_Session_Save
" Save a taglist session (information about all the displayed files
" and the tags) into the specified file
function! s:Tlist_Session_Save(...)
    if a:0 == 0 || a:1 == ''
        call s:Tlist_Warning_Msg('Usage: TlistSessionSave <filename>')
        return
    endif

    let sessionfile = a:1

    if s:tlist_file_count == 0
        " There is nothing to save
        call s:Tlist_Warning_Msg('Warning: Taglist is empty. Nothing to save.')
        return
    endif

    if filereadable(sessionfile)
        let ans = input("Do you want to overwrite " . sessionfile . " (Y/N)?")
        if ans !=? 'y'
            return
        endif

        echo "\n"
    endif

    exe 'redir! > ' . sessionfile

    silent! echo '" Taglist session file. This file is auto-generated.'
    silent! echo '" File information'
    silent! echo 'let tlist_file_count = ' . s:tlist_file_count

    let i = 0

    while i < s:tlist_file_count
        " Store information about the file
        silent! echo 'let tlist_' . i . "_filename = '" . 
                                            \ s:tlist_{i}_filename . "'"
        silent! echo 'let tlist_' . i . '_sort_type = "' . 
                                                \ s:tlist_{i}_sort_type . '"'
        silent! echo 'let tlist_' . i . '_filetype = "' . 
                                            \ s:tlist_{i}_filetype . '"'
        silent! echo 'let tlist_' . i . '_tag_count = ' . 
                                                        \ s:tlist_{i}_tag_count
        " Store information about all the tags
        let j = 1
        while j <= s:tlist_{i}_tag_count
            let txt = escape(s:tlist_{i}_tag_{j}, '"\\')
            silent! echo 'let tlist_' . i . '_tag_' . j . ' = "' . txt . '"'
            let j = j + 1
        endwhile

        " Store information about all the tags grouped by their type
        let ftype = s:tlist_{i}_filetype
        let j = 1
        while j <= s:tlist_{ftype}_count
            let ttype = s:tlist_{ftype}_{j}_name
            if s:tlist_{i}_{ttype}_count != 0
                let txt = substitute(s:tlist_{i}_{ttype}, "\n", "\\\\n", "g")
                silent! echo 'let tlist_' . i . '_' . ttype . ' = "' . 
                                                \ txt . '"'
                silent! echo 'let tlist_' . i . '_' . ttype . '_count = ' . 
                                                     \ s:tlist_{i}_{ttype}_count
                let k = 1
                while k <= s:tlist_{i}_{ttype}_count
                    silent! echo 'let tlist_' . i . '_' . ttype . '_' . k . 
                                \ ' = ' . s:tlist_{i}_{ttype}_{k}
                    let k = k + 1
                endwhile
            endif
            let j = j + 1
        endwhile

        silent! echo

        let i = i + 1
    endwhile

    redir END
endfunction

" Tlist_Update_File_Display
" Update a file displayed in the taglist window.
" action == 1, Close the fold for the file
" action == 2, Remove the file from the taglist window
function! s:Tlist_Update_File_Display(filename, action)
    " Make sure a valid filename is supplied
    if a:filename == ''
        return
    endif

    " Make sure the taglist window is present
    let winnum = bufwinnr(g:TagList_title)
    if winnum == -1
        call s:Tlist_Warning_Msg('Taglist: Error - Taglist window is not open')
        return
    endif

    " Save the original window number
    let org_winnr = winnr()
    if org_winnr == winnum
        let in_taglist_window = 1
    else
        let in_taglist_window = 0
    endif

    " Go to the taglist window
    if !in_taglist_window
        exe winnum . 'wincmd w'
    endif

    " Get tag list index of the specified file
    let idx = s:Tlist_Get_File_Index(a:filename)
    if idx != -1
        " Save the cursor position
        let save_lnum = line('.')

        " Perform the requested action on the file
        if a:action == 1
            " Close the fold for the file

            if g:Tlist_File_Fold_Auto_Close
                " Close the fold for the file
                if has('folding')
                    exe "silent! " . s:tlist_{idx}_start . "," . 
                                \ s:tlist_{idx}_end . "foldclose"
                endif
            endif
        elseif a:action == 2
            " Remove the file from the list
            call s:Tlist_Remove_File(idx)
        endif

        " Move the cursor to the original location
        exe save_lnum
    endif

    " Go back to the original window
    if !in_taglist_window
        let prev_Tlist_Skip_Refresh = s:Tlist_Skip_Refresh
        let s:Tlist_Skip_Refresh = 1
        exe org_winnr . 'wincmd w'
        let s:Tlist_Skip_Refresh = prev_Tlist_Skip_Refresh
    endif
endfunction

" Define the taglist autocommand to automatically open the taglist window on
" Vim startup
if g:Tlist_Auto_Open
    autocmd VimEnter * nested Tlist
endif

" Refresh the taglist
if g:Tlist_Process_File_Always
    autocmd BufEnter * call <SID>Tlist_Refresh()
endif

" Define the user commands to manage the taglist window
command! -nargs=0 Tlist call s:Tlist_Toggle_Window()
command! -nargs=0 TlistClose call s:Tlist_Close_Window()
command! -nargs=0 TlistUpdate call s:Tlist_Update_Tags()
command! -nargs=0 TlistSync call s:Tlist_Highlight_Tag(
                            \ fnamemodify(bufname('%'), ':p'), line('.'), 2)
command! -nargs=* -complete=buffer TlistShowPrototype 
            \ echo Tlist_Get_Tag_Prototype_By_Line(<f-args>)
command! -nargs=* -complete=buffer TlistShowTag 
            \ echo Tlist_Get_Tagname_By_Line(<f-args>)
command! -nargs=* -complete=file TlistSessionLoad 
            \ call s:Tlist_Session_Load(<q-args>)
command! -nargs=* -complete=file TlistSessionSave 
            \ call s:Tlist_Session_Save(<q-args>)

" Tlist_Set_App
" Set the name of the external plugin/application to which taglist
" belongs.
" Taglist plugin is part of another plugin like cream or winmanager.
function! Tlist_Set_App(name)
    if a:name == ""
        return
    endif

    let s:tlist_app_name = a:name
endfunction

" Winmanager integration

" Initialization required for integration with winmanager
function! TagList_Start()
    " If current buffer is not taglist buffer, then don't proceed
    if bufname('%') != '__Tag_List__'
        return
    endif

    call Tlist_Set_App("winmanager")

    " Get the current filename from the winmanager plugin
    let bufnum = WinManagerGetLastEditedFile()
    if bufnum != -1
        let filename = fnamemodify(bufname(bufnum), ':p')
        let ftype = getbufvar(bufnum, '&filetype')
    endif

    " Initialize the taglist window, if it is not already initialized
    if !exists("s:tlist_window_initialized") || !s:tlist_window_initialized
        call s:Tlist_Init_Window()
        call s:Tlist_Refresh_Window()
        let s:tlist_window_initialized = 1
    endif

    " Open the taglist window
    if bufnum != -1
        call s:Tlist_Explore_File(filename, ftype)
    endif
endfunction

function! TagList_IsValid()
    return 0
endfunction

function! TagList_WrapUp()
    return 0
endfunction

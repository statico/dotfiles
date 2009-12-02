" Vim syntax file
" Language:	Parrot IMCC
" Maintainer:	Luke Palmer <fibonaci@babylonia.flatirons.org>
" Modified: Joshua Isom
" Last Change:	Jan 6 2006

" For installation please read:
" :he filetypes
" :he syntax
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax clear

syn include @Pod syntax/pod.vim
syn region pirPod start="^=[a-z]" end="^=cut" keepend contains=@Pod fold

syn keyword pirType int float num string pmc
syn match   pirPMC  /\.\(Compiler\|Continuation\|Coroutine\|CSub\|NCI\|Eval\|Sub\|Scratchpad\)/
syn match   pirPMC  /\.\(BigInt\|Boolean\|Complex\|Float\|Integer\|PMC\|String\|Hash\)/
syn match   pirPMC  /\.\(Fixed\|Resizable\)\(Boolean\|Float\|Integer\|PMC\|String\)Array/
syn match   pirPMC  /\.\(IntList\|Iterator\|Key\|ManagedStruct\|UnManagedStruct\|Pointer\)/
syn match   pirPMC  /\.\(FloatVal\|Multi\|S\|String\)\?Array/
syn match   pirPMC  /\.Perl\(Array\|Env\|Hash\|Int\|Num\|Scalar\|String\|Undef\)/
syn match   pirPMC  /\.Parrot\(Class\|Interpreter\|IO\|Library\|Object\|Thread\)/
syn keyword pirPMC self

syn keyword pirOp   goto if unless global addr

syn match pirDirectiveSub    /\.\(sub\|end\s*$\)/
syn match pirDirectiveMacro  /\.\(macro\|endm\)/
syn match pirDirective  /\.\(pcc_sub\|emit\|eom\)/
syn match pirDirective  /\.\(local\|sym\|const\|lex\|global\|globalconst\)/
syn match pirDirective  /\.\(endnamespace\|namespace\)/
syn match pirDirective  /\.\(param\|arg\|return\|yield\)/
syn match pirDirective  /\.\(pragma\|HLL_map\|HLL\|include\|loadlib\)/
syn match pirDirective  /\.\(pcc_begin\|pcc_call\|pcc_end\|invocant\|meth_call\|nci_call\)/
syn match pirDirective  /\.\(pcc_begin_return\|pcc_end_return\)/
syn match pirDirective  /\.\(pcc_begin_yield\|pcc_end_yield\)/

syn match pirDirective  /:\(main\|method\|load\|init\|anon\|multi\|immediate\|outer\|lex\|vtable\)/
syn match pirDirective  /:\(flat\|slurpy\|optional\|opt_flag\|named\)/

" Macro invocation
syn match pirDirective  /\.\I\i*(/he=e-1


" pirWord before pirRegister
" FIXME :: in identifiers and labels
syn match pirWord           /[A-Za-z_][A-Za-z0-9_]*/
syn match pirComment        /#.*/
syn match pirLabel          /[A-Za-z0-9_]\+:/he=e-1
syn match pirRegister       /[INPS]\([12][0-9]\|3[01]\|[0-9]\)/
syn match pirDollarRegister /\$[INPS][0-9]\+/

syn match pirNumber         /[+-]\?[0-9]\+\(\.[0-9]*\([Ee][+-]\?[0-9]\+\)\?\)\?/
syn match pirNumber         /0[xX][0-9a-fA-F]\+/
syn match pirNumber         /0[oO][0-7]\+/
syn match pirNumber         /0[bB][01]\+/

syn region pirString start=/"/ skip=/\\"/ end=/"/ contains=pirStringSpecial
syn region pirString start=/<<"\z(\I\i*\)"/ end=/^\z1$/ contains=pirStringSpecial
syn region pirString start=/<<'\z(\I\i*\)'/ end=/^\z1$/
syn region pirString start=/'/ end=/'/
syn match  pirStringSpecial "\\\([abtnvfre\\"]\|\o\{1,3\}\|x{\x\{1,8\}}\|x\x\{1,2\}\|u\x\{4\}\|U\x\{8\}\|c[A-Z]\)" contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pasm_syntax_inits")
  if version < 508
    let did_pasm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pirPod             Comment
  HiLink pirWord            Normal
  HiLink pirComment         Comment
  HiLink pirLabel           Label
  HiLink pirRegister        Identifier
  HiLink pirDollarRegister  Identifier
  HiLink pirType            Type
  HiLink pirPMC             Type
  HiLink pirString          String
  HiLink pirStringSpecial   Special
  HiLink pirNumber          Number
  HiLink pirDirective       Macro
  HiLink pirDirectiveSub    Macro
  HiLink pirDirectiveMacro  Macro
  HiLink pirOp              Conditional

  delcommand HiLink
endif

let b:current_syntax = "pir"

" Folding rules
syn region foldManual  start=/^\s*#.*{{{/ end=/^\s*#.*}}}/ contains=ALL keepend fold
syn region foldMakro   start=/\.macro/ end=/\.endm/ contains=ALLBUT,pirDirectiveMacro keepend fold
syn region foldSub     start=/\.sub/ end=/\.end/ contains=ALLBUT,pirDirectiveSub,pirDirectiveMacro keepend fold
syn region foldIf      start=/^\s*if.*goto\s*\z(\I\i*\)\s*$/ end=/^\s*\z1:\s*$/ contains=ALLBUT,pirDirectiveSub,pirDirectiveMacro keepend fold
syn region foldUnless  start=/^\s*unless.*goto\s*\z(\I\i*\)\s*$/ end=/^\s*\z1:\s*$/ contains=ALLBUT,pirDirectiveSub,pirDirectiveMacro keepend fold

" Ops -- dynamically generated from ops2vim.pl
syn keyword pirOp band bands bnot n_bnot bnots n_bnots bor bors shl shr
syn keyword pirOp lsr rot bxor bxors eq eq_str eq_num eq_addr ne ne_str
syn keyword pirOp ne_num ne_addr lt lt_str lt_num le le_str le_num gt
syn keyword pirOp gt_str gt_num ge ge_str ge_num if_null unless_null cmp
syn keyword pirOp cmp_str cmp_num issame isntsame istrue isfalse isnull
syn keyword pirOp isgt isge isle islt iseq isne and not n_not or xor end
syn keyword pirOp noop cpu_ret check_events check_events__ wrapper__
syn keyword pirOp prederef__ reserved load_bytecode branch branch_cs bsr
syn keyword pirOp ret jsr jump enternative if unless invokecc invoke
syn keyword pirOp yield tailcall returncc newclosure set_args get_results
syn keyword pirOp get_params set_returns result_info set_addr get_addr
syn keyword pirOp push_eh clear_eh throw rethrow die exit pushmark
syn keyword pirOp popmark pushaction debug bounds profile trace gc_debug
syn keyword pirOp interpinfo warningson warningsoff errorson errorsoff
syn keyword pirOp runinterp getinterp sweep collect sweepoff sweepon
syn keyword pirOp collectoff collecton needs_destroy loadlib dlfunc dlvar
syn keyword pirOp compile compreg new_callback debug_init debug_load
syn keyword pirOp debug_break debug_print backtrace getline getfile
syn keyword pirOp print_newline gcd splice slice iter morph exec
syn keyword pirOp classname trap pow getclass new instantiate pmcinfo
syn keyword pirOp add_io_event need_finalize newpdd15class close fdopen
syn keyword pirOp getfd getstdin getstdout getstderr setstdout setstderr
syn keyword pirOp pioctl open print printerr read readline peek stat seek
syn keyword pirOp tell socket sockaddr connect recv send poll bind listen
syn keyword pirOp accept infix n_infix abs n_abs add cmod dec div fdiv
syn keyword pirOp ceil floor inc mod mul neg n_neg sub sqrt acos asec
syn keyword pirOp asin atan cos cosh exp ln log10 log2 sec sech sin sinh
syn keyword pirOp tan tanh lcm fact callmethodcc callmethod
syn keyword pirOp tailcallmethod addmethod can does isa newclass subclass
syn keyword pirOp get_class class addparent removeparent addrole
syn keyword pirOp addattribute removeattribute getattribute setattribute
syn keyword pirOp classoffset inspect covers exsec hav vers pic_infix__
syn keyword pirOp pic_inline_sub__ pic_get_params__ pic_set_returns__
syn keyword pirOp pic_callr__ typeof find_type valid_type get_repr
syn keyword pirOp find_method defined exists delete elements push pop
syn keyword pirOp unshift shift setprop getprop delprop prophash freeze
syn keyword pirOp thaw mmdvtregister mmdvtfind register unregister hash
syn keyword pirOp get_mro clone exchange set assign setref deref setp_ind
syn keyword pirOp setn_ind sets_ind seti_ind null cleari clearn clears
syn keyword pirOp clearp saveall restoreall entrytype depth lookback save
syn keyword pirOp savec restore rotate_up stm_start stm_validate
syn keyword pirOp stm_commit stm_wait stm_abort stm_depth ord chr chopn
syn keyword pirOp concat repeat length bytelength pin unpin substr
syn keyword pirOp substr_r index sprintf find_encoding stringinfo upcase
syn keyword pirOp downcase titlecase join split charset charsetname
syn keyword pirOp find_charset trans_charset encoding encodingname
syn keyword pirOp trans_encoding is_cclass find_cclass find_not_cclass
syn keyword pirOp escape compose spawnw err time gmtime localtime
syn keyword pirOp decodetime decodelocaltime sysinfo sleep sizeof
syn keyword pirOp store_lex find_lex get_namespace get_hll_namespace
syn keyword pirOp get_root_namespace get_global get_hll_global
syn keyword pirOp get_root_global set_global set_hll_global
syn keyword pirOp set_root_global store_global find_global find_name

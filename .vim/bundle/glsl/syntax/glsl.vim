" Vim syntax file the OpenGL Shading Language
" Language:     GLSL
" Author:       Nathan Cournia <nathan@cournia.com>
" Date:         June 30, 2004
" File Types:   .frag .vert .glsl .fp .vp
" Version:      1.10.00
" Notes:        Adapted from c.vim - Bram Moolenaar <bram.vim.org>
"               Adapted from cg.vim - Kevin Bjorke <kbjorke@nvidia.com>

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" a bunch of useful keywords
syn keyword         glslConditional     if else
syn keyword         glslStatement       break return continue discard
syn keyword         glslRepeat          while for do
syn keyword         glslTodo            contained TODO FIXME XXX

" glslCommentGroup allows adding matches for special things in comments
syn cluster         glslCommentGroup    contains=glslTodo

"catch errors caused by wrong parenthesis and brackets
syn cluster         glslParenGroup      contains=glslParenError,glslIncluded,glslSpecial,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslUserCont,glslUserLabel,glslBitField,glslCommentSkip,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom
if exists("c_no_bracket_error")
  syn region        glslParen           transparent start='(' end=')' contains=ALLBUT,@glslParenGroup,glslCppParen,glslCppString
  " glslCppParen: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppParen        transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@glslParenGroup,glslParen,glslString
  syn match         glslParenError      display ")"
  syn match         glslErrInParen      display contained "[{}]"
else
  syn region        glslParen           transparent start='(' end=')' contains=ALLBUT,@glslParenGroup,glslCppParen,glslErrInBracket,glslCppBracket,glslCppString
  " glslCppParen: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppParen        transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@glslParenGroup,glslErrInBracket,glslParen,glslBracket,glslString
  syn match         glslParenError      display "[\])]"
  syn match         glslErrInParen      display contained "[\]{}]"
  syn region        glslBracket         transparent start='\[' end=']' contains=ALLBUT,@glslParenGroup,glslErrInParen,glslCppParen,glslCppBracket,glslCppString
  " glslCppBracket: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppBracket      transparent start='\[' skip='\\$' excludenl end=']' end='$' contained contains=ALLBUT,@glslParenGroup,glslErrInParen,glslParen,glslBracket,glslString
  syn match         glslErrInBracket    display contained "[);{}]"
endif

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match           glslNumbers         display transparent "\<\d\|\.\d" contains=glslNumber,glslFloat,glslOctalError,glslOctal
" Same, but without octal error (for comments)
syn match           glslNumbersCom      display contained transparent "\<\d\|\.\d" contains=glslNumber,glslFloat,glslOctal
syn match           glslNumber          display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match           glslNumber          display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match           glslOctal           display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=glslOctalZero
syn match           glslOctalZero       display contained "\<0"
syn match           glslFloat           display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match           glslFloat           display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match           glslFloat           display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match           glslFloat           display contained "\d\+e[-+]\=\d\+[fl]\=\>"
" flag an octal number with wrong digits
syn match           glslOctalError      display contained "0\o*[89]\d*"
syn case match

if exists("c_comment_strings")
  " A comment can contain glslString, glslCharacter and glslNumber.
  " But a "*/" inside a glslString in a glslComment DOES end the comment!  So we
  " need to use a special type of glslString: glslCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as glslomment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match      glslCommentSkip     contained "^\s*\*\($\|\s\+\)"
  syntax region     glslCommentString   contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=glslSpecial,glslCommentSkip
  syntax region     glslComment2String  contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=glslSpecial
  syntax region     glslCommentL        start="//" skip="\\$" end="$" keepend contains=@glslCommentGroup,glslComment2String,glslCharacter,glslNumbersCom,glslSpaceError
  syntax region     glslComment         matchgroup=glslCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@glslCommentGroup,glslCommentStartError,glslCommentString,glslCharacter,glslNumbersCom,glslSpaceError
else
  syn region        glslCommentL        start="//" skip="\\$" end="$" keepend contains=@glslCommentGroup,glslSpaceError
  syn region        glslComment         matchgroup=glslCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@glslCommentGroup,glslCommentStartError,glslSpaceError
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match        glslCommentError        display "\*/"
syntax match        glslCommentStartError   display "/\*"me=e-1 contained

syn keyword        glslType                void
syn keyword        glslType                bool  bvec2 bvec3 bvec4
syn keyword        glslType                int   ivec2 ivec3 ivec4
syn keyword        glslType                float vec2  vec3  vec4
syn keyword        glslType                mat2  mat3  mat4
syn keyword        glslType                sampler1D sampler2D sampler3D samplerCUBE sampler1DShadow sampler2DShadow

syn keyword        glslStructure           struct

syn keyword        glslStorageClass        const attribute varying uniform
syn keyword        glslStorageClass        in out inout

syn keyword        glslConstant            __LINE__ __FILE__ __VERSION__

syn keyword        glslConstant            true false

syn region         glslPreCondit           start="^\s*#\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=glslComment,glslCppString,glslCharacter,glslCppParen,glslParenError,glslNumbers,glslCommentError,glslSpaceError
syn match          glslPreCondit           display "^\s*#\s*\(else\|endif\)\>"
syn region         glslCppOut              start="^\s*#\s*if\s\+0\+\>" end=".\|$" contains=glslCppOut2
syn region         glslCppOut2             contained start="0" end="^\s*#\s*\(endif\>\|else\>\|elif\>\)" contains=glslSpaceError,glslCppSkip
syn region         glslCppSkip             contained start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*#\s*endif\>" contains=glslSpaceError,glslCppSkip
"syn match glslLineSkip        "\\$"
syn cluster        glslPreProglslGroup     contains=glslPreCondit,glslIncluded,glslInclude,glslDefine,glslErrInParen,glslErrInBracket,glslUserLabel,glslSpecial,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom,glslString,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslParen,glslBracket,glslMulti
syn region         glslDefine              start="^\s*#\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@glslPreProglslGroup
syn region         glslPreProc             start="^\s*#\s*\(pragma\>\|line\>\|error\>\|version\>\|extension\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@glslPreProglslGroup

" Highlight User Labels
syn cluster        glslMultiGroup          contains=glslIncluded,glslSpecial,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslUserCont,glslUserLabel,glslBitField,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom,glslCppParen,glslCppBracket,glslCppString
syn region         glslMulti               transparent start='?' skip='::' end=':' contains=ALLBUT,@glslMultiGroup
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster        glslLabelGroup          contains=glslUserLabel
syn match          glslUserCont            display "^\s*\I\i*\s*:$" contains=@glslLabelGroup
syn match          glslUserCont            display ";\s*\I\i*\s*:$" contains=@glslLabelGroup
syn match          glslUserCont            display "^\s*\I\i*\s*:[^:]"me=e-1 contains=@glslLabelGroup
syn match          glslUserCont            display ";\s*\I\i*\s*:[^:]"me=e-1 contains=@glslLabelGroup

syn match          glslUserLabel           display "\I\i*" contained

" Avoid recognizing most bitfields as labels
syn match          glslBitField            display "^\s*\I\i*\s*:\s*[1-9]"me=e-1
syn match          glslBitField            display ";\s*\I\i*\s*:\s*[1-9]"me=e-1

syn keyword        glslState               gl_Position gl_PointSize gl_ClipVertex
syn keyword        glslState               gl_FragCoord gl_FrontFacing gl_FragColor gl_FragData gl_FragDepth

" vertex attributes
syn keyword        glslState               gl_Color gl_SecondaryColor gl_Normal gl_Vertex gl_FogCoord
syn match          glslState               display "gl_MultiTexCoord\d\+"

" varying variables
syn keyword        glslState               gl_FrontColor gl_BackColor gl_FrontSecondaryColor gl_BackSecondaryColor gl_TexCoord gl_FogFragCoord

" uniforms
syn keyword        glslUniform             gl_ModelViewMatrix gl_ProjectionMatrix gl_ModelViewProjectionMatrix gl_NormalMatrix gl_TextureMatrix
syn keyword        glslUniform             gl_NormalScale gl_DepthRange gl_ClipPlane gl_Point gl_FrontMaterial gl_BackMaterial
syn keyword        glslUniform             gl_LightSource gl_LightModel gl_FrontLightModelProduct gl_BackLightModelProduct
syn keyword        glslUniform             gl_FrontLightProduct gl_BackLightProduct glTextureEnvColor
syn keyword        glslUniform             gl_TextureEnvColor gl_Fog
syn match          glslUniform             display "gl_EyePlane[STRQ]"
syn match          glslUniform             display "gl_ObjectPlane[STRQ]"
syn keyword        glslUniform             gl_ModelViewMatrixInverse gl_ProjectionMatrixInverse gl_ModelViewProjectionMatrixInverse 
syn keyword        glslUniform             gl_TextureMatrixInverse gl_ModelViewMatrixTranspose gl_ProjectionMatrixTranspose
syn keyword        glslUniform             gl_ModelViewProjectionMatrixTranspose gl_TextureMatrixTranspose gl_ModelViewMatrixInverseTranspose
syn keyword        glslUniform             gl_ProjectionMatrixInverseTranspose gl_ModelViewProjectionMatrixInverseTranspose gl_TextureMatrixInverseTranspose

" uniform types
syn keyword        glslType                gl_DepthRangeParameters gl_PointParameters gl_MaterialParameters
syn keyword        glslType                gl_LightSourceParameters gl_LightModelParameters gl_LightModelProducts
syn keyword        glslType                gl_LightProducts gl_FogParameters

" constants
syn keyword        glslConstant            gl_MaxLights gl_MaxClipPlanes gl_MaxTextureUnits gl_MaxTextureCoords gl_MaxVertexAttribs
syn keyword        glslConstant            gl_MaxVertexUniformComponents gl_MaxVaryingFloats gl_MaxVertexTextureImageUnits
syn keyword        glslConstant            gl_MaxCombinedTextureImageUnits gl_MaxTextureImageUnits gl_MaxFragmentUniformComponents 
syn keyword        glslConstant            gl_MaxDrawBuffers

" swizzling
syn match          glslSwizzle             /\.[xyzw]\{1,4\}\>/
syn match          glslSwizzle             /\.[rgba]\{1,4\}\>/
syn match          glslSwizzle             /\.[stpq]\{1,4\}\>/

" built in functions
syn keyword        glslFunc                radians degrees sin cos tan asin acos atan pow exp2 log2 sqrt inversesqrt
syn keyword        glslFunc                abs sign floor ceil fract mod min max clamp mix step smoothstep 
syn keyword        glslFunc                length distance dot cross normalize ftransform faceforward reflect
syn keyword        glslFunc                matrixcompmult lessThan lessThanEqual greaterThan greaterThanEqual equal notEqual any all not
syn keyword        glslFunc                texture1D texture1DProj texture1DLod texture1DProjLod
syn keyword        glslFunc                texture2D texture2DProj texture2DLod texture2DProjLod
syn keyword        glslFunc                texture3D texture3DProj texture3DLod texture3DProjLod
syn keyword        glslFunc                textureCube textureCubeLod
syn keyword        glslFunc                shadow1D shadow1DProj shadow1DLod shadow1DProjLod
syn keyword        glslFunc                shadow2D shadow2DProj shadow2DLod shadow2DProjLod
syn keyword        glslFunc                dFdx dFdy fwidth noise1 noise2 noise3 noise4
syn keyword        glslFunc                refract exp log

" highlight unsupported keywords
syn keyword        glslUnsupported         asm
syn keyword        glslUnsupported         class union enum typedef template this packed
syn keyword        glslUnsupported         goto switch default
syn keyword        glslUnsupported         inline noinline volatile public static extern external interface
syn keyword        glslUnsupported         long short double half fixed unsigned
syn keyword        glslUnsupported         input output
syn keyword        glslUnsupported         hvec2 hvec3 hvec4 dvec2 dvec3 dvec4 fvec2 fvec3 fvec4 
syn keyword        glslUnsupported         sampler2DRect sampler3DRect sampler2DRectShadow
syn keyword        glslUnsupported         sizeof cast
syn keyword        glslUnsupported         namespace using

"wtf?
"let b:c_minlines = 50        " #if 0 constructs can be long
"exec "syn sync ccomment glslComment minlines=" . b:c_minlines

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_glsl_syn_inits")
  if version < 508
    let did_glsl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink glslFormat                   glslSpecial
  HiLink glslCppString                glslString
  HiLink glslCommentL                 glslComment
  HiLink glslCommentStart             glslComment
  HiLink glslLabel                    Label
  HiLink glslUserLabel                Label
  HiLink glslConditional              Conditional
  HiLink glslRepeat                   Repeat
  HiLink glslCharacter                Character
  HiLink glslSpecialCharacter         glslSpecial
  HiLink glslNumber                   Number
  HiLink glslOctal                    Number
  HiLink glslOctalZero                PreProc         " link this to Error if you want
  HiLink glslFloat                    Float
  HiLink glslOctalError               glslError
  HiLink glslParenError               glslError
  HiLink glslErrInParen               glslError
  HiLink glslErrInBracket             glslError
  HiLink glslCommentError             glslError
  HiLink glslCommentStartError        glslError
  HiLink glslSpaceError               glslError
  HiLink glslSpecialError             glslError
  HiLink glslOperator                 Operator
  HiLink glslStructure                Structure
  HiLink glslStorageClass             StorageClass
  HiLink glslInclude                  Include
  HiLink glslPreProc                  PreProc
  HiLink glslDefine                   Macro
  HiLink glslIncluded                 glslString
  HiLink glslError                    Error
  HiLink glslStatement                Statement
  HiLink glslPreCondit                PreCondit
  HiLink glslType                     Type
  HiLink glslConstant                 Constant
  HiLink glslCommentString            glslString
  HiLink glslComment2String           glslString
  HiLink glslCommentSkip              glslComment
  HiLink glslString                   String
  HiLink glslComment                  Comment
  HiLink glslSpecial                  SpecialChar
  HiLink glslSwizzle                  SpecialChar
  HiLink glslTodo                     Todo
  HiLink glslCppSkip                  glslCppOut
  HiLink glslCppOut2                  glslCppOut
  HiLink glslCppOut                   Comment
  HiLink glslUniform                  glslType
  HiLink glslState                    glslType
  HiLink glslFunc                     glslStatement
  HiLink glslUnsupported              glslError

  delcommand HiLink
endif

let b:current_syntax = "glsl"

" vim: ts=8

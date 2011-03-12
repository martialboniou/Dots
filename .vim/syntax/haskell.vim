" Vim syntax file
" Language:     Haskell
" Maintainer:   Rui Carlos A. Goncalves <rcgoncalves.pt@gmail.com>
" Last Change:  8 March, 2009 -- see http://adradh.org.uk/b/2009/03/08 for
"                                changes
"
" Version:      1.2
" Url:          http://www.rcg-pt.net/programacao/haskell.vim.gz
"
" Original Author: John Williams <jrw@pobox.com>

" Remove any old syntax stuff hanging around
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif


" (Qualified) identifiers (no default highlighting)
syn match ConId         "\(\<[A-Z][a-zA-Z0-9_']*\.\)*\<[A-Z][a-zA-Z0-9_']*\>\('\=#\<\@!\)\="
syn match VarId         "\(\<[A-Z][a-zA-Z0-9_']*\.\)*\<[a-z][a-zA-Z0-9_']*\>\('\=#\<\@!\)\="

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match hsVarSym      "\(\<[A-Z][a-zA-Z0-9_']*\.\)*[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match hsConSym      "\(\<[A-Z][a-zA-Z0-9_']*\.\)*:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match hsVarSym      "`\(\<[A-Z][a-zA-Z0-9_']*\.\)*[a-z][a-zA-Z0-9_']*#\=`"
syn match hsConSym      "`\(\<[A-Z][a-zA-Z0-9_']*\.\)*[A-Z][a-zA-Z0-9_']*#\=`"


" Reserved symbols--cannot be overloaded.
syn match hsDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}\|\.\."
" Used in arrow syntax -- (| |) where the | isn't part of an identifier
syn match hsBanana     "\((|[^-!#$%&\*\+/<=>\?@\\^|~.:)]\@=\)\|\([^-!#$%&\*\+/<=>\?@\\^|~.:(]\@<=|)\)"
" Unboxed tuples -- (# #)
syn match hsUTuple     "\((#[^-!#$%&\*\+/<=>\?@\\^|~.:)]\@=\)\|\([^-!#$%&\*\+/<=>\?@\\^|~.:(]\@<=#)\)"


" Strings and constants
syn match   hsSpecialChar       contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   hsSpecialChar       contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   hsSpecialCharError  contained "\\&\|'''\+"
syn region  hsString            start=+"+  skip=+\\\\\|\\"+  end=+"#\=+  contains=hsSpecialChar
syn match   hsCharacter         "[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'#\="lc=1 contains=hsSpecialChar,hsSpecialCharError
syn match   hsCharacter         "^'\([^\\]\|\\[^']\+\|\\'\)'#\=" contains=hsSpecialChar,hsSpecialCharError
syn match   hsNumber            "\(\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>\)#\{,2}"
syn match   hsFloat             "\(\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>\)#\{,2}"


" Keyword definitions. These must be patters instead of keywords
" because otherwise they would match as keywords at the start of a
" "literate" comment (see lhs.vim).
syn match hsModule      "\<module\>#\@!"
syn match hsImport      "\<import\>#\@!.\{-}\($\|(\|;\)\@="he=s+6 contains=hsImportMod
syn match hsImportMod   contained "\<\(as\|qualified\|hiding\)\>"
syn match hsInfix       "\<\(infix\|infixl\|infixr\)\>#\@!"
syn match hsStructure   "\<\(class\|data\|deriving\|instance\|default\|where\)\>#\@!"
syn match hsTypedef     "\<\(type\|newtype\)\>#\@!"
syn match hsStatement   "\<\(do\|return\|case\|of\|let\|in\|rec\|proc\|mdo\)\>#\@!"
syn match hsConditional "\<\(if\|then\|else\)\>#\@!"
syn match hsForall      "\<forall\>#\@!\_.\{-}\."me=s+6

" FFI
syn match hsFFI         "\<foreign\>#\@!.\{-}\($\|;\)\@="he=s+7 contains=hsString,hsFFIDir,hsFFISafety,hsFFIConv,hsVarSym,hsConSym,hsType
syn match hsFFIDir      "\<\(import\|export\)\>" contained
syn match hsFFISafety   "\<\(unsafe\|safe\|\)\>" contained
syn match hsFFIConv     "\<\(ccall\|stdcall\|cplusplus\|jvm\|dotnet\)\>" contained


" Types from the standard prelude.
syn match hsType        "\<\(Bool\|Maybe\|Either\|Ordering\)\>#\@!"
syn match hsType        "\<\(Char\|String\|Int\|Integer\|Float\|Double\|Rational\|IO\)\>#\@!"
syn match hsType        "\<\(ReadS\|ShowS\)\>#\@!"
syn match hsType        "\<\(FilePath\|IOError\)\>#\@!"

" GHC.Prim types
syn match hsPrimType    "\<\(Char\|Int\|Word\|Int64\|Word64\|Double\|Float\|Addr\)\>#"

" Classes from the standard prelude
syn match hsType        "\<\(Eq\|Ord\|Enum\|Bounded\|Num\|Real\|Integral\|Fractional\)\>#\@!"
syn match hsType        "\<\(Floating\|RealFrac\|RealFloat\|Monad\|Functor\)\>#\@!"
syn match hsType        "\<\(Show\|Read\)\>#\@!"


" Constants from the standard prelude.
syn match hsBoolean     "\<\(True\|False\)\>#\@!"
syn match hsMaybe       "\<\(Nothing\|Just\)\>#\@!"
syn match hsConstant    "\<\(Left\|Right\)\>#\@!"
syn match hsOrdering    "\<\(LT\|EQ\|GT\)\>#\@!"


" Functions from the standard prelude.
syn match hsFunction    "\<\(compare\|max\|min\)\>#\@!"
syn match hsFunction    "\<\(succ\|pred\|toEnum\|fromEnum\|enumFrom\|enumFromThen\|enumFromTo\|enumFromThenTo\)\>#\@!"
syn match hsFunction    "\<\(minBound\|maxBound\)\>#\@!"
syn match hsFunction    "\<\(negate\|abs\|signum\|fromInteger\)\>#\@!"
syn match hsFunction    "\<toRational\>#\@!"
syn match hsFunction    "\<\(quot\|rem\|div\|mod\|quotRem\|divMod\|toInteger\)\>#\@!"
syn match hsFunction    "\<\(recip\|fromRational\)\>#\@!"
syn match hsFunction    "\<\(pi\|exp\|log\|sqrt\|logBase\|sin\|cos\|tan\|asin\|acos\|atan\|sinh\|cosh\|tanh\|asinh\|acosh\|atanh\)\>#\@!"
syn match hsFunction    "\<\(properFraction\|truncate\|round\|ceiling\|floor\)\>#\@!"
syn match hsFunction    "\<\(floatRadix\|floatDigits\|floatRange\|decodeFloat\|encodeFloat\|exponent\|significand\|scaleFloat\|isNaN\|isInfinite\|isDenormalized\|isIEEE\|isNegativeZero\|atan2\)\>#\@!"
syn match hsFunction    "\<\(return\|fail\)\>#\@!"
syn match hsFunction    "\<\(fmap\)\>#\@!"
syn match hsFunction    "\<\(mapM\|mapM_\|sequence\|sequence_\)\>#\@!"
syn match hsFunction    "\<\(maybe\|either\)\>#\@!"
syn match hsFunction    "\<\(not\|otherwise\)\>#\@!"
syn match hsFunction    "\<\(subtract\|even\|odd\|gcd\|lcm\)\>#\@!"
syn match hsFunction    "\<\(fromIntegral\|realToFrac\)\>#\@!"
syn match hsFunction    "\<\(fst\|snd\|curry\|uncurry\|id\|const\|flip\|until\)\>#\@!"
syn match hsFunction    "\<\(asTypeOf\|error\|undefined\)\>#\@!"
syn match hsFunction    "\<\(seq\)\>#\@!"
syn match hsFunction    "\<\(map\|filter\|concat\|concatMap\)\>#\@!"
syn match hsFunction    "\<\(head\|last\|tail\|init\|null\|length\)\>#\@!"
syn match hsFunction    "\<\(foldl\|foldl1\|scanl\|scanl1\|foldr\|foldr1\|scanr\|scanr1\)\>#\@!"
syn match hsFunction    "\<\(iterate\|repeat\|replicate\|cycle\)\>#\@!"
syn match hsFunction    "\<\(take\|drop\|splitAt\|takeWhile\|dropWhile\|span\|break\)\>#\@!"
syn match hsFunction    "\<\(lines\|words\|unlines\|unwords\|reverse\|and\|or\)\>#\@!"
syn match hsFunction    "\<\(any\|elem\|notElem\|lookup\)\>#\@!"
syn match hsFunction    "\<\(sum\|product\|maximum\|minimum\)\>#\@!"
syn match hsFunction    "\<\(zip\|zip3\|zipWith\|zipWith3\|unzip\|unzip3\)\>#\@!"
syn match hsFunction    "\<\(readsPrec\|readList\)\>#\@!"
syn match hsFunction    "\<\(showsPrec\|show\|showList\)\>#\@!"
syn match hsFunction    "\<\(reads\|shows\|read\|lex\)\>#\@!"
syn match hsFunction    "\<\(showChar\|showString\|readParen\|showParen\)\>#\@!"
syn match hsFunction    "\<\(ioError\|userError\|catch\)\>#\@!"
syn match hsFunction    "\<\(putChar\|putStr\|putStrLn\|print\)\>#\@!"
syn match hsFunction    "\<\(getChar\|getLine\|getContents\|interact\)\>#\@!"
syn match hsFunction    "\<\(readFile\|writeFile\|appendFile\|readIO\|readLn\)\>#\@!"


" Comments
syn match   hsLineComment      "--.*"
syn region  hsBlockComment     start="{-"  end="-}" contains=hsBlockComment
syn region  hsPragma           start="{-#" end="#-}"

" Literate comments--any line not starting with '>' is a comment.
if exists("b:hs_literate_comments")
  syn region  hsLiterateComment   start="^" end="^>"
endif

" C Preprocessor directives. Shamelessly ripped from c.vim and trimmed
" First, see whether to flag directive-like lines or not
if (!exists("hs_allow_hash_operator"))
    syn match   cError          display "^\s*\(%:\|#\).*$"
endif
" Accept %: for # (C99)
syn region      cPreCondit      start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCommentError
syn match       cPreCondit      display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region      cCppOut         start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
syn region      cCppOut2        contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cCppSkip
syn region      cCppSkip        contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cCppSkip
syn region      cIncluded       display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match       cIncluded       display contained "<[^>]*>"
syn match       cInclude        display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=cIncluded
syn cluster     cPreProcGroup   contains=cPreCondit,cIncluded,cInclude,cDefine,cCppOut,cCppOut2,cCppSkip,cCommentStartError
syn region      cDefine         matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$"
syn region      cPreProc        matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend

syn region      cComment        matchgroup=cCommentStart start="/\*" end="\*/" contains=cCommentStartError,cSpaceError contained
syntax match    cCommentError   display "\*/" contained
syntax match    cCommentStartError display "/\*"me=e-1 contained
syn region      cCppString      start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained


if !exists("hs_minlines")
  let hs_minlines = 50
endif
exec "syn sync lines=" . hs_minlines

if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hsModule               hsStructure
  HiLink hsImport               Include
  HiLink hsImportMod            hsImport
  HiLink hsInfix                PreProc
  HiLink hsStructure            Structure
  HiLink hsStatement            Statement
  HiLink hsConditional          Conditional
  HiLink hsSpecialChar          SpecialChar
  HiLink hsTypedef              Typedef
  HiLink hsVarSym               hsOperator
  HiLink hsConSym               hsOperator
  HiLink hsOperator             Operator
  HiLink hsSpecialCharError     Error
  HiLink hsString               String
  HiLink hsCharacter            Character
  HiLink hsNumber               Number
  HiLink hsFloat                Float
  HiLink hsConditional          Conditional
  HiLink hsLiterateComment      hsComment
  HiLink hsBlockComment         hsComment
  HiLink hsLineComment          hsComment
  HiLink hsComment              Comment
  HiLink hsPragma               SpecialComment
  HiLink hsBoolean              Boolean
  HiLink hsType                 Type
  HiLink hsPrimType             hsType
  HiLink hsFunction             Function
  HiLink hsMaybe                hsEnumConst
  HiLink hsOrdering             hsEnumConst
  HiLink hsEnumConst            Constant
  HiLink hsConstant             Constant
  HiLink hsDebug                Debug
  HiLink hsForall               Typedef

  HiLink hsFFI                  PreProc
  HiLink hsFFIDir               hsFFI
  HiLink hsFFISafety            hsFFI
  HiLink hsFFIConv              hsFFI

  if exists("hs_highlight_delimiters")
      HiLink HsDelimiter        Delimiter
      HiLink HsBanana           hsDelimiter
      HiLink HsUTuple           hsDelimiter
  endif

  HiLink cCppString             hsString
  HiLink cCommentStart          hsComment
  HiLink cCommentError          hsError
  HiLink cCommentStartError     hsError
  HiLink cInclude               Include
  HiLink cPreProc               PreProc
  HiLink cDefine                Macro
  HiLink cIncluded              hsString
  HiLink cError                 Error
  HiLink cPreCondit             PreCondit
  HiLink cComment               Comment
  HiLink cCppSkip               cCppOut
  HiLink cCppOut2               cCppOut
  HiLink cCppOut                Comment

  delcommand HiLink
endif

let b:current_syntax = "haskell"

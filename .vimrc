"
" Vimrc by mars
"
" UTF-8 + Common Lisp + JavaScript + ObjectiveC/C++ + Erlang + Io + PHP
"
" note: Dvorak navigation key changes are deactivated (the same in viper-mode for emacs)
"
" hondana.net/vim
" hondana@gmx.com (2004-2010)
"

set enc=utf-8
set encoding=utf-8
set fileencodings=utf-8
set nocompatible
"lines setting in .gvimrc
"set so=10 "scope lines' height
set background=dark
set history=1000
set clipboard+=unnamed
set ffs=unix,mac,dos
set viminfo+=!
set sw=4
set sts=4
set ts=4
set expandtab
set smarttab
set visualbell t_vb=
set autowrite
set scrolloff=3 " maintain more context around the cursor: 999 to centerize
set sw=2 " in general
set noswapfile
set guioptions-=T
set guioptions+=c
set guioptions-=m
set guioptions-=r " no right scrollbar
set guioptions-=R
set guioptions-=l " no left scrollbar
set guioptions-=L
set backup
set history=1000 " boost history up
set shortmess=atI " stifle many interruptive prompts

set backupdir=~/.vim/backup
set directory=~/.vim/tmp
helptags ~/.vim/doc

set incsearch
set ignorecase
set smartcase
set lsp=0

" BASH-like wildmode completion
set wildmenu
set wildmode=longest,list,full
set wildignore+=*.o,*.obj

set hidden
set ruler
set cmdheight=2
set lz
set hid
set backspace=indent,eol,start
set mouse=a
set shortmess=atI
set report=0
set noerrorbells
"set fillchars=vert:\,stl:\,stlnc:\
set showmatch
set cpoptions-=m
set mat=5
set nohlsearch
"set listchars=tab:\|\,trail:.,extentds:>,precedes:<,eol:$
set novisualbell
set statusline=%F%m%r%h%w\ \{%04l:%04v\}\ [%Y]\ [%{&ff}]\ [code:\%03.3b]\ --\ %p%%
set laststatus=2
"set statusline=%F%m%r%h%w\ [TYPE=%Y]\ [POS=%04l,%04v][%p%%]\ [FORMAT=%{&ff}]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [LEN=%L]
set fo=tcrqn " see formatoptions help (auto-wraps text+comments using textwidth + auto-inserts the current comment leader + allows formatting of comments + align text)
set ai
set si
set cindent

set nojoinspaces
set nocp
" highlightnings
set number
"highlight LineNr ctermbg=black ctermfg=gray
set cursorline
"highlight CursorLine term=reverse cterm=reverse
"highlight TabLine term=none cterm=none
"highlight TabLineSel ctermbg=darkblue
"sign define information text=>> linehl=Warning texthl=Error
set whichwrap=b,s,<,>,[,]
set bufhidden=hide " no distant file reload

" inspirated by twerth files
set equalalways " Multiple windows, when created, are equal in size
set splitbelow splitright

" IMPORTANT: gf DOESN'T WORK AS EXPECTED (use <C-W><C-F> locally)
" See: .vim/shortkeys.vim
" if you want to support filenames with spaces in when using gf
set isfname+=32
" View/Session
set viewdir=$HOME/.vim/views/
set sessionoptions+=winpos
set sessionoptions-=folds

" folds
set foldenable " Turn on folding
set foldmethod=indent " Make folding indent sensitive
"set foldlevel=100 " Don't autofold anything (but I can still fold manually)
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff
set foldminlines=2 " don't fold 3 lines
set fillchars=fold:=
set foldtext=MyFoldFunction()

if &t_Co > 2 || has("gui_running")
    syntax on
endif
if &t_Co == 256 || has("gui_running")
    colorscheme inkpot " ir_black \" underwater-mod \" dusk \" inkpot \" ir_black \" railscasts \" from Ruby theme in Textmate
endif

"" color template pattern
"3match Todo /<+.\++>/

""
iab xdate <c-r>=strftime("%d/%m/%y %H:%M:%S")<cr>
""
" A set of the funny icons for the statusline
let icons = [">+<", "<+>", ">-<", "<->", ">#<", "<#>", ">=<", "<=>",
            \">O<", "<O>", ">o<", "<o>", ">X<", "<X>", ">x<", "<x>"]

" These encodings will be listed in the GUI menu
let encodings = ["ascii", "cp1251", "cp866", "iso-8859-5", "koi8-r",
               \ "latin1", "utf-16le", "utf-8"]

" These options will be kept between editing sessions
let options = ["iminsert", "spell", "textwidth", "wrap"]

" Miscellaneous defaults, will be changed by VimEnter() function
let EXTERNAL = 1

" Helpheight is different in GUI and text modes
let &helpheight = &lines - 8


filetype plugin indent on
" IMPORTANT: UPDATE CTAGS WITH C-L
" use: ctags -f ~/.vim/stdtags -R --c++-kinds=+p --fields=+iaS --extra=+q .
" in /usr/include
"set tags=~/.vim/tags/qt4,~/.vim/tags/local,~/.vim/tags/opt,~/.vim/tags/sw,~/.vim/tags/std,tags,.tags,../tags
"""set tags=~/.vim/tags/qt4
if has("autocmd")
    filetype plugin indent on
    let loaded_matchit = 1
    autocmd BufEnter * :cd %:p:h " USE CURRENT DIR AS DEFAULT
    autocmd InsertLeave * if pumvisible() == 0|pclose|endif
" keep settings when moving between multiple folders
    "    autocmd BufWinLeave     * mkview
    "    autocmd BufWinEnter     * silent loadview
    " restore/save Session
    autocmd BufEnter * :syntax sync fromstart " ensure every file does syntax highlighting (full)
    "autocmd VimEnter * call RestoreSession()
    "autocmd VimLeave * call SaveSession()
"    autocmd BufEnter,BufReadPost,BufWritePost * call WhenModified(1)
    
    " open file with the cursor at the latest position
    autocmd BufReadPost * silent! call s:CursorOldPosition()
    autocmd BufWritePre * ks|call TimeStamp()|'s
"    autocmd CursorMoved,CursorMovedI * call WhenModified(0) 
    
    augroup filetypedetect
        " Set some modes based of file extension
        au BufNewFile,BufRead *.json set filetype=json
        au BufNewFile,BufRead *.asp set filetype=aspjscript " all my .asp files ARE jscript
        "au BufNewFile,BufRead *.tpl set filetype=html " all my .tpl files ARE html
        au BufNewFile,BufRead *.hta set filetype=html " all my .tpl files ARE html
        au BufNewFile,BufRead *.rb,*.rjs,*.rbw,*.gem,*.gemspec setlocal filetype=ruby
        au BufNewFile,BufRead *.as setlocal filetype=actionscript  
        au BufNewFile,BufRead *.mxml setlocal filetype=xml  
        au BufNewFile,BufRead *.wiki setlocal filetype=Wikipedia  
        au BufNewFile,BufRead *.json setlocal nowrap sw=4 ts=4 sts=0 noet smartindent number  
        "" Lisp languages and JavaScript scripts are better editable with
        "" Emacs + Vimpulse (including SBCL, CLISP, ECL, Clojure, SpiderMonkey
        "" REPL)
        "@Common Lisp
        au FileType lisp setlocal sw=1
        "@Scheme48
        au FileType scheme setlocal sw=1
        "@Clojure
        " cf. end of file """ Clojure let clj ...
        "@newLISP (deprecated)
        au! filetypedetect BufNewFile, BufRead *.lsp set filetype=newlisp

        "@Haskell
        au BufEnter *.hs compiler ghc
        au FileType haskell setlocal et ts=4 sw=4 sts=4 completeopt=menu,menuone
        "omnifunc=haskellcomplete#CompleteHaskell " same expand as Java / Haskell recommends no tabs
		let g:haddock_browser = "open"
		let g:haddock_docdir  = "/Users/mars/.cabal/share/haddock-2.4.2/html/" " beware of the last slash [martial: 2009/03/24]
        let g:haddock_browser_callformat = "%s %s"
        let g:haddock_indexfiledir = "~/.vim/Haddock/"
        " .vim/after/syntax/haskell.vim added
        "au FileType cabal setlocal et " Cabal is more sensible to TAB than common Haskell code 
        "@GST
        au FileType st call FT_st()
        "@Web ft  
        au FileType ruby setlocal et ts=2 sw=2 sts=2 omnifunc=rubycomplete#Complete  
        au FileType eruby setlocal et ts=2 sw=2 sts=2  
        au FileType css setlocal et ts=2 sw=2 sts=2  
        au FileType actionscript setlocal nowrap sw=4 ts=4 sts=0 noet smartindent efm=%f(%l):\ col:\ %c\ Error:\ %m  
        au FileType text setlocal textwidth=78 nonumber
        " IMPORTANT: TODO tidy clean up
        "au FileType xml exe ":silent 1,$!tidy --input-xml true --indent yes -q"
        "au FileType html,htm exe ":silent 1,$!tidy --indent yes -q"
        au FileType html,htm,xml,xsl setlocal spell nonumber  
        au FileType html,xml,xsl runtime plugin/closetag.vim
        au FileType xml runtime ftplugin/xml.vim 
        au FileType cpp,c setlocal makeprg=make
        au FileType plaintex setlocal spell  
        "@Io Language
        au BufNewFile,BufRead *.io setfiletype=io
        "@ObjectiveC and C header as ObjC
        au BufNewFile *.m set ft=objc
        au BufNewFile, BufRead *.h set ft=objc
        "@OmniComplete known scripting language
        au FileType javascript set omnifunc=javascriptcomplete#CompleteJS
        au FileType html set omnifunc=htmlcomplete#CompleteTags
        au FileType css set omnifunc=csscomplete#CompleteCss
        au FileType xml set omnifunc=xmlcomplete#CompleteTags
        au FileType php set omnifunc=phpcomplete#CompletePHP
        au FileType python set omnifunc=pythoncomplete#Complete
        au FileType c set omnifunc=ccomplete#Complete
        "@LoodTemplate
        au BufNewFile * silent! call LoadTemplate('%:e', 'all')
        au BufRead * silent! call LoadTemplate('%:e', 'noTemplate')
        "@Erlang
        au BufNewFile,BufRead *.yaws setf erlang
        au BufNewFile,BufRead *.pro  set filetype=qmake "Qt Projects
        "Hard tabs
        au BufNewFile,BufRead Makefile* set noexpandtab
		"@Markdown in ftdetect
		"au! BufRead,BufNewFile *.md,*.mkd setfiletype mkd
		"au  FileType mkd set ai formatoptions=tcroqn2 comments=n:>
		au FileType markdown set ai formatoptions=tcroqn2 comments=n:>
    augroup END
endif " has ("autocmd")

" Called automagically on every buffer saving, updates 'Last change:'
" timestamp and RCS Id
function! TimeStamp()
  let lines = line("$") < 10 ? line("$") : 10
  let pattern1 = '\(Last [Cc]hange:\s\+\)\d\d\d\d \u\l\l \d\d'
  let replace1 = '\1' . strftime("%Y %b %d")
  execute printf('1,%ds/\C\m%s/%s/e', lines, pattern1, replace1)
  execute printf('$-%d+1,$s/\C\m%s/%s/e', lines, pattern1, replace1)
  let pattern2 = '\($Id: \f\+ \d\+\.\d\+\(\.\d\+\.\d\+\)*\)\(+\(\d\+\)\)\? '
             \ . '\(\d\d\d\d[-/]\d\d[-/]\d\d \d\d:\d\d:\d\d\)\(+\d\d\)\?'
  let replace2 = '\=submatch(1) . "+" . (submatch(4) + 1) . " "'
             \ . '. strftime("%Y\/%m\/%d %H:%M:%S") . submatch(6)'
  execute printf('1,%ds/\C\m%s/%s/e', lines, pattern2, replace2)
  execute printf('$-%d+1,$s/\C\m%s/%s/e', lines, pattern2, replace2)
endfunction

" Called automagically in various conditions, sets statusline color
" depending of &modified state
function! WhenModified(force)
  if a:force || &modified != g:was_modified
    execute 'setlocal statusline=%' . &modified . '*%t\ %L'
        \ . '\ %y%r[%{&tw}]%=\ %{Moon()}\ %{Lang()}\ %{Icon()}'
        \ . '\ %{&fenc}\ %m\ %-15(%{HexDec()}%)%-10(%l,%v%)%P'
    let g:was_modified = &modified
  endif
endfunction

" Bogus value, just not to leave it undefined
let was_modified = 2

" Phase of the Moon calculation
let time = localtime()
let fullday = 86400
let offset = 592500
let period = 2551443
let phase = (time - offset) % period
let phase = phase / fullday

" Moon phase and paste flag for the statusline. Weird, eh?
function! Moon()
  return printf(&paste ? "[%d]" : "(%d)", g:phase)
endfunction

" Name of current keymap for the statusline (not as generic as "%k")
function! Lang()
  return &iminsert ? "ru" : "en"
endfunction

" Funny icon for the statusline, combined from the set of status bits
function! Icon()
  return g:icons[8 * exists("b:syn_sync") + 4 * exists("g:syntax_on")
                \ + 2 * &spell + &wrap]
endfunction

" Value of a character under cursor; better than standard '0x%02B (%b)'
function! HexDec()
  let char = matchstr(getline("."), ".", col(".") - 1)
  if g:EXTERNAL
    let char = iconv(char, &encoding, &fileencoding)
    let format = "0x%02X <%d>"
  else
    let format = "0x%02X (%d)"
  endif
  let char = char2nr(char)
  return printf(format, char, char)
endfunction

" For putting code in blogs  
let html_use_css = 1  
let use_xhtml = 1  
let html_number_lines = 0  
function! HTML(line1, line2) range  
    exec (a:line1. ',' . a:line2) . 'TOhtml'  
    exec '0,7d'  
    exec '14,15d'  
    exec '%s/<pre>/<pre>/'  
    exec '$-1,$d'  
endfunction  
command! -range=% HTML :call HTML(, )

"@HOWM like on Emacs
"set runtimepath+=~/.vim/plugin/howm
"let g:howm_dir='~/Documents/Wiki'
"let g:howm_findprg=""
"let g:howm_findprg=""
"let g:howm_instantpreview=1


" DVORAK
"set langmap:s;;
"set langmap=;z;
"set langmap='q,\,w,.e,pr,yt,fy,gu,ci,ro,lp,/[,=],os,ed,uf,ig,dh,hj,tk,nl,-',qx,jc,kv,xb,bn,mm,w\,,v.,z/,[-,]=,\"Q,<W,>E, \ PR,YT,FY,GU,CI,RO,LP,?{,+},AA,OS,ED,UF,IG, \
"DH,HJ,TK,NL,S:,_\",:Z,QX,JC,KV,XB,BN,MM,W<,V>,Z? 


" BONUS FUNC
   "Debut de Completion
function! InsertTabWrapper(direction)
   let col = col('.') - 1
   if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
   elseif "backward" == a:direction
    return "\<c-p>"
   else
    return "\<c-n>"
   endif
endfunction
" use snippy so:
"inoremap <tab> <c-r>=InsertTabWrapper("forward")<cr>
"inoremap <s-tab> <c-r>=InsertTabWrapper("backward")<cr>
"" Fin de Completion

" Deplacement de l'onglet courant vers la gauche ou vers la droite
function! MoveTab(direction)
    if (a:direction == 'left')
        let tabNum = tabpagenr()-2
        if (tabNum < 0)
            let tabNum = 0
        endif
        execute 'tabm ' . tabNum
    else
        let tabNum = tabpagenr()
        execute 'tabm ' . tabNum
    endif
endfunction

" SESSIONS
let s:sessionName = ''
function! s:IsSessionInUse()
    if (s:sessionName != '')
        call s:DisplayStatus('The session named "' . s:sessionName . '"' . " is currently used. Please leave Vim to load a new session.")
        return 1
    else
        return 0
    endif
endfunction

function! s:GetSessionName()
    let l:tmpSessionName = input('Session name : ')
    if (!match(l:tmpSessionName, '^[a-zA-Z]*\n$'))
        call s:DisplayStatus('Cancel: illegal character in the session name')
        return -1
    endif
    if (l:tmpSessionName == '')
        call s:DisplayStatus('Cancel: no name for this session')
        return -1
    endif
    return l:tmpSessionName
endfunction

function! SaveSession()
    let l:saveSession = 1
    if (s:sessionName == '')
        let l:keepDesc = 0
        let l:name = s:GetSessionName()
        if (l:name == -1)
            return -1
        endif
        let s:sessionDesc = input('Description : ')
        if (s:sessionDesc == '')
            let s:sessionDesc = 'No description'
        endif
        let s:sessionName = l:name
        if filereadable($HOME . '/.vim/sessions/' . s:sessionName . '.vim')
            let l:saveSession = confirm('This session already exists! Write it ?', "&Yes (default)\n&No", 1)
        endif
    else
        let l:keepDesc = 1
    endif
    if (l:saveSession == 1)
        execute 'mksession! '. $HOME . '/.vim/sessions/' . s:sessionName . '.vim'
        execute 'wviminfo! ' . $HOME . '/.vim/sessions/' . s:sessionName . '.viminfo'
        if (!l:keepDesc)
            execute 'split ' . $HOME . '/.vim/sessions/' . s:sessionName . '.desc'
            execute 'ldelete'
            execute 'call append(0, "' . s:sessionName . ' : ' . s:sessionDesc . '")'
            execute 'wq'
        endif
        call s:DisplayStatus('The session named "' . s:sessionName . '" is recorded')
    endif
endfunction

function! LoadSessionDirect()
    if (s:IsSessionInUse())
        return -1
    endif
    let l:name = s:GetSessionName()
    if (l:name == -1)
        return -1
    endif
    let l:fileBaseName = $HOME . '/.vim/sessions/' .  l:name
    if (filereadable(l:fileBaseName . '.vim') && filereadable(l:fileBaseName . '.viminfo'))
        let s:sessionName = l:name
        execute 'source ' . l:fileBaseName . '.vim'
        execute 'rviminfo ' . l:fileBaseName . '.viminfo'
        call s:DisplayStatus('The session "' . s:sessionName . '" is loaded')
    else
        call s:DisplayStatus('The session "' . s:sessionName . '" is unreachable')
    endif
endfunction

function! LoadSession()
    if (s:IsSessionInUse())
        return -1
    endif
    let l:sessionList = split(system('ls ' . $HOME . '/.vim/sessions/*desc'))
    let l:sessionNumber = 1
    for item in l:sessionList
        echo l:sessionNumber . '. ' . substitute(system('cat ' . item) , '\n\n' , '\n', '')
        let l:sessionNumber += 1
    endfor
    call LoadSessionDirect()
endfunction

" Display Status
function! s:DisplayStatus(msg)
    echohl Todo
    echo a:msg
    echohl None
endfunction

" Mouse Management
let s:mouseActivation = 1
function! ToggleMouseActivation()
    if (s:mouseActivation = 0)
        let s:mouseActivation = 0
        set mouse=n
        set paste
        call s:DisplayStatus('Mouse manager is deactivated')
    else
        let s:mouseActivation = 1
        set mouse=a
        set nopaste
        call s:DisplayStatus('Mouse manager is activated')
    endif
endfunction
set mouse=a
set nopaste

" Clean ugly DOS code
function! CleanCode()
    %retab
    %s/^M//g
    call s:DisplayStatus('Code clean now')
endfunction

" marks
let s:signMarks = {}
let s:jumpMarks = {}

function! MarkSign()
    let filename = bufname(winbufnr(winnr()))
    if (has_key(s:signMarks, filename))
        let s:signMarks[filename] += 1
    else
        let s:signMarks[filename] = 1
    endif
    exe ':sign place ' . s:signMarks[filename] . ' line=' . line('.') . ' name=information file=' . expand('%:p')
endfunction

function! JumpToSign()
    let filename = bufname(winbufnr(winnr()))
    if (has_key(s:signMarks, filename))
        if (has_key(s:jumpMarks, filename))
            let s:jumpMarks[filename] += 1
        else
            let s:jumpMarks[filename] = 1
        endif
        if (s:jumpMarks[filename] > s:signMarks[filename])
            let s:jumpMarks[filename] = 1
        endif
        silent! execute ':sign jump ' . s:jumpMarks[filename] . ' file=' . expand('%:p')
    endif
endfunction

" Remove trailing spaces
function TrimWhiteSpace()
  %s/\s*$//
endfunction

" key bindings
runtime shortkeys.vim

runtime unix.vim

" cursor position
function! s:CursorOldPosition()
    if line("'\"") > 0 && line("'\"") <= line("$")
        exe "normal g`\""
    endif
endfunction

" template activation
function! LoadTemplate(extension, type)
    if (a:type == 'all')
        silent! execute '0r ' . $HOME . '/.vim/templates/' . a:extension . '.tpl'
        silent! execute 'runtime templates/autocompletion.vim'
    endif
    " Insert abbrev if any
    silent! execute 'runtime abbreviations/' . a:extension . "abb.vim"
    " Insert dictionary
    set dictionary+=$HOME . '/.vim/dictionary' . a:extension . '.dict'
    " Insert specific commannds
    silent! execute 'runtime specific/' . a:extension . '.vim'
endfunction

let helpDisplay = 0
" Memory helper
function! HelpMemory()
    if g:helpDisplay == 0
        silent! topleft vertical 40split +buffer helpMemory
        let g:helpDisplay = 1
        set buftype=nofile
        0read $HOME/.vim/memory.txt
        highlight Memory ctermfg=white ctermbg=blue
        3match Memory /^.\+\ \+:/
    else
        bdelete helpMemory
        let g:helpDisplay = 0
    endif
endfunction

" folds
function! MyFoldFunction()
    let line = getline(v:foldstart)
    let sub = substitute(line, '/\*\|\*/\|^\s+', '', 'g')
    let lines = v:foldend - v:foldstart +1
    return '[+] ' . v:folddashes.sub . '...' . lines . ' lines ...' . getline(v:foldend)
endfunction

" Load the MRU list on plugin startup
":MRU

" Getting mini buffer explorer (by amix.dk)
"let g:miniBufExplModSelTarget = 1
"let g:miniBufExplorerMoreThanOne = 0
"let g:miniBufExplModSelTarget = 0
"let g:miniBufExplUseSingleClick = 1
"let g:miniBufExplMapWindowsNavVim = 1
"let g:miniBufExplHSplit = 25
"let g:miniBufExplSplitBelow = 1

" Vim 7 spellchecking
if v:version <=700
    function! ToggleSpell()
        if &spell != 1
            setlocal spell spelllang=en_us
            set spellsuggest=5
        else
            setlocal spell!
        endif
    endfunction
    nnoremap <silent> <Leader>s <ESC>:call ToggleSpell()<CR>
    setlocal spell spelllang=en_us
endif

" Replace MiniBufExplorer default tab appearance
function! GuiTabLabel()
    let bufnrlist = tabpagebuflist(v:lnum)
    let bufId = bufnrlist[tabpagewinnr(v:lnum) - 1]
    let fn = bufname(bufId)
    let lastSlash = strridx(fn, '/')
    return strpart(fn, lastSlash+1, strlen(fn))
endfunction
"set guitablabel=%{GuiTabLabel()}

" Visual mode contextual comment (co)
function! CommentLines()
 " let Comment="#" " shell, perl
 exe ":s@^@".g:Comment."@g"
 exe ":s@$@".g:EndComment."^@g"
endfunction " vmap to co

""" Common Lisp
" Colors in Limp hack for Lispem
hi Brackets      ctermbg=53 ctermfg=black
hi BracketsBlock ctermbg=235 guibg=lightgray
hi StatusLine    ctermbg=white ctermfg=160
hi StatusLineNC  ctermbg=black ctermfg=gray
hi Pmenu         ctermbg=53 ctermfg=255
hi PmenuSel      ctermbg=255 ctermfg=53

""" GNU Smalltalk
" Filetype style
function! FT_st()
 set tabstop=8
 set softtabsstop=4
 set shiftwidth=4
 set noexpandtab
 retab 8
endfunction

""" JavaScript
" JavaScript Syntax Checking using Spidermonkey
"cabbr js !js ~/.vim/script/javascript/runjslint.js "`cat %`" \| ~/.vim/script/python/format_lint_output.py

""" Clojure
let clj_highlight_builtins   = 1
let clj_highlight_contrib    = 1
let clj_paren_rainbow        = 1
let clj_want_gorilla         = 1
let vimclojure#NailgunClient = "/usr/local/bin/ng"

" Viki -- seen on Byte Of Vim book but a predefined variable is missing for OS X
if has("mac")
 let g:vikiOpenUrlWith_ANY  = "exec 'silent !open '. escape('%{URL}', ' &!%')"
 let g:vikiOpenFileWith_ANY = "exec'silent !open '. shellescape('%{FILE}')"
endif

" CSApprox: color schemes terminal adapter
if !has("x11")
 let g:CSApprox_loaded = 1
endif

" URxvt
if &term == "rxvt-unicode"
 set t_Co=256
" colorscheme ir_black " force the color scheme to be loaded
 set columns=93
 let currentHost = system('hostname | tr -d "\n"')
 if currentHost == "cendre"
  set lines=42 " aqua display with Dock.app
 elseif currentHost == "monarde"
  set lines=38 " 12'' laptop on Ubuntu
 elseif currentHost == "macabre"
  set lines=49 " gnome/clfswm display on 4:3 screen
 endif
endif

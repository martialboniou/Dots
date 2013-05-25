"" Vimrc by mars for Vim 7.3+ (git & ruby required)
"
" Language: Vim
" Maintainer: Martial
" Creator: Martial
" Version: 0.4.9
" Goal: supports Shen, Haskell, common script languages (incl. VimL & CSS) edition
"       adds snippets, completion & syntax checker
"       extends indent, tabular & parentheses managementt (resp.for Python / Haskell / Lisp)
"
" hondana@gmx.com (2004-2010,2012-2013)
"

" BASIC BEHAVIOR
set hidden
set nobackup
set noswapfile
set mouse=a
set isfname+=32 "valid filename w/ space
let mapleader=","
nnoremap ; :
set path=.,** " search in file's dir then current subdir
au BufEnter * :cd %:p:h "current dir is file dir (so path != .,,**)
if &t_Co > 2 || has("gui_running") | syntax on | endif
if v:version >= 703 | set rnu | endif
set sts=2 ts=2 sw=2 et
set laststatus=2

" VUNDLE
let s:iCanHazVundle=1
set nocompatible
filetype off
"" VUNDLE SYSTEM
if !isdirectory(expand("~/.vim/bundle/vundle/.git"))
  let s:iCanHazVundle=0
  echo "Installing Vundle..."
  echo ""
  " PUSHD/POPD/MKDIR behave the same on M$ or POSIX system
  silent exe 'silent !pushd' . expand('~')
  silent !mkdir .vim
  silent !mkdir .vim/bundle
  silent !git clone https://github.com/gmarik/vundle .vim/bundle/vundle
  silent !popd
endif
set rtp+=~/.vim/bundle/vundle
cal vundle#rc()
Bundle 'gmarik/vundle'
"" VUNDLE PACKAGES
""" colorscheme
Bundle 'mrtazz/molokai.vim'
if &t_Co>=256 || has("gui_running")
  try
    colorscheme molokai
  catch /^Vim\%((\a\+)\)\=:E185/
    "no way
  endtry
endif
""" tag sidebar for large files
Bundle 'majutsushi/tagbar'
""" statusline enhancer
Bundle 'Lokaltog/vim-powerline'
""" motion helper: <leader><leader>w displays word jump
Bundle 'Lokaltog/vim-easymotion'
""" <TAB> everywhere (completion+search)
Bundle 'ervandew/supertab'
Bundle 'SearchComplete'
""" yank stack (use <leader>p instead of <M>p to cycle backwards)
Bundle 'maxbrunsfeld/vim-yankstack'
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
""" undo tree
Bundle 'Gundo'
""" indent levels visualizer: <leader>ig to switch off
Bundle 'nathanaelkane/vim-indent-guides'
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
"au VimEnter * :IndentGuidesEnable
""" tabular
Bundle 'godlygeek/tabular'
""" emacs' paredit slurping/barfing tool (a bliss for Lisp): use <leader>< / <leader>>
Bundle 'paredit.vim'
au BufNewFile,BufRead *.shen cal PareditInitBuffer()
""" surrounding symbols & tags fast editing
Bundle 'surround.vim'
""" rainbow parens: <leader>r to switch off
Bundle 'rainbow_parentheses.vim'
let g:rbpt_colorpairs = [
          \ ['blue',       '#FF6000'],
          \ ['cyan', '#00FFFF'],
          \ ['yellow',   '#FFFF00'],
          \ ['darkgreen',    '#00FF00'],
          \ ['White',         '#c0c0c0'],
          \ ['blue',       '#FF6000'],
          \ ['cyan', '#00FFFF'],
          \ ['yellow',   '#FFFF00'],
          \ ['darkgreen',    '#00FF00'],
          \ ['White',         '#c0c0c0'],
          \ ['blue',       '#FF6000'],
          \ ['cyan', '#00FFFF'],
          \ ['yellow',   '#FFFF00'],
          \ ['darkgreen',    '#00FF00'],
          \ ['White',         '#c0c0c0'],
          \ ]
let g:rbpt_max = 16
au VimEnter * RainbowParenthesesToggle
au Syntax   * RainbowParenthesesLoadRound
au Syntax   * RainbowParenthesesLoadSquare
nmap <leader>r :RainbowParenthesesToggle<CR>
""" circular snippets (<C-\> expands; <C-r><C-r><C-\> lists; <Tab>/<CR> goes in/out)
Bundle 'drmingdrmer/xptemplate'
" let g:SuperTabMappingForward = '<Plug>supertabKey' " + SuperTab
" let g:xptemplate_fallback = '<Plug>supertabKey'
" let g:xptemplate_key = '<Tab>'
" let g:xptemplate_pum_tab_nav = 1
" let g:xptemplate_minimal_prefix = 'full'
""" autocomplete
Bundle 'Shougo/neocomplcache'
let g:acp_enableAtStartup = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
let g:neocomplcache_dictionary_filetype_lists = {
      \ 'default' : '',
      \ 'vimshell' : '~/.vimshell_hist'
      \}
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()
" <CR> to close popup
inoremap <expr><CR>  pumvisible() ? neocomplcache#smart_close_popup() : "\<CR>"
" <TAB> to navigate (<C-n> induces <CR> so prefer <Down>)
inoremap <expr><TAB> pumvisible() ? "\<Down>" : "\<TAB>"
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS>  neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()
""" syntax checker
Bundle 'scrooloose/syntastic'
""" structure ending tool
" Bundle 'tpope/vim-endwise' " TODO: find other
""" comment tool
Bundle 'tomtom/tcomment_vim'
""" timestamp changes on save (iff English time display format)
Bundle 'timestamp.vim'
let g:timestamp_rep = '%a %d %b %Y %H:%M:%S %Z' " 24-hour clock
if has('win32') || has('win64')
  " match non-abbreviated %Z
  let g:timestamp_regexp = '\v\C%(<Last %([cC]hanged?|[mM]odified)\s*:\s+)@<=\a+ \d{2} \a+ \d{4} \d{2}:\d{2}:\d{2} %(\a|\s)*|TIMESTAMP'
endif
""" intelligent finder
Bundle 'FuzzyFinder'
Bundle 'kien/ctrlp.vim'
""" terminal
Bundle 'acx0/Conque-Shell'
"""  git manager
Bundle 'tpope/vim-fugitive'
map <Leader>gb :Gblame<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gd :Gdiff<CR>
map <Leader>gl :Glog<CR>
map <Leader>gc :Gcommit<CR>
""" haskell
""" markdown
Bundle 'haesken/vim-markdown'
""" javascript
Bundle 'kchmck/vim-coffee-script'
Bundle 'maksimr/vim-jsbeautify'
""" racket
""" python
Bundle 'tmhedberg/SimpylFold'
Bundle 'nvie/vim-flake8'
Bundle 'davidhalter/jedi-vim'
""" html/css
Bundle 'mattn/zencoding-vim'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'othree/html5.vim'
Bundle 'rstacruz/sparkup',{'rtp': 'vim/'}
""" ruby
Bundle 'tpope/vim-rails.git'
""" viml
Bundle 'L9'
""" rpn calc
Bundle 'tomtom/tcalc_vim'

if s:iCanHazVundle == 0
  echo "Installing Bundles..."
  echo ""
  :BundleInstall!
endif
filetype plugin indent on

" DEFAULT BEHAVIOR
set enc=utf-8
set encoding=utf-8
set fileencodings=utf-8
set background=dark
set history=1000
set clipboard+=unnamed
set ffs=unix,mac,dos
set viminfo+=!
set shiftround
set smarttab
set visualbell t_vb=
set autowrite
set scrolloff=3 " maintain more context around the cursor: 999 to centerize
set guioptions-=T
set guioptions+=c
set guioptions-=m
set guioptions-=r " no right scrollbar
set guioptions-=R
set guioptions-=l " no left scrollbar
set guioptions-=L
set history=1000 " boost history up
set undolevels=1000
set shortmess=atI " stifle many interruptive prompts
"silent exe '!umask 027; mkdir -p ~/.vim/{backup,doc,tmp,views}
set backupdir=~/.vim/backup " unused
set directory=~/.vim/tmp
"helptags ~/.vim/doc
set incsearch
set ignorecase
set smartcase
set lsp=0
set wildmenu
set wildmode=longest,list,full " bash-like
set wildignore+=*.o,*.obj,*.swp,*.bak,*.pyc,*.elc,*.class,*.*fsl,*.fasl
set ruler
set cmdheight=2
set lz
set hid
set backspace=indent,eol,start
set shortmess=atI
set report=0
set noerrorbells "set fillchars=vert:\,stl:\,stlnc:\
set showmatch
set cpoptions-=m
set mat=5
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
set novisualbell
set fo=tcrqn " see formatoptions help (auto-wraps text+comments using textwidth + auto-inserts the current comment leader + allows formatting of comments + align text)
set ai
set si
set cindent
set nojoinspaces
set nocp
set cursorline
set whichwrap=b,s,<,>,[,]
set bufhidden=hide " no distant file reload
set equalalways " multiple windows, when created, are equal in size
set splitbelow splitright

" WINDOWS OS CASE
if has('win32') || has('win64')
  " PURGE CLASSIC PATH FILE
  set rtp-=~/vimfiles
  set rtp+=~/.vim
  " DISPLAY BETTER DEFAULT FONT
  if has('gui_running')
    if filereadable("C:\\Windows\\Fonts\\DejaVuSansMono.ttf")
      set guifont=DejaVu_Sans_Mono:h12
    elseif
      set guifont=Consolas:h9:cANSI
    endif
    set lines=40
  endif
endif

" KEY
"" <F2> to paste
set pastetoggle=<F2>
"" ,ev & ,sv to edit & source this file
nmap <silent> <leader>ev :sp $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
"" Q to format the current paragraph (or selection)
vmap Q gq
nmap Q gqap
"" easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
"" highlighted searches' clearing
nmap <silent> ,/ :nohlsearch<CR>
"" sudo admin
cmap w!! w !sudo tee % >/dev/null
"" session (function below)
nmap <silent> <leader>ss :cal SaveSession()<CR>

" VIEW + SESSION
set viewdir=~/.vim/views
set sessionoptions=blank,buffers,curdir,globals,help,localoptions,options,resize,tabpages,winsize,folds
if !exists("s:sessionautoloaded")
  let s:sessionautoloaded = 0
endif
au VimEnter * exe MySessionLoader()
fun! MySessionLoader()
  if filereadable('./.session.vim')
    if s:sessionautoloaded == 0
      source ./.session.vim
      let s:sessionautoloaded = 1
    endif
  endif
endfun
fun! SaveSession()
  if s:sessionautoloaded == 1
    mksession! ./.session.vim
    echo "Session saved."
  else
    echo "No session to save. Please create session with ':mksession .session.vim' first!"
  endif
endfun

" FOLD
set foldenable " turn on folding
set foldmethod=syntax " make folding syntax sensitive
set foldlevel=5 " don't autofold anything (but I can still fold manually)
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff
set foldminlines=2 " don't fold 3 lines
set fillchars=fold:=
set foldtext=MyFoldFunction()
fun! MyFoldFunction()
    let line = getline(v:foldstart)
    let sub = substitute(line, '/\*\|\*/\|^\s+', '', 'g')
    let lines = v:foldend - v:foldstart +1
    return '[+] ' . v:folddashes.sub . '...' . lines . ' lines ...' . getline(v:foldend)
endfun

" SPECIFIC FILETYPE BEHAVIOR
"" HTML
au Filetype html,htm,xml setl listchars-=tab:>.
"" INDENT STYLE FAMILY
au Filetype make,python,ruby setl noet
"" TRAILING WHITESPACE KEEPER'S FAMILY
au Filetype ruby,javascript,perl let b:noStripWhitespace=1
"" OMNIFUNC
au Filetype html,htm,markdown setl omnifunc=htmlcomplete#CompleteTags
au Filetype xml setl omnifunc=xmlcomplete#CompleteTags
au Filetype css setl omnifunc=csscomplete#CompleteCSS
au Filetype javascript setl omnifunc=javascriptcomplete#CompleteJS
au Filetype python setl omnifunc=pythoncomplete#Complete
" + NEOCOMPLCACHE
" if !exists('g:neocomplcache_omni_patterns')
"   let g:neocomplcache_omni_patterns = {}
" endif
" let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
" let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
" let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
" let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'

" ADDITIONAL FILETYPE ASSOCIATION
augroup filetypedetect
  au BufNewFile,BufRead *.shen set filetype=shen
augroup END

" UTILITY FUNCTIONS
"" trailing whitespace crusher
fun! Preserve( command )
  let _s = @/
  let l = line(".")
  let c = col(".")
  exe a:command
  let @/=_s
  cal cursor( l, c )
endfun
au! BufWritePre * if !&bin && &ft != 'diff' && !exists('b:noStripWhitespace') | cal Preserve( "%s/\\s\\+$//e" ) | endif

"" restore cursor position on reload
fun! s:CursorOldPosition()
  if line("'\"") > 0 && line("'\"") <= line("$")
    exe "normal g`\""
  endif
endfun
au BufReadPost * silent! cal s:CursorOldPosition()

"" toggle numbers
if v:version >= 703
  fun! NumberToggle()
    if (&relativenumber == 1) | set nu | else | set rnu | endif
  endfun
  nnoremap <F3> :cal NumberToggle()<CR>
endif

" Last Modified: Tue 05 Feb 2013 14:18:36 CET

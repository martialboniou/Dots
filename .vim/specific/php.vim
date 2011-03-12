" Detect syntax error
set makeprg=php\ -l\ %
set errorformat=%m\ in\ %f\ on\ line\ %1
let php_folding=1
let php_sql_query=1
let php_htmlInStrings=1
let php_noShortTags=1

" Load the PHPDoc addon
execute 'source ' . $HOME . '/.vim/specific/php-doc.vim'
map <C-p> <ESC>:call PhpDocSingle()<CR>
imap <C-p> <ESC>:call PhpDocSingle()<CR>i

" syntax error checker
map <A-F2> <ESC>:make<CR>
map <A-F3> <ESC>:split<CR>:exe 'vimgrep /' . expand('<cWORD>') . '/**/*.php **/*.inc **/*.ini'<CR><ESC>:exe '3match Search /' . expand('<cWORD>') . '/'<CR>
map <A-F10> <ESC>:exe '!open http://fr2.php.net/manual/fr/function.' . substitute(expand('<cWORD>'), '_', '-', '') . '.php'<CR><CR>
imap <A-F10> <ESC>:exe '!open http://fr2.php.net/manual/fr/function.' . substitute(expand('<cWORD>'), '_', '-', '') . '.php'<CR><CR>


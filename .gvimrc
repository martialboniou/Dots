"" Mars GVimrc
"colorscheme inkpot " this theme is cool too!
"colorscheme ir_black
set columns=93
if has("mac")
  set gfn=DejaVu_Sans_Mono:h14
  set lines=52
elseif has("x11")
  if filereadable(".notebook") " if notebook => smaller fonts
    set gfn=DejaVu\ Sans\ Mono\ 10
  else
    set gfn=DejaVu\ Sans\ Mono\ 14
  endif
 set lines=40
endif

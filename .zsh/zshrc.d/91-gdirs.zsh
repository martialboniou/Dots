#!/usr/bin/env zsh
# William G. Scott/Wataru Kagawa/Gary Kerbaugh

# ---[ Load ]----------------------------------------------------------

autoload -U dirstack dirdump cpath

# ---[ Limit Size ]----------------------------------------------------

# keep the zdirdump file from growing too long (250 entries seems ok)
tmp_timestamp=$(date | awk '{print $4}')
#[[ ! -d ~/.zsh ]] && mkdir -p ~/.zsh
command touch $ZDOTDIR/zdirdump
command cp $ZDOTDIR/zdirdump /tmp/$tmp_timestamp.zdirdump
command tail -n 250 /tmp/$tmp_timestamp.zdirdump >| $ZDOTDIR/zdirdump
command rm -f /tmp/$tmp_timestamp.zdirdump

typeset -ga chpwd_functions
chpwd_functions+=settab
chpwd_functions+=settitle
chpwd_functions+=dirdump

# ---[ GUI ]-----------------------------------------------------------
typeset -U dirs_shared
dirstack $1 > /dev/null

# ---[ Alias Section ]-------------------------------------------------
alias dirstack="dirdump; typeset -U dirs_shared ; dirstack"
alias cd\?="dirdump; typeset -U dirs_shared; dirstack"
if [[ $(uname) == Darwin ]];then
 autoload -U _guidirs
 #_guidirs 2> /dev/null
 alias gdirs="dirdump; typeset -U dirs_shared; dirstack > /dev/null ; _guidirs"
 alias gd="dirdump; typeset -U dirs_shared; dirstack > /dev/null ; _guidirs"
fi

# ---[ Dump ]----------------------------------------------------------
##if [[ -f $ZDOTDIR/dirstack ]] && [[ ${#dirstack[*]} -eq 0 ]]; then
## dirstack=( ${(uf)"$(< $ZDOTDIR/dirstack)"} )
## echo "Loaded the dirstack from disk..."
##fi
##chpwd() { dirs -pl >! $ZDOTDIR/dirstack }


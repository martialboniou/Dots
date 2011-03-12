#!/usr/bin/zsh
#
# use ALT_U and ALT_P to go up one level in history and 
# go backwards respectively so we have no more 'cd ..'
# lines in our terminal

go-up () {
 cd ..
 zle reset-prompt
}; zle -N go-up

go-to-previous () {
 popd
 zle reset-prompt
}; zle -N go-to-previous

bindkey '^[u' go-up
bindkey '^[p' go-to-previous



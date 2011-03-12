# ---[ Key bindings ]--------------------------------------------------
bindkey -e
bindkey "^[[3~" delete-char
bindkey '\eq' push-line-or-edit
bindkey '^p' history-search-backward
bindkey "^[[3A"  history-beginning-search-backward
bindkey "^[[3B"  history-beginning-search-forward
bindkey -s '^B' " &\n"


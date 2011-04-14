# ---[ Key bindings ]--------------------------------------------------
bindkey -v
# Vi modal edition rules but need some extras like the very addicted ^A/^E/^T
bindkey -M viins '^a' beginning-of-line
bindkey -M vicmd '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M vicmd '^e' end-of-line
bindkey -M viins '^t' transpose-chars
bindkey -M vicmd '^t' transpose-chars
bindkey "^[[3~" delete-char
bindkey '^q' push-line-or-edit
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward
bindkey "^[[3A"  history-beginning-search-backward
bindkey "^[[3B"  history-beginning-search-forward
bindkey -s '^B' " &\n"

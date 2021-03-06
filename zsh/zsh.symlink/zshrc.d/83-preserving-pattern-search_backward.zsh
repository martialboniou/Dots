# ---[ Ctrl-R to history incremental preserving pattern search backward ]--------
autoload -Uz narrow-to-region
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
function _history-incremental-preserving-pattern-search-backward
{
  local state
  MARK=CURSOR # magick, else multiple ^R don't work
  narrow-to-region -p "$LBUFFER${BUFFER:+>>}" -P "${BUFFER:+<<}$RBUFFER" -S state
  zle end-of-history
  zle history-incremental-pattern-search-backward
  narrow-to-region -R state
}
zle -N _history-incremental-preserving-pattern-search-backward
bindkey "^R" _history-incremental-preserving-pattern-search-backward
bindkey -M isearch "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward

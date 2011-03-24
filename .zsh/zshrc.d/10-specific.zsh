# ---[ Specific ]------------------------------------------------------

# MPD daemon
# `ls` colors & dircolors
# source-highlight
# OpenStep `open` command for all systems

# Mac OS X specificities first
if [ "`uname`" = "Darwin" ]; then
  # MPD daemon launch issue on Mac OS X 10.5 
  #type detach &>/dev/null && alias mpd="detach mpd --no-daemon" || echo "MPD doesn't work as expected"
  SRC_HILITE_LESSPIPE=/opt/local/bin/src-hilite-lesspipe.sh # port install source-highlight
  OPEN_COMMAND='open'
  MANPATH=`/usr/bin/manpath`
  export MANPATH=/opt/local/share/man:$MANPATH # MacPorts: man
  export LSCOLORS=dxgxcxdxbxegedabagacad
  alias ls='ls -G' # AIX/BSD ls command
  alias ghci='EDITOR=mvim ghci'
  alias gvim=mvim
  function wman() {
    url="man:${1}"
    echo `open $url` # install Bwana; ManOpen is DEPRECATED
  }
  alias gitk="/usr/bin/wish $(which gitk)"
  MAN_COMMAND=wman
else # Default
  test -r "$HOME/.dircolors" && eval `dircolors -b $HOME/.dircolors`
  SRC_HILITE_LESSPIPE=/usr/share/source-highlight/src-hilite-lesspipe.sh
  OPEN_COMMAND='gnome-open' # I prefer Gnome to XFCE but I switch metacity for Xmonad
  alias ls='ls --color=auto'
  alias -g open="$OPEN_COMMAND"
fi
# check 50-alias.zsh to avoid overrides

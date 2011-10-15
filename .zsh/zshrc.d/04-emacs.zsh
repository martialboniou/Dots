# ---[ Emacs ]---------------------------------------------------------
isemacs(){ return 0 }
if [[ "$TERM" == "eterm-color" ]]; then
  TERM=xterm-256color # eterm-color is not terminfo friendly
  isemacs(){          # remember we boot on emacs
    return 1
  }
fi
# isemacs should be unset

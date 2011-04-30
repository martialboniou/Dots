#!/usr/bin/env zsh
# TMUX
# if no session is started, start a new session
# http://chm.duquesne.free.fr/blog

if test -z ${TMUX}; then
  tmux $* # $* to pass args like `chdir` at startup 
fi
# when quitting tmux, try to attach if DTMUX is not unset
while [[ -z ${TMUX} ]]; do
  nop() { }
  trap nop INT
  ( sleep 5; kill -INT $$ 2> /dev/null ) & disown %%
  echo -n "Don't re-attach last session [y/k/_]? "
  read -k1 DTMUX
  case ${DTMUX:-out} in
      y) tmux attach || break
          ;;
      K) while true; do
          exit
         done
          ;;
      out) echo "$(tput cuu1)$(tput dl1)*** timeout ***"
          break
          ;;
      *) break
          ;;
  esac
done

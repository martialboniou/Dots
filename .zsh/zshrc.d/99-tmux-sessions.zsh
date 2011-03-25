#!/usr/bin/env zsh
# TMUX
# if no session is started, start a new session
# http://chm.duquesne.free.fr/blog

if test -z ${TMUX}; then
  tmux -2
fi
# when quitting tmux, try to attach if DTMUX is not unset
while [[ -z ${TMUX} ]]; do
  echo "Don't re-attach last session [y/_]?"
  read DTMUX
  echo $DTMUX | grep "[y|Y].*"
  if [ $? -ne 0 ]; then
    tmux attach || break
  else
    break
  fi
done

HIDDEN_DIRECTORY=.tools # you should hide the home root directory on desktop systems (NeXT/OSX)
_MAYBE_HOME=${HOME}/${HIDDEN_DIRECTORY}
if [[ ! -d "$_MAYBE_HOME" ]]; then _MAYBE_HOME=${HOME} fi
[[ -d "$_MAYBE_HOME/bin" ]] && add_path "$_MAYBE_HOME/bin"
unset _MAYBE_HOME

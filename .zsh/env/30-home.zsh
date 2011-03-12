HIDDEN_DIRECTORY=.tools # you should hide the home root directory on desktop systems (NeXT/OSX)
MAYBE_HOME=${HOME}/${HIDDEN_DIRECTORY}
if [[ ! -d "${MAYBE_HOME}" ]]; then MAYBE_HOME=${HOME} fi

add_path ${MAYBE_HOME}/bin

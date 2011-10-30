# ---[ Emacs ]---------------------------------------------------------

# check if we start the shell from emacs and remember this
emacs_acquaint(){ return 0 }
if [[ "$TERM" == "eterm-color" ]]; then
  TERM=xterm-256color
  emacs_acquaint(){
    return 1
  }
fi

# append your configuration directory (ie .emacs.d/lisp) to the emacs 'LOAD-PATH
reloadpath(){
  if (( $+commands[emacs] )); then
    local elp=`emacs -batch -eval "(princ (mapconcat #'(lambda (e) (format \"%s\" e)) load-path \":\"))"`
    if [[ "/" = "`echo $elp | awk 'BEGIN{FS=\"\"}{print $1}'`" ]]; then
      # emacs 'LOAD-PATH looks like a path
      export EMACSLOADPATH="~/.emacs.d/lisp:$elp"
    fi
  fi
} && reloadpath

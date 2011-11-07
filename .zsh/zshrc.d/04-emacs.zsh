# ---[ Emacs ]---------------------------------------------------------

# check if we start the shell from emacs and remember this
emacs_acquaint(){ return 0 }
if [[ "$TERM" == "eterm-color" ]]; then
  TERM=xterm-256color
  emacs_acquaint(){
    return 1
  }
fi

# generate EMACS load-path and append your configuration directory (here .emacs.d/lisp)
build_emacs_load_path(){
  if (( $+commands[emacs] )); then
    local elp=`emacs -batch \
-eval "(princ (mapconcat #'(lambda (e) (format \"%s\" e)) load-path \":\"))"`
    if [[ "/" = "`echo $elp | awk 'BEGIN{FS=\"\"}{print $1}'`" ]]; then
      # emacs 'LOAD-PATH looks like a path
      export EMACSLOADPATH="${HOME}/.emacs.d/lisp:$elp"
    fi
  fi
}

fetch_emacs_load_path(){
  local path_file="${ZDOTDIR}/.emacsloadpath"
  if [[ -a ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
    source ${path_file}
  else
    build_emacs_load_path
    echo "export EMACSLOADPATH=${EMACSLOADPATH}" > ${path_file}
  fi
} && fetch_emacs_load_path

# MEMO: rehash
reloadpath () {
  local force_reload
  fetch_emacs_load_path
}

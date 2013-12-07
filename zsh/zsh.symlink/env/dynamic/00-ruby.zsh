install_ruby_source () {
  [[ -s "$1" ]] && source "$1" && return 0
  return 1
}

() {
  local -a ruby_installer ruby_associate_post_install
  ruby_installs=( "/usr/local/share/chruby/chruby.sh" "$HOME/.rvm/scripts/rvm" )
  ruby_post_installs=( _chruby_post_install true )
  for (( i = 1; i <= ${#ruby_installs[@]}; i++ )); do
    if install_ruby_source "${ruby_installs[$i]}"; then
      local post_install=${ruby_post_install[$i]}
      chruby 2.0.0
      # if [[ -f $post_install ]]; then
      # return 0
    fi
  done
}

unfunction install_ruby_source

# .zshenv
#

add_path () {
  path=($1 $path)
}

# generate full path
build_path () {
  local SYSTEM=`uname`
  local ADMIN_ACTION=1           # 0 not to reach sbin subdirs
  local env_directory=${ZDOTDIR}/env
  if [[ -d "${env_directory}" ]]; then
    foreach file in $(command ls -dr ${env_directory}/*)
      source $file
    end
  else
    unset path # reset when no env sub-directory
  fi
  # TODO: check msys path
  [[ -d "/usr/X11/bin" ]] && path=($path /usr/X11/bin)
  path=($path /usr/bin /usr/libexec /bin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/usr/sbin" ]] &&  path=($path /usr/sbin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/sbin" ]] && path=($path /sbin)
  typeset -U -g PATH
}

fetch_path () {
  local path_file="${ZDOTDIR}/.path"
  if [[ -a ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
    source ${path_file}
  else
    build_path
    echo "PATH=${PATH}" > ${path_file}
  fi
} && fetch_path

# MEMO: rehash
repath () {
  local force_reload
  fetch_path
  rehash
}

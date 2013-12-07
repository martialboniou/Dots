# .zshenv
#

add_path () {
  path=($1 $path)
}

absolute_path () {
  echo $(perl -e 'use Cwd "abs_path"; print abs_path("$1");')
}

# source each file in a path
_source_path () {
  if [[ -d "$1" ]]; then
    foreach file in $(find $1 -type f | sort -r 2>/dev/null)
      source $file
    end
    return 0
  else
    return 1
  fi
}

# generate basic path from env/static
_build_path () {
  local SYSTEM=`uname`
  local ADMIN_ACTION=1           # 0 not to reach sbin subdirs
  local env_directory=${ZDOTDIR}/env/static
  _source_path $env_directory || unset path # reset when nothing
  # TODO: check msys path
  [[ -d "/usr/X11/bin" ]] && path=($path /usr/X11/bin)
  path=($path /usr/bin /usr/libexec /bin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/usr/sbin" ]] &&  path=($path /usr/sbin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/sbin" ]] && path=($path /sbin)
}

# complete global path from env/dynamic
_complete_path () {
  local env_directory=${ZDOTDIR}/env/dynamic
  _source_path "$env_directory" || return 0
}

# get the full path
_fetch_path () {
  local path_file="${ZDOTDIR}/env/static/.path"
  if [[ -a ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
    source ${path_file}
  else
    _build_path
    echo "PATH=${PATH}" > ${path_file}
  fi
  _complete_path
  typeset -U -g PATH
} && _fetch_path

# MEMO: rehash
repath () {
  local force_reload
  if [[ "`uname -s`" == "Darwin" ]]; then
    # IMPORTANT: ensure reset before calling `path_helper -s'
    export PATH=
    source /etc/zshenv
  else
    export PATH=/usr/bin:/bin # ensure reset
  fi
  _fetch_path
  rehash
}

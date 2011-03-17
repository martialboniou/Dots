# .zshenv
#

unset __purge  # procedural flag for the ENV_DIR loop
# ENV_DIR scripts cannot use the current path so define some vars here
SYSTEM=`uname`
ADMIN_ACTION=1                  # 0 not to reach sbin

function add_path () {
  path=($1 $path)
}

ENV_DIR=${ZDOTDIR}/env
if [[ -d "${ENV_DIR}" ]]; then
  foreach file in $(command ls -dr ${ENV_DIR}/*)
    if [ ! $__purge ]; then
      __purge=1
    fi
    source $file
  end
else
  unset path # reset when no env sub-directory
fi

# TODO: cygwin AND windows path
[[ -d "/usr/X11/bin" ]] && path=($path /usr/X11/bin)
path=($path /usr/bin /usr/libexec /bin)
[[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/usr/sbin" ]] &&  path=($path /usr/sbin)
[[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/sbin" ]] && path=($path /sbin)
typeset -U PATH # remove dup

unset __purge
unset ADMIN_ACTION

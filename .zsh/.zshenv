# .zshenv
#

unset __purge  # procedural flag for the ENV_DIR loop
# ENV_DIR scripts cannot use the current path so define some vars here
SYSTEM=`uname`

function add_path () {
  path=($path $1)
}

ENV_DIR=${ZDOTDIR}/env
if [[ -d "${ENV_DIR}" ]]; then
  foreach file in $(command ls -d ${ENV_DIR}/*)
    if [ ! $__purge ]; then
      unset path # reset the first time
      __purge=1
    fi
    source $file
  end
else
  unset path # reset when no env sub-directory
fi

# TODO: cygwin AND windows path
path=($path /usr/local/bin /usr/local/sbin /usr/X11/bin /usr/bin /usr/sbin /usr/libexec /bin /sbin)
typeset -U PATH # remove dup

unset __purge

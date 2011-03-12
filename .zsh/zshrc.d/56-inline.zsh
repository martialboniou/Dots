# ---[ Inline functions ]----------------------------------------------
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }
# Simple commandline calculator
function calc () {
    awk "BEGIN { print $@ }"
}


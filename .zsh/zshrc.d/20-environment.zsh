# ---[ Environment ]---------------------------------------------------
export EDITOR=vi
# Unicode Locale
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
#export LANG=fr_FR.UTF-8
#export LC_ALL=fr_FR.UTF-8
export PS_PERSONALITY='linux'

# Manpath & Manualpage search order
export MANSECT=3:2:9:8:1:5:4:7:6:n

# Syntax highlight for less with 'source-highlight'
PAGER='less -X -M'
export LESSOPEN="| $SRC_HILITE_LESSPIPE %s"
export LESS=' -R '

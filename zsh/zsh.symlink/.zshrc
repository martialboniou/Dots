# .zshrc for MacOSX/ubuntu/debian
# config,alias,dvorakize 2010 by Martial <hondana@gmx.net>
# idea (c) 2001 by Robert Manea <rob dot manea at gmail dot com>
#
# installed packages: source-highlight
# note: - dark background is the standard here
#       - local user path for binaries/libraries is not $HOME but $HOME/.tools
#       - dirstack is saved between sessions (only if there's directory changes
#       during the session); use the alias 'd' to get the dirs listing
#

# files
ZDOT_SRC_ZSH=${ZDOTDIR}/zshrc.d
ZDOT_FUNCTIONS=${ZDOT_SRC_ZSH}/functions

# additional functions
if [[ -d $ZDOT_FUNCTIONS ]]; then
 fpath=($ZDOT_FUNCTIONS $fpath)
fi
export fpath
typeset -U fpath

# zshrc.d = debian-like zshrc sourcing
if [[ -d "${ZDOT_SRC_ZSH}" ]]; then
 foreach file in $(command ls -d ${ZDOT_SRC_ZSH}/* | grep -v $ZDOT_FUNCTIONS)
 source $file
 end
fi

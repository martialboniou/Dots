# ---[ ZSH Options ]----------------------------------------------------
# General
setopt   ALWAYS_TO_END BASH_AUTO_LIST NO_BEEP CLOBBER
setopt   AUTO_CD CD_ABLE_VARS MULTIOS CORRECT_ALL

# Job Control
setopt   CHECK_JOBS NO_HUP

# History
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_DUPS HIST_FIND_NO_DUPS
setopt	 EXTENDED_HISTORY HIST_EXPIRE_DUPS_FIRST
setopt   HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS

# Stay compatible to sh and IFS
setopt	 SH_WORD_SPLIT

setopt   notify globdots pushdtohome
setopt   recexact longlistjobs
setopt   autoresume pushdsilent
setopt   autopushd pushdminus extendedglob rcquotes mailwarning pushdsilent pushdtohome
unsetopt BG_NICE HUP autoparamslash

# Prompt: minimal prompt for emacsen vt
if [[ isemacs -eq 0 ]]; then
  source $ZDOTDIR/prompts/advanced
else
  source $ZDOTDIR/prompts/minimal
fi
setprompt

# Don't expand files matching:
fignore=(.o .c~ .old .pro)


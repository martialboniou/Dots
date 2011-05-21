# ---[ Modules ]-------------------------------------------------------
zmodload zsh/complist
autoload -Uz compinit
compinit
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile
autoload -U zmv

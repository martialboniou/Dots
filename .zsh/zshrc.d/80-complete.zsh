# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' squeeze-slashes true # remove trailing slash (useful for ln)
zstyle ':completion:*:cd:*' ignore-parents parent pwd # cd ../<tab> doesn't show parent
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# grml.org/zsh/zsh-lovers::proxy for faster apt/port/dpkg
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/completion-cache
# grml.org/zsh/zsh-lovers::CVS uncompleted
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'
# process ID
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle :compinstall filename '/home/robert/.zshrc'


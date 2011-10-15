# ---[ Emacs ]---------------------------------------------------------
if [ "$EMACS" ];then
    export TERM=xterm-256color # define (multi-)term as xterm to avoid old incompatibility
fi
isemacs(){
    [[ "$EMACS" != "" ]] && return 0
    return 1
}

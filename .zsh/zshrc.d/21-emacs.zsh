# ---[ Emacs ]---------------------------------------------------------
if [ "$EMACS" ];then
    export TERM=Eterm-color
fi
isemacs(){
    [[ "$EMACS" != "" ]] && return 0
    return 1
}

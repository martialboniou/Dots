if [[ "$SYSTEM" == "Darwin" ]]; then # use X11 version, it's faster
    _VW_VERSION=7.6nc
    _VW_ROOT=${HOME}/Dynamics/Smalltalk
    VW_BIN=${_VW_ROOT}/Visualworks/vw${_VW_VERSION}/bin/macxx11/visual.app/Contents/MacOS
    add_path ${VW_BIN}
    unset _VW_ROOT
    unset _VW_VERSION
fi

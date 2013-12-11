() {
  if [[ "$SYSTEM" == "Darwin" ]]; then # use X11 version, it's faster
    local _VW_VERSION=7.6nc
    local _VW_ROOT=${HOME}/Documents/Code/st
    VW_BIN=${_VW_ROOT}/Visualworks/vw${_VW_VERSION}/bin/macxx11/visual.app/Contents/MacOS
    [[ -x "$VW_BIN" ]] && add_path ${VW_BIN}
  fi
}

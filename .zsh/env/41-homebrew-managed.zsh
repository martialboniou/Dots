() {
  local brewery="/usr/local"
  if [[ "$SYSTEM" == "Darwin" ]]; then
      [[ -d "/usr/brewery" ]] && brewery="/usr/brewery"
      # sbin if auth
      [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "${brewery}/sbin" ]] && add_path "${brewery}/sbin"
      # bin if any
      [[ -d "${brewery}/bin" ]] && add_path "${brewery}/bin"
      # python scripts first
      [[ -d "${brewery}/share/python" ]] && add_path "${brewery}/share/python"
  fi
}

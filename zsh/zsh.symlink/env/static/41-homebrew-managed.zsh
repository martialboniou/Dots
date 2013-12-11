() {
  if [[ "$SYSTEM" == "Darwin" ]]; then
      local brewery=`brew --prefix` || "/usr/local"
      # sbin if auth
      [[ $ADMIN_ACTION -gt 0 ]] && add_path "${brewery}/sbin"
      # bin if any
      add_path "${brewery}/bin"
      # python scripts first
      add_path "${brewery}/share/python"
  fi
}

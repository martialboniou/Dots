if [[ "$SYSTEM" == "Darwin" ]]; then
    _HOMEBREW=`brew --prefix`
    # sbin if auth
    [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "$_HOMEBREW/sbin" ]] && add_path "$_HOMEBREW/sbin"
    # bin if any
    [[ -d "$_HOMEBREW/bin" ]] && add_path "$_HOMEBREW/bin"
    # python scripts first
    [[ -d "$_HOMEBREW/share/python" ]] && add_path "$_HOMEBREW/share/python"
    unset _HOMEBREW
fi

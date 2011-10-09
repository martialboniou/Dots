if [[ "$SYSTEM" == "Darwin" ]]; then
    _HOMEBREW=/usr/brewery
    [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "$_HOMEBREW/sbin" ]] && add_path "$_HOMEBREW/sbin"
    [[ -d "$_HOMEBREW/bin" ]] && add_path "$_HOMEBREW/bin"
    unset _HOMEBREW
fi

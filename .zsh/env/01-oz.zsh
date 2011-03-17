if [[ "$SYSTEM" -eq "Darwin" ]]; then
    _MOZART=${HOME}/Applications/Mozart.app/Contents/Resources
    [[ -d "$_MOZART/bin" ]] && add_path "$_MOZART/bin"
    unset _MOZART
fi

if [[ "$SYSTEM" -eq "Darwin" ]]; then
    [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/opt/local/sbin" ]] && add_path /opt/local/sbin
    [[ -d "/opt/local/bin" ]] && add_path /opt/local/bin
fi

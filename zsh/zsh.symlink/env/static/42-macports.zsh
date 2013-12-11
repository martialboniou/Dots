if [[ "$SYSTEM" == "Darwin" ]]; then
    [[ $ADMIN_ACTION -gt 0 ]] && add_path /opt/local/sbin; add_path /opt/local/bin
fi

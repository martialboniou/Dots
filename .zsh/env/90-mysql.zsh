if [[ "$SYSTEM" -eq "Darwin" ]]; then
  [[ -d "/Library/MySQL/bin" ]] && add_path /Library/MySQL/bin
fi

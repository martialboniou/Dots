# ---[ Terminal settings ]---------------------------------------------
case "$TERM" in
	linux)
		bindkey '\e[1~' beginning-of-line	# Home
		bindkey '\e[4~' end-of-line		# End
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert
		;;
	screen)
		# In Linux console
		bindkey '\e[1~' beginning-of-line	# Home
		bindkey '\e[4~' end-of-line		# End
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert
		bindkey '\e[7~' beginning-of-line	# home
		bindkey '\e[8~' end-of-line		# end
		# In rxvt
		bindkey '\eOc' forward-word		# ctrl cursor right
		bindkey '\eOd' backward-word		# ctrl cursor left
		# bindkey '\e[3~' backward-delete-char	# This should not be necessary!
		;;
	urxvt*|rxvt*)
		bindkey '\e[7~' beginning-of-line	# home
		bindkey '\e[8~' end-of-line		# end
		bindkey '\eOc' forward-word		# ctrl cursor right
		bindkey '\eOd' backward-word		# ctrl cursor left
		bindkey '\e[3~' backward-delete-char	# This should not be necessary!
		bindkey '\e[2~' overwrite-mode		# Insert
		;;
	xterm*)
		bindkey "\e[1~" beginning-of-line	# Home
		bindkey "\e[4~" end-of-line		# End
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert
		;;
	sun)
		bindkey '\e[214z' beginning-of-line       # Home
		bindkey '\e[220z' end-of-line             # End
		bindkey '^J'      delete-char             # Del
		bindkey '^H'      backward-delete-char    # Backspace
		bindkey '\e[247z' overwrite-mode          # Insert
		;;
esac

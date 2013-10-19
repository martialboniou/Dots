if [[ "$SYSTEM" == "Darwin" ]]; then
    TETEX_BIN=/usr/texbin
    if [[ -d "$TETEX_BIN" ]]; then
        add_path ${TETEX_BIN}
    else
        unset TETEX_BIN
    fi
fi

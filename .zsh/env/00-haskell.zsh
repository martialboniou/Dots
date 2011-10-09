if [[ "$SYSTEM" == "Darwin" ]]; then
  # /Library/Haskell/bin are symlinked to /usr/bin
  _HASKELL_PLATFORM=${HOME}/Library/Haskell/bin
  [[ -d "$_HASKELL_PLATFORM" ]] && add_path $_HASKELL_PLATFORM
  unset _HASKELL_PLATFORM
else
  CABAL_BIN=${HOME}/.cabal/bin
  if [[ -d "$CABAL_BIN" ]]; then
    add_path "$CABAL_BIN"
    export CABAL_BIN
  else
    unset CABAL_BIN
  fi
fi

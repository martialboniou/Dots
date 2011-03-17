if [[ "$SYSTEM" -eq "Darwin" ]]; then
  # /Library/Haskell/bin are symlinked to /usr/bin
  _HASKELL_PLATFORM=${HOME}/Library/Haskell/bin
  [[ -d "$_HASKELL_PLATFORM" ]] && add_path $_HASKELL_PLATFORM
  unset _HASKELL_PLATFORM
else
  CABAL_HOME=${HOME}/.cabal
  if [[ -d "$CABAL_HOME/bin" ]]; then
    add_path "$CABAL_HOME/bin"
    export CABAL_HOME
  else
    unset CABAL_HOME
  fi
fi

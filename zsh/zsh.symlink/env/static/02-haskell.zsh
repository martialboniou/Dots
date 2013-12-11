[[ "$SYSTEM" == "Darwin" ]] && add_path ${HOME}/Library/Haskell/bin
export CABAL_BIN="$HOME/.cabal/bin"
[[ -d "$CABAL_BIN" ]] && add_path "$CABAL_BIN" || unset CABAL_BIN

# standard OS X git installer (useful if no homebrew/macports)
_GIT=/usr/local/git
[[ -d "$_GIT" ]] && add_path "$_GIT/bin"
unset _GIT

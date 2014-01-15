() {
  local MAJOR=$(echo -e "import sys\nprint sys.version_info.major" | python 2>/dev/null)
  local MINOR=$(echo -e "import sys\nprint sys.version_info.minor" | python 2>/dev/null)
  local SITE="/usr/local/lib/python${MAJOR}.${MINOR}/site-packages"
  [[ "$MAJOR" != "" ]] && [[ "$MINOR" != "" ]] && [[ -d "${SITE}" ]] && export PYTHONPATH="${SITE}:${PYTHONPATH}"
}

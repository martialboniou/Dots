() {
  [[ -x `which gem` ]] && add_path "$(gem environment gemhome)/bin"
}

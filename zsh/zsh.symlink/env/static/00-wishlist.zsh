# wishlist
# load a wishlist.table (see the pattern in this file)

#() {
#  for i in `cat wishlist.table`
#}

DIR=${0%/*}
zmodload zsh/mapfile

setopt shwordsplit

trim () {
  echo "$1" | sed -e 's/^ *//g' -e 's/ *$//g'
}

to_come_soon () {
  FILENAME="$DIR/wishlist.table"
  typeset -a lines
  typeset -a columns
  lines=( "${(f)mapfile[$FILENAME]}" )
  for (( i = ${#lines[@]}; i >= 1; i-- )); do
    line="$lines[i]"
    echo $line
    if [[ ! "$line" =~ ^\! ]]; then
      local ADMIN=0
      local FULLPATH=0
      local COM=""
      local saveIFS="$IFS"
      IFS=':'
      columns=( ${line} )
      IFS="$saveIFS"
      action=$(trim "$columns[1]")
      folder=$(trim "$columns[2]")
      os=$(trim "$columns[3]")
      variable=$(trim "$columns[4]")
      if [[ $ADMIN_ACTION -gt 0 && "$action" =~ ^\\* ]]; then
        ADMIN=1 # only admin can have `sbin'
      elif [[ "$action" =~ ^\= ]]; then
        FULLPATH=1 # mutually exclusive with ADMIN
      else
        # permute if no action column
        variable=$os; os=$folder; folder=$action
      fi
      [[ "$folder" == "" ]] && continue
      [[ "$os" != "" && "$SYSTEM" != "$os" ]] && continue
      # case of folder named by command
      [[ "$folder" =~ ^\` && "$folder" =~ \`\$ ]] &&\
        eval "folder=$folder" 2>/dev/null || continue
      [[ $ADMIN_ACTION -gt 0 && $ADMIN -gt 0 ]] && add_path "${folder}/sbin"
      [[ $FULLPATH -eq 0 ]] && folder+="/bin"
      #[[ $variable != "" ]] &&\
      #  echo "${variable}=\"${folder}\"" 2>/dev/null #||\
  #      echo "wishlist: error when setting ${variable}"
      add_path "${folder}"
    fi
  done
}

unset DIR

# Last Modified: Wed 11 Dec 2013 21:18:20 CET

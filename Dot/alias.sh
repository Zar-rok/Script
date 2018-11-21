#!/bin/bash

# ripgrep is also good
function find_file() {
  if [ "$#" -lt 2 ]
  then
    printf "[#] Usage : %s [Directory] [String to find]" "$0"
  else
    grep -rnw "$1" -e "$2"
  fi
}

function docker_ip() {
  if [ "$#" -lt 1 ]
  then
    printf "[#] Usage : %s [Container name]" $0"
  else
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
  fi
}

alias open-rc="nano ~/.zshrc"
alias relo-rc=". ~/.zshrc"
alias 2up="sudo apt update && sudo apt upgrade"
alias open="xdg-open"

# for xcas
alias calc="giac"

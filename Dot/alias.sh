#!/bin/bash

# ripgrep is also good
function find_file() {
  if [ "$#" -lt 2 ]; then
    printf "[#] Usage : %s [Directory] [String to find]" "$0"
  else
    grep -rnw "$1" -e "$2"
  fi
}

function docker_ip() {
  if [ "$#" -lt 1 ]; then
    printf "[#] Usage : %s [Container name]" "$0"
  else
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
  fi
}

export EDITOR=nvim
alias open-rc="nvim ~/.zshrc"
alias reload-rc=". ~/.zshrc"
alias ocaml="rlwrap ocaml"
alias 2up="sudo apt update && sudo apt upgrade"
alias open="xdg-open"
alias calc="giac"
alias yt2mp3="youtube-dl -x --audio-format mp3"
alias presentation="pdfpc -c --notes=right"
alias rename_space="rename 's/ /_/g'"
alias colortest="msgcat --color=test"

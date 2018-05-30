#!/bin/bash

function find_file() {
  if [ "$#" -lt 2 ]
  then
    printf "[#] Usage : find_file [Directory] [String to find]"
  else
    grep -rnw "$1" -e "$2"
  fi
}

function docker_ip() {
  if [ "$#" -lt 1 ]
  then
    printf "[#] Usage : docker_ip [container name]"
  else
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$1"
  fi
}

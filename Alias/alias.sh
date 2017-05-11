#!/bin/bash

function find_file() {
        if [ $# -lt 2 ]
        then
                printf "Usage : find_file [Directory] [String to find]"
        else
                grep -rnw "$1" -e "$2"
        fi
}

alias find_file="find_file"

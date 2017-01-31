#!/bin/bash

elements=("░" "▒" "▓")
width=$(($(tput cols) + 1))
height=$(($(tput lines) + 2))

clear
trap 'printf "\x1B[0m"; clear; exit' 2
while :
do
	tput setaf $(($RANDOM%256))
	printf "\033[%d;%dH%s" $(($RANDOM%$height)) $(($RANDOM%$width)) ${elements[$(($RANDOM%3))]}
done

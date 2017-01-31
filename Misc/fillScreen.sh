#!/bin/bash

function end {
	# Reset the default color
	printf "\x1B[0m"
	clear
	# Little bit of delay else
	# the following F11 doesn't execute
	sleep 0.1
	xdotool key F11
	exit
}

xdotool key F11

elements=("░" "▒" "▓")
width=$(($(tput cols) + 1))
height=$(($(tput lines) + 1))

clear
trap 'end' SIGINT
while :
do
	tput setaf $(($RANDOM%256))
	printf "\033[%d;%dH%s" $(($RANDOM%$height)) $(($RANDOM%$width)) ${elements[$(($RANDOM%3))]}
done

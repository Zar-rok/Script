#!/bin/bash

elements=("░" "▒" "▓")
width=$(tput cols)
height=$(tput lines)

while :
do
	printf "\033[%d;%dH%s" $(($RANDOM%$height)) $(($RANDOM%$width)) ${elements[$(($RANDOM%3))]}
done

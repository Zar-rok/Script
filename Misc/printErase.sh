#!/bin/bash

if [ $# -ne 1 ]
then
	echo "Usage : ./printErase.sh [String to display]"
	exit 2
fi

clear
NBRCHAR=$(($(echo "$1" | wc -c)-1))
COLS=$((($(tput cols)/2) - $NBRCHAR))
for letter in $(echo "$1" | grep -o .)
do
	printf "\033[%d;%dH$letter" $(($(tput lines)/2)) $COLS
	sleep 0.2
	COLS=$(($COLS+1))
done
sleep 2

COLS=$((($(tput cols)/2) - $NBRCHAR))
for letter in $(seq 1 $NBRCHAR)
do
        printf "\033[%d;%dH " $(($(tput lines)/2)) $COLS
        sleep 0.2
        COLS=$(($COLS+1))
done
sleep 2
printf "\n"

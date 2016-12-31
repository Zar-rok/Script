#!/bin/bash

MSG="$1"
LINES=$(($(tput lines) + 1))
COLUMNS=$(tput cols)
FLAKE=("※")
TAIL=(" " "  ")
 
declare -A snowflakes
declare -A lastflakes
 
function move_flake() {

	if [ "${snowflakes[$1]}" = "" ] || [ "${snowflakes[$1]}" = "$LINES" ]
	then
		snowflakes[$1]=0
	else
		if [ "${lastflakes[$1]}" != "" ]
		then
			printf "\033[%d;%dH%s" ${lastflakes[$1]} $1 "${TAIL[$(($RANDOM % ${#TAIL[@]}))]}"
		fi
	fi
	
	printf "\033[%d;%dH%s" ${snowflakes[$1]} $1 "${FLAKE[$(($RANDOM % ${#FLAKE[@]}))]}"
	lastflakes[$1]=${snowflakes[$1]}
	snowflakes[$1]=$((${snowflakes[$1]}+1))
}

clear

setterm --cursor off
trap "setterm --cursor on && exit" 2
while :
do
	move_flake $(($RANDOM % $COLUMNS))
	
	for x in "${!lastflakes[@]}"
	do
		printf "\033[%d;%dH%s" $((LINES / 2)) $((COLUMNS / 2 - $((${#MSG} / 2)) )) "$MSG"
		move_flake "$x"
	done

	sleep 5.0e-2
done

clear
NBRCHAR=$(($(echo "$1" | wc -c)-1))
COLS=$((($(tput cols)/2) - $NBRCHAR))
for letter in $(echo "$1" | grep -o .)
do
	printf "\033[%d;%dH$letter" $(($(tput lines)/2)) $COLS
	sleep 0.2
	COLS=$(($COLS+1))
done
sleep 5

COLS=$((($(tput cols)/2) - $NBRCHAR))
for letter in $(seq 1 $NBRCHAR)
do
        printf "\033[%d;%dH " $(($(tput lines)/2)) $COLS
        sleep 0.2
        COLS=$(($COLS+1))
done
sleep 5
printf "\n"

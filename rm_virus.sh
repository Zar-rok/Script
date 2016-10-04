#!/bin/bash

function rm_vir(){
	for TARGET in *
	do
		if [ -d "$TARGET" ]
		then
			cd "$TARGET"
			rm_vir "$1"
		else
			if [ -n $(head -c 12 "$TARGET" | grep "#!/bin/*sh") ]
			then
				if [ -z $(tail -n 25 "$TARGET" | grep "#POUET") ]
				then
					tac "$TARGET" | sed "1, 25d" | tac > "$TARGET"
					echo "$TARGET clean"
				fi
			fi
		fi
	done
}

rm_vir "$PWD/${0##*/}"

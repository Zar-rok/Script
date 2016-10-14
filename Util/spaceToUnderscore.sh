#!/bin/bash
# Replace all the space in the filename by underscore.

if [ $# -ne 1 ]
then
	echo "Usage : ./spaceToUnderscore.sh [Name of the directory]"
	exit 2
fi

dir=$(pwd)

find "$1" -regextype sed -regex ".* .*" -type f -print0 |
while read -d $'\0' file
do
	name=$(echo "$file" | sed 's/ /_/g')
	echo "${file##*/} ==> ${name##*/}"
	mv "$dir/$file" "$dir/$name"
done

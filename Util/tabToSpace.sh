#!/bin/bash

if [ $# -lt 3 ]
then
	printf "[#] Usage : %s [directory] [pattern to search] [nbr space]\n" "$0"
	exit 2
fi

printf "[*] Processed files :\n"
find "$1" -name "$2" ! -type d -exec bash -c "expand -t "$3" \"\$0\" > /tmp/tmp_file && mv /tmp/tmp_file \"\$0\" && printf \"    [-] \$0\n\"" {} \;

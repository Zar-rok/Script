#!/bin/bash

FILE_NAME="screen.png"
FILE_PATH="/tmp"
FILE_FULL="${FILE_PATH}/${FILE_NAME}"

# Take a screenshot and pixelize it
scrot "${FILE_NAME}" -e "mv ${FILE_NAME} ${FILE_PATH}"

TAINT=$(printf "%02x%02x%02x\n" $((RANDOM%256)) $((RANDOM%256)) $((RANDOM%256)))
convert "${FILE_FULL}" -scale 10% -scale 1000% -blur 0x1 -fill "#${TAINT}" -tint 50 "${FILE_FULL}"

# Add the given picture in the middle of the screenshot
if [ -f "$1" ]
then
  convert "${FILE_FULL}" "$1" -gravity center -composite -matte "${FILE_FULL}"
fi

# Lock and display the screenshot
i3lock -u -i "${FILE_FULL}"

rm "${FILE_FULL}"

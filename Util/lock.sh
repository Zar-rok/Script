#!/bin/bash

# Take a screenshot and pixelize it
convert x:root -scale 10% -scale 1000% /tmp/screen.png

# Add the given picture in the middle of the screenshot
if [ -f "$1" ]
then
  convert /tmp/screen.png "$1" -gravity center -composite -matte /tmp/screen.png
fi

# Lock and display the screenshot
i3lock -u -i /tmp/screen.png

rm /tmp/screen.png

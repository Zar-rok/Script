#!/usr/bin/env bash
set -euo pipefail

STR=$(rofi -dmenu -l 0 -p 'Search');

if [[ -n "$STR" ]]; then
    catfish \
      --start --large-icons \
      --thumbnails --start \
      --path=/home/zar "$STR"
fi

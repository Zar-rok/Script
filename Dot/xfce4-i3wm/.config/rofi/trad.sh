#!/usr/bin/env bash
set -euo pipefail

STR=$(rofi -dmenu -l 0 -p 'Trad');

if [[ -n "$STR" ]]; then
    xfce4-dict "$STR"
fi

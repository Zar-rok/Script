#!/usr/bin/env bash
#
# Move the focused window to the *n*-nth workspace to the left (or to the right for negative *n*).
# https://stackoverflow.com/a/55313834

set -euo pipefail

if [ "${#}" -eq 1 ]; then
    wsNext=$(($(i3-msg -t get_workspaces | jq '.[] | select(.focused).num') + $1))
    if [ -n "${wsNext}" ]; then
        i3-msg -q move container to workspace "${wsNext}"
        i3-msg -q workspace "${wsNext}"
    fi
fi

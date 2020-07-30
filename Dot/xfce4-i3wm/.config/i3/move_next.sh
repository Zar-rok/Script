#!/usr/bin/env bash
#
# Move the focused window to the *n*-nth workspace to the left (or to the right for negative *n*).
# https://stackoverflow.com/a/55313834

set -euo pipefail

wsNext=$(( $( i3-msg -t get_workspaces | jq '.[] | select(.focused).num' ) + $1))
i3-msg move container to workspace $wsNext
i3-msg workspace $wsNext

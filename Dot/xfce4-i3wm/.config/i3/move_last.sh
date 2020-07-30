#!/usr/bin/env bash
#
# Move the focused window to the last available workspace.
# i.e., if there is 3 workspace, the window is moved to the 4th workspace.

set -euo pipefail

wsNext=$(( $( i3-msg -t get_workspaces | jq '.[].num' | sort -r | head -1 ) + 1))
i3-msg move container to workspace $wsNext
i3-msg workspace $wsNext

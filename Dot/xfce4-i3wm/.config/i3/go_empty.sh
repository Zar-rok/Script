#!/usr/bin/env bash
set -euo pipefail

workspace_seq=$(i3-msg -t get_workspaces | jq '.[].num' | sort -n)
last_workspace=$(tail -n 1 <<<${workspace_seq})
empty_workspace=$(comm -13 <(printf "${workspace_seq}") <(seq 1 ${last_workspace}))

if [ -z "${empty_workspace}" ]; then
    empty_workspace=$((${last_workspace} + 1))
fi

i3-msg workspace ${empty_workspace}

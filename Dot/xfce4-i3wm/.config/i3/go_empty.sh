#!/usr/bin/env bash
set -euo pipefail

workspace_seq=$(i3-msg -t get_workspaces | jq '.[].num' | sort -n)
last_workspace=$(tail -n 1 <<<"${workspace_seq}")
empty_workspace=$(comm -13 <(printf "%s" "${workspace_seq}") <(seq 1 "${last_workspace}") | head -n 1)

if [ -z "${empty_workspace}" ]; then
    empty_workspace=$((last_workspace + 1))
fi

if [ "${#}" -eq 1 ] && [ "${1}" == "--move-window" ]; then
    i3-msg -q move container to workspace "${empty_workspace}"
fi

i3-msg -q workspace "${empty_workspace}"

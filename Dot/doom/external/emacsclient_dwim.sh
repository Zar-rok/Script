#!/usr/bin/env bash
set -euo pipefail

nbr_frame=$(emacsclient --eval "(length (frame-list))" 2>/dev/null)
if [[ nbr_frame -ge 2 ]]
then
    emacsclient "$@"
else
    emacsclient -c "$@"
fi

#!/usr/bin/env bash
# shellcheck disable=SC2145

# https://stackoverflow.com/a/28678964 for the dynamic --line-length
args=()
[ -n "$2" ] && args+=( "$2" )

rcf=$(mktemp)
rff=$(mktemp)
ruff check --silent --select=I --select=F401 --select=F841\
      "${args[@]}"\
      --stdin-filename="$1" --fix <&0 >"$rcf" -\
      && ruff format --silent\
      "${args[@]}"\
      --stdin-filename="$1" <"$rcf" >"$rff" -

if [ -s "$rff" ]; then
      cat "$rff"
else
      cat "$rcf"
fi

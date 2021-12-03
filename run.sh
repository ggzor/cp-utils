#!/usr/bin/env bash

set -euo pipefail

source cp-utils/common.sh

if (( $# < 1 )); then
  cat <<EOF
Usage: $0 FILE [ARGS...] [< INPUT]

Runs the given FILE with the correct interpreter/compiler with the
given args.

If the program uses stdin input you could provide it with INPUT.
EOF
  exit 1
fi


main() {
  FILE_PATH=$1
  FILE_DIR=$(dirname "$FILE_PATH")
  FILE_NAME=$(basename "$FILE_PATH")
  FILE_EXTENSION=${FILE_NAME##*.}

  if ! declare -F "$FILE_EXTENSION" &> /dev/null; then
    exit_error "No handler registered for extension: $FILE_EXTENSION"
  fi

  (
    cd "$FILE_DIR"
    "$FILE_EXTENSION" "$FILE_NAME" "${@:2}"
  )
}

# Extension handlers
hs() { runghc "$@"; }

main "$@"


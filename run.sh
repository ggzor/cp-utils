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

  ALTERNATIVE_RUNNER="_$FILE_EXTENSION"

  if declare -F "$ALTERNATIVE_RUNNER" &> /dev/null; then
    RUNNER=$ALTERNATIVE_RUNNER
  else
    if declare -F "$FILE_EXTENSION" &> /dev/null; then
      RUNNER=$FILE_EXTENSION
    else
      exit_error "No handler registered for extension: $FILE_EXTENSION"
    fi
  fi

  (
    cd "$FILE_DIR"
    "$RUNNER" "$FILE_NAME" "${@:2}"
  )
}

# Extension handlers
hs() { runghc "$@"; }
py() { python "$@"; }
_scala() { scala "$@"; }

main "$@"


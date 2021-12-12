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

CACHE_DIR='.cp_cache'
# Cache handling
cache_dir_for() {
  FILE=$1

  HASH=$(sha256sum "$FILE" | cut -d' ' -f1)
  TARGET_DIR="$CACHE_DIR/$HASH"

  printf '%s' "$TARGET_DIR"
  if [[ -d "$TARGET_DIR" ]]; then
    return 0
  else
    mkdir -p "$TARGET_DIR"
    return 1
  fi
}

cache_build() {
  FILE_NAME=$1
  BUILDER_ARGS=( "${@:2}" )

  if TARGET_DIR=$(cache_dir_for "$FILE_NAME"); then
    cd "$TARGET_DIR"
  else
    cp "$FILE_NAME" "$TARGET_DIR"

    local OLD_DIR=$(pwd)

    cd "$TARGET_DIR"
    if ! "${BUILDER_ARGS[@]}" "$FILE_NAME"; then
      cd "$OLD_DIR"
      rm -rf "$TARGET_DIR"
      exit 1
    fi
  fi
}

# Extension handlers
py() { python "$@"; }

hs() {
  FILE_NAME=$1
  FILE_STEM=${FILE_NAME%.rs}

  cache_build "$FILE_NAME" ghc -O3 -o "$FILE_STEM" 1>/dev/null \
    && "./$FILE_STEM" "${@:2}"
}

_scala() {
  FILE_NAME=$1

  cache_build "$FILE_NAME" scalac && scala main "${@:2}"
}

rs() {
  FILE_NAME=$1
  FILE_STEM=${FILE_NAME%.rs}

  cache_build "$FILE_NAME" rustc -o "$FILE_STEM" && "./$FILE_STEM" "${@:2}"
}

main "$@"


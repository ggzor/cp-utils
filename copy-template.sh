#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

source cp-utils/common.sh

usage() {
  cat <<EOF
Usage: $0 DIR LANGUAGES...

Copy to DIR the templates for the specified LANGUAGES.

The templates will be named the same as DIR, but with the first letter
capitalized.
EOF
  exit 1
}

if (( $# > 0 )); then
  DIR=$1
  shift
else
  echoerr "No directory specified"
  usage
fi

if (( $# > 0 )); then
  LANGS=( "$@" )
else
  echoerr "No languages specified"
  usage
fi
ALL_LANGS=$(printf '%s\n' "${LANGS[@]}")

should_use_file() {
  local FILE_PATH=$1
  local FILE_NAME=$(basename "$FILE_PATH")
  local FILE_WITHOUT_NAME=${FILE_NAME#*.}

  grep -F "$FILE_WITHOUT_NAME" <<< "$ALL_LANGS" &>/dev/null
}

TARGET_NAME=${DIR^}

mkdir -p "$DIR"
(
  cd "$DIR"

  CHECKED_FILES=0

  for SRC in ../cp-utils/templates/*; do
    if [[ ! -L "$SRC" ]]; then
      FILE_DIR=$(dirname "$SRC")
      FILE_NAME=$(basename "$SRC")
      LANGUAGE=${FILE_NAME%%.*}
      LANGUAGE_EXT=${FILE_NAME#*.}

      TARGET="${TARGET_NAME}.${LANGUAGE_EXT}"

      if should_use_file "$SRC"; then
        if [[ -f "$TARGET" ]]; then
          echo "Skipping $LANGUAGE"
          (( CHECKED_FILES += 1 ))
        else
          echo cp "$SRC" "$TARGET"
          cp "$SRC" "$TARGET"
          (( CHECKED_FILES += 1 ))
        fi
      fi
    fi
  done

  if (( CHECKED_FILES == 0 )); then
    exit_error "No template files were found for the specified languages"
  fi
)


#!/usr/bin/env bash

set -euo pipefail
shopt -s extglob

source cp-utils/common.sh

usage() {
  cat <<EOF
Usage: $0 PROGRAM [ARGS...]

Test PROGRAM run with ARGS against the test cases found in the
spec.yaml file
EOF
  exit 1
}

if (( $# < 1 )); then
  echoerr "No program specified"
  usage
fi

PROGRAM=$1
shift

PROGRAM_DIR=$(dirname "$PROGRAM")
SPEC_FILE="$PROGRAM_DIR/spec.sh"

OLD_DIR=$(pwd)

if [[ ! -f "$SPEC_FILE" ]]; then
  exit_error "No spec.sh file was found inside '$PROGRAM_DIR'"
fi

JUST_CHECK=0
_check() {
  JUST_CHECK=1

  check "$@"

  JUST_CHECK=0
}

resolve_path() {
  TARGET="$1"

  if [[ "$TARGET" == /* ]]; then
    echo "$TARGET"
  else
    echo "$PROGRAM_DIR/$1"
  fi
}

# Get some useful params
MAX_INPUT_LEN=0
check() {
  local INPUT=$(resolve_path "$1")

  if [[ "$1" == '-s' ]]; then
    INPUT="'$2'"
  fi

  if (( ${#INPUT} > $MAX_INPUT_LEN )); then
    MAX_INPUT_LEN=${#INPUT}
  fi
}

. "$SPEC_FILE"

align_first() {
  local ALIGN_WIDTH="$1"
  local FIRST="$2"

  printf " \e[2m%-${ALIGN_WIDTH}s\e[0m" "$FIRST"
  printf '\e[2m%s\e[0m' " ${@:3}"
}

check() {
  local INPUT=$(resolve_path "$1")

  local IS_STRING=0

  if [[ "$1" == '-s' ]]; then
    local INPUT_STR="$2"

    local TEMP_FILE=$(mktemp)
    printf '%s' "$INPUT_STR" > "$TEMP_FILE"

    INPUT="$TEMP_FILE"
    IS_STRING=1

    shift 1
  fi

  local TMP=$(mktemp -d)
  local EXPECTED_FILE="$TMP/expected"
  local OUTPUT_FILE="$TMP/current"

  set +e
  cat > "$EXPECTED_FILE"
  (
    cd "$OLD_DIR" \
    && cp-utils/run.sh "$PROGRAM" "${@:2}" < "$INPUT" 2>&1
  ) > "$OUTPUT_FILE"

  CMD_STATUS=$?
  set -e

  local MULTILINE=0
  if (( $(wc -l "$EXPECTED_FILE" | cut -d' ' -f1) > 1 )) \
    || (( $(wc -l "$OUTPUT_FILE" | cut -d' ' -f1) > 1 )); then
    MULTILINE=1
  fi

  if (( $CMD_STATUS == 0 )); then
    if (( $JUST_CHECK == 1 )); then
      STATUS=CHECK
    else
      if (( $MULTILINE == 1 )); then
        if diff "$EXPECTED_FILE" "$OUTPUT_FILE" &>/dev/null; then
          STATUS=SUCCESS
        else
          STATUS=WRONG
        fi
      else
        local OUTPUT=$(< "$OUTPUT_FILE")
        local EXPECTED=$(< "$EXPECTED_FILE")

        if [[ "$OUTPUT" == "$EXPECTED" ]]; then
          STATUS=SUCCESS
        else
          STATUS=WRONG
        fi
      fi
    fi
  else
    STATUS=RUNTIME_ERROR
  fi

  case $STATUS in
    SUCCESS) echo -en "\e[1;32m[RIGHT]\e[0m" ;;
    CHECK) echo -en "\e[1;2m[CHECK]\e[0m" ;;
    WRONG) echo -en "\e[1;31m[WRONG]\e[0m" ;;
    RUNTIME_ERROR) echo -en "\e[1;33m[ERROR]\e[0m" ;;
  esac

  # Handle exit code
  case $STATUS in
    SUCCESS | CHECK) ;;
    *) EXIT_STATUS=1 ;;
  esac

  local INPUT_LABEL="$INPUT"
  if (( $IS_STRING == 1 )); then
    INPUT_LABEL="'$INPUT_STR'"
  fi
  align_first "$MAX_INPUT_LEN" "$INPUT_LABEL" "${@:2}"

  if (( MULTILINE == 0 )) && [[ $STATUS != RUNTIME_ERROR ]]; then
    local EXPECTED=$(< "$EXPECTED_FILE")
    local OUTPUT=$(< "$OUTPUT_FILE")

    if [[ $EXPECTED =~ [[:space:]] ]] || [[ $OUTPUT =~ [[:space:]] ]]; then
      OUTPUT="'$OUTPUT'"
      EXPECTED="'$EXPECTED'"
    fi
  fi

  case $STATUS in
    SUCCESS)
      if (( $MULTILINE == 1 )); then
        printf '\n'
        cat "$OUTPUT_FILE"
        printf '\n'
      else
        printf ' -> \e[32m%s\n\e[0m' "$OUTPUT"
      fi
      ;;
    CHECK)
      if (( $MULTILINE == 1 )); then
        printf '\n'
        cat "$OUTPUT_FILE"
        printf '\n'
      else
        printf ' -> \e[1m%s\n\e[0m' "$OUTPUT"
      fi
      ;;
    WRONG)
      if (( $MULTILINE == 1 )); then
        if type delta &> /dev/null; then
          diff -u "$OUTPUT_FILE" "$EXPECTED_FILE" \
            | grep -vF '\ No newline at end of file' \
            | DELTA_PAGER=cat delta --file-style=omit
        else
          diff "$OUTPUT_FILE" "$EXPECTED_FILE"
        fi
      else
        printf ' -> \e[1;31m%s\e[0m != \e[1;32m%s\e[0m\n' "$OUTPUT" "$EXPECTED"
      fi
      ;;
    RUNTIME_ERROR)
      printf '\n'
      cat "$OUTPUT_FILE"
      printf '\n'
      ;;
  esac
}

EXIT_STATUS=0

cd "$PROGRAM_DIR"
. spec.sh

exit "$EXIT_STATUS"


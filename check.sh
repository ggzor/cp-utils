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

# Get some useful params
MAX_INPUT_LEN=0
check() {
  local INPUT="$PROGRAM_DIR/$1"

  if (( ${#INPUT} > $MAX_INPUT_LEN )); then
    MAX_INPUT_LEN=${#INPUT}
  fi
}

. "$SPEC_FILE"

check() {
  set +e

  local INPUT="$PROGRAM_DIR/$1"
  local EXPECTED=$(cat)
  local OUTPUT
  OUTPUT=$(cd "$OLD_DIR" && cp-utils/run.sh "$PROGRAM" "${@:2}" < "$INPUT" 2>&1)
  CMD_STATUS=$?

  if (( $CMD_STATUS == 0 )); then
    if (( $JUST_CHECK == 1 )); then
      STATUS=CHECK
    else
      if diff <(cat <<< "$EXPECTED") <(cat <<< "$OUTPUT") &>/dev/null; then
        STATUS=SUCCESS
      else
        STATUS=WRONG
      fi
    fi
  else
    STATUS=RUNTIME_ERROR
  fi

  local MULTILINE=0
  if [[ $EXPECTED == *$'\n'+(?) ]] || [[ $OUTPUT == *$'\n'+(?) ]]; then
    MULTILINE=1
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

  printf " \e[2m%-${MAX_INPUT_LEN}s\e[0m" "$INPUT"
  printf '\e[2m%s\e[0m' " ${@:2}"

  if (( MULTILINE == 0 )) && [[ $STATUS != RUNTIME_ERROR ]]; then
    if [[ $EXPECTED =~ [[:space:]] ]] || [[ $OUTPUT =~ [[:space:]] ]]; then
      OUTPUT="'$OUTPUT'"
      EXPECTED="'$EXPECTED'"
    fi
  fi

  case $STATUS in
    SUCCESS)
      if (( $MULTILINE == 1 )); then
        printf '\n%s\n' "$OUTPUT"
      else
        printf ' -> \e[32m%s\n\e[0m' "$OUTPUT"
      fi
      ;;
    CHECK)
      if (( $MULTILINE == 1 )); then
        printf '\n%s\n' "$OUTPUT"
      else
        printf ' -> \e[1m%s\n\e[0m' "$OUTPUT"
      fi
      ;;
    WRONG)
      if (( $MULTILINE == 1 )); then
        local TMP=$(mktemp -d)
        local EXPECTED_FILE="$TMP/expected"
        local OUTPUT_FILE="$TMP/current"

        printf '%s' "$EXPECTED" > "$EXPECTED_FILE"
        printf '%s' "$OUTPUT" > "$OUTPUT_FILE"

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
      printf '\n%s\n' "$OUTPUT"
      ;;
  esac

  set -e
}

EXIT_STATUS=0

cd "$PROGRAM_DIR"
. spec.sh

exit "$EXIT_STATUS"


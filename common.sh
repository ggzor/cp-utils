#!/usr/bin/env bash

shopt -s extglob

# echo to stderr
echoerr() {
  echo -e "\e[1;31mError: \e[0m$@" 1>&2
}

exit_error() {
  echoerr "$@"
  exit 1
}


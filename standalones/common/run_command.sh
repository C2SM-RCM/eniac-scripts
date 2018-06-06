#!/bin/bash -e 

function run_command {
  "$@"
  local status=$?
  if [ $status -ne 0 ]; then
    echo "error with $1 (l.${BASH_LINENO})" >&2
    exit 1
  fi
  return $status
}

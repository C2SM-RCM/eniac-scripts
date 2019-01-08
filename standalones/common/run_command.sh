#!/bin/bash -e 

function run_command {
  nice "$@"
  local status=$?
  if [ $status -ne 0 ]; then
    executing_script="$(basename "$(dirname $0)")/"
    executing_script+=$(basename $0)
    echo "error with $1 in ${executing_script} (l.${BASH_LINENO})" >&2
    exit 1
  fi
  return $status
}

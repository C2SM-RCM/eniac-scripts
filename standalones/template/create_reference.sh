#!/bin/bash -e

if [[ $# -ne 3 ]]; then
  echo "USAGE: script requires 3 arguments"
  echo "       create_reference.sh slave compiler target"
  exit 1
fi

# Script arguments
slave="$1"
compiler="$2"
target="$3"
datadir="ERROR_DATA_SHOULD_NOT_BE_NEEDED"

# Environment setup
testroutine="++TESTROUTINE++"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh

# Create reference data for standalone tests
run_command ${commondir}/create_reference.sh || exit 1

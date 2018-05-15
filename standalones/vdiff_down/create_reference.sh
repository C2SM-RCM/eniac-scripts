#!/bin/bash -e

if [[ $# -ne 4 ]]; then
  echo "USAGE: script requires 4 arguments"
  echo "       create_reference.sh slave compiler target /path/to/data/root"
  exit 1
fi

# Script arguments
slave="$1"
compiler="$2"
target="$3"
datadir="$4"

# Environment setup
testroutine="vdiff_down"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh

# Create reference data for standalone tests
run_command ${commondir}/create_reference.sh || exit 1

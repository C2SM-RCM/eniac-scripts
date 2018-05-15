#!/bin/bash -e

if [[ $# -ne 2 ]]; then
  echo "USAGE: script requires 2 arguments"
  echo "       extract_standalone.sh slave /path/to/data/root"
  exit 1
fi

# Script arguments
slave="$1"
compiler="gcc"
target="cpu"
datadir="$4"

# Environment setup
testroutine="vdiff_down"
testmodule="mo_vdiff_downward_sweep"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh
export testmodule

# Run standalone tests
run_command ${commondir}/extract_standalone.sh || exit 1

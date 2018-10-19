#!/bin/bash -e

if [[ $# -ne 1 ]]; then
  echo "USAGE: script requires 1 argument"
  echo "       extract_standalone.sh slave"
  exit 1
fi

# Script arguments
slave="$1"
compiler="gcc"
target="cpu"

# Environment setup
testroutine="++TESTROUTINE++"
testmodule="++TESTMODULE++"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh
export testmodule

# Run standalone tests
run_command ${commondir}/extract_standalone.sh || exit 1

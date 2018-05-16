#!/bin/bash -e

if [[ $# -lt 1 ]]; then
  echo "USAGE: script requires 1 argument and 1 optional"
  echo "       extract_standalone.sh slave [revert_noacc]"
  exit 1
fi

# Script arguments
slave="$1"
compiler="gcc"
target="cpu"
datadir="ERROR_DATA_SHOULD_NOT_BE_NEEDED"

# Environment setup
testroutine="nsurf_diag"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"
standalonedir="standalone/${testroutine}"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh
export standalonedir

# Run standalone tests
run_command ${commondir}/generate_patches.sh $2 || exit 1

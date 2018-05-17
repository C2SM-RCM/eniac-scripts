#!/bin/bash -e

if [[ $# -lt 2 ]]; then
  echo "USAGE: script requires 2 argument and 1 optional"
  echo "       extract_standalone.sh slave {standalone,extract} [revert_noacc]|{init,intermezzo,finish}"
  exit 1
fi

# Script arguments
slave="$1"
compiler="gcc"
target="cpu"
datadir="ERROR_DATA_SHOULD_NOT_BE_NEEDED"

# Environment setup
testroutine="vdiff_up"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"
standalonedir="standalone/${testroutine}"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh
export standalonedir

# Run standalone tests
if [ "$2" == "standalone" ];then
  run_command ${commondir}/generate_patches_standalone.sh ${@:3} || exit 1
elif [ "$2" == "extract" ];then
  run_command ${commondir}/generate_patches_extract.sh ${@:3} || exit 1
else
  run_command echo "Unknown patch type" || exit 1
  exit 1
fi

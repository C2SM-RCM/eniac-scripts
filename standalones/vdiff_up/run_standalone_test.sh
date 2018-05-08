#!/bin/bash -e

# Script arguments
slave="$1"
compiler="$2"
target="$3"
datadir="$4"

# Environment setup
testroutine="vdiff_up"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh

# Run standalone tests
run_command ${commondir}/run_standalone_test.sh || exit 1

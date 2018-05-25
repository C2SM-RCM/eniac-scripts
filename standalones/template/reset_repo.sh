#!/bin/bash -e

# Environment setup
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"
source ${commondir}/run_command.sh

# Remove standalone test source
rm -f "src/tests/ftg_++TESTROUTINE++_test.f90"

# More standalone specific cleanup
#

# General cleanup of repo
run_command ${commondir}/reset_repo.sh || exit 1

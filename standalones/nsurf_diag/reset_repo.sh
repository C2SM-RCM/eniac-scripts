#!/bin/bash -e

# Environment setup
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"
source ${commondir}/run_command.sh

# standalone specific cleanup

# Remove standalone test source
rm -f "src/tests/ftg_nsurf_diag_test.f90"

# General cleanup of repo
run_command ${commondir}/reset_repo.sh || exit 1

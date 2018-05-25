#!/bin/bash -e

# Environment setup
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"
source ${commondir}/run_command.sh

# standalone specific cleanup

# Remove standalone test source
rm -f "src/tests/ftg_update_surface_test.f90"

# reset JSBACH repository
cd externals/jsbach
git reset --hard HEAD
cd ../..

# General cleanup of repo
run_command ${commondir}/reset_repo.sh || exit 1

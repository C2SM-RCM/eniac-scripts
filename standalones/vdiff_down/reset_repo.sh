#!/bin/bash -e

# clean build
make distclean

# reset ICON repository
git reset --hard HEAD

# Remove log and output files
rm -f standalone_configure.log
rm -f standalone_build.log
rm -f standalone_deps.txt
rm -f standalone_deps.out
rm -f validation.result
rm -f validation.failed
rm -rf experiments/atm_amip_test

# Remove reference submission script
rm -f submit_reference.sh

# Remove standalone test source
rm src/tests/ftg_vdiff_down_test.f90

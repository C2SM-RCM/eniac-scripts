#!/bin/bash -e

# clean build
make distclean >& /dev/null

# reset ICON repository
git reset --hard HEAD

# Remove log and output files
rm -f standalone_configure.log
rm -f standalone_build.log
rm -f standalone_configure_c.log
rm -f standalone_build_c.log
rm -f standalone_configure_r.log
rm -f standalone_build_r.log
rm -f standalone_deps.txt
rm -f standalone_deps.out
rm -f validation.result
rm -f validation.failed
rm -rf experiments/atm_amip_test
rm -rf fortrancallgraph/
rm -rf fortrantestgenerator/

# Remove reference submission scripts
rm -f run/dict.atm_amip_test
rm -f submit_reference.sh

# Remove standalone test source
rm src/tests/ftg_nsurf_diag_test.f90

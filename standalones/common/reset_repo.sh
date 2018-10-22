#!/bin/bash -e

# clean build
make distclean >& /dev/null || true

# reset ICON repository
git reset --hard HEAD

# Remove log and output files
rm -f standalone_configure.log
rm -f standalone_build.log
rm -f standalone_configure_c.log
rm -f standalone_build_c.log
rm -f standalone_ftg_c.log
rm -f standalone_configure_r.log
rm -f standalone_build_r.log
rm -f standalone_ftg_r.log
rm -f standalone_deps.txt
rm -f standalone_deps.out
rm -f standalone_ftg_b.log
rm -f standalone_make_runscripts.log
rm -f validation.result
rm -f validation.failed
rm -rf experiments/atm_amip_test
rm -rf fortrancallgraph/
rm -rf fortrantestgenerator/
rm -rf eniac_a
rm -rf eniac_b
rm -rf tmp_experiments

# Remove reference submission script
rm -f run/dict.atm_amip_test
rm -f submit_reference.sh

# Remove failed patches .orig and .rej files
for f in $(find . -type f -name '*.orig');do
  rm $f;
done
for f in $(find . -type f -name '*.rej');do
  rm $f;
done

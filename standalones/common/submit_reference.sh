#!/bin/bash -e

cd run/
opts=" --wait --time=00:05:00 "
if [ "${slave}" = "daint" ]; then
  sbatch --input=none --partition=normal --account=g110 --constraint=gpu ${opts} exp.atm_amip_test.run
else
  sbatch --input=none --partition=debug ${opts} exp.atm_amip_test.run
fi

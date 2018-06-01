#!/bin/bash -e

cd run/
opts=" --input=none --wait --time=00:05:00 "
if [ "${SBATCH_ACCOUNT}" ]; then
  opts+=" --account=${SBATCH_ACCOUNT} "
fi
if [ "${SBATCH_PARTITION}" ]; then
  opts+=" --partition=${SBATCH_PARTITION} "
fi
if [ "${slave}" = "daint" ]; then
  opts+=" --constraint=gpu"
fi
sbatch ${opts} exp.atm_amip_test.run

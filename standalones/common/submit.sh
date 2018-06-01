#!/bin/bash -e

if [ "${slave}" = "kesch" ]; then
  eval "$(grep 'load_modules=' ${workdir}/build_command | sed -e 's/load_modules=/module load /')"
fi
opts=" --input=none --time=00:05:00 --gres=gpu:1 --output=${testname}.log "
if [ "${SBATCH_ACCOUNT}" ]; then
  opts+=" --account=${SBATCH_ACCOUNT} "
fi
if [ "${SBATCH_PARTITION}" ]; then
  opts+=" --partition=${SBATCH_PARTITION} "
fi
if [ "${slave}" = "daint" ]; then
  opts+=" --constraint=gpu"
fi
srun ${opts} ./${testname} || exit 0

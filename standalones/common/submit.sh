#!/bin/bash -e

if [ "${slave}" = "kesch" ]; then
  eval "$(grep 'load_modules=' ${workdir}/build_command | sed -e 's/load_modules=/module load /')"
fi
opts=" --time=00:05:00 --gres=gpu:1 --output=${testname}.log "
if [ "${slave}" = "daint" ]; then
  srun --input=none --partition=normal --account=g110 --constraint=gpu ${opts} ./${testname}
else
  srun --input=none --partition=debug ${opts} ./${testname}
fi

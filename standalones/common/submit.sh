#!/bin/bash -e

if [ "${slave}" = "kesch" ]; then
  eval "$(grep 'load_modules=' ${workdir}/build_command | sed -e 's/load_modules=/module load /')"
elif [ "${slave}" = "daint" ]; then
  mods="$(grep 'load_modules=' ${workdir}/build_command | sed -e 's/load_modules=//')"
  cur_prgenv="$(module list |& grep PrgEnv- | awk '{print $2;}')"
  new_prgenv="$(echo $mods | tr ' ' '\n' | grep PrgEnv-)"
  only_mods="$(echo $mods | sed -e 's/ PrgEnv-[a-zA-Z]* / /')"
  module swap "${cur_prgenv}" "${new_prgenv}"
  module load "${only_mods}"
fi
opts=" --input=none --time=00:05:00 --gres=gpu:1 --output=${testname}_nproma_${nproma}.log "
if [ "${SBATCH_ACCOUNT}" ]; then
  opts+=" --account=${SBATCH_ACCOUNT} "
fi
if [ "${SBATCH_PARTITION}" ]; then
  opts+=" --partition=${SBATCH_PARTITION} "
fi
if [ "${slave}" = "daint" ]; then
  opts+=" --constraint=gpu"
fi
set -x
srun ${opts} ./${testname} || exit 0
set +x

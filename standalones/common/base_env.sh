#!/bin/bash -e

testname="ftg_${testroutine}_test"
ftginputdir="${SCRATCH}/data/icon-eniac/sa_${testroutine}/ftg/${compiler}/${slave}"
expdir="${workdir}/experiments/atm_amip_test"
ftgoutputdir="${expdir}/ftg"
dataftgdir="${datadir}/ftg/${compiler}/${slave}"
fdepdir=/project/c14/data-eniac/standalones/fdependencies
scriptdir="${workdir}/eniac-scripts/standalones/${testroutine}"

# Check if global install of Cheetah3 is required
need_cheetah="$(python -c 'import imp;imp.find_module("Cheetah")' >& /dev/null;echo $?)"
if [ "${need_cheetah}" -eq "1" ];then
  CHEETAHTYPE="GLOBAL"
  if [ "${slave}" == "kesch" ];then
    CHEETAHPATH=/project/c14/install/kesch/cheetah3-3.1.0
  elif [ "${slave}" == "daint" ];then
    CHEETAHPATH=/apps/daint/UES/6.0.UP04/sandboxes/wsawyer/PYTHON/packages/lib/python2.7/site-packages/
  fi
  export PYTHONPATH=${PYTHONPATH}${PYTHONPATH+":"}${CHEETAHPATH}
else
  CHEETAHTYPE="LOCAL"
fi
CHEETAHVERSION="$(python -c 'import Cheetah;print(Cheetah.Version)')"
echo "---------------------------------------------------------------------"
echo "Using ${CHEETAHTYPE} install of Cheetah3 v${CHEETAHVERSION}"
echo ""

export slave
export compiler
export target
export datadir
export workdir
export testroutine
export testname
export scriptdir
export commondir
export expdir
export ftginputdir
export ftgoutputdir
export dataftgdir
export fdepdir

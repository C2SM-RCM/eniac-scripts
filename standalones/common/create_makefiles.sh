#!/bin/bash -e

if [[ $# -ne 2 ]]; then
  echo "USAGE: script requires 2 arguments"
  echo "       create_makefiles.sh slave compiler"
  echo "       create_makefiles.sh slave all"
  exit 1
fi

# Script arguments
slave="$1"
compiler="$2"

# Environment setup
testroutine="notneeded"
workdir="$(pwd)"
commondir="${workdir}/eniac-scripts/standalones/common"

# Export slave, compiler, target, datadir, workdir, testroutine, testname, ftginputdir, ftgoutputdir and dataftgdir
source ${commondir}/base_env.sh
source ${commondir}/run_command.sh

if [ "${compiler}" == "all" ]; then
  compiler_list="gcc cray pgi cray_gpu pgi_gpu"
else
  compiler_list="${compiler}"
fi

# Generate Makefile(s)
for c in ${compiler_list}; do
  run_command echo "Generating Makefile for ${slave} with ${c}..." || exit 1
  run_command ./configure --with-fortran=${c} >& standalone_configure.log || exit 1
  run_command cp Makefile Makefile.${slave}.${c} || exit 1
  run_command cp build_command build_command.${slave}.${c} || exit 1
  run_command make distclean >& /dev/null || exit 1
done

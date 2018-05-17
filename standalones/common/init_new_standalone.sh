#!/bin/bash -e

if [[ $# -ne 4 ]]; then
  echo "USAGE: script requires 4 arguments"
  echo "       init_new_standalone.sh routine module module_dir new_standalone_dir"
  exit 1
fi

testroutine=$1
testmodule=$2
testmoduledir=$3
newdir=$4

if [ -d ${newdir} ]; then
  echo "###########################################" || exit 1
  echo "## Destination directory already exists! ##" || exit 1
  echo "###########################################" || exit 1
  exit 1
else
  mkdir -p ${newdir}
fi

templatedir=$(realpath ${newdir})/../template
if [ ! -d ${templatedir} ]; then
  echo "###################################" || exit 1
  echo "## Template directory not found! ##" || exit 1
  echo "###################################" || exit 1
  exit 1
else
  cp -r ${templatedir}/* ${newdir}
  sed -i -e "s|++TESTROUTINE++|${testroutine}|g" -e "s|++TESTMODULE++|${testmodule}|g" -e "s|++TESTMODULEDIR++|${testmoduledir}|g" ${newdir}/*
fi

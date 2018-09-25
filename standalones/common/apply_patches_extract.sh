#!/bin/bash -e

if [[ $# -ne 1 ]]; then
  echo "USAGE: script requires 1 argument"
  echo "       apply_patches_extract.sh {init,intermezzo,finish}"
  exit 1
fi

timing=$1

specific_patches ()
{
  # Extraction patches
  if [ -d ${patchdir} ]; then
    shopt -s nullglob
    for f in ${patchdir}/*.patch; do
      patch -p1 <${f}
    done
    shopt -u nullglob

    # Extraction patches for JSBACH
    if [ -d ${patchdir}/jsbach ]; then
      shopt -s nullglob
      for f in ${patchdir}/jsbach/*.patch; do
        patch -p1 <$f
      done
      shopt -u nullglob
    fi
  fi
}

if [ "${timing}" == "init" ]; then

  patchdir=${scriptdir}/patches/extract/${timing}
  specific_patches
  exit 0

elif [ "${timing}" == "intermezzo" ]; then

  patchdir=${scriptdir}/patches/extract/${timing}
  specific_patches
  exit 0

elif [ "${timing}" == "finish" ]; then

  patchdir=${scriptdir}/patches/extract/${timing}
  specific_patches
  exit 0

else

  echo "Unknown timing: ${timing}"
  exit 1

fi

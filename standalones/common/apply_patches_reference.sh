#!/bin/bash -e

compiler_list="gcc pgi cray"

specific_patches ()
{
  if [ "$1" ]; then
    patchcmd="-d $1"
  else
    patchcmd=""
  fi

  if [ -d ${patchdir} ]; then
    for f in ${patchdir}/*.patch; do
      patch ${patchcmd} -p1 <$f
    done

    # Compiler specific patches
    for c in ${compiler_list}; do
      if [ "${compiler}" = "$c" ] && [ -d ${patchdir}/$c ]; then
        for f in ${patchdir}/$c/*.patch; do
          patch ${patchcmd} -p1 <$f
        done
      fi
    done
  fi
}

# Common patches
if [ -f ${commondir}/patches/${expname}_${slave}.patch ]; then
  patch -p1 < ${commondir}/patches/${expname}_${slave}.patch
fi

# run specific patches
patchdir=${scriptdir}/patches/reference
specific_patches

# JSBACH specific patches
patchdir=${patchdir}/jsbach
specific_patches externals/jsbach

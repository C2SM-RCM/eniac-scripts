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
patch -p1 <${commondir}/patches/configure.patch
patch -p1 <${commondir}/patches/mh-linux_ftg.patch

# run specific patches
patchdir=${scriptdir}/patches/run
specific_patches

# JSBACH specific patches
patchdir=${patchdir}/jsbach
specific_patches externals/jsbach

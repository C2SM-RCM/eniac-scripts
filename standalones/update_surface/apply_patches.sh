#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/configure.patch
patch -p1 <${commondir}/patches/mh-linux_ftg.patch

# update_surface specific patches
patch -p1 < ${scriptdir}/patches/mo_surface_ftg.patch
patch -p1 < ${scriptdir}/patches/make_public_ftg.patch

# JSBACH specific patches
patch -d externals/jsbach -p1 <${scriptdir}/patches/jsbach_ftg.patch
if [ "${compiler}" = "pgi" ]; then
  patch -d externals/jsbach -p1 <${datadir}/patches/pgi_jsbach_generic_soil.patch
fi

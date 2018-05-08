#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/mh-linux_ftg.patch
if [ ${slave} = "kesch" ]; then
  patch -p1 < ${commondir}/patches/atm_amip_test_kesch.patch
fi

# vdiff_up specific patches
patch -p1 < ${scriptdir}/patches/mo_vdiff_downward_sweep_ftg.patch
patch -p1 < ${scriptdir}/patches/make_public_ftg.patch

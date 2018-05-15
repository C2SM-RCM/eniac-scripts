#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/mh-linux_ftg.patch
if [ ${slave} = "kesch" ]; then
  patch -p1 < ${commondir}/patches/atm_amip_test_kesch.patch
fi

# velocity_tendencies specific patches
patch -p1 < ${scriptdir}/patches/mo_velocity_advection_ftg.patch

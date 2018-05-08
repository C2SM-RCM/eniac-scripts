#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/configure.patch
patch -p1 <${commondir}/patches/mh-linux_ftg.patch

# vdiff_down specific patches
patch -p1 < ${scriptdir}/patches/mo_vdiff_downward_sweep_ftg.patch

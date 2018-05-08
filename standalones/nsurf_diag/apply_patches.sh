#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/configure.patch
patch -p1 <${commondir}/patches/mh-linux_ftg.patch

# nsurf_diag specific patches
patch -p1 < ${scriptdir}/patches/mo_surface_diag_ftg.patch

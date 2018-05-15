#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/mh-linux_extract_ftg.patch

# Force serialization patches
patch -p1 <${scriptdir}/patches/mo_surface_force_ftg.patch
patch -p1 <${scriptdir}/patches/make_public_force_ftg.patch
patch -d externals/jsbach -p1 <${scriptdir}/patches/jsbach_extract_ftg.patch

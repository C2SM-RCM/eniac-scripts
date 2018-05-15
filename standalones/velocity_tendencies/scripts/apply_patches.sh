#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/configure.patch
patch -p1 <${commondir}/patches/mh-linux_ftg.patch

# velocity_tendencies specific patches
patch -p1 < ${scriptdir}/patches/mo_velocity_advection_ftg.patch

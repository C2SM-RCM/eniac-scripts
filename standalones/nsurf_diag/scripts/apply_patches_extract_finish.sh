#!/bin/bash -e

# Common patches
patch -p1 <${scriptdir}/patches/mo_surface_diag_endif_ftg.patch

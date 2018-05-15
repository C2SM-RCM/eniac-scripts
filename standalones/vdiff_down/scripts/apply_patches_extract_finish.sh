#!/bin/bash -e

# vdiff_down FTG finish patches
patch -p1 <${scriptdir}/patches/ftg_vdiff_down_test_finish_ftg.patch

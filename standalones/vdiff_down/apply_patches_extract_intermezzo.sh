#!/bin/bash -e

# vdiff_down FTG intermezzo patches
patch -p0 <${scriptdir}/patches/mo_convect_tables_intermezzo_ftg.patch
patch -p0 <${scriptdir}/patches/mo_vdiff_downward_sweep_intermezzo_ftg.patch

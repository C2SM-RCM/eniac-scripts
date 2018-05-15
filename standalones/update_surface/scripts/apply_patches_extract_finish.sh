#!/bin/bash -e

# Force serialization patches
patch -p1 <${scriptdir}/patches/ftg_test_force.patch

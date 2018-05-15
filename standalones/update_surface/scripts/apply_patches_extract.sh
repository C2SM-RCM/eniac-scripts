#!/bin/bash -e

# Common patches
patch -p1 <${commondir}/patches/mh-linux_extract_ftg.patch

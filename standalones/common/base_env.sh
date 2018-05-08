#!/bin/bash -e

testname="ftg_${testroutine}_test"
ftginputdir="${SCRATCH}/data/icon-eniac/sa_${testroutine}/ftg/${compiler}/${slave}"
ftgoutputdir="${workdir}/ftg"
dataftgdir="${datadir}/ftg/${compiler}/${slave}"
fdepdir=/project/c14/data-eniac/standalones/fdependencies
scriptdir="${workdir}/eniac-scripts/${testroutine}"

export slave
export compiler
export target
export datadir
export workdir
export testroutine
export testname
export scriptdir
export commondir
export ftginputdir
export ftgoutputdir
export dataftgdir
export fdepdir

#!/bin/bash -e 

source ${commondir}/run_command.sh

# Get build directory
builddir="$(grep 'exec_prefix = .*$' Makefile | sed -e 's|exec_prefix = ||g')"

# Extract standaone dependencies
run_command ln -s ${workdir}/${builddir}/src ${workdir}/src/dsl4jsb || exit 1
run_command rm ${workdir}/src/lnd_phy_jsbach || exit 1
run_command python ${fdepdir}/generate_dep.py --recursive src/ tests/${testname}.f90 > standalone_deps.txt 2> standalone_deps.out || exit 1
run_command rm ${workdir}/src/dsl4jsb || exit 1
run_command ln -s ${workdir}/externals/jsbach/src ${workdir}/src/lnd_phy_jsbach || exit 1
ftgdeps=".DEFAULT_GOAL := standalone\nFTGOBJS ="
while read -r f;do 
  if [ "$f" != "tests/${testname}.f90" ];then
    ftgdeps+=" $(basename $f .f90).o"; 
  else
    break
  fi
done <standalone_deps.txt
# Manually add missed dependencies
# ftgdeps+=" something.f90"

# Patch Makefile to only compile standalone executable
run_command printf "${ftgdeps}\n\n" | cat - ${builddir}/src/Makefile > Makefile.tmp || exit 1
run_command mv Makefile.tmp ${builddir}/src/Makefile || exit 1
run_command sed -i -e "s|../bin/${testname}:|../bin/${testname}_test_icon:|" ${builddir}/src/Makefile || exit 1
run_command sed -e "s|++TESTNAME++|${testname}|g"  ${commondir}/Makefile.standalone.template >> ${builddir}/src/Makefile || exit 1

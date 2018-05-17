#!/bin/bash -e

source ${commondir}/run_command.sh

# This only works with GCC on CPU
if [[ !("${compiler}" == "gcc" && "${target}" == "cpu") ]]; then
  echo "Extraction of standalone only works with GCC on CPU"
  exit 1
fi

# Apply patches for standalone extraction
run_command ${commondir}/apply_patches_extract.sh init || exit 1

# Clone and configure FCG and FTG
run_command git clone https://github.com/fortesg/fortrancallgraph || exit 1
run_command sed -e "s|++ICONDIR++|${workdir}|g" ${commondir}/ftg/config_fortrancallgraph.py.tmpl > fortrancallgraph/config_fortrancallgraph.py || exit 1
run_command git clone https://github.com/fortesg/fortrantestgenerator || exit 1
run_command sed -e "s|++ICONDIR++|${workdir}|g" ${commondir}/ftg/config_fortrantestgenerator.py.tmpl > fortrantestgenerator/config_fortrantestgenerator.py || exit 1
run_command cp -r ${commondir}/ftg/icon_standalone_eniac fortrantestgenerator/templates || exit 1

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran=gcc >& standalone_configure_c.log || exit 1

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build_c.log || exit 1

# Generate capture code
run_command cd fortrantestgenerator/
run_command ./FortranTestGenerator.py -c ${testmodule} ${testroutine}
run_command cd ../

# Apply intermezzo patches
run_command ${commondir}/apply_patches_extract.sh intermezzo || exit 1

# Cleanup
run_command make distclean >& /dev/null || exit 1

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran=gcc >& standalone_configure_r.log || exit 1

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build_r.log || exit 1

# Generate replay code
run_command cd fortrantestgenerator/
run_command ./FortranTestGenerator.py -r ${testmodule} ${testroutine}
run_command cd ../

# Apply finish patches
run_command ${commondir}/apply_patches_extract.sh finish || exit 1

# Isolate standalone
export standalonedir="standalone/${testroutine}"
run_command mkdir -p ${standalonedir} || exit 1

# Cleanup
run_command make distclean >& /dev/null || exit 1

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran=gcc >& standalone_configure.log || exit 1
# Tune configuration for standalone
${commondir}/tune_configuration.sh

# Get build directory
builddir="$(grep 'exec_prefix = .*$' Makefile | sed -e 's|exec_prefix = ||g')"

# Extract standalone dependencies
run_command ln -s ${workdir}/${builddir}/src ${workdir}/src/dsl4jsb || exit 1
run_command rm ${workdir}/src/lnd_phy_jsbach || exit 1
run_command python ${fdepdir}/generate_dep.py --recursive src/ tests/${testname}.f90 > standalone_deps.txt 2> standalone_deps.out || exit 1
run_command rm ${workdir}/src/dsl4jsb || exit 1
run_command ln -s ${workdir}/externals/jsbach/src ${workdir}/src/lnd_phy_jsbach || exit 1
ftgdeps=""
while read -r f;do 
  if [ "$f" != "tests/${testname}.f90" ]; then
    if [[ "$f" != *_dsl4jsb.f90 ]]; then
      ftgdeps+=" src/$f"; 
    fi
  else
    break
  fi
done <standalone_deps.txt
# Manually add extra dependencies
if [ -e "${scriptdir}/extra_dependencies.txt" ]; then
  while read -r f; do
    ftgdeps+=" src/$f"
  done <${scriptdir}/extra_dependencies.txt
fi

# Copy into standalone directory
run_command cp --parents ${ftgdeps} ${standalonedir} || exit 1
run_command cp --parents src/tests/${testname}.f90 ${standalonedir} || exit 1

# Manually add external dependencies
run_command cp -r --parents src/include support/ config/ Makefile.in ${standalonedir} || exit 1
new_ac_unique="ac_unique_file=\"src/tests/${testname}.f90\""
run_command sed -e "s|ac_unique_file=.*$|${new_ac_unique}|" configure > ${standalonedir}/configure
for lib in mtime self tixi yac; do
  if [ "$(grep -c ${lib} ${scriptdir}/external_dependencies.txt)" == "0" ]; then
    run_command sed -i -e "s|-l${lib}||g" ${standalonedir}/configure || exit 1
    run_command sed -i -e "s|externals/${lib}/src||g" ${standalonedir}/configure || exit 1
  fi
done
if [ "$(grep -c yac ${scriptdir}/external_dependencies.txt)" == "0" ]; then
  run_command sed -i -e 's|use_yac="yes"|use_yac="no"|g' ${standalonedir}/configure || exit 1
fi
if [ "$(grep -c jsbach ${scriptdir}/external_dependencies.txt)" == "0" ]; then
  run_command sed -i -e 's|enable_jsbach=yes|enable_jsbach=no|g' ${standalonedir}/configure || exit 1
fi
if [ -e "${scriptdir}/external_dependencies.txt" ]; then
  while read -r f; do
    run_command cp -r --parents $f ${standalonedir} || exit 1
  done <${scriptdir}/external_dependencies.txt
fi

# Revert FTG changes
run_command cd fortrantestgenerator/ || exit 1
run_command ./FortranTestGenerator.py -b ${testmodule} ${testroutine} || exit 1
run_command cd ../ || exit 1

# Reset all changes
run_command ${scriptdir}/reset_repo.sh || exit 1

# Create patches
run_command ${commondir}/generate_patches.sh || exit 1

# Remove existing ACC statements unrelated to standalone
run_command cd ${standalonedir} || exit 1
if [ -f ${scriptdir}/noacc_list.txt ]; then
  while read -r f;do
    if [ -f $f ]; then
      run_command sed -i -e 's/!$ACC/!NOENIAC/g' -e 's/!$acc/!noeniac/g' "$f" || exit 1
    fi
  done <${scriptdir}/noacc_list.txt
fi
run_command cd ${workdir} || exit 1

#!/bin/bash -e

source ${commondir}/run_command.sh

# This only works with GCC on CPU
if [[ !("${compiler}" == "gcc" && "${target}" == "cpu") ]]; then
  echo "Extraction of standalone only works with GCC on CPU"
  exit 1
fi

# Apply patches for standalone extraction
run_command ${scriptdir}/scripts/apply_patches_extract.sh || exit 1

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
if [ -f ${scriptdir}/scripts/apply_patches_extract_intermezzo.sh ]; then
  run_command ${scriptdir}/scripts/apply_patches_extract_intermezzo.sh || exit 1
fi

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
if [ -f ${scriptdir}/scripts/apply_patches_extract_finish.sh ]; then
  run_command ${scriptdir}/scripts/apply_patches_extract_finish.sh || exit 1
fi

# Isolate standalone
standalonedir="standalone/${testroutine}"
run_command mkdir -p ${standalonedir} || exit 1

# Cleanup
run_command make distclean >& /dev/null || exit 1

# Remove existing ACC statements unrelated to standalone
while read -r f;do 
  run_command sed -i -e 's/!$ACC/!NOACC/g' -e 's/!$acc/!noacc/g' "$f" || exit 1
done <${commondir}/noacc_list.txt

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
run_command cp -r --parents src/include support/ config/ Makefile.in configure configure.ac ${standalonedir} || exit 1
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
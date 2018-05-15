#!/bin/bash -e

source ${commondir}/run_command.sh

# Build compiler name for configure script by extending with target (except CPU)
compiler_target="${compiler}"
if [ "${target}" = "gpu" ]; then
  compiler_target+="_${target}"
fi

# Apply patches for standalone
run_command ${scriptdir}/scripts/apply_patches_reference.sh || exit 1

# Copy standalone files and tune for current install
run_command cp -r ${scriptdir}/src . || exit 1
while read -r f;do
  run_command sed -i -e "s|++FTGDATADIR++|${ftgoutputdir}|g" "$f" || exit 1
done <${scriptdir}/ftgdatadir_files.txt

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran="${compiler_target}" >& standalone_configure.log || exit 1

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build.log || exit 1

# Make runscript
run_command ${workdir}/make_runscripts || exit 1
run_command sed -i -e "s/--ntasks-per-node=[0-9]*$/--ntasks-per-node=1/g" -e "s/mpi_procs_pernode=[0-9]*$/mpi_procs_pernode=1/g" run/exp.atm_amip_test.run || exit 1
run_command cp ${commondir}/submit_reference.sh ${workdir}/ || exit 1

# Run serialization
for nproma in 16 2729 20480; do

  # Prepare output directories
  run_command mkdir -p "${ftgoutputdir}/nproma_${nproma}" || exit 1
  run_command ln -s "${ftgoutputdir}/nproma_${nproma}" "${ftgoutputdir}/data" || exit 1

  # Run executable
  run_command echo "Running AMIP test with serialization ..."
  run_command sed -i -e "s/nproma=[0-9]*$/nproma=${nproma}/g" run/exp.atm_amip_test.run || exit 1
  run_command ./submit_reference.sh || exit 1

  run_command rm "${ftgoutputdir}/data" || exit 1

done

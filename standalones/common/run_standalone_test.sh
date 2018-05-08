#!/bin/bash -e

source ${commondir}/run_command.sh

# Build compiler name for configure script by extending with target (except CPU)
compiler_target="${compiler}"
if [ "${target}" = "gpu" ]; then
  compiler_target+="_${target}"
fi

# Apply patches for standalone
run_command ${scriptdir}/apply_patches.sh || exit 1

# Copy standalone files and tune for current install
run_command cp -r ${scriptdir}/src . || exit 1
while read -r f;do
  run_command sed -i -e "s|++FTGDATADIR++|${ftgoutputdir}|g" "$f" || exit 1
done <${scriptdir}/ftgdatadir_files.txt

# Remove existing ACC statements unrelated to standalone
while read -r f;do 
  run_command sed -i -e 's/!$ACC/!NOACC/g' -e 's/!$acc/!noacc/g' "$f" || exit 1
done <${commondir}/noacc_list.txt

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran="${compiler_target}" >& standalone_configure.log || exit 1
# Tune configuration for standalone
${commondir}/tune_configuration.sh

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build.log || exit 1

# Sync run data and prepare run
run_command rsync -aqv "${datadir}/experiments" ${workdir} || exit 1
run_command cp ${commondir}/submit.sh ${expdir} || exit 1
run_command cd ${expdir} || exit 1
run_command ln -sf ${workdir}/build/*/bin/${testname} . || exit 1

# Run tests
teststatus=0
tid=0
export nproma
for nproma in 16 2729 20480; do

  # Sync serialization data for FTG
  run_command mkdir -p "${ftginputdir}" || exit 1
  run_command mkdir -p "${ftgoutputdir}/nproma_${nproma}" || exit 1
  run_command rsync -aqv "${dataftgdir}/nproma_${nproma}" "${ftginputdir}/" || exit 1
  run_command ln -s "${ftgoutputdir}/nproma_${nproma}" "${ftgoutputdir}/data" || exit 1
  run_command ln -s "${ftginputdir}/nproma_${nproma}/input" "${ftgoutputdir}/data/input" || exit 1
  run_command ln -s "${ftginputdir}/nproma_${nproma}/output" "${ftgoutputdir}/data/output" || exit 1

  # Run executable
  run_command echo "Running standalone ..."
  if [ -e ${scriptdir}/tune_run.sh ]; then
    run_command ${scriptdir}/tun_run.sh || exit 1
  fi
  run_command ./submit.sh || exit 1

  # Validate run
  run_command ${commondir}/validate.sh || exit 1

  if [ -f "validation_nproma_${nproma}.failed" ]; then
    let "teststatus+=2**${tid}"
  fi

  run_command rm "${ftgoutputdir}/data/input" || exit 1
  run_command rm "${ftgoutputdir}/data/output" || exit 1
  run_command rm "${ftgoutputdir}/data" || exit 1
  let "tid+=1"

done

# Final status report
if [ "${teststatus}" -eq 0 ]; then
      run_command echo "#-----------------------------#" || exit 1
      run_command echo "#---   All tests PASSED!   ---#" || exit 1
      run_command echo "#-----------------------------#" || exit 1
  exit 0
else
      run_command echo "#------------------------------#" || exit 1
      run_command echo "#---   Some tests FAILED:   ---#" || exit 1
      run_command echo "#---   Status: " $(echo "obase=2;${teststatus}" | bc) "  ---#" || exit 1
      run_command echo "#------------------------------#" || exit 1
  exit 1
fi
#!/bin/bash -e

source ${commondir}/run_command.sh
source ${commondir}/interactive_step.sh

# Build compiler name for configure script by extending with target (except CPU)
compiler_target="${compiler}"
if [ "${target}" = "gpu" ]; then
  compiler_target+="_${target}"
fi

# Allow for interactive interruption
interactive_step "apply standalone run patches"
if [[ ${run_next_part} -eq 1 ]]; then

  # Apply patches for standalone
  run_command ${commondir}/apply_patches_run.sh || exit 1

fi

# Allow for interactive interruption
interactive_step "get standalone files and configure them"
if [[ ${run_next_part} -eq 1 ]]; then

  # Copy standalone files and tune for current install
  run_command cp -r ${commondir}/src . || exit 1
  run_command cp -r ${scriptdir}/src . || exit 1
  while read -r f;do
    run_command sed -i -e "s|++FTGDATADIR++|${ftgoutputdir}|g" "$f" || exit 1
  done <${scriptdir}/ftgdatadir_files.txt
  if [ -z "${rperturb}" ]; then
    run_command sed -i -e "s|++FTGPERTURB++|0.0|g" src/tests/${testname}.f90 || exit 1
  else
    run_command sed -i -e "s|++FTGPERTURB++|${rperturb}*10.0_wp**(-PRECISION(1.0_wp))|g" src/tests/${testname}.f90 || exit 1
  fi

fi

# Allow for interactive interruption
interactive_step "fix existing OpenACC statements"
if [[ ${run_next_part} -eq 1 ]]; then

  # Remove existing ACC statements unrelated to standalone
  if [ -f ${scriptdir}/noacc_list.txt ]; then
    while read -r f;do
      run_command sed -i -e 's/!$ACC/!NOENIAC/g' -e 's/!$acc/!noeniac/g' "$f" || exit 1
    done <${scriptdir}/noacc_list.txt
  fi

fi

# Allow for interactive interruption
interactive_step "configure and build standalone"
if [[ ${run_next_part} -eq 1 ]]; then

  # Configure build
  run_command echo "Configuring standalone ..." || exit 1
  run_command ./configure --with-fortran="${compiler_target}_standalone" >& standalone_configure.log || exit 1
  # Tune configuration for standalone
  ${commondir}/tune_configuration.sh

  # Build
  run_command echo "Building standalone ..." || exit 1
  run_command ./build_command >& standalone_build.log || exit 1

fi

# Allow for interactive interruption
interactive_step "sync serialization data and prepare run"
if [[ ${run_next_part} -eq 1 ]]; then

  tmpexpdir="${workdir}/tmp_experiments"
  # Sync run data and prepare run
  if [ -h "${workdir}/experiments" ]; then
    run_command mv "${workdir}/experiments" "${tmpexpdir}" || exit 1
  else
    run_command mkdir "${tmpexpdir}" || exit 1
  fi
  run_command rsync -aqv --delete "${datadir}/experiments" "${workdir}" || exit 1
  run_command cp ${commondir}/submit.sh ${expdir} || exit 1
  run_command pushd ${expdir} > /dev/null || exit 1
  run_command rsync -aqvL ../$(basename $expdir) "${tmpexpdir}" || exit
  run_command popd > /dev/null || exit 1
  run_command rm -r "${workdir}/experiments" || exit 1
  run_command mv "${tmpexpdir}" "${workdir}/experiments" || exit 1
  run_command cd ${expdir} || exit 1
  run_command cp ${workdir}/build/*/bin/${testname} . || exit 1

fi


# Allow for interactive interruption
interactive_step "run validation tests"
if [[ ${run_next_part} -eq 1 ]]; then

  # Run tests
  teststatus=0
  tid=0
  export nproma
  while read -r nproma; do
    run_command echo "Test with nproma = ${nproma}"

    # Sync serialization data for FTG
    run_command mkdir -p "${ftginputdir}" || exit 1
    run_command mkdir -p "${ftgoutputdir}/nproma_${nproma}" || exit 1
    run_command rsync -aqv "${dataftgdir}/nproma_${nproma}" "${ftginputdir}/" || exit 1
    run_command ln -s "${ftgoutputdir}/nproma_${nproma}" "${ftgoutputdir}/data" || exit 1
    run_command ln -s "${ftginputdir}/nproma_${nproma}/input" "${ftgoutputdir}/data/input" || exit 1
    run_command ln -s "${ftginputdir}/nproma_${nproma}/output" "${ftgoutputdir}/data/output" || exit 1

    # Run executable
    run_command echo "Running standalone ..."
    if [ -e ${scriptdir}/scripts/tune_run.sh ]; then
      run_command ${scriptdir}/scripts/tune_run.sh || exit 1
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

  done <${scriptdir}/nproma_list.txt

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

fi

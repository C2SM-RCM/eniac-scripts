#!/bin/bash -e

source ${commondir}/run_command.sh

# Validate serialization data
run_command echo "Validating standalone output ..."

# Cleanup results from previous run
rm -f validation_nproma_${nproma}.result
rm -f validation_nproma_${nproma}.failed

# Initialized job as failed
run_command echo "Test was not executed" > validation_nproma_${nproma}.failed || exit 1

if [ "${slave}" = "kesch" ]; then
  module load python/3.6.2-gmvolf-17.02 || exit 1
  if [ "${compiler}" = "pgi" ]; then
    module load pgi/17.10
  fi
elif [ "${slave}" = "daint" ]; then
  module load cray-python/3.6.1.1 || exit 1
fi

compare_cmd="$(grep "SERIALBOX2ROOT *=" ${workdir}/Makefile | sed -e 's/SERIALBOX2ROOT *= *//' -e 's/ *$//')/python/compare/compare.py"
metadata=MetaData-ftg_${testroutine}_output_0.json
# Ignore meta data errors
compare_options+=" -w"
# Use tolerance file if it exists
tolerance_file="${scriptdir}/tolerances/nproma_${nproma}.json"
if [ -e  "${tolerance_file}" ]; then
  compare_options+=" -T ${tolerance_file}"
  run_command echo "Using tolerance file with compare.py tool" || exit 1
fi
nice python3 "${compare_cmd}" ${compare_options} ${ftgoutputdir}/data/output/${metadata} ${ftgoutputdir}/data/output_test/${metadata} > validation_nproma_${nproma}.result || true
grep -B 5 '\[  FAILED  \].* (' validation_nproma_${nproma}.result > validation_nproma_${nproma}.failed || true
if [ "$(wc -l <validation_nproma_${nproma}.result)" -eq 0 ]; then
  run_command echo '[  FAILED  ] compare.py failed!' > validation_nproma_${nproma}.failed || exit 1
fi
code="$(wc -l <validation_nproma_${nproma}.failed)"
if [ ${code} -eq 0 ]; then
  run_command echo "###############################################" || exit 1
  run_command echo "###   nproma=${nproma}: All tests PASSED!   ###" || exit 1
  run_command echo "###############################################" || exit 1
  run_command echo "" || exit 1
  run_command rm validation_nproma_${nproma}.failed || exit 1
else
  run_command echo "################################################" || exit 1
  run_command echo "###   nproma=${nproma}: Validation FAILED!   ###" || exit 1
  run_command echo "" || exit 1
  run_command cat validation_nproma_${nproma}.failed || exit 1
  run_command echo "################################################" || exit 1
  run_command echo "" || exit 1
fi

exit 0

#!/bin/bash -e

if [[ $# -lt 3 ]]; then
  echo "USAGE: script requires at least 3 arguments"
  echo "       update_tolerances /path/to/validation_nproma_X.failed /path/to/tolerances/files nproma [nproma [...]]"
  exit 1
fi

# General setup
scale=3.0
jq_cmd=~/bin/jq

# Get script arguments
error_path=$1
tolerances_path=$2
npromas="${@:3}"

# Loop over requested nproma values
for n in ${npromas}; do
  error_file="${error_path}/validation_nproma_${n}.failed"
  # Check if error file exists
  if [ -e ${error_file} ]; then

    echo "Updating tolerances for nproma = ${n}: "
    echo "============================================" 
    tolerances_json="${tolerances_path}/nproma_${n}.json"

    # Extracting relative errors for validation log
    awk_tmp="$(mktemp tmp_awk_cmd_XXXXX)"
    echo '$2 > 0.0 {print ".[\""$4"\"] = "$2 * '${scale}';} $2 == 0 {print ".[\""$4"\"] = "$1 * '${scale}';}' > ${awk_tmp}
    tolerances_tmp="$(mktemp tmp_tolerances_XXXXX.json)"
    grep -B 2 '\[  FAILED  \]' ${error_file} | sed -e 's/  Maximum absolute error: //' -e 's/  Maximum relative error: //' -e 's/\[  FAILED  \]//' | tr '\n' ' ' | sed -e 's/ -- /;/g' | tr ';' '\n' | awk -f ${awk_tmp} | tr '\n' '|' | sed -e "s/|$//" > ${tolerances_tmp}
    updated_tolerances_tmp="$(mktemp tmp_update_tolerances_XXXXX.json)"
    ${jq_cmd} -f ${tolerances_tmp} ${tolerances_json} > ${updated_tolerances_tmp} || exit 1
    echo "    Updating following values:"
    echo "------------------------------"
    cat ${tolerances_tmp} | tr '|' '\n'
    echo ""
    echo "------------------------------"
    mv ${updated_tolerances_tmp} ${tolerances_json}

    # Cleanup temporary files
    rm ${awk_tmp}
    rm ${tolerances_tmp}

  else
    echo "No errors found. Skipping nproma = ${n}"
    echo "============================================" 
  fi
done


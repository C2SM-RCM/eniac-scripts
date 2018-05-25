#!/bin/bash -e 

function interactive_step() {
  eniac_step_counter=$((eniac_step_counter+1))
  echo "---------------------------------------------------------------------"
  if [[ -z $1 ]]; then
    echo "Step ${eniac_step_counter}"
  else
    echo "Step ${eniac_step_counter}: $1"
  fi
  echo "---------------------------------------------------------------------"
  if [[ ${ENIAC_INTERACTIVE} -eq 1 ]]; then 
    select yn in "Continue" "Abort" "Skip"; do
      case $yn in
        Continue ) run_next_part=1;break;;
        Abort ) run_next_part=0;exit 0;;
        Skip ) run_next_part=0;break;;
      esac
    done
  else
    run_next_part=1
  fi
}

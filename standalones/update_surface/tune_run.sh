#!/bin/bash -e

source ${commondir}/run_command.sh

run_command sed -e "s/++NPROMA++/${nproma}/" NAMELIST_atm_amip_test_atm.template > NAMELIST_atm_amip_test_atm || exit 1
run_command sed -e "s/++NPROMA++/${nproma}/" NAMELIST_ICON_output_atm.template > NAMELIST_ICON_output_atm || exit 1

#!/bin/bash -e

source ${commondir}/run_command.sh

# This only works with GCC on CPU
if [ !("${compiler}" == "gcc" && "${target}" == "cpu" ]; then
  echo "Extraction of standalone only works with GCC on CPU"
  exit 1
fi

# Apply patches for standalone extraction
run_command ${scriptdir}/apply_patches_extract.sh || exit 1

# Clone and configure FCG and FTG
run_command git clone https://github.com/fortesg/fortrancallgraph
run_command sed -e "s|++ICONDIR++|${workdir}|g" ${commondir}/ftg/config_fortrancallgraph.py.tmpl > fortrancallgraph/config_fortrancallgraph.py
run_command git clone https://github.com/fortesg/fortrantestgenerator
run_command sed -e "s|++ICONDIR++|${workdir}|g" ${commondir}/ftg/config_fortrantestgenerator.py.tmpl > fortrantestgenerator/config_fortrantestgenerator.py

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran=gcc >& standalone_configure.log || exit 1

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build.log || exit 1

# Generate capture code
run_command cd fortrantestgenerator/
run_command FortranTestGenerator.py -c ${testmodule} ${testroutine}
run_command cd ../

# Cleanup
make distclean

# Configure build
run_command echo "Configuring standalone ..." || exit 1
run_command ./configure --with-fortran=gcc >& standalone_configure.log || exit 1

# Build
run_command echo "Building standalone ..." || exit 1
run_command ./build_command >& standalone_build.log || exit 1

# Generate replay code
run_command cd fortrantestgenerator/
run_command FortranTestGenerator.py -c ${testmodule} ${testroutine}
run_command cd ../

# Isolate standalone
run_command mkdir standalone || exit 1

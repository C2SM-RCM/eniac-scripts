#!/bin/bash -e

source ${commondir}/run_command.sh

timing=$1

diff_files ()
{
  adir=$1
  bdir=$2
  run_command mkdir -p ${patchdir} || exit 1
  for f in `find -L ${bdir}/src/ -type f`;do 
    patchfile=${patchdir}/$(basename ${f/.f90/.patch})
    diff -uBb ${f/eniac_b\//eniac_a\/} $f > ${patchfile} || true
    if [ `wc -l <${patchfile}` -eq 0 ]; then
      run_command rm ${patchfile} || exit 1
    fi
  done

  # ... Cleanup
  run_command rmdir --ignore-fail-on-non-empty ${patchdir} 
}

# Create SRC patches
patchdir="eniac_patches/extract/${timing}"
diff_files "eniac_a" "eniac_b"

# Create externals patches
patchbasedir=${patchdir}
for ext in ${workdir}/externals/*; do
  patchdir=${patchbasedir}/$(basename ${ext})
  diff_files "eniac_a/externals/$(basename ${ext})" "eniac_b/externals/$(basename ${ext})"
done

# Cleanup
run_command rmdir --ignore-fail-on-non-empty ${patchbasedir} 

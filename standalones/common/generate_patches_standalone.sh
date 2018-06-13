#!/bin/bash -e

revert_noacc=0
if [ $1 ]; then
  if [ $1 == "revert_noacc" ]; then
    revert_noacc=1
  fi
fi

source ${commondir}/run_command.sh

# Revert NOACC replacements (ie. not first run)
if [ ${revert_noacc} -eq 1 ]; then
  run_command pushd ${standalonedir} || exit 1
  if [ -f ${scriptdir}/noacc_list.txt ]; then
    while read -r f;do
      if [ -f $f ]; then
        run_command sed -i -e 's/!NOENIAC/!$ACC/g' -e 's/!noeniac/!$acc/g' "$f" || exit 1
      fi
    done <${scriptdir}/noacc_list.txt
  fi
  run_command popd || exit 1
fi

diff_files ()
{
  adir=$1
  bdir=$2
  run_command mkdir ${patchdir} || exit 1
  run_command mkdir eniac_a eniac_b || exit 1
  run_command ln -sr ${adir}/src eniac_a/src || exit 1
  run_command ln -sr ${bdir}/src eniac_b/src || exit 1
  for f in `find -L eniac_b/src/ -type f`;do 
    patchfile=${patchdir}/$(basename ${f/.f90/.patch})
    diff -uBb ${f/eniac_b\//eniac_a\/} $f > ${patchfile} 2> /dev/null || true
    if [ `wc -l <${patchfile}` -eq 0 ]; then
      run_command rm ${patchfile} || exit 1
    fi
  done

  # ... Cleanup
  run_command rmdir --ignore-fail-on-non-empty ${patchdir} 
  run_command rm eniac_a/src eniac_b/src || exit 1
  run_command rmdir eniac_a eniac_b || exit 1
}

# Create SRC patches
patchdir=${standalonedir}/eniac_patches
diff_files "./" "${standalonedir}"

# Create externals patches
patchbasedir=${patchdir}
for ext in ${standalonedir}/externals/*; do
  patchdir=${patchbasedir}/$(basename ${ext})
  diff_files "externals/$(basename ${ext})" "${ext}"
done

# Remove existing ACC statements unrelated to standalone
if [ ${revert_noacc} -eq 1 ]; then
  run_command pushd ${standalonedir} || exit 1
  if [ -f ${scriptdir}/noacc_list.txt ]; then
    while read -r f;do
      if [ -f $f ]; then
        run_command sed -i -e 's/!$ACC/!NOENIAC/g' -e 's/!$acc/!noeniac/g' "$f" || exit 1
      fi
    done <${scriptdir}/noacc_list.txt
  fi
  run_command popd ${workdir} || exit 1
fi

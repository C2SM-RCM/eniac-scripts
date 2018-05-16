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
  run_command cd ${standalonedir} || exit 1
  if [ -f ${scriptdir}/noacc_list.txt ]; then
    while read -r f;do
      if [ -f $f ]; then
        run_command sed -i -e 's/!NOENIAC/!$ACC/g' -e 's/!noeniac/!$acc/g' "$f" || exit 1
      fi
    done <${scriptdir}/noacc_list.txt
  fi
  run_command cd ${workdir} || exit 1
fi

diff_files ()
{
  adir=$1
  bdir=$2
  run_command mkdir ${patchdir} || exit 1
  run_command mkdir a b || exit 1
  run_command ln -s ${adir}/src a/src || exit 1
  run_command ln -s ${bdir}/src b/src || exit 1
  for f in `find -L b/src/ -type f`;do 
    patchfile=${patchdir}/$(basename ${f/.f90/.patch})
    diff -uBb ${f/b\//a\/} $f > ${patchfile} || true
    if [ `wc -l <${patchfile}` -eq 0 ]; then
      run_command rm ${patchfile} || exit 1
    fi
  done

  # ... Cleanup
  run_command rmdir --ignore-fail-on-non-empty ${patchdir} 
  run_command rm a/src b/src || exit 1
  run_command rmdir a b || exit 1
}

# Create SRC patches
patchdir=${standalonedir}/patches
diff_files "${workdir}" "${workdir}/${standalonedir}"

# Create externals patches
for ext in ${standalonedir}/externals/*; do
  patchdir=${standalonedir}/patches/$(basename ${ext})
  diff_files "${workdir}/externals/$(basename ${ext})" "${workdir}/${ext}"
done

# Remove existing ACC statements unrelated to standalone
if [ ${revert_noacc} -eq 1 ]; then
  run_command cd ${standalonedir} || exit 1
  if [ -f ${scriptdir}/noacc_list.txt ]; then
    while read -r f;do
      if [ -f $f ]; then
        run_command sed -i -e 's/!$ACC/!NOENIAC/g' -e 's/!$acc/!noeniac/g' "$f" || exit 1
      fi
    done <${scriptdir}/noacc_list.txt
  fi
  run_command cd ${workdir} || exit 1
fi

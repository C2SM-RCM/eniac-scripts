Scripts
=======

extract_standalone.sh
-------------------
Master script to extract standalone from icon-cscs.git.

generate_patches.sh
-------------------
Master script to (re)generate patches during extraction and runing the standalone.

create_reference.sh
-------------------
Master script to generate standalone reference data.

run_standalone_test.sh
----------------------
Master script to run standalone validation scripts.

reset_repo.sh
-------------
Cleanup script to reset repository to original state when working manually. WARNING: THERE ARE NO SAFEGUARDS, IT MIGHT WIPE OUT CHANGES IN YOUR REPO!

Files
=====

ftgdatadir_files.txt
--------------------
List of files containing templated FTG serialization data paths: ++FTGDATADIR++ -> /scratch/jenkins/.../

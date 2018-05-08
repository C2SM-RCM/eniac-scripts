Directories
===========

patches/
--------
Patches to apply on top of repository checkout to produce standalone.

src/
----
Additional source files to inject into source tree. (i.e. src/tests/ftg_update_surface_test.f90)

Scripts
=======

jenkins_run.sh
--------------
Master script to run standalone validation scripts.

jenkins_reference.sh
--------------
Master script to generate standalone reference data.

apply_patches.sh
----------------
Apply patches required for standalone.

apply_patches_reference.sh
----------------
Apply patches required for generating reference data.

tune_configuration.sh
---------------------
Tune configuraton to produce standalone executable.

reset_repo.sh
-------------
Cleanup script to reset repository to original state when working manually. WARNING: THERE ARE NO SAFEGUARDS, IT MIGHT WIPE OUT CHANGES IN YOUR REPO!

Other
=====

ftgdatadir_files.txt
--------------------
List of files containing templated FTG serialization data paths: ++FTGDATADIR++ -> /scratch/jenkins/.../

extra_depencies.txt
--------------------
List of Fortran source files that need to be manually injected into dependency tree (i.e. not detected by fdependencies)

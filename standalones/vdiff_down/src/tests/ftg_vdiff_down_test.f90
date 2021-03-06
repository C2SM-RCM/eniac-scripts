
PROGRAM ftg_vdiff_down_test
  
  USE mtime
  USE mo_kind
  USE timing_tools
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_vdiff_downward_sweep, ONLY: vdiff_down, ftg_vdiff_down_capture_input_enabled, ftg_vdiff_down_capture_output_enabled, &
  &  ftg_vdiff_down_capture_round, ftg_vdiff_down_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_echam_vdf_config, ONLY: mo_echam_vdf_config__echam_vdf_config => echam_vdf_config, t_echam_vdf_config
  USE mo_vdiff_solver, ONLY: mo_vdiff_solver__ixl => ixl, mo_vdiff_solver__ixi => ixi, mo_vdiff_solver__nmatrix => nmatrix, &
  &  mo_vdiff_solver__imh => imh, mo_vdiff_solver__itotte => itotte, mo_vdiff_solver__nvar_vdiff => nvar_vdiff, &
  &  mo_vdiff_solver__ixv => ixv, mo_vdiff_solver__imqv => imqv, mo_vdiff_solver__matrix_idx => matrix_idx, &
  &  mo_vdiff_solver__ibtm_var => ibtm_var, mo_vdiff_solver__ithv => ithv, mo_vdiff_solver__iqv => iqv, mo_vdiff_solver__ibtm_mtrx &
  &  => ibtm_mtrx, mo_vdiff_solver__ih => ih, mo_vdiff_solver__itrc_start => itrc_start, mo_vdiff_solver__iv => iv, &
  &  mo_vdiff_solver__iu => iu
  USE mo_convect_tables, ONLY: mo_convect_tables__tlucu => tlucu, mo_convect_tables__lookupoverflow => lookupoverflow
#ifdef __SPLINE_TEST__
  USE mo_convect_tables, ONLY: mo_convect_tables__za => za, mo_convect_tables__ua => ua, mo_convect_tables__dua => dua
#endif
  USE mo_model_domain, ONLY: mo_model_domain__p_patch => p_patch, t_patch
  USE mo_echam_vdiff_params, ONLY: mo_echam_vdiff_params__itop => itop
  
  USE mo_echam_vdf_config, ONLY: t_echam_vdf_config
  USE mo_model_domain, ONLY: t_patch
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = ++FTGPERTURB++
  
  CALL start_mpi('ftg_vdiff_down_test') !ICON
  
  CALL ftg_test_vdiff_down()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_vdiff_down()
    
    INTEGER :: jg
    INTEGER :: jb
    INTEGER :: jcs
    INTEGER :: kproma
    INTEGER :: kbdim
    INTEGER :: klev
    INTEGER :: klevm1
    INTEGER :: klevp1
    INTEGER :: ktrac
    INTEGER :: ksfc_type
    INTEGER :: idx_wtr
    INTEGER :: idx_ice
    INTEGER :: idx_lnd
    REAL(wp) :: pdtime
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcoriol
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pzf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pzh
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptsfc_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocv
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ppsfc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pum1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pqm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxlm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxim1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxm1
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: pxtm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pmair
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pmref
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: paphm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: papm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: paclc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxt_emis
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pz0m_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptottem1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pustar
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pwstar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pwstar_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pqsat_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: phdtcbl
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pri
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pri_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pmixlen
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfh
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfv
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcftotte
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfthv
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: aa
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: aa_btm
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: bb
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: bb_btm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pfactor_sfc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pzthvvar
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pthvsig
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pztottevn
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pch_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbhn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbh_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcsat
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcair
    REAL(wp), DIMENSION(:), ALLOCATABLE :: paz0lh
    
    CALL message('FTG', '*** Run test for vdiff_down ***')
    
    ftg_vdiff_down_capture_input_enabled = .FALSE.
    ftg_vdiff_down_capture_output_enabled = OUTPUT_ENABLED
    ftg_vdiff_down_output_dir = OUTPUT_DIR
    ftg_vdiff_down_capture_round = 1
    
    CALL ftg_vdiff_down_init_for_replay('input')
    CALL ftg_vdiff_down_replay_input(jg, jb, jcs, kproma, kbdim, klev, klevm1, klevp1, ktrac, ksfc_type, idx_wtr, idx_ice, &
    &  idx_lnd, pdtime, pcoriol, pzf, pzh, pfrc, ptsfc_tile, pocu, pocv, ppsfc, pum1, pvm1, ptm1, pqm1, pxlm1, pxim1, pxm1, pxtm1, &
    &  pmair, pmref, paphm1, papm1, ptvm1, paclc, pxt_emis, pthvvar, pxvar, pz0m_tile, ptottem1, pustar, pwstar, pwstar_tile, &
    &  pqsat_tile, phdtcbl, pri, pri_tile, pmixlen, pcfm, pcfm_tile, pcfh, pcfh_tile, pcfv, pcftotte, pcfthv, aa, aa_btm, bb, &
    &  bb_btm, pfactor_sfc, pcpt_tile, pcptgz, pzthvvar, pthvsig, pztottevn, pch_tile, pbn_tile, pbhn_tile, pbm_tile, pbh_tile, &
    &  pcsat, pcair, paz0lh)
    CALL ftg_destroy_serializer()
    
    CALL start_loc_timing("vdiff_down", 1)
    CALL vdiff_down(jg, jb, jcs, kproma, kbdim, klev, klevm1, klevp1, ktrac, ksfc_type, idx_wtr, idx_ice, idx_lnd, pdtime, &
    &  pcoriol, pzf, pzh, pfrc, ptsfc_tile, pocu, pocv, ppsfc, pum1, pvm1, ptm1, pqm1, pxlm1, pxim1, pxm1, pxtm1, pmair, pmref, &
    &  paphm1, papm1, ptvm1, paclc, pxt_emis, pthvvar, pxvar, pz0m_tile, ptottem1, pustar, pwstar, pwstar_tile, pqsat_tile, &
    &  phdtcbl, pri, pri_tile, pmixlen, pcfm, pcfm_tile, pcfh, pcfh_tile, pcfv, pcftotte, pcfthv, aa, aa_btm, bb, bb_btm, &
    &  pfactor_sfc, pcpt_tile, pcptgz, pzthvvar, pthvsig, pztottevn, pch_tile, pbn_tile, pbhn_tile, pbm_tile, pbh_tile, pcsat, &
    &  pcair, paz0lh)
    CALL end_loc_timing(1)
    CALL print_loc_timing()
    
  END SUBROUTINE ftg_test_vdiff_down
  
  
  SUBROUTINE ftg_vdiff_down_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_vdiff_down_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT vdiff_down '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
    call init_loc_timing()
    
  END SUBROUTINE ftg_vdiff_down_init_for_replay
  
  SUBROUTINE ftg_vdiff_down_replay_input(jg, jb, jcs, kproma, kbdim, klev, klevm1, klevp1, ktrac, ksfc_type, idx_wtr, idx_ice, &
  &  idx_lnd, pdtime, pcoriol, pzf, pzh, pfrc, ptsfc_tile, pocu, pocv, ppsfc, pum1, pvm1, ptm1, pqm1, pxlm1, pxim1, pxm1, pxtm1, &
  &  pmair, pmref, paphm1, papm1, ptvm1, paclc, pxt_emis, pthvvar, pxvar, pz0m_tile, ptottem1, pustar, pwstar, pwstar_tile, &
  &  pqsat_tile, phdtcbl, pri, pri_tile, pmixlen, pcfm, pcfm_tile, pcfh, pcfh_tile, pcfv, pcftotte, pcfthv, aa, aa_btm, bb, &
  &  bb_btm, pfactor_sfc, pcpt_tile, pcptgz, pzthvvar, pthvsig, pztottevn, pch_tile, pbn_tile, pbhn_tile, pbm_tile, pbh_tile, &
  &  pcsat, pcair, paz0lh)
    
    INTEGER, INTENT(inout) :: jg
    INTEGER, INTENT(inout) :: jb
    INTEGER, INTENT(inout) :: jcs
    INTEGER, INTENT(inout) :: kproma
    INTEGER, INTENT(inout) :: kbdim
    INTEGER, INTENT(inout) :: klev
    INTEGER, INTENT(inout) :: klevm1
    INTEGER, INTENT(inout) :: klevp1
    INTEGER, INTENT(inout) :: ktrac
    INTEGER, INTENT(inout) :: ksfc_type
    INTEGER, INTENT(inout) :: idx_wtr
    INTEGER, INTENT(inout) :: idx_ice
    INTEGER, INTENT(inout) :: idx_lnd
    REAL(wp), INTENT(inout) :: pdtime
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pcoriol
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pzf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pzh
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptsfc_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocv
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: ppsfc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pum1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pqm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxlm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxim1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxm1
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: pxtm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pmair
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pmref
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: paphm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: papm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: paclc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxt_emis
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pz0m_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptottem1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pustar
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pwstar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pwstar_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pqsat_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: phdtcbl
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pri
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pri_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pmixlen
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfh
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfv
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcftotte
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfthv
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout) :: aa
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout) :: aa_btm
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: bb
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: bb_btm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pfactor_sfc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pzthvvar
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pthvsig
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pztottevn
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pch_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbhn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbh_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pcsat
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pcair
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: paz0lh
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA vdiff_down (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("jg", jg)
    CALL ftg_read("jb", jb)
    CALL ftg_read("jcs", jcs)
    CALL ftg_read("kproma", kproma)
    CALL ftg_read("kbdim", kbdim)
    CALL ftg_read("klev", klev)
    CALL ftg_read("klevm1", klevm1)
    CALL ftg_read("klevp1", klevp1)
    CALL ftg_read("ktrac", ktrac)
    CALL ftg_read("ksfc_type", ksfc_type)
    CALL ftg_read("idx_wtr", idx_wtr)
    CALL ftg_read("idx_ice", idx_ice)
    CALL ftg_read("idx_lnd", idx_lnd)
    CALL ftg_read("pdtime", pdtime)
    CALL ftg_allocate_and_read_allocatable("pcoriol", pcoriol, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcoriol)
    !$ACC ENTER DATA COPYIN( pcoriol )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pzf", pzf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pzf)
    !$ACC ENTER DATA COPYIN( pzf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pzh", pzh, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pzh)
    !$ACC ENTER DATA COPYIN( pzh )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pfrc", pfrc, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pfrc)
    !$ACC ENTER DATA COPYIN( pfrc )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ptsfc_tile", ptsfc_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ptsfc_tile)
    !$ACC ENTER DATA COPYIN( ptsfc_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pocu", pocu, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pocu)
    !$ACC ENTER DATA COPYIN( pocu )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pocv", pocv, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pocv)
    !$ACC ENTER DATA COPYIN( pocv )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ppsfc", ppsfc, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ppsfc)
    !$ACC ENTER DATA COPYIN( ppsfc )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pum1", pum1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pum1)
    !$ACC ENTER DATA COPYIN( pum1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pvm1", pvm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pvm1)
    !$ACC ENTER DATA COPYIN( pvm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ptm1", ptm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ptm1)
    !$ACC ENTER DATA COPYIN( ptm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pqm1", pqm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pqm1)
    !$ACC ENTER DATA COPYIN( pqm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxlm1", pxlm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxlm1)
    !$ACC ENTER DATA COPYIN( pxlm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxim1", pxim1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxim1)
    !$ACC ENTER DATA COPYIN( pxim1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxm1", pxm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxm1)
    !$ACC ENTER DATA COPYIN( pxm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxtm1", pxtm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxtm1)
    !$ACC ENTER DATA COPYIN( pxtm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pmair", pmair, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pmair)
    !$ACC ENTER DATA COPYIN( pmair )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pmref", pmref, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pmref)
    !$ACC ENTER DATA COPYIN( pmref )
#endif
    
    CALL ftg_allocate_and_read_allocatable("paphm1", paphm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_paphm1)
    !$ACC ENTER DATA COPYIN( paphm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("papm1", papm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_papm1)
    !$ACC ENTER DATA COPYIN( papm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ptvm1", ptvm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ptvm1)
    !$ACC ENTER DATA COPYIN( ptvm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("paclc", paclc, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_paclc)
    !$ACC ENTER DATA COPYIN( paclc )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxt_emis", pxt_emis, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxt_emis)
    !$ACC ENTER DATA COPYIN( pxt_emis )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pthvvar", pthvvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pthvvar)
    !$ACC ENTER DATA COPYIN( pthvvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxvar", pxvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxvar)
    !$ACC ENTER DATA COPYIN( pxvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pz0m_tile", pz0m_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pz0m_tile)
    !$ACC ENTER DATA COPYIN( pz0m_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ptottem1", ptottem1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ptottem1)
    !$ACC ENTER DATA COPYIN( ptottem1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pustar", pustar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pustar)
    !$ACC ENTER DATA COPYIN( pustar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pwstar", pwstar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pwstar)
    !$ACC ENTER DATA COPYIN( pwstar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pwstar_tile", pwstar_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pwstar_tile)
    !$ACC ENTER DATA COPYIN( pwstar_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pqsat_tile", pqsat_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pqsat_tile)
    !$ACC ENTER DATA COPYIN( pqsat_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("phdtcbl", phdtcbl, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_phdtcbl)
    !$ACC ENTER DATA COPYIN( phdtcbl )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pri", pri, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pri)
    !$ACC ENTER DATA COPYIN( pri )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pri_tile", pri_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pri_tile)
    !$ACC ENTER DATA COPYIN( pri_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pmixlen", pmixlen, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pmixlen)
    !$ACC ENTER DATA COPYIN( pmixlen )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfm", pcfm, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfm)
    !$ACC ENTER DATA COPYIN( pcfm )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfm_tile", pcfm_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfm_tile)
    !$ACC ENTER DATA COPYIN( pcfm_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfh", pcfh, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfh)
    !$ACC ENTER DATA COPYIN( pcfh )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfh_tile", pcfh_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfh_tile)
    !$ACC ENTER DATA COPYIN( pcfh_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfv", pcfv, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfv)
    !$ACC ENTER DATA COPYIN( pcfv )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcftotte", pcftotte, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcftotte)
    !$ACC ENTER DATA COPYIN( pcftotte )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfthv", pcfthv, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfthv)
    !$ACC ENTER DATA COPYIN( pcfthv )
#endif
    
    CALL ftg_allocate_and_read_allocatable("aa", aa, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_aa)
    !$ACC ENTER DATA COPYIN( aa )
#endif
    
    CALL ftg_allocate_and_read_allocatable("aa_btm", aa_btm, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_aa_btm)
    !$ACC ENTER DATA COPYIN( aa_btm )
#endif
    
    CALL ftg_allocate_and_read_allocatable("bb", bb, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_bb)
    !$ACC ENTER DATA COPYIN( bb )
#endif
    
    CALL ftg_allocate_and_read_allocatable("bb_btm", bb_btm, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_bb_btm)
    !$ACC ENTER DATA COPYIN( bb_btm )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pfactor_sfc", pfactor_sfc, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pfactor_sfc)
    !$ACC ENTER DATA COPYIN( pfactor_sfc )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcpt_tile", pcpt_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcpt_tile)
    !$ACC ENTER DATA COPYIN( pcpt_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcptgz", pcptgz, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcptgz)
    !$ACC ENTER DATA COPYIN( pcptgz )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pzthvvar", pzthvvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pzthvvar)
    !$ACC ENTER DATA COPYIN( pzthvvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pthvsig", pthvsig, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pthvsig)
    !$ACC ENTER DATA COPYIN( pthvsig )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pztottevn", pztottevn, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pztottevn)
    !$ACC ENTER DATA COPYIN( pztottevn )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pch_tile", pch_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pch_tile)
    !$ACC ENTER DATA COPYIN( pch_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pbn_tile", pbn_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pbn_tile)
    !$ACC ENTER DATA COPYIN( pbn_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pbhn_tile", pbhn_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pbhn_tile)
    !$ACC ENTER DATA COPYIN( pbhn_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pbm_tile", pbm_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pbm_tile)
    !$ACC ENTER DATA COPYIN( pbm_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pbh_tile", pbh_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pbh_tile)
    !$ACC ENTER DATA COPYIN( pbh_tile )
#endif
    
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(pcsat)) THEN
      CALL ftg_allocate_and_read_allocatable("pcsat", pcsat, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcsat)
      !$ACC ENTER DATA COPYIN( pcsat )
#endif
      
    END IF
    IF (PRESENT(pcair)) THEN
      CALL ftg_allocate_and_read_allocatable("pcair", pcair, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcair)
      !$ACC ENTER DATA COPYIN( pcair )
#endif
      
    END IF
    IF (PRESENT(paz0lh)) THEN
      CALL ftg_allocate_and_read_allocatable("paz0lh", paz0lh, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_paz0lh)
      !$ACC ENTER DATA COPYIN( paz0lh )
#endif
      
    END IF
    
    ! TYPE MEMBERS
    
    
    ! GLOBALS
#ifdef __SPLINE_TEST__
    CALL ftg_read("mo_convect_tables__dua", mo_convect_tables__dua)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_convect_tables__dua)
    !$ACC ENTER DATA COPYIN( mo_convect_tables__dua )
#endif
#endif
    
    CALL ftg_allocate_and_read_allocatable("mo_vdiff_solver__ibtm_mtrx", mo_vdiff_solver__ibtm_mtrx, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_vdiff_solver__ibtm_mtrx)
    !$ACC ENTER DATA COPYIN( mo_vdiff_solver__ibtm_mtrx )
#endif
    
    CALL ftg_allocate_and_read_allocatable("mo_vdiff_solver__ibtm_var", mo_vdiff_solver__ibtm_var, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_vdiff_solver__ibtm_var)
    !$ACC ENTER DATA COPYIN( mo_vdiff_solver__ibtm_var )
#endif
    
    CALL ftg_read("mo_vdiff_solver__ih", mo_vdiff_solver__ih)
    
    CALL ftg_read("mo_vdiff_solver__imh", mo_vdiff_solver__imh)
    
    CALL ftg_read("mo_vdiff_solver__imqv", mo_vdiff_solver__imqv)
    
    CALL ftg_read("mo_vdiff_solver__iqv", mo_vdiff_solver__iqv)
    
    CALL ftg_read("mo_vdiff_solver__ithv", mo_vdiff_solver__ithv)
    
    CALL ftg_read("mo_echam_vdiff_params__itop", mo_echam_vdiff_params__itop)
    
    CALL ftg_read("mo_vdiff_solver__itotte", mo_vdiff_solver__itotte)
    
    CALL ftg_read("mo_vdiff_solver__itrc_start", mo_vdiff_solver__itrc_start)
    
    CALL ftg_read("mo_vdiff_solver__iu", mo_vdiff_solver__iu)
    
    CALL ftg_read("mo_vdiff_solver__iv", mo_vdiff_solver__iv)
    
    CALL ftg_read("mo_vdiff_solver__ixi", mo_vdiff_solver__ixi)
    
    CALL ftg_read("mo_vdiff_solver__ixl", mo_vdiff_solver__ixl)
    
    CALL ftg_read("mo_vdiff_solver__ixv", mo_vdiff_solver__ixv)
    
    CALL ftg_read("mo_convect_tables__lookupoverflow", mo_convect_tables__lookupoverflow)
    
    CALL ftg_allocate_and_read_allocatable("mo_vdiff_solver__matrix_idx", mo_vdiff_solver__matrix_idx, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_vdiff_solver__matrix_idx)
    !$ACC ENTER DATA COPYIN( mo_vdiff_solver__matrix_idx )
#endif
    
    CALL ftg_read("mo_vdiff_solver__nmatrix", mo_vdiff_solver__nmatrix)
    
    CALL ftg_read("mo_vdiff_solver__nvar_vdiff", mo_vdiff_solver__nvar_vdiff)
    
    CALL ftg_read("mo_convect_tables__tlucu", mo_convect_tables__tlucu)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_convect_tables__tlucu)
    !$ACC ENTER DATA COPYIN( mo_convect_tables__tlucu )
#endif
    
#ifdef __SPLINE_TEST__
    CALL ftg_read("mo_convect_tables__ua", mo_convect_tables__ua)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_convect_tables__ua)
    !$ACC ENTER DATA COPYIN( mo_convect_tables__ua )
#endif
    
    CALL ftg_read("mo_convect_tables__za", mo_convect_tables__za)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_convect_tables__za)
    !$ACC ENTER DATA COPYIN( mo_convect_tables__za )
#endif
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%c_e", mo_echam_vdf_config__echam_vdf_config%c_e)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_c_e)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%c_e )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%c_f", mo_echam_vdf_config__echam_vdf_config%c_f)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_c_f)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%c_f )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%c_n", mo_echam_vdf_config__echam_vdf_config%c_n)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_c_n)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%c_n )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%f_tau0", mo_echam_vdf_config__echam_vdf_config%f_tau0)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_f_tau0)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%f_tau0 )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%f_theta0", mo_echam_vdf_config__echam_vdf_config%f_theta0)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_f_theta0)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%f_theta0 )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%fbl", mo_echam_vdf_config__echam_vdf_config%fbl)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_fbl)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%fbl )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%fsl", mo_echam_vdf_config__echam_vdf_config%fsl)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_fsl)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%fsl )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%lsfc_heat_flux", mo_echam_vdf_config__echam_vdf_config%lsfc_heat_flux)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_lsfc_heat_flux)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%lsfc_heat_flux )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%lsfc_mom_flux", mo_echam_vdf_config__echam_vdf_config%lsfc_mom_flux)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_lsfc_mom_flux)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%lsfc_mom_flux )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%pr0", mo_echam_vdf_config__echam_vdf_config%pr0)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_pr0)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%pr0 )
#endif
    
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%wmc", mo_echam_vdf_config__echam_vdf_config%wmc)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_echam_vdf_config__echam_vdf_config_wmc)
    ! THIS IS CURRENTLY DISABLED AS DEALING WITH DERIVED TYPES DOES NOT WORK
    !NOACC ENTER DATA COPYIN( mo_echam_vdf_config__echam_vdf_config%wmc )
#endif
    
    
    ftg_c = "mo_model_domain__p_patch"
    IF (ftg_field_exists(ftg_c)) THEN
      ftg_bounds = ftg_get_bounds(ftg_c)
      ALLOCATE(mo_model_domain__p_patch(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_model_domain__p_patch(0))
    END IF
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%decomp_info%glb_index'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%decomp_info%glb_index, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%edges%center'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%edges%center(ftg_bounds(1):ftg_bounds(2), ftg_bounds(3):ftg_bounds(4)))
      ELSE
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%edges%center(0, 0))
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%edges%center%lat'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%edges%center%lat)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%edges%center%lon'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%edges%center%lon)
    END DO
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_vdiff_down_replay_input
  
END PROGRAM ftg_vdiff_down_test



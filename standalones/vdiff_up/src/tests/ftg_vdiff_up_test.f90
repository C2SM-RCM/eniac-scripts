
PROGRAM ftg_vdiff_up_test
  
  USE mtime
  USE mo_kind
  USE timing_tools
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_vdiff_upward_sweep, ONLY: vdiff_up, ftg_vdiff_up_capture_input_enabled, ftg_vdiff_up_capture_output_enabled, &
  &  ftg_vdiff_up_capture_round, ftg_vdiff_up_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_vdiff_solver, ONLY: mo_vdiff_solver__iqv => iqv, mo_vdiff_solver__ixl => ixl, mo_vdiff_solver__ixi => ixi, &
  &  mo_vdiff_solver__nmatrix => nmatrix, mo_vdiff_solver__itotte => itotte, mo_vdiff_solver__matrix_idx => matrix_idx, &
  &  mo_vdiff_solver__ibtm_var => ibtm_var, mo_vdiff_solver__itrc_start => itrc_start, mo_vdiff_solver__iv => iv, &
  &  mo_vdiff_solver__nvar_vdiff => nvar_vdiff, mo_vdiff_solver__ithv => ithv, mo_vdiff_solver__iu => iu, mo_vdiff_solver__ixv => &
  &  ixv, mo_vdiff_solver__ih => ih
  USE mo_echam_vdiff_params, ONLY: mo_echam_vdiff_params__itop => itop
  
  
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = ++FTGPERTURB++
  
  CALL start_mpi('ftg_vdiff_up_test') !ICON
  
  CALL ftg_test_vdiff_up()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_vdiff_up()
    
    INTEGER :: jcs
    INTEGER :: kproma
    INTEGER :: kbdim
    INTEGER :: klev
    INTEGER :: klevm1
    INTEGER :: ktrac
    INTEGER :: ksfc_type
    INTEGER :: idx_wtr
    REAL(wp) :: pdtime
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfm_tile
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: aa
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pum1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pmair
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pmref
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pqm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxlm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxim1
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: pxtm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pgeom1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pztottevn
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: bb
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pzthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pz0m_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pkedisp
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pute_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pvte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pq_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pqte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxlte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pxite_vdf
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: pxtte_vdf
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pz0m
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptotte
    REAL(wp), DIMENSION(:), ALLOCATABLE :: psh_vdiff
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pqv_vdiff
    
    CALL message('FTG', '*** Run test for vdiff_up ***')
    
    ftg_vdiff_up_capture_input_enabled = .FALSE.
    ftg_vdiff_up_capture_output_enabled = OUTPUT_ENABLED
    ftg_vdiff_up_output_dir = OUTPUT_DIR
    ftg_vdiff_up_capture_round = 1
    
    CALL ftg_vdiff_up_init_for_replay('input')
    CALL ftg_vdiff_up_replay_input(jcs, kproma, kbdim, klev, klevm1, ktrac, ksfc_type, idx_wtr, pdtime, pfrc, pcfm_tile, aa, &
    &  pcptgz, pum1, pvm1, ptm1, pmair, pmref, pqm1, pxlm1, pxim1, pxtm1, pgeom1, pztottevn, bb, pzthvvar, pxvar, pz0m_tile, &
    &  pkedisp, pute_vdf, pvte_vdf, pq_vdf, pqte_vdf, pxlte_vdf, pxite_vdf, pxtte_vdf, pz0m, pthvvar, ptotte, psh_vdiff, pqv_vdiff)
    CALL ftg_destroy_serializer()
    
    CALL start_loc_timing("vdiff_up", 1)
    CALL vdiff_up(jcs, kproma, kbdim, klev, klevm1, ktrac, ksfc_type, idx_wtr, pdtime, pfrc, pcfm_tile, aa, pcptgz, pum1, pvm1, &
    &  ptm1, pmair, pmref, pqm1, pxlm1, pxim1, pxtm1, pgeom1, pztottevn, bb, pzthvvar, pxvar, pz0m_tile, pkedisp, pute_vdf, &
    &  pvte_vdf, pq_vdf, pqte_vdf, pxlte_vdf, pxite_vdf, pxtte_vdf, pz0m, pthvvar, ptotte, psh_vdiff, pqv_vdiff)
    CALL end_loc_timing(1)
    CALL print_loc_timing()
    
  END SUBROUTINE ftg_test_vdiff_up
  
  
  SUBROUTINE ftg_vdiff_up_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_vdiff_up_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT vdiff_up '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
    call init_loc_timing()
    
  END SUBROUTINE ftg_vdiff_up_init_for_replay
  
  SUBROUTINE ftg_vdiff_up_replay_input(jcs, kproma, kbdim, klev, klevm1, ktrac, ksfc_type, idx_wtr, pdtime, pfrc, pcfm_tile, aa, &
  &  pcptgz, pum1, pvm1, ptm1, pmair, pmref, pqm1, pxlm1, pxim1, pxtm1, pgeom1, pztottevn, bb, pzthvvar, pxvar, pz0m_tile, &
  &  pkedisp, pute_vdf, pvte_vdf, pq_vdf, pqte_vdf, pxlte_vdf, pxite_vdf, pxtte_vdf, pz0m, pthvvar, ptotte, psh_vdiff, pqv_vdiff)
    
    INTEGER, INTENT(inout) :: jcs
    INTEGER, INTENT(inout) :: kproma
    INTEGER, INTENT(inout) :: kbdim
    INTEGER, INTENT(inout) :: klev
    INTEGER, INTENT(inout) :: klevm1
    INTEGER, INTENT(inout) :: ktrac
    INTEGER, INTENT(inout) :: ksfc_type
    INTEGER, INTENT(inout) :: idx_wtr
    REAL(wp), INTENT(inout) :: pdtime
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfm_tile
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout) :: aa
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pum1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pvm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pmair
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pmref
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pqm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxlm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxim1
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: pxtm1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pgeom1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pztottevn
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: bb
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pzthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pz0m_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pkedisp
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pute_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pvte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pq_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pqte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxlte_vdf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pxite_vdf
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: pxtte_vdf
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pz0m
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pthvvar
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptotte
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: psh_vdiff
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pqv_vdiff
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA vdiff_up (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("jcs", jcs)
    CALL ftg_read("kproma", kproma)
    CALL ftg_read("kbdim", kbdim)
    CALL ftg_read("klev", klev)
    CALL ftg_read("klevm1", klevm1)
    CALL ftg_read("ktrac", ktrac)
    CALL ftg_read("ksfc_type", ksfc_type)
    CALL ftg_read("idx_wtr", idx_wtr)
    CALL ftg_read("pdtime", pdtime)
    CALL ftg_allocate_and_read_allocatable("pfrc", pfrc, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pfrc)
    !$ACC ENTER DATA COPYIN( pfrc )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcfm_tile", pcfm_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcfm_tile)
    !$ACC ENTER DATA COPYIN( pcfm_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("aa", aa, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_aa)
    !$ACC ENTER DATA COPYIN( aa )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pcptgz", pcptgz, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pcptgz)
    !$ACC ENTER DATA COPYIN( pcptgz )
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
    
    CALL ftg_allocate_and_read_allocatable("pmair", pmair, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pmair)
    !$ACC ENTER DATA COPYIN( pmair )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pmref", pmref, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pmref)
    !$ACC ENTER DATA COPYIN( pmref )
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
    
    CALL ftg_allocate_and_read_allocatable("pxtm1", pxtm1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxtm1)
    !$ACC ENTER DATA COPYIN( pxtm1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pgeom1", pgeom1, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pgeom1)
    !$ACC ENTER DATA COPYIN( pgeom1 )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pztottevn", pztottevn, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pztottevn)
    !$ACC ENTER DATA COPYIN( pztottevn )
#endif
    
    CALL ftg_allocate_and_read_allocatable("bb", bb, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_bb)
    !$ACC ENTER DATA COPYIN( bb )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pzthvvar", pzthvvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pzthvvar)
    !$ACC ENTER DATA COPYIN( pzthvvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxvar", pxvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxvar)
    !$ACC ENTER DATA COPYIN( pxvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pz0m_tile", pz0m_tile, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pz0m_tile)
    !$ACC ENTER DATA COPYIN( pz0m_tile )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pkedisp", pkedisp, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pkedisp)
    !$ACC ENTER DATA COPYIN( pkedisp )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pute_vdf", pute_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pute_vdf)
    !$ACC ENTER DATA COPYIN( pute_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pvte_vdf", pvte_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pvte_vdf)
    !$ACC ENTER DATA COPYIN( pvte_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pq_vdf", pq_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pq_vdf)
    !$ACC ENTER DATA COPYIN( pq_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pqte_vdf", pqte_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pqte_vdf)
    !$ACC ENTER DATA COPYIN( pqte_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxlte_vdf", pxlte_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxlte_vdf)
    !$ACC ENTER DATA COPYIN( pxlte_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxite_vdf", pxite_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxite_vdf)
    !$ACC ENTER DATA COPYIN( pxite_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pxtte_vdf", pxtte_vdf, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pxtte_vdf)
    !$ACC ENTER DATA COPYIN( pxtte_vdf )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pz0m", pz0m, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pz0m)
    !$ACC ENTER DATA COPYIN( pz0m )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pthvvar", pthvvar, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pthvvar)
    !$ACC ENTER DATA COPYIN( pthvvar )
#endif
    
    CALL ftg_allocate_and_read_allocatable("ptotte", ptotte, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_ptotte)
    !$ACC ENTER DATA COPYIN( ptotte )
#endif
    
    CALL ftg_allocate_and_read_allocatable("psh_vdiff", psh_vdiff, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_psh_vdiff)
    !$ACC ENTER DATA COPYIN( psh_vdiff )
#endif
    
    CALL ftg_allocate_and_read_allocatable("pqv_vdiff", pqv_vdiff, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_pqv_vdiff)
    !$ACC ENTER DATA COPYIN( pqv_vdiff )
#endif
    
    
    ! OPTIONAL ARGUMENTS
    
    ! TYPE MEMBERS
    
    
    ! GLOBALS
    CALL ftg_allocate_and_read_allocatable("mo_vdiff_solver__ibtm_var", mo_vdiff_solver__ibtm_var, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_vdiff_solver__ibtm_var)
    !$ACC ENTER DATA COPYIN( mo_vdiff_solver__ibtm_var )
#endif
    
    CALL ftg_read("mo_vdiff_solver__ih", mo_vdiff_solver__ih)
    
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
    
    CALL ftg_allocate_and_read_allocatable("mo_vdiff_solver__matrix_idx", mo_vdiff_solver__matrix_idx, ftg_rperturb)
#if defined(FTG_ACC_COPYIN) && !defined(FTG_ACC_NOCOPYIN_mo_vdiff_solver__matrix_idx)
    !$ACC ENTER DATA COPYIN( mo_vdiff_solver__matrix_idx )
#endif
    
    CALL ftg_read("mo_vdiff_solver__nmatrix", mo_vdiff_solver__nmatrix)
    
    CALL ftg_read("mo_vdiff_solver__nvar_vdiff", mo_vdiff_solver__nvar_vdiff)
    
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_vdiff_up_replay_input
  
END PROGRAM ftg_vdiff_up_test




PROGRAM ftg_nsurf_diag_test
  
  USE mtime
  USE mo_kind
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_surface_diag, ONLY: nsurf_diag, ftg_nsurf_diag_capture_input_enabled, ftg_nsurf_diag_capture_output_enabled, &
  &  ftg_nsurf_diag_capture_round, ftg_nsurf_diag_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_echam_convect_tables, ONLY: mo_echam_convect_tables__tlucu => tlucu
  USE mo_echam_phy_memory, ONLY: mo_echam_phy_memory__cdimissval => cdimissval
  
  
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
    '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
    '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  
  CALL start_mpi('ftg_nsurf_diag_test') !ICON
  
  CALL ftg_test_nsurf_diag()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_nsurf_diag()
    
    INTEGER :: kproma
    INTEGER :: kbdim
    INTEGER :: ksfc_type
    INTEGER :: idx_lnd
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pfrc
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pqm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: papm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: paphm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pxm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pum1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pvm1
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocv
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pzf
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pzs
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbhn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pbm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pri_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: psfcWind_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pdew2_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: puas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pvas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptasmax
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptasmin
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: psfcWind_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptas_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pdew2_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: puas_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pvas_tile
    
    CALL message('FTG', '*** Run test for nsurf_diag ***')
    
    ftg_nsurf_diag_capture_input_enabled = .FALSE.
    ftg_nsurf_diag_capture_output_enabled = OUTPUT_ENABLED
    ftg_nsurf_diag_output_dir = OUTPUT_DIR
    ftg_nsurf_diag_capture_round = 1
    
    CALL ftg_nsurf_diag_init_for_replay('input')
    CALL ftg_nsurf_diag_replay_input(kproma, kbdim, ksfc_type, idx_lnd, pfrc, pqm1, ptm1, papm1, paphm1, pxm1, pum1, pvm1, pocu, &
    &  pocv, pzf, pzs, pcptgz, pcpt_tile, pbn_tile, pbhn_tile, pbh_tile, pbm_tile, pri_tile, psfcWind_gbm, ptas_gbm, pdew2_gbm, &
    &  puas_gbm, pvas_gbm, ptasmax, ptasmin, psfcWind_tile, ptas_tile, pdew2_tile, puas_tile, pvas_tile)
    CALL ftg_destroy_serializer()
    
    CALL nsurf_diag(kproma, kbdim, ksfc_type, idx_lnd, pfrc, pqm1, ptm1, papm1, paphm1, pxm1, pum1, pvm1, pocu, pocv, pzf, pzs, &
    &  pcptgz, pcpt_tile, pbn_tile, pbhn_tile, pbh_tile, pbm_tile, pri_tile, psfcWind_gbm, ptas_gbm, pdew2_gbm, puas_gbm, &
    &  pvas_gbm, ptasmax, ptasmin, psfcWind_tile, ptas_tile, pdew2_tile, puas_tile, pvas_tile)
    
  END SUBROUTINE ftg_test_nsurf_diag
  
  
  SUBROUTINE ftg_nsurf_diag_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_nsurf_diag_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT nsurf_diag '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
  END SUBROUTINE ftg_nsurf_diag_init_for_replay
  
  SUBROUTINE ftg_nsurf_diag_replay_input(kproma, kbdim, ksfc_type, idx_lnd, pfrc, pqm1, ptm1, papm1, paphm1, pxm1, pum1, pvm1, &
  &  pocu, pocv, pzf, pzs, pcptgz, pcpt_tile, pbn_tile, pbhn_tile, pbh_tile, pbm_tile, pri_tile, psfcWind_gbm, ptas_gbm, &
  &  pdew2_gbm, puas_gbm, pvas_gbm, ptasmax, ptasmin, psfcWind_tile, ptas_tile, pdew2_tile, puas_tile, pvas_tile)
    
    INTEGER, INTENT(inout) :: kproma
    INTEGER, INTENT(inout) :: kbdim
    INTEGER, INTENT(inout) :: ksfc_type
    INTEGER, INTENT(inout) :: idx_lnd
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pfrc
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pqm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: ptm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: papm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: paphm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pxm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pum1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pvm1
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocv
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pzf
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pzs
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pcptgz
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbhn_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pbm_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pri_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: psfcWind_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: ptas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pdew2_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: puas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pvas_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: ptasmax
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: ptasmin
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: psfcWind_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptas_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pdew2_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: puas_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pvas_tile
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA nsurf_diag (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("kproma", kproma)
    CALL ftg_read("kbdim", kbdim)
    CALL ftg_read("ksfc_type", ksfc_type)
    CALL ftg_read("idx_lnd", idx_lnd)
    CALL ftg_allocate_and_read_allocatable("pfrc", pfrc)
    CALL ftg_allocate_and_read_allocatable("pqm1", pqm1)
    CALL ftg_allocate_and_read_allocatable("ptm1", ptm1)
    CALL ftg_allocate_and_read_allocatable("papm1", papm1)
    CALL ftg_allocate_and_read_allocatable("paphm1", paphm1)
    CALL ftg_allocate_and_read_allocatable("pxm1", pxm1)
    CALL ftg_allocate_and_read_allocatable("pum1", pum1)
    CALL ftg_allocate_and_read_allocatable("pvm1", pvm1)
    CALL ftg_allocate_and_read_allocatable("pocu", pocu)
    CALL ftg_allocate_and_read_allocatable("pocv", pocv)
    CALL ftg_allocate_and_read_allocatable("pzf", pzf)
    CALL ftg_allocate_and_read_allocatable("pzs", pzs)
    CALL ftg_allocate_and_read_allocatable("pcptgz", pcptgz)
    CALL ftg_allocate_and_read_allocatable("pcpt_tile", pcpt_tile)
    CALL ftg_allocate_and_read_allocatable("pbn_tile", pbn_tile)
    CALL ftg_allocate_and_read_allocatable("pbhn_tile", pbhn_tile)
    CALL ftg_allocate_and_read_allocatable("pbh_tile", pbh_tile)
    CALL ftg_allocate_and_read_allocatable("pbm_tile", pbm_tile)
    CALL ftg_allocate_and_read_allocatable("pri_tile", pri_tile)
    CALL ftg_allocate_and_read_allocatable("psfcWind_gbm", psfcWind_gbm)
    CALL ftg_allocate_and_read_allocatable("ptas_gbm", ptas_gbm)
    CALL ftg_allocate_and_read_allocatable("pdew2_gbm", pdew2_gbm)
    CALL ftg_allocate_and_read_allocatable("puas_gbm", puas_gbm)
    CALL ftg_allocate_and_read_allocatable("pvas_gbm", pvas_gbm)
    CALL ftg_allocate_and_read_allocatable("ptasmax", ptasmax)
    CALL ftg_allocate_and_read_allocatable("ptasmin", ptasmin)
    CALL ftg_allocate_and_read_allocatable("psfcWind_tile", psfcWind_tile)
    CALL ftg_allocate_and_read_allocatable("ptas_tile", ptas_tile)
    CALL ftg_allocate_and_read_allocatable("pdew2_tile", pdew2_tile)
    CALL ftg_allocate_and_read_allocatable("puas_tile", puas_tile)
    CALL ftg_allocate_and_read_allocatable("pvas_tile", pvas_tile)
    
    ! OPTIONAL ARGUMENTS
    
    ! TYPE MEMBERS
    
    
    ! GLOBALS
    CALL ftg_read("mo_echam_phy_memory__cdimissval", mo_echam_phy_memory__cdimissval)
    CALL ftg_read("mo_echam_convect_tables__tlucu", mo_echam_convect_tables__tlucu)
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_nsurf_diag_replay_input
  
END PROGRAM ftg_nsurf_diag_test




PROGRAM ftg_graupel_test
  
  USE mtime
  USE mo_kind
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE gscp_graupel, ONLY: graupel, ftg_graupel_capture_input_enabled, ftg_graupel_capture_output_enabled, &
  &  ftg_graupel_capture_round, ftg_graupel_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE gscp_graupel, ONLY: message_text
  USE gscp_data, ONLY: gscp_data__ccsagg => ccsagg, gscp_data__zvzxp => zvzxp, gscp_data__ccsaxp => ccsaxp, gscp_data__ccdvtp => &
  &  ccdvtp, gscp_data__zbev => zbev, gscp_data__zcev => zcev, gscp_data__ccsdxp => ccsdxp, gscp_data__ccslxp => ccslxp, &
  &  gscp_data__zcevxp => zcevxp, gscp_data__ccslam => ccslam, gscp_data__zceff_min => zceff_min, gscp_data__ccsdep => ccsdep, &
  &  gscp_data__ccswxp => ccswxp, gscp_data__zvz0i => zvz0i, gscp_data__ccsvel => ccsvel, gscp_data__ccsvxp => ccsvxp, &
  &  gscp_data__ccsrim => ccsrim, gscp_data__zvz0r => zvz0r, gscp_data__ccidep => ccidep, gscp_data__ccshi1 => ccshi1, &
  &  gscp_data__zbevxp => zbevxp, gscp_data__v0snow_gr => v0snow_gr, gscp_data__zconst => zconst
  
  
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = ++FTGPERTURB++
  
  CALL start_mpi('ftg_graupel_test') !ICON
  
  CALL ftg_test_graupel()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_graupel()
    
    INTEGER :: nvec
    INTEGER :: ke
    INTEGER :: ivstart
    INTEGER :: ivend
    INTEGER :: kstart
    INTEGER :: idbg
    REAL(KIND=wp) :: zdt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: dz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: t
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: p
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: rho
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qi
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qr
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qs
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qg
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE :: qnc
    REAL(KIND=wp) :: qi0
    REAL(KIND=wp) :: qc0
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE :: prr_gsp
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE :: prs_gsp
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE :: prg_gsp
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: tinc_lh
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: pstoph
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: tt_lheat
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: qrsflux
    LOGICAL :: l_cv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_t
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qi
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qr
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qs
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_au
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_ac
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_ev
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_nuc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_idep
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_sdep
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_agg
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_rim
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_rcri
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_icri
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_dau
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_iau
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_imelt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_smelt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_cfrz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_rfrz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_diag_shed
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE :: ddt_tend_qg
    
    CALL message('FTG', '*** Run test for graupel ***')
    
    ftg_graupel_capture_input_enabled = .FALSE.
    ftg_graupel_capture_output_enabled = OUTPUT_ENABLED
    ftg_graupel_output_dir = OUTPUT_DIR
    ftg_graupel_capture_round = 1
    
    CALL ftg_graupel_init_for_replay('input')
    CALL ftg_graupel_replay_input(nvec, ke, ivstart, ivend, kstart, idbg, zdt, dz, t, p, rho, qv, qc, qi, qr, qs, qg, qnc, qi0, &
    &  qc0, prr_gsp, prs_gsp, prg_gsp, tinc_lh, pstoph, tt_lheat, qrsflux, l_cv, ddt_tend_t, ddt_tend_qv, ddt_tend_qc, &
    &  ddt_tend_qi, ddt_tend_qr, ddt_tend_qs, ddt_diag_au, ddt_diag_ac, ddt_diag_ev, ddt_diag_nuc, ddt_diag_idep, ddt_diag_sdep, &
    &  ddt_diag_agg, ddt_diag_rim, ddt_diag_rcri, ddt_diag_icri, ddt_diag_dau, ddt_diag_iau, ddt_diag_imelt, ddt_diag_smelt, &
    &  ddt_diag_cfrz, ddt_diag_rfrz, ddt_diag_shed, ddt_tend_qg)
    CALL ftg_destroy_serializer()
    
    CALL graupel(nvec, ke, ivstart, ivend, kstart, idbg, zdt, dz, t, p, rho, qv, qc, qi, qr, qs, qg, qnc, qi0, qc0, prr_gsp, &
    &  prs_gsp, prg_gsp, tinc_lh, pstoph, tt_lheat, qrsflux, l_cv, ddt_tend_t, ddt_tend_qv, ddt_tend_qc, ddt_tend_qi, ddt_tend_qr, &
    &  ddt_tend_qs, ddt_diag_au, ddt_diag_ac, ddt_diag_ev, ddt_diag_nuc, ddt_diag_idep, ddt_diag_sdep, ddt_diag_agg, ddt_diag_rim, &
    &  ddt_diag_rcri, ddt_diag_icri, ddt_diag_dau, ddt_diag_iau, ddt_diag_imelt, ddt_diag_smelt, ddt_diag_cfrz, ddt_diag_rfrz, &
    &  ddt_diag_shed, ddt_tend_qg)
    
  END SUBROUTINE ftg_test_graupel
  
  
  SUBROUTINE ftg_graupel_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_graupel_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT graupel '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
  END SUBROUTINE ftg_graupel_init_for_replay
  
  SUBROUTINE ftg_graupel_replay_input(nvec, ke, ivstart, ivend, kstart, idbg, zdt, dz, t, p, rho, qv, qc, qi, qr, qs, qg, qnc, &
  &  qi0, qc0, prr_gsp, prs_gsp, prg_gsp, tinc_lh, pstoph, tt_lheat, qrsflux, l_cv, ddt_tend_t, ddt_tend_qv, ddt_tend_qc, &
  &  ddt_tend_qi, ddt_tend_qr, ddt_tend_qs, ddt_diag_au, ddt_diag_ac, ddt_diag_ev, ddt_diag_nuc, ddt_diag_idep, ddt_diag_sdep, &
  &  ddt_diag_agg, ddt_diag_rim, ddt_diag_rcri, ddt_diag_icri, ddt_diag_dau, ddt_diag_iau, ddt_diag_imelt, ddt_diag_smelt, &
  &  ddt_diag_cfrz, ddt_diag_rfrz, ddt_diag_shed, ddt_tend_qg)
    
    INTEGER, INTENT(inout) :: nvec
    INTEGER, INTENT(inout) :: ke
    INTEGER, INTENT(inout), OPTIONAL :: ivstart
    INTEGER, INTENT(inout), OPTIONAL :: ivend
    INTEGER, INTENT(inout), OPTIONAL :: kstart
    INTEGER, INTENT(inout), OPTIONAL :: idbg
    REAL(KIND=wp), INTENT(inout) :: zdt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: dz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: t
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: p
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: rho
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qi
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qr
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qs
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qg
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: qnc
    REAL(KIND=wp), INTENT(inout) :: qi0
    REAL(KIND=wp), INTENT(inout) :: qc0
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: prr_gsp
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: prs_gsp
    REAL(KIND=wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: prg_gsp
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: tinc_lh
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pstoph
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: tt_lheat
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: qrsflux
    LOGICAL, INTENT(inout), OPTIONAL :: l_cv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_t
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qv
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qi
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qr
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qs
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_au
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_ac
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_ev
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_nuc
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_idep
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_sdep
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_agg
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_rim
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_rcri
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_icri
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_dau
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_iau
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_imelt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_smelt
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_cfrz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_rfrz
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_diag_shed
    REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ddt_tend_qg
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA graupel (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("nvec", nvec)
    CALL ftg_read("ke", ke)
    CALL ftg_read("zdt", zdt)
    CALL ftg_allocate_and_read_allocatable("dz", dz, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("t", t, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p", p, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rho", rho, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qv", qv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qc", qc, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qi", qi, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qr", qr, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qs", qs, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qg", qg, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qnc", qnc, ftg_rperturb)
    CALL ftg_read("qi0", qi0)
    CALL ftg_read("qc0", qc0)
    CALL ftg_allocate_and_read_allocatable("prr_gsp", prr_gsp, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("prs_gsp", prs_gsp, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("prg_gsp", prg_gsp, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("tinc_lh", tinc_lh, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pstoph", pstoph, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("tt_lheat", tt_lheat, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("qrsflux", qrsflux, ftg_rperturb)
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(ivstart)) THEN
      CALL ftg_read("ivstart", ivstart)
    END IF
    IF (PRESENT(ivend)) THEN
      CALL ftg_read("ivend", ivend)
    END IF
    IF (PRESENT(kstart)) THEN
      CALL ftg_read("kstart", kstart)
    END IF
    IF (PRESENT(idbg)) THEN
      CALL ftg_read("idbg", idbg)
    END IF
    IF (PRESENT(l_cv)) THEN
      CALL ftg_read("l_cv", l_cv)
    END IF
    IF (PRESENT(ddt_tend_t)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_t", ddt_tend_t, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qv)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qv", ddt_tend_qv, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qc)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qc", ddt_tend_qc, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qi)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qi", ddt_tend_qi, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qr)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qr", ddt_tend_qr, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qs)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qs", ddt_tend_qs, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_au)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_au", ddt_diag_au, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_ac)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_ac", ddt_diag_ac, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_ev)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_ev", ddt_diag_ev, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_nuc)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_nuc", ddt_diag_nuc, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_idep)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_idep", ddt_diag_idep, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_sdep)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_sdep", ddt_diag_sdep, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_agg)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_agg", ddt_diag_agg, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_rim)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_rim", ddt_diag_rim, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_rcri)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_rcri", ddt_diag_rcri, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_icri)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_icri", ddt_diag_icri, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_dau)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_dau", ddt_diag_dau, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_iau)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_iau", ddt_diag_iau, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_imelt)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_imelt", ddt_diag_imelt, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_smelt)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_smelt", ddt_diag_smelt, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_cfrz)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_cfrz", ddt_diag_cfrz, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_rfrz)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_rfrz", ddt_diag_rfrz, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_diag_shed)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_diag_shed", ddt_diag_shed, ftg_rperturb)
    END IF
    IF (PRESENT(ddt_tend_qg)) THEN
      CALL ftg_allocate_and_read_allocatable("ddt_tend_qg", ddt_tend_qg, ftg_rperturb)
    END IF
    
    ! TYPE MEMBERS
    
    
    ! GLOBALS
    CALL ftg_read("gscp_data__ccdvtp", gscp_data__ccdvtp)
    CALL ftg_read("gscp_data__ccidep", gscp_data__ccidep)
    CALL ftg_read("gscp_data__ccsagg", gscp_data__ccsagg)
    CALL ftg_read("gscp_data__ccsaxp", gscp_data__ccsaxp)
    CALL ftg_read("gscp_data__ccsdep", gscp_data__ccsdep)
    CALL ftg_read("gscp_data__ccsdxp", gscp_data__ccsdxp)
    CALL ftg_read("gscp_data__ccshi1", gscp_data__ccshi1)
    CALL ftg_read("gscp_data__ccslam", gscp_data__ccslam)
    CALL ftg_read("gscp_data__ccslxp", gscp_data__ccslxp)
    CALL ftg_read("gscp_data__ccsrim", gscp_data__ccsrim)
    CALL ftg_read("gscp_data__ccsvel", gscp_data__ccsvel)
    CALL ftg_read("gscp_data__ccsvxp", gscp_data__ccsvxp)
    CALL ftg_read("gscp_data__ccswxp", gscp_data__ccswxp)
    ! *** WARNING: Type not supported by serialbox ***
    !     message_text
    !     CHARACTER(132) | dimension: 0
    CALL ftg_read("gscp_data__v0snow_gr", gscp_data__v0snow_gr)
    CALL ftg_read("gscp_data__zbev", gscp_data__zbev)
    CALL ftg_read("gscp_data__zbevxp", gscp_data__zbevxp)
    CALL ftg_read("gscp_data__zceff_min", gscp_data__zceff_min)
    CALL ftg_read("gscp_data__zcev", gscp_data__zcev)
    CALL ftg_read("gscp_data__zcevxp", gscp_data__zcevxp)
    CALL ftg_read("gscp_data__zconst", gscp_data__zconst)
    CALL ftg_read("gscp_data__zvz0i", gscp_data__zvz0i)
    CALL ftg_read("gscp_data__zvz0r", gscp_data__zvz0r)
    CALL ftg_read("gscp_data__zvzxp", gscp_data__zvzxp)
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_graupel_replay_input
  
END PROGRAM ftg_graupel_test



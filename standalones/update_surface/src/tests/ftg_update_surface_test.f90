
PROGRAM ftg_update_surface_test
  
  USE mtime
  USE mo_kind
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_surface, ONLY: update_surface, ftg_update_surface_capture_input_enabled, ftg_update_surface_capture_output_enabled, &
  &  ftg_update_surface_capture_round, ftg_update_surface_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_dynamics_config, ONLY: mo_dynamics_config__iequations => iequations
  USE mo_sea_ice_nml, ONLY: mo_sea_ice_nml__hci_layer => hci_layer, mo_sea_ice_nml__use_no_flux_gradients => &
  &  use_no_flux_gradients, mo_sea_ice_nml__i_ice_albedo => i_ice_albedo, mo_sea_ice_nml__i_ice_therm => i_ice_therm
  USE mo_echam_sfc_indices, ONLY: mo_echam_sfc_indices__nsfc_type => nsfc_type
  USE mo_echam_vdf_config, ONLY: mo_echam_vdf_config__echam_vdf_config => echam_vdf_config, t_echam_vdf_config
  USE mo_jsb_test, ONLY: mo_jsb_test__write_interface_vars => write_interface_vars
  USE mo_vdiff_solver, ONLY: mo_vdiff_solver__imh => imh, mo_vdiff_solver__imuv => imuv, mo_vdiff_solver__nmatrix => nmatrix, &
  &  mo_vdiff_solver__imqv => imqv, mo_vdiff_solver__nvar_vdiff => nvar_vdiff, mo_vdiff_solver__iqv => iqv, mo_vdiff_solver__ih => &
  &  ih, mo_vdiff_solver__iv => iv, mo_vdiff_solver__iu => iu
  USE mo_run_config, ONLY: mo_run_config__ltimer => ltimer
  USE mo_psrad_orbit, ONLY: mo_psrad_orbit__initialized => initialized, mo_psrad_orbit__declination => declination
  USE mo_echam_phy_config, ONLY: mo_echam_phy_config__echam_phy_config => echam_phy_config, t_echam_phy_config
  USE mo_master_control, ONLY: mo_master_control__master_namelist_filename => master_namelist_filename
  USE mo_echam_phy_memory, ONLY: mo_echam_phy_memory__cdimissval => cdimissval
  USE mo_parallel_config, ONLY: mo_parallel_config__nproma => nproma
  USE mo_timer, ONLY: mo_timer__timer_ice_fast => timer_ice_fast, mo_timer__timer_jsbach => timer_jsbach
  USE mo_read_netcdf_distributed, ONLY: mo_read_netcdf_distributed__basic_data => basic_data, t_basic_distrib_read_data
  USE mo_jsb_control, ONLY: mo_jsb_control__l_timer => l_timer, mo_jsb_control__debug => debug, mo_jsb_control__is_standalone => &
  &  is_standalone
  USE mo_surface, ONLY: lsfc_heat_flux, lsfc_mom_flux
  USE mo_model_domain, ONLY: mo_model_domain__p_patch => p_patch, t_patch
  
  USE mo_read_netcdf_distributed, ONLY: t_basic_distrib_read_data
  USE mo_echam_vdf_config, ONLY: t_echam_vdf_config
  USE mo_model_domain, ONLY: t_patch
  USE mo_echam_phy_config, ONLY: t_echam_phy_config
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = ++FTGPERTURB++
  
  CALL start_mpi('ftg_update_surface_test') !ICON
  
  CALL ftg_test_update_surface()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_update_surface()
    
    INTEGER :: jg
    INTEGER :: kproma
    INTEGER :: kbdim
    INTEGER :: kice
    INTEGER :: klev
    INTEGER :: ksfc_type
    INTEGER :: idx_wtr
    INTEGER :: idx_ice
    INTEGER :: idx_lnd
    REAL(wp) :: pdtime
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcfm_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pfac_sfc
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pocv
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: aa
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: aa_btm
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: bb
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: bb_btm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pqsat_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: ptsfc_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pu_stress_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pv_stress_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: plhflx_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pshflx_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pevap_gbm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pu_stress_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pv_stress_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: plhflx_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pshflx_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pevap_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pco2nat
    INTEGER :: nblock
    REAL(wp), DIMENSION(:), ALLOCATABLE :: lsm
    REAL(wp), DIMENSION(:), ALLOCATABLE :: alake
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pu
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pv
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptemp
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pq
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pco2
    REAL(wp), DIMENSION(:), ALLOCATABLE :: prsfl
    REAL(wp), DIMENSION(:), ALLOCATABLE :: prsfc
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pssfl
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pssfc
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rlds
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rlus
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rsds
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rsus
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rvds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rpds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rnds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rvds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rpds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE :: rnds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ps
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcosmu0
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pch_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcsat
    REAL(wp), DIMENSION(:), ALLOCATABLE :: pcair
    REAL(wp), DIMENSION(:), ALLOCATABLE :: q_snocpymlt
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: z0m_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: z0h_lnd
    REAL(wp), DIMENSION(:), ALLOCATABLE :: albvisdir
    REAL(wp), DIMENSION(:), ALLOCATABLE :: albnirdir
    REAL(wp), DIMENSION(:), ALLOCATABLE :: albvisdif
    REAL(wp), DIMENSION(:), ALLOCATABLE :: albnirdif
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albvisdir_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albnirdir_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albvisdif_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albnirdif_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: albedo
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albedo_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: pco2_flux_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptsfc
    REAL(wp), DIMENSION(:), ALLOCATABLE :: ptsfc_rad
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rsns_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rlns_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE :: lake_ice_frc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: Tsurf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: T1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: T2
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: hi
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: hs
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: Qtop
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: Qbot
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: conc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albvisdir_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albvisdif_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albnirdir_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE :: albnirdif_ice
    
    CALL message('FTG', '*** Run test for update_surface ***')
    
    ftg_update_surface_capture_input_enabled = .FALSE.
    ftg_update_surface_capture_output_enabled = OUTPUT_ENABLED
    ftg_update_surface_output_dir = OUTPUT_DIR
    ftg_update_surface_capture_round = 1
    
    CALL ftg_update_surface_init_for_replay('input')
    CALL ftg_update_surface_replay_input(jg, kproma, kbdim, kice, klev, ksfc_type, idx_wtr, idx_ice, idx_lnd, pdtime, pfrc, &
    &  pcfh_tile, pcfm_tile, pfac_sfc, pocu, pocv, aa, aa_btm, bb, bb_btm, pcpt_tile, pqsat_tile, ptsfc_tile, pu_stress_gbm, &
    &  pv_stress_gbm, plhflx_gbm, pshflx_gbm, pevap_gbm, pu_stress_tile, pv_stress_tile, plhflx_tile, pshflx_tile, pevap_tile, &
    &  pco2nat, nblock, lsm, alake, pu, pv, ptemp, pq, pco2, prsfl, prsfc, pssfl, pssfc, rlds, rlus, rsds, rsus, rvds_dir, &
    &  rpds_dir, rnds_dir, rvds_dif, rpds_dif, rnds_dif, ps, pcosmu0, pch_tile, pcsat, pcair, q_snocpymlt, z0m_tile, z0h_lnd, &
    &  albvisdir, albnirdir, albvisdif, albnirdif, albvisdir_tile, albnirdir_tile, albvisdif_tile, albnirdif_tile, albedo, &
    &  albedo_tile, pco2_flux_tile, ptsfc, ptsfc_rad, rsns_tile, rlns_tile, lake_ice_frc, Tsurf, T1, T2, hi, hs, Qtop, Qbot, conc, &
    &  albvisdir_ice, albvisdif_ice, albnirdir_ice, albnirdif_ice)
    CALL ftg_destroy_serializer()
    
    CALL update_surface(jg, kproma, kbdim, kice, klev, ksfc_type, idx_wtr, idx_ice, idx_lnd, pdtime, pfrc, pcfh_tile, pcfm_tile, &
    &  pfac_sfc, pocu, pocv, aa, aa_btm, bb, bb_btm, pcpt_tile, pqsat_tile, ptsfc_tile, pu_stress_gbm, pv_stress_gbm, plhflx_gbm, &
    &  pshflx_gbm, pevap_gbm, pu_stress_tile, pv_stress_tile, plhflx_tile, pshflx_tile, pevap_tile, pco2nat, nblock, lsm, alake, &
    &  pu, pv, ptemp, pq, pco2, prsfl, prsfc, pssfl, pssfc, rlds, rlus, rsds, rsus, rvds_dir, rpds_dir, rnds_dir, rvds_dif, &
    &  rpds_dif, rnds_dif, ps, pcosmu0, pch_tile, pcsat, pcair, q_snocpymlt, z0m_tile, z0h_lnd, albvisdir, albnirdir, albvisdif, &
    &  albnirdif, albvisdir_tile, albnirdir_tile, albvisdif_tile, albnirdif_tile, albedo, albedo_tile, pco2_flux_tile, ptsfc, &
    &  ptsfc_rad, rsns_tile, rlns_tile, lake_ice_frc, Tsurf, T1, T2, hi, hs, Qtop, Qbot, conc, albvisdir_ice, albvisdif_ice, &
    &  albnirdir_ice, albnirdif_ice)
    
  END SUBROUTINE ftg_test_update_surface
  
  
  SUBROUTINE ftg_update_surface_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_update_surface_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT update_surface '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
  END SUBROUTINE ftg_update_surface_init_for_replay
  
  SUBROUTINE ftg_update_surface_replay_input(jg, kproma, kbdim, kice, klev, ksfc_type, idx_wtr, idx_ice, idx_lnd, pdtime, pfrc, &
  &  pcfh_tile, pcfm_tile, pfac_sfc, pocu, pocv, aa, aa_btm, bb, bb_btm, pcpt_tile, pqsat_tile, ptsfc_tile, pu_stress_gbm, &
  &  pv_stress_gbm, plhflx_gbm, pshflx_gbm, pevap_gbm, pu_stress_tile, pv_stress_tile, plhflx_tile, pshflx_tile, pevap_tile, &
  &  pco2nat, nblock, lsm, alake, pu, pv, ptemp, pq, pco2, prsfl, prsfc, pssfl, pssfc, rlds, rlus, rsds, rsus, rvds_dir, rpds_dir, &
  &  rnds_dir, rvds_dif, rpds_dif, rnds_dif, ps, pcosmu0, pch_tile, pcsat, pcair, q_snocpymlt, z0m_tile, z0h_lnd, albvisdir, &
  &  albnirdir, albvisdif, albnirdif, albvisdir_tile, albnirdir_tile, albvisdif_tile, albnirdif_tile, albedo, albedo_tile, &
  &  pco2_flux_tile, ptsfc, ptsfc_rad, rsns_tile, rlns_tile, lake_ice_frc, Tsurf, T1, T2, hi, hs, Qtop, Qbot, conc, albvisdir_ice, &
  &  albvisdif_ice, albnirdir_ice, albnirdif_ice)
  !=================== START MANUALLY ADDED FOR FTG =====================!
  USE mo_time_config,    ONLY: set_tc_startdate, set_tc_stopdate, &
                               set_tc_dt_model, set_tc_current_date, &
                               set_tc_exp_startdate, set_tc_exp_stopdate
  USE mo_jsb_base,       ONLY: jsbach_setup_models, jsbach_setup_tiles
  USE mo_jsb_model_init, ONLY: jsbach_setup_grid, jsbach_init
  USE mo_echam_convect_tables, ONLY: init_convect_tables
  USE mo_zaxis_type, ONLY: zaxisTypeList, t_zaxisTypeList
  !===================== END MANUALLY ADDED FOR FTG =====================!
    
    INTEGER, INTENT(inout) :: jg
    INTEGER, INTENT(inout) :: kproma
    INTEGER, INTENT(inout) :: kbdim
    INTEGER, INTENT(inout) :: kice
    INTEGER, INTENT(inout) :: klev
    INTEGER, INTENT(inout) :: ksfc_type
    INTEGER, INTENT(inout) :: idx_wtr
    INTEGER, INTENT(inout) :: idx_ice
    INTEGER, INTENT(inout) :: idx_lnd
    REAL(wp), INTENT(inout) :: pdtime
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pfrc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfh_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcfm_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pfac_sfc
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocu
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pocv
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout) :: aa
    REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout) :: aa_btm
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: bb
    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: bb_btm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pcpt_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pqsat_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: ptsfc_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pu_stress_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pv_stress_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: plhflx_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pshflx_gbm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: pevap_gbm
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pu_stress_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pv_stress_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: plhflx_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pshflx_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: pevap_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pco2nat
    INTEGER, INTENT(inout), OPTIONAL :: nblock
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: lsm
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: alake
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pu
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pv
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ptemp
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pq
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pco2
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: prsfl
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: prsfc
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pssfl
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pssfc
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rlds
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rlus
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rsds
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rsus
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rvds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rpds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rnds_dir
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rvds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rpds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: rnds_dif
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ps
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pcosmu0
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pch_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pcsat
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pcair
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: q_snocpymlt
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: z0m_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: z0h_lnd
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdir
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdir
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdif
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdif
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdir_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdir_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdif_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdif_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albedo
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albedo_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pco2_flux_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ptsfc
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: ptsfc_rad
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rsns_tile
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: rlns_tile
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(inout), OPTIONAL :: lake_ice_frc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: Tsurf
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: T1
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: T2
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: hi
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: hs
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: Qtop
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: Qbot
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: conc
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdir_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albvisdif_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdir_ice
    REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: albnirdif_ice
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA update_surface (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("jg", jg)
    CALL ftg_read("kproma", kproma)
    CALL ftg_read("kbdim", kbdim)
    CALL ftg_read("kice", kice)
    CALL ftg_read("klev", klev)
    CALL ftg_read("ksfc_type", ksfc_type)
    CALL ftg_read("idx_wtr", idx_wtr)
    CALL ftg_read("idx_ice", idx_ice)
    CALL ftg_read("idx_lnd", idx_lnd)
    CALL ftg_read("pdtime", pdtime)
    CALL ftg_allocate_and_read_allocatable("pfrc", pfrc, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pcfh_tile", pcfh_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pcfm_tile", pcfm_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pfac_sfc", pfac_sfc, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pocu", pocu, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pocv", pocv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("aa", aa, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("aa_btm", aa_btm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("bb", bb, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("bb_btm", bb_btm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pcpt_tile", pcpt_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pqsat_tile", pqsat_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("ptsfc_tile", ptsfc_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pu_stress_gbm", pu_stress_gbm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pv_stress_gbm", pv_stress_gbm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("plhflx_gbm", plhflx_gbm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pshflx_gbm", pshflx_gbm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pevap_gbm", pevap_gbm, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pu_stress_tile", pu_stress_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pv_stress_tile", pv_stress_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("plhflx_tile", plhflx_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pshflx_tile", pshflx_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("pevap_tile", pevap_tile, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rvds_dir", rvds_dir, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rpds_dir", rpds_dir, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rnds_dir", rnds_dir, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rvds_dif", rvds_dif, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rpds_dif", rpds_dif, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("rnds_dif", rnds_dif, ftg_rperturb)
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(pco2nat)) THEN
      CALL ftg_allocate_and_read_allocatable("pco2nat", pco2nat, ftg_rperturb)
    END IF
    IF (PRESENT(nblock)) THEN
      CALL ftg_read("nblock", nblock)
    END IF
    IF (PRESENT(lsm)) THEN
      CALL ftg_allocate_and_read_allocatable("lsm", lsm, ftg_rperturb)
    END IF
    IF (PRESENT(alake)) THEN
      CALL ftg_allocate_and_read_allocatable("alake", alake, ftg_rperturb)
    END IF
    IF (PRESENT(pu)) THEN
      CALL ftg_allocate_and_read_allocatable("pu", pu, ftg_rperturb)
    END IF
    IF (PRESENT(pv)) THEN
      CALL ftg_allocate_and_read_allocatable("pv", pv, ftg_rperturb)
    END IF
    IF (PRESENT(ptemp)) THEN
      CALL ftg_allocate_and_read_allocatable("ptemp", ptemp, ftg_rperturb)
    END IF
    IF (PRESENT(pq)) THEN
      CALL ftg_allocate_and_read_allocatable("pq", pq, ftg_rperturb)
    END IF
    IF (PRESENT(pco2)) THEN
      CALL ftg_allocate_and_read_allocatable("pco2", pco2, ftg_rperturb)
    END IF
    IF (PRESENT(prsfl)) THEN
      CALL ftg_allocate_and_read_allocatable("prsfl", prsfl, ftg_rperturb)
    END IF
    IF (PRESENT(prsfc)) THEN
      CALL ftg_allocate_and_read_allocatable("prsfc", prsfc, ftg_rperturb)
    END IF
    IF (PRESENT(pssfl)) THEN
      CALL ftg_allocate_and_read_allocatable("pssfl", pssfl, ftg_rperturb)
    END IF
    IF (PRESENT(pssfc)) THEN
      CALL ftg_allocate_and_read_allocatable("pssfc", pssfc, ftg_rperturb)
    END IF
    IF (PRESENT(rlds)) THEN
      CALL ftg_allocate_and_read_allocatable("rlds", rlds, ftg_rperturb)
    END IF
    IF (PRESENT(rlus)) THEN
      CALL ftg_allocate_and_read_allocatable("rlus", rlus, ftg_rperturb)
    END IF
    IF (PRESENT(rsds)) THEN
      CALL ftg_allocate_and_read_allocatable("rsds", rsds, ftg_rperturb)
    END IF
    IF (PRESENT(rsus)) THEN
      CALL ftg_allocate_and_read_allocatable("rsus", rsus, ftg_rperturb)
    END IF
    IF (PRESENT(ps)) THEN
      CALL ftg_allocate_and_read_allocatable("ps", ps, ftg_rperturb)
    END IF
    IF (PRESENT(pcosmu0)) THEN
      CALL ftg_allocate_and_read_allocatable("pcosmu0", pcosmu0, ftg_rperturb)
    END IF
    IF (PRESENT(pch_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("pch_tile", pch_tile, ftg_rperturb)
    END IF
    IF (PRESENT(pcsat)) THEN
      CALL ftg_allocate_and_read_allocatable("pcsat", pcsat, ftg_rperturb)
    END IF
    IF (PRESENT(pcair)) THEN
      CALL ftg_allocate_and_read_allocatable("pcair", pcair, ftg_rperturb)
    END IF
    IF (PRESENT(q_snocpymlt)) THEN
      CALL ftg_allocate_and_read_allocatable("q_snocpymlt", q_snocpymlt, ftg_rperturb)
    END IF
    IF (PRESENT(z0m_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("z0m_tile", z0m_tile, ftg_rperturb)
    END IF
    IF (PRESENT(z0h_lnd)) THEN
      CALL ftg_allocate_and_read_allocatable("z0h_lnd", z0h_lnd, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdir)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdir", albvisdir, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdir)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdir", albnirdir, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdif)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdif", albvisdif, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdif)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdif", albnirdif, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdir_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdir_tile", albvisdir_tile, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdir_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdir_tile", albnirdir_tile, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdif_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdif_tile", albvisdif_tile, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdif_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdif_tile", albnirdif_tile, ftg_rperturb)
    END IF
    IF (PRESENT(albedo)) THEN
      CALL ftg_allocate_and_read_allocatable("albedo", albedo, ftg_rperturb)
    END IF
    IF (PRESENT(albedo_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("albedo_tile", albedo_tile, ftg_rperturb)
    END IF
    IF (PRESENT(pco2_flux_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("pco2_flux_tile", pco2_flux_tile, ftg_rperturb)
    END IF
    IF (PRESENT(ptsfc)) THEN
      CALL ftg_allocate_and_read_allocatable("ptsfc", ptsfc, ftg_rperturb)
    END IF
    IF (PRESENT(ptsfc_rad)) THEN
      CALL ftg_allocate_and_read_allocatable("ptsfc_rad", ptsfc_rad, ftg_rperturb)
    END IF
    IF (PRESENT(rsns_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("rsns_tile", rsns_tile, ftg_rperturb)
    END IF
    IF (PRESENT(rlns_tile)) THEN
      CALL ftg_allocate_and_read_allocatable("rlns_tile", rlns_tile, ftg_rperturb)
    END IF
    IF (PRESENT(lake_ice_frc)) THEN
      CALL ftg_allocate_and_read_allocatable("lake_ice_frc", lake_ice_frc, ftg_rperturb)
    END IF
    IF (PRESENT(Tsurf)) THEN
      CALL ftg_allocate_and_read_allocatable("Tsurf", Tsurf, ftg_rperturb)
    END IF
    IF (PRESENT(T1)) THEN
      CALL ftg_allocate_and_read_allocatable("T1", T1, ftg_rperturb)
    END IF
    IF (PRESENT(T2)) THEN
      CALL ftg_allocate_and_read_allocatable("T2", T2, ftg_rperturb)
    END IF
    IF (PRESENT(hi)) THEN
      CALL ftg_allocate_and_read_allocatable("hi", hi, ftg_rperturb)
    END IF
    IF (PRESENT(hs)) THEN
      CALL ftg_allocate_and_read_allocatable("hs", hs, ftg_rperturb)
    END IF
    IF (PRESENT(Qtop)) THEN
      CALL ftg_allocate_and_read_allocatable("Qtop", Qtop, ftg_rperturb)
    END IF
    IF (PRESENT(Qbot)) THEN
      CALL ftg_allocate_and_read_allocatable("Qbot", Qbot, ftg_rperturb)
    END IF
    IF (PRESENT(conc)) THEN
      CALL ftg_allocate_and_read_allocatable("conc", conc, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdir_ice)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdir_ice", albvisdir_ice, ftg_rperturb)
    END IF
    IF (PRESENT(albvisdif_ice)) THEN
      CALL ftg_allocate_and_read_allocatable("albvisdif_ice", albvisdif_ice, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdir_ice)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdir_ice", albnirdir_ice, ftg_rperturb)
    END IF
    IF (PRESENT(albnirdif_ice)) THEN
      CALL ftg_allocate_and_read_allocatable("albnirdif_ice", albnirdif_ice, ftg_rperturb)
    END IF
    
    ! TYPE MEMBERS
    
    
    ! GLOBALS
    CALL ftg_read("mo_echam_phy_memory__cdimissval", mo_echam_phy_memory__cdimissval)
    CALL ftg_read("mo_jsb_control__debug", mo_jsb_control__debug)
    CALL ftg_read("mo_psrad_orbit__declination", mo_psrad_orbit__declination)
    CALL ftg_read("mo_sea_ice_nml__hci_layer", mo_sea_ice_nml__hci_layer)
    CALL ftg_read("mo_sea_ice_nml__i_ice_albedo", mo_sea_ice_nml__i_ice_albedo)
    CALL ftg_read("mo_sea_ice_nml__i_ice_therm", mo_sea_ice_nml__i_ice_therm)
    CALL ftg_read("mo_dynamics_config__iequations", mo_dynamics_config__iequations)
    CALL ftg_read("mo_vdiff_solver__ih", mo_vdiff_solver__ih)
    CALL ftg_read("mo_vdiff_solver__imh", mo_vdiff_solver__imh)
    CALL ftg_read("mo_vdiff_solver__imqv", mo_vdiff_solver__imqv)
    CALL ftg_read("mo_vdiff_solver__imuv", mo_vdiff_solver__imuv)
    CALL ftg_read("mo_psrad_orbit__initialized", mo_psrad_orbit__initialized)
    CALL ftg_read("mo_vdiff_solver__iqv", mo_vdiff_solver__iqv)
    CALL ftg_read("mo_jsb_control__is_standalone", mo_jsb_control__is_standalone)
    CALL ftg_read("mo_vdiff_solver__iu", mo_vdiff_solver__iu)
    CALL ftg_read("mo_vdiff_solver__iv", mo_vdiff_solver__iv)
    CALL ftg_read("mo_jsb_control__l_timer", mo_jsb_control__l_timer)
    CALL ftg_allocate_and_read_pointer("lsfc_heat_flux", lsfc_heat_flux, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("lsfc_mom_flux", lsfc_mom_flux, ftg_rperturb)
    CALL ftg_read("mo_run_config__ltimer", mo_run_config__ltimer)
    ! *** WARNING: Type not supported by serialbox ***
    !     mo_master_control__master_namelist_filename
    !     CHARACTER(len=filename_max) | dimension: 0
    CALL ftg_read("mo_vdiff_solver__nmatrix", mo_vdiff_solver__nmatrix)
    CALL ftg_read("mo_parallel_config__nproma", mo_parallel_config__nproma)
    CALL ftg_read("mo_echam_sfc_indices__nsfc_type", mo_echam_sfc_indices__nsfc_type)
    CALL ftg_read("mo_vdiff_solver__nvar_vdiff", mo_vdiff_solver__nvar_vdiff)
    CALL ftg_read("mo_timer__timer_ice_fast", mo_timer__timer_ice_fast)
    CALL ftg_read("mo_timer__timer_jsbach", mo_timer__timer_jsbach)
    CALL ftg_read("mo_sea_ice_nml__use_no_flux_gradients", mo_sea_ice_nml__use_no_flux_gradients)
    CALL ftg_read("mo_jsb_test__write_interface_vars", mo_jsb_test__write_interface_vars)
    IF (ftg_field_exists("mo_read_netcdf_distributed__basic_data")) THEN
      ftg_bounds = ftg_get_bounds("mo_read_netcdf_distributed__basic_data")
      ALLOCATE(mo_read_netcdf_distributed__basic_data(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_read_netcdf_distributed__basic_data(0))
    END IF
    CALL ftg_read("mo_read_netcdf_distributed__basic_data%n_g", mo_read_netcdf_distributed__basic_data%n_g)
    CALL ftg_read("mo_echam_phy_config__echam_phy_config%lamip", mo_echam_phy_config__echam_phy_config%lamip)
    CALL ftg_read("mo_echam_phy_config__echam_phy_config%lice", mo_echam_phy_config__echam_phy_config%lice)
    CALL ftg_read("mo_echam_phy_config__echam_phy_config%ljsb", mo_echam_phy_config__echam_phy_config%ljsb)
    CALL ftg_read("mo_echam_phy_config__echam_phy_config%llake", mo_echam_phy_config__echam_phy_config%llake)
    CALL ftg_read("mo_echam_phy_config__echam_phy_config%lmlo", mo_echam_phy_config__echam_phy_config%lmlo)
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%lsfc_heat_flux", mo_echam_vdf_config__echam_vdf_config%lsfc_heat_flux)
    CALL ftg_read("mo_echam_vdf_config__echam_vdf_config%lsfc_mom_flux", mo_echam_vdf_config__echam_vdf_config%lsfc_mom_flux)
    ! *** WARNING: Type not supported by serialbox ***
    !     mo_model_domain__p_patch%grid_filename
    !     CHARACTER(LEN=filename_max) | dimension: 0
    IF (ftg_field_exists("mo_model_domain__p_patch")) THEN
      ftg_bounds = ftg_get_bounds("mo_model_domain__p_patch")
      ALLOCATE(mo_model_domain__p_patch(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_model_domain__p_patch(0))
    END IF
    CALL ftg_read("mo_model_domain__p_patch%id", mo_model_domain__p_patch%id)
    CALL ftg_read("mo_model_domain__p_patch%n_patch_cells", mo_model_domain__p_patch%n_patch_cells)
    CALL ftg_read("mo_model_domain__p_patch%n_patch_cells_g", mo_model_domain__p_patch%n_patch_cells_g)
    CALL ftg_read("mo_model_domain__p_patch%nblks_c", mo_model_domain__p_patch%nblks_c)
    CALL ftg_read("mo_read_netcdf_distributed__basic_data%io_chunk%first", mo_read_netcdf_distributed__basic_data%io_chunk%first)
    CALL ftg_read("mo_read_netcdf_distributed__basic_data%io_chunk%size", mo_read_netcdf_distributed__basic_data%io_chunk%size)
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%area'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%area, ftg_rperturb)
    END DO
    CALL ftg_read("mo_model_domain__p_patch%geometry_info%mean_characteristic_length", mo_model_domain__p_patch%geometry_info% &
    &  mean_characteristic_length)
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%center'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%cells%center(ftg_bounds(1):ftg_bounds(2), ftg_bounds(3):ftg_bounds(4)))
      ELSE
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%cells%center(0, 0))
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%center%lat'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%center%lat)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%center%lon'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%center%lon)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data)
      ELSE
        ALLOCATE(mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%basic_data_index'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%basic_data_index)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%n_pnts'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern%n_pnts)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%n_recv'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern%n_recv)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%n_send'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern%n_send)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%np_recv'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern%np_recv)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%np_send'
      CALL ftg_read(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern%np_send)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%pelist_recv'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  pelist_recv, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%pelist_send'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  pelist_send, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_count'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_count, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_dst_blk'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_dst_blk, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_dst_idx'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_dst_idx, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_limits'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_limits, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_src'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_src, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%recv_startidx'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  recv_startidx, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%send_count'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  send_count, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%send_limits'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  send_limits, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%send_src_blk'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  send_src_blk, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%send_src_idx'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  send_src_idx, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_model_domain__p_patch, 1), UBOUND(mo_model_domain__p_patch, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_model_domain__p_patch(', ftg_d1, ')%cells%dist_io_data%redistrib_pattern%send_startidx'
      CALL ftg_allocate_and_read_allocatable(ftg_c, mo_model_domain__p_patch(ftg_d1)%cells%dist_io_data%redistrib_pattern% &
      &  send_startidx, ftg_rperturb)
    END DO
    
    !===================== START MANUALLY ADDED FOR FTG ===================!
    mo_run_config__ltimer = .FALSE.
    CALL init_convect_tables()
    mo_master_control__master_namelist_filename = "icon_master.namelist"
    mo_model_domain__p_patch%grid_filename = 'icon_grid_0005_R02B04_G.nc'
    CALL set_tc_startdate('1979-01-01T00:00:00Z')
    CALL set_tc_stopdate('1979-01-01T00:16:00Z')
    CALL set_tc_exp_startdate('1979-01-01T00:00:00Z')
    CALL set_tc_exp_stopdate('1979-01-01T00:16:00Z')
    CALL set_tc_current_date('1979-01-01T00:00:00Z')
    CALL set_tc_dt_model('PT4M')

    ! Do basic initialization of JSBACH
    zaxisTypeList = t_zaxisTypeList()
    CALL jsbach_setup_models(mo_master_control__master_namelist_filename)
    mo_jsb_control__l_timer = .FALSE.

    ! Now continue initialization of JSBACH for the different grids
    IF (mo_echam_phy_config__echam_phy_config(jg)%ljsb) THEN 
      CALL jsbach_setup_grid(jg, mo_model_domain__p_patch(jg))
      CALL jsbach_setup_tiles(jg)
    END IF
    CALL jsbach_init(jg)
    CALL set_tc_current_date('1979-01-01T00:04:00Z')
    !===================== END MANUALLY ADDED FOR FTG =====================!
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_update_surface_replay_input
  
END PROGRAM ftg_update_surface_test



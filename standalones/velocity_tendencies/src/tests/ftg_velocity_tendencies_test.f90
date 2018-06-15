
PROGRAM ftg_velocity_tendencies_test
  
  USE mtime
  USE mo_kind
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_velocity_advection, ONLY: velocity_tendencies, ftg_velocity_tendencies_capture_input_enabled, &
  &  ftg_velocity_tendencies_capture_output_enabled, ftg_velocity_tendencies_capture_round, ftg_velocity_tendencies_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_run_config, ONLY: mo_run_config__timers_level => timers_level, mo_run_config__ltimer => ltimer, mo_run_config__lvert_nest &
  &  => lvert_nest
  USE mo_init_vgrid, ONLY: mo_init_vgrid__nflatlev => nflatlev
  USE mo_nonhydrostatic_config, ONLY: mo_nonhydrostatic_config__lextra_diffu => lextra_diffu
  USE mo_timer, ONLY: mo_timer__timer_solve_nh_veltend => timer_solve_nh_veltend, mo_timer__timer_intp => timer_intp
  USE mo_vertical_grid, ONLY: mo_vertical_grid__nrdmax => nrdmax
  USE mo_parallel_config, ONLY: mo_parallel_config__nproma => nproma
  
  USE mo_nonhydro_types, ONLY: t_nh_metrics, t_nh_diag, t_nh_prog
  USE mo_model_domain, ONLY: t_patch
  USE mo_intp_data_strc, ONLY: t_int_state
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = ++FTGPERTURB++
  
  CALL start_mpi('ftg_velocity_tendencies_test') !ICON
  
  CALL ftg_test_velocity_tendencies()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_velocity_tendencies()
    
    TYPE(t_nh_prog) :: p_prog
    TYPE(t_patch) :: p_patch
    TYPE(t_int_state) :: p_int
    TYPE(t_nh_metrics) :: p_metrics
    TYPE(t_nh_diag) :: p_diag
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_w_concorr_me
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_kin_hor_e
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_vt_ie
    INTEGER :: ntnd
    INTEGER :: istep
    LOGICAL :: lvn_only
    REAL(wp) :: dtime
    
    CALL message('FTG', '*** Run test for velocity_tendencies ***')
    
    ftg_velocity_tendencies_capture_input_enabled = .FALSE.
    ftg_velocity_tendencies_capture_output_enabled = OUTPUT_ENABLED
    ftg_velocity_tendencies_output_dir = OUTPUT_DIR
    ftg_velocity_tendencies_capture_round = 1
    
    CALL ftg_velocity_tendencies_init_for_replay('input')
    CALL ftg_velocity_tendencies_replay_input(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, &
    &  ntnd, istep, lvn_only, dtime)
    CALL ftg_destroy_serializer()
    
    CALL velocity_tendencies(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, ntnd, istep, &
    &  lvn_only, dtime)
    
  END SUBROUTINE ftg_test_velocity_tendencies
  
  
  SUBROUTINE ftg_velocity_tendencies_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_velocity_tendencies_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT velocity_tendencies '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
  END SUBROUTINE ftg_velocity_tendencies_init_for_replay
  
  SUBROUTINE ftg_velocity_tendencies_replay_input(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, &
  &  ntnd, istep, lvn_only, dtime)
    
    TYPE(t_nh_prog), INTENT(inout) :: p_prog
    TYPE(t_patch), INTENT(inout) :: p_patch
    TYPE(t_int_state), INTENT(inout) :: p_int
    TYPE(t_nh_metrics), INTENT(inout) :: p_metrics
    TYPE(t_nh_diag), INTENT(inout) :: p_diag
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_w_concorr_me
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_kin_hor_e
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_vt_ie
    INTEGER, INTENT(inout) :: ntnd
    INTEGER, INTENT(inout) :: istep
    LOGICAL, INTENT(inout) :: lvn_only
    REAL(wp), INTENT(inout) :: dtime
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA velocity_tendencies (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_allocate_and_read_allocatable("z_w_concorr_me", z_w_concorr_me, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("z_kin_hor_e", z_kin_hor_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("z_vt_ie", z_vt_ie, ftg_rperturb)
    CALL ftg_read("ntnd", ntnd)
    CALL ftg_read("istep", istep)
    CALL ftg_read("lvn_only", lvn_only)
    CALL ftg_read("dtime", dtime)
    
    ! OPTIONAL ARGUMENTS
    
    ! TYPE MEMBERS
    CALL ftg_allocate_and_read_pointer("p_prog%vn", p_prog%vn, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_prog%w", p_prog%w, ftg_rperturb)
    CALL ftg_read("p_patch%id", p_patch%id)
    CALL ftg_read("p_patch%n_childdom", p_patch%n_childdom)
    CALL ftg_read("p_patch%nblks_c", p_patch%nblks_c)
    CALL ftg_read("p_patch%nblks_e", p_patch%nblks_e)
    CALL ftg_read("p_patch%nblks_v", p_patch%nblks_v)
    CALL ftg_read("p_patch%nlev", p_patch%nlev)
    CALL ftg_read("p_patch%nlevp1", p_patch%nlevp1)
    CALL ftg_read("p_patch%nshift", p_patch%nshift)
    CALL ftg_allocate_and_read_pointer("p_patch%cells%area", p_patch%cells%area, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_blk", p_patch%cells%edge_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_idx", p_patch%cells%edge_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_block", p_patch%cells%end_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_index", p_patch%cells%end_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_blk", p_patch%cells%neighbor_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_idx", p_patch%cells%neighbor_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_block", p_patch%cells%start_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_index", p_patch%cells%start_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%area_edge", p_patch%edges%area_edge, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_blk", p_patch%edges%cell_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_idx", p_patch%edges%cell_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_block", p_patch%edges%end_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_index", p_patch%edges%end_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%f_e", p_patch%edges%f_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_dual_edge_length", p_patch%edges%inv_dual_edge_length, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_primal_edge_length", p_patch%edges%inv_primal_edge_length, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_blk", p_patch%edges%quad_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_idx", p_patch%edges%quad_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_block", p_patch%edges%start_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_index", p_patch%edges%start_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%tangent_orientation", p_patch%edges%tangent_orientation, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_blk", p_patch%edges%vertex_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_idx", p_patch%edges%vertex_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%cell_blk", p_patch%verts%cell_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%cell_idx", p_patch%verts%cell_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%edge_blk", p_patch%verts%edge_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%edge_idx", p_patch%verts%edge_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_blk", p_patch%verts%end_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_block", p_patch%verts%end_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_index", p_patch%verts%end_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_blk", p_patch%verts%start_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_block", p_patch%verts%start_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_index", p_patch%verts%start_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%decomp_info%owner_mask", p_patch%cells%decomp_info%owner_mask, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%c_lin_e", p_int%c_lin_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%cells_aw_verts", p_int%cells_aw_verts, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%e_bln_c_s", p_int%e_bln_c_s, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_grdiv", p_int%geofac_grdiv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_n2s", p_int%geofac_n2s, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_rot", p_int%geofac_rot, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%rbf_vec_coeff_e", p_int%rbf_vec_coeff_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff1_dwdz", p_metrics%coeff1_dwdz, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff2_dwdz", p_metrics%coeff2_dwdz, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff_gradekin", p_metrics%coeff_gradekin, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddqz_z_full_e", p_metrics%ddqz_z_full_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddqz_z_half", p_metrics%ddqz_z_half, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddxn_z_full", p_metrics%ddxn_z_full, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddxt_z_full", p_metrics%ddxt_z_full, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfac_c", p_metrics%wgtfac_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfac_e", p_metrics%wgtfac_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfacq_e", p_metrics%wgtfacq_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_diag%ddt_vn_adv", p_diag%ddt_vn_adv, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_diag%ddt_w_adv", p_diag%ddt_w_adv, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_diag%dvn_ie_ubc", p_diag%dvn_ie_ubc, ftg_rperturb)
    CALL ftg_read("p_diag%max_vcfl_dyn", p_diag%max_vcfl_dyn)
    CALL ftg_allocate_and_read_pointer("p_diag%vn_ie", p_diag%vn_ie, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_diag%vt", p_diag%vt, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_diag%w_concorr_c", p_diag%w_concorr_c, ftg_rperturb)
    
    
    ! GLOBALS
    CALL ftg_read("mo_nonhydrostatic_config__lextra_diffu", mo_nonhydrostatic_config__lextra_diffu)
    CALL ftg_read("mo_run_config__ltimer", mo_run_config__ltimer)
    CALL ftg_read("mo_run_config__lvert_nest", mo_run_config__lvert_nest)
    CALL ftg_read("mo_init_vgrid__nflatlev", mo_init_vgrid__nflatlev)
    CALL ftg_read("mo_parallel_config__nproma", mo_parallel_config__nproma)
    CALL ftg_read("mo_vertical_grid__nrdmax", mo_vertical_grid__nrdmax)
    CALL ftg_read("mo_timer__timer_intp", mo_timer__timer_intp)
    CALL ftg_read("mo_timer__timer_solve_nh_veltend", mo_timer__timer_solve_nh_veltend)
    CALL ftg_read("mo_run_config__timers_level", mo_run_config__timers_level)
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_velocity_tendencies_replay_input
  
END PROGRAM ftg_velocity_tendencies_test



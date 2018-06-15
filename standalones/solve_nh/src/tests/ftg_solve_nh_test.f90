
PROGRAM ftg_solve_nh_test
  
  USE mtime
  USE mo_kind
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_exception,      ONLY: message
  USE mo_mpi,            ONLY: get_my_mpi_all_id, start_mpi, stop_mpi !ICON
  
  USE mo_solve_nonhydro, ONLY: solve_nh, ftg_solve_nh_capture_input_enabled, ftg_solve_nh_capture_output_enabled, &
  &  ftg_solve_nh_capture_round, ftg_solve_nh_output_dir
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  
  USE mo_dynamics_config, ONLY: mo_dynamics_config__idiv_method => idiv_method
  USE mo_run_config, ONLY: mo_run_config__timers_level => timers_level, mo_run_config__ltimer => ltimer, mo_run_config__lvert_nest &
  &  => lvert_nest
  USE mo_initicon_config, ONLY: mo_initicon_config__is_iau_active => is_iau_active, mo_initicon_config__iau_wgt_dyn => iau_wgt_dyn
  USE mo_sync, ONLY: mo_sync__log_unit => log_unit, mo_sync__do_sync_checks => do_sync_checks
  USE mo_init_vgrid, ONLY: mo_init_vgrid__nflatlev => nflatlev
  USE mo_vertical_coord_table, ONLY: mo_vertical_coord_table__vct_a => vct_a
  USE mo_interpol_config, ONLY: mo_interpol_config__lsq_high_set => lsq_high_set, mo_interpol_config__nudge_max_coeff => &
  &  nudge_max_coeff, mo_interpol_config__lsq_high_ord => lsq_high_ord, t_lsq_set
  USE mo_nonhydrostatic_config, ONLY: mo_nonhydrostatic_config__divdamp_type => divdamp_type, &
  &  mo_nonhydrostatic_config__veladv_offctr => veladv_offctr, mo_nonhydrostatic_config__l_open_ubc => l_open_ubc, &
  &  mo_nonhydrostatic_config__ndyn_substeps_var => ndyn_substeps_var, mo_nonhydrostatic_config__itime_scheme => itime_scheme, &
  &  mo_nonhydrostatic_config__kstart_moist => kstart_moist, mo_nonhydrostatic_config__kstart_dd3d => kstart_dd3d, &
  &  mo_nonhydrostatic_config__divdamp_order => divdamp_order, mo_nonhydrostatic_config__divdamp_fac_o2 => divdamp_fac_o2, &
  &  mo_nonhydrostatic_config__rhotheta_offctr => rhotheta_offctr, mo_nonhydrostatic_config__iadv_rhotheta => iadv_rhotheta, &
  &  mo_nonhydrostatic_config__lextra_diffu => lextra_diffu, mo_nonhydrostatic_config__rayleigh_type => rayleigh_type, &
  &  mo_nonhydrostatic_config__lhdiff_rcf => lhdiff_rcf, mo_nonhydrostatic_config__igradp_method => igradp_method, &
  &  mo_nonhydrostatic_config__divdamp_fac => divdamp_fac
  USE mo_timer, ONLY: mo_timer__timer_exch_data_wait => timer_exch_data_wait, mo_timer__timer_icon_comm_wait => &
  &  timer_icon_comm_wait, mo_timer__timer_icon_comm_fillrecv => timer_icon_comm_fillrecv, mo_timer__timer_icon_comm_fillsend => &
  &  timer_icon_comm_fillsend, mo_timer__timer_solve_nh_vnupd => timer_solve_nh_vnupd, mo_timer__timer_icon_comm_ircv => &
  &  timer_icon_comm_ircv, mo_timer__timer_icon_comm_sync => timer_icon_comm_sync, mo_timer__timer_icon_comm_barrier_2 => &
  &  timer_icon_comm_barrier_2, mo_timer__timer_solve_nh_cellcomp => timer_solve_nh_cellcomp, mo_timer__timer_exch_data => &
  &  timer_exch_data, mo_timer__timer_back_traj => timer_back_traj, mo_timer__timer_solve_nh_exch => timer_solve_nh_exch, &
  &  mo_timer__timer_solve_nh => timer_solve_nh, mo_timer__timer_intp => timer_intp, mo_timer__timer_barrier => timer_barrier, &
  &  mo_timer__timer_solve_nh_edgecomp => timer_solve_nh_edgecomp, mo_timer__timer_solve_nh_veltend => timer_solve_nh_veltend, &
  &  mo_timer__timer_solve_nh_vimpl => timer_solve_nh_vimpl
  USE mo_advection_config, ONLY: mo_advection_config__zeta => zeta, mo_advection_config__advection_config => advection_config, &
  &  mo_advection_config__eta => eta, mo_advection_config__wgt_zeta => wgt_zeta, mo_advection_config__shape_func => shape_func, &
  &  mo_advection_config__wgt_eta => wgt_eta, t_advection_config
  USE mo_icon_comm_lib, ONLY: mo_icon_comm_lib__recv_procs_buffer => recv_procs_buffer, mo_icon_comm_lib__send_buffer => &
  &  send_buffer, mo_icon_comm_lib__this_is_mpi_sequential => this_is_mpi_sequential, mo_icon_comm_lib__comm_variable => &
  &  comm_variable, mo_icon_comm_lib__active_comm_variables => active_comm_variables, mo_icon_comm_lib__log_file_id => &
  &  log_file_id, mo_icon_comm_lib__comm_lib_is_initialized => comm_lib_is_initialized, mo_icon_comm_lib__send_procs_buffer => &
  &  send_procs_buffer, mo_icon_comm_lib__recv_buffer => recv_buffer, mo_icon_comm_lib__max_send_buffer_size => &
  &  max_send_buffer_size, mo_icon_comm_lib__active_recv_buffers => active_recv_buffers, mo_icon_comm_lib__active_send_buffers => &
  &  active_send_buffers, mo_icon_comm_lib__buffer_comm_status => buffer_comm_status, mo_icon_comm_lib__my_work_communicator => &
  &  my_work_communicator, mo_icon_comm_lib__max_active_comm_variables => max_active_comm_variables, &
  &  mo_icon_comm_lib__grid_comm_pattern_list => grid_comm_pattern_list, t_comm_process_buffer, t_comm_variable_real, &
  &  t_grid_comm_pattern
  USE mo_vertical_grid, ONLY: mo_vertical_grid__nflat_gradp => nflat_gradp, mo_vertical_grid__nrdmax => nrdmax
  USE mo_parallel_config, ONLY: mo_parallel_config__icon_comm_debug => icon_comm_debug, mo_parallel_config__n_ghost_rows => &
  &  n_ghost_rows, mo_parallel_config__iorder_sendrecv => iorder_sendrecv, mo_parallel_config__sync_barrier_mode => &
  &  sync_barrier_mode, mo_parallel_config__p_test_run => p_test_run, mo_parallel_config__nproma => nproma, &
  &  mo_parallel_config__max_mpi_message_size => max_mpi_message_size, mo_parallel_config__max_no_of_comm_variables => &
  &  max_no_of_comm_variables, mo_parallel_config__itype_comm => itype_comm, mo_parallel_config__use_dycore_barrier => &
  &  use_dycore_barrier, mo_parallel_config__icon_comm_method => icon_comm_method, mo_parallel_config__l_log_checks => &
  &  l_log_checks, mo_parallel_config__itype_exch_barrier => itype_exch_barrier, mo_parallel_config__use_icon_comm => use_icon_comm
  USE mo_grid_config, ONLY: mo_grid_config__l_limited_area => l_limited_area
  USE mo_gridref_config, ONLY: mo_gridref_config__grf_intmethod_e => grf_intmethod_e
  USE mo_advection_utils, ONLY: mo_advection_utils__ptr_delp_mc_new => ptr_delp_mc_new, mo_advection_utils__ptr_delp_mc_now => &
  &  ptr_delp_mc_now
  
  USE mo_interpol_config, ONLY: t_lsq_set
  USE mo_nh_prepadv_types, ONLY: t_prepare_adv
  USE mo_nonhydro_types, ONLY: t_nh_state
  USE mo_model_domain, ONLY: t_patch
  USE mo_advection_config, ONLY: t_advection_config
  USE mo_icon_comm_lib, ONLY: t_grid_comm_pattern, t_comm_variable_real, t_comm_process_buffer
  USE mo_intp_data_strc, ONLY: t_int_state
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  '++FTGDATADIR++/data/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  '++FTGDATADIR++/data/output_test'
  LOGICAL, PARAMETER :: OUTPUT_ENABLED = .TRUE.
  LOGICAL, PARAMETER :: SERIALBOX_DEBUG = .FALSE.
  REAL, PARAMETER :: ftg_rperturb = 0.0
  
  CALL start_mpi('ftg_solve_nh_test') !ICON
  
  CALL ftg_test_solve_nh()
  
  CALL stop_mpi() !ICON
  
CONTAINS
  
  SUBROUTINE ftg_test_solve_nh()
    
    TYPE(t_nh_state) :: p_nh
    TYPE(t_patch) :: p_patch
    TYPE(t_int_state) :: p_int
    TYPE(t_prepare_adv) :: prep_adv
    INTEGER :: nnow
    INTEGER :: nnew
    LOGICAL :: l_init
    LOGICAL :: l_recompute
    LOGICAL :: lsave_mflx
    LOGICAL :: lprep_adv
    LOGICAL :: lclean_mflx
    INTEGER :: idyn_timestep
    INTEGER :: jstep
    LOGICAL :: l_bdy_nudge
    REAL(wp) :: dtime
    
    CALL message('FTG', '*** Run test for solve_nh ***')
    
    ftg_solve_nh_capture_input_enabled = .FALSE.
    ftg_solve_nh_capture_output_enabled = OUTPUT_ENABLED
    ftg_solve_nh_output_dir = OUTPUT_DIR
    ftg_solve_nh_capture_round = 1
    
    CALL ftg_solve_nh_init_for_replay('input')
    CALL ftg_solve_nh_replay_input(p_nh, p_patch, p_int, prep_adv, nnow, nnew, l_init, l_recompute, lsave_mflx, lprep_adv, &
    &  lclean_mflx, idyn_timestep, jstep, l_bdy_nudge, dtime)
    CALL ftg_destroy_serializer()
    
    CALL solve_nh(p_nh, p_patch, p_int, prep_adv, nnow, nnew, l_init, l_recompute, lsave_mflx, lprep_adv, lclean_mflx, &
    &  idyn_timestep, jstep, l_bdy_nudge, dtime)
    
  END SUBROUTINE ftg_test_solve_nh
  
  
  SUBROUTINE ftg_solve_nh_init_for_replay(stage)
    
    CHARACTER(*), INTENT(IN) :: stage
    
    CHARACTER(len=MAX_CHAR_LENGTH) :: basename
    
    WRITE (basename,'(a,a,a,i1)') 'ftg_solve_nh_', TRIM(stage), '_', get_my_mpi_all_id()
    
    WRITE (0,*) 'FTG INIT solve_nh '//TRIM(basename)
    CALL ftg_set_serializer(TRIM(INPUT_DIR), TRIM(basename), 'r')
    IF (SERIALBOX_DEBUG) THEN
      CALL ftg_print_serializer_debuginfo()
    END IF
    
  END SUBROUTINE ftg_solve_nh_init_for_replay
  
  SUBROUTINE ftg_solve_nh_replay_input(p_nh, p_patch, p_int, prep_adv, nnow, nnew, l_init, l_recompute, lsave_mflx, lprep_adv, &
  &  lclean_mflx, idyn_timestep, jstep, l_bdy_nudge, dtime)
    
    TYPE(t_nh_state), INTENT(inout) :: p_nh
    TYPE(t_patch), INTENT(inout) :: p_patch
    TYPE(t_int_state), INTENT(inout) :: p_int
    TYPE(t_prepare_adv), INTENT(inout) :: prep_adv
    INTEGER, INTENT(inout) :: nnow
    INTEGER, INTENT(inout) :: nnew
    LOGICAL, INTENT(inout) :: l_init
    LOGICAL, INTENT(inout) :: l_recompute
    LOGICAL, INTENT(inout) :: lsave_mflx
    LOGICAL, INTENT(inout) :: lprep_adv
    LOGICAL, INTENT(inout) :: lclean_mflx
    INTEGER, INTENT(inout) :: idyn_timestep
    INTEGER, INTENT(inout) :: jstep
    LOGICAL, INTENT(inout) :: l_bdy_nudge
    REAL(wp), INTENT(inout) :: dtime
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
    CHARACTER(len=256) :: ftg_c
    INTEGER ftg_mtime_calendar
    
    CALL ftg_set_savepoint('input')
    
    WRITE (0,'(a,i1,a)') 'FTG READ INPUT DATA solve_nh (', get_my_mpi_all_id(), ')'
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! BASIC ARGUMENTS
    CALL ftg_read("nnow", nnow)
    CALL ftg_read("nnew", nnew)
    CALL ftg_read("l_init", l_init)
    CALL ftg_read("l_recompute", l_recompute)
    CALL ftg_read("lsave_mflx", lsave_mflx)
    CALL ftg_read("lprep_adv", lprep_adv)
    CALL ftg_read("lclean_mflx", lclean_mflx)
    CALL ftg_read("idyn_timestep", idyn_timestep)
    CALL ftg_read("jstep", jstep)
    CALL ftg_read("l_bdy_nudge", l_bdy_nudge)
    CALL ftg_read("dtime", dtime)
    
    ! OPTIONAL ARGUMENTS
    
    ! TYPE MEMBERS
    CALL ftg_allocate_and_read_pointer("p_nh%diag%ddt_exner_phy", p_nh%diag%ddt_exner_phy, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%ddt_vn_adv", p_nh%diag%ddt_vn_adv, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%ddt_vn_phy", p_nh%diag%ddt_vn_phy, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%ddt_w_adv", p_nh%diag%ddt_w_adv, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dtheta_v_ic_int", p_nh%diag%dtheta_v_ic_int, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dtheta_v_ic_ubc", p_nh%diag%dtheta_v_ic_ubc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dvn_ie_int", p_nh%diag%dvn_ie_int, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dvn_ie_ubc", p_nh%diag%dvn_ie_ubc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dw_int", p_nh%diag%dw_int, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%dw_ubc", p_nh%diag%dw_ubc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%exner_dyn_incr", p_nh%diag%exner_dyn_incr, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%exner_incr", p_nh%diag%exner_incr, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%exner_pr", p_nh%diag%exner_pr, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_bdy_mflx", p_nh%diag%grf_bdy_mflx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_tend_mflx", p_nh%diag%grf_tend_mflx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_tend_rho", p_nh%diag%grf_tend_rho, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_tend_thv", p_nh%diag%grf_tend_thv, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_tend_vn", p_nh%diag%grf_tend_vn, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%grf_tend_w", p_nh%diag%grf_tend_w, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%mass_fl_e", p_nh%diag%mass_fl_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%mass_fl_e_sv", p_nh%diag%mass_fl_e_sv, ftg_rperturb)
    CALL ftg_read("p_nh%diag%max_vcfl_dyn", p_nh%diag%max_vcfl_dyn)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%mflx_ic_int", p_nh%diag%mflx_ic_int, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%mflx_ic_ubc", p_nh%diag%mflx_ic_ubc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%rho_ic", p_nh%diag%rho_ic, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%rho_incr", p_nh%diag%rho_incr, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%theta_v_ic", p_nh%diag%theta_v_ic, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%vn_ie", p_nh%diag%vn_ie, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%vn_incr", p_nh%diag%vn_incr, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%vt", p_nh%diag%vt, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%diag%w_concorr_c", p_nh%diag%w_concorr_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%bdy_halo_c_blk", p_nh%metrics%bdy_halo_c_blk, ftg_rperturb)
    CALL ftg_read("p_nh%metrics%bdy_halo_c_dim", p_nh%metrics%bdy_halo_c_dim)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%bdy_halo_c_idx", p_nh%metrics%bdy_halo_c_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%bdy_mflx_e_blk", p_nh%metrics%bdy_mflx_e_blk, ftg_rperturb)
    CALL ftg_read("p_nh%metrics%bdy_mflx_e_dim", p_nh%metrics%bdy_mflx_e_dim)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%bdy_mflx_e_idx", p_nh%metrics%bdy_mflx_e_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%coeff1_dwdz", p_nh%metrics%coeff1_dwdz, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%coeff2_dwdz", p_nh%metrics%coeff2_dwdz, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%coeff_gradekin", p_nh%metrics%coeff_gradekin, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%coeff_gradp", p_nh%metrics%coeff_gradp, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%d2dexdz2_fac1_mc", p_nh%metrics%d2dexdz2_fac1_mc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%d2dexdz2_fac2_mc", p_nh%metrics%d2dexdz2_fac2_mc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%d_exner_dz_ref_ic", p_nh%metrics%d_exner_dz_ref_ic, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%ddqz_z_full_e", p_nh%metrics%ddqz_z_full_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%ddqz_z_half", p_nh%metrics%ddqz_z_half, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%ddxn_z_full", p_nh%metrics%ddxn_z_full, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%ddxt_z_full", p_nh%metrics%ddxt_z_full, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%exner_exfac", p_nh%metrics%exner_exfac, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%exner_ref_mc", p_nh%metrics%exner_ref_mc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%hmask_dd3d", p_nh%metrics%hmask_dd3d, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%inv_ddqz_z_full", p_nh%metrics%inv_ddqz_z_full, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%mask_prog_halo_c", p_nh%metrics%mask_prog_halo_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%nudge_e_blk", p_nh%metrics%nudge_e_blk, ftg_rperturb)
    CALL ftg_read("p_nh%metrics%nudge_e_dim", p_nh%metrics%nudge_e_dim)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%nudge_e_idx", p_nh%metrics%nudge_e_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%pg_edgeblk", p_nh%metrics%pg_edgeblk, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%pg_edgeidx", p_nh%metrics%pg_edgeidx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%pg_exdist", p_nh%metrics%pg_exdist, ftg_rperturb)
    CALL ftg_read("p_nh%metrics%pg_listdim", p_nh%metrics%pg_listdim)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%pg_vertidx", p_nh%metrics%pg_vertidx, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%rayleigh_vn", p_nh%metrics%rayleigh_vn, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%rayleigh_w", p_nh%metrics%rayleigh_w, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%rho_ref_mc", p_nh%metrics%rho_ref_mc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%rho_ref_me", p_nh%metrics%rho_ref_me, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%scalfac_dd3d", p_nh%metrics%scalfac_dd3d, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%theta_ref_ic", p_nh%metrics%theta_ref_ic, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%theta_ref_mc", p_nh%metrics%theta_ref_mc, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%theta_ref_me", p_nh%metrics%theta_ref_me, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%vertidx_gradp", p_nh%metrics%vertidx_gradp, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%vwind_expl_wgt", p_nh%metrics%vwind_expl_wgt, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%vwind_impl_wgt", p_nh%metrics%vwind_impl_wgt, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%wgtfac_c", p_nh%metrics%wgtfac_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%wgtfac_e", p_nh%metrics%wgtfac_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%wgtfacq1_c", p_nh%metrics%wgtfacq1_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%wgtfacq_c", p_nh%metrics%wgtfacq_c, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%wgtfacq_e", p_nh%metrics%wgtfacq_e, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%metrics%zdiff_gradp", p_nh%metrics%zdiff_gradp, ftg_rperturb)
    
    ftg_c = "p_nh%prog"
    IF (ftg_field_exists(ftg_c)) THEN
      ftg_bounds = ftg_get_bounds(ftg_c)
      ALLOCATE(p_nh%prog(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(p_nh%prog(0))
    END IF
    DO ftg_d1 = LBOUND(p_nh%prog, 1), UBOUND(p_nh%prog, 1)
      WRITE (ftg_c,'(A,I0,A)') 'p_nh%prog(', ftg_d1, ')%exner'
      CALL ftg_allocate_and_read_pointer(ftg_c, p_nh%prog(ftg_d1)%exner, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(p_nh%prog, 1), UBOUND(p_nh%prog, 1)
      WRITE (ftg_c,'(A,I0,A)') 'p_nh%prog(', ftg_d1, ')%rho'
      CALL ftg_allocate_and_read_pointer(ftg_c, p_nh%prog(ftg_d1)%rho, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(p_nh%prog, 1), UBOUND(p_nh%prog, 1)
      WRITE (ftg_c,'(A,I0,A)') 'p_nh%prog(', ftg_d1, ')%theta_v'
      CALL ftg_allocate_and_read_pointer(ftg_c, p_nh%prog(ftg_d1)%theta_v, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(p_nh%prog, 1), UBOUND(p_nh%prog, 1)
      WRITE (ftg_c,'(A,I0,A)') 'p_nh%prog(', ftg_d1, ')%vn'
      CALL ftg_allocate_and_read_pointer(ftg_c, p_nh%prog(ftg_d1)%vn, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(p_nh%prog, 1), UBOUND(p_nh%prog, 1)
      WRITE (ftg_c,'(A,I0,A)') 'p_nh%prog(', ftg_d1, ')%w'
      CALL ftg_allocate_and_read_pointer(ftg_c, p_nh%prog(ftg_d1)%w, ftg_rperturb)
    END DO
    CALL ftg_allocate_and_read_pointer("p_nh%ref%vn_ref", p_nh%ref%vn_ref, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_nh%ref%w_ref", p_nh%ref%w_ref, ftg_rperturb)
    CALL ftg_read("p_patch%id", p_patch%id)
    CALL ftg_read("p_patch%n_childdom", p_patch%n_childdom)
    CALL ftg_read("p_patch%n_patch_cells", p_patch%n_patch_cells)
    CALL ftg_read("p_patch%n_patch_cells_g", p_patch%n_patch_cells_g)
    CALL ftg_read("p_patch%n_patch_edges", p_patch%n_patch_edges)
    CALL ftg_read("p_patch%n_patch_edges_g", p_patch%n_patch_edges_g)
    CALL ftg_read("p_patch%n_patch_verts", p_patch%n_patch_verts)
    CALL ftg_read("p_patch%n_patch_verts_g", p_patch%n_patch_verts_g)
    CALL ftg_read("p_patch%nblks_c", p_patch%nblks_c)
    CALL ftg_read("p_patch%nblks_e", p_patch%nblks_e)
    CALL ftg_read("p_patch%nblks_v", p_patch%nblks_v)
    CALL ftg_read("p_patch%nlev", p_patch%nlev)
    CALL ftg_read("p_patch%nlevp1", p_patch%nlevp1)
    CALL ftg_read("p_patch%nshift", p_patch%nshift)
    CALL ftg_read("p_patch%nshift_child", p_patch%nshift_child)
    CALL ftg_read("p_patch%nshift_total", p_patch%nshift_total)
    CALL ftg_read("p_patch%sync_cells_not_owned", p_patch%sync_cells_not_owned)
    CALL ftg_read("p_patch%sync_edges_not_owned", p_patch%sync_edges_not_owned)
    CALL ftg_allocate_and_read_pointer("p_patch%cells%area", p_patch%cells%area, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_blk", p_patch%cells%edge_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_idx", p_patch%cells%edge_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_blk", p_patch%cells%end_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_block", p_patch%cells%end_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_index", p_patch%cells%end_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_blk", p_patch%cells%neighbor_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_idx", p_patch%cells%neighbor_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%refin_ctrl", p_patch%cells%refin_ctrl, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_blk", p_patch%cells%start_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_block", p_patch%cells%start_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_index", p_patch%cells%start_index, ftg_rperturb)
    CALL ftg_read("p_patch%comm_pat_c%n_pnts", p_patch%comm_pat_c%n_pnts)
    CALL ftg_read("p_patch%comm_pat_c%n_recv", p_patch%comm_pat_c%n_recv)
    CALL ftg_read("p_patch%comm_pat_c%n_send", p_patch%comm_pat_c%n_send)
    CALL ftg_read("p_patch%comm_pat_c%np_recv", p_patch%comm_pat_c%np_recv)
    CALL ftg_read("p_patch%comm_pat_c%np_send", p_patch%comm_pat_c%np_send)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%pelist_recv", p_patch%comm_pat_c%pelist_recv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%pelist_send", p_patch%comm_pat_c%pelist_send, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_count", p_patch%comm_pat_c%recv_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_dst_blk", p_patch%comm_pat_c%recv_dst_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_dst_idx", p_patch%comm_pat_c%recv_dst_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_limits", p_patch%comm_pat_c%recv_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_src", p_patch%comm_pat_c%recv_src, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%recv_startidx", p_patch%comm_pat_c%recv_startidx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%send_count", p_patch%comm_pat_c%send_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%send_limits", p_patch%comm_pat_c%send_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%send_src_blk", p_patch%comm_pat_c%send_src_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%send_src_idx", p_patch%comm_pat_c%send_src_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c%send_startidx", p_patch%comm_pat_c%send_startidx, ftg_rperturb)
    CALL ftg_read("p_patch%comm_pat_c1%n_pnts", p_patch%comm_pat_c1%n_pnts)
    CALL ftg_read("p_patch%comm_pat_c1%n_recv", p_patch%comm_pat_c1%n_recv)
    CALL ftg_read("p_patch%comm_pat_c1%n_send", p_patch%comm_pat_c1%n_send)
    CALL ftg_read("p_patch%comm_pat_c1%np_recv", p_patch%comm_pat_c1%np_recv)
    CALL ftg_read("p_patch%comm_pat_c1%np_send", p_patch%comm_pat_c1%np_send)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%pelist_recv", p_patch%comm_pat_c1%pelist_recv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%pelist_send", p_patch%comm_pat_c1%pelist_send, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_count", p_patch%comm_pat_c1%recv_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_dst_blk", p_patch%comm_pat_c1%recv_dst_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_dst_idx", p_patch%comm_pat_c1%recv_dst_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_limits", p_patch%comm_pat_c1%recv_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_src", p_patch%comm_pat_c1%recv_src, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%recv_startidx", p_patch%comm_pat_c1%recv_startidx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%send_count", p_patch%comm_pat_c1%send_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%send_limits", p_patch%comm_pat_c1%send_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%send_src_blk", p_patch%comm_pat_c1%send_src_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%send_src_idx", p_patch%comm_pat_c1%send_src_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_c1%send_startidx", p_patch%comm_pat_c1%send_startidx, ftg_rperturb)
    CALL ftg_read("p_patch%comm_pat_e%n_pnts", p_patch%comm_pat_e%n_pnts)
    CALL ftg_read("p_patch%comm_pat_e%n_recv", p_patch%comm_pat_e%n_recv)
    CALL ftg_read("p_patch%comm_pat_e%n_send", p_patch%comm_pat_e%n_send)
    CALL ftg_read("p_patch%comm_pat_e%np_recv", p_patch%comm_pat_e%np_recv)
    CALL ftg_read("p_patch%comm_pat_e%np_send", p_patch%comm_pat_e%np_send)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%pelist_recv", p_patch%comm_pat_e%pelist_recv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%pelist_send", p_patch%comm_pat_e%pelist_send, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_count", p_patch%comm_pat_e%recv_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_dst_blk", p_patch%comm_pat_e%recv_dst_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_dst_idx", p_patch%comm_pat_e%recv_dst_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_limits", p_patch%comm_pat_e%recv_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_src", p_patch%comm_pat_e%recv_src, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%recv_startidx", p_patch%comm_pat_e%recv_startidx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%send_count", p_patch%comm_pat_e%send_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%send_limits", p_patch%comm_pat_e%send_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%send_src_blk", p_patch%comm_pat_e%send_src_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%send_src_idx", p_patch%comm_pat_e%send_src_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_e%send_startidx", p_patch%comm_pat_e%send_startidx, ftg_rperturb)
    CALL ftg_read("p_patch%comm_pat_v%n_pnts", p_patch%comm_pat_v%n_pnts)
    CALL ftg_read("p_patch%comm_pat_v%n_recv", p_patch%comm_pat_v%n_recv)
    CALL ftg_read("p_patch%comm_pat_v%n_send", p_patch%comm_pat_v%n_send)
    CALL ftg_read("p_patch%comm_pat_v%np_recv", p_patch%comm_pat_v%np_recv)
    CALL ftg_read("p_patch%comm_pat_v%np_send", p_patch%comm_pat_v%np_send)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%pelist_recv", p_patch%comm_pat_v%pelist_recv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%pelist_send", p_patch%comm_pat_v%pelist_send, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_count", p_patch%comm_pat_v%recv_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_dst_blk", p_patch%comm_pat_v%recv_dst_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_dst_idx", p_patch%comm_pat_v%recv_dst_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_limits", p_patch%comm_pat_v%recv_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_src", p_patch%comm_pat_v%recv_src, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%recv_startidx", p_patch%comm_pat_v%recv_startidx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%send_count", p_patch%comm_pat_v%send_count, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%send_limits", p_patch%comm_pat_v%send_limits, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%send_src_blk", p_patch%comm_pat_v%send_src_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%send_src_idx", p_patch%comm_pat_v%send_src_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%comm_pat_v%send_startidx", p_patch%comm_pat_v%send_startidx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%area_edge", p_patch%edges%area_edge, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_blk", p_patch%edges%cell_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_idx", p_patch%edges%cell_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%edge_cell_length", p_patch%edges%edge_cell_length, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_blk", p_patch%edges%end_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_block", p_patch%edges%end_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_index", p_patch%edges%end_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%f_e", p_patch%edges%f_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_dual_edge_length", p_patch%edges%inv_dual_edge_length, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_primal_edge_length", p_patch%edges%inv_primal_edge_length, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_blk", p_patch%edges%quad_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_idx", p_patch%edges%quad_idx, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%refin_ctrl", p_patch%edges%refin_ctrl, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_blk", p_patch%edges%start_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_block", p_patch%edges%start_block, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_index", p_patch%edges%start_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%tangent_orientation", p_patch%edges%tangent_orientation, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_blk", p_patch%edges%vertex_blk, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_idx", p_patch%edges%vertex_idx, ftg_rperturb)
    CALL ftg_read("p_patch%geometry_info%cell_type", p_patch%geometry_info%cell_type)
    CALL ftg_read("p_patch%geometry_info%mean_cell_area", p_patch%geometry_info%mean_cell_area)
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
    CALL ftg_allocate_and_read_pointer("p_patch%cells%decomp_info%decomp_domain", p_patch%cells%decomp_info%decomp_domain, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%decomp_info%glb_index", p_patch%cells%decomp_info%glb_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%decomp_info%owner_mask", p_patch%cells%decomp_info%owner_mask, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("p_patch%edges%decomp_info%decomp_domain", p_patch%edges%decomp_info%decomp_domain, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%decomp_info%glb_index", p_patch%edges%decomp_info%glb_index, ftg_rperturb)
    IF (ftg_field_exists("p_patch%edges%dual_normal_cell")) THEN
      ftg_bounds = ftg_get_bounds("p_patch%edges%dual_normal_cell")
      ALLOCATE(p_patch%edges%dual_normal_cell(ftg_bounds(1):ftg_bounds(2), ftg_bounds(3):ftg_bounds(4), ftg_bounds(5):ftg_bounds( &
      &  6)))
    ELSE
      ALLOCATE(p_patch%edges%dual_normal_cell(0, 0, 0))
    END IF
    CALL ftg_read("p_patch%edges%dual_normal_cell%v1", p_patch%edges%dual_normal_cell%v1)
    CALL ftg_read("p_patch%edges%dual_normal_cell%v2", p_patch%edges%dual_normal_cell%v2)
    IF (ftg_field_exists("p_patch%edges%primal_normal_cell")) THEN
      ftg_bounds = ftg_get_bounds("p_patch%edges%primal_normal_cell")
      ALLOCATE(p_patch%edges%primal_normal_cell(ftg_bounds(1):ftg_bounds(2), ftg_bounds(3):ftg_bounds(4), ftg_bounds(5): &
      &  ftg_bounds(6)))
    ELSE
      ALLOCATE(p_patch%edges%primal_normal_cell(0, 0, 0))
    END IF
    CALL ftg_read("p_patch%edges%primal_normal_cell%v1", p_patch%edges%primal_normal_cell%v1)
    CALL ftg_read("p_patch%edges%primal_normal_cell%v2", p_patch%edges%primal_normal_cell%v2)
    CALL ftg_allocate_and_read_pointer("p_patch%verts%decomp_info%decomp_domain", p_patch%verts%decomp_info%decomp_domain, &
    &  ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%decomp_info%glb_index", p_patch%verts%decomp_info%glb_index, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%c_bln_avg", p_int%c_bln_avg, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%c_lin_e", p_int%c_lin_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%cells_aw_verts", p_int%cells_aw_verts, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%e_bln_c_s", p_int%e_bln_c_s, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%e_flx_avg", p_int%e_flx_avg, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_div", p_int%geofac_div, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_grdiv", p_int%geofac_grdiv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_n2s", p_int%geofac_n2s, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_rot", p_int%geofac_rot, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%nudgecoeff_e", p_int%nudgecoeff_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%pos_on_tplane_e", p_int%pos_on_tplane_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%rbf_vec_coeff_e", p_int%rbf_vec_coeff_e, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_blk_c", p_int%lsq_high%lsq_blk_c, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_idx_c", p_int%lsq_high%lsq_idx_c, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_moments", p_int%lsq_high%lsq_moments, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_pseudoinv", p_int%lsq_high%lsq_pseudoinv, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_qtmat_c", p_int%lsq_high%lsq_qtmat_c, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_rmat_rdiag_c", p_int%lsq_high%lsq_rmat_rdiag_c, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("p_int%lsq_high%lsq_rmat_utri_c", p_int%lsq_high%lsq_rmat_utri_c, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("prep_adv%mass_flx_ic", prep_adv%mass_flx_ic, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("prep_adv%mass_flx_me", prep_adv%mass_flx_me, ftg_rperturb)
    CALL ftg_allocate_and_read_allocatable("prep_adv%vn_traj", prep_adv%vn_traj, ftg_rperturb)
    
    
    ! GLOBALS
    CALL ftg_read("mo_icon_comm_lib__active_comm_variables", mo_icon_comm_lib__active_comm_variables)
    CALL ftg_read("mo_icon_comm_lib__active_recv_buffers", mo_icon_comm_lib__active_recv_buffers)
    CALL ftg_read("mo_icon_comm_lib__active_send_buffers", mo_icon_comm_lib__active_send_buffers)
    CALL ftg_read("mo_icon_comm_lib__buffer_comm_status", mo_icon_comm_lib__buffer_comm_status)
    CALL ftg_read("mo_icon_comm_lib__comm_lib_is_initialized", mo_icon_comm_lib__comm_lib_is_initialized)
    CALL ftg_read("mo_nonhydrostatic_config__divdamp_fac", mo_nonhydrostatic_config__divdamp_fac)
    CALL ftg_read("mo_nonhydrostatic_config__divdamp_fac_o2", mo_nonhydrostatic_config__divdamp_fac_o2)
    CALL ftg_read("mo_nonhydrostatic_config__divdamp_order", mo_nonhydrostatic_config__divdamp_order)
    CALL ftg_read("mo_nonhydrostatic_config__divdamp_type", mo_nonhydrostatic_config__divdamp_type)
    CALL ftg_read("mo_sync__do_sync_checks", mo_sync__do_sync_checks)
    CALL ftg_read("mo_advection_config__eta", mo_advection_config__eta)
    CALL ftg_read("mo_gridref_config__grf_intmethod_e", mo_gridref_config__grf_intmethod_e)
    CALL ftg_read("mo_nonhydrostatic_config__iadv_rhotheta", mo_nonhydrostatic_config__iadv_rhotheta)
    CALL ftg_read("mo_initicon_config__iau_wgt_dyn", mo_initicon_config__iau_wgt_dyn)
    CALL ftg_read("mo_parallel_config__icon_comm_debug", mo_parallel_config__icon_comm_debug)
    CALL ftg_read("mo_parallel_config__icon_comm_method", mo_parallel_config__icon_comm_method)
    CALL ftg_read("mo_dynamics_config__idiv_method", mo_dynamics_config__idiv_method)
    CALL ftg_read("mo_nonhydrostatic_config__igradp_method", mo_nonhydrostatic_config__igradp_method)
    CALL ftg_read("mo_parallel_config__iorder_sendrecv", mo_parallel_config__iorder_sendrecv)
    CALL ftg_read("mo_initicon_config__is_iau_active", mo_initicon_config__is_iau_active)
    CALL ftg_read("mo_nonhydrostatic_config__itime_scheme", mo_nonhydrostatic_config__itime_scheme)
    CALL ftg_read("mo_parallel_config__itype_comm", mo_parallel_config__itype_comm)
    CALL ftg_read("mo_parallel_config__itype_exch_barrier", mo_parallel_config__itype_exch_barrier)
    CALL ftg_read("mo_nonhydrostatic_config__kstart_dd3d", mo_nonhydrostatic_config__kstart_dd3d)
    CALL ftg_read("mo_nonhydrostatic_config__kstart_moist", mo_nonhydrostatic_config__kstart_moist)
    CALL ftg_read("mo_grid_config__l_limited_area", mo_grid_config__l_limited_area)
    CALL ftg_read("mo_parallel_config__l_log_checks", mo_parallel_config__l_log_checks)
    CALL ftg_read("mo_nonhydrostatic_config__l_open_ubc", mo_nonhydrostatic_config__l_open_ubc)
    CALL ftg_read("mo_nonhydrostatic_config__lextra_diffu", mo_nonhydrostatic_config__lextra_diffu)
    CALL ftg_read("mo_nonhydrostatic_config__lhdiff_rcf", mo_nonhydrostatic_config__lhdiff_rcf)
    CALL ftg_read("mo_icon_comm_lib__log_file_id", mo_icon_comm_lib__log_file_id)
    CALL ftg_read("mo_sync__log_unit", mo_sync__log_unit)
    CALL ftg_read("mo_interpol_config__lsq_high_ord", mo_interpol_config__lsq_high_ord)
    CALL ftg_read("mo_run_config__ltimer", mo_run_config__ltimer)
    CALL ftg_read("mo_run_config__lvert_nest", mo_run_config__lvert_nest)
    CALL ftg_read("mo_icon_comm_lib__max_active_comm_variables", mo_icon_comm_lib__max_active_comm_variables)
    CALL ftg_read("mo_parallel_config__max_mpi_message_size", mo_parallel_config__max_mpi_message_size)
    CALL ftg_read("mo_parallel_config__max_no_of_comm_variables", mo_parallel_config__max_no_of_comm_variables)
    CALL ftg_read("mo_icon_comm_lib__max_send_buffer_size", mo_icon_comm_lib__max_send_buffer_size)
    CALL ftg_read("mo_icon_comm_lib__my_work_communicator", mo_icon_comm_lib__my_work_communicator)
    CALL ftg_read("mo_parallel_config__n_ghost_rows", mo_parallel_config__n_ghost_rows)
    CALL ftg_read("mo_nonhydrostatic_config__ndyn_substeps_var", mo_nonhydrostatic_config__ndyn_substeps_var)
    CALL ftg_read("mo_vertical_grid__nflat_gradp", mo_vertical_grid__nflat_gradp)
    CALL ftg_read("mo_init_vgrid__nflatlev", mo_init_vgrid__nflatlev)
    CALL ftg_read("mo_parallel_config__nproma", mo_parallel_config__nproma)
    CALL ftg_read("mo_vertical_grid__nrdmax", mo_vertical_grid__nrdmax)
    CALL ftg_read("mo_interpol_config__nudge_max_coeff", mo_interpol_config__nudge_max_coeff)
    CALL ftg_read("mo_parallel_config__p_test_run", mo_parallel_config__p_test_run)
    CALL ftg_allocate_and_read_pointer("mo_advection_utils__ptr_delp_mc_new", mo_advection_utils__ptr_delp_mc_new, ftg_rperturb)
    CALL ftg_allocate_and_read_pointer("mo_advection_utils__ptr_delp_mc_now", mo_advection_utils__ptr_delp_mc_now, ftg_rperturb)
    CALL ftg_read("mo_nonhydrostatic_config__rayleigh_type", mo_nonhydrostatic_config__rayleigh_type)
    CALL ftg_allocate_and_read_allocatable("mo_icon_comm_lib__recv_buffer", mo_icon_comm_lib__recv_buffer, ftg_rperturb)
    CALL ftg_read("mo_nonhydrostatic_config__rhotheta_offctr", mo_nonhydrostatic_config__rhotheta_offctr)
    CALL ftg_allocate_and_read_allocatable("mo_icon_comm_lib__send_buffer", mo_icon_comm_lib__send_buffer, ftg_rperturb)
    CALL ftg_read("mo_advection_config__shape_func", mo_advection_config__shape_func)
    CALL ftg_read("mo_parallel_config__sync_barrier_mode", mo_parallel_config__sync_barrier_mode)
    CALL ftg_read("mo_icon_comm_lib__this_is_mpi_sequential", mo_icon_comm_lib__this_is_mpi_sequential)
    CALL ftg_read("mo_timer__timer_back_traj", mo_timer__timer_back_traj)
    CALL ftg_read("mo_timer__timer_barrier", mo_timer__timer_barrier)
    CALL ftg_read("mo_timer__timer_exch_data", mo_timer__timer_exch_data)
    CALL ftg_read("mo_timer__timer_exch_data_wait", mo_timer__timer_exch_data_wait)
    CALL ftg_read("mo_timer__timer_icon_comm_barrier_2", mo_timer__timer_icon_comm_barrier_2)
    CALL ftg_read("mo_timer__timer_icon_comm_fillrecv", mo_timer__timer_icon_comm_fillrecv)
    CALL ftg_read("mo_timer__timer_icon_comm_fillsend", mo_timer__timer_icon_comm_fillsend)
    CALL ftg_read("mo_timer__timer_icon_comm_ircv", mo_timer__timer_icon_comm_ircv)
    CALL ftg_read("mo_timer__timer_icon_comm_sync", mo_timer__timer_icon_comm_sync)
    CALL ftg_read("mo_timer__timer_icon_comm_wait", mo_timer__timer_icon_comm_wait)
    CALL ftg_read("mo_timer__timer_intp", mo_timer__timer_intp)
    CALL ftg_read("mo_timer__timer_solve_nh", mo_timer__timer_solve_nh)
    CALL ftg_read("mo_timer__timer_solve_nh_cellcomp", mo_timer__timer_solve_nh_cellcomp)
    CALL ftg_read("mo_timer__timer_solve_nh_edgecomp", mo_timer__timer_solve_nh_edgecomp)
    CALL ftg_read("mo_timer__timer_solve_nh_exch", mo_timer__timer_solve_nh_exch)
    CALL ftg_read("mo_timer__timer_solve_nh_veltend", mo_timer__timer_solve_nh_veltend)
    CALL ftg_read("mo_timer__timer_solve_nh_vimpl", mo_timer__timer_solve_nh_vimpl)
    CALL ftg_read("mo_timer__timer_solve_nh_vnupd", mo_timer__timer_solve_nh_vnupd)
    CALL ftg_read("mo_run_config__timers_level", mo_run_config__timers_level)
    CALL ftg_read("mo_parallel_config__use_dycore_barrier", mo_parallel_config__use_dycore_barrier)
    CALL ftg_read("mo_parallel_config__use_icon_comm", mo_parallel_config__use_icon_comm)
    CALL ftg_allocate_and_read_allocatable("mo_vertical_coord_table__vct_a", mo_vertical_coord_table__vct_a, ftg_rperturb)
    CALL ftg_read("mo_nonhydrostatic_config__veladv_offctr", mo_nonhydrostatic_config__veladv_offctr)
    CALL ftg_read("mo_advection_config__wgt_eta", mo_advection_config__wgt_eta)
    CALL ftg_read("mo_advection_config__wgt_zeta", mo_advection_config__wgt_zeta)
    CALL ftg_read("mo_advection_config__zeta", mo_advection_config__zeta)
    CALL ftg_read("mo_advection_config__advection_config%beta_fct", mo_advection_config__advection_config%beta_fct)
    CALL ftg_read("mo_advection_config__advection_config%llsq_svd", mo_advection_config__advection_config%llsq_svd)
    IF (ftg_field_exists("mo_icon_comm_lib__comm_variable")) THEN
      ftg_bounds = ftg_get_bounds("mo_icon_comm_lib__comm_variable")
      ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_icon_comm_lib__comm_variable(0))
    END IF
    CALL ftg_read("mo_icon_comm_lib__comm_variable%comm_buffer", mo_icon_comm_lib__comm_variable%comm_buffer)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%comm_pattern_index", mo_icon_comm_lib__comm_variable%comm_pattern_index)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%comm_status", mo_icon_comm_lib__comm_variable%comm_status)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%dim_4", mo_icon_comm_lib__comm_variable%dim_4)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%grid_dim", mo_icon_comm_lib__comm_variable%grid_dim)
    ! *** WARNING: Type not supported by serialbox ***
    !     mo_icon_comm_lib__comm_variable%name
    !     CHARACTER(len=32) | dimension: 0
    CALL ftg_read("mo_icon_comm_lib__comm_variable%no_of_variables", mo_icon_comm_lib__comm_variable%no_of_variables)
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%recv_values_2d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%recv_values_2d, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%recv_values_3d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%recv_values_3d, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%recv_values_4d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%recv_values_4d, ftg_rperturb)
    END DO
    CALL ftg_read("mo_icon_comm_lib__comm_variable%request", mo_icon_comm_lib__comm_variable%request)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%scope", mo_icon_comm_lib__comm_variable%scope)
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%send_values_2d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%send_values_2d, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%send_values_3d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%send_values_3d, ftg_rperturb)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%send_values_4d'
      CALL ftg_allocate_and_read_pointer(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%send_values_4d, ftg_rperturb)
    END DO
    CALL ftg_read("mo_icon_comm_lib__comm_variable%status", mo_icon_comm_lib__comm_variable%status)
    CALL ftg_read("mo_icon_comm_lib__comm_variable%vertical_layers", mo_icon_comm_lib__comm_variable%vertical_layers)
    IF (ftg_field_exists("mo_icon_comm_lib__grid_comm_pattern_list")) THEN
      ftg_bounds = ftg_get_bounds("mo_icon_comm_lib__grid_comm_pattern_list")
      ALLOCATE(mo_icon_comm_lib__grid_comm_pattern_list(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_icon_comm_lib__grid_comm_pattern_list(0))
    END IF
    CALL ftg_read("mo_icon_comm_lib__grid_comm_pattern_list%status", mo_icon_comm_lib__grid_comm_pattern_list%status)
    CALL ftg_read("mo_interpol_config__lsq_high_set%dim_c", mo_interpol_config__lsq_high_set%dim_c)
    CALL ftg_read("mo_interpol_config__lsq_high_set%dim_unk", mo_interpol_config__lsq_high_set%dim_unk)
    IF (ftg_field_exists("mo_icon_comm_lib__recv_procs_buffer")) THEN
      ftg_bounds = ftg_get_bounds("mo_icon_comm_lib__recv_procs_buffer")
      ALLOCATE(mo_icon_comm_lib__recv_procs_buffer(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_icon_comm_lib__recv_procs_buffer(0))
    END IF
    CALL ftg_read("mo_icon_comm_lib__recv_procs_buffer%buffer_size", mo_icon_comm_lib__recv_procs_buffer%buffer_size)
    CALL ftg_read("mo_icon_comm_lib__recv_procs_buffer%current_index", mo_icon_comm_lib__recv_procs_buffer%current_index)
    CALL ftg_read("mo_icon_comm_lib__recv_procs_buffer%pid", mo_icon_comm_lib__recv_procs_buffer%pid)
    CALL ftg_read("mo_icon_comm_lib__recv_procs_buffer%start_index", mo_icon_comm_lib__recv_procs_buffer%start_index)
    IF (ftg_field_exists("mo_icon_comm_lib__send_procs_buffer")) THEN
      ftg_bounds = ftg_get_bounds("mo_icon_comm_lib__send_procs_buffer")
      ALLOCATE(mo_icon_comm_lib__send_procs_buffer(ftg_bounds(1):ftg_bounds(2)))
    ELSE
      ALLOCATE(mo_icon_comm_lib__send_procs_buffer(0))
    END IF
    CALL ftg_read("mo_icon_comm_lib__send_procs_buffer%buffer_size", mo_icon_comm_lib__send_procs_buffer%buffer_size)
    CALL ftg_read("mo_icon_comm_lib__send_procs_buffer%current_index", mo_icon_comm_lib__send_procs_buffer%current_index)
    CALL ftg_read("mo_icon_comm_lib__send_procs_buffer%end_index", mo_icon_comm_lib__send_procs_buffer%end_index)
    CALL ftg_read("mo_icon_comm_lib__send_procs_buffer%pid", mo_icon_comm_lib__send_procs_buffer%pid)
    CALL ftg_read("mo_icon_comm_lib__send_procs_buffer%start_index", mo_icon_comm_lib__send_procs_buffer%start_index)
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%no_of_recv_procs'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%no_of_recv_procs)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%no_of_send_procs'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%no_of_send_procs)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(ftg_bounds(1):ftg_bounds(2)))
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(0))
      END IF
      DO ftg_d2 = LBOUND(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv, 1), UBOUND( &
      &  mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv, 1)
        WRITE (ftg_c,'(A,I0,A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv(', ftg_d2, ')%block_no'
        CALL ftg_allocate_and_read_allocatable(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(ftg_d2)% &
        &  block_no, ftg_rperturb)
      END DO
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv%buffer_index'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv%buffer_index)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(ftg_bounds(1):ftg_bounds(2)))
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(0))
      END IF
      DO ftg_d2 = LBOUND(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv, 1), UBOUND( &
      &  mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv, 1)
        WRITE (ftg_c,'(A,I0,A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv(', ftg_d2, ')%index_no'
        CALL ftg_allocate_and_read_allocatable(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv(ftg_d2)% &
        &  index_no, ftg_rperturb)
      END DO
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%recv%no_of_points'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%recv%no_of_points)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_bounds(1):ftg_bounds(2)))
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(0))
      END IF
      DO ftg_d2 = LBOUND(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send, 1), UBOUND( &
      &  mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send, 1)
        WRITE (ftg_c,'(A,I0,A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send(', ftg_d2, ')%block_no'
        CALL ftg_allocate_and_read_allocatable(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)% &
        &  block_no, ftg_rperturb)
      END DO
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send%buffer_index'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send%buffer_index)
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern)
      END IF
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send'
      IF (ftg_field_exists(ftg_c)) THEN
        ftg_bounds = ftg_get_bounds(ftg_c)
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_bounds(1):ftg_bounds(2)))
      ELSE
        ALLOCATE(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(0))
      END IF
      DO ftg_d2 = LBOUND(mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send, 1), UBOUND( &
      &  mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send, 1)
        WRITE (ftg_c,'(A,I0,A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send(', ftg_d2, ')%index_no'
        CALL ftg_allocate_and_read_allocatable(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)% &
        &  index_no, ftg_rperturb)
      END DO
    END DO
    
    DO ftg_d1 = LBOUND(mo_icon_comm_lib__comm_variable, 1), UBOUND(mo_icon_comm_lib__comm_variable, 1)
      WRITE (ftg_c,'(A,I0,A)') 'mo_icon_comm_lib__comm_variable(', ftg_d1, ')%grid_comm_pattern%send%no_of_points'
      CALL ftg_read(ftg_c, mo_icon_comm_lib__comm_variable(ftg_d1)%grid_comm_pattern%send%no_of_points)
    END DO
    
    
    CALL ftg_destroy_savepoint()
    
  END SUBROUTINE ftg_solve_nh_replay_input
  
END PROGRAM ftg_solve_nh_test



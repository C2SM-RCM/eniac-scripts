!+ Module for Additional local timing utilities
!------------------------------------------------------------------------------

!!#define _LOC_TIMING_SYNC
!!#define _LOC_TIMING_DETAIL
!Use MPI_Abort if available
#ifdef NOMPI
#define TIMING_ABORT(COMM,IERR) STOP
#else
#define TIMING_ABORT(COMM,IERR) CALL MPI_Abort(COMM,IERR)
#endif

MODULE timing_tools

!------------------------------------------------------------------------------
!
! Description:
!   This module provides additional local timers with a start stop timing routine
!   Note : the timers are only active if the compiler flag -D_LOC_TIMING is
!          used
!
!   Routines (module procedures) currently contained:
!
!     - init_loc_timing:
!       initialize the global variables, needs to be call before 
!       any timers
!
!     - reset_loc_timing:
!       reset all timers
!
!     - start_loc_timing:
!       start timer
!
!     - end_loc_timing:
!       end timer
!
!     - print_loc_timing:
!       print timing results
!
!
! Method:
!   - timing uses SYSTEM_CLOCK or get_mpi_time if available. 
!     In addition a call to !$acc wait is done 
!     when running on GPU to ensure that all asynchronous activities are done
!     The acc wait may alter the overall performance of the application. 
!     The timers are suppose to be used on compute process (not design for 
!     IO activities)
!   - each timer is associated with a tag and an id number. The same id number 
!     may be use at different places in the code. It is the user responsibility
!     to verify that the id number are consitent (ex: same id number for all 
!     communications).
!   - output gives a the min, max, mean values over all compute_process
!
!
! Current Code Owner: C2SM, ETH Zurich, Xavier Lapillonne
!  phone: +41 44 256 92 37
!  fax  : +41 44 256 92 78
!  email:  xavier.lapillonne@env.ethz.ch
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.1        2014/03/11 Xavier Lapillonne
!
!
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

! Modules used:
USE mo_kind, only : dp, wp
USE mo_mpi

!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------

! global constants
INTEGER, PARAMETER :: max_timing = 1000

! global variables
!!$ INTEGER :: cur_timing = 0

LOGICAL, SAVE  :: ltiming = .FALSE., ltiming_list(max_timing)

CHARACTER (LEN=64), SAVE :: tag_list(max_timing)

INTEGER, SAVE ::  &
     icountnew(max_timing), icountold(max_timing), &
     icountrate, icountmax, ncalls(max_timing)

#ifndef NOMPI
REAL (KIND=dp), SAVE :: stiming(max_timing)
#endif

REAL (KIND=dp), SAVE :: rtiming(max_timing)
REAL (KIND=dp), SAVE :: rsync(max_timing)

!INCLUDE 'mpif.h'

!==============================================================================
! Module procedures in timing_tools
!==============================================================================

PUBLIC :: init_loc_timing
PUBLIC :: start_loc_timing
PUBLIC :: end_loc_timing
PUBLIC :: print_loc_timing

CONTAINS

!==============================================================================
!+ Module procedure to initialize the timings
!------------------------------------------------------------------------------

SUBROUTINE init_loc_timing

!------------------------------------------------------------------------------
!
! Description: initialize global variables
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: idummy

!- End of header -

  ltiming         = .TRUE.
  ltiming_list(:) = .FALSE.
  rtiming(:)      = 0.0_dp
  rsync(:)        = 0.0_dp
  ncalls(:)       = 0
  icountold(:)    = -1  ! set to -1 to signal that this timer has never been used
#ifdef NOMPI
  CALL SYSTEM_CLOCK(COUNT=idummy, COUNT_RATE=icountrate, COUNT_MAX=icountmax)
#else
  stiming(:) = -1.0_dp
  icountrate  = 0
  icountmax   = 0
#endif
!- End of the Subroutine

END SUBROUTINE init_loc_timing

!==============================================================================
!+ Module procedure to reset all timers, e.g. to skip first time step
!------------------------------------------------------------------------------

SUBROUTINE reset_loc_timing

!------------------------------------------------------------------------------
!
! Description: reset all timers
!   
!------------------------------------------------------------------------------

  IMPLICIT NONE

!- End of header -

  rtiming(:) = 0.0_dp
  rsync(:)   = 0.0_dp
  ncalls(:)  = 0
#ifndef NOMPI
  stiming(:) = -1.0_dp
#endif

!- End of the Subroutine

END SUBROUTINE reset_loc_timing

!==============================================================================
!+ Module procedure to start the timing inum
!------------------------------------------------------------------------------

SUBROUTINE start_loc_timing(tag, inum)

!------------------------------------------------------------------------------
!
! Description: start local timing
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER (LEN=*), INTENT(IN) :: tag
  INTEGER, INTENT(IN) :: inum
  INTEGER :: izerror

!- End of header -

#ifdef _LOC_TIMING 
!!$  ! assign new index
!!$  IF (inum <= 0) THEN
!!$    cur_timing = cur_timing + 1
!!$    inum = cur_timing
!!$  END IF

  ! check inum
  IF (inum < 1 .OR. inum > max_timing) THEN
    PRINT *, 'ERROR: Problem in start_loc_timing (inum < 1 or inum > max_timing)'
    TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
  END IF

  ! make sure this is the first call or previously end_loc_timing has been called
  IF (icountold(inum) /= -1 .AND. icountold(inum) /= -2) THEN
    PRINT *, 'ERROR: Problem in start_loc_timing (no previous end_loc_timing)', tag_list(inum), inum
    TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
  ENDIF

  ! save tag if this is the first call (check tag in debug mode otherwise)
  IF (icountold(inum) == -1) THEN
    tag_list(inum) = TRIM(tag)
    ltiming_list(inum) = .TRUE.
  ELSE
#ifdef DEBUG
    IF (.NOT. ltiming_list(inum)) THEN
      PRINT *, 'ERROR: Problem in start_loc_timing (timer not active)', tag_list(inum), inum
      TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
    ENDIF
    IF (TRIM(tag_list(inum)) /= TRIM(tag)) THEN
      PRINT *, 'ERROR: Problem in start_loc_timing (tags do not match)', tag, tag_list(inum), inum
      TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
    ENDIF
#endif
  ENDIF

  !$acc wait
#ifdef NOMPI
  CALL SYSTEM_CLOCK(COUNT=icountold(inum))
#else
  stiming(inum)=get_mpi_time()
  icountold(inum)=1  !Flag inum in use
#endif
  ncalls(inum) = ncalls(inum) + 1
#endif

!- End of the Subroutine

END SUBROUTINE start_loc_timing

!==============================================================================
!+ Module procedure to end the timing inum
!------------------------------------------------------------------------------

SUBROUTINE end_loc_timing(inum, elapsed_time)

!------------------------------------------------------------------------------
!
! Description: stop local timing (requires a previous call to start_loc_timing)
!
!------------------------------------------------------------------------------

  IMPLICIT none

  INTEGER, INTENT(IN) :: inum
  REAL (KIND=wp), OPTIONAL :: elapsed_time
  REAL (KIND=dp) :: etiming,ztime
  INTEGER :: izerror

!- End of header -

#ifdef _LOC_TIMING 
  !$acc wait
  IF (icountold(inum) == -1 .OR. icountold(inum) == -2) THEN
    PRINT *, 'ERROR: Problem in end_loc_timing (no matching start_loc_timing)', tag_list(inum), inum
    TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
  ELSE
#ifdef NOMPI
    CALL SYSTEM_CLOCK(COUNT=icountnew(inum))
    IF ( icountnew(inum) >= icountold(inum) ) THEN
      ztime = ( REAL(icountnew(inum) - icountold(inum), dp) )      &
                    / REAL(icountrate, dp)
    ELSE
      ztime = REAL(icountmax - (icountold(inum)-icountnew(inum) ), dp)     &
                    / REAL(icountrate, dp)
    ENDIF
#else
    etiming=get_mpi_time()
    ztime=etiming-stiming(inum)
    icountnew(inum)=1 !Flag inum in use
#endif
    IF (PRESENT(elapsed_time)) THEN
      elapsed_time = REAL(ztime,wp)
    ENDIF
    rtiming(inum) = rtiming(inum) + ztime

#ifdef _LOC_TIMING_SYNC
    ! and now get sync time
    CALL MPI_BARRIER(get_my_mpi_work_communicator(), izerror)
    IF (icountold(inum) == -1 .OR. icountold(inum) == -2) THEN
      PRINT *, 'ERROR: Problem in end_loc_timing (error in MPI_BARRIER)', izerror
      TIMING_ABORT(get_my_mpi_work_communicator(),izerror)
    ENDIF
!Reset start time to last end time to count only synchronization time
#ifdef NOMPI
    icountold(inum) = icountnew(inum)
    CALL SYSTEM_CLOCK(COUNT=icountnew(inum))
    IF ( icountnew(inum) >= icountold(inum) ) THEN
      ztime = ( REAL(icountnew(inum) - icountold(inum), dp) )      &
                    / REAL(icountrate, dp)
    ELSE
      ztime = REAL(icountmax - (icountold(inum)-icountnew(inum) ), dp)     &
                    / REAL(icountrate, dp)
    ENDIF
#else
    stiming(inum) = etiming
    etiming=get_mpi_time()
    ztime=etiming-stiming(inum)
#endif
    rsync(inum) = rsync(inum) + ztime
#endif

    ! release timer
    icountold(inum) = -2 ! set to -2 to ensure a start_timing is called before next end_timing

  END IF
#endif

!- End of the Subroutine

END SUBROUTINE end_loc_timing

!==============================================================================
!+ Module procedure to print all timings
!------------------------------------------------------------------------------

SUBROUTINE print_loc_timing

!------------------------------------------------------------------------------
!
! Description: print all timings results
!
!------------------------------------------------------------------------------


  IMPLICIT none

  INTEGER :: inum
  REAL (KIND=dp) :: ztime_mean, ztime_min, ztime_max
  INTEGER :: izerror

!- End of header -

#ifdef _LOC_TIMING 
  IF (ltiming) THEN

#ifdef _LOC_TIMING_DETAIL
    WRITE(700+get_my_mpi_all_id(),*) '--------------------------------------------------------------------------'
    WRITE(700+get_my_mpi_all_id(),*) ' Local timers:'
    WRITE(700+get_my_mpi_all_id(),"(A,I4)") '   NCOMP_PE= ', num_work_procs
    WRITE(700+get_my_mpi_all_id(),*) '--------------------------------------------------------------------------'
    WRITE(700+get_my_mpi_all_id(),*) ' Id      Tag                     Ncalls         run[s]       sync[s] '
#endif

    IF (get_my_mpi_all_id() == 0) PRINT *, '--------------------------------------------------------------------------'
    IF (get_my_mpi_all_id() == 0) PRINT *, ' Local timers:'
    IF (get_my_mpi_all_id() == 0) WRITE(*,"(A,I4)") '   NCOMP_PE= ', num_work_procs
    IF (get_my_mpi_all_id() == 0) PRINT *, '--------------------------------------------------------------------------'
    IF (get_my_mpi_all_id() == 0) PRINT *, ' Id      Tag                     Ncalls         min[s]        max[s]       mean[s] '
    DO inum = 1, max_timing

      ! NOTE: this IF-statement can go terribly wrong if not all locations are
      ! called on all ranks (cf. MPI_REDUCE below)
      IF (ltiming_list(inum)) THEN 

        ! get run time
        IF (num_work_procs > 1) THEN
          CALL MPI_REDUCE(rtiming(inum), ztime_min, 1, p_real_dp, p_min_op(), 0, get_my_mpi_work_communicator(), izerror)
          CALL MPI_REDUCE(rtiming(inum), ztime_max, 1, p_real_dp, p_max_op(), 0, get_my_mpi_work_communicator(), izerror)
          CALL MPI_REDUCE(rtiming(inum), ztime_mean, 1, p_real_dp, p_sum_op(), 0, get_my_mpi_work_communicator(), izerror)
          ztime_mean = ztime_mean / REAL(num_work_procs, dp)
        ELSE
          ztime_mean = rtiming(inum)
          ztime_min  = rtiming(inum)
          ztime_max  = rtiming(inum)
        END IF
        IF (get_my_mpi_all_id() == 0) WRITE(*,"(I4, A28,I8,F14.4,F14.4,F14.4)")  &
             inum, TRIM(tag_list(inum)), ncalls(inum), ztime_min, ztime_max, ztime_mean

#ifdef _LOC_TIMING_SYNC
        ! get sync time
        IF (num_work_procs > 1) THEN
          CALL MPI_REDUCE(rsync(inum), ztime_min, 1, p_real_dp, p_min_op(), 0, get_my_mpi_work_communicator(), izerror)
          CALL MPI_REDUCE(rsync(inum), ztime_max, 1, p_real_dp, p_max_op(), 0, get_my_mpi_work_communicator(), izerror)
          CALL MPI_REDUCE(rsync(inum), ztime_mean, 1, p_real_dp, p_sum_op(), 0, get_my_mpi_work_communicator(), izerror)
          ztime_mean = ztime_mean / REAL(num_work_procs, dp)
        ELSE
          ztime_mean = rsync(inum)
          ztime_min  = rsync(inum)
          ztime_max  = rsync(inum)
        END IF
        IF (get_my_mpi_all_id() == 0) WRITE(*,"(I4, A28,I8,F14.4,F14.4,F14.4)")  &
             inum, TRIM(tag_list(inum))//" (sync)", ncalls(inum), ztime_min, ztime_max, ztime_mean
#endif

#ifdef _LOC_TIMING_DETAIL
        WRITE(700+get_my_mpi_all_id(),"(I4, A28,I8,F14.4,F14.4,F14.4)")  &
             inum, TRIM(tag_list(inum)), ncalls(inum), rtiming(inum), rsync(inum)
#endif

      END IF
    END DO
    IF (get_my_mpi_all_id() == 0) PRINT *, '--------------------------------------------------------------------------'
  END IF
#endif

!- End of the Subroutine

END SUBROUTINE print_loc_timing

!==============================================================================

END MODULE timing_tools


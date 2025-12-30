!*****************************************************************************************************!
!                            Copyright 2008-2020  The ALaDyn Collaboration                            !
!*****************************************************************************************************!

!*****************************************************************************************************!
!  This file is part of ALaDyn.                                                                       !
!                                                                                                     !
!  ALaDyn is free software: you can redistribute it and/or modify                                     !
!  it under the terms of the GNU General Public License as published by                               !
!  the Free Software Foundation, either version 3 of the License, or                                  !
!  (at your option) any later version.                                                                !
!                                                                                                     !
!  ALaDyn is distributed in the hope that it will be useful,                                          !
!  but WITHOUT ANY WARRANTY; without even the implied warranty of                                     !
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                      !
!  GNU General Public License for more details.                                                       !
!                                                                                                     !
!  You should have received a copy of the GNU General Public License                                  !
!  along with ALaDyn.  If not, see <http://www.gnu.org/licenses/>.                                    !
!*****************************************************************************************************!

module read_json

 use json_module
 use precision_def
 use sim_params_types

 implicit none

 type(json_file)    :: namelist

 contains

 subroutine read_input_json( input_name, namelist_in, parameters_out )
  !! Subroutines that reads the JSON inputn file, if found
  character(LEN=:), allocatable, intent(in) :: input_name
  type(json_file), intent(inout) :: namelist_in
  type(parameters_t), intent(inout) :: parameters_out
  type(json_value), pointer :: grid => null()
  type(json_value), pointer :: sim => null()
  type(json_value), pointer :: targ_desc => null()
  type(json_value), pointer :: laser => null()
  type(json_value), pointer :: beam_inject => null()
  type(json_value), pointer :: window => null()
  type(json_value), pointer :: output => null()
  type(json_value), pointer :: tracking => null()
  type(json_value), pointer :: mpi => null()
  logical :: status_ok, found
  character(:), allocatable :: error

  ! Initializing the json file
  ! Enabling the comments on lines that start with !
  call namelist_in%initialize(comment_char=json_CK_'!', strict_integer_type_checking=.false.)

  ! Loading the file
  call namelist_in%load_file(trim(input_name))
  if ( namelist_in%failed() ) then
   write(6, *) 'Error while reading ', trim(input_name)
   stop
  end if
  ! Check the namelist for errors
  call namelist_in%check_for_errors(status_ok, error_msg = error)
  if ( .not. status_ok ) then
   write(6, *) error
   stop
  end if

  !========================================
  ! Read grid parameters
  !========================================
  
  call namelist_in%get('grid', grid, found)
  if ( .not. found ) then
   write(6, *) "'Grid' not found in json input"
   stop
  end if
  allocate(parameters_out%grid_params)
  call read_json_grid( grid, parameters_out%grid_params )
  parameters_out%exists_grid = .true.

  !========================================
  ! Read simulation parameters
  !========================================
  
  call namelist_in%get('simulation', sim, found)
  if ( .not. found ) then
   write(6, *) "'Simulation' not found in json input"
   stop
  end if
  allocate(parameters_out%sim_params)
  call read_json_sim( sim, parameters_out%sim_params )
  parameters_out%exists_simulation = .true.  

  !========================================
  ! Read target parameters
  !========================================
  
  call namelist_in%get('target_description', targ_desc, found)
  if ( .not. found ) then
   ! write(6, *) "'Target_description' not found in json input"
  else
   allocate(parameters_out%targ_params)
   call read_json_targ( targ_desc, parameters_out%targ_params )
   parameters_out%exists_target = .true.
  end if

  !========================================
  ! Read laser parameters
  !========================================
  
  call namelist_in%get('laser', laser, found)
  if ( .not. found ) then
   ! write(6, *) "'laser' not found in json input"
  else
   allocate(parameters_out%laser_params)
   call read_json_laser( laser, parameters_out%laser_params )
   parameters_out%exists_laser = .true.
  end if
  !========================================
  ! Read beam parameters
  !========================================
  
  call namelist_in%get('beam_inject', beam_inject, found)
  if ( .not. found ) then
   ! write(6, *) "'beam_inject' not found in json input"
  else
   allocate(parameters_out%beam_params)
   call read_json_beam( beam_inject, parameters_out%beam_params )
   parameters_out%exists_beam = .true.
  end if

  !========================================
  ! Read window parameters
  !========================================
  
  call namelist_in%get('moving_window', window, found)
  if ( .not. found ) then
   ! write(6, *) "'moving_window' not found in json input"
  else
   allocate(parameters_out%window_params)
   call read_json_window( window, parameters_out%window_params )
   parameters_out%exists_window = .true.
  end if

  !========================================
  ! Read output parameters
  !========================================
  
  call namelist_in%get('output', output, found)
  if ( .not. found ) then
   write(6, *) "'output' not found in json input"
   stop
  end if
  allocate(parameters_out%output_params)
  call read_json_output( output, parameters_out%output_params )
  parameters_out%exists_output = .true.

  !========================================
  ! Read tracking parameters
  !========================================
  
  call namelist_in%get('tracking', tracking, found)
  if ( .not. found ) then
   ! write(6, *) "'tracking' not found in json input"
  else
   allocate(parameters_out%track_params)
   call read_json_tracking( tracking, parameters_out%track_params ) 
   parameters_out%exists_tracking = .true.
  end if

  !========================================
  ! Read MPI parameters
  !========================================
  
  call namelist_in%get('mpiparams', mpi, found)
  if ( .not. found ) then
   ! write(6, *) "'mpiparams' not found in json input"
  else
   allocate(parameters_out%mpi_params)
   call read_json_mpi( mpi, parameters_out%mpi_params )
   parameters_out%exists_mpi = .true.
  end if

  call namelist_in%destroy()

 end subroutine

 subroutine read_json_grid( grid_json, grid_params)
  type(json_value), pointer, intent(in) :: grid_json
  type(grid_parameters_t), intent(inout) :: grid_params
  type(json_core) :: json
  type(grid_parameters_t) :: grid_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(grid_json, 'nx', grid_params%nx, found, &
   default=grid_params_init%nx)
  call json%get(grid_json, 'ny', grid_params%ny, found, &
   default=grid_params_init%ny)
  call json%get(grid_json, 'nz', grid_params%nz, found, &
   default=grid_params_init%nz)
  call json%get(grid_json, 'ny_targ', grid_params%ny_targ, found, &
   default=grid_params_init%ny_targ)
  call json%get(grid_json, 'k0', grid_params%k0, found, &
   default=grid_params_init%k0)
  call json%get(grid_json, 'yx_rat', grid_params%yx_rat, found, &
   default=grid_params_init%yx_rat)
  call json%get(grid_json, 'zx_rat', grid_params%zx_rat, found, &
   default=grid_params_init%zx_rat)

  call json%destroy()
 end subroutine

 subroutine read_json_sim( sim_json, sim_params)
  type(json_value), pointer, intent(in) :: sim_json
  type(simulation_parameters_t), intent(inout) :: sim_params
  type(json_core) :: json
  type(simulation_parameters_t) :: sim_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(sim_json, 'lpf_ord', sim_params%lpf_ord, found, &
   default=sim_params_init%lpf_ord)
  call json%get(sim_json, 'der_ord', sim_params%der_ord, found, &
   default=sim_params_init%der_ord)
  call json%get(sim_json, 'str_flag', sim_params%str_flag, found, &
   default=sim_params_init%str_flag)
  call json%get(sim_json, 'iform', sim_params%iform, found, &
   default=sim_params_init%iform)
  call json%get(sim_json, 'model_id', sim_params%model_id, found, &
   default=sim_params_init%model_id)
  call json%get(sim_json, 'dmodel_id', sim_params%dmodel_id, found, &
   default=sim_params_init%dmodel_id)
  call json%get(sim_json, 'ibx', sim_params%ibx, found, &
   default=sim_params_init%ibx)
  call json%get(sim_json, 'iby', sim_params%iby, found, &
   default=sim_params_init%iby)
  call json%get(sim_json, 'ibz', sim_params%ibz, found, &
   default=sim_params_init%ibz)
  call json%get(sim_json, 'ibeam', sim_params%ibeam, found, &
   default=sim_params_init%ibeam)
  call json%get(sim_json, 'density_limiter', sim_params%density_limiter, found, &
   default=sim_params_init%density_limiter)
  call json%get(sim_json, 'pusher', sim_params%pusher, found, &
   default=sim_params_init%pusher)
  call json%get(sim_json, 'n_substeps', sim_params%n_substeps, found, &
   default=sim_params_init%n_substeps)

  call json%destroy()
 end subroutine

 subroutine read_json_targ( targ_json, targ_params)
  type(json_value), pointer, intent(in) :: targ_json
  type(targ_description_parameters_t), intent(inout) :: targ_params
  type(json_core) :: json
  type(targ_description_parameters_t):: targ_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(targ_json, 'nsp', targ_params%nsp, found, &
   default=targ_params_init%nsp)
  call json%get(targ_json, 'nsb', targ_params%nsb, found, &
   default=targ_params_init%nsb)
  call json%get(targ_json, 'ionz_lev', targ_params%ionz_lev, found, &
   default=targ_params_init%ionz_lev)
  call json%get(targ_json, 'ion_min', targ_params%ion_min, found, &
   default=targ_params_init%ion_min)
  call json%get(targ_json, 'ion_max', targ_params%ion_max, found, &
   default=targ_params_init%ion_max)
  call json%get(targ_json, 'atomic_number', targ_params%atomic_number, found, &
   default=targ_params_init%atomic_number)
  call json%get(targ_json, 'mass_number', targ_params%mass_number, found, &
   default=targ_params_init%mass_number)
  call json%get(targ_json, 't0_pl', targ_params%t0_pl, found, &
   default=targ_params_init%t0_pl)
  call json%get(targ_json, 'np_per_xc', targ_params%np_per_xc, found, &
   default=targ_params_init%np_per_xc)
  call json%get(targ_json, 'np_per_yc', targ_params%np_per_yc, found, &
   default=targ_params_init%np_per_yc)
  call json%get(targ_json, 'np_per_zc', targ_params%np_per_zc, found, &
   default=targ_params_init%np_per_zc)
  call json%get(targ_json, 'concentration', targ_params%concentration, found, &
   default=targ_params_init%concentration)
  call json%get(targ_json, 'transverse_dist', targ_params%transverse_dist, found, &
   default=targ_params_init%transverse_dist)
  call json%get(targ_json, 'lpx', targ_params%lpx, found, &
   default=targ_params_init%lpx)
  call json%get(targ_json, 'lpy', targ_params%lpy, found, &
   default=targ_params_init%lpy)
  call json%get(targ_json, 'n0_ref', targ_params%n0_ref, found, &
   default=targ_params_init%n0_ref)
  call json%get(targ_json, 'np1', targ_params%np1, found, &
   default=targ_params_init%np1)
  call json%get(targ_json, 'np2', targ_params%np2, found, &
   default=targ_params_init%np2)
  call json%get(targ_json, 'r_c', targ_params%r_c, found, &
   default=targ_params_init%r_c)
  call json%get(targ_json, 'l_disable_rng_seed', targ_params%l_disable_rng_seed, found, &
   default=targ_params_init%l_disable_rng_seed)

  call json%destroy()
 end subroutine

 subroutine read_json_laser( laser_json, laser_params)
  type(json_value), pointer, intent(in) :: laser_json
  type(laser_parameters_t), intent(inout) :: laser_params
  type(json_core) :: json
  type(laser_parameters_t):: laser_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(laser_json, 'nb_laser', laser_params%nb_laser, found, &
   default=laser_params_init%nb_laser)
  call json%get(laser_json, 'g_prof', laser_params%g_prof, found, &
   default=laser_params_init%g_prof)
  call json%get(laser_json, 't0_lp', laser_params%t0_lp, found, &
   default=laser_params_init%t0_lp)
  call json%get(laser_json, 'xc_lp', laser_params%xc_lp, found, &
   default=laser_params_init%xc_lp)
  call json%get(laser_json, 'tau_fwhm', laser_params%tau_fwhm, found, &
   default=laser_params_init%tau_fwhm)
  call json%get(laser_json, 'w0_y', laser_params%w0_y, found, &
   default=laser_params_init%w0_y)
  call json%get(laser_json, 'a0', laser_params%a0, found, &
   default=laser_params_init%a0)
  call json%get(laser_json, 'lam0', laser_params%lam0, found, &
   default=laser_params_init%lam0)
  call json%get(laser_json, 'lp_delay', laser_params%lp_delay, found, &
   default=laser_params_init%lp_delay)
  call json%get(laser_json, 'lp_offset', laser_params%lp_offset, found, &
   default=laser_params_init%lp_offset)
  call json%get(laser_json, 't1_lp', laser_params%t1_lp, found, &
   default=laser_params_init%t1_lp)
  call json%get(laser_json, 'tau1_fwhm', laser_params%tau1_fwhm, found, &
   default=laser_params_init%tau1_fwhm)
  call json%get(laser_json, 'w1_y', laser_params%w1_y, found, &
   default=laser_params_init%w1_y)
  call json%get(laser_json, 'a1', laser_params%a1, found, &
   default=laser_params_init%a1)
  call json%get(laser_json, 'lam1', laser_params%lam1, found, &
   default=laser_params_init%lam1)
  call json%get(laser_json, 'symmetrization_pulse', laser_params%symmetrization_pulse, found, &
   default=laser_params_init%symmetrization_pulse)
  call json%get(laser_json, 'a_symm_rat', laser_params%a_symm_rat, found, &
   default=laser_params_init%a_symm_rat)
  call json%get(laser_json, 'enable_ionization', laser_params%enable_ionization, found, &
   default=laser_params_init%enable_ionization)
  call json%get(laser_json, 'y0_cent', laser_params%y0_cent, found, &
   default=laser_params_init%y0_cent)
  call json%get(laser_json, 'z0_cent', laser_params%z0_cent, found, &
   default=laser_params_init%z0_cent)
  call json%get(laser_json, 'y1_cent', laser_params%y1_cent, found, &
   default=laser_params_init%y1_cent)
  call json%get(laser_json, 'z1_cent', laser_params%z1_cent, found, &
   default=laser_params_init%z1_cent)
  call json%get(laser_json, 'incid_angle', laser_params%incid_angle, found, &
   default=laser_params_init%incid_angle)
  call json%get(laser_json, 'improved_envelope', laser_params%improved_envelope, found, &
   default=laser_params_init%improved_envelope)

  call json%destroy()
 end subroutine

 subroutine read_json_beam( beam_json, beam_params)
  type(json_value), pointer, intent(in) :: beam_json
  type(beam_parameters_t), intent(inout) :: beam_params
  type(json_core) :: json
  type(beam_parameters_t):: beam_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(beam_json, 'nb_1', beam_params%nb_1, found, &
   default=beam_params_init%nb_1)
  call json%get(beam_json, 'xc_1', beam_params%xc_1, found, &
   default=beam_params_init%xc_1)
  call json%get(beam_json, 'gam_1', beam_params%gam_1, found, &
   default=beam_params_init%gam_1)
  call json%get(beam_json, 'sxb_1', beam_params%sxb_1, found, &
   default=beam_params_init%sxb_1)
  call json%get(beam_json, 'syb_1', beam_params%syb_1, found, &
   default=beam_params_init%syb_1)
  call json%get(beam_json, 'epsy_1', beam_params%epsy_1, found, &
   default=beam_params_init%epsy_1)
  call json%get(beam_json, 'epsz_1', beam_params%epsz_1, found, &
   default=beam_params_init%epsz_1)
  call json%get(beam_json, 'dg_1', beam_params%dg_1, found, &
   default=beam_params_init%dg_1)
  call json%get(beam_json, 'charge_1', beam_params%charge_1, found, &
   default=beam_params_init%charge_1)
  call json%get(beam_json, 'ap1_twiss', beam_params%ap1_twiss, found, &
   default=beam_params_init%ap1_twiss)
  call json%get(beam_json, 'bt1_twiss', beam_params%bt1_twiss, found, &
   default=beam_params_init%bt1_twiss)
  call json%get(beam_json, 't_inject', beam_params%t_inject, found, &
   default=beam_params_init%t_inject)

  call json%destroy()
 end subroutine

 subroutine read_json_window( window_json, window_params)
  type(json_value), pointer, intent(in) :: window_json
  type(window_parameters_t), intent(inout) :: window_params
  type(json_core) :: json
  type(window_parameters_t):: window_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(window_json, 'w_sh', window_params%w_sh, found, &
   default=window_params_init%w_sh)
  call json%get(window_json, 'wi_time', window_params%wi_time, found, &
   default=window_params_init%wi_time)
  call json%get(window_json, 'wf_time', window_params%wf_time, found, &
   default=window_params_init%wf_time)
  call json%get(window_json, 'w_speed', window_params%w_speed, found, &
   default=window_params_init%w_speed)

  call json%destroy()
 end subroutine

 subroutine read_json_output( output_json, output_params)
  type(json_value), pointer, intent(in) :: output_json
  type(output_parameters_t), intent(inout) :: output_params
  type(json_core) :: json
  type(output_parameters_t):: output_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(output_json, 'nouts', output_params%nouts, found, &
   default=output_params_init%nouts)
  call json%get(output_json, 'iene', output_params%iene, found, &
   default=output_params_init%iene)
  call json%get(output_json, 'nvout', output_params%nvout, found, &
   default=output_params_init%nvout)
  call json%get(output_json, 'nden', output_params%nden, found, &
   default=output_params_init%nden)
  call json%get(output_json, 'ncurr', output_params%ncurr, found, &
   default=output_params_init%ncurr)
  call json%get(output_json, 'npout', output_params%npout, found, &
   default=output_params_init%npout)
  call json%get(output_json, 'jump', output_params%jump, found, &
   default=output_params_init%jump)
  call json%get(output_json, 'pjump', output_params%pjump, found, &
   default=output_params_init%pjump)
  call json%get(output_json, 'gam_min', output_params%gamma_min, found, &
   default=output_params_init%gamma_min)
  call json%get(output_json, 'xp0_out', output_params%xp0_out, found, &
   default=output_params_init%xp0_out)
  call json%get(output_json, 'xp1_out', output_params%xp1_out, found, &
   default=output_params_init%xp1_out)
  call json%get(output_json, 'yp_out', output_params%yp_out, found, &
   default=output_params_init%yp_out)
  call json%get(output_json, 'tmax', output_params%tmax, found, &
   default=output_params_init%tmax)
  call json%get(output_json, 'cfl', output_params%cfl, found, &
   default=output_params_init%cfl)
  call json%get(output_json, 'new_sim', output_params%new_sim, found, &
   default=output_params_init%new_sim)
  call json%get(output_json, 'id_new', output_params%id_new, found, &
   default=output_params_init%id_new)
  call json%get(output_json, 'dump', output_params%dump, found, &
   default=output_params_init%dump)
  call json%get(output_json, 'time_interval_dumps', output_params%time_interval_dumps, found, &
   default=output_params_init%time_interval_dumps)
  call json%get(output_json, 'l_force_singlefile_output', output_params%l_force_single_output, found, &
   default=output_params_init%l_force_single_output)
  call json%get(output_json, 'l_print_j_on_grid', output_params%l_print_j_on_grid, found, &
   default=output_params_init%l_print_j_on_grid)
  call json%get(output_json, 'l_first_output_on_restart', output_params%l_first_output_on_restart, found, &
   default=output_params_init%l_first_output_on_restart)
  call json%get(output_json, 'l_env_modulus', output_params%l_env_modulus, found, &
   default=output_params_init%l_env_modulus)

  call json%destroy()
 end subroutine

 subroutine read_json_tracking( tracking_json, tracking_params)
  type(json_value), pointer, intent(in) :: tracking_json
  type(tracking_parameters_t), intent(inout) :: tracking_params
  type(json_core) :: json
  type(tracking_parameters_t):: tracking_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(tracking_json, 'every_track', tracking_params%every_track, found, &
   default=tracking_params_init%every_track)
  call json%get(tracking_json, 'nkjump', tracking_params%nkjump, found, &
   default=tracking_params_init%nkjump)
  call json%get(tracking_json, 'txmin', tracking_params%txmin, found, &
   default=tracking_params_init%txmin)
  call json%get(tracking_json, 'txmax', tracking_params%txmax, found, &
   default=tracking_params_init%txmax)
  call json%get(tracking_json, 'tymin', tracking_params%tymin, found, &
   default=tracking_params_init%tymin)
  call json%get(tracking_json, 'tymax', tracking_params%tymax, found, &
   default=tracking_params_init%tymax)
  call json%get(tracking_json, 'tzmin', tracking_params%tzmin, found, &
   default=tracking_params_init%tzmin)
  call json%get(tracking_json, 'tzmax', tracking_params%tzmax, found, &
   default=tracking_params_init%tzmax)
  call json%get(tracking_json, 't_in', tracking_params%t_in, found, &
   default=tracking_params_init%t_in)
  call json%get(tracking_json, 't_out', tracking_params%t_out, found, &
   default=tracking_params_init%t_out)
  call json%get(tracking_json, 'p_tracking', tracking_params%p_tracking, found, &
   default=tracking_params_init%p_tracking)
  call json%get(tracking_json, 'a_on_particles', tracking_params%a_on_particles, found, &
   default=tracking_params_init%a_on_particles)

  call json%destroy()
 end subroutine
 
 subroutine read_json_mpi( mpi_json, mpi_params)
  type(json_value), pointer, intent(in) :: mpi_json
  type(mpi_parameters_t), intent(inout) :: mpi_params
  type(json_core) :: json
  type(mpi_parameters_t):: mpi_params_init
  logical :: found

  call json%initialize(comment_char=json_CK_'!')

  call json%get(mpi_json, 'nprocx', mpi_params%nprocx, found, &
   default=mpi_params_init%nprocx)
  call json%get(mpi_json, 'nprocy', mpi_params%nprocy, found, &
   default=mpi_params_init%nprocy)
  call json%get(mpi_json, 'nprocz', mpi_params%nprocz, found, &
   default=mpi_params_init%nprocz)

  call json%destroy()
 end subroutine

end module
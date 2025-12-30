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

 module read_input

  use read_namelist
#if defined(JSON_FORTRAN)
  use read_json
#endif
  use sim_params_types

  implicit none

 contains


  subroutine read_main_input(parameters_out, exist_json_out)
  !! Reads the input file
   type(parameters_t), intent(inout) :: parameters_out
   logical, intent(inout) :: exist_json_out
   logical :: exist_nml = .false.

   input_json_filename = 'input.json'
   input_namelist_filename = 'input.nml'
   inquire (file=input_namelist_filename, exist=exist_nml)

#if defined(JSON_FORTRAN)
   inquire (file=input_json_filename, exist=exist_json_out)   
   if (exist_json_out) then
    call read_input_json( input_json_filename, namelist, parameters_out)
   end if
#endif

   if (.not. exist_json_out .and. exist_nml) then
    write (6, *) '==========================================='
    write (6, *) 'No JSON input file has been found.'
    write (6, *) 'Instead, the old nml format will be used.'
    write (6, *) '==========================================='
    call read_input_nml
   else if ((.not. exist_json_out) .and. (.not. exist_nml)) then
    write (6, *) 'No input file (.json or .nml) has been found'
    stop
   end if
  end subroutine

#if defined(JSON_FORTRAN)
  subroutine assign_parameters( parameters_in, exist_json_in )
  !! Assignes all the parameters contained in parameters_in and read from the json input file
  !! to the global parameters used in the simulation
    type(parameters_t), intent(in) :: parameters_in
    logical, intent(in) :: exist_json_in
    integer :: sz

    if (.not. exist_json_in) then
      return
    end if
    !========================
    ! Assign grid parameters
    !========================
    if( .not. allocated(parameters_in%grid_params) ) then
      write(6, *) 'ERROR: grid parameters not found'
      stop
    end if
    nx = parameters_in%grid_params%nx
    ny = parameters_in%grid_params%ny
    nz = parameters_in%grid_params%nz
    ny_targ = parameters_in%grid_params%ny_targ
    k0 = parameters_in%grid_params%k0
    yx_rat = parameters_in%grid_params%yx_rat
    zx_rat = parameters_in%grid_params%zx_rat

    !==============================
    ! Assign simulation parameters
    !==============================
    if( .not. allocated(parameters_in%sim_params) ) then
      write(6, *) 'ERROR: simulation parameters not found'
      stop
    end if
    lpf_ord = parameters_in%sim_params%lpf_ord
    der_ord = parameters_in%sim_params%der_ord
    str_flag = parameters_in%sim_params%str_flag
    iform = parameters_in%sim_params%iform
    model_id = parameters_in%sim_params%model_id
    dmodel_id = parameters_in%sim_params%dmodel_id
    ibx = parameters_in%sim_params%ibx
    iby = parameters_in%sim_params%iby
    ibz = parameters_in%sim_params%ibz
    ibeam = parameters_in%sim_params%ibeam
    density_limiter = parameters_in%sim_params%density_limiter
    pusher = parameters_in%sim_params%pusher
    n_substeps = parameters_in%sim_params%n_substeps

    !==============================
    ! Assign target parameters
    !==============================
    if( allocated(parameters_in%targ_params) ) then
      nsp = parameters_in%targ_params%nsp
      nsb = parameters_in%targ_params%nsb
      ionz_lev = parameters_in%targ_params%ionz_lev
      ionz_model = parameters_in%targ_params%ionz_model

      if (allocated(parameters_in%targ_params%ion_min)) then
        ion_min(1:nsp - 1) = parameters_in%targ_params%ion_min(1:nsp - 1)
      else
        if (nsp > 1 .and. ionz_lev > 0) then
          write(6, *) "Please specify the ionization level 'ion_min'"
          stop
        end if
        ion_min(:) = 1
      end if

      if (allocated(parameters_in%targ_params%ion_max)) then
        ion_max(1:nsp - 1) = parameters_in%targ_params%ion_max(1:nsp - 1)
      else
        if (nsp > 1 .and. ionz_lev > 0) then
          write(6, *) "Please specify the ionization level 'ion_max'"
          stop
        end if
        ion_max(:) = 1
      end if

      if (allocated(parameters_in%targ_params%atomic_number)) then
        atomic_number(1:nsp - 1) = parameters_in%targ_params%atomic_number(1:nsp - 1)
      else
        if (nsp > 1) then
          write(6, *) "Please specify the species 'atomic_number'"
          stop
        end if
        atomic_number(:) = 1
      end if

      if (allocated(parameters_in%targ_params%mass_number)) then
        mass_number(1:nsp - 1) = parameters_in%targ_params%mass_number(1:nsp - 1)
      else
        if (nsp > 1) then
          write(6, *) "Please specify the species 'mass_number'"
          stop
        end if
        mass_number(:) = 1
      end if

      if (allocated(parameters_in%targ_params%t0_pl)) then
        sz = size(parameters_in%targ_params%t0_pl)
        if (sz < nsp) then
          t0_pl(1:sz) = parameters_in%targ_params%t0_pl(1:sz)
          t0_pl(sz + 1:nsp) = zero_dp
        else if (sz >= nsp) then
          t0_pl(1:nsp) = parameters_in%targ_params%t0_pl(1:nsp)
        end if
      else
        t0_pl(1:nsp) = zero_dp
      end if

      if (allocated(parameters_in%targ_params%np_per_xc)) then
        sz = size(parameters_in%targ_params%np_per_xc)
        if (sz < nsp) then
          np_per_xc(1:sz) = parameters_in%targ_params%np_per_xc(1:sz)
          np_per_xc(sz + 1:nsp) = -1
        else if (sz >= nsp) then
          np_per_xc(1:nsp) = parameters_in%targ_params%np_per_xc(1:nsp)
        end if
      else
        np_per_xc(1:nsp) = -1
      end if

      if (allocated(parameters_in%targ_params%np_per_yc)) then
        sz = size(parameters_in%targ_params%np_per_yc)
        if (sz < nsp) then
          np_per_yc(1:sz) = parameters_in%targ_params%np_per_yc(1:sz)
          np_per_yc(sz + 1:nsp) = -1
        else if (sz >= nsp) then
          np_per_yc(1:nsp) = parameters_in%targ_params%np_per_yc(1:nsp)
        end if
      else
        np_per_yc(1:nsp) = -1
      end if

      if (allocated(parameters_in%targ_params%np_per_zc)) then
        sz = size(parameters_in%targ_params%np_per_zc)
        if (sz < nsp) then
          np_per_zc(1:sz) = parameters_in%targ_params%np_per_zc(1:sz)
          np_per_zc(sz + 1:nsp) = -1
        else if (sz >= nsp) then
          np_per_zc(1:nsp) = parameters_in%targ_params%np_per_zc(1:nsp)
        end if
      else
        np_per_zc(1:nsp) = -1
      end if

      if (allocated(parameters_in%targ_params%concentration)) then
        sz = size(parameters_in%targ_params%concentration)
        if (sz < nsp) then
          concentration(1:sz) = parameters_in%targ_params%concentration(1:sz)
          concentration(sz + 1:nsp) = zero_dp
        else if (sz >= nsp) then
          concentration(1:nsp) = parameters_in%targ_params%concentration(1:nsp)
        end if
      else
        concentration(1:nsp) = zero_dp
        concentration(1) = one_dp
      end if

      if (allocated(parameters_in%targ_params%lpx)) then
        if (size(parameters_in%targ_params%lpx) < 7) then
          write(6, *) "'lpx' size must be of 7 elements"
          stop
        end if
        lpx(1:7) = parameters_in%targ_params%lpx(1:7)
      else
        lpx(1:7) = zero_dp
      end if

      if (allocated(parameters_in%targ_params%lpy)) then
        if (size(parameters_in%targ_params%lpy) < 2) then
          write(6, *) "'lpy' size must be of 2 elements"
          stop
        end if
        lpy(1:2) = parameters_in%targ_params%lpy(1:2)
      else
        lpy(1:2) = zero_dp
      end if

      transverse_dist = parameters_in%targ_params%transverse_dist
      n0_ref = parameters_in%targ_params%n0_ref
      np1 = parameters_in%targ_params%np1
      np2 = parameters_in%targ_params%np2
      r_c = parameters_in%targ_params%r_c
      l_disable_rng_seed = parameters_in%targ_params%l_disable_rng_seed
    else
      nsp = 0
      np_per_xc(:) = 0
      np_per_yc(:) = 0
      np_per_zc(:) = 0
      lpx(:) = zero_dp
      lpy(:) = zero_dp
    end if

    !==============================
    ! Assign laser parameters
    !==============================
    if( allocated(parameters_in%laser_params) ) then
      g_prof = parameters_in%laser_params%g_prof
      nb_laser = parameters_in%laser_params%nb_laser
      t0_lp = parameters_in%laser_params%t0_lp
      xc_lp = parameters_in%laser_params%xc_lp
      tau_fwhm = parameters_in%laser_params%tau_fwhm
      w0_y = parameters_in%laser_params%w0_y
      a0 = parameters_in%laser_params%a0
      lam0 = parameters_in%laser_params%lam0

      if (allocated(parameters_in%laser_params%lp_delay)) then
        sz = size(parameters_in%laser_params%lp_delay)
        if (sz < nb_laser - 1) then
          lp_delay(1:sz) = parameters_in%laser_params%lp_delay(1:sz)
          lp_delay(sz + 1: nb_laser - 1) = zero_dp
        else if (sz >= nb_laser - 1) then
          lp_delay(1:nb_laser - 1) = parameters_in%laser_params%lp_delay(1:nb_laser - 1)
        end if
      else
        if (nb_laser > 1) then
          write(6, *) "Please specify the delay between laser pusles with 'lp_delay'"
          stop
        end if
        lp_delay(1:nb_laser - 1 ) = zero_dp
      end if

      if (parameters_in%laser_params%lp_offset /= 0) then
        lp_offset = parameters_in%laser_params%lp_offset
      end if

      t1_lp = parameters_in%laser_params%t1_lp
      tau1_fwhm = parameters_in%laser_params%tau1_fwhm
      w1_y = parameters_in%laser_params%w1_y
      a1 = parameters_in%laser_params%a1
      lam1 = parameters_in%laser_params%lam1
      symmetrization_pulse = parameters_in%laser_params%symmetrization_pulse
      a_symm_rat = parameters_in%laser_params%a_symm_rat

      if (allocated(parameters_in%laser_params%enable_ionization)) then
        select case(size(parameters_in%laser_params%enable_ionization, DIM=1))
        case(1)
          enable_ionization(1) = parameters_in%laser_params%enable_ionization(1)
          enable_ionization(2) = .true.
        case(2)
          enable_ionization(1) = parameters_in%laser_params%enable_ionization(1)
          enable_ionization(2) = parameters_in%laser_params%enable_ionization(2)
        case default
          enable_ionization(1) = .true.
          enable_ionization(2) = .true.
        end select
      else
        enable_ionization(:) = .true.
      end if

      if (allocated(parameters_in%laser_params%y0_cent)) then
        sz = size(parameters_in%laser_params%y0_cent)
        if (sz < nb_laser) then
          y0_cent(1:sz) = parameters_in%laser_params%y0_cent(1:sz)
          y0_cent(sz + 1:nb_laser) = zero_dp
        else if (sz >= nb_laser) then
          y0_cent(1:nb_laser) = parameters_in%laser_params%y0_cent(1:nb_laser)
        end if
      else
        y0_cent(1:nb_laser) = zero_dp
      end if

      if (allocated(parameters_in%laser_params%z0_cent)) then
        sz = size(parameters_in%laser_params%z0_cent)
        if (sz < nb_laser) then
          z0_cent(1:sz) = parameters_in%laser_params%z0_cent(1:sz)
          z0_cent(sz + 1:nb_laser) = zero_dp
        else if (sz >= nb_laser) then
          z0_cent(1:nb_laser) = parameters_in%laser_params%z0_cent(1:nb_laser)
        end if
      else
        z0_cent(1:nb_laser) = zero_dp
      end if

      y1_cent = parameters_in%laser_params%y1_cent
      z1_cent = parameters_in%laser_params%z1_cent
      incid_angle = parameters_in%laser_params%incid_angle
      improved_envelope = parameters_in%laser_params%improved_envelope
    else
      nb_laser = 1
      a0 = zero_dp
      lam0 = one_dp
      xc_lp = zero_dp
      tau_fwhm = one_dp
      w0_y = one_dp
    end if

    !==============================
    ! Assign beam parameters
    !==============================
    if( allocated(parameters_in%beam_params) ) then
      nb_1 = parameters_in%beam_params%nb_1
      xc_1 = parameters_in%beam_params%xc_1
      gam_1 = parameters_in%beam_params%gam_1
      sxb_1 = parameters_in%beam_params%sxb_1
      syb_1 = parameters_in%beam_params%syb_1
      epsy_1 = parameters_in%beam_params%epsy_1
      epsz_1 = parameters_in%beam_params%epsz_1
      dg_1 = parameters_in%beam_params%dg_1
      charge_1 = parameters_in%beam_params%charge_1
      ap1_twiss = parameters_in%beam_params%ap1_twiss
      bt1_twiss = parameters_in%beam_params%bt1_twiss
      t_inject = parameters_in%beam_params%t_inject
    else
      nsb = 0
    end if

    !==============================
    ! Assign window parameters
    !==============================
    if( allocated(parameters_in%window_params) ) then
      w_sh = parameters_in%window_params%w_sh
      wi_time = parameters_in%window_params%wi_time
      wf_time = parameters_in%window_params%wf_time
      w_speed = parameters_in%window_params%w_speed
    else
      w_sh = 0
      wi_time = zero_dp
      wf_time = zero_dp
      w_speed = zero_dp
    end if

    !==============================
    ! Assign output parameters
    !==============================
    if( .not. allocated(parameters_in%output_params) ) then
      write(6, *) 'ERROR: output parameters not found'
      stop
    end if
    nouts = parameters_in%output_params%nouts
    iene = parameters_in%output_params%iene
    nvout = parameters_in%output_params%nvout
    nden = parameters_in%output_params%nden
    ncurr = parameters_in%output_params%ncurr
    npout = parameters_in%output_params%npout
    jump = parameters_in%output_params%jump
    pjump = parameters_in%output_params%pjump
    gam_min = parameters_in%output_params%gamma_min
    xp0_out = parameters_in%output_params%xp0_out
    xp1_out = parameters_in%output_params%xp1_out
    yp_out = parameters_in%output_params%yp_out
    tmax = parameters_in%output_params%tmax
    cfl = parameters_in%output_params%cfl
    new_sim = parameters_in%output_params%new_sim
    id_new = parameters_in%output_params%id_new
    dump = parameters_in%output_params%dump
    l_force_singlefile_output = parameters_in%output_params%l_force_single_output
    l_print_j_on_grid = parameters_in%output_params%l_print_j_on_grid
    l_first_output_on_restart = parameters_in%output_params%l_first_output_on_restart
    l_env_modulus = parameters_in%output_params%l_env_modulus
    time_interval_dumps = parameters_in%output_params%time_interval_dumps

    !==============================
    ! Assign MPI parameters
    !==============================
    if( allocated(parameters_in%mpi_params) ) then
      nprocx = parameters_in%mpi_params%nprocx
      nprocy = parameters_in%mpi_params%nprocy
      nprocz = parameters_in%mpi_params%nprocz
    else
      nprocx = 1
      nprocy = 1
      nprocz = 1
    end if

    
    !==============================
    ! Assign tracking parameters
    !==============================
    if( allocated(parameters_in%track_params) ) then
      p_tracking = parameters_in%track_params%p_tracking
      a_on_particles = parameters_in%track_params%a_on_particles
      every_track = parameters_in%track_params%every_track
      nkjump = parameters_in%track_params%nkjump
      txmin = parameters_in%track_params%txmin
      txmax = parameters_in%track_params%txmax
      tymin = parameters_in%track_params%tymin
      tymax = parameters_in%track_params%tymax
      tzmin = parameters_in%track_params%tzmin
      tzmax = parameters_in%track_params%tzmax
      t_in = parameters_in%track_params%t_in
      t_out = parameters_in%track_params%t_out
    else
      p_tracking = .false.
    end if
 
  end subroutine

  subroutine write_json_checklist(parameters_in)
    !! Writes the checklist with all the read parameters.
    !! @note In a future release this should be changed
    !!       and the code should instead refer to the
    !!       `parameter` structure instead of the global variables.

    type(parameters_t), intent(in) :: parameters_in
    character(13) :: filename_json_out
    type(json_core) :: json
    type(json_value), pointer :: p => null()
    type(json_value), pointer :: json_group => null()

    ! Initialize the json object
    call json%initialize()
    ! Create check file for input.nml
    call json%create_object(p, '')

    ! Construct the file

    ! Add the grid
    call json%create_object(json_group, 'grid')
    call json%add(p, json_group)
    call json%add(json_group, 'nx', nx)
    call json%add(json_group, 'ny', ny)
    call json%add(json_group, 'nz', nz)
    call json%add(json_group, 'ny_targ', ny_targ)
    call json%add(json_group, 'k0', k0)
    call json%add(json_group, 'yx_rat', yx_rat)
    call json%add(json_group, 'zx_rat', zx_rat)
    nullify(json_group)

    ! Add the simulation
    call json%create_object(json_group, 'simulation')
    call json%add(p, json_group)
    call json%add(json_group, 'lpf_ord', lpf_ord)
    call json%add(json_group, 'der_ord', der_ord)
    call json%add(json_group, 'str_flag', str_flag)
    call json%add(json_group, 'iform', iform)
    call json%add(json_group, 'model_id', model_id)
    call json%add(json_group, 'dmodel_id', dmodel_id)
    call json%add(json_group, 'ibx', ibx)
    call json%add(json_group, 'iby', iby)
    call json%add(json_group, 'ibz', ibz)
    call json%add(json_group, 'ibeam', ibeam)
    call json%add(json_group, 'density_limiter', density_limiter)
    call json%add(json_group, 'pusher', pusher)
    call json%add(json_group, 'n_substeps', n_substeps)
    nullify(json_group)

    ! Add the target
    if (parameters_in%exists_target) then
      call json%create_object(json_group, 'target_description')
      call json%add(p, json_group)
      call json%add(json_group, 'nsp', nsp)
      call json%add(json_group, 'nsb', nsb)
      call json%add(json_group, 'ionz_lev', ionz_lev)
      call json%add(json_group, 'ion_min', ion_min(1: nsp-1))
      call json%add(json_group, 'ion_max', ion_max(1: nsp-1))
      call json%add(json_group, 'atomic_number', atomic_number(1: nsp-1))
      call json%add(json_group, 'mass_number', mass_number(1: nsp-1))
      call json%add(json_group, 't0_pl', t0_pl(1: nsp))
      call json%add(json_group, 'np_per_xc', np_per_xc(1: nsp))
      call json%add(json_group, 'np_per_yc', np_per_yc(1: nsp))
      call json%add(json_group, 'np_per_zc', np_per_zc(1: nsp))
      call json%add(json_group, 'concentration', concentration(1: nsp))
      call json%add(json_group, 'transverse_dist', transverse_dist)
      call json%add(json_group, 'lpx', lpx)
      call json%add(json_group, 'lpy', lpy)
      call json%add(json_group, 'n0_ref', n0_ref)
      call json%add(json_group, 'np1', np1)
      call json%add(json_group, 'np2', np2)
      call json%add(json_group, 'r_c', r_c)
      call json%add(json_group, 'l_disable_rng_seed', l_disable_rng_seed)
      nullify(json_group)
    end if

    ! Add the laser
    if (parameters_in%exists_laser) then
      call json%create_object(json_group, 'laser')
      call json%add(p, json_group)
      call json%add(json_group, 'nb_laser', nb_laser)
      call json%add(json_group, 'g_prof', g_prof)
      call json%add(json_group, 't0_lp', t0_lp)
      call json%add(json_group, 'xc_lp', xc_lp)
      call json%add(json_group, 'tau_fwhm', tau_fwhm)
      call json%add(json_group, 'w0_y', w0_y)
      call json%add(json_group, 'a0', a0)
      call json%add(json_group, 'lam0', lam0)
      call json%add(json_group, 'lp_delay', lp_delay(1: nb_laser-1))
      call json%add(json_group, 'lp_offset', lp_offset)
      call json%add(json_group, 't1_lp', t1_lp)
      call json%add(json_group, 'tau1_fwhm', tau1_fwhm)
      call json%add(json_group, 'w1_y', w1_y)
      call json%add(json_group, 'a1', a1)
      call json%add(json_group, 'lam1', lam1)
      call json%add(json_group, 'symmetrization_pulse', symmetrization_pulse)
      call json%add(json_group, 'a_symm_rat', a_symm_rat)
      call json%add(json_group, 'enable_ionization', enable_ionization)
      call json%add(json_group, 'y0_cent', y0_cent(1: nb_laser))
      call json%add(json_group, 'z0_cent', z0_cent(1: nb_laser))
      call json%add(json_group, 'y1_cent', y1_cent)
      call json%add(json_group, 'z1_cent', z1_cent)
      call json%add(json_group, 'incid_angle', incid_angle)
      nullify(json_group)
    end if

    ! Add the beam
    if (parameters_in%exists_beam) then
      call json%create_object(json_group, 'beam_inject')
      call json%add(p, json_group)
      call json%add(json_group, 'nb_1', nb_1)
      call json%add(json_group, 'xc_1', xc_1)
      call json%add(json_group, 'gam_1', gam_1)
      call json%add(json_group, 'sxb_1', sxb_1)
      call json%add(json_group, 'syb_1', syb_1)
      call json%add(json_group, 'epsy_1', epsy_1)
      call json%add(json_group, 'epsz_1', epsz_1)
      call json%add(json_group, 'dg_1', dg_1)
      call json%add(json_group, 'charge_1', charge_1)
      call json%add(json_group, 'ap1_twiss', ap1_twiss)
      call json%add(json_group, 'bt1_twiss', bt1_twiss)
      call json%add(json_group, 't_inject', t_inject)
      nullify(json_group)
    end if

    ! Add the window
    if (parameters_in%exists_window) then
      call json%create_object(json_group, 'moving_window')
      call json%add(p, json_group)
      call json%add(json_group, 'w_sh', w_sh)
      call json%add(json_group, 'wi_time', wi_time)
      call json%add(json_group, 'wf_time', wf_time)
      call json%add(json_group, 'w_speed', w_speed)
      nullify(json_group)
    end if

    ! Add the output
    if (parameters_in%exists_output) then
      call json%create_object(json_group, 'output')
      call json%add(p, json_group)
      call json%add(json_group, 'nouts', nouts)
      call json%add(json_group, 'iene', iene)
      call json%add(json_group, 'nvout', nvout)
      call json%add(json_group, 'nden', nden)
      call json%add(json_group, 'ncurr', ncurr)
      call json%add(json_group, 'npout', npout)
      call json%add(json_group, 'jump', jump)
      call json%add(json_group, 'pjump', pjump)
      call json%add(json_group, 'nouts', nouts)
      call json%add(json_group, 'gam_min', gam_min)
      call json%add(json_group, 'xp0_out', xp0_out)
      call json%add(json_group, 'xp1_out', xp1_out)
      call json%add(json_group, 'yp_out', yp_out)
      call json%add(json_group, 'tmax', tmax)
      call json%add(json_group, 'cfl', cfl)
      call json%add(json_group, 'new_sim', new_sim)
      call json%add(json_group, 'id_new', id_new)
      call json%add(json_group, 'dump', dump)
      call json%add(json_group, 'l_force_singlefile_output', l_force_singlefile_output)
      call json%add(json_group, 'l_print_j_on_grid', l_print_j_on_grid)
      call json%add(json_group, 'l_first_output_on_restart', l_first_output_on_restart)
      call json%add(json_group, 'l_env_modulus', l_env_modulus)
      call json%add(json_group, 'time_interval_dumps', time_interval_dumps)
      nullify(json_group)
    end if

    ! Add the tracking
    if (parameters_in%exists_tracking) then
      call json%create_object(json_group, 'tracking')
      call json%add(p, json_group)
      call json%add(json_group, 'p_tracking', p_tracking(1:nsp))
      call json%add(json_group, 'every_track', every_track(1:nsp))
      call json%add(json_group, 'nkjump', nkjump(1:nsp))
      call json%add(json_group, 'txmin', txmin(1:nsp))
      call json%add(json_group, 'txmax', txmax(1:nsp))
      call json%add(json_group, 'tymin', tymin(1:nsp))
      call json%add(json_group, 'tymax', tymax(1:nsp))
      call json%add(json_group, 'tzmin', tzmin(1:nsp))
      call json%add(json_group, 'tzmax', tzmax(1:nsp))
      call json%add(json_group, 't_in', t_in(1:nsp))
      call json%add(json_group, 't_out', t_out(1:nsp))
      call json%add(json_group, 'a_on_particles', a_on_particles(1:nsp))
      nullify(json_group)
    end if

    ! Add the MPI
    if (parameters_in%exists_mpi) then
      call json%create_object(json_group, 'mpiparams')
      call json%add(p, json_group)
      call json%add(json_group, 'nprocx', nprocx)
      call json%add(json_group, 'nprocy', nprocy)
      call json%add(json_group, 'nprocz', nprocz)
      nullify(json_group)
    end if

    ! Writing the check file
    write(filename_json_out, '(a6,i2.2,a5)') 'input_', id_new, '.json'
    call json%print(p, filename_json_out)
 
    call json%destroy()
  end subroutine
#else
  subroutine write_json_checklist(parameters_in)
    type(parameters_t), intent(in) :: parameters_in
    type(parameters_t) :: parameters_mock

    parameters_mock = parameters_in
  end subroutine

  subroutine assign_parameters( parameters_in, exist_json_in )
    type(parameters_t), intent(in) :: parameters_in
    logical, intent(in) :: exist_json_in
    type(parameters_t) :: parameters_mock

    if (.not. exist_json_in) then
      return
    end if

    parameters_mock = parameters_in
  end subroutine
#endif

  subroutine write_checklist(parameters_in, exist_json_in)
    !! Wrapper function to write the checklist
    type(parameters_t), intent(in) :: parameters_in
    logical, intent(in) :: exist_json_in

    if (exist_json_in) then
      call write_json_checklist(parameters_in)
    else
      call write_read_nml
    end if
  end subroutine
 end module

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

  use common_param
  use control_bunch_input
  use code_util
  use mpi_var

  implicit none
  private
  public :: read_main_input, write_read_nml

  integer :: nml_iounit = 1, nml_ierr = 0
  character(100) :: nml_error_message = ''

 contains

  subroutine read_main_input
   logical exist_nml
   logical exist_data

   inquire (file=input_namelist_filename, exist=exist_nml)
   inquire (file=input_data_filename, exist=exist_data)

   if (exist_nml) then
    call read_input_nml
   else
    write (6, *) 'No usable input file (.nml or .data) has been found'
    stop 5
   end if
  end subroutine

  subroutine read_input_nml
   !===========================================================
   !=
   != Reads the input namelist
   !=
   !===========================================================

   namelist /grid/ nx, ny, nz, ny_targ, k0, yx_rat, zx_rat
   namelist /simulation/ lpf_ord, der_ord, str_flag, iform, model_id, &
    dmodel_id, ibx, iby, ibz, ibeam
   namelist /target_description/ nsp, nsb, ionz_lev, ionz_model, ion_min, &
    ion_max, atomic_number, mass_number, t0_pl, ppc, np_per_xc, &
    np_per_yc, np_per_zc, concentration, lpx, lpy, n0_ref, np1, np2, &
    r_c, l_disable_rng_seed
   namelist /laser/ g_prof, nb_laser, t0_lp, xc_lp, tau_fwhm, w0_y, a0, &
    lam0, lp_delay, lp_offset, t1_lp, tau1_fwhm, w1_y, a1, lam1, &
    symmetrization_pulse, a_symm_rat, enable_ionization, y0_cent, &
    z0_cent, y1_cent, z1_cent, incid_angle
   namelist /beam_inject/ nb_1, xc_1, gam_1, sxb_1, syb_1, epsy_1, &
    epsz_1, dg_1, charge_1, ap1_twiss, bt1_twiss, t_inject
   namelist /moving_window/ w_sh, wi_time, wf_time, w_speed
   namelist /output/ nouts, iene, nvout, nden, npout, nbout, jump, pjump, &
    gam_min, xp0_out, xp1_out, yp_out, tmax, cfl, new_sim, id_new, &
    dump, l_force_singlefile_output, time_interval_dumps, &
    l_print_j_on_grid, l_first_output_on_restart, l_env_modulus
   namelist /tracking/ tkjump, nkjump, txmin, txmax, tymin, tymax, tzmin, &
    tzmax, t_in, t_out, p_tracking
   namelist /mpiparams/ nprocx, nprocy, nprocz

   !--- reading grid parameters ---!
   yx_rat = -1.
   zx_rat = -1.
   r_c = 0.0
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, grid, iostat=nml_ierr)
   nml_error_message = 'GRID'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error
   call consistency_check_grid

   !--- reading sim parameters ---!
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, simulation, iostat=nml_ierr)
   nml_error_message = 'SIMULATION'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

   !--- reading target parameters ---!
   mass_number(1:3) = 1.0
   ppc = -1
   np_per_xc = -1
   np_per_yc = -1
   np_per_zc = -1
   l_disable_rng_seed = .false.
   concentration(:) = zero_dp
   concentration(1) = one_dp
   n0_ref = 1.
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, target_description, iostat=nml_ierr)
   nml_error_message = 'TARGET_DESCRIPTION'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error
   !call consistency_check_number_of_particles
   call consistency_check_number_of_particles_comp

   !--- reading laser parameters ---!
   symmetrization_pulse = .false.
   a_symm_rat = 0.
   enable_ionization(:) = .true.
   y0_cent(:) = zero_dp
   z0_cent(:) = zero_dp
   y1_cent = zero_dp
   z1_cent = zero_dp
   incid_angle = zero_dp
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, laser, iostat=nml_ierr)
   nml_error_message = 'LASER'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

   if (nsb > 0) then
    !--- reading injected beam parameters ---!
    open (nml_iounit, file=input_namelist_filename, status='old')
    read (nml_iounit, beam_inject, iostat=nml_ierr)
    nml_error_message = 'BEAM'
    close (nml_iounit)
    if (nml_ierr > 0) call print_at_screen_nml_error
    n_bunches = nsb
    nb_tot(1) = nb_1*100000
    xc_bunch(1) = xc_1
    gam(1) = gam_1
    sxb(1) = sxb_1
    syb(1) = syb_1
    epsy(1) = epsy_1
    epsz(1) = epsz_1
    dg(1) = dg_1
    bunch_charge(1) = charge_1
    alpha_twiss(1) = ap1_twiss
    beta_twiss(1) = bt1_twiss
   end if

   !--- reading moving window parameters ---!
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, moving_window, iostat=nml_ierr)
   nml_error_message = 'MOVING_WINDOW'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

   !--- reading output parameters ---!
   time_interval_dumps = -1. !if -1 use classical output
   l_force_singlefile_output = .true.
   l_first_output_on_restart = .false.
   l_print_j_on_grid = .true.
   l_env_modulus = .true.
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, output, iostat=nml_ierr)
   nml_error_message = 'OUTPUT'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, tracking, iostat=nml_ierr)
   nml_error_message = 'TRACKING'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

   !--- reading mpi decomposition ---!
   nprocx = -1
   nprocy = -1
   nprocz = -1
   open (nml_iounit, file=input_namelist_filename, status='old')
   read (nml_iounit, mpiparams, iostat=nml_ierr)
   nml_error_message = 'MPIPARAMS'
   close (nml_iounit)
   if (nml_ierr > 0) call print_at_screen_nml_error

  end subroutine

  subroutine write_read_nml
   character(len=12) :: output_filename
   !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   !C
   !C write namelist on a file 'input_  .nml'
   !C
   !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   namelist /grid/ nx, ny, nz, ny_targ, k0, yx_rat, zx_rat
   namelist /simulation/ lpf_ord, der_ord, str_flag, iform, model_id, &
    dmodel_id, ibx, iby, ibz, ibeam
   namelist /target_description/ nsp, nsb, ionz_lev, ionz_model, ion_min, &
    ion_max, atomic_number, mass_number, t0_pl, ppc, np_per_xc, &
    np_per_yc, np_per_zc, concentration, lpx, lpy, n0_ref, np1, np2, &
    r_c
   namelist /laser/ g_prof, nb_laser, t0_lp, xc_lp, tau_fwhm, w0_y, a0, &
    lam0, lp_delay, lp_offset, t1_lp, tau1_fwhm, w1_y, a1, lam1, &
    symmetrization_pulse, a_symm_rat, enable_ionization, y0_cent, &
    z0_cent, y1_cent, z1_cent, incid_angle
   namelist /moving_window/ w_sh, wi_time, wf_time, w_speed
   namelist /output/ nouts, iene, nvout, nden, npout, nbout, jump, pjump, &
    gam_min, xp0_out, xp1_out, yp_out, tmax, cfl, new_sim, id_new, &
    dump, l_force_singlefile_output, time_interval_dumps, &
    l_print_j_on_grid, l_first_output_on_restart, l_env_modulus
   namelist /tracking/ tkjump, nkjump, txmin, txmax, tymin, tymax, tzmin, &
    tzmax, t_in, t_out, p_tracking
   namelist /mpiparams/ nprocx, nprocy, nprocz
   namelist /number_bunches/ n_bunches, l_particles, &
    l_intdiagnostics_pwfa, l_intdiagnostics_classic, &
    l_embunchevolution, number_of_slices
   namelist /bunches/ nb_tot, bunch_type, bunch_shape, rhob, xc_bunch, &
    yc_bunch, zc_bunch, gam, sxb, syb, epsy, epsz, dg, charge_right, &
    charge_left, sigma_cut_bunch, ppc_x_bunch, ppc_y_bunch, ppc_z_bunch
   namelist /twiss/ l_twiss, alpha_twiss, beta_twiss
   namelist /bpoloidal/ l_bpoloidal, b_ex_poloidal, radius_poloidal

   write (output_filename, 100) 'input_', id_new, '.nml'
100 format(a6, i2.2, a4)
   open (nml_iounit, file=output_filename)
   write (nml_iounit, nml=grid, err=110)
   write (nml_iounit, nml=simulation, err=110)
   write (nml_iounit, nml=target_description, err=110)
   write (nml_iounit, nml=laser, err=110)
   write (nml_iounit, nml=moving_window, err=110)
   write (nml_iounit, nml=output, err=110)
   if (p_tracking) write (nml_iounit, nml=tracking, err=110)
   write (nml_iounit, nml=mpiparams, err=110)
   write (nml_iounit, nml=number_bunches, err=110)
   write (nml_iounit, nml=bunches, err=110)
   write (nml_iounit, nml=twiss, err=110)
   write (nml_iounit, nml=bpoloidal, err=110)
110 continue
   close (nml_iounit)
  end subroutine

  subroutine consistency_check_number_of_particles_comp

   if (all(ppc >= 1)) then
    call from_ppc_to_npx_npy_npz
   else if (all(np_per_zc == -1)) then
    np_per_zc = np_per_yc
   end if
  end subroutine

  subroutine consistency_check_grid

   if (zx_rat < 0. .and. yx_rat > 0.) then
    zx_rat = yx_rat
    !write(6,'(A)') "force zx_rat equal to yx_rat"
   else if (zx_rat > 0. .and. yx_rat < 0.) then
    yx_rat = zx_rat
    !write(6,'(A)') "force yx_rat equal to zx_rat"
   else if (zx_rat < 0. .and. yx_rat < 0.) then
    yx_rat = 1.
    zx_rat = 1.
    !write(6,'(A)') "force yx_rat=1 and zx_rat=1"
   end if
  end subroutine

  !------------------------------------------------------!
  subroutine consistency_check_number_of_particles
   !excluded temporarily because it doesn't deal with few cases, most of all np_per_xc=0

   !--->case 0: ppc is the only defined (new nml)
   if (all(ppc >= 1) .and. all(np_per_xc == -1) .and. &
       all(np_per_yc == -1) .and. all(np_per_zc == -1)) then
    call from_ppc_to_npx_npy_npz

    !--->case 1: np_per_zc not defined: copy from np_per_yc
   else if (all(ppc == -1) .and. all(np_per_xc >= 0) .and. &
            all(np_per_yc >= 0) .and. all(np_per_zc == -1)) then
    np_per_zc = np_per_yc

    !--->case 3: new and old methods both defined
   else if (all(ppc >= 1) .and. (all(np_per_xc >= 1) .or. all(np_per_yc >= &
                                                              1) .or. all(np_per_zc >= 1))) then
    np_per_xc = -1
    np_per_yc = -1
    np_per_zc = -1
    call from_ppc_to_npx_npy_npz

    !--->case default
   else
    ppc = 8
    call from_ppc_to_npx_npy_npz
   end if

  end subroutine

  !------> Particle organisation
  subroutine from_ppc_to_npx_npy_npz
   !--->Subdivide ppc into np_per_xc,np_per_yc and theoretically into np_per_zc
   !logical isprime
   integer i, number_of_factors
   integer, allocatable, dimension(:) :: factors

   !verify input 'ppc' are not prime numbers
   do i = 1, 6
    do while (isprime(ppc(i)))
     !if(pe0) write(6,'(A,I1,A,I3)')'The input parameter ppc(',i,') is prime - corrected to >',ppc(i)+1
     ppc(i) = ppc(i) + 1
    end do
   end do

   !subdivide ppc into np_per_xc,yc,zc
   do i = 1, 6
    allocate (factors(ppc(i)/2))
    call primefactors(ppc(i), factors, number_of_factors)
    if (ndim == 2) then
     np_per_xc(i) = factors(1)
     np_per_yc(i) = product(factors(2:number_of_factors))
     !if(pe0) write(6,'(A,I2,A,I3,A,I3,A)') 'layer:',i,' > ',np_per_xc(i),'*',np_per_yc(i),' particles'
    else if (ndim == 3) then
     if (number_of_factors > 2) then
      np_per_xc(i) = factors(1)
      np_per_yc(i) = factors(2)
      np_per_zc(i) = product(factors(3:number_of_factors))
     else
      np_per_xc(i) = 1
      np_per_yc(i) = factors(1)
      np_per_zc(i) = factors(2)
     end if
     !if(pe0) write(6,'(A,I2,A,I3,A,I3,A,I3,A)') 'layer:',i,' > ',np_per_xc(i),'*',np_per_yc(i),'*',np_per_zc(i),' particles'
    end if
    deallocate (factors)
   end do
  end subroutine

  function isprime(num)
   integer, intent(in) :: num !input number
   integer :: i
   logical :: isprime

   isprime = .true.

   do i = 2, num - 1
    if (mod(num, i) == 0) then
     isprime = .false.
     exit
    end if
   end do
  end function

  subroutine primefactors(num, factors, number_factors)
   integer, intent(in) :: num !input number
   integer, intent(out), dimension((num/2)) :: factors !Array to store factors
   integer, intent(inout) :: number_factors
   integer :: i, n

   i = 2 !Eligible factor
   number_factors = 1 !Number of factors
   n = num !store input number into a temporary variable
   do
    if (mod(n, i) == 0) then !If i divides 2, it is a factor
     factors(number_factors) = i
     number_factors = number_factors + 1
     n = n/i
    else
     i = i + 1 !Not a factor. Move to next number
    end if
    if (n == 1) then
     !Since f is incremented after a factor is found
     number_factors = number_factors - 1 !its value will be one more than the number of factors
     !Hence the value of number_factors is decremented
     exit
    end if
   end do
  end subroutine

  !--- *** *** *** ---!
  subroutine print_at_screen_nml_error
   !character(100) :: line
   !backspace(nml_iounit)
   !read(nml_iounit,fmt='(A)') line
   write (*, '(A)') '*** *** *** *** *** *** *** *** *** *** *** ***'
   write (*, '(A)') 'Error in namelist:      '// &
    trim(nml_error_message)
   !write(*,'(A)')    'Invalid namelist entry: '//trim(line)
   write (*, '(A,I5)') 'iostat type of error:   ', nml_ierr
   write (*, '(A)') '*** *** *** *** *** *** *** *** *** *** *** ***'
   !stop
  end subroutine

  !--- *** *** *** ---!
  subroutine select_number_of_bunch_particles()
   integer :: i

   do i = 1, n_bunches

    if (ppc_x_bunch(i) == -1 .and. ppc_y_bunch(i) == -1 .and. &
        ppc_z_bunch(i) == -1 .and. nb_tot(i) == -1) then
     ppc_bunch(i, :) = 1
     nb_tot(i) = -1
    else if (ppc_x_bunch(i) >= 1 .and. ppc_y_bunch(i) >= 1 .and. &
             ppc_z_bunch(i) >= 1 .and. nb_tot(i) >= 1) then
     ppc_bunch(i, 1) = ppc_x_bunch(i)
     ppc_bunch(i, 2) = ppc_y_bunch(i)
     ppc_bunch(i, 3) = ppc_z_bunch(i)
     nb_tot(i) = -1
    else
     ppc_bunch(i, 1) = ppc_x_bunch(i)
     ppc_bunch(i, 2) = ppc_y_bunch(i)
     ppc_bunch(i, 3) = ppc_z_bunch(i)
     nb_tot(i) = -1
    end if

   end do
  end subroutine

 end module

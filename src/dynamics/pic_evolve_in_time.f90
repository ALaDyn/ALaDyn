
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

 module pic_evolve

  use window
  use boris_push
  use curr_and_fields_util
  use mpi_part_interface
  use init_grid_field
  use ionize
  use fluid_density_momenta
  use tracking
  use util, only: write_warning

  implicit none

  interface lpf2_evolve
   module procedure lpf2_evolve_new
   module procedure lpf2_evolve_old
  end interface

 
 contains

  subroutine lpf2_evolve_old(iter_loc, spec_in, spec_aux_in, mempool)
   integer, intent (in) :: iter_loc
   type(species), allocatable, intent(inout), dimension(:) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: ic, np, id_ch
   real(dp) :: ef2_ion(1), loc_ef2_ion(1)
   logical, parameter :: mw = .false.
   !============================
   call pfields_prepare(ebf, nfield, 2, 2)
   ! =================================
   ! Ionization is deactivated for now with new species
   ! =================================
   ! if (ionization) then
   !  if (iter_loc==0) then
   !   call init_random_seed(mype)
   !  end if
   !  id_ch = nd2 + 1
   !  do ic = 2, nsp_ionz
   !   np = loc_npart(imody, imodz, imodx, ic)
   !   if (np>0) then
   !    call set_ion_efield(ebf, spec(ic), ebfp, np)
   !    if (mod(iter_loc,100)==0) then !refresh ionization tables, if needed
   !     loc_ef2_ion(1) = maxval(ebfp(1:np,id_ch))
   !     loc_ef2_ion(1) = sqrt(loc_ef2_ion(1))
   !     ef2_ion(1) = loc_ef2_ion(1)
   !     !if(prl)call allreduce_dpreal(MAXV,loc_ef2_ion,ef2_ion,1)
   !     if (ef2_ion(1)>lp_max) then
   !      lp_max = 1.1*ef2_ion(1)
   !      call set_field_ioniz_wfunction(ion_min(ic-1), &
   !        atomic_number(ic-1), ic, ionz_lev, ionz_model, lp_max)
   !     end if
   !    end if
   !    call ionization_cycle(spec(ic), ebfp, np, ic, iter_loc, 0, de_inv)
   !   end if
   !   !======== injects new electrons. 
   !  end do
   ! end if
   !===================END IONIZATION MODULE============
   !    ions enter with new ionization levels and new electrons
   !                   are injected
   !=============================================
   jc(:, :, :, :) = zero_dp
   !curr_clean
   do ic = 1, nsp_run
    np = loc_npart(imody, imodz, imodx, ic)
     !============
     call set_lpf_acc(ebf, spec_in(ic), spec_aux_in, np, nfield)
     call field_charge_multiply(spec_in(ic), spec_aux_in, np, nfield)

     if (initial_time) call init_lpf_momenta(spec_in(ic), spec_aux_in, np, ic)
     call lpf_momenta_and_positions(spec_in(ic), spec_aux_in, np, ic)
     ! For each species :
     ! spec_aux_in(1:3) store (X^{n+1}-X_n)=V^{n+1/2}*dt
     ! spec_aux_in(4:7) store old x^n positions and dt/gam at t^{n+1/2}
     if (part) call cell_part_dist(mw, spec_in, spec_aux_in, ic)
     !
     np = loc_npart(imody, imodz, imodx, ic)
     call curr_accumulate(spec_in(ic), spec_aux_in, jc, np)
     !================= only old ion charge saved
   end do
   !==========================================
   if (part) call curr_mpi_collect(jc)
   !================ sums and normalize currents
   if (hybrid) then
    call set_momentum_density_flux(up, flux)

    call update_adam_bash_fluid_variables(up, up0, flux, ebf)
    ! In flux(1:curr_ndim+1) are stored fluid (P,den) at t^{n+1/2}
    call fluid_curr_accumulate(flux, jc)
    !=====================================
    ! In jc(1:3) exit total current density array Dt*(Jx,Jy,Jz)^{n+1/2}
   end if
   !=======================
   ! Inject fields at i=i1-1  for inflow Lp_inject=T
   !call wave_field_left_inject(ebf, xmn)  !(Bz=Ey By=Ez are injected at i1-1 point
   call advance_lpf_fields(ebf, jc, 0)
   !============================
  end subroutine
  !===============
  subroutine lpf2_evolve_new(iter_loc, spec_in, spec_aux_in, mempool)
   integer, intent (in) :: iter_loc
   type(species_new), allocatable, intent(inout), dimension(:) :: spec_in
   type(species_aux), allocatable, intent(inout), dimension(:) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: ic, np, step
   real(dp) :: dt_step
   real (dp) :: ef2_ion(1), loc_ef2_ion(1)
   !============================
   call pfields_prepare(ebf, nfield, 2, 2)
   ! =================================
   ! Ionization is deactivated for now with new species
   ! =================================
   ! if (ionization) then
   !  if (iter_loc==0) then
   !   call init_random_seed(mype)
   !  end if
   !  id_ch = nd2 + 1
   !  do ic = 2, nsp_ionz
   !   np = loc_npart(imody, imodz, imodx, ic)
   !   if (np>0) then
   !    call set_ion_efield(ebf, spec(ic), ebfp, np, mempool)
   !    if (mod(iter_loc,100)==0) then !refresh ionization tables, if needed
   !     loc_ef2_ion(1) = maxval(ebfp(1:np,id_ch))
   !     loc_ef2_ion(1) = sqrt(loc_ef2_ion(1))
   !     ef2_ion(1) = loc_ef2_ion(1)
   !     !if(prl)call allreduce_dpreal(MAXV,loc_ef2_ion,ef2_ion,1)
   !     if (ef2_ion(1)>lp_max) then
   !      lp_max = 1.1*ef2_ion(1)
   !      call set_field_ioniz_wfunction(ion_min(ic-1), &
   !        atomic_number(ic-1), ic, ionz_lev, ionz_model, lp_max)
   !     end if
   !    end if
   !    call ionization_cycle(spec(ic), ebfp, np, ic, iter_loc, 0, de_inv)
   !   end if
   !   !======== injects new electrons. 
   !  end do
   ! end if
   !===================END IONIZATION MODULE============
   !    ions enter with new ionization levels and new electrons
   !                   are injected
   !=============================================
   jc(:, :, :, :) = zero_dp
   dt_step = dt_loc
   !curr_clean
   do ic = 1, nsp_run
    np = loc_npart(imody, imodz, imodx, ic)
    !============
    call particle_motion( ebf, spec_in(ic), spec_aux_in(ic), dt_step, &
     np, nfield, ic, initial_time, mempool)
   !============
    np = loc_npart(imody, imodz, imodx, ic)
    call curr_accumulate(spec_in(ic), spec_aux_in(ic), jc, np, mempool)
    !================= only old ion charge saved
   end do
   !==========================================
   if (part) call curr_mpi_collect(jc)
   !================ sums and normalize currents
   if (hybrid) then
    call set_momentum_density_flux(up, flux)

    call update_adam_bash_fluid_variables(up, up0, flux, ebf)
    ! In flux(1:curr_ndim+1) are stored fluid (P,den) at t^{n+1/2}
    call fluid_curr_accumulate(flux, jc)
    !=====================================
    ! In jc(1:3) exit total current density array Dt*(Jx,Jy,Jz)^{n+1/2}
   end if
   !================================================
   ! Laser field is interpolated on tracked particles, if requested from output
   call interpolate_field_on_tracking( ebf, spec_in, spec_aux_in, iter_loc, A_PARTICLE, mempool )
   !=======================
   ! Inject fields at i=i1-1  for inflow Lp_inject=T
   !call wave_field_left_inject(ebf, xmn)  !(Bz=Ey By=Ez are injected at i1-1 point
   call advance_lpf_fields(ebf, jc, 0)
   !============================
  end subroutine
  !===============
  ! END SECTION for Leap-frog one-cycle integrator in LP regime
  !===============
  subroutine lp_run(t_loc, iter_loc, mempool)

   real (dp), intent (in) :: t_loc
   integer, intent (in) :: iter_loc
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp) :: ts
   !================================

   !=========================
   call lpf2_evolve(iter_loc, spec, ebfp, mempool)
   !===================================
   ts = t_loc
   if (w_speed>0.0) then ! moves the computational box with w_speed>0.
    if (ts>=wi_time .and. ts<wf_time) then
     if (mod(iter_loc,w_sh)==0) then
      call lp_window_xshift(w_sh, iter_loc, spec, ebfp, mempool)
     end if
    end if
   end if
   if (comoving) then
    if (ts>=wi_time .and. ts<wf_time) then
     if (mod(iter_loc,w_sh)==0) then
      call comoving_coordinate(vbeam, w_sh, iter_loc, spec, ebfp, mempool)
     end if
    end if
   end if
   !==============================
   !vbeam=-w_speed
   !vbeam >0 uses the xw=(x+vbeam*t)
   !x=xi=(xw-vbeam*t) fixed
   !==============================
  end subroutine

  subroutine particle_motion(ef, spec_in, spec_aux_in, dt_in, np, nfields, &
    ic, initial_time_in, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (inout) :: spec_in
   type (species_aux), intent (inout) :: spec_aux_in
   real (dp), intent (in) :: dt_in
   integer, intent (in) :: np, nfields, ic
   logical, intent (inout) :: initial_time_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   logical, parameter :: mw = .false.


   !==========================================================
   ! Fields interpolation on particles positions
   call set_lpf_acc(ef, spec_in, spec_aux_in, np, nfields, mempool)

   !==========================================================
   ! Fields are multiplied by the particles charge
   call field_charge_multiply(spec_in, spec_aux_in)

   !==========================================================
   ! Initializes particles momenta on the initial time
   call init_lpf_momenta(spec_in, spec_aux_in, dt_in, np, ic, initial_time_in, mempool)

   !==========================================================
   ! Particles are pushed via the chosen pusher
   call lpf_momenta_and_positions(spec_in, spec_aux_in, dt_in, np, ic, mempool)
    ! For each species :
    ! spec_aux_in%V_COMP stores (X^{n+1}-X_n)=V^{n+1/2}*dt
    ! spec_aux_in%OLD_COMP stores old x^n positions
    ! spec_aux_in%INV_GAMMA_COMP stores dt/gam at t^{n+1/2}
   !==========================================================
   ! MPI particles exchange
   if (part) call cell_part_dist(mw, spec_in, spec_aux_in, ic)

  end subroutine

 end module


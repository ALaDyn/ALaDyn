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

 module pic_in

  use boris_push, only: init_lpf_momenta
  use curr_and_fields_util, only: field_charge_multiply, set_lpf_acc
  use init_laser_field
  use init_part_distrib
  use tracking
  use util, only: write_warning

  implicit none

  real (dp) :: xf0

 contains
  subroutine init(mempool)
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: ic
   !======================================
   select case (model_id)
   case(1:2)
    call lp_pulse(model_id, xf0) !Linear polarization along y (1)   z(2) 
   case (3)
     call cp_pulse(model_id, xf0) !Circular polarization
   case (4)
    call set_envelope(xf0) !Envelope  approximation for laser
     ! vector potential Ay
   end select
   call part_distribute(spec, ebfp, dmodel_id, xf0)

   if (hybrid) call init_fluid_density_momenta(dmodel_id, xf0)

#if !defined(OLD_SPECIES)
   call initialize_tracking( spec, ebfp, mempool )
   !==========================================================
   ! Initialize particles momentum
   !==========================================================
   do ic = 1, nsp
      call initialize_momenta( ebf, spec(ic), ebfp(ic), dt_loc, nfield, ic, initial_time, mempool)
   end do
#endif

  end subroutine

  subroutine initialize_momenta(ef, spec_in, spec_aux_in, dt_in, nfields, ic, initial_time_in, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (inout) :: spec_in
   type (species_aux), intent (inout) :: spec_aux_in
   real (dp), intent (in) :: dt_in
   integer, intent (in) :: nfields, ic
   logical, intent(inout) :: initial_time_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: np

   np = spec_in%how_many()
   !==========================================================
   ! Fields interpolation on particles positions
   call set_lpf_acc(ef, spec_in, spec_aux_in, np, nfields, mempool)

   !==========================================================
   ! Fields are multiplied by the particles charge
   call field_charge_multiply(spec_in, spec_aux_in)

   !==========================================================
   ! Initializes particles momenta on the initial time
   call init_lpf_momenta(spec_in, spec_aux_in, dt_in, np, ic, initial_time_in, mempool)

  end subroutine
 end module

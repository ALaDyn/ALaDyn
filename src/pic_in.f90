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

  use init_laser_field
  use init_part_distrib

  implicit none

  real(dp) :: xf0

 contains
  subroutine init
   !======================================
   if (model_id < 3) then
    call lp_pulse(model_id, xf0) !Linear polarization along y (1)   z(2)
   else
    select case (model_id)
    case (3)
     call cp_pulse(model_id, xf0) !Circular polarization
    case (4)
     call set_envelope(xf0) !Envelope  approximation for laser
     ! vector potential Ay
    end select
   end if
   call part_distribute(dmodel_id, xf0)

   if (hybrid) call init_fluid_density_momenta(dmodel_id, xf0)

  end subroutine

 end module

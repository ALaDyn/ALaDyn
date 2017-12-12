 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                                Alberto Marocchino                                   !
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

 module pwfa_bunch_field_calculation

 use precision_def
 use pic_in
 use pic_evolve_in_time
 use particles
 use pic_rutil
 use fft_lib
 use grid_fields
 use pstruct_data
 use fstruct_data
 use all_param

 implicit none


 contains

 !---gamma min for all bunches
 function find_gamma_min_allbunches()
 integer :: nbunch
 real(dp) :: find_gamma_min_allbunches

 find_gamma_min_allbunches = 1e10

 do nbunch=1,nsb
  find_gamma_min_allbunches=min(find_gamma_min_allbunches,find_gamma_min(nbunch))
 enddo
 end function find_gamma_min_allbunches

 !---gamma max for all bunches
 function find_gamma_max_allbunches()
 integer :: nbunch
 real(dp) :: find_gamma_max_allbunches

 find_gamma_max_allbunches = -1.

 do nbunch=1,nsb
  find_gamma_max_allbunches=max(find_gamma_max_allbunches,find_gamma_max(nbunch))
 enddo
 end function find_gamma_max_allbunches

 !---gamma min single bunch
 function find_gamma_min(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: np_local
 real(dp) :: mu_gamma_local(1), mu_gamma(1), find_gamma_min

 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 !---
 mu_gamma_local = minval( sqrt( 1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ) )
 !---
 call allreduce_dpreal(-1,mu_gamma_local,mu_gamma,1)
 !---
 find_gamma_min = mu_gamma(1)
 !--- --- ---!
 end function find_gamma_min


 !---gamma max single bunch
 function find_gamma_max(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: np_local
 real(dp) :: mu_gamma_local(1), mu_gamma(1), find_gamma_max

 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 !---
 mu_gamma_local = maxval( sqrt( 1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ) )
 !---
 call allreduce_dpreal(1,mu_gamma_local,mu_gamma,1)
 !---
 find_gamma_max = mu_gamma(1)
 !--- --- ---!
 end function find_gamma_max

 !---gamma max single bunch
 function determine_gammadelta(gmin,gmax)
 real(dp),intent(in) :: gmin,gmax
 real(dp) :: gdiff,determine_gammadelta

 gdiff=gmax-gmin

 if(gdiff<=20) then
  determine_gammadelta=(gmax+gmin)/4.
 else
  determine_gammadelta=20.
 endif
 !--- --- ---!
 end function determine_gammadelta


 !---------------------------
 end module pwfa_bunch_field_calculation

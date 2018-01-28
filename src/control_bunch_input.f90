 !*****************************************************************************************************!
 !                            Copyright 2008-2018  The ALaDyn Collaboration                            !
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

 module control_bunch_input

 use precision_def

 implicit none

 integer :: n_bunches,bunch_type(5),nb_tot(5),number_of_slices(4),bunch_shape(5)

 integer :: np_1,np_2,np_3,np_4,np_5
 integer :: bunch_type_1,bunch_type_2,bunch_type_3,bunch_type_4,bunch_type_5
 integer :: bunch_shape_1,bunch_shape_2,bunch_shape_3,&
                bunch_shape_4,bunch_shape_5

 real(dp) :: bunch_charge(5),bunch_volume(5),jb_norm(5),reduced_charge(5),Lorentz_bfact(5)
 real(dp) :: Charge_right(5), Charge_left(5)
 real(dp) :: gam(5),rhob(5)
 real(dp) :: xc_bunch(5),yc_bunch(5),zc_bunch(5)
 real(dp) :: sxb(5),syb(5)
 real(dp) :: epsy(5),epsz(5),dg(5)
 real(dp) :: sigma_cut_bunch(5)
 real(dp) :: alpha_twiss(5),beta_twiss(5)
 real(dp) :: B_ex_poloidal,radius_poloidal
 integer  :: ppc_x_bunch(5),ppc_y_bunch(5),ppc_z_bunch(5),ppc_bunch(5,3)

 logical :: L_particles,L_Twiss(5),L_Bpoloidal,L_EMBunchEvolution

 end module control_bunch_input

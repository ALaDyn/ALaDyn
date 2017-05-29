 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                 Alberto Marocchino                                                  !
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
 integer :: ppc_bunch(5)
 integer :: ppc_bunch_1,ppc_bunch_2,ppc_bunch_3,&
                  ppc_bunch_4,ppc_bunch_5
 integer :: bunch_type_1,bunch_type_2,bunch_type_3,bunch_type_4,bunch_type_5
 integer :: bunch_shape_1,bunch_shape_2,bunch_shape_3,&
                bunch_shape_4,bunch_shape_5

 real(dp) :: bunch_charge(5),bunch_volume(5),jb_norm(5),reduced_charge(5),Lorentz_bfact(5)
 real(dp) :: Charge_right(5), Charge_left(5)
 real(dp) :: gam(5),rhob(5)
 real(dp) :: xc_bunch(5),yc_bunch(5),zc_bunch(5)
 real(dp) :: sxb(5),syb(5)
 real(dp) :: epsy(5),epsz(5),dg(5)
 real(dp) :: alpha_twiss(5),beta_twiss(5)
 real(dp) :: B_ex_poloidal,radius_poloidal

 real(dp) :: gamma_1,gamma_2,gamma_3,gamma_4,gamma_5
 real(dp) :: rho_b_1,rho_b_2,rho_b_3,rho_b_4,rho_b_5
 real(dp) :: xb_1,xb_2,xb_3,xb_4,xb_5
 real(dp) :: yb_1,yb_2,yb_3,yb_4,yb_5
 real(dp) :: zb_1,zb_2,zb_3,zb_4,zb_5
 real(dp) :: sx_1,sx_2,sx_3,sx_4,sx_5
 real(dp) :: sy_1,sy_2,sy_3,sy_4,sy_5
 real(dp) :: epsy_1,epsy_2,epsy_3,epsy_4,epsy_5
 real(dp) :: epsz_1,epsz_2,epsz_3,epsz_4,epsz_5
 real(dp) :: dg_1,dg_2,dg_3,dg_4,dg_5
 real(dp) :: Charge_right_1,Charge_right_2,Charge_right_3,&
                  Charge_right_4,Charge_right_5
 real(dp) :: Charge_left_1,Charge_left_2,Charge_left_3,&
                  Charge_left_4,Charge_left_5

 logical :: L_particles,L_Twiss(5),L_Bpoloidal,L_EMBunchEvolution

 end module control_bunch_input

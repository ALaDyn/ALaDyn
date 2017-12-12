 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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

 module phys_param
 use precision_def
 implicit none
 real(dp),parameter :: epsilon = 1.0e-8
 real(dp),parameter :: pi = 3.141592653589793, pi2 = 6.283185307179586
 real(dp),parameter :: giant_field = 1.0e4
 real(dp),parameter :: electron_charge_norm = -1.0
 real(dp),parameter :: electron_mass_norm = 1.0
 real(dp),parameter :: proton_charge_norm = 1.0
 real(dp),parameter :: proton_mass_norm = 1836.1527706 ! in units of electron mass
 real(dp),parameter :: size_of_stretch_along_x = 1./4.
 real(dp),parameter :: size_of_stretch_along_y = 1./6.
 real(dp),parameter :: e_charge=1.6021766*1.e-7 !e charge in pC
 real(dp),parameter :: electron_mass=0.510998928 !e mass in MeV
 real(dp),parameter :: rc0=2.81794033 !rc0=e^2/mc^2 classical electron radius, in units 10^-13[cm]
 real(dp),parameter :: speed_of_light=0.299792458 !mu/fs
 real(dp),parameter :: energy_unit=electron_mass*1.e+06 !mc^2(eV)
 real(dp),parameter :: T_unit=0.299792458/0.510998928 !Tesla in GV/m units
 real(dp),parameter :: MG_unit=0.299792458/5.10998928 !MegaGauss in TV/m units
 real(dp),parameter :: fe_unit=0.514 !field atomic unit TV/m
 !======================================
 !10 eV =>  beta = 0.00625603 => gamma = 1.0000196 => beta*gamma = 0.00625615
 !20 eV =>  beta = 0.00884723 => gamma = 1.0000391 => beta*gamma = 0.00884758
 !30 eV =>  beta = 0.01083544 => gamma = 1.0000587 => beta*gamma = 0.01083608
 end module phys_param

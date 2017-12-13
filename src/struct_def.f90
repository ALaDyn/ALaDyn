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

 module struct_def
 use precision_def
 implicit none

 integer,parameter :: P_ncmp=7,G_size=48
 integer,parameter :: Ref_nsp=4

 type vec9
  real(dp) :: cp(9)
 end type vec9
 type vec6
  real(dp) :: cp(6)
 end type vec6
 type vec3
  real(dp) :: cp(3)
 end type vec3

 type particle
  real(dp) :: cmp(P_ncmp)
 end type particle

 type species
  real(dp),allocatable :: part(:,:)
 end type species

 type grid
  integer :: ng,p_ind(2)
  real(dp) :: gmin,gmax
 end type grid

 type sgrid
  integer :: sind(2)
  real(dp) :: smin,smax
 end type sgrid

 end module struct_def

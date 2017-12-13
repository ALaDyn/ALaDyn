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

 module mpi_var
 implicit none
 integer :: np_max,pe_npmax,np_min,pe_npmin
 integer :: mype,imodx,imody,imodz,npe,npe_yloc,npe_zloc,npe_xloc
 integer :: nprocx,nprocy,nprocz,npe_yz,mpi_size,mpi_rank
 integer :: partype
 integer :: imodzx,imodyz,imodyx
 integer :: pe_min,pe_max
 integer :: ndims,dims(3)
 logical :: pe0y,pe0z,pe1y,pe1z,pe0,pe1,prl,prlx,prly,prlz
 logical :: pe0x,pe1x
 end module mpi_var
 !---------------------------

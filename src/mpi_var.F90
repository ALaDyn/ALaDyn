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

 module mpi_var

  use precision_def
  
#if defined (ALaDyn_USE_MPI) && ! defined (FORCE_OLD_MPI)
  use mpi_f08, only: MPI_Comm, MPI_Datatype
  implicit none
  type(MPI_Comm) :: comm, comm_col(3)
  type(MPI_Datatype) :: partype
#else
  implicit none
  integer :: comm, partype, comm_col(3)
#endif


  integer, allocatable :: loc_npart(:, :, :, :), loc_nbpart(:, :, :, :)
  integer, allocatable :: loc_ne_ionz(:, :, :), loc_tpart(:)
  integer, allocatable :: yp_next(:), yp_prev(:)
  integer, allocatable :: zp_next(:), zp_prev(:)
  integer, allocatable :: xp_next(:), xp_prev(:)

  integer :: np_max, pe_npmax, np_min, pe_npmin
  integer :: mype, imodx, imody, imodz, npe, npe_yloc, npe_zloc, &
             npe_xloc
  integer :: npe_yz, mpi_size, mpi_rank
  integer :: imodzx, imodyz, imodyx
  integer :: pe_min, pe_max
  integer :: pe_min_y, pe_max_y, pe_min_z, pe_max_z, pe_min_x, pe_max_x
  integer :: ndims, dims(3)
  logical :: pe0y, pe0z, pe1y, pe1z, pe0, pe1, prl, prlx, prly, prlz
  logical :: xl_bd, yl_bd, zl_bd, xr_bd, yr_bd, zr_bd
  logical :: pe0x, pe1x, pex0, pex1
  integer :: coor(3), col_or(3)
 end module

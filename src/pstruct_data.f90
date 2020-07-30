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

!--------------------------
 module pstruct_data

  use precision_def

  use struct_def

  implicit none

  real(dp), allocatable :: ebfp(:, :), ebfb(:, :)
  real(dp), allocatable :: ebfp0(:, :), ebfp1(:, :)
  real(dp), allocatable :: pdata_tracking(:, :, :)
  real(dp), allocatable :: track_aux(:)
  real(dp), allocatable :: xpt(:, :), ypt(:, :), zpt(:, :), wghpt(:, :)
  real(dp), allocatable :: loc_ypt(:, :), loc_zpt(:, :), &
                           loc_wghyz(:, :, :)
  real(dp), allocatable :: loc_xpt(:, :), loc_wghx(:, :)
  type(species) :: spec(4), bunch(5)
  integer(hp_int), parameter :: ihx = 3

 end module

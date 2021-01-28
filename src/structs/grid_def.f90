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

 module grid_def

  use precision_def
  implicit none
  public

  type grid
   integer :: ng
   !!Number of cells in a given direction of the grid
   integer :: p_ind(2)
   !!Minimum and maximum cell number of the grid
   real (dp) :: gmin
   !!Value of the corresponding axis at the minimum cell
   real (dp) :: gmax
   !!Value of the corresponding axis at the maximum cell
   integer :: min_cell
   !!Initial cell of the grid in absolute units (i.e. respect to the total grid)
   integer :: max_cell
   !!Final cell of the grid in absolute units (i.e. respect to the total grid)
   integer :: shift
   !!Number of guard cells for a given grid
  end type

  type sgrid
   integer :: sind(2)
   !!Initial and final stretched cell (sind(1) also coincides with the number of
   !!stretched cells)
   real (dp) :: smin
   !!Axis value on the boundary between stretched and unstretched grid (left side of the box)
   real (dp) :: smax
   !!Axis value on the boundary between stretched and unstretched grid (right side of the box)
   real (dp) :: stretched_length
   !!Length in microns (axis units) of the stretched section of the grid
  end type
 end module

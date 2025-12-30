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

 module index_array_module
  implicit none
  public

  type index_array
   !! Type defining an array of consecutive integer numbers, useful as
   !! indices in arrays.
   integer, allocatable :: indices(:)
   contains
    procedure, public :: find_index
    procedure, public :: count_index
  end type

  interface index_array
   module procedure new_index_array
  end interface

  contains

  function new_index_array(length, lb ) result(this)
   !! Constructor for the index_array type
   integer, intent(in) :: length
   integer, intent(in), optional :: lb
   type(index_array) :: this
   integer :: i, lowb

   lowb = 1
   if ( present(lb) ) then
    lowb = lb
   end if

   allocate( this%indices(length) )
   this%indices = [ (i, i = lowb, length + lowb - 1) ]
  end function

  subroutine find_index( index_in, mask )
   !! Type bound procedure that finds and pack all the array indices
   !! according to the given mask
   class(index_array), intent(inout) :: index_in
   logical, intent(in) :: mask(:)
   index_in%indices = PACK( index_in%indices, mask )
  end subroutine

  pure function count_index( this ) result(number)
   !! Returns the number of indices (selected particles) available
   class(index_array), intent(in) :: this
   integer :: number
   number = SIZE( this%indices, DIM=1 )
  end function
 end module
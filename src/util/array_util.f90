
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

 module array_util

  use precision_def

  implicit none
  interface array_copy
   module procedure array_copy_r, array_copy_d, array_copy_i
  end interface

  contains

  subroutine array_copy_r (src, dest, n_copied, n_not_copied)
   !
   !=======================================================================
   !                                                                      !
   !  Copy single precision array where size of source not known in       !
   !  advance.                                                            !
   !                                                                      !
   !=======================================================================

   real(sp), intent(in) :: src(:)
   real(sp), intent(out) :: dest(:)
   integer, intent(out) :: n_copied, n_not_copied
   !
   !-----------------------------------------------------------------------
   !  Copy single precision array.
   !-----------------------------------------------------------------------
   !
   n_copied = MIN(SIZE(src), SIZE(dest))
   n_not_copied = SIZE(src) - n_copied
   dest(1:n_copied) = src(1:n_copied)
   return
  end subroutine array_copy_r

  subroutine array_copy_d (src, dest, n_copied, n_not_copied)
   !
   !=======================================================================
   !                                                                      !
   !  Copy double precision array where size of source not known in       !
   !  advance.                                                            !
   !                                                                      !
   !=======================================================================
   real(dp), intent(in) :: src(:)
   real(dp), intent(out) :: dest(:)
   integer, intent(out) :: n_copied, n_not_copied
   !
   !-----------------------------------------------------------------------
   !  Copy double precision array.
   !-----------------------------------------------------------------------
   !
   n_copied = MIN(SIZE(src), SIZE(dest))
   n_not_copied = SIZE(src) - n_copied
   dest(1:n_copied) = src(1:n_copied)
   return
  end subroutine array_copy_d
  subroutine array_copy_i (src, dest, n_copied, n_not_copied)
   !
   !=======================================================================
   !                                                                      !
   !  Copy integer array where size of source not known in advance.       !
   !                                                                      !
   !=======================================================================
   integer, intent(in) :: src(:)
   integer, intent(out) :: dest(:)
   integer, intent(out) :: n_copied, n_not_copied
   !
   !-----------------------------------------------------------------------
   !  Copy integer array.
   !-----------------------------------------------------------------------
   !
   n_copied = MIN(SIZE(src), SIZE(dest))
   n_not_copied = SIZE(src) - n_copied
   dest(1:n_copied) = src(1:n_copied)
   return
  end subroutine array_copy_i
 end module
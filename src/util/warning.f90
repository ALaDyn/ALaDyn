
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

 module warnings

  implicit none
  public

  contains
  subroutine write_warning( text, task )
   character(len=*), intent(in), optional :: text
   integer, intent(in), optional :: task
   logical :: exist

   if ( .not. present(text) ) then
    return
   end if

   inquire(file="warnings.txt", exist=exist)
   if (exist) then
       open(90, file="warnings.txt", status="old", position="append", action="write")
   else
       open(90, file="warnings.txt", status="new", action="write")
   end if

   if ( present(task) ) then
    write( 90, *) '====================='
    write( 90, *) 'MYPE = ', task
    write( 90, *) '====================='
   end if
   write( 90, *) '====================='
   write( 90, *) text
   write( 90, *) '====================='

   close( 90 )
  end subroutine
 end module
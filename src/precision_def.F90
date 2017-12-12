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

 module precision_def

 implicit none

 !F2003 version
 integer, parameter :: sp = selected_real_kind(6, 37)
 integer, parameter :: dp = selected_real_kind(15, 307)
 integer, parameter :: dp_int = selected_int_kind(16)
 integer, parameter :: qp = selected_real_kind(33, 4931)

 real(dp), parameter :: zero_dp = 0.0
 real(dp), parameter :: one_dp = 1.0
 integer, parameter :: zero = 0
 integer, parameter :: one = 1
 end module precision_def

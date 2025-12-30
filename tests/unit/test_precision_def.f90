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

!! @file test_precision_def.f90
!! @brief Unit tests for precision_def module
!! @details Tests precision kinds and utility functions

program test_precision_def
 use test_assertions
 use test_runner
 use precision_def

 implicit none

 call start_test_suite('precision_def')

 call test_precision_kinds()
 call test_precision_constants()
 ! Note: is_zero function test would require linking against ALaDyn object files
 ! This is a limitation since ALaDyn is built as an executable, not a library

 call end_test_suite('precision_def')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_precision_kinds()
  real(sp) :: single_val
  real(dp) :: double_val
  integer(dp_int) :: large_int
  integer(hp_int) :: small_int

  call run_test('precision_kinds')

  ! Test that sp is single precision (6 decimal digits)
  call assert_true(sp > 0, 'sp kind is defined')
  call assert_true(precision(single_val) >= 6, 'sp has at least 6 digits of precision')

  ! Test that dp is double precision (15 decimal digits)
  call assert_true(dp > 0, 'dp kind is defined')
  call assert_true(precision(double_val) >= 15, 'dp has at least 15 digits of precision')

  ! Test integer kinds
  call assert_true(dp_int > 0, 'dp_int kind is defined')
  call assert_true(hp_int > 0, 'hp_int kind is defined')

  ! Test that dp_int can hold large values
  large_int = 10000000000_dp_int
  call assert_true(large_int > 0, 'dp_int can hold 10^10')

  ! Test that hp_int is smaller
  small_int = 1000_hp_int
  call assert_true(small_int == 1000, 'hp_int can hold 1000')
 end subroutine

 subroutine test_precision_constants()
  call run_test('precision_constants')

  ! Test zero constants
  call assert_near_dp(zero_dp, 0.0_dp, 1.0e-15_dp, 'zero_dp equals 0.0')
  call assert_near_sp(zero_sp, real(0.0, sp), 1.0e-6_sp, 'zero_sp equals 0.0')

  ! Test one constants
  call assert_near_dp(one_dp, 1.0_dp, 1.0e-15_dp, 'one_dp equals 1.0')
  call assert_near_sp(one_sp, real(1.0, sp), 1.0e-6_sp, 'one_sp equals 1.0')

  ! Test integer constants
  call assert_equal_int(zero, 0, 'zero integer constant equals 0')
  call assert_equal_int(one, 1, 'one integer constant equals 1')
 end subroutine

end program test_precision_def

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

!! @file test_assertions.f90
!! @brief Test assertion utilities for ALaDyn test suite
!! @details Provides assertion subroutines for comparing values with tolerances

module test_assertions

 use precision_def, only: sp, dp, dp_int

 implicit none
 private

 ! Test result counters
 integer, save :: total_tests = 0
 integer, save :: passed_tests = 0
 integer, save :: failed_tests = 0

 ! Default tolerance for floating point comparisons
 real(dp), parameter :: DEFAULT_TOL = 1.0e-12_dp

 public :: sp, dp, dp_int
 public :: assert_true, assert_false
 public :: assert_equal_int, assert_equal_dp, assert_equal_sp
 public :: assert_near_dp, assert_near_sp
 public :: assert_array_equal_dp, assert_array_near_dp
 public :: reset_test_counts, get_test_summary, print_test_summary
 public :: total_tests, passed_tests, failed_tests
 public :: DEFAULT_TOL

contains

 !> Reset all test counters to zero
 subroutine reset_test_counts()
  total_tests = 0
  passed_tests = 0
  failed_tests = 0
 end subroutine

 !> Get test summary as formatted string
 subroutine get_test_summary(summary)
  character(len=*), intent(out) :: summary
  write(summary, '(A,I0,A,I0,A,I0)') &
   'Tests: ', total_tests, ' | Passed: ', passed_tests, ' | Failed: ', failed_tests
 end subroutine

 !> Print test summary to stdout
 subroutine print_test_summary()
  write(*,'(A)') '============================================'
  write(*,'(A)') 'TEST SUMMARY'
  write(*,'(A)') '============================================'
  write(*,'(A,I0)') 'Total tests:  ', total_tests
  write(*,'(A,I0)') 'Passed:       ', passed_tests
  write(*,'(A,I0)') 'Failed:       ', failed_tests
  write(*,'(A)') '============================================'
  if (failed_tests > 0) then
   write(*,'(A)') 'RESULT: FAILED'
  else
   write(*,'(A)') 'RESULT: PASSED'
  end if
  write(*,'(A)') '============================================'
 end subroutine

 !> Assert that a condition is true
 subroutine assert_true(condition, test_name)
  logical, intent(in) :: condition
  character(len=*), intent(in) :: test_name

  total_tests = total_tests + 1
  if (condition) then
   passed_tests = passed_tests + 1
   write(*,'(A,A,A)') '[PASS] ', trim(test_name), ''
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A,A)') '[FAIL] ', trim(test_name), ' - Expected TRUE but got FALSE'
  end if
 end subroutine

 !> Assert that a condition is false
 subroutine assert_false(condition, test_name)
  logical, intent(in) :: condition
  character(len=*), intent(in) :: test_name

  total_tests = total_tests + 1
  if (.not. condition) then
   passed_tests = passed_tests + 1
   write(*,'(A,A,A)') '[PASS] ', trim(test_name), ''
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A,A)') '[FAIL] ', trim(test_name), ' - Expected FALSE but got TRUE'
  end if
 end subroutine

 !> Assert that two integers are equal
 subroutine assert_equal_int(expected, actual, test_name)
  integer, intent(in) :: expected, actual
  character(len=*), intent(in) :: test_name

  total_tests = total_tests + 1
  if (expected == actual) then
   passed_tests = passed_tests + 1
   write(*,'(A,A)') '[PASS] ', trim(test_name)
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A,A,I0,A,I0)') '[FAIL] ', trim(test_name), &
    ' - Expected: ', expected, ' Actual: ', actual
  end if
 end subroutine

 !> Assert that two double precision values are equal
 subroutine assert_equal_dp(expected, actual, test_name)
  real(dp), intent(in) :: expected, actual
  character(len=*), intent(in) :: test_name

  call assert_near_dp(expected, actual, DEFAULT_TOL, test_name)
 end subroutine

 !> Assert that two single precision values are equal
 subroutine assert_equal_sp(expected, actual, test_name)
  real(sp), intent(in) :: expected, actual
  character(len=*), intent(in) :: test_name

  call assert_near_sp(expected, actual, real(DEFAULT_TOL, sp), test_name)
 end subroutine

 !> Assert that two double precision values are nearly equal
 subroutine assert_near_dp(expected, actual, tol, test_name)
  real(dp), intent(in) :: expected, actual, tol
  character(len=*), intent(in) :: test_name
  real(dp) :: diff

  total_tests = total_tests + 1
  diff = abs(expected - actual)

  if (diff <= tol) then
   passed_tests = passed_tests + 1
   write(*,'(A,A)') '[PASS] ', trim(test_name)
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A)') '[FAIL] ', trim(test_name)
   write(*,'(A,ES15.8,A,ES15.8,A,ES15.8)') &
    '       Expected: ', expected, ' Actual: ', actual, ' Diff: ', diff
  end if
 end subroutine

 !> Assert that two single precision values are nearly equal
 subroutine assert_near_sp(expected, actual, tol, test_name)
  real(sp), intent(in) :: expected, actual, tol
  character(len=*), intent(in) :: test_name
  real(sp) :: diff

  total_tests = total_tests + 1
  diff = abs(expected - actual)

  if (diff <= tol) then
   passed_tests = passed_tests + 1
   write(*,'(A,A)') '[PASS] ', trim(test_name)
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A)') '[FAIL] ', trim(test_name)
   write(*,'(A,ES12.5,A,ES12.5,A,ES12.5)') &
    '       Expected: ', expected, ' Actual: ', actual, ' Diff: ', diff
  end if
 end subroutine

 !> Assert that two double precision arrays are equal element-wise
 subroutine assert_array_equal_dp(expected, actual, n, test_name)
  real(dp), intent(in) :: expected(:), actual(:)
  integer, intent(in) :: n
  character(len=*), intent(in) :: test_name

  call assert_array_near_dp(expected, actual, n, DEFAULT_TOL, test_name)
 end subroutine

 !> Assert that two double precision arrays are nearly equal element-wise
 subroutine assert_array_near_dp(expected, actual, n, tol, test_name)
  real(dp), intent(in) :: expected(:), actual(:)
  integer, intent(in) :: n
  real(dp), intent(in) :: tol
  character(len=*), intent(in) :: test_name
  integer :: i
  real(dp) :: max_diff
  logical :: all_pass

  total_tests = total_tests + 1
  all_pass = .true.
  max_diff = 0.0_dp

  do i = 1, n
   if (abs(expected(i) - actual(i)) > tol) then
    all_pass = .false.
   end if
   max_diff = max(max_diff, abs(expected(i) - actual(i)))
  end do

  if (all_pass) then
   passed_tests = passed_tests + 1
   write(*,'(A,A)') '[PASS] ', trim(test_name)
  else
   failed_tests = failed_tests + 1
   write(*,'(A,A,A,ES15.8)') '[FAIL] ', trim(test_name), ' - Max diff: ', max_diff
  end if
 end subroutine

end module test_assertions

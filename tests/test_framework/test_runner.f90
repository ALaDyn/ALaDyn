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

!! @file test_runner.f90
!! @brief Test runner utilities for ALaDyn test suite
!! @details Provides utilities for organizing and running test suites

module test_runner

 use test_assertions

 implicit none
 private

 public :: start_test_suite, end_test_suite
 public :: run_test, skip_test
 public :: test_suite_passed

contains

 !> Start a test suite with the given name
 subroutine start_test_suite(suite_name)
  character(len=*), intent(in) :: suite_name

  write(*,'(A)') ''
  write(*,'(A)') '============================================'
  write(*,'(A,A)') 'TEST SUITE: ', trim(suite_name)
  write(*,'(A)') '============================================'
  write(*,'(A)') ''

  call reset_test_counts()
 end subroutine

 !> End a test suite and print summary
 subroutine end_test_suite(suite_name)
  character(len=*), intent(in) :: suite_name

  write(*,'(A)') ''
  write(*,'(A,A)') 'End of test suite: ', trim(suite_name)
  call print_test_summary()
 end subroutine

 !> Check if the test suite passed
 function test_suite_passed() result(passed)
  logical :: passed
  passed = (failed_tests == 0)
 end function

 !> Mark a test as being run
 subroutine run_test(test_name)
  character(len=*), intent(in) :: test_name
  write(*,'(A,A,A)') '--- Running: ', trim(test_name), ' ---'
 end subroutine

 !> Skip a test with a reason
 subroutine skip_test(test_name, reason)
  character(len=*), intent(in) :: test_name
  character(len=*), intent(in) :: reason
  write(*,'(A,A,A,A)') '[SKIP] ', trim(test_name), ' - ', trim(reason)
 end subroutine

end module test_runner

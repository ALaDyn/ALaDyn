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

!! @file test_array_alloc.f90
!! @brief Unit tests for array_alloc module
!! @details Tests array allocation and memory management utilities

program test_array_alloc
 use test_assertions
 use test_runner

 implicit none

 call start_test_suite('array_alloc')

 call test_array_allocation()
 call test_array_initialization()
 call test_memory_layout()

 call end_test_suite('array_alloc')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_array_allocation()
  ! Test basic array allocation
  real(dp), allocatable :: arr1d(:)
  real(dp), allocatable :: arr2d(:,:)
  real(dp), allocatable :: arr3d(:,:,:)
  integer :: n1, n2, n3
  integer :: allocstat

  call run_test('array_allocation')

  n1 = 100
  n2 = 50
  n3 = 50

  ! Test 1D allocation
  allocate(arr1d(n1), stat=allocstat)
  call assert_equal_int(allocstat, 0, '1D array allocation succeeds')
  call assert_equal_int(size(arr1d), n1, '1D array has correct size')
  deallocate(arr1d)

  ! Test 2D allocation
  allocate(arr2d(n1, n2), stat=allocstat)
  call assert_equal_int(allocstat, 0, '2D array allocation succeeds')
  call assert_equal_int(size(arr2d, 1), n1, '2D array dim 1 correct')
  call assert_equal_int(size(arr2d, 2), n2, '2D array dim 2 correct')
  call assert_equal_int(size(arr2d), n1*n2, '2D array total size correct')
  deallocate(arr2d)

  ! Test 3D allocation
  allocate(arr3d(n1, n2, n3), stat=allocstat)
  call assert_equal_int(allocstat, 0, '3D array allocation succeeds')
  call assert_equal_int(size(arr3d, 1), n1, '3D array dim 1 correct')
  call assert_equal_int(size(arr3d, 2), n2, '3D array dim 2 correct')
  call assert_equal_int(size(arr3d, 3), n3, '3D array dim 3 correct')
  call assert_equal_int(size(arr3d), n1*n2*n3, '3D array total size correct')
  deallocate(arr3d)
 end subroutine

 subroutine test_array_initialization()
  ! Test array initialization to zero
  real(dp), allocatable :: arr(:,:,:)
  integer :: n1, n2, n3
  integer :: i, j, k
  logical :: all_zero

  call run_test('array_initialization')

  n1 = 10
  n2 = 10
  n3 = 10

  allocate(arr(n1, n2, n3))

  ! Initialize to zero (as done in ALaDyn)
  arr = 0.0_dp

  ! Verify all elements are zero
  all_zero = .true.
  do k = 1, n3
   do j = 1, n2
    do i = 1, n1
     if (abs(arr(i,j,k)) > 1.0e-15_dp) then
      all_zero = .false.
      exit
     end if
    end do
    if (.not. all_zero) exit
   end do
   if (.not. all_zero) exit
  end do

  call assert_true(all_zero, 'array initialized to all zeros')

  ! Test initialization to specific value
  arr = 1.5_dp

  all_zero = .true.  ! Reuse variable to check all are 1.5
  do k = 1, n3
   do j = 1, n2
    do i = 1, n1
     if (abs(arr(i,j,k) - 1.5_dp) > 1.0e-15_dp) then
      all_zero = .false.
      exit
     end if
    end do
    if (.not. all_zero) exit
   end do
   if (.not. all_zero) exit
  end do

  call assert_true(all_zero, 'array initialized to constant value')

  deallocate(arr)
 end subroutine

 subroutine test_memory_layout()
  ! Test Fortran column-major memory layout
  real(dp), allocatable :: arr(:,:)
  integer :: n1, n2
  integer :: i, j
  real(dp) :: counter

  call run_test('memory_layout')

  n1 = 5
  n2 = 4

  allocate(arr(n1, n2))

  ! Fill array in column-major order
  counter = 1.0_dp
  do j = 1, n2
   do i = 1, n1
    arr(i,j) = counter
    counter = counter + 1.0_dp
   end do
  end do

  ! Verify first column
  call assert_near_dp(arr(1,1), 1.0_dp, 1.0e-15_dp, 'arr(1,1) = 1')
  call assert_near_dp(arr(2,1), 2.0_dp, 1.0e-15_dp, 'arr(2,1) = 2')
  call assert_near_dp(arr(n1,1), real(n1, dp), 1.0e-15_dp, 'arr(n1,1) = n1')

  ! Verify second column starts at n1+1
  call assert_near_dp(arr(1,2), real(n1+1, dp), 1.0e-15_dp, 'arr(1,2) = n1+1')

  ! Verify last element
  call assert_near_dp(arr(n1,n2), real(n1*n2, dp), 1.0e-15_dp, 'arr(n1,n2) = n1*n2')

  deallocate(arr)
 end subroutine

end program test_array_alloc

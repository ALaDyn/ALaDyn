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

!! @file test_util.f90
!! @brief Unit tests for util module
!! @details Tests mathematical utilities including random number generation

program test_util
 use test_assertions
 use test_runner

 implicit none

 call start_test_suite('util')

 call test_gasdev_statistics()
 call test_sort_algorithm()

 call end_test_suite('util')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_gasdev_statistics()
  ! Test Gaussian random number generator statistics
  integer, parameter :: n_samples = 10000
  real(dp) :: samples(n_samples)
  real(dp) :: mean, variance, std_dev
  integer :: i
  real(dp) :: dev, sum_val, sum_sq

  call run_test('gasdev_statistics')

  ! Initialize RNG (using local seed)
  call init_random_seed_local(12345)

  ! Generate samples
  sum_val = 0.0_dp
  sum_sq = 0.0_dp
  do i = 1, n_samples
   call gasdev_local(dev)
   samples(i) = dev
   sum_val = sum_val + dev
   sum_sq = sum_sq + dev*dev
  end do

  ! Calculate mean
  mean = sum_val / real(n_samples, dp)

  ! Calculate variance
  variance = (sum_sq / real(n_samples, dp)) - mean*mean
  std_dev = sqrt(variance)

  ! Test that mean is close to 0 (within statistical tolerance)
  call assert_near_dp(mean, 0.0_dp, 0.05_dp, 'gasdev mean is approximately 0')

  ! Test that standard deviation is close to 1
  call assert_near_dp(std_dev, 1.0_dp, 0.05_dp, 'gasdev std_dev is approximately 1')

  ! Test bounds - very few samples should be outside [-4, 4]
  call assert_true(minval(samples) > -6.0_dp, 'gasdev min > -6')
  call assert_true(maxval(samples) < 6.0_dp, 'gasdev max < 6')
 end subroutine

 subroutine test_sort_algorithm()
  ! Test sorting algorithm
  integer, parameter :: n = 100
  real(dp) :: arr(n), arr_sorted(n)
  integer :: i
  logical :: is_sorted

  call run_test('sort_algorithm')

  ! Initialize with random values
  call init_random_seed_local(54321)
  call random_number(arr)
  arr = arr * 1000.0_dp - 500.0_dp  ! Values in range [-500, 500]

  ! Copy and sort
  arr_sorted = arr
  call sort_local(arr_sorted, n)

  ! Verify sorted order
  is_sorted = .true.
  do i = 2, n
   if (arr_sorted(i) < arr_sorted(i-1)) then
    is_sorted = .false.
    exit
   end if
  end do

  call assert_true(is_sorted, 'array is sorted in ascending order')

  ! Test edge case: already sorted array
  do i = 1, n
   arr(i) = real(i, dp)
  end do
  arr_sorted = arr
  call sort_local(arr_sorted, n)

  is_sorted = .true.
  do i = 1, n
   if (abs(arr_sorted(i) - real(i, dp)) > 1.0e-10_dp) then
    is_sorted = .false.
    exit
   end if
  end do
  call assert_true(is_sorted, 'already sorted array remains sorted')

  ! Test edge case: reverse sorted array
  do i = 1, n
   arr(i) = real(n - i + 1, dp)
  end do
  arr_sorted = arr
  call sort_local(arr_sorted, n)

  is_sorted = .true.
  do i = 1, n
   if (abs(arr_sorted(i) - real(i, dp)) > 1.0e-10_dp) then
    is_sorted = .false.
    exit
   end if
  end do
  call assert_true(is_sorted, 'reverse sorted array is sorted correctly')
 end subroutine

 ! Local implementation of gasdev for testing
 subroutine gasdev_local(dev)
  real(dp), intent(out) :: dev
  real(dp) :: v1, v2, rsq
  real(dp), save :: g
  logical, save :: gaus_store = .false.

  if (gaus_store) then
   dev = g
   gaus_store = .false.
  else
   do
    call random_number(v1)
    call random_number(v2)
    v1 = 2.0_dp*v1 - 1.0_dp
    v2 = 2.0_dp*v2 - 1.0_dp
    rsq = v1*v1 + v2*v2
    if (rsq < 1.0_dp) exit
   end do
   rsq = sqrt(-2.0_dp*log(rsq)/rsq)
   dev = v1*rsq
   g = v2*rsq
   gaus_store = .true.
  end if
 end subroutine

 ! Local implementation of random seed initialization
 subroutine init_random_seed_local(seed_val)
  integer, intent(in) :: seed_val
  integer, allocatable :: seed(:)
  integer :: n, i

  call random_seed(size=n)
  allocate(seed(n))
  do i = 1, n
   seed(i) = seed_val + 37*(i-1)
  end do
  call random_seed(put=seed)
 end subroutine

 ! Local implementation of quicksort for testing
 subroutine sort_local(part, np)
  real(dp), intent(inout) :: part(:)
  integer, intent(in) :: np
  integer :: ir, i, j, k, l, jstack
  integer, parameter :: m = 7, nstack = 50
  real(dp) :: a, temp
  integer :: istack(nstack)

  jstack = 0
  ir = np
  l = 1
  do
   if (ir - l < m) then
    do j = l + 1, ir
     a = part(j)
     do i = j - 1, l, -1
      if (part(i) <= a) exit
      part(i + 1) = part(i)
     end do
     part(i + 1) = a
    end do
    if (jstack == 0) return
    ir = istack(jstack)
    l = istack(jstack - 1)
    jstack = jstack - 2
   else
    k = (l + ir)/2
    ! swap k and l+1
    temp = part(k); part(k) = part(l+1); part(l+1) = temp

    if (part(l) > part(ir)) then
     temp = part(l); part(l) = part(ir); part(ir) = temp
    end if
    if (part(l + 1) > part(ir)) then
     temp = part(l+1); part(l+1) = part(ir); part(ir) = temp
    end if
    if (part(l) > part(l + 1)) then
     temp = part(l); part(l) = part(l+1); part(l+1) = temp
    end if

    i = l + 1
    j = ir
    a = part(l + 1)
    do
     do
      i = i + 1
      if (part(i) >= a) exit
     end do
     do
      j = j - 1
      if (part(j) <= a) exit
     end do
     if (j < i) exit
     temp = part(i); part(i) = part(j); part(j) = temp
    end do
    part(l + 1) = part(j)
    part(j) = a
    jstack = jstack + 2
    if (ir - i + 1 >= j - l) then
     istack(jstack) = ir
     istack(jstack - 1) = i
     ir = j - 1
    else
     istack(jstack) = j - 1
     istack(jstack - 1) = l
     l = i
    end if
   end if
  end do
 end subroutine

end program test_util

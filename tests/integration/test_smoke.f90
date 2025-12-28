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

!! @file test_smoke.f90
!! @brief Smoke test for basic ALaDyn functionality
!! @details Validates that basic operations work correctly

program test_smoke
 use test_assertions
 use test_runner

 implicit none

 call start_test_suite('smoke_test')

 call test_basic_math_operations()
 call test_array_operations()
 call test_intrinsic_functions()
 call test_basic_physics()

 call end_test_suite('smoke_test')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_basic_math_operations()
  real(dp) :: a, b, c

  call run_test('basic_math_operations')

  ! Test basic arithmetic
  a = 3.0_dp
  b = 4.0_dp
  c = sqrt(a*a + b*b)
  call assert_near_dp(c, 5.0_dp, 1.0e-14_dp, 'Pythagorean theorem: sqrt(3^2 + 4^2) = 5')

  ! Test exponential and logarithm
  a = exp(1.0_dp)
  call assert_near_dp(a, 2.718281828459045_dp, 1.0e-14_dp, 'exp(1) = e')

  b = log(a)
  call assert_near_dp(b, 1.0_dp, 1.0e-14_dp, 'log(e) = 1')

  ! Test trigonometric functions
  a = sin(0.0_dp)
  b = cos(0.0_dp)
  call assert_near_dp(a, 0.0_dp, 1.0e-15_dp, 'sin(0) = 0')
  call assert_near_dp(b, 1.0_dp, 1.0e-15_dp, 'cos(0) = 1')

  ! Test atan2
  a = atan2(1.0_dp, 1.0_dp)
  call assert_near_dp(a, 3.141592653589793_dp/4.0_dp, 1.0e-14_dp, 'atan2(1,1) = pi/4')
 end subroutine

 subroutine test_array_operations()
  real(dp) :: arr(10), arr2(10)
  real(dp) :: sum_val, max_val, min_val
  integer :: i

  call run_test('array_operations')

  ! Initialize array
  do i = 1, 10
   arr(i) = real(i, dp)
  end do

  ! Test sum
  sum_val = sum(arr)
  call assert_near_dp(sum_val, 55.0_dp, 1.0e-14_dp, 'sum(1:10) = 55')

  ! Test max/min
  max_val = maxval(arr)
  min_val = minval(arr)
  call assert_near_dp(max_val, 10.0_dp, 1.0e-14_dp, 'maxval(1:10) = 10')
  call assert_near_dp(min_val, 1.0_dp, 1.0e-14_dp, 'minval(1:10) = 1')

  ! Test array assignment
  arr2 = arr * 2.0_dp
  call assert_near_dp(arr2(5), 10.0_dp, 1.0e-14_dp, 'arr*2 at index 5 = 10')

  ! Test dot product
  sum_val = dot_product(arr, arr)
  call assert_near_dp(sum_val, 385.0_dp, 1.0e-14_dp, 'dot_product(1:10, 1:10) = 385')
 end subroutine

 subroutine test_intrinsic_functions()
  real(dp) :: x, y
  integer :: n

  call run_test('intrinsic_functions')

  ! Test floor and ceiling
  x = 3.7_dp
  n = floor(x)
  call assert_equal_int(n, 3, 'floor(3.7) = 3')

  n = ceiling(x)
  call assert_equal_int(n, 4, 'ceiling(3.7) = 4')

  ! Test modulo
  x = mod(17.0_dp, 5.0_dp)
  call assert_near_dp(x, 2.0_dp, 1.0e-14_dp, 'mod(17, 5) = 2')

  ! Test abs
  x = abs(-5.0_dp)
  call assert_near_dp(x, 5.0_dp, 1.0e-14_dp, 'abs(-5) = 5')

  ! Test sign
  x = sign(3.0_dp, -1.0_dp)
  call assert_near_dp(x, -3.0_dp, 1.0e-14_dp, 'sign(3, -1) = -3')
 end subroutine

 subroutine test_basic_physics()
  ! Test basic physics relationships used in PIC codes
  real(dp) :: gamma, beta, p, energy
  real(dp) :: omega_p, n_e, lambda_p
  real(dp), parameter :: pi = 3.141592653589793_dp
  real(dp), parameter :: c = 1.0_dp  ! Normalized speed of light

  call run_test('basic_physics')

  ! Test Lorentz factor relationship: gamma = 1/sqrt(1 - beta^2)
  beta = 0.8_dp
  gamma = 1.0_dp / sqrt(1.0_dp - beta*beta)
  call assert_near_dp(gamma, 5.0_dp/3.0_dp, 1.0e-14_dp, 'gamma for beta=0.8')

  ! Test momentum: p = gamma * m * v (m=1, v=beta*c, c=1)
  p = gamma * beta
  call assert_near_dp(p, 4.0_dp/3.0_dp, 1.0e-14_dp, 'momentum p = gamma*beta')

  ! Test energy-momentum relation: E^2 = p^2 + m^2 (m=1, c=1)
  energy = sqrt(p*p + 1.0_dp)
  call assert_near_dp(energy, gamma, 1.0e-14_dp, 'E = gamma for m=1')

  ! Test plasma frequency scaling: omega_p^2 ~ n_e
  n_e = 1.0e18_dp  ! Reference density
  omega_p = sqrt(n_e)
  call assert_true(omega_p > 0.0_dp, 'plasma frequency is positive')

  ! Test plasma wavelength: lambda_p = 2*pi*c/omega_p
  lambda_p = 2.0_dp * pi * c / omega_p
  call assert_true(lambda_p > 0.0_dp, 'plasma wavelength is positive')

  ! Test relativistic addition of velocities (co-linear)
  ! v_total = (v1 + v2) / (1 + v1*v2/c^2)
  beta = (0.5_dp + 0.5_dp) / (1.0_dp + 0.5_dp*0.5_dp)
  call assert_near_dp(beta, 0.8_dp, 1.0e-14_dp, 'relativistic velocity addition')
  call assert_true(beta < 1.0_dp, 'resultant velocity < c')
 end subroutine

end program test_smoke

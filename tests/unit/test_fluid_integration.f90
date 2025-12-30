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

!! @file test_fluid_integration.f90
!! @brief Unit tests for higher-order Adams-Bashforth fluid integration schemes
!! @details Tests the AB2, AB3, and AB4 integration methods for the fluid equations

program test_fluid_integration
 use test_assertions
 use test_runner

 implicit none

 ! Adams-Bashforth coefficients at module level to avoid duplication
 real(dp), parameter :: ABF3_0 = 23.0_dp/12.0_dp
 real(dp), parameter :: ABF3_1 = -16.0_dp/12.0_dp
 real(dp), parameter :: ABF3_2 = 5.0_dp/12.0_dp
 real(dp), parameter :: ABF4_0 = 55.0_dp/24.0_dp
 real(dp), parameter :: ABF4_1 = -59.0_dp/24.0_dp
 real(dp), parameter :: ABF4_2 = 37.0_dp/24.0_dp
 real(dp), parameter :: ABF4_3 = -9.0_dp/24.0_dp

 call start_test_suite('fluid_integration')

 call test_ab_coefficients()
 call test_ab2_basic()
 call test_ab3_basic()
 call test_ab4_basic()
 call test_ab_stability()

 call end_test_suite('fluid_integration')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_ab_coefficients()
  !! Test that Adams-Bashforth coefficients sum to 1 (consistency requirement)
  real(dp) :: ab2_sum, ab3_sum, ab4_sum
  real(dp), parameter :: tol = 1.0e-14_dp

  call run_test('ab_coefficients')

  ! AB2 coefficients: 3/2, -1/2
  ab2_sum = 1.5_dp + (-0.5_dp)
  call assert_near_dp(ab2_sum, 1.0_dp, tol, 'AB2 coefficients sum to 1')

  ! AB3 coefficients: 23/12, -16/12, 5/12
  ab3_sum = 23.0_dp/12.0_dp + (-16.0_dp/12.0_dp) + 5.0_dp/12.0_dp
  call assert_near_dp(ab3_sum, 1.0_dp, tol, 'AB3 coefficients sum to 1')

  ! AB4 coefficients: 55/24, -59/24, 37/24, -9/24
  ab4_sum = 55.0_dp/24.0_dp + (-59.0_dp/24.0_dp) + 37.0_dp/24.0_dp + (-9.0_dp/24.0_dp)
  call assert_near_dp(ab4_sum, 1.0_dp, tol, 'AB4 coefficients sum to 1')
 end subroutine

 subroutine test_ab2_basic()
  !! Test AB2 basic integration using simple harmonic oscillator
  !! du/dt = -u (decay equation), u(0) = 1
  !! Exact solution: u(t) = exp(-t)
  real(dp) :: u, f_n, f_nm1, dt, t_final, t
  real(dp) :: exact, error
  real(dp), parameter :: tol = 0.01_dp
  integer :: n, nsteps

  call run_test('ab2_basic')

  dt = 0.01_dp
  t_final = 1.0_dp
  nsteps = nint(t_final / dt)

  u = 1.0_dp  ! Initial condition
  f_nm1 = -u  ! f(u) = -u at t=0

  ! First step: Euler
  f_n = -u
  u = u + dt * f_n
  f_nm1 = f_n

  ! Subsequent steps: AB2
  ! u^{n+1} = u^n + dt * (3/2*f^n - 1/2*f^{n-1})
  do n = 2, nsteps
   f_n = -u
   u = u + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
   f_nm1 = f_n
  end do

  exact = exp(-t_final)
  error = abs(u - exact)

  call assert_true(error < tol, 'AB2 solves decay equation')
  call assert_near_dp(u, exact, tol, 'AB2 solution within tolerance')
 end subroutine

 subroutine test_ab3_basic()
  !! Test AB3 basic integration
  real(dp) :: u, f_n, f_nm1, f_nm2, dt, t_final
  real(dp) :: exact, error
  real(dp), parameter :: tol = 0.001_dp
  integer :: n, nsteps

  call run_test('ab3_basic')

  dt = 0.01_dp
  t_final = 1.0_dp
  nsteps = nint(t_final / dt)

  u = 1.0_dp
  f_nm2 = -u
  f_nm1 = -u

  ! First step: Euler
  f_n = -u
  u = u + dt * f_n
  f_nm2 = f_nm1
  f_nm1 = f_n

  ! Second step: AB2
  f_n = -u
  u = u + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
  f_nm2 = f_nm1
  f_nm1 = f_n

  ! Subsequent steps: AB3
  do n = 3, nsteps
   f_n = -u
   u = u + dt * (ABF3_0 * f_n + ABF3_1 * f_nm1 + ABF3_2 * f_nm2)
   f_nm2 = f_nm1
   f_nm1 = f_n
  end do

  exact = exp(-t_final)
  error = abs(u - exact)

  call assert_true(error < tol, 'AB3 solves decay equation')
  call assert_near_dp(u, exact, tol, 'AB3 solution within tolerance')
 end subroutine

 subroutine test_ab4_basic()
  !! Test AB4 basic integration
  real(dp) :: u, f_n, f_nm1, f_nm2, f_nm3, dt, t_final
  real(dp) :: exact, error
  real(dp), parameter :: tol = 0.0001_dp
  integer :: n, nsteps

  call run_test('ab4_basic')

  dt = 0.01_dp
  t_final = 1.0_dp
  nsteps = nint(t_final / dt)

  u = 1.0_dp
  f_nm3 = -u
  f_nm2 = -u
  f_nm1 = -u

  ! First step: Euler
  f_n = -u
  u = u + dt * f_n
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n

  ! Second step: AB2
  f_n = -u
  u = u + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n

  ! Third step: AB3
  f_n = -u
  u = u + dt * (ABF3_0 * f_n + ABF3_1 * f_nm1 + ABF3_2 * f_nm2)
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n

  ! Subsequent steps: AB4
  do n = 4, nsteps
   f_n = -u
   u = u + dt * (ABF4_0 * f_n + ABF4_1 * f_nm1 + ABF4_2 * f_nm2 + ABF4_3 * f_nm3)
   f_nm3 = f_nm2
   f_nm2 = f_nm1
   f_nm1 = f_n
  end do

  exact = exp(-t_final)
  error = abs(u - exact)

  call assert_true(error < tol, 'AB4 solves decay equation')
  call assert_near_dp(u, exact, tol, 'AB4 solution within tolerance')
 end subroutine

 subroutine test_ab_stability()
  !! Test stability of AB methods with small time steps
  !! Verifies that solutions don't blow up
  real(dp) :: u_ab2, u_ab3, u_ab4, dt, t_final
  real(dp) :: f_n, f_nm1, f_nm2, f_nm3
  integer :: n, nsteps

  call run_test('ab_stability')

  dt = 0.01_dp
  t_final = 2.0_dp
  nsteps = nint(t_final / dt)

  ! AB2 stability
  u_ab2 = 1.0_dp
  f_nm1 = -u_ab2
  f_n = -u_ab2
  u_ab2 = u_ab2 + dt * f_n
  f_nm1 = f_n
  do n = 2, nsteps
   f_n = -u_ab2
   u_ab2 = u_ab2 + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
   f_nm1 = f_n
  end do
  call assert_true(abs(u_ab2) < 2.0_dp, 'AB2 stable for decay equation')

  ! AB3 stability
  u_ab3 = 1.0_dp
  f_nm2 = -u_ab3
  f_nm1 = -u_ab3
  f_n = -u_ab3
  u_ab3 = u_ab3 + dt * f_n
  f_nm2 = f_nm1
  f_nm1 = f_n
  f_n = -u_ab3
  u_ab3 = u_ab3 + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
  f_nm2 = f_nm1
  f_nm1 = f_n
  do n = 3, nsteps
   f_n = -u_ab3
   u_ab3 = u_ab3 + dt * (ABF3_0 * f_n + ABF3_1 * f_nm1 + ABF3_2 * f_nm2)
   f_nm2 = f_nm1
   f_nm1 = f_n
  end do
  call assert_true(abs(u_ab3) < 2.0_dp, 'AB3 stable for decay equation')

  ! AB4 stability
  u_ab4 = 1.0_dp
  f_nm3 = -u_ab4
  f_nm2 = -u_ab4
  f_nm1 = -u_ab4
  f_n = -u_ab4
  u_ab4 = u_ab4 + dt * f_n
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n
  f_n = -u_ab4
  u_ab4 = u_ab4 + dt * (1.5_dp * f_n - 0.5_dp * f_nm1)
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n
  f_n = -u_ab4
  u_ab4 = u_ab4 + dt * (ABF3_0 * f_n + ABF3_1 * f_nm1 + ABF3_2 * f_nm2)
  f_nm3 = f_nm2
  f_nm2 = f_nm1
  f_nm1 = f_n
  do n = 4, nsteps
   f_n = -u_ab4
   u_ab4 = u_ab4 + dt * (ABF4_0 * f_n + ABF4_1 * f_nm1 + ABF4_2 * f_nm2 + ABF4_3 * f_nm3)
   f_nm3 = f_nm2
   f_nm2 = f_nm1
   f_nm1 = f_n
  end do
  call assert_true(abs(u_ab4) < 2.0_dp, 'AB4 stable for decay equation')
 end subroutine

end program test_fluid_integration

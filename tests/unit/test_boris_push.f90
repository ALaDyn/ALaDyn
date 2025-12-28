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

!! @file test_boris_push.f90
!! @brief Unit tests for boris_push module
!! @details Tests the Boris particle pusher algorithm for relativistic particles

program test_boris_push
 use test_assertions
 use test_runner

 implicit none

 real(dp), parameter :: pi = 3.141592653589793_dp

 call start_test_suite('boris_push')

 call test_lorentz_factor()
 call test_boris_rotation()
 call test_electric_acceleration()
 call test_magnetic_gyration()
 call test_energy_conservation()

 call end_test_suite('boris_push')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_lorentz_factor()
  ! Test Lorentz factor (gamma) calculation
  real(dp) :: px, py, pz, gam, gam2

  call run_test('lorentz_factor')

  ! Non-relativistic case: p << 1
  px = 0.1_dp
  py = 0.0_dp
  pz = 0.0_dp
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam = sqrt(gam2)
  call assert_near_dp(gam, sqrt(1.01_dp), 1.0e-14_dp, 'gamma for p=0.1')
  call assert_true(gam < 1.01_dp, 'non-relativistic gamma close to 1')

  ! Mildly relativistic: p ~ 1
  px = 1.0_dp
  py = 0.0_dp
  pz = 0.0_dp
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam = sqrt(gam2)
  call assert_near_dp(gam, sqrt(2.0_dp), 1.0e-14_dp, 'gamma for p=1')

  ! Highly relativistic: p >> 1
  px = 10.0_dp
  py = 0.0_dp
  pz = 0.0_dp
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam = sqrt(gam2)
  call assert_near_dp(gam, sqrt(101.0_dp), 1.0e-14_dp, 'gamma for p=10')
  call assert_true(gam > 10.0_dp, 'highly relativistic gamma ~ p')

  ! 3D momentum
  px = 3.0_dp
  py = 4.0_dp
  pz = 0.0_dp
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam = sqrt(gam2)
  call assert_near_dp(gam, sqrt(26.0_dp), 1.0e-14_dp, 'gamma for |p|=5')
 end subroutine

 subroutine test_boris_rotation()
  ! Test Boris rotation (velocity rotation due to magnetic field)
  real(dp) :: vx, vy, vz, bx, by, bz
  real(dp) :: tx, ty, tz, sx, sy, sz, t2
  real(dp) :: vx_minus, vy_minus, vz_minus
  real(dp) :: vx_prime, vy_prime, vz_prime
  real(dp) :: vx_plus, vy_plus, vz_plus
  real(dp) :: v_mag_before, v_mag_after

  call run_test('boris_rotation')

  ! Initial velocity (perpendicular to B)
  vx = 1.0_dp
  vy = 0.0_dp
  vz = 0.0_dp

  ! Magnetic field in z direction
  bx = 0.0_dp
  by = 0.0_dp
  bz = 0.5_dp  ! dt*B/2*q/m

  v_mag_before = sqrt(vx*vx + vy*vy + vz*vz)

  ! Boris rotation algorithm
  ! t = q*B*dt/(2*m*gamma)
  tx = bx
  ty = by
  tz = bz
  t2 = tx*tx + ty*ty + tz*tz

  ! s = 2*t/(1 + t^2)
  sx = 2.0_dp*tx / (1.0_dp + t2)
  sy = 2.0_dp*ty / (1.0_dp + t2)
  sz = 2.0_dp*tz / (1.0_dp + t2)

  ! v_minus = v (already have it)
  vx_minus = vx
  vy_minus = vy
  vz_minus = vz

  ! v_prime = v_minus + v_minus x t
  vx_prime = vx_minus + (vy_minus*tz - vz_minus*ty)
  vy_prime = vy_minus + (vz_minus*tx - vx_minus*tz)
  vz_prime = vz_minus + (vx_minus*ty - vy_minus*tx)

  ! v_plus = v_minus + v_prime x s
  vx_plus = vx_minus + (vy_prime*sz - vz_prime*sy)
  vy_plus = vy_minus + (vz_prime*sx - vx_prime*sz)
  vz_plus = vz_minus + (vx_prime*sy - vy_prime*sx)

  v_mag_after = sqrt(vx_plus*vx_plus + vy_plus*vy_plus + vz_plus*vz_plus)

  ! Magnetic field should not change speed (only direction)
  call assert_near_dp(v_mag_after, v_mag_before, 1.0e-14_dp, &
   'Boris rotation preserves velocity magnitude')

  ! With B in z, velocity should rotate in xy plane
  call assert_near_dp(vz_plus, 0.0_dp, 1.0e-14_dp, 'vz remains zero')
  call assert_true(abs(vy_plus) > 0.0_dp, 'vy changes due to rotation')
 end subroutine

 subroutine test_electric_acceleration()
  ! Test electric field acceleration
  real(dp) :: px, py, pz
  real(dp) :: ex, ey, ez
  real(dp) :: dt, charge_mass_ratio
  real(dp) :: px_new, py_new, pz_new
  real(dp) :: expected_px

  call run_test('electric_acceleration')

  ! Initial momentum (at rest)
  px = 0.0_dp
  py = 0.0_dp
  pz = 0.0_dp

  ! Electric field in x direction
  ex = 1.0_dp
  ey = 0.0_dp
  ez = 0.0_dp

  ! Time step and charge/mass ratio
  dt = 0.1_dp
  charge_mass_ratio = 1.0_dp  ! Normalized units

  ! Half-step acceleration: p_minus = p + q*E*dt/2
  px_new = px + charge_mass_ratio * ex * dt / 2.0_dp
  py_new = py + charge_mass_ratio * ey * dt / 2.0_dp
  pz_new = pz + charge_mass_ratio * ez * dt / 2.0_dp

  expected_px = 0.05_dp
  call assert_near_dp(px_new, expected_px, 1.0e-14_dp, 'half-step E acceleration')
  call assert_near_dp(py_new, 0.0_dp, 1.0e-14_dp, 'py unchanged')
  call assert_near_dp(pz_new, 0.0_dp, 1.0e-14_dp, 'pz unchanged')
 end subroutine

 subroutine test_magnetic_gyration()
  ! Test that particle gyrates in magnetic field (cyclotron motion)
  real(dp) :: x, y, vx, vy, px, py
  real(dp) :: bz, dt
  real(dp) :: gam, gam2
  real(dp) :: omega_c, period, radius
  integer :: nsteps, i
  real(dp) :: x_init, y_init, dist_from_center
  real(dp) :: tx, ty, tz, sx, sy, sz, t2
  real(dp) :: vx_prime, vy_prime, vx_plus, vy_plus

  call run_test('magnetic_gyration')

  ! Initial conditions: particle with velocity perpendicular to B
  x = 0.0_dp
  y = 0.0_dp
  vx = 0.1_dp  ! Non-relativistic
  vy = 0.0_dp

  ! Magnetic field and time step
  bz = 1.0_dp
  dt = 0.01_dp

  ! Calculate gyration parameters
  gam2 = 1.0_dp + vx*vx + vy*vy
  gam = sqrt(gam2)
  omega_c = bz / gam  ! Cyclotron frequency (normalized)
  period = 2.0_dp * pi / omega_c
  radius = sqrt(vx*vx + vy*vy) / omega_c

  ! Number of steps for one period
  nsteps = int(period / dt)

  x_init = x
  y_init = y
  px = vx * gam
  py = vy * gam

  ! Advance particle for one period using Boris pusher
  do i = 1, nsteps
   ! Calculate gamma
   gam2 = 1.0_dp + px*px + py*py
   gam = sqrt(gam2)

   ! t parameter for Boris
   tz = bz * dt / (2.0_dp * gam)
   tx = 0.0_dp
   ty = 0.0_dp
   t2 = tz*tz

   ! s parameter
   sz = 2.0_dp * tz / (1.0_dp + t2)
   sx = 0.0_dp
   sy = 0.0_dp

   ! Boris rotation
   vx = px / gam
   vy = py / gam

   vx_prime = vx + vy*tz
   vy_prime = vy - vx*tz

   vx_plus = vx + vy_prime*sz
   vy_plus = vy - vx_prime*sz

   px = vx_plus * gam
   py = vy_plus * gam

   ! Update position
   x = x + vx_plus * dt
   y = y + vy_plus * dt
  end do

  ! After one period, particle should return close to initial position
  dist_from_center = sqrt((x - x_init)**2 + (y - y_init)**2)
  call assert_true(dist_from_center < 0.1_dp * radius, &
   'particle returns near initial position after one period')
 end subroutine

 subroutine test_energy_conservation()
  ! Test energy conservation in pure magnetic field
  real(dp) :: px, py, pz
  real(dp) :: bx, by, bz
  real(dp) :: gam_init, gam_final, gam, gam2
  real(dp) :: dt
  integer :: nsteps, i
  real(dp) :: tx, ty, tz, sx, sy, sz, t2
  real(dp) :: vx, vy, vz
  real(dp) :: vx_prime, vy_prime, vz_prime
  real(dp) :: vx_plus, vy_plus, vz_plus

  call run_test('energy_conservation')

  ! Initial relativistic momentum
  px = 5.0_dp
  py = 3.0_dp
  pz = 2.0_dp

  ! Magnetic field (no electric field)
  bx = 0.2_dp
  by = 0.3_dp
  bz = 1.0_dp

  dt = 0.01_dp
  nsteps = 1000

  ! Initial gamma
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam_init = sqrt(gam2)

  ! Evolve with Boris pusher
  do i = 1, nsteps
   gam2 = 1.0_dp + px*px + py*py + pz*pz
   gam = sqrt(gam2)

   ! t parameter
   tx = bx * dt / (2.0_dp * gam)
   ty = by * dt / (2.0_dp * gam)
   tz = bz * dt / (2.0_dp * gam)
   t2 = tx*tx + ty*ty + tz*tz

   ! s parameter
   sx = 2.0_dp * tx / (1.0_dp + t2)
   sy = 2.0_dp * ty / (1.0_dp + t2)
   sz = 2.0_dp * tz / (1.0_dp + t2)

   ! Velocities
   vx = px / gam
   vy = py / gam
   vz = pz / gam

   ! v_prime = v + v x t
   vx_prime = vx + (vy*tz - vz*ty)
   vy_prime = vy + (vz*tx - vx*tz)
   vz_prime = vz + (vx*ty - vy*tx)

   ! v_plus = v + v_prime x s
   vx_plus = vx + (vy_prime*sz - vz_prime*sy)
   vy_plus = vy + (vz_prime*sx - vx_prime*sz)
   vz_plus = vz + (vx_prime*sy - vy_prime*sx)

   ! Update momenta
   px = vx_plus * gam
   py = vy_plus * gam
   pz = vz_plus * gam
  end do

  ! Final gamma
  gam2 = 1.0_dp + px*px + py*py + pz*pz
  gam_final = sqrt(gam2)

  ! Energy should be conserved to high precision
  call assert_near_dp(gam_final, gam_init, 1.0e-10_dp, &
   'energy conserved in pure magnetic field')
 end subroutine

end program test_boris_push

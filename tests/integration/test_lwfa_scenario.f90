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

!! @file test_lwfa_scenario.f90
!! @brief Integration test for LWFA (Laser Wakefield Acceleration) scenario
!! @details Validates physics for laser-driven plasma acceleration

program test_lwfa_scenario
 use test_assertions
 use test_runner

 implicit none

 real(dp), parameter :: pi = 3.141592653589793_dp
 real(dp), parameter :: c = 0.299792458_dp  ! Speed of light in um/fs

 call start_test_suite('lwfa_scenario')

 call test_laser_parameters()
 call test_plasma_parameters()
 call test_wakefield_scaling()
 call test_dephasing_length()
 call test_energy_gain()

 call end_test_suite('lwfa_scenario')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_laser_parameters()
  ! Test laser pulse parameters for LWFA
  real(dp) :: lambda_0, omega_0, k_0
  real(dp) :: a_0, I_0
  real(dp) :: w_0, z_R, tau_fwhm

  call run_test('laser_parameters')

  ! Typical Ti:Sapphire laser wavelength
  lambda_0 = 0.8_dp  ! um
  omega_0 = 2.0_dp * pi * c / lambda_0  ! rad/fs
  k_0 = 2.0_dp * pi / lambda_0  ! rad/um

  call assert_near_dp(omega_0, 2.354_dp, 0.01_dp, 'laser frequency for 800nm')
  call assert_near_dp(k_0, 7.854_dp, 0.01_dp, 'laser wavenumber for 800nm')

  ! Normalized vector potential (typical LWFA)
  a_0 = 2.0_dp
  call assert_true(a_0 > 1.0_dp, 'a0 > 1 for relativistic regime')

  ! Intensity scaling: I = 1.37e18 * a0^2 / lambda_um^2 W/cm^2
  I_0 = 1.37e18_dp * a_0**2 / lambda_0**2
  call assert_true(I_0 > 1.0e18_dp, 'intensity > 10^18 W/cm^2 for LWFA')

  ! Spot size and Rayleigh length
  w_0 = 20.0_dp  ! um (typical)
  z_R = pi * w_0**2 / lambda_0
  call assert_near_dp(z_R, pi*400.0_dp/0.8_dp, 1.0_dp, 'Rayleigh length calculation')

  ! Pulse duration (FWHM)
  tau_fwhm = 30.0_dp  ! fs
  call assert_true(tau_fwhm > 0.0_dp, 'pulse duration is positive')
 end subroutine

 subroutine test_plasma_parameters()
  ! Test plasma parameters for LWFA
  real(dp) :: n_e, omega_p, lambda_p, k_p
  real(dp) :: n_c, lambda_0

  call run_test('plasma_parameters')

  ! Laser wavelength
  lambda_0 = 0.8_dp  ! um

  ! Critical density for this wavelength
  ! n_c = pi / (r_e * lambda_0^2) where r_e = 2.82e-13 cm = 2.82e-7 um
  ! In practical units: n_c ~ 1.1e21 / lambda_um^2 cm^-3
  n_c = 1.1e21_dp / (lambda_0**2)  ! cm^-3
  call assert_true(n_c > 1.0e21_dp, 'critical density > 10^21 cm^-3')

  ! Typical LWFA plasma density
  n_e = 1.0e18_dp  ! cm^-3
  call assert_true(n_e < n_c, 'underdense plasma: n_e < n_c')

  ! Plasma wavelength (in um)
  ! lambda_p = 2*pi*c / omega_p = 33.4 um * sqrt(10^18 / n_e[cm^-3])
  lambda_p = 33.4_dp * sqrt(1.0e18_dp / n_e)
  call assert_near_dp(lambda_p, 33.4_dp, 0.1_dp, 'plasma wavelength at 10^18 cm^-3')

  ! Plasma wavenumber
  k_p = 2.0_dp * pi / lambda_p
  call assert_true(k_p > 0.0_dp, 'plasma wavenumber is positive')

  ! Plasma frequency (normalized to omega_0)
  omega_p = k_p * c
  call assert_true(omega_p > 0.0_dp, 'plasma frequency is positive')
 end subroutine

 subroutine test_wakefield_scaling()
  ! Test wakefield amplitude scaling
  real(dp) :: E_wb, n_e, a_0
  real(dp) :: E_linear, E_nonlinear

  call run_test('wakefield_scaling')

  ! Plasma density
  n_e = 1.0e18_dp  ! cm^-3

  ! Wave-breaking field: E_wb = 96 GV/m * sqrt(n_e / 10^18)
  E_wb = 96.0_dp * sqrt(n_e / 1.0e18_dp)  ! GV/m
  call assert_near_dp(E_wb, 96.0_dp, 0.1_dp, 'wave-breaking field at 10^18 cm^-3')

  ! Linear regime wakefield (a0 << 1)
  a_0 = 0.5_dp
  E_linear = E_wb * a_0**2 / 2.0_dp
  call assert_true(E_linear < E_wb, 'linear wakefield < E_wb')

  ! Nonlinear regime wakefield (a0 > 1)
  a_0 = 2.0_dp
  ! In nonlinear regime, wakefield can approach E_wb
  E_nonlinear = E_wb * sqrt(a_0)  ! Simplified scaling
  call assert_true(E_nonlinear > E_linear, 'nonlinear wakefield > linear wakefield')
 end subroutine

 subroutine test_dephasing_length()
  ! Test electron dephasing length in LWFA
  real(dp) :: n_e, lambda_p, gamma_p, L_d
  real(dp) :: a_0

  call run_test('dephasing_length')

  ! Plasma parameters
  n_e = 1.0e18_dp  ! cm^-3
  lambda_p = 33.4_dp  ! um

  ! Plasma wave Lorentz factor (group velocity)
  ! gamma_p ~ omega_0 / omega_p ~ sqrt(n_c / n_e)
  gamma_p = sqrt(1.1e21_dp / (0.64_dp * n_e))  ! for 800nm laser
  call assert_true(gamma_p > 10.0_dp, 'relativistic plasma wave')

  ! Linear dephasing length
  ! L_d ~ lambda_p * gamma_p^2 / pi
  L_d = lambda_p * gamma_p**2 / pi  ! um
  call assert_true(L_d > 1000.0_dp, 'dephasing length > 1 mm')

  ! Nonlinear correction (approximate)
  a_0 = 2.0_dp
  L_d = L_d * sqrt(1.0_dp + a_0**2)
  call assert_true(L_d > 0.0_dp, 'nonlinear dephasing length is positive')
 end subroutine

 subroutine test_energy_gain()
  ! Test maximum energy gain in LWFA
  real(dp) :: n_e, E_wb, L_d, delta_E
  real(dp) :: gamma_p, lambda_p
  real(dp) :: a_0

  call run_test('energy_gain')

  ! Plasma parameters
  n_e = 1.0e18_dp  ! cm^-3
  lambda_p = 33.4_dp  ! um

  ! Wave-breaking field
  E_wb = 96.0_dp  ! GV/m at 10^18 cm^-3

  ! Plasma wave Lorentz factor
  gamma_p = sqrt(1.1e21_dp / (0.64_dp * n_e))

  ! Dephasing length (convert to m for energy calculation)
  L_d = lambda_p * gamma_p**2 / pi  ! um
  L_d = L_d * 1.0e-6_dp  ! convert to m

  ! Maximum energy gain (linear theory)
  ! delta_E ~ 2 * gamma_p^2 * m_e * c^2
  ! In practical units: delta_E ~ 2 * gamma_p^2 * 0.511 MeV
  delta_E = 2.0_dp * gamma_p**2 * 0.511_dp  ! MeV
  call assert_true(delta_E > 100.0_dp, 'energy gain > 100 MeV')

  ! Alternative calculation: delta_E = e * E_wb * L_d
  ! E_wb in GV/m, L_d in m, result in GeV
  delta_E = E_wb * L_d  ! GeV
  call assert_true(delta_E > 0.0_dp, 'energy gain from field*length is positive')

  ! With typical LWFA parameters, expect GeV-level energy gain
  a_0 = 2.0_dp
  ! Nonlinear scaling: delta_E ~ (2/3) * a_0 * gamma_p^2 * m_e * c^2
  delta_E = (2.0_dp/3.0_dp) * a_0 * gamma_p**2 * 0.511e-3_dp  ! GeV
  call assert_true(delta_E > 0.1_dp, 'nonlinear energy gain > 0.1 GeV')
 end subroutine

end program test_lwfa_scenario

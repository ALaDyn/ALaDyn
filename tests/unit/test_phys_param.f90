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

!! @file test_phys_param.f90
!! @brief Unit tests for phys_param module
!! @details Tests physical constants and parameters

program test_phys_param
 use test_assertions
 use test_runner
 use phys_param

 implicit none

 call start_test_suite('phys_param')

 call test_mathematical_constants()
 call test_physical_constants()
 call test_normalization_constants()
 call test_stretch_parameters()

 call end_test_suite('phys_param')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_mathematical_constants()
  call run_test('mathematical_constants')

  ! Test pi value (should be accurate to double precision)
  call assert_near_dp(pi, 3.141592653589793_dp, 1.0e-15_dp, 'pi value is correct')

  ! Test pi2 = 2*pi
  call assert_near_dp(pi2, 2.0_dp * pi, 1.0e-14_dp, 'pi2 equals 2*pi')
  call assert_near_dp(pi2, 6.283185307179586_dp, 1.0e-15_dp, 'pi2 value is correct')
 end subroutine

 subroutine test_physical_constants()
  call run_test('physical_constants')

  ! Test epsilon (small parameter)
  call assert_true(epsilon > 0.0_dp, 'epsilon is positive')
  call assert_true(epsilon < 1.0e-6_dp, 'epsilon is small')
  call assert_near_dp(epsilon, 1.0e-8_dp, 1.0e-15_dp, 'epsilon value is correct')

  ! Test giant_field (large control parameter)
  call assert_true(giant_field > 0.0_dp, 'giant_field is positive')
  call assert_near_dp(giant_field, 1.0e4_dp, 1.0e-10_dp, 'giant_field value is correct')

  ! Test speed of light
  call assert_near_dp(speed_of_light, 0.299792458_dp, 1.0e-10_dp, 'speed of light is correct')

  ! Test electron charge in pC
  call assert_near_dp(e_charge, 1.6021766e-7_dp, 1.0e-14_dp, 'electron charge value is correct')

  ! Test electron mass in MeV
  call assert_near_dp(electron_mass, 0.510998928_dp, 1.0e-9_dp, 'electron mass value is correct')

  ! Test classical electron radius
  call assert_near_dp(rc0, 2.81794033_dp, 1.0e-8_dp, 'classical electron radius is correct')

  ! Test proton mass ratio
  call assert_near_dp(proton_mass_norm, 1836.1527706_dp, 1.0e-7_dp, 'proton mass ratio is correct')
 end subroutine

 subroutine test_normalization_constants()
  call run_test('normalization_constants')

  ! Test normalized electron charge
  call assert_near_dp(electron_charge_norm, -1.0_dp, 1.0e-15_dp, 'normalized electron charge is -1')

  ! Test normalized electron mass
  call assert_near_dp(electron_mass_norm, 1.0_dp, 1.0e-15_dp, 'normalized electron mass is 1')

  ! Test normalized proton charge
  call assert_near_dp(proton_charge_norm, 1.0_dp, 1.0e-15_dp, 'normalized proton charge is +1')

  ! Test reference density
  call assert_near_dp(reference_density, 1.0e6_dp, 1.0e-10_dp, 'reference density is 1e6')

  ! Test energy unit
  call assert_true(energy_unit > 0.0_dp, 'energy_unit is positive')
 end subroutine

 subroutine test_stretch_parameters()
  call run_test('stretch_parameters')

  ! Test stretch along x (1/4)
  call assert_near_dp(size_of_stretch_along_x, 0.25_dp, 1.0e-15_dp, 'stretch along x is 1/4')

  ! Test stretch along y (1/6)
  call assert_near_dp(size_of_stretch_along_y, 1.0_dp/6.0_dp, 1.0e-14_dp, 'stretch along y is 1/6')
 end subroutine

end program test_phys_param

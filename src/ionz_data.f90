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

 module ionz_data
  use precision_def

  implicit none
  character(10), dimension(0:28), parameter :: species_name = [ &
                                               ' Electron ', ' Hydrogen ', '  Helium  ', ' Lithium  ', &
                                               'Berillium ', ' Boron    ', '  Carbon  ', ' Nitrogen ', &
                                               ' Oxygen   ', ' Florine  ', '   Neon   ', ' Sodium   ', &
                                               'Magnesium ', ' Aluminium', ' Silicon  ', 'Phosphorus', &
                                               ' Solfur   ', ' Chlorine ', ' Argon    ', 'Potassium ', &
                                               ' Calcium  ', ' Scandium ', ' Titanium ', '          ', &
                                               ' Chromium ', 'Manganese ', '  Iron    ', '          ', &
                                               '  Nickel  ']

  integer, parameter :: an_max = 40
  real(dp) :: v(an_max), vfact(an_max, 3), nstar(an_max, 3), &
              c_nstar(an_max, 3)
  real(dp) :: e_c(an_max, 3), e_b(an_max, 3), e_m(an_max, 3), &
              v_norm(an_max, 3)

  integer :: nl_fact(0:an_max), nl_indx(0:an_max), &
             ne_shell(1:6, 0:an_max)
  integer :: l_fact(an_max), z1_coll
  real(dp) :: be(10, 0:an_max), p_nl(10, 0:an_max) !(nl shell,ion charge)
  real(dp), allocatable :: wi(:, :, :), wsp(:, :, :, :), &
                           w_one_lev(:, :, :)
  real(dp), allocatable :: sigma_coll(:, :, :), e_coll(:, :)
  real(dp), parameter :: v_h = 13.5984 !The H ionizazion potential(eV)
  real(dp), parameter :: e_unit = 0.511*1.e+06 !mc^2(eV)
  real(dp), parameter :: omega_a = 41.3 ! 1/fs=E15/s
  real(dp), parameter :: c_au = 137.0 ! speed of light in au
  real(dp), parameter :: euler = 2.71828, omega_l = 0.057 !(laser frequency in a.u.)
  real(dp), parameter :: pig = 3.141592653589793, tiny = 1.e-10
  integer, parameter :: n_ge = 10000, nec = 1000
  real(dp) :: lstar, dge, d2ge, de_inv, deb_inv, dt_fs
  real(dp) :: dgi, d2gi, dei_inv

 contains
  subroutine set_atomic_weight(at_number, w_number)
   integer, intent(in) :: at_number
   real(dp), intent(out) :: w_number

   select case (at_number)
   case (1) !H
    w_number = 1.
   case (2) !He
    w_number = 4.
   case (3) !Li
    w_number = 6.
   case (6) !C
    w_number = 12.
   case (7) !N
    w_number = 14.
   case (8) !O
    w_number = 16.
   case (10) !Ne
    w_number = 20.
   case (13) !Al
    w_number = 26.98
   case (14) !Si
    w_number = 28.09
   case (18) !Ar
    w_number = 39.948
   case (22) !Ti
    w_number = 47.8
   case (28) !Ni
    w_number = 58.7
   case (29) !Cu
    w_number = 63.54
   end select
  end subroutine
  !================================
  subroutine set_atoms_per_molecule(at_number, n_mol_atoms)
   integer, intent(in) :: at_number
   integer, intent(inout) :: n_mol_atoms
   !========================================
   ! This subroutine sets if the neutral gas
   ! Is mono- or diatomic
   !========================================
   select case (at_number)
   case (1)
    n_mol_atoms = 2 !H
   case (2)
    n_mol_atoms = 1 !He
   case (7)
    n_mol_atoms = 2 !N
   case (8)
    n_mol_atoms = 2 !O
   case (9)
    n_mol_atoms = 2 !F
   case (10)
    n_mol_atoms = 1 !Ne
   case (17)
    n_mol_atoms = 2 !Cl
   case (18)
    n_mol_atoms = 1 !Ar
   case (35)
    n_mol_atoms = 2 !Br
   case (36)
    n_mol_atoms = 1 !Kr
   case (53)
    n_mol_atoms = 2 !I
   case (54)
    n_mol_atoms = 1 !Xe
   case default
    n_mol_atoms = 1
   end select
  end subroutine
  !================================
  subroutine set_ionization_coeff(an, sp_ionz)
   integer, intent(in) :: an(:), sp_ionz
   integer :: i, j, loc_zm
   ! The atomin number An identifies the element
   ! Z_max <= An indicates the local max ionization level allowed
   !==============================================
   loc_zm = maxval(an(1:sp_ionz - 1))
   !===========================
   allocate (wi(n_ge + 1, loc_zm + 1, sp_ionz))
   allocate (wsp(0:n_ge + 1, 0:loc_zm, 0:loc_zm, 1:sp_ionz))
   allocate (w_one_lev(0:n_ge + 1, 0:loc_zm, 1:sp_ionz))

   ne_shell = 0
   l_fact = 1
   z1_coll = 1
   nl_indx(0:an_max) = 1
   nl_fact(0:an_max) = 1
   be(1:10, 0:an_max) = 0.0
   p_nl(1:10, 1:an_max) = 0.0
   !Ne_shell(nl,z)= electrons in suborbitals s,p,d of shell n=1,2,3
   !=================================
   ! Potentials V(i) to ionize from state Z=i-1 to Z= i
   !            V(i) from i=1, atomic number = Z_max
   !==========================================================
   do j = 1, sp_ionz - 1
    !=========== cycles over all ion species to be ionized sp_ionz=1,2,3
    select case (an(j)) !The atomic number
    case (1) !H: 1s
     v(1) = 13.5984
    case (2) !He 1s^2
     z1_coll = 0
     v(1) = 24.5874
     v(2) = 54.41776
     be(1, 0) = 23. !He^0+
     be(1, 1) = 54. !He^1+
     p_nl(1, 0) = be(1, 0) + v(2) - be(1, 1)
     nl_indx(0) = 1
     ne_shell(1, 0) = 2
     p_nl(1, 1) = v(2)
     ne_shell(1, 1) = 1
     nl_indx(1) = 1

    case (3) !Li [He]2s
     v(1) = 5.3917
     v(2) = 75.6400
     v(3) = 122.4543
    case (6) !C [He] 2s^2 2p^2
     v(1) = 11.2603
     v(2) = 24.3833
     l_fact(1:2) = 3 ! the 2p shell
     v(3) = 47.8878
     v(4) = 64.4939
     !=========== L shell
     v(5) = 392.087
     v(6) = 489.993
     !=========== K shell
    case (7) !N [He] 2s^2 2p^3
     v(1) = 14.53
     v(2) = 29.601
     v(3) = 47.449
     l_fact(1:3) = 3 ! the 2p shell
     v(4) = 77.473
     v(5) = 97.89
     nl_fact(1:5) = 2
     !========= L-shell
     v(6) = 552.07
     v(7) = 667.046
     nl_fact(6:7) = 1 ! the 1s shell
     !========= K-shell
     !========= Binding energies
     z1_coll = 0
     be(1, 0) = 411 !N^0+
     be(2, 0) = 22
     be(3, 0) = 13
     p_nl(1:3, 0) = be(1:3, 0) + v(1) - be(3, 0)
     ne_shell(1, 0) = 2
     ne_shell(2, 0) = 2
     ne_shell(3, 0) = 3
     !N^1+
     be(1, 1) = 433
     be(2, 1) = 39
     be(3, 1) = 27
     p_nl(1:3, 1) = be(1:3, 1) + v(2) - be(3, 1)
     ne_shell(1, 1) = 2
     ne_shell(2, 1) = 2
     ne_shell(3, 1) = 2

     be(1, 2) = 457
     be(2, 2) = 53
     be(3, 2) = 47
     p_nl(1:3, 2) = be(1:3, 2) + v(3) - be(3, 2)
     nl_indx(0:2) = 3
     ne_shell(1, 2) = 2
     ne_shell(2, 2) = 2
     ne_shell(3, 2) = 1

     be(1, 3) = 488
     be(2, 3) = 75
     p_nl(1:2, 3) = be(1:2, 3) + v(4) - be(2, 3)
     ne_shell(1, 3) = 2
     ne_shell(2, 3) = 2
     ne_shell(3, 3:6) = 0
     be(1, 4) = 516
     be(2, 4) = 98
     p_nl(1:2, 4) = be(1:2, 4) + v(5) - be(2, 4)
     nl_indx(3:4) = 2
     ne_shell(1, 4) = 2
     ne_shell(2, 4) = 1
     p_nl(1, 5) = v(6)
     ne_shell(1, 5) = 2
     p_nl(1, 6) = v(7)
     ne_shell(1, 6) = 1
     ne_shell(2, 5:6) = 0
     nl_indx(5:6) = 1
     !====================
    case (10) !Ne: [He]2s^2 2p^6
     v(1) = 21.5646
     v(2) = 40.9633
     v(3) = 63.45
     v(4) = 97.12
     v(5) = 126.21
     v(6) = 157.93
     l_fact(1:6) = 3 ! the 2p shell
     v(7) = 207.276
     v(8) = 239.099 ! the 2s shell
     nl_fact(1:8) = 2
     !========= L-shell
     v(9) = 1195.8286
     v(10) = 1362.199
     nl_fact(9:10) = 1
     !========= K-shell
     !========= Binding energies
     z1_coll = 0

     be(1, 0) = 869 !Ne^0+
     be(2, 0) = 49
     be(3, 0) = 20
     p_nl(1:3, 0) = be(1:3, 0) + v(1) - be(3, 0)
     nl_indx(0) = 3
     ne_shell(1, 0) = 2
     ne_shell(2, 0) = 2
     ne_shell(3, 0) = 6
     !Ne^1+
     be(1, 1) = 895
     be(2, 1) = 66
     be(3, 1) = 40
     p_nl(1:3, 1) = be(1:3, 1) + v(2) - be(3, 1)
     nl_indx(1) = 3
     ne_shell(1, 1) = 2
     ne_shell(2, 1) = 2
     ne_shell(3, 1) = 5
     !Ne^2+
     be(1, 1) = 925
     be(2, 2) = 87
     be(3, 2) = 67
     p_nl(1:3, 2) = be(1:3, 2) + v(3) - be(3, 2)
     nl_indx(2) = 3
     ne_shell(1, 2) = 2
     ne_shell(2, 2) = 2
     ne_shell(3, 2) = 4
     !Ne^3+
     be(1, 3) = 962
     be(2, 3) = 113
     be(3, 3) = 94
     p_nl(1:3, 3) = be(1:3, 3) + v(4) - be(3, 3)
     nl_indx(3) = 3
     ne_shell(1, 3) = 2
     ne_shell(2, 3) = 2
     ne_shell(3, 3) = 3
     !Ne^4+
     be(1, 4) = 1004
     be(2, 4) = 143
     be(3, 4) = 123
     p_nl(1:3, 4) = be(1:3, 4) + v(5) - be(3, 4)
     nl_indx(4) = 3
     ne_shell(1, 4) = 2
     ne_shell(2, 4) = 2
     ne_shell(3, 4) = 2
     !Ne^5+
     be(1, 5) = 1048
     be(2, 5) = 169
     be(3, 5) = 158
     p_nl(1:3, 5) = be(1:3, 5) + v(6) - be(3, 5)
     nl_indx(5) = 3
     ne_shell(1, 5) = 2
     ne_shell(2, 5) = 2
     ne_shell(3, 5) = 1
     !Ne^6+
     be(1, 6) = 1099
     be(2, 6) = 204
     p_nl(1:2, 6) = be(1:2, 6) + v(7) - be(2, 6)
     ne_shell(1, 6) = 2
     ne_shell(2, 6) = 2
     ne_shell(3, 6:9) = 0
     !Ne^7+
     be(1, 7) = 1143
     be(2, 7) = 239
     p_nl(1:2, 7) = be(1:2, 7) + v(8) - be(2, 7)
     nl_indx(6:7) = 2
     ne_shell(1, 7) = 2
     ne_shell(2, 7) = 1
     !Ne^8+
     be(1, 8) = 1195
     p_nl(1, 8) = v(9)
     ne_shell(1, 8) = 2
     ne_shell(2, 8:9) = 0
     !Ne^9+
     be(1, 9) = 1362
     p_nl(1, 9) = v(10)
     ne_shell(1, 9) = 1
     nl_indx(8:9) = 1
     !============================
    case (13) !Al [Ne] 3s^2 3p^1
     v(1) = 5.98577
     l_fact(1) = 3 ! 3p shell
     v(2) = 18.8285
     v(3) = 28.4476
     ! 3s shell
     nl_fact(1:3) = 3
     !============ M shell
     v(4) = 119.992
     v(5) = 153.825
     v(6) = 190.40
     v(7) = 241.76
     v(8) = 284.66
     v(9) = 330.13
     l_fact(4:9) = 3 ! 2p shell
     v(10) = 398.75
     v(11) = 442.0
     nl_fact(4:11) = 2
     !======= L-shell
     v(12) = 2085.98
     v(13) = 2304.14
     nl_fact(12:13) = 1
     !=================== K-shell
     !========= Binding energies
     z1_coll = 2
     ne_shell(1, 2:9) = 2 !1s^2
     ne_shell(2, 2:9) = 2 !2s^2
     be(1, 2) = 1592 !A^2+
     be(2, 2) = 150
     be(3, 2) = 104
     be(4, 2) = 28
     p_nl(1:4, 2) = be(1:4, 2) + v(3) - be(4, 2)
     nl_indx(2) = 4
     ne_shell(3, 2) = 6 !2p^6
     ne_shell(4, 2) = 1 !3s^1
     be(1, 3) = 1608 !A^3+
     be(2, 3) = 165
     be(3, 3) = 119
     be(4, 3) = 118
     p_nl(1:4, 3) = be(1:4, 3) + v(4) - be(4, 3)
     nl_indx(3) = 4
     ne_shell(3, 3) = 6 !2p^6
     ne_shell(4, 3:11) = 0 !3s^1
     !A^4+
     be(1, 4) = 1653
     be(2, 4) = 193
     be(3, 4) = 153
     p_nl(1:3, 4) = be(1:3, 4) + v(5) - be(3, 4)
     nl_indx(4) = 3
     ne_shell(3, 4) = 5 !2p^5
     !A^5+
     be(1, 5) = 1703
     be(2, 5) = 227
     be(3, 5) = 196
     p_nl(1:3, 5) = be(1:3, 5) + v(6) - be(3, 5)
     nl_indx(5) = 3
     ne_shell(3, 5) = 4 !2p^4
     !A^6+
     be(1, 6) = 1760
     be(2, 6) = 265
     be(3, 6) = 237
     p_nl(1:3, 6) = be(1:3, 6) + v(7) - be(3, 6)
     nl_indx(6) = 3
     ne_shell(3, 6) = 3 !2p^3
     !A^7+
     be(1, 7) = 1822
     be(2, 7) = 309
     be(3, 7) = 280
     p_nl(1:3, 7) = be(1:3, 7) + v(8) - be(3, 7)
     nl_indx(7) = 3
     ne_shell(3, 7) = 2 !2p^2
     !A^8+
     be(1, 8) = 1885
     be(2, 8) = 346
     be(3, 8) = 331
     p_nl(1:3, 8) = be(1:3, 8) + v(9) - be(3, 8)
     nl_indx(8) = 3
     ne_shell(3, 8) = 1 !2p^1
     !A^9
     be(1, 9) = 1957
     be(2, 9) = 394
     p_nl(1:2, 9) = be(1:2, 9) + v(10) - be(2, 9)
     nl_indx(9) = 2
     ne_shell(3, 9:11) = 0 !2p^1
     !A^10+
     be(1, 10) = 2016
     be(2, 10) = 442
     p_nl(1:2, 10) = be(1:2, 10) + v(11) - be(2, 10)
     nl_indx(10) = 2
     ne_shell(1, 10) = 2
     ne_shell(2, 10) = 1
     !A^11+
     be(1, 11) = 2085
     p_nl(1, 11) = v(12)
     nl_indx(11) = 1
     ne_shell(1, 11) = 2
     ne_shell(2, 11:12) = 0
     !A^12+
     be(1, 12) = 2304
     p_nl(1, 12) = v(13)
     nl_indx(12) = 1
     ne_shell(1, 12) = 1
     !============================
    case (14) !Si [Ne] 3s^2 3p^2
     v(1) = 8.1516
     v(2) = 16.3458
     v(3) = 33.4930
     v(4) = 45.1418
     nl_fact(1:4) = 3
     !========== M-shell 3s,3p
     v(5) = 166.767
     v(6) = 205.27
     v(7) = 246.5
     v(8) = 303.54
     v(9) = 351.12
     v(10) = 401.37
     v(11) = 476.36
     v(12) = 523.42
     !================= L-shell
     nl_fact(5:12) = 2
     v(13) = 2437.63
     v(14) = 2673.18
     nl_fact(13:14) = 1
     !================= K-shell
    case (18) !Ar =  [Ne] 3s^2 3p^6
     v(1) = 15.75962
     v(2) = 27.62967
     v(3) = 40.74
     v(4) = 59.81
     v(5) = 75.02
     v(6) = 91.009
     l_fact(1:6) = 3
     v(7) = 124.323
     v(8) = 143.460
     nl_fact(1:8) = 3
     !==================== M shell nl=3  3s^2  3p^6
     v(9) = 422.45
     v(10) = 478.69
     v(11) = 538.96
     v(12) = 618.26
     v(13) = 686.10
     v(14) = 755.74
     l_fact(9:14) = 3 ! 2p shell
     v(15) = 854.77
     v(16) = 918.03
     nl_fact(9:16) = 2
     !===========================L shell nl=2 2s^2 2p^6
     v(17) = 4120.8857
     v(18) = 4426.2296
     !K-shell nl=1 1s^2 shell
     nl_fact(17:18) = 1
     !============================
     !========================
    case (22) !Ti [Ar] 3d^2 4s^2
     v(1) = 6.82
     v(2) = 13.57
     ! the 4s shell
     v(3) = 27.49
     v(4) = 43.26
     l_fact(3:4) = 5 ! 3d shell
     v(5) = 99.30
     v(6) = 119.53
     v(7) = 140.8
     v(8) = 170.4
     v(9) = 192.1
     v(10) = 215.92
     l_fact(5:10) = 3 ! 3p shell
     v(11) = 265.07
     v(12) = 291.5
     nl_fact(3:12) = 3 !the 3 (s,p,d)shell
     !========== M-shell 3s,3p, 3d
     v(13) = 787.84
     v(14) = 863.1
     v(15) = 941.9
     v(16) = 1044.
     v(17) = 1131.
     v(18) = 1221.
     l_fact(13:18) = 3 ! 2p shell
     v(19) = 1346.
     v(20) = 1425.4
     nl_fact(13:20) = 2
     !========== L-shell 2s,2p
     v(21) = 6249.
     v(22) = 6625.
     nl_fact(21:22) = 1
     !========== K-shell
     !========= Binding energies
     z1_coll = 2
     !==================
     !=====================
     !A2+
     be(1, 2) = 4994 ! 1s
     be(2, 2) = 591 ! 2s
     be(3, 2) = 490 ! 2p
     be(4, 2) = 91 ! 3s
     be(5, 2) = 60 ! 3p
     be(6, 2) = 26 ! 3d-
     p_nl(1:6, 2) = be(1:6, 2) + v(3) - be(6, 2)
     nl_indx(2) = 6
     ne_shell(1, 2:20) = 2 !1s^2
     ne_shell(1, 21) = 1 !1s^1

     ne_shell(2, 2:18) = 2 !2s^2
     ne_shell(2, 19) = 1 !2s^1
     ne_shell(2, 20:21) = 0

     ne_shell(3, 2:12) = 6 !2p^6
     ne_shell(3, 13) = 5 !2p^5
     ne_shell(3, 14) = 4 !2p^4
     ne_shell(3, 15) = 3 !2p^3
     ne_shell(3, 16) = 2 !2p^2
     ne_shell(3, 17) = 1 !2p^1
     ne_shell(3, 18:21) = 0

     ne_shell(4, 2:10) = 2 !3s^2
     ne_shell(4, 11) = 1 !3s^1
     ne_shell(4, 12:21) = 0

     ne_shell(5, 2:4) = 6 !3p^6
     ne_shell(5, 5) = 5 !3p^5
     ne_shell(5, 6) = 4 !3p^4
     ne_shell(5, 7) = 3 !3p^3
     ne_shell(5, 8) = 2 !3p^2
     ne_shell(5, 9) = 1 !3p^1
     ne_shell(5, 10:21) = 0

     ne_shell(6, 2) = 2 !3d^2
     ne_shell(6, 3) = 1 !3d^2
     ne_shell(6, 4:21) = 0
     !===============
     be(1, 3) = 5014 !A^3+
     be(2, 3) = 612
     be(3, 3) = 503
     be(4, 3) = 109
     be(5, 3) = 76
     be(6, 3) = 42
     p_nl(1:6, 3) = be(1:6, 3) + v(4) - be(6, 3)
     nl_indx(3) = 6

     be(1, 4) = 5037 !A^4+
     be(2, 4) = 636
     be(3, 4) = 528
     be(4, 4) = 130
     be(5, 4) = 98
     p_nl(1:5, 4) = be(1:5, 4) + v(5) - be(5, 4)
     nl_indx(4) = 5

     be(1, 5) = 5065 !A^5+
     be(2, 5) = 662
     be(3, 5) = 553
     be(4, 5) = 147
     be(5, 5) = 119
     p_nl(1:5, 5) = be(1:5, 5) + v(6) - be(5, 5)
     nl_indx(5) = 5

     be(1, 6) = 5096 !A^6+
     be(2, 6) = 691
     be(3, 6) = 582
     be(4, 6) = 187
     be(5, 6) = 143
     p_nl(1:5, 6) = be(1:5, 6) + v(7) - be(5, 6)
     nl_indx(6) = 5

     be(1, 7) = 5129 !A^7+
     be(2, 7) = 721
     be(3, 7) = 612
     be(4, 7) = 189
     be(5, 7) = 166
     p_nl(1:5, 7) = be(1:5, 7) + v(8) - be(5, 7)
     nl_indx(7) = 5

     be(1, 8) = 5164 !A^8+
     be(2, 8) = 753
     be(3, 8) = 645
     be(4, 8) = 215
     p_nl(1:4, 8) = be(1:4, 8) + v(9) - be(4, 8)
     nl_indx(8) = 4

     be(1, 9) = 5200 !A^9+
     be(2, 9) = 785
     be(3, 9) = 678
     be(4, 9) = 235
     p_nl(1:4, 9) = be(1:4, 9) + v(10) - be(4, 9)
     nl_indx(9) = 4

     be(1, 10) = 5240 !A^10+
     be(2, 10) = 820
     be(3, 10) = 714
     be(4, 10) = 263
     p_nl(1:4, 10) = be(1:4, 10) + v(11) - be(4, 10)
     nl_indx(10) = 4

     be(1, 11) = 5278 !A^11+
     be(2, 11) = 853
     be(3, 11) = 749
     be(4, 11) = 291
     p_nl(1:4, 11) = be(1:4, 11) + v(12) - be(4, 11)
     nl_indx(11) = 4

     be(1, 12) = 5319 !A^12+
     be(2, 12) = 890
     be(3, 12) = 786
     p_nl(1:3, 12) = be(1:3, 12) + v(13) - be(3, 12)
     nl_indx(12) = 3
     !A^13+
     be(1, 13) = 5421
     be(2, 13) = 951
     be(3, 13) = 864
     p_nl(1:3, 13) = be(1:3, 13) + v(14) - be(3, 13)
     nl_indx(13) = 3
     !A^14+
     be(1, 14) = 5529
     be(2, 14) = 1020
     be(3, 14) = 953
     p_nl(1:3, 14) = be(1:3, 14) + v(15) - be(3, 14)
     nl_indx(14) = 3
     !A^15+
     be(1, 15) = 5645
     be(2, 15) = 1097
     be(3, 15) = 1036
     p_nl(1:3, 15) = be(1:3, 15) + v(16) - be(3, 15)
     nl_indx(15) = 3
     !A^16+
     be(1, 16) = 5768
     be(2, 16) = 1181
     be(3, 16) = 1126
     p_nl(1:3, 16) = be(1:3, 16) + v(17) - be(3, 16)
     nl_indx(16) = 3
     !A^17+
     be(1, 17) = 5888
     be(2, 17) = 1251
     be(3, 17) = 1222
     p_nl(1:3, 17) = be(1:3, 17) + v(18) - be(3, 17)
     nl_indx(17) = 3
     !A18^
     be(1, 18) = 6020
     be(2, 18) = 1340
     p_nl(1:2, 18) = be(1:2, 18) + v(19) - be(2, 18)
     nl_indx(18) = 2
     !A^19+
     be(1, 19) = 6126
     be(2, 19) = 1425
     p_nl(1:2, 19) = be(1:2, 19) + v(20) - be(2, 19)
     nl_indx(19) = 2
     !A^20+
     be(1, 20) = 6248
     p_nl(1, 20) = v(21)
     nl_indx(20) = 1
     !A^21+
     be(1, 21) = 6626
     p_nl(1, 21) = v(22)
     nl_indx(21) = 1
     !==============================
    case (28) !Ni   [Ar] 3d^8 4s^2
     z1_coll = 3
     v(1) = 7.6398
     v(2) = 18.1688
     !============a       the 4s shell
     v(3) = 35.19
     v(4) = 54.9
     v(5) = 76.06
     v(6) = 108.0
     v(7) = 133.0
     v(8) = 162.0
     v(9) = 193.0
     v(10) = 224.6
     l_fact(3:10) = 5 !3d shell
     v(11) = 321.0
     v(12) = 352.0
     v(13) = 384.0
     v(14) = 430.0
     v(15) = 464.0
     v(16) = 499.0
     l_fact(11:16) = 3 !3p shell
     v(17) = 571.08
     v(18) = 607.06
     nl_fact(3:18) = 3
     !==================    the M shell nl=3 3s^2  3p^6  3d^8
     v(19) = 1541.0
     v(20) = 1648.0
     v(21) = 1756.0
     v(22) = 1894.0
     v(23) = 2011.0
     v(24) = 2131.0
     l_fact(19:24) = 3 !2p^6 shell
     v(25) = 2295.0
     v(26) = 2399.2 !2s^2 shell
     nl_fact(19:26) = 2
     !===============      !L shell nl=2 2s^2 2p^6
     v(27) = 10288.8
     v(28) = 10775.4 !1s shell
     nl_fact(27:28) = 1
     !========== K-shell
     !=====================
    case (29) !Cu =[Ar] 3d^10 4s^1
     z1_coll = 2
     v(1) = 7.726
     !============       the 4s shell, nl=4
     v(2) = 20.29
     v(3) = 36.84
     v(4) = 57.38
     v(5) = 79.8
     v(6) = 103.0
     v(7) = 139.0
     v(8) = 166.0
     v(9) = 199.0
     v(10) = 232.0
     v(11) = 265.0
     l_fact(2:11) = 5 !3d shell
     v(12) = 369.0
     v(13) = 384.0
     v(14) = 401.0
     v(15) = 435.0
     v(16) = 484.0
     v(17) = 520.0
     l_fact(12:17) = 3 !3p shell
     v(18) = 557.0
     v(19) = 670.6
     !3s shell
     nl_fact(2:19) = 3
     !===============      !M shell nl=3  3s^2 3p^6 3d^10
     v(20) = 1697.0
     v(21) = 1804.0
     v(22) = 1916.0
     v(23) = 2060.0
     v(24) = 2182.0
     v(25) = 2308.0
     l_fact(20:25) = 3 !2p shell
     v(26) = 2478.0 !2s shell
     v(27) = 2587.8
     nl_fact(20:27) = 2
     !===============      !L shell nl=2  2s^2 2p^6
     v(28) = 11062.0
     v(29) = 11567.0 !K-shell nl=1 1s
     nl_fact(28:29) = 1
     !========== K-shell
    case default
     write (6, *) 'set_ionization_coeff -> atomic number unknown'
     stop
    end select
    !=============== Coefficients for field ionization
    ! in common coefficients functions of(z,sp_ioniz)
    do i = 1, an(j) !V(i) is the potential for z=i-1 => z=i  ionization transition
     v_norm(i, j) = v(i)/v_h
     e_c(i, j) = v_norm(i, j)*v_norm(i, j)/(16.*real(i, dp))
     e_m(i, j) = omega_l*sqrt(v_norm(i, j))
     nstar(i, j) = real(i, dp)/sqrt(v_norm(i, j))
     vfact(i, j) = (v_norm(i, j))*sqrt(v_norm(i, j))
     e_b(i, j) = 2.*vfact(i, j)/(3.*(2.*nstar(i, j) - 1.))
    end do
    lstar = sqrt(v(1)/v_h)
    lstar = 1./lstar - 1.
    do i = 1, an(j)
     c_nstar(i, j) = (2.*euler/nstar(i, j))**(2*nstar(i, j))
     c_nstar(i, j) = c_nstar(i, j)*l_fact(i)/(2.*pig*nstar(i, j))
     c_nstar(i, j) = 0.5*v_norm(i, j)*c_nstar(i, j)
    end do
    !============= m=0 assumed
    !================================
   end do
   !===================================
  end subroutine
  !================================
  subroutine set_impact_ioniz_wfunction(zm, imod)
   integer, intent(in) :: zm, imod
   integer :: i, k, j, m, bc
   real(dp) :: ei, uu, sigma_m, sigma_a
   real(dp) :: g0, g1, g2, g3, fs, rf, gr
   real(dp) :: eta(6), etap(6), qnl(6), sbell(7), efact, efact1
   real(dp) :: a_bell(0:7, 6), mby(6), f_ion
   real(dp), parameter :: lam = 0.067
   !=================== zm is the atomic number
   select case (imod)

   case (1)
    !             Implements GKLV scheme
    eta(1) = 0.499
    eta(2) = 0.4
    eta(3) = 0.57
    eta(4) = 0.65
    eta(5) = 0.70
    eta(6) = 1.15
    etap(1) = 2.0
    etap(2) = 1.0
    etap(3) = 1.0
    etap(4) = 2.0
    etap(5) = 0.30
    etap(6) = 0.1

    dgi = 0.1
    do k = z1_coll, zm - 1 !the ionization state
     qnl(1) = zm - ne_shell(1, k)
     do j = 2, 6
      qnl(j) = zm - sum(ne_shell(1:j, k))
     end do
     do j = 1, nl_indx(k)
      p_nl(j, k) = be(j, k)
     end do
     m = nl_indx(k)
     do i = 1, nec
      ei = p_nl(m, k)*(1.+dgi*real(i, dp))
      e_coll(i, k) = ei/e_unit
     end do
     do j = 1, nl_indx(k)
      p_nl(j, k) = p_nl(j, k)/e_unit
     end do
     do j = 1, nl_indx(k) !the subshells  index max nlindx=6
      efact = eta(j)/p_nl(j, k)
      efact1 = 0.141/p_nl(j, k)
      do i = 1, nec
       sigma_coll(i, j, k) = 0.0
       ei = e_coll(i, k)
       uu = ei/p_nl(j, k)
       if (uu > 1.) then
        fs = 1.
        if (uu <= 1.70) fs = 2.5*(1.-1./uu)
        rf = 1.+0.054*(uu**lam)
        g0 = (ei + 1.)
        g1 = g0*g0/(ei*(g0 + 1.))
        g2 = ei/g0
        g2 = 0.5*g2*g2
        g3 = (2.*ei + 1.)/(g0*g0)
        sigma_m = efact*g1*(1.-(1.-g2 + g3*log(uu))/uu)
        !================
        g3 = 1.243*(ei + 2.)/p_nl(j, k)
        sigma_a = efact1*g1*(log(g3) - ei*(ei + 2.)/(g0*g0))
        !Ei=Ei+etap(j)*P_nl(j,k)/sqrt(1.+qnl(j))
        sigma_a = fs*sigma_a
        sigma_coll(i, j, k) = ne_shell(j, k)*rf*(sigma_a + sigma_m)
       end if
      end do
     end do
    end do
    !Implements (R)MBELL scheme
   case (2)
    !============ the empirical MBEL coefficients in 10^{-13}(eV)^2 cm^2
    a_bell(6:7, 1:4) = 0.0
    ! 1s  subshell
    a_bell(0, 1) = 0.5250
    a_bell(1, 1) = -0.510
    a_bell(2, 1) = 0.200
    a_bell(3, 1) = 0.050
    a_bell(4, 1) = -0.0250
    a_bell(5, 1) = -0.100
    ! 2s  subshell
    a_bell(0, 2) = 0.530
    a_bell(1, 2) = -0.410
    a_bell(2, 2) = 0.150
    a_bell(3, 2) = 0.150
    a_bell(4, 2) = -0.020
    a_bell(5, 2) = -0.150
    ! 2p  subshell
    a_bell(0, 3) = 0.600
    a_bell(1, 3) = -0.400
    a_bell(2, 3) = 0.710
    a_bell(3, 3) = 0.655
    a_bell(4, 3) = 0.425
    a_bell(5, 3) = -0.750
    ! 3s  subshell
    a_bell(0, 4) = 0.130
    a_bell(1, 4) = 0.250
    a_bell(2, 4) = -1.500
    a_bell(3, 4) = 2.400
    a_bell(4, 4) = 3.220
    a_bell(5, 4) = -3.667
    ! 3p  subshell
    a_bell(0, 5) = 0.388
    a_bell(1, 5) = -0.200
    a_bell(2, 5) = -0.2356
    a_bell(3, 5) = 0.5355
    a_bell(4, 5) = 3.150
    a_bell(5, 5) = -8.500
    a_bell(6, 5) = 5.050
    a_bell(7, 5) = 0.370
    ! 3d  subshell
    a_bell(0, 6) = 0.350
    a_bell(1, 6) = 1.600
    a_bell(2, 6) = -3.000
    a_bell(3, 6) = 4.000
    a_bell(4, 6) = 2.000
    a_bell(5, 6) = -5.000
    a_bell(6, 6) = -1.500
    a_bell(7, 6) = 3.500
    mby(1:2) = 1.27
    mby(3) = 0.542
    mby(6) = 0.95
    mby(4) = mby(1)
    mby(5) = mby(3)
    !========================
    dgi = 0.1
    do k = z1_coll, zm - 1 !the ionization state
     qnl(1) = zm - ne_shell(1, k)
     do j = 2, 6
      qnl(j) = zm - sum(ne_shell(1:j, k))
     end do
     m = nl_indx(k)
     do i = 1, nec
      ei = p_nl(m, k)*(1.+dgi*real(i, dp))
      e_coll(i, k) = ei !(E,P) in eV
     end do
     do j = 1, nl_indx(k) !the subshells  index max nlindx=6
      bc = 5
      if (j > 4) bc = 7
      g0 = e_unit/p_nl(j, k)
      do i = 1, nec
       sigma_coll(i, j, k) = 0.0
       ei = p_nl(j, k)*(1.+dgi*real(i, dp))
       uu = ei/p_nl(j, k)
       if (uu > 1.) then
        sbell(1) = (1.-1./uu)
        do m = 2, bc
         sbell(m) = (1.-1./uu)*sbell(m - 1)
        end do
        g1 = (1.+2.*g0)/(uu + 2.*g0)
        g2 = (uu + g0)/(1.+g0)
        g1 = g1*g2*g2
        g2 = (1.+uu)*(uu + 2.*g0)*(1.+g0)*(1.+g0)
        g3 = g0*g0*(1.+2.*g0) + uu*(uu + 2.*g0)*(1.+g0)*(1.+g0)
        gr = g1*(g2/g3)**1.5
        !==============
        f_ion = 1.+3.*(qnl(j)/(uu*zm))**mby(j)
        !=====================
        efact = dot_product(a_bell(1:bc, j), sbell(1:bc))
        sigma_coll(i, j, k) = (a_bell(0, j)*log(uu) + efact)/(p_nl(j, k)*ei)
        sigma_coll(i, j, k) = gr*f_ion*ne_shell(j, k)* &
                              sigma_coll(i, j, k)
       end if
      end do
     end do
    end do
   end select
  end subroutine
  !====================
 end module

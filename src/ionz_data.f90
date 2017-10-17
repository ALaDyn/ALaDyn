 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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
 character(10),dimension(0:28), parameter:: species_name=(/&
  ' Electron ',&
  ' Hydrogen ','  Helium  ',' Lithium  ','Berillium ', &
  ' Boron    ','  Carbon  ',' Nytrogen ',' Oxygen   ', &
  ' Florine  ','   Neon   ',' Sodium   ','Magnesium ', &
  ' Aluminium',' Silicon  ','Phosphorus',' Solfur   ', &
  ' Chlorine ',' Argon    ','Potassium ',' Calcium  ', &
  ' Scandium ',' Titanium ','          ',' Chromium ',&
  'Manganese ','  Iron    ','          ','  Nickel  '/)

 integer,parameter :: An_max=40
 real(dp) :: V(An_max), Vfact(An_max,3),nstar(An_max,3),C_nstar(An_max,3)
 real(dp) :: E_c(An_max,3), E_b(An_max,3),E_m(An_max,3),V_norm(An_max,3)

 integer :: nl_fact(0:An_max),nl_indx(0:An_max),Ne_shell(1:6,0:An_max)
 integer :: l_fact(An_max),z1_coll
 real(dp) :: Be(10,0:An_max), P_nl(10,0:An_max) !(nl shell,ion charge)
 real(dp),allocatable :: Wi(:,:,:),Wsp(:,:,:,:),W_one_lev(:,:,:)
 real(dp),allocatable :: sigma_coll(:,:,:),E_coll(:,:)
 real(dp),parameter :: V_H=13.5984!The H ionizazion potential(eV)
 real(dp),parameter :: e_unit = 0.511*1.e+06 !mc^2(eV)
 real(dp),parameter :: omega_a= 41.3 ! 1/fs=E15/s
 real(dp),parameter :: c_au= 137.0 ! speed of light in au
 real(dp),parameter :: euler= 2.71828,omega_L=0.057 !(laser frequency in a.u.)
 real(dp),parameter :: pig = 3.141592653589793,tiny=1.e-10
 integer,parameter :: N_ge= 10000,Nec=1000
 real(dp) :: lstar,dge,d2ge,de_inv,deb_inv,dt_fs
 real(dp) :: dgi,d2gi,dei_inv

 contains
 subroutine set_atomic_weight(At_number,W_number)
 integer,intent(in) :: At_number
 real,intent(out) :: W_number


 select case(At_number)
 case(1)                    !H
  W_number=1.
 case(2)                    !He
  W_number=4.
 case(3)                    !Li
  W_number=6.
 case(6)                    !C
  W_number=12.
 case(7)                   !N
  W_number=14.
 case(8)                   !O
  W_number=16.
 case(10)                  !Ne
  W_number= 20.
 case(13)               !Al
  W_number =26.98
 case(14)              !Si
  W_number =28.09
 case(18)   !Ar
  W_number =39.948
 case(22)              !Ti
  W_number =47.8
 case(28)              !Ni
  W_number =58.7
 case(29)              !Cu
  W_number =63.54
 end select
 end subroutine set_atomic_weight
 !================================
 subroutine set_ionization_coeff(An,sp_ionz)
 integer,intent(in) :: An(:),sp_ionz
 integer :: i,j,loc_zm
 ! The atomin number An identifies the element
 ! Z_max <= An indicates the local max ionization level allowed
 !==============================================
 loc_zm=maxval(An(1:sp_ionz-1))
 !===========================
 allocate(Wi(N_ge+1,loc_zm+1,sp_ionz))
 allocate(Wsp(0:N_ge+1,0:loc_zm,0:loc_zm,1:sp_ionz))
 allocate(W_one_lev(0:N_ge+1,0:loc_zm,1:sp_ionz))

 Ne_shell=0
 l_fact=1
 z1_coll=1
 nl_indx(0:An_max)=1
 nl_fact(0:An_max)=1
 Be(1:10,0:An_max)=0.0
 P_nl(1:10,1:An_max)=0.0
 !Ne_shell(nl,z)= electrons in suborbitals s,p,d of shell n=1,2,3
 !=================================
 ! Potentials V(i) to ionize from state Z=i-1 to Z= i
 !            V(i) from i=1, atomic number = Z_max
 !==========================================================
 do j=1,sp_ionz-1
  !=========== cycles over all ion species to be ionized sp_ionz=1,2,3
  select case(An(j))            !The atomic number
  case(1)                    !H: 1s
   V(1)= 13.5984
  case(2)                    !He 1s^2
   z1_coll=0
   V(1)=24.5874
   V(2)=54.41776
   Be(1,0)=23.               !He^0+
   Be(1,1)=54.               !He^1+
   P_nl(1,0)=Be(1,0)+V(2)-Be(1,1)
   nl_indx(0)=1
   Ne_shell(1,0)=2
   P_nl(1,1)=V(2)
   Ne_shell(1,1)=1
   nl_indx(1)=1

  case(3)                    !Li [He]2s
   V(1)= 5.3917
   V(2)= 75.6400
   V(3)= 122.4543
  case(6)                    !C [He] 2s^2 2p^2
   V(1)= 11.2603
   V(2)= 24.3833
   l_fact(1:2)=3             ! the 2p shell
   V(3)= 47.8878
   V(4)= 64.4939
   !=========== L shell
   V(5)= 392.087
   V(6)= 489.993
   !=========== K shell
  case(7)                   !N [He] 2s^2 2p^3
   V(1)= 14.53
   V(2)= 29.601
   V(3)= 47.449
   l_fact(1:3)=3            ! the 2p shell
   V(4)= 77.473
   V(5)= 97.89
   nl_fact(1:5)=2
   !========= L-shell
   V(6)= 552.07
   V(7)= 667.046
   nl_fact(6:7)=1           ! the 1s shell
   !========= K-shell
   !========= Binding energies
   z1_coll=0
   Be(1,0)= 411    !N^0+
   Be(2,0)=22
   Be(3,0)=13
   P_nl(1:3,0)=Be(1:3,0)+V(1)-Be(3,0)
   Ne_shell(1,0)=2
   Ne_shell(2,0)=2
   Ne_shell(3,0)=3
   !N^1+
   Be(1,1)=433
   Be(2,1)=39
   Be(3,1)=27
   P_nl(1:3,1)=Be(1:3,1)+V(2)-Be(3,1)
   Ne_shell(1,1)=2
   Ne_shell(2,1)=2
   Ne_shell(3,1)=2

   Be(1,2)=457
   Be(2,2)=53
   Be(3,2)=47
   P_nl(1:3,2)=Be(1:3,2)+V(3)-Be(3,2)
   nl_indx(0:2)=3
   Ne_shell(1,2)=2
   Ne_shell(2,2)=2
   Ne_shell(3,2)=1

   Be(1,3)=488
   Be(2,3)=75
   P_nl(1:2,3)=Be(1:2,3)+V(4)-Be(2,3)
   Ne_shell(1,3)=2
   Ne_shell(2,3)=2
   Ne_shell(3,3:6)=0
   Be(1,4)=516
   Be(2,4)=98
   P_nl(1:2,4)=Be(1:2,4)+V(5)-Be(2,4)
   nl_indx(3:4)=2
   Ne_shell(1,4)=2
   Ne_shell(2,4)=1
   P_nl(1,5)=V(6)
   Ne_shell(1,5)=2
   P_nl(1,6)=V(7)
   Ne_shell(1,6)=1
   Ne_shell(2,5:6)=0
   nl_indx(5:6)=1
   !====================
  case(10)                  !Ne: [He]2s^2 2p^6
   V(1)= 21.5646
   V(2)= 40.9633
   V(3)= 63.45
   V(4)= 97.12
   V(5)= 126.21
   V(6)= 157.93
   l_fact(1:6) = 3         ! the 2p shell
   V(7)= 207.276
   V(8)= 239.099           ! the 2s shell
   nl_fact(1:8)=2
   !========= L-shell
   V(9)= 1195.8286
   V(10)= 1362.199
   nl_fact(9:10)=1
   !========= K-shell
   !========= Binding energies
   z1_coll=0

   Be(1,0)=869    !Ne^0+
   Be(2,0)=49
   Be(3,0)=20
   P_nl(1:3,0)=Be(1:3,0)+V(1)-Be(3,0)
   nl_indx(0)=3
   Ne_shell(1,0)=2
   Ne_shell(2,0)=2
   Ne_shell(3,0)=6
   !Ne^1+
   Be(1,1)=895
   Be(2,1)=66
   Be(3,1)=40
   P_nl(1:3,1)=Be(1:3,1)+V(2)-Be(3,1)
   nl_indx(1)=3
   Ne_shell(1,1)=2
   Ne_shell(2,1)=2
   Ne_shell(3,1)=5
   !Ne^2+
   Be(1,1)=925
   Be(2,2)=87
   Be(3,2)=67
   P_nl(1:3,2)=Be(1:3,2)+V(3)-Be(3,2)
   nl_indx(2)=3
   Ne_shell(1,2)=2
   Ne_shell(2,2)=2
   Ne_shell(3,2)=4
   !Ne^3+
   Be(1,3)=962
   Be(2,3)=113
   Be(3,3)=94
   P_nl(1:3,3)=Be(1:3,3)+V(4)-Be(3,3)
   nl_indx(3)=3
   Ne_shell(1,3)=2
   Ne_shell(2,3)=2
   Ne_shell(3,3)=3
   !Ne^4+
   Be(1,4)=1004
   Be(2,4)=143
   Be(3,4)=123
   P_nl(1:3,4)=Be(1:3,4)+V(5)-Be(3,4)
   nl_indx(4)=3
   Ne_shell(1,4)=2
   Ne_shell(2,4)=2
   Ne_shell(3,4)=2
   !Ne^5+
   Be(1,5)=1048
   Be(2,5)=169
   Be(3,5)=158
   P_nl(1:3,5)=Be(1:3,5)+V(6)-Be(3,5)
   nl_indx(5)=3
   Ne_shell(1,5)=2
   Ne_shell(2,5)=2
   Ne_shell(3,5)=1
   !Ne^6+
   Be(1,6)=1099
   Be(2,6)=204
   P_nl(1:2,6)=Be(1:2,6)+V(7)-Be(2,6)
   Ne_shell(1,6)=2
   Ne_shell(2,6)=2
   Ne_shell(3,6:9)=0
   !Ne^7+
   Be(1,7)=1143
   Be(2,7)=239
   P_nl(1:2,7)=Be(1:2,7)+V(8)-Be(2,7)
   nl_indx(6:7)=2
   Ne_shell(1,7)=2
   Ne_shell(2,7)=1
   !Ne^8+
   Be(1,8)=1195
   P_nl(1,8)=V(9)
   Ne_shell(1,8)=2
   Ne_shell(2,8:9)=0
   !Ne^9+
   Be(1,9)=1362
   P_nl(1,9)=V(10)
   Ne_shell(1,9)=1
   nl_indx(8:9)=1
   !============================
  case(13)                  !Al [Ne] 3s^2 3p^1
   V(1)= 5.98577
   l_fact(1) = 3            ! 3p shell
   V(2)= 18.8285
   V(3)= 28.4476
   ! 3s shell
   nl_fact(1:3)=3
   !============ M shell
   V(4)= 119.992
   V(5)= 153.825
   V(6)= 190.40
   V(7)= 241.76
   V(8)= 284.66
   V(9)= 330.13
   l_fact(4:9) =3           ! 2p shell
   V(10)= 398.75
   V(11)= 442.0
   nl_fact(4:11)=2
   !======= L-shell
   V(12)= 2085.98
   V(13)= 2304.14
   nl_fact(12:13)=1
   !=================== K-shell
   !========= Binding energies
   z1_coll=2
   Ne_shell(1,2:9)=2   !1s^2
   Ne_shell(2,2:9)=2   !2s^2
   Be(1,2)=1592    !A^2+
   Be(2,2)= 150
   Be(3,2)= 104
   Be(4,2)= 28
   P_nl(1:4,2)=Be(1:4,2)+V(3)-Be(4,2)
   nl_indx(2)=4
   Ne_shell(3,2)=6   !2p^6
   Ne_shell(4,2)=1   !3s^1
   Be(1,3)=1608    !A^3+
   Be(2,3)= 165
   Be(3,3)= 119
   Be(4,3)= 118
   P_nl(1:4,3)=Be(1:4,3)+V(4)-Be(4,3)
   nl_indx(3)=4
   Ne_shell(3,3)=6   !2p^6
   Ne_shell(4,3:11)=0!3s^1
   !A^4+
   Be(1,4)=1653
   Be(2,4)= 193
   Be(3,4)= 153
   P_nl(1:3,4)=Be(1:3,4)+V(5)-Be(3,4)
   nl_indx(4)=3
   Ne_shell(3,4)=5   !2p^5
   !A^5+
   Be(1,5)= 1703
   Be(2,5)= 227
   Be(3,5)= 196
   P_nl(1:3,5)=Be(1:3,5)+V(6)-Be(3,5)
   nl_indx(5)=3
   Ne_shell(3,5)=4   !2p^4
   !A^6+
   Be(1,6)= 1760
   Be(2,6)= 265
   Be(3,6)= 237
   P_nl(1:3,6)=Be(1:3,6)+V(7)-Be(3,6)
   nl_indx(6)=3
   Ne_shell(3,6)=3   !2p^3
   !A^7+
   Be(1,7)= 1822
   Be(2,7)= 309
   Be(3,7)= 280
   P_nl(1:3,7)=Be(1:3,7)+V(8)-Be(3,7)
   nl_indx(7)=3
   Ne_shell(3,7)=2   !2p^2
   !A^8+
   Be(1,8)= 1885
   Be(2,8)=346
   Be(3,8)=331
   P_nl(1:3,8)=Be(1:3,8)+V(9)-Be(3,8)
   nl_indx(8)=3
   Ne_shell(3,8)=1   !2p^1
   !A^9
   Be(1,9)= 1957
   Be(2,9)= 394
   P_nl(1:2,9)=Be(1:2,9)+V(10)-Be(2,9)
   nl_indx(9)=2
   Ne_shell(3,9:11)=0   !2p^1
   !A^10+
   Be(1,10)= 2016
   Be(2,10)= 442
   P_nl(1:2,10)=Be(1:2,10)+V(11)-Be(2,10)
   nl_indx(10)=2
   Ne_shell(1,10)=2
   Ne_shell(2,10)=1
   !A^11+
   Be(1,11)=2085
   P_nl(1,11)=V(12)
   nl_indx(11)=1
   Ne_shell(1,11)=2
   Ne_shell(2,11:12)=0
   !A^12+
   Be(1,12)= 2304
   P_nl(1,12)=V(13)
   nl_indx(12)=1
   Ne_shell(1,12)=1
   !============================
  case(14)                  !Si [Ne] 3s^2 3p^2
   V(1)= 8.1516
   V(2)= 16.3458
   V(3)= 33.4930
   V(4)= 45.1418
   nl_fact(1:4)=3
   !========== M-shell 3s,3p
   V(5)= 166.767
   V(6)= 205.27
   V(7)= 246.5
   V(8)= 303.54
   V(9)= 351.12
   V(10)= 401.37
   V(11)= 476.36
   V(12)= 523.42
   !================= L-shell
   nl_fact(5:12)=2
   V(13)= 2437.63
   V(14)= 2673.18
   nl_fact(13:14)=1
   !================= K-shell
  case(18)                  !Ar =  [Ne] 3s^2 3p^6
   V(1) = 15.75962
   V(2) = 27.62967
   V(3) = 40.74
   V(4) = 59.81
   V(5) = 75.02
   V(6) = 91.009
   l_fact(1:6)=3
   V(7) = 124.323
   V(8) = 143.460
   nl_fact(1:8)=3
   !==================== M shell nl=3  3s^2  3p^6
   V(9) = 422.45
   V(10)= 478.69
   V(11)= 538.96
   V(12)= 618.26
   V(13)= 686.10
   V(14)= 755.74
   l_fact(9:14)=3             ! 2p shell
   V(15)= 854.77
   V(16)= 918.03
   nl_fact(9:16)=2
   !===========================L shell nl=2 2s^2 2p^6
   V(17)= 4120.8857
   V(18)= 4426.2296
   !K-shell nl=1 1s^2 shell
   nl_fact(17:18)=1
   !============================
   !========================
  case(22)                  !Ti [Ar] 3d^2 4s^2
   V(1)= 6.82
   V(2)= 13.57
   ! the 4s shell
   V(3)= 27.49
   V(4)= 43.26
   l_fact(3:4) =5           ! 3d shell
   V(5)= 99.30
   V(6)= 119.53
   V(7)= 140.8
   V(8)= 170.4
   V(9)= 192.1
   V(10)= 215.92
   l_fact(5:10)=3           ! 3p shell
   V(11)= 265.07
   V(12)= 291.5
   nl_fact(3:12)=3          !the 3 (s,p,d)shell
   !========== M-shell 3s,3p, 3d
   V(13)= 787.84
   V(14)= 863.1
   V(15)= 941.9
   V(16)= 1044.
   V(17)= 1131.
   V(18)= 1221.
   l_fact(13:18) =3       ! 2p shell
   V(19)= 1346.
   V(20)= 1425.4
   nl_fact(13:20)=2
   !========== L-shell 2s,2p
   V(21)= 6249.
   V(22)= 6625.
   nl_fact(21:22)=1
   !========== K-shell
   !========= Binding energies
   z1_coll=2
   !==================
   !=====================
   !A2+
   Be(1,2)= 4994    ! 1s
   Be(2,2)= 591     ! 2s
   Be(3,2)= 490     ! 2p
   Be(4,2)= 91      ! 3s
   Be(5,2)= 60      ! 3p
   Be(6,2)= 26      ! 3d-
   P_nl(1:6,2)=Be(1:6,2)+V(3)-Be(6,2)
   nl_indx(2)=6
   Ne_shell(1,2:20)=2    !1s^2
   Ne_shell(1,21)=1     !1s^1

   Ne_shell(2,2:18)=2    !2s^2
   Ne_shell(2,19)=1      !2s^1
   Ne_shell(2,20:21)=0

   Ne_shell(3,2:12)=6    !2p^6
   Ne_shell(3,13)=5      !2p^5
   Ne_shell(3,14)=4      !2p^4
   Ne_shell(3,15)=3      !2p^3
   Ne_shell(3,16)=2      !2p^2
   Ne_shell(3,17)=1      !2p^1
   Ne_shell(3,18:21)=0

   Ne_shell(4,2:10)=2      !3s^2
   Ne_shell(4,11)=1        !3s^1
   Ne_shell(4,12:21)=0

   Ne_shell(5,2:4)=6      !3p^6
   Ne_shell(5,5)=5        !3p^5
   Ne_shell(5,6)=4        !3p^4
   Ne_shell(5,7)=3        !3p^3
   Ne_shell(5,8)=2        !3p^2
   Ne_shell(5,9)=1        !3p^1
   Ne_shell(5,10:21)=0

   Ne_shell(6,2)=2      !3d^2
   Ne_shell(6,3)=1      !3d^2
   Ne_shell(6,4:21)=0
   !===============
   Be(1,3)= 5014    !A^3+
   Be(2,3)= 612
   Be(3,3)= 503
   Be(4,3)= 109
   Be(5,3)= 76
   Be(6,3)= 42
   P_nl(1:6,3)=Be(1:6,3)+V(4)-Be(6,3)
   nl_indx(3)=6

   Be(1,4)= 5037    !A^4+
   Be(2,4)= 636
   Be(3,4)= 528
   Be(4,4)= 130
   Be(5,4)= 98
   P_nl(1:5,4)=Be(1:5,4)+V(5)-Be(5,4)
   nl_indx(4)=5

   Be(1,5)= 5065    !A^5+
   Be(2,5)= 662
   Be(3,5)= 553
   Be(4,5)= 147
   Be(5,5)= 119
   P_nl(1:5,5)=Be(1:5,5)+V(6)-Be(5,5)
   nl_indx(5)=5

   Be(1,6)= 5096   !A^6+
   Be(2,6)= 691
   Be(3,6)= 582
   Be(4,6)= 187
   Be(5,6)= 143
   P_nl(1:5,6)=Be(1:5,6)+V(7)-Be(5,6)
   nl_indx(6)=5

   Be(1,7)= 5129    !A^7+
   Be(2,7)= 721
   Be(3,7)= 612
   Be(4,7)= 189
   Be(5,7)= 166
   P_nl(1:5,7)=Be(1:5,7)+V(8)-Be(5,7)
   nl_indx(7)=5

   Be(1,8)= 5164 !A^8+
   Be(2,8)= 753
   Be(3,8)= 645
   Be(4,8)= 215
   P_nl(1:4,8)=Be(1:4,8)+V(9)-Be(4,8)
   nl_indx(8)=4

   Be(1,9)= 5200   !A^9+
   Be(2,9)= 785
   Be(3,9)= 678
   Be(4,9)= 235
   P_nl(1:4,9)=Be(1:4,9)+V(10)-Be(4,9)
   nl_indx(9)=4

   Be(1,10)= 5240   !A^10+
   Be(2,10)= 820
   Be(3,10)= 714
   Be(4,10)= 263
   P_nl(1:4,10)=Be(1:4,10)+V(11)-Be(4,10)
   nl_indx(10)=4

   Be(1,11)= 5278   !A^11+
   Be(2,11)= 853
   Be(3,11)= 749
   Be(4,11)= 291
   P_nl(1:4,11)=Be(1:4,11)+V(12)-Be(4,11)
   nl_indx(11)=4

   Be(1,12)= 5319    !A^12+
   Be(2,12)= 890
   Be(3,12)= 786
   P_nl(1:3,12)=Be(1:3,12)+V(13)-Be(3,12)
   nl_indx(12)=3
   !A^13+
   Be(1,13)= 5421
   Be(2,13)= 951
   Be(3,13)= 864
   P_nl(1:3,13)=Be(1:3,13)+V(14)-Be(3,13)
   nl_indx(13)=3
   !A^14+
   Be(1,14)= 5529
   Be(2,14)= 1020
   Be(3,14)= 953
   P_nl(1:3,14)=Be(1:3,14)+V(15)-Be(3,14)
   nl_indx(14)=3
   !A^15+
   Be(1,15)= 5645
   Be(2,15)= 1097
   Be(3,15)= 1036
   P_nl(1:3,15)=Be(1:3,15)+V(16)-Be(3,15)
   nl_indx(15)=3
   !A^16+
   Be(1,16)= 5768
   Be(2,16)= 1181
   Be(3,16)= 1126
   P_nl(1:3,16)=Be(1:3,16)+V(17)-Be(3,16)
   nl_indx(16)=3
   !A^17+
   Be(1,17)= 5888
   Be(2,17)= 1251
   Be(3,17)= 1222
   P_nl(1:3,17)=Be(1:3,17)+V(18)-Be(3,17)
   nl_indx(17)=3
   !A18^
   Be(1,18)= 6020
   Be(2,18)= 1340
   P_nl(1:2,18)=Be(1:2,18)+V(19)-Be(2,18)
   nl_indx(18)=2
   !A^19+
   Be(1,19)=  6126
   Be(2,19)=  1425
   P_nl(1:2,19)=Be(1:2,19)+V(20)-Be(2,19)
   nl_indx(19)=2
   !A^20+
   Be(1,20)= 6248
   P_nl(1,20)=V(21)
   nl_indx(20)=1
   !A^21+
   Be(1,21)= 6626
   P_nl(1,21)=V(22)
   nl_indx(21)=1
   !==============================
  case(28)             !Ni   [Ar] 3d^8 4s^2
   z1_coll=3
   V(1)= 7.6398
   V(2)= 18.1688
   !============a       the 4s shell
   V(3)= 35.19
   V(4) = 54.9
   V(5) = 76.06
   V(6) = 108.0
   V(7) = 133.0
   V(8) = 162.0
   V(9) = 193.0
   V(10) = 224.6
   l_fact(3:10) = 5      !3d shell
   V(11) = 321.0
   V(12) = 352.0
   V(13) = 384.0
   V(14) = 430.0
   V(15) = 464.0
   V(16)= 499.0
   l_fact(11:16) =3         !3p shell
   V(17) = 571.08
   V(18) = 607.06
   nl_fact(3:18)=3
   !==================    the M shell nl=3 3s^2  3p^6  3d^8
   V(19) = 1541.0
   V(20) = 1648.0
   V(21) = 1756.0
   V(22) = 1894.0
   V(23) = 2011.0
   V(24) = 2131.0
   l_fact(19:24)= 3    !2p^6 shell
   V(25) = 2295.0
   V(26)= 2399.2       !2s^2 shell
   nl_fact(19:26)=2
   !===============      !L shell nl=2 2s^2 2p^6
   V(27) = 10288.8
   V(28) = 10775.4     !1s shell
   nl_fact(27:28)=1
   !========== K-shell
   !=====================
  case(29)            !Cu =[Ar] 3d^10 4s^1
   z1_coll=2
   V(1)= 7.726
   !============       the 4s shell, nl=4
   V(2)= 20.29
   V(3)= 36.84
   V(4)= 57.38
   V(5) = 79.8
   V(6) = 103.0
   V(7) = 139.0
   V(8) = 166.0
   V(9) = 199.0
   V(10) = 232.0
   V(11) = 265.0
   l_fact(2:11) = 5      !3d shell
   V(12) = 369.0
   V(13) = 384.0
   V(14) = 401.0
   V(15) = 435.0
   V(16)= 484.0
   V(17) = 520.0
   l_fact(12:17) =3         !3p shell
   V(18) = 557.0
   V(19) = 670.6
   !3s shell
   nl_fact(2:19)=3
   !===============      !M shell nl=3  3s^2 3p^6 3d^10
   V(20) = 1697.0
   V(21) = 1804.0
   V(22) = 1916.0
   V(23) = 2060.0
   V(24) = 2182.0
   V(25) = 2308.0
   l_fact(20:25)= 3    !2p shell
   V(26)= 2478.0       !2s shell
   V(27) = 2587.8
   nl_fact(20:27)=2
   !===============      !L shell nl=2  2s^2 2p^6
   V(28) = 11062.0
   V(29) = 11567.0     !K-shell nl=1 1s
   nl_fact(28:29)=1
   !========== K-shell
   case DEFAULT
   write(6,*) 'set_ionization_coeff -> atomic number unknown'
   stop
  end select
  !========:====== Coefficients for field ionization
  ! in common coefficients functions of(z,sp_ioniz)
  do i=1,An(j)   !V(i) is the potential for z=i-1 => z=i  ionization transition
   V_norm(i,j)=V(i)/V_H
   E_c(i,j)=V_norm(i,j)*V_norm(i,j)/(16.*real(i,dp))
   E_m(i,j)=omega_L*sqrt(V_norm(i,j))
   nstar(i,j)=real(i,dp)/sqrt(V_norm(i,j))
   Vfact(i,j)=(V_norm(i,j))*sqrt(V_norm(i,j))
   E_b(i,j)=2.*Vfact(i,j)/(3.*(2.*nstar(i,j)-1.))
  end do
  lstar=sqrt(V(1)/V_H)
  lstar=1./lstar -1.
  do i=1,An(j)
   C_nstar(i,j)=(2.*euler/nstar(i,j))**(2*nstar(i,j))
   C_nstar(i,j)=C_nstar(i,j)*l_fact(i)/(2.*pig*nstar(i,j))
   C_nstar(i,j)=0.5*V_norm(i,j)*C_nstar(i,j)
  end do
  !============= m=0 assumed
  !================================
 end do
 !===================================
 end subroutine set_ionization_coeff
 !================================
 subroutine set_impact_ioniz_wfunction(zm,imod)
 integer,intent(in) :: zm,imod
 integer :: i,k,j,m,bc
 real(dp) :: Ei,uu,sigma_m,sigma_a
 real(dp) :: g0,g1,g2,g3,fs,rf,Gr
 real(dp) :: eta(6),etap(6),qnl(6),sbell(7),efact,efact1
 real(dp) :: A_bell(0:7,6),mby(6),F_ion
 real(dp),parameter :: lam= 0.067
 !=================== zm is the atomic number
 select case(imod)

 case (1)
  !             Implements GKLV scheme
  eta(1)= 0.499
  eta(2)= 0.4
  eta(3)= 0.57
  eta(4)= 0.65
  eta(5)= 0.70
  eta(6)= 1.15
  etap(1)= 2.0
  etap(2)= 1.0
  etap(3)= 1.0
  etap(4)= 2.0
  etap(5)= 0.30
  etap(6)= 0.1

  dgi=0.1
  do k=z1_coll,zm-1   !the ionization state
   qnl(1)=zm-Ne_shell(1,k)
   do j=2,6
    qnl(j)=zm-sum(Ne_shell(1:j,k))
   end do
   do j=1, nl_indx(k)
    P_nl(j,k)=Be(j,k)
   end do
   m=nl_indx(k)
   do i=1,Nec
    Ei=P_nl(m,k)*(1.+dgi*real(i,dp))
    E_coll(i,k)=Ei/e_unit
   end do
   do j=1, nl_indx(k)
    P_nl(j,k)=P_nl(j,k)/e_unit
   end do
   do j=1, nl_indx(k)  !the subshells  index max nlindx=6
    efact=eta(j)/P_nl(j,k)
    efact1=0.141/P_nl(j,k)
    do i=1,Nec
     sigma_coll(i,j,k)=0.0
     Ei=E_coll(i,k)
     uu= Ei/P_nl(j,k)
     if(uu > 1.)then
      fs=1.
      if(uu <= 1.70)fs=2.5*(1.-1./uu)
      rf=1.+0.054*(uu**lam)
      g0= (Ei+1.)
      g1= g0*g0/(Ei*(g0+1.))
      g2= Ei/g0
      g2= 0.5*g2*g2
      g3= (2.*Ei+1.)/(g0*g0)
      sigma_m=efact*g1*(1.-(1.-g2+g3*log(uu))/uu)
      !================
      g3=1.243*(Ei+2.)/P_nl(j,k)
      sigma_a= efact1*g1*(log(g3)-Ei*(Ei+2.)/(g0*g0))
      !Ei=Ei+etap(j)*P_nl(j,k)/sqrt(1.+qnl(j))
      sigma_a=fs*sigma_a
      sigma_coll(i,j,k)=Ne_shell(j,k)*rf*(sigma_a+sigma_m)
     endif
    end do
   end do
  end do
  !Implements (R)MBELL scheme
 case (2)
  !============ the empirical MBEL coefficients in 10^{-13}(eV)^2 cm^2
  A_bell(6:7,1:4)=0.0
  ! 1s  subshell
  A_bell(0,1)= 0.5250
  A_bell(1,1)=-0.510
  A_bell(2,1)= 0.200
  A_bell(3,1)= 0.050
  A_bell(4,1)=-0.0250
  A_bell(5,1)=-0.100
  ! 2s  subshell
  A_bell(0,2)= 0.530
  A_bell(1,2)=-0.410
  A_bell(2,2)= 0.150
  A_bell(3,2)= 0.150
  A_bell(4,2)=-0.020
  A_bell(5,2)=-0.150
  ! 2p  subshell
  A_bell(0,3)= 0.600
  A_bell(1,3)=-0.400
  A_bell(2,3)= 0.710
  A_bell(3,3)= 0.655
  A_bell(4,3)= 0.425
  A_bell(5,3)=-0.750
  ! 3s  subshell
  A_bell(0,4)= 0.130
  A_bell(1,4)= 0.250
  A_bell(2,4)=-1.500
  A_bell(3,4)= 2.400
  A_bell(4,4)= 3.220
  A_bell(5,4)=-3.667
  ! 3p  subshell
  A_bell(0,5)= 0.388
  A_bell(1,5)=-0.200
  A_bell(2,5)=-0.2356
  A_bell(3,5)= 0.5355
  A_bell(4,5)= 3.150
  A_bell(5,5)=-8.500
  A_bell(6,5)= 5.050
  A_bell(7,5)= 0.370
  ! 3d  subshell
  A_bell(0,6)= 0.350
  A_bell(1,6)= 1.600
  A_bell(2,6)=-3.000
  A_bell(3,6)= 4.000
  A_bell(4,6)= 2.000
  A_bell(5,6)=-5.000
  A_bell(6,6)=-1.500
  A_bell(7,6)= 3.500
  mby(1:2)=1.27
  mby(3)=0.542
  mby(6)=0.95
  mby(4)=mby(1)
  mby(5)=mby(3)
  !========================
  dgi=0.1
  do k=z1_coll,zm-1   !the ionization state
   qnl(1)=zm-Ne_shell(1,k)
   do j=2,6
    qnl(j)=zm-sum(Ne_shell(1:j,k))
   end do
   m=nl_indx(k)
   do i=1,Nec
    Ei=P_nl(m,k)*(1.+dgi*real(i,dp))
    E_coll(i,k)=Ei                !(E,P) in eV
   end do
   do j=1, nl_indx(k)  !the subshells  index max nlindx=6
    bc=5
    if(j >4)bc=7
    g0=e_unit/P_nl(j,k)
    do i=1,Nec
     sigma_coll(i,j,k)=0.0
     Ei=P_nl(j,k)*(1.+dgi*real(i,dp))
     uu= Ei/P_nl(j,k)
     if(uu > 1.)then
      sbell(1)=(1.-1./uu)
      do m=2,bc
       sbell(m)=(1.-1./uu)*sbell(m-1)
      end do
      g1=(1.+2.*g0)/(uu+2.*g0)
      g2=(uu+g0)/(1.+g0)
      g1=g1*g2*g2
      g2=(1.+uu)*(uu+2.*g0)*(1.+g0)*(1.+g0)
      g3=g0*g0*(1.+2.*g0)+uu*(uu+2.*g0)*(1.+g0)*(1.+g0)
      Gr=g1*(g2/g3)**1.5
      !==============
      F_ion=1.+3.*(qnl(j)/(uu*zm))**mby(j)
      !=====================
      efact=dot_product(A_bell(1:bc,j),sbell(1:bc))
      sigma_coll(i,j,k)=(A_bell(0,j)*log(uu)+efact)/(P_nl(j,k)*Ei)
      sigma_coll(i,j,k)=Gr*F_ion*Ne_shell(j,k)*sigma_coll(i,j,k)
     endif
    end do
   end do
  end do
 end select
 end subroutine set_impact_ioniz_wfunction

 end module ionz_data

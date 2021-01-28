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

 module diag_part_and_fields

  use pstruct_data
  use fstruct_data
  use code_util
  use phys_param
  use parallel
  use grid_param
  use control_bunch_input, only: reduced_charge, bunch_charge, epsy,&
   epsz, sxb, syb, gam, dg
  use array_alloc, only: v_realloc

  implicit none

  real (dp) :: tloc(10000) = zero_dp, tsp(1:1001) = zero_dp, eavg(10, 1001) = zero_dp, &
    eavg1(10, 1001) = zero_dp, pavg(15, 1001, 4) = zero_dp, favg(30, 1001) = zero_dp
  integer, parameter :: ne = 100
  real (dp) :: nde0(ne) = zero_dp, nde1(ne) = zero_dp, nde2(ne) = zero_dp
  real (dp) :: nde(ne, 500, 4) = zero_dp, eksp_max(500, 4) = zero_dp, nde_sm(ne, 500, 4) = zero_dp, &
    nde_sp(ne, 500, 4) = zero_dp
  integer :: ionz_number(500) = 0, hgam_number(500) = 0, bunch_number(500,5) = 0
  real (dp) :: ionz_bavg(500, 18) = zero_dp, bunch_bavg(500, 18,5) = zero_dp, tbunch(1000) = zero_dp, &
    tionz(500) = zero_dp, hgam_charge(500) = zero_dp, ionz_charge(500) = zero_dp,bcharge(500,5) = zero_dp
  real (dp) :: hgam_bavg(500, 18) = zero_dp, tgam(500) = zero_dp

  real (dp), allocatable, dimension(:, :), private :: diag_part_aux

  interface energy_momenta
   module procedure energy_momenta_new
   module procedure energy_momenta_old
  end interface

  interface Envar
   module procedure Envar_new
   module procedure Envar_old
  end interface
 contains

  !==============================================
  subroutine energy_spect(np, ekem)
   integer, intent (in) :: np
   real (dp), intent (in) :: ekem
   integer :: p, ix
   real(dp) :: xx, de, wght
   ! activated only for np>0

   call v_realloc(diag_part_aux, np, nd2 + 1)
   de = ekem/real(ne, dp)
   if (ekem < 1.e-06) return
   do p = 1, np
    xx = diag_part_aux(p, 1)/de !0.5*mc^2*(gamma-1) energy in MeV
    wght = diag_part_aux(p, 2) !weight >0 to be multiplied by np_per_cell
    ix = nint(xx)
    ix = min(ix + 1, ne)
    nde0(ix) = nde0(ix) + wght
   end do
  end subroutine

  subroutine select_energy_spect(np, ekem, xl, xr)
   integer, intent (in) :: np
   real (dp), intent (in) :: ekem, xl, xr
   integer :: p, ix
   real(dp) :: xx, de, wght
   ! activated only for np>0

   call v_realloc( diag_part_aux, np, nd2 + 1)
   de = ekem/real(ne, dp)
   if (ekem < 1.e-06) return
   do p = 1, np
    xx = diag_part_aux(p, 1)/de !0.5*mc^2*(gamma-1) energy in MeV
    wght = diag_part_aux(p, 2) !weight >0
    ix = nint(xx)
    ix = min(ix+1, ne)
    if (diag_part_aux(p,4)<xl) then
     nde0(ix) = nde0(ix) + wght
    end if
    if (diag_part_aux(p,4)>xr) then
     nde1(ix) = nde1(ix) + wght
    end if
   end do

  end subroutine

  !--------------------------

  subroutine energy_momenta_new(spec_in, np, ek, ekmax)
   type (species_new), intent (in) :: spec_in
   integer, intent (in) :: np
   real (dp), intent (out) :: ek(:), ekmax
   integer :: ip, ik
   real (dp) :: xp(3), vp(3), gamm, gam1

   ek = 0.0
   ekmax = 0.0
   ! if (curr_ndim<3) then
   !  do ip = 1, np
   !   vp(1:2) = sp_loc%part(ip, 3:4)
   !   gamm = sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2))
   !   gfield(ip, 1) = pmass*(gamm-1.)
   !   wgh_cmp = sp_loc%part(ip, 5)
   !   gfield(ip, 2) = wgh
   !   gfield(ip, 3) = vp(1)
   !   gfield(ip, 4) = sp_loc%part(ip, 1)
   !   gam1 = gamm - 1.
   !   do ik = 1, curr_ndim
   !    ek(ik) = ek(ik) + wgh*vp(ik)
   !   end do
   !   ek(6) = ek(6) + real(charge*wgh, dp)
   !   ek(7) = ek(7) + real(wgh*gam1, dp)
   !   ekmax = max(ekmax, gam1)
   !  end do
   ! else
   !  do ip = 1, np
   !   xp(1:3) = sp_loc%part(ip, 1:3)
   !   vp(1:3) = sp_loc%part(ip, 4:6)
   !   gamm = sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3))
   !   gfield(ip, 1) = pmass*(gamm-1.)
   !   wgh_cmp = sp_loc%part(ip, 7)
   !   gfield(ip, 2) = wgh
   !   gfield(ip, 3) = vp(1)
   !   gfield(ip, 4) = sp_loc%part(ip, 1)
   !   gam1 = gamm - 1.
   !   do ik = 1, curr_ndim
   !    ek(ik) = ek(ik) + vp(ik) !momenta
   !   end do
   !   ek(4) = ek(4) + wgh*(xp(2)*vp(3)-xp(3)*vp(2))
   !   ek(6) = ek(6) + real(charge*wgh, dp)
   !   ek(7) = ek(7) + real(wgh*gam1, dp)
   !   ekmax = max(ekmax, gam1)
   !  end do
   ! end if
  end subroutine
  !--------------------------

  subroutine energy_momenta_old(sp_loc, np, ek, ekmax)
   type (species), intent (in) :: sp_loc
   integer, intent (in) :: np
   real (dp), intent (out) :: ek(:), ekmax
   integer :: ip, ik
   real(dp) :: xp(3), vp(3), gamm, gam1

   call v_realloc( diag_part_aux, np, nd2 + 1 )
   ek = 0.0
   ekmax = 0.0
   if (curr_ndim < 3) then
    do ip = 1, np
     vp(1:2) = sp_loc%part(ip, 3:4)
     gamm = sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2))
     diag_part_aux(ip, 1) = pmass*(gamm-1.)
     wgh_cmp = sp_loc%part(ip, 5)
     diag_part_aux(ip, 2) = wgh
     diag_part_aux(ip, 3) = vp(1)
     diag_part_aux(ip, 4) = sp_loc%part(ip, 1)
     gam1 = gamm - 1.
     do ik = 1, curr_ndim
      ek(ik) = ek(ik) + wgh*vp(ik)
     end do
     ek(6) = ek(6) + real(charge*wgh, dp)
     ek(7) = ek(7) + real(wgh*gam1, dp)
     ekmax = max(ekmax, gam1)
    end do
   else
    do ip = 1, np
     xp(1:3) = sp_loc%part(ip, 1:3)
     vp(1:3) = sp_loc%part(ip, 4:6)
     gamm = sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3))
     diag_part_aux(ip, 1) = pmass*(gamm-1.)
     wgh_cmp = sp_loc%part(ip, 7)
     diag_part_aux(ip, 2) = wgh
     diag_part_aux(ip, 3) = vp(1)
     diag_part_aux(ip, 4) = sp_loc%part(ip, 1)
     gam1 = gamm - 1.
     do ik = 1, curr_ndim
      ek(ik) = ek(ik) + vp(ik) !momenta
     end do
     ek(4) = ek(4) + wgh*(xp(2)*vp(3) - xp(3)*vp(2))
     ek(6) = ek(6) + real(charge*wgh, dp)
     ek(7) = ek(7) + real(wgh*gam1, dp)
     ekmax = max(ekmax, gam1)
    end do
   end if
  end subroutine

  !--------------------------
  subroutine laser_struct_data(nst)

   integer, intent(in) :: nst
   integer :: i1, j1, k1, i2, ic, nb_tot
   integer :: ik, ix, iy, iz
   real(dp) :: xm, a2, ekt(10), xcm(10), eks(10), xcms(10)

   j1 = jy1
   k1 = kz1
   i1 = ix1
   i2 = nxp
   xm = loc_xgrid(imodx)%gmin

   ekt = 0.0
   xcm = 0.0
   ! !data on driver laser field
   ! CComputes the COM x- coordinates of nb_laser Ey fields (=> group velocity)
   !===================
   ik = 3
   if (ndim < 3) ik = 2
   nb_tot = nb_laser
   if (Two_color) nb_tot = nb_laser + 1
   eavg(1:nb_tot, nst) = 0.0
   !================ field component selection
   do ic = 1, nb_laser
    if (lp_in(ic) > xm) then
     do iz = k1, nzp
      do iy = j1, nyp
       do ix = i1, i2
        if (x(ix) >= lp_in(ic) .and. x(ix) <= lp_end(ic)) then
         a2 = ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik)
         xcm(ic) = xcm(ic) + x(ix)*a2
         ekt(ic) = ekt(ic) + a2
        end if
       end do
      end do
     end do
    end if
   end do
   if (Two_color) then
    if (lp_ionz_in > xm) then
     ic = nb_laser + 1
     do iz = k1, nzp
      do iy = j1, nyp
       do ix = i1, i2
        if (x(ix) >= lp_ionz_in .and. x(ix) <= lp_ionz_end) then
         a2 = ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik)
         xcm(ic) = xcm(ic) + x(ix)*a2
         ekt(ic) = ekt(ic) + a2
        end if
       end do
      end do
     end do
    end if
   end if
   eks(1:nb_tot) = ekt(1:nb_tot)
   xcms(1:nb_tot) = xcm(1:nb_tot)
   call allreduce_dpreal(sumv, ekt, eks, nb_tot)
   call allreduce_dpreal(sumv, xcm, xcms, nb_tot)
   do ic = 1, nb_tot
    if (eks(ic) > 0.0) eavg(ic, nst) = xcms(ic)/eks(ic) !Sum(xE^2)/sum(E^2)
   end do
   !=================
  end subroutine

  subroutine envelope_struct_data(nst)

   integer, intent(in) :: nst
   integer :: i1, j1, k1, i2, kk
   integer :: ik, ix, iy, iz, i01, i02, i0_lp, j, k
   real(dp) :: ekt(7), ekm(7)
   real(dp) :: dvol, dgvol, rr, yy, zz
   real(dp) :: dar, dai, a2, aph1, aph2
   real(dp), parameter :: field_energy = 1.156e-06

   dgvol = dx*dy*dz
   if (ndim == 2) dgvol = dx*dy*dy
   j1 = jy1
   k1 = kz1
   i1 = ix1
   i2 = nxp

   ekt = 0.0

   ! env(1)=Re[A], env(2)=Im[A] A in adimensional form
   aph1 = 0.5*dx_inv
   aph2 = 0.0
   i01 = i1 + 1
   i02 = i2 - 1
   if (der_ord == 4) then
    aph1 = 4.*dx_inv/3.
    aph2 = -dx_inv/6.
    i01 = i01 + 1
    i02 = i02 - 1
   end if
   kk = 0
   do iz = k1, nzp
    do iy = j1, nyp
     do ix = i01, i02
      ik = ix - 2
      dar = aph1*(env(ix + 1, iy, iz, 1) - env(ix - 1, iy, iz, 1)) + &
            aph2*(env(ix + 2, iy, iz, 1) - env(ix - 2, iy, iz, 1))
      dai = aph1*(env(ix + 1, iy, iz, 2) - env(ix - 1, iy, iz, 2)) + &
            aph2*(env(ix + 2, iy, iz, 2) - env(ix - 2, iy, iz, 2))
      a2 = env(ix, iy, iz, 1)*env(ix, iy, iz, 1) + &
           env(ix, iy, iz, 2)*env(ix, iy, iz, 2)

      ekt(1) = ekt(1) + x(ik)*a2 ! Centroid
      ekt(2) = ekt(2) + a2 ! !A|^2
      ekt(6) = dai*env(ix, iy, iz, 1) - dar*env(ix, iy, iz, 2)
      ekt(3) = ekt(3) + oml*oml*a2 + 2.*oml*ekt(6) + dar*dar + dai*dai
      !|Z|^2=(Ey^2+Bz^2)/2= field energy
      ekt(4) = ekt(4) + oml*a2 + ekt(6) ! Action
      ekt(7) = max(ekt(7), sqrt(a2)) ! Max |A|
      kk = kk + 1
     end do
    end do
   end do
   dvol = 1./real(kk, dp)
   call allreduce_dpreal(sumv, ekt, ekm, 4)
   if (ekm(2) > 0.0) then
    ekm(1) = ekm(1)/ekm(2) !Centroid
   end if
   eavg(2, nst) = ekm(1) !Centroid
   eavg(4, nst) = field_energy*dgvol*ekm(3) !Energy
   eavg(5, nst) = dvol*ekm(4) !Action
   !===============
   i0_lp = i1 + nint(dx_inv*ekm(1))
   ekt(1:2) = 0.0
   do iz = k1, nzp
    zz = 0.0
    if (k1>2) then
     k = iz - gcz + 1
     zz = loc_zg(k, 2, imodz)
    end if
    do iy = j1, nyp
     j = iy - gcy + 1
     yy = loc_yg(j, 2, imody)
     rr = sqrt(zz*zz + yy*yy)
     do ix = i01, i02
      a2 = env(ix, iy, iz, 1)*env(ix, iy, iz, 1) + &
           env(ix, iy, iz, 2)*env(ix, iy, iz, 2)
      ekt(1) = ekt(1) + rr*a2
     end do
    end do
   end do
   call allreduce_dpreal(sumv, ekt, ekm, 1)
   if (ekm(2) > 0.0) then
    ekm(1) = ekm(1)/ekm(2) ! env radius
   end if
   eavg(3, nst) = ekm(1) !radius
   !===============
   ekt(1) = ekt(7)
   if (ekt(1) > giant_field) then
    write (6, *) ' WARNING: Env field too big ', ekt(1)
    write (6, '(a23,3i4)') ' At the mpi_task=', imodx, imody, imodz
   end if
   ekm(1) = ekt(1)
   if (prl) call allreduce_dpreal(maxv, ekt, ekm, 1)
   eavg(1, nst) = ekm(1)
   if (Two_color) then
    kk = 0
    ekt = 0.0
    do iz = k1, nzp
     do iy = j1, nyp
      do ix = i1, i2
       ik = ix - 2
       dar = aph1*(env1(ix + 1, iy, iz, 1) - env1(ix - 1, iy, iz, 1)) + &
             aph2*(env1(ix + 2, iy, iz, 1) - env1(ix - 2, iy, iz, 1))
       dai = aph1*(env1(ix + 1, iy, iz, 2) - env1(ix - 1, iy, iz, 2)) + &
             aph2*(env1(ix + 2, iy, iz, 2) - env1(ix - 2, iy, iz, 2))
       a2 = env1(ix, iy, iz, 1)*env1(ix, iy, iz, 1) + &
            env1(ix, iy, iz, 2)*env1(ix, iy, iz, 2)
       ekt(1) = ekt(1) + x(ik)*a2 ! Centroid
       ekt(2) = ekt(2) + a2 ! !A|^2
       ekt(6) = dai*env1(ix, iy, iz, 1) - dar*env1(ix, iy, iz, 2)
       ekt(3) = ekt(3) + om1*om1*a2 + 2.*om1*ekt(6) + dar*dar + dai*dai
       !|Z|^2=(Ey^2+Bz^2)/2= field energy
       ekt(4) = ekt(4) + om1*a2 + ekt(6) ! Action
       ekt(7) = max(ekt(7), sqrt(a2)) ! Max |A|
       kk = kk + 1
      end do
     end do
    end do
    dvol = 1./real(kk, dp)
    call allreduce_dpreal(sumv, ekt, ekm, 4)
    if (ekm(2) > 0.0) then
     ekm(1) = ekm(1)/ekm(2) !Centroid
    end if
    eavg1(2, nst) = ekm(1)
    eavg1(4, nst) = field_energy*dgvol*ekm(3) !Energy
    eavg1(5, nst) = dvol*ekm(4) !Action
    !===============
    i0_lp = i1 + nint(dx_inv*ekm(1))
    ekt(1:2) = 0.0
    do iz = k1, nzp
     zz = 0.0
     if (k1>2) then
      k = iz - gcz + 1
      zz = loc_zg(k, 2, imodz)
     end if
     do iy = j1, nyp
      j = iy - gcy + 1
      yy = loc_yg(j, 2, imody)
      rr = sqrt(zz*zz + yy*yy)
      do ix = i01, i02
       a2 = env1(ix, iy, iz, 1)*env1(ix, iy, iz, 1) + &
            env1(ix, iy, iz, 2)*env1(ix, iy, iz, 2)
       ekt(1) = ekt(1) + rr*a2
      end do
     end do
    end do
    call allreduce_dpreal(sumv, ekt, ekm, 1)
    if (ekm(2) > 0.0) then
     ekm(1) = ekm(1)/ekm(2) ! env radius
    end if
    eavg(3, nst) = ekm(1) !radius
    !===============
    ekt(1) = ekt(7)
    if (ekt(1) > giant_field) then
     write (6, *) ' WARNING: Env1 field too big ', ekt(1)
     write (6, '(a23,3i4)') ' At the mpi_task=', imodx, imody, imodz
    end if
    ekm(1) = ekt(1)
    if (prl) call allreduce_dpreal(maxv, ekt, ekm, 1)
    eavg1(1, nst) = ekm(1)
   end if
  end subroutine
  !===========================
  subroutine fields_on_target(nst)

   integer, intent(in) :: nst
   integer :: i1, j1, k1, i2, ii
   integer :: ik, ix, iy, iz
   real(dp) :: ekt(7), ekm(7)
   real(dp) :: dgvol
   real(dp), parameter :: field_energy = 1.156e-06

   dgvol = dx*dy*dz
   if (ndim == 2) dgvol = dx*dy*dy
   j1 = jy1
   k1 = kz1
   i1 = ix1
   i2 = nxp
   ekt = 0.0
   do ix = i1, i2
    ii = ix - 2
    if (x(ii) >= targ_in) then
     do ik = 1, nfield
      do iz = k1, nzp
       do iy = j1, nyp
        ekt(ik) = ekt(ik) + ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik)
       end do
      end do
     end do
    end if
   end do
   ekt(1:nfield) = dgvol*ekt(1:nfield)
   call allreduce_dpreal(sumv, ekt, ekm, nfield)
   eavg(1:nfield, nst) = field_energy*ekm(1:nfield)
   !=======================
  end subroutine
  
  !=======================
  subroutine Envar_new(nst, spec_in)

   integer, intent (in) :: nst
   type (species_new), dimension(:), intent (in) :: spec_in
   integer :: np, ik, ix, iy, iz, ic, i1, i2, ndv
   integer :: j1, k1, ii, jj, kk, j, k, l
   real (dp) :: ek_max(1), ekt(7), ekm(7), ekmax(1)
   real (dp) :: dvol, dgvol, sgz, sg, ef2
   real (dp) :: np_norm, p_energy_norm
   real (dp), parameter :: mev_to_joule = 1.602e-13
   real (dp), parameter :: field_energy = 1.156e-06
   integer, parameter :: zg_ind(6) = [ 3, 3, 4, 4, 4, 3 ]
   integer, parameter :: yg_ind(6) = [ 3, 4, 3, 4, 3, 4 ]
   integer, parameter :: xg_ind(6) = [ 4, 3, 3, 3, 4, 4 ]
   !================================================
   ! field_energy transforms the energy density u=E^2/2 in adimensional
   ! form to energy density in Joule/mu^3
   ! field_energy =epsilon_0*(E_0^2)/2 in SI or
   !              =(E_0^2)/8pi         in cgs (Gaussian) units
   !==================================================

   ! dgvol = dx*dy*dz
   ! if (ndim==2) dgvol = dx*dy*dy
   ! ndv = nd2 + 1
   ! j1 = jy1
   ! k1 = kz1
   ! i1 = ix1
   ! i2 = nxp

   ! tloc(nst) = tnow
   ! tsp(nst) = tnow
   ! if (nst==1) then
   !  pavg = 0.0
   !  favg = 0.0
   !  eavg = 0.0
   !  nde_sp = 0.0
   !  nde_sm = 0.0
   !  nde = 0.0
   ! end if
   ! ekt(1) = real(nx*ny*nz, dp)
   ! dvol = 1./ekt(1)

   ! if (part) then
   !  p_energy_norm = np_per_cell*mev_to_joule
   !  do ic = 1, nsp
   !   ekm = 0.0
   !   ekt = 0.0
   !   ekmax = 0.0
   !   ek_max = 0.0
   !   np = loc_npart(imody, imodz, imodx, ic)
   !   ekt(1) = real(np, dp)
   !   call allreduce_dpreal(sumv, ekt, ekm, 1)
   !   np_norm = 1.
   !   if (ekm(1)>0.0) np_norm = 1.0/ekm(1)
   !   pmass = electron_mass*mass(ic) !In MeV
   !   ekt(1) = 0.0
   !   ekm(1) = 0.0
   !   if (np>0) then
   !    call energy_momenta(spec(ic), ebfp, np, ekt, ekmax(1))
   !    !  WARNING: total variables multipied by a weight = 1/nmacro_per cell
   !    !  Momenta ekt(1:3) are averaged by the macroparticle total number
   !    !==================================
   !    ekt(4) = pmass*ekt(4) !Total angular momentum (Mev/c^2)
   !    ekt(7) = pmass*ekt(7) !the ic-species TOTAL energy (MeV)
   !    ekmax(1) = pmass*ekmax(1)

   !   end if
   !   call allreduce_dpreal(maxv, ekmax, ek_max, 1)
   !   !============= spectra section
   !   nde0(1:ne) = 0.0
   !   if (np>0) call energy_spect(np, ek_max(1), ebfp)
   !   nde1(1:ne) = nde0(1:ne)
   !   call allreduce_dpreal(sumv, nde0, nde1, ne)
   !   nde(1:ne, nst, ic) = nde1(1:ne)
   !   if (solid_target) then
   !    nde0(1:ne) = 0.0
   !    nde1(1:ne) = 0.0
   !    if (np>0) call select_energy_spect(np, ek_max(1), targ_in, &
   !      targ_end, ebfp)
   !    nde2(1:ne) = nde0(1:ne)
   !    call allreduce_dpreal(sumv, nde0, nde2, ne)
   !    nde_sm(1:ne, nst, ic) = nde2(1:ne)
   !    nde2(1:ne) = nde1(1:ne)
   !    call allreduce_dpreal(sumv, nde1, nde2, ne)
   !    nde_sp(1:ne, nst, ic) = nde2(1:ne)
   !   end if
   !   !======================= end spectra section
   !   call allreduce_dpreal(sumv, ekt, ekm, 7)
   !   do ik = 1, curr_ndim
   !    ekm(ik) = ekm(ik)*np_norm !Average Momenta
   !   end do
   !   !======================= phase space integrated data for each species
   !   eksp_max(nst, ic) = ek_max(1)
   !   pavg(1, nst, ic) = p_energy_norm*ekm(7) !Total energy of ic species (Joule)
   !   pavg(2, nst, ic) = ek_max(1) ! Max energy (MeV)
   !   pavg(3, nst, ic) = mev_to_joule*ekm(4) !total angular Momenta
   !   pavg(4:6, nst, ic) = pmass*ekm(1:3) !averaged linear Momenta (MeV/c)
   !   pavg(10, nst, ic) = np_norm*ekm(6) !Mean charge
   !   pavg(11, nst, ic) = dvol*ekm(6) !Charge per cell
   !   ekt(1:3) = 0.0
   !   if (np>0) then
   !    do ik = 1, curr_ndim
   !     kk = ik + curr_ndim
   !     do ix = 1, np
   !      sg = spec(ic)%part(ix, kk) - ekm(ik)
   !      ekt(ik) = ekt(ik) + sg*sg ! <[p -<p>]^2>, p=gamma*v/c
   !     end do
   !    end do
   !   end if
   !   call allreduce_dpreal(sumv, ekt, ekm, 3)
   !   ekm(1:3) = np_norm*ekm(1:3)
   !   do ik = 1, curr_ndim
   !    pavg(6+ik, nst, ic) = 1.e+03*pmass*ekm(ik) !
   !    !sigma^2 of particle momenta (in KeV)
   !   end do
   !  end do
   ! end if

   ! if (ionization) call enb_ionz(nst, tnow, gam_min) !select ioniz.electrons with gamma > gam_min

   ! if (high_gamma) call enb_hgam(nst, tnow, gam_min)

   ! if (inject_beam) then
   !  bunch_bavg(nst,:,:) = 0.0
   !  tbunch(nst) = tnow
   !  do ik=1,nsb
   !   call enb_bunch(nst,ik)
   !  end do
   ! end if

   ! !   END PARTICLE SECTION
   ! !======================== Field  section
   ! ekt = 0.0
   ! ekm = 0.0
   ! if (stretch) then
   !  if (ndim==3) then
   !   do ik = 1, nfield
   !    k = zg_ind(ik) !staggering of stretched grid cell
   !    j = yg_ind(ik)
   !    l = xg_ind(ik)
   !    do iz = k1, nzp
   !     kk = iz - gcz + 1
   !     sgz = 1./loc_zg(kk, k, imodz)
   !     do iy = j1, nyp
   !      jj = iy - gcy + 1
   !      sg = sgz/loc_yg(jj, j, imody)
   !      do ix = i1, i2
   !       ii = ix - gcx + 1
   !       dvol = sg/loc_xg(ii, l, imodx)
   !       ekt(ik) = ekt(ik) + dvol*ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik &
   !         )
   !      end do
   !     end do
   !    end do
   !   end do
   !  else
   !   do ik = 1, nfield
   !    j = yg_ind(ik)
   !    l = xg_ind(ik)
   !    do iz = k1, nzp
   !     sgz = 1.
   !     do iy = j1, nyp
   !      jj = iy - gcy + 1
   !      sg = sgz/loc_yg(jj, j, imody)
   !      do ix = i1, i2
   !       ii = ix - gcx + 1
   !       dvol = sg/loc_xg(ii, l, imodx)
   !       ekt(ik) = ekt(ik) + dvol*ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik &
   !         )
   !      end do
   !     end do
   !    end do
   !   end do
   !  end if
   ! else
   !  do ik = 1, nfield
   !   do iz = k1, nzp
   !    do iy = j1, nyp
   !     do ix = i1, i2
   !      ekt(ik) = ekt(ik) + ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik)
   !     end do
   !    end do
   !   end do
   !  end do
   ! end if
   ! ekt(1:nfield) = dgvol*ekt(1:nfield)
   ! call allreduce_dpreal(sumv, ekt, ekm, nfield)
   ! favg(1:3, nst) = field_energy*ekm(1:3) !field itotal energy (in Joule)
   ! favg(7:9, nst) = field_energy*ekm(4:6)

   ! ekt = 0.0
   ! do iz = k1, nzp
   !  do iy = j1, nyp
   !   do ix = i1, i2
   !    ef2 = dot_product(ebf(ix,iy,iz,1:curr_ndim), &
   !      ebf(ix,iy,iz,1:curr_ndim))
   !    ekt(7) = max(ekt(7), ef2)
   !   end do
   !  end do
   ! end do
   ! do ik = 1, nfield
   !  if (ekm(ik)>0.0) ekt(ik) = maxval(abs(ebf(i1:i2,j1:nyp,k1:nzp,ik)))
   !  if (ekt(ik)>giant_field) then
   !   write (6, *) ' WARNING: Ebf field too big at component=', ik
   !   write (6, *) 'max fields', mype, ekt(ik)
   !   do iz = k1, nzp
   !    do iy = j1, nyp
   !     do ix = i1, i2
   !      if (abs(ebf(ix,iy,iz,ik)-ekt(ik))<epsilon) then
   !       ii = ix
   !       jj = iy
   !       kk = iz
   !      end if
   !     end do
   !    end do
   !   end do
   !   write (6, '(a23,3i4)') ' At the mpi_task=', imodx, imody, imodz
   !   write (6, '(a19,3i6)') ' At the local grid=', ii, jj, kk
   !  end if
   ! end do
   ! ekm = 0.0 ! Max values
   ! call allreduce_dpreal(maxv, ekt, ekm, 7)
   ! favg(4:6, nst) = e0*ekm(1:3) !Max fields in TV/m
   ! favg(10:12, nst) = e0*ekm(4:6)
   ! !=====================================
   ! if (wake) then
   !  if (envelope) then
   !   call envelope_struct_data(nst)
   !   !else
   !   ! call laser_struct_data(nst)
   !  end if
   ! end if
   ! if (solid_target) call fields_on_target(nst)

  end subroutine
  !=======================
  subroutine Envar_old(nst, spec_in)

   integer, intent (in) :: nst
   type (species), dimension(:), intent (in) :: spec_in
   integer :: np, ik, ix, iy, iz, ic, i1, i2, ndv
   integer :: j1, k1, ii, jj, kk, j, k, l
   integer, allocatable, dimension(:) :: np_all
   real (dp) :: ek_max(1), ekt(7), ekm(7), ekmax(1)
   real (dp) :: dvol, dgvol, sgz, sg, ef2
   real (dp) :: np_norm, p_energy_norm
   real (dp), parameter :: mev_to_joule = 1.602e-13
   real (dp), parameter :: field_energy = 1.156e-06
   integer, parameter :: zg_ind(6) = [ 3, 3, 4, 4, 4, 3 ]
   integer, parameter :: yg_ind(6) = [ 3, 4, 3, 4, 3, 4 ]
   integer, parameter :: xg_ind(6) = [ 4, 3, 3, 3, 4, 4 ]
   !================================================
   ! field_energy transforms the energy density u=E^2/2 in adimensional
   ! form to energy density in Joule/mu^3
   ! field_energy =epsilon_0*(E_0^2)/2 in SI or
   !              =(E_0^2)/8pi         in cgs (Gaussian) units
   !==================================================

   dgvol = dx*dy*dz
   if (ndim == 2) dgvol = dx*dy*dy
   ndv = nd2 + 1
   j1 = jy1
   k1 = kz1
   i1 = ix1
   i2 = nxp
   
   tloc(nst) = tnow
   tsp(nst) = tnow
   if (nst == 1) then
    pavg = 0.0
    favg = 0.0
    eavg = 0.0
    nde_sp = 0.0
    nde_sm = 0.0
    nde = 0.0
   end if
   ekt(1) = real(nx*ny*nz, dp)
   dvol = 1./ekt(1)
   
   if (part) then
    allocate( np_all(nsp) )
    np_all = loc_npart(imody, imodz, imodx, 1:nsp)
    call v_realloc( diag_part_aux, maxval(np_all), nd2 + 1)
    p_energy_norm = np_per_cell*mev_to_joule
    do ic = 1, nsp
     ekm = 0.0
     ekt = 0.0
     ekmax = 0.0
     ek_max = 0.0
     np = loc_npart(imody, imodz, imodx, ic)
     ekt(1) = real(np, dp)
     call allreduce_dpreal(sumv, ekt, ekm, 1)
     np_norm = 1.
     if (ekm(1) > 0.0) np_norm = 1.0/ekm(1)
     pmass = electron_mass*mass(ic) !In MeV
     ekt(1) = 0.0
     ekm(1) = 0.0
     if (np>0) then
      call energy_momenta(spec_in(ic), np, ekt, ekmax(1))
      !  WARNING: total variables multipied by a weight = 1/nmacro_per cell
      !  Momenta ekt(1:3) are averaged by the macroparticle total number
      !==================================
      ekt(4) = pmass*ekt(4) !Total angular momentum (Mev/c^2)
      ekt(7) = pmass*ekt(7) !the ic-species TOTAL energy (MeV)
      ekmax(1) = pmass*ekmax(1)

     end if
     call allreduce_dpreal(maxv, ekmax, ek_max, 1)
     !============= spectra section
     nde0(1:ne) = 0.0
     if (np>0) call energy_spect(np, ek_max(1))
     nde1(1:ne) = nde0(1:ne)
     call allreduce_dpreal(sumv, nde0, nde1, ne)
     nde(1:ne, nst, ic) = nde1(1:ne)
     if (solid_target) then
      nde0(1:ne) = 0.0
      nde1(1:ne) = 0.0
      if (np>0) call select_energy_spect(np, ek_max(1), targ_in, &
        targ_end)
      nde2(1:ne) = nde0(1:ne)
      call allreduce_dpreal(sumv, nde0, nde2, ne)
      nde_sm(1:ne, nst, ic) = nde2(1:ne)
      nde2(1:ne) = nde1(1:ne)
      call allreduce_dpreal(sumv, nde1, nde2, ne)
      nde_sp(1:ne, nst, ic) = nde2(1:ne)
     end if
     !======================= end spectra section
     call allreduce_dpreal(sumv, ekt, ekm, 7)
     do ik = 1, curr_ndim
      ekm(ik) = ekm(ik)*np_norm !Average Momenta
     end do
     !======================= phase space integrated data for each species
     eksp_max(nst, ic) = ek_max(1)
     pavg(1, nst, ic) = p_energy_norm*ekm(7) !Total energy of ic species (Joule)
     pavg(2, nst, ic) = ek_max(1) ! Max energy (MeV)
     pavg(3, nst, ic) = mev_to_joule*ekm(4) !total angular Momenta
     pavg(4:6, nst, ic) = pmass*ekm(1:3) !averaged linear Momenta (MeV/c)
     pavg(10, nst, ic) = np_norm*ekm(6) !Mean charge
     pavg(11, nst, ic) = dvol*ekm(6) !Charge per cell
     ekt(1:3) = 0.0
     if (np > 0) then
      do ik = 1, curr_ndim
       kk = ik + curr_ndim
       do ix = 1, np
        sg = spec_in(ic)%part(ix, kk) - ekm(ik)
        ekt(ik) = ekt(ik) + sg*sg ! <[p -<p>]^2>, p=gamma*v/c
       end do
      end do
     end if
     call allreduce_dpreal(sumv, ekt, ekm, 3)
     ekm(1:3) = np_norm*ekm(1:3)
     do ik = 1, curr_ndim
      pavg(6 + ik, nst, ic) = 1.e+03*pmass*ekm(ik) !
      !sigma^2 of particle momenta (in KeV)
     end do
    end do
   end if

   if (ionization) call enb_ionz(spec_in, nst, tnow, gam_min) !select ioniz.electrons with gamma > gam_min

   if (high_gamma) call enb_hgam(spec_in, nst, tnow, gam_min)

   if (inject_beam) then
    bunch_bavg(nst, :, :) = 0.0
    tbunch(nst) = tnow
    do ik=1,nsb
     call enb_bunch(spec_in, nst,ik)
    end do
   end if

   !   END PARTICLE SECTION
   !======================== Field  section
   ekt = 0.0
   ekm = 0.0
   if (stretch) then
    if (ndim == 3) then
     do ik = 1, nfield
      k = zg_ind(ik) !staggering of stretched grid cell
      j = yg_ind(ik)
      l = xg_ind(ik)
      do iz = k1, nzp
       kk = iz - gcz + 1
       sgz = 1./loc_zg(kk, k, imodz)
       do iy = j1, nyp
        jj = iy - gcy + 1
        sg = sgz/loc_yg(jj, j, imody)
        do ix = i1, i2
         ii = ix - gcx + 1
         dvol = sg/loc_xg(ii, l, imodx)
         ekt(ik) = ekt(ik) + dvol*ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik &
                                                          )
        end do
       end do
      end do
     end do
    else
     do ik = 1, nfield
      j = yg_ind(ik)
      l = xg_ind(ik)
      do iz = k1, nzp
       sgz = 1.
       do iy = j1, nyp
        jj = iy - gcy + 1
        sg = sgz/loc_yg(jj, j, imody)
        do ix = i1, i2
         ii = ix - gcx + 1
         dvol = sg/loc_xg(ii, l, imodx)
         ekt(ik) = ekt(ik) + dvol*ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik &
                                                          )
        end do
       end do
      end do
     end do
    end if
   else
    do ik = 1, nfield
     do iz = k1, nzp
      do iy = j1, nyp
       do ix = i1, i2
        ekt(ik) = ekt(ik) + ebf(ix, iy, iz, ik)*ebf(ix, iy, iz, ik)
       end do
      end do
     end do
    end do
   end if
   ekt(1:nfield) = dgvol*ekt(1:nfield)
   call allreduce_dpreal(sumv, ekt, ekm, nfield)
   favg(1:3, nst) = field_energy*ekm(1:3) !field itotal energy (in Joule)
   favg(7:9, nst) = field_energy*ekm(4:6)

   ekt = 0.0
   do iz = k1, nzp
    do iy = j1, nyp
     do ix = i1, i2
      ef2 = dot_product(ebf(ix, iy, iz, 1:curr_ndim), &
                        ebf(ix, iy, iz, 1:curr_ndim))
      ekt(7) = max(ekt(7), ef2)
     end do
    end do
   end do
   do ik = 1, nfield
    if (ekm(ik) > 0.0) ekt(ik) = maxval(abs(ebf(i1:i2, j1:nyp, k1:nzp, ik)))
    if (ekt(ik) > giant_field) then
     write (6, *) ' WARNING: Ebf field too big at component=', ik
     write (6, *) 'max fields', mype, ekt(ik)
     do iz = k1, nzp
      do iy = j1, nyp
       do ix = i1, i2
        if (abs(ebf(ix, iy, iz, ik) - ekt(ik)) < epsilon) then
         ii = ix
         jj = iy
         kk = iz
        end if
       end do
      end do
     end do
     write (6, '(a23,3i4)') ' At the mpi_task=', imodx, imody, imodz
     write (6, '(a19,3i6)') ' At the local grid=', ii, jj, kk
    end if
   end do
   ekm = 0.0 ! Max values
   call allreduce_dpreal(maxv, ekt, ekm, 7)
   favg(4:6, nst) = e0*ekm(1:3) !Max fields in TV/m
   favg(10:12, nst) = e0*ekm(4:6)
   !=====================================
   if (wake) then
    if (envelope) then
     call envelope_struct_data(nst)
     !else
     ! call laser_struct_data(nst)
    end if
   end if
   if (solid_target) call fields_on_target(nst)

  end subroutine
  !--------------------------
  subroutine bunch_corr(bch, np_loc, np_norm, bcorr)
   real(dp), intent(in) :: bch(:, :)
   integer, intent(in) :: np_loc
   real(dp), intent(out) :: bcorr(16)
   real(dp), intent(in) :: np_norm

   integer :: ik, kk, ndv
   real(dp) :: gmb, pp(3), mu(7), ekt(9), ekm(9)
   real(dp) :: corr2(8), emy, emz, dgam, w_norm
   !=====================
   bcorr = 0.0
   mu = 0.0
   corr2 = 0.0
   ndv = 2*curr_ndim
   ekt = 0.0
   if (np_loc > 0) then
    if (curr_ndim == 2) then
     do kk = 1, np_loc
      wgh_cmp = bch(kk, 5)
      ekt(1:2) = ekt(1:2) + wgh*bch(kk, 1:2) ! <w*X>  <w*Y>
      pp(1:2) = bch(kk, 3:4)
      ekt(3) = ekt(3) + wgh*pp(1)
      ekt(4) = ekt(4) + wgh*pp(2)
      gmb = sqrt(1.+pp(1)*pp(1) + pp(2)*pp(2))
      ekt(ndv + 1) = ekt(ndv + 1) + wgh*gmb
      ekt(8) = ekt(8) + wgh
     end do
     ekt(7) = ekt(ndv + 1) ! <w*gam>
     ekt(6) = 0.0 ! <w*Pz>
     ekt(5) = ekt(4) ! <w*Py>
     ekt(4) = ekt(3) ! <w*Px>
     ekt(3) = 0.0 ! <w*z>
    else
     do kk = 1, np_loc
      wgh_cmp = bch(kk, 7)
      ekt(1:3) = ekt(1:3) + wgh*bch(kk, 1:3) ! weight*(X,Y,Z) coordinates
      pp(1:3) = bch(kk, 4:6)
      gmb = sqrt(1.+pp(1)*pp(1) + pp(2)*pp(2) + pp(3)*pp(3))
      ekt(4:6) = ekt(4:6) + wgh*pp(1:3)
      ekt(7) = ekt(7) + wgh*gmb
      ekt(8) = ekt(8) + wgh
     end do
    end if
   end if
   call allreduce_dpreal(sumv, ekt, ekm, 8)
   w_norm = np_norm
   if (ekm(8) > 0.0) w_norm = 1./ekm(8)
   !===================
   mu(1:3) = w_norm*ekm(1:3) !weighted averages <(x,y,z)>
   mu(4:7) = w_norm*ekm(4:7) !weighted averages <(Px,Py,Pz,gamma)>
   !=========== 2th moments
   ekm = 0.0
   ekt = 0.0
   if (np_loc > 0) then
    if (curr_ndim == 2) then
     do kk = 1, np_loc
      wgh_cmp = bch(kk, 5)
      ekt(1) = ekt(1) + wgh*bch(kk, 1)*bch(kk, 1)
      ekt(2) = ekt(2) + wgh*bch(kk, 2)*bch(kk, 2)
      pp(1:2) = bch(kk, 3:4)
      ekt(3) = ekt(3) + wgh*pp(1)*pp(1) ! <w*p*p>
      ekt(4) = ekt(4) + wgh*pp(2)*pp(2)
      ekt(7) = ekt(7) + wgh*pp(2)*bch(kk, 2) ! <y*w*py>
      gmb = 1.+pp(1)*pp(1) + pp(2)*pp(2)
      ekt(9) = ekt(9) + wgh*gmb ! <w*gam**2>
     end do
     ekt(6) = 0.0 ! <Pz*Pz>
     ekt(8) = 0.0 ! <z*Pz)
     ekt(5) = ekt(4) ! <Py*Py>
     ekt(4) = ekt(3) ! <Px*Px>
     ekt(3) = 0.0 ! <z*z>
    else
     do kk = 1, np_loc
      wgh_cmp = bch(kk, 7)
      ekt(1:3) = ekt(1:3) + wgh*bch(kk, 1:3)*bch(kk, 1:3)
      pp(1:3) = bch(kk, 4:6)
      ekt(4:6) = ekt(4:6) + wgh*pp(1:3)*pp(1:3)
      ekt(7) = ekt(7) + wgh*pp(2)*bch(kk, 2) ! <y*w*py>
      ekt(8) = ekt(8) + wgh*pp(3)*bch(kk, 3) ! <z*w*pz>
      gmb = wgh*(1.+pp(1)*pp(1) + pp(2)*pp(2) + pp(3)*pp(3))
      ekt(9) = ekt(9) + gmb ! <(w*gam**2>
     end do
    end if
   end if
   call allreduce_dpreal(sumv, ekt, ekm, 9)
   ekm(1:9) = w_norm*ekm(1:9)

   do ik = 1, 6
    corr2(ik) = ekm(ik) - mu(ik)*mu(ik) ! <UU>-<U><U>   U[X,Y,Z,Px,Py,Pz]
   end do
   corr2(7) = ekm(7) - mu(2)*mu(5)
   corr2(8) = ekm(8) - mu(3)*mu(6)
   !    emy^2= corr2_y*corr2_py -mixed
   ! <yy><p_yp_y>-(<yp_y>-<y><p_y>)^2
   emy = corr2(2)*corr2(5) - corr2(7)*corr2(7)
   emz = corr2(3)*corr2(6) - corr2(8)*corr2(8)
   gmb = mu(7)*mu(7) ! <gam><gam>
   dgam = 0.0
   if (gmb > 0.0) dgam = ekm(9)/gmb - 1.0
   if (dgam > 0.0) dgam = sqrt(dgam) !Dgamm/gamma
   bcorr(1:6) = mu(1:6)
   bcorr(7:12) = corr2(1:6)
   bcorr(13) = emy
   bcorr(14) = emz
   bcorr(15) = mu(7)
   bcorr(16) = dgam
   !==========================
  end subroutine
!===========================
  subroutine enb_bunch(spec_in, nst, ib)
   type(species), dimension(:), intent(in) :: spec_in
   integer, intent (in) :: nst,ib

   integer :: ik, np, p, q
   real(dp) :: np_norm, bcorr(16), ekt(2), ekm(2)

   ik = 0
   np = loc_npart(imody, imodz, imodx, 1)
   call v_realloc( diag_part_aux, np, nd2 + 1)
   ekt = 0.0
   if (np > 0) then
    select case (curr_ndim)
    case (2)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 5)
      if (part_ind ==ib) then
       ekt(2) = ekt(2) + wgh
       ik = ik + 1
       do q = 1, nd2 + 1
        diag_part_aux(ik, q) = spec_in(1)%part(p, q)
       end do
      end if
     end do
    case (3)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 7)
      if (part_ind ==ib) then
       ekt(2) = ekt(2) + wgh
       ik = ik + 1
       do q = 1, nd2 + 1
        diag_part_aux(ik, q) = spec_in(1)%part(p, q)
       end do
      end if
     end do
    end select
   end if
   !================================
   ekt(1) = real(ik, dp)
   call allreduce_dpreal(sumv, ekt, ekm, 2)
   bunch_number(nst, ib) = nint(ekm(1))
   np_norm = 1.
   if (ekm(1)>0.0) np_norm = 1./ekm(1)
   call bunch_corr(diag_part_aux, ik, np_norm, bcorr)
   bunch_bavg(nst, 1:16,ib) = bcorr(1:16)
   bcharge(nst,ib) = e_charge*np_per_cell*ekm(2)
  end subroutine
!============================================
  subroutine enb_ionz(spec_in, nst, t_loc, gmm)
   type(species), dimension(:), intent(in) :: spec_in
   integer, intent (in) :: nst
   real (dp), intent (in) :: t_loc, gmm

   integer :: ik, np, p, q
   real(dp) :: np_norm, bcorr(16), ekt(2), ekm(2)
   real(dp) :: pp(3), gamma
   real(sp) :: ch_ion

   ik = 0
   ch_ion = real(wgh_ion, sp)
   np = loc_npart(imody, imodz, imodx, 1)
   call v_realloc( diag_part_aux, np, nd2 + 1 )
   ekt = 0.0
   if (np > 0) then
    select case (curr_ndim)
    case (2)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 5)
      pp(1:2) = spec_in(1)%part(p, 3:4)
      gamma = sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2))
      if (part_ind < 0) then
       if (gamma > gmm) then
        ekt(2) = ekt(2) + wgh
        ik = ik + 1
        do q = 1, nd2 + 1
         diag_part_aux(ik, q) = spec_in(1)%part(p, q)
        end do
       end if
      end if
     end do
    case (3)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 7)
      pp(1:3) = spec_in(1)%part(p, 4:6)
      gamma = sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
      if (part_ind < 0) then
       if (gamma > gmm) then
        ekt(2) = ekt(2) + wgh
        ik = ik + 1
        do q = 1, nd2 + 1
         diag_part_aux(ik, q) = spec_in(1)%part(p, q)
        end do
       end if
      end if
     end do
    end select
   end if
   ionz_bavg(nst, :) = 0.0
   tionz(nst) = t_loc
   !================================
   ekt(1) = real(ik, dp)
   call allreduce_dpreal(sumv, ekt, ekm, 2)
   ionz_number(nst) = nint(ekm(1))
   np_norm = 1.
   if (ekm(1)>0.0) np_norm = 1./ekm(1)
   call bunch_corr(diag_part_aux, ik, np_norm, bcorr)
   ionz_bavg(nst, 1:16) = bcorr(1:16)
   ionz_charge(nst) = e_charge*np_per_cell*ekm(2)
  end subroutine
  !============================
  subroutine enb_hgam(spec_in, nst, t_loc, gmm)
   type(species), dimension(:), intent(in) :: spec_in
   integer, intent (in) :: nst
   real (dp), intent (in) :: t_loc, gmm

   integer :: ik, np, p, q
   real(dp) :: np_norm, bcorr(16), ekt(2), ekm(2)
   real(dp) :: pp(3), gamma

   ik = 0
   np = loc_npart(imody, imodz, imodx, 1)
   call v_realloc( diag_part_aux, np, nd2 + 1 )
   ekt = 0.0
   if (np > 0) then
    select case (curr_ndim)
    case (2)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 5)
      pp(1:2) = spec_in(1)%part(p, 3:4)
      gamma = sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2))
      if (gamma > gmm) then
       ekt(2) = ekt(2) + wgh
       ik = ik + 1
       do q = 1, nd2 + 1
        diag_part_aux(ik, q) = spec_in(1)%part(p, q)
       end do
      end if
     end do
    case (3)
     do p = 1, np
      wgh_cmp = spec_in(1)%part(p, 7)
      pp(1:3) = spec_in(1)%part(p, 4:6)
      gamma = sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
      if (gamma > gmm) then
       ekt(2) = ekt(2) + wgh
       ik = ik + 1
       do q = 1, nd2 + 1
        diag_part_aux(ik, q) = spec_in(1)%part(p, q)
       end do
      end if
     end do
    end select
   end if
   hgam_bavg(nst, :) = 0.0
   tgam(nst) = t_loc
   !================================
   ekt(1) = real(ik, dp)
   call allreduce_dpreal(sumv, ekt, ekm, 2)
   hgam_number(nst) = nint(ekm(1))
   np_norm = 1.
   if (ekm(1)>0.0) np_norm = 1./ekm(1)
   call bunch_corr(diag_part_aux, ik, np_norm, bcorr)
   hgam_bavg(nst, 1:16) = bcorr(1:16)
   hgam_charge(nst) = e_charge*np_per_cell*ekm(2)
   !==========================
  end subroutine
  !=====================================
  !   WRITE envar  DATA SECTION
  subroutine en_data(nst, itr, idata)

   integer, intent(in) :: nst, itr, idata

   call general_en_data(nst, itr, idata)

   if (ionization) call en_ionz_data(nst, itr, idata)
   if (high_gamma) call en_high_gamma_data(nst, itr, idata)

  end subroutine

  subroutine general_en_data(nst, itr, idata)

   integer, intent(in) :: nst, itr, idata
   character(6) :: fname = '      '
   character(14), dimension(4), parameter :: sp_type = [ &
                                             '   Electrons  ', '  A1-Z1 Ions  ', '  A2-Z2 Ions  ', &
                                             '  A3-Z3 Ions  ']

   character(14), dimension(11), parameter :: pe = [' Tot Ek[J]    ', &
                                                    ' Ek_max[Mev]  ', ' Jz ang-moment', '<px>-momentum ', &
                                                    '<py>-momentum ', '<pz>-momentum ', 'sigma_px[KeV] ', &
                                                    'sigma_py[KeV] ', 'sigma_pz[KeV] ', 'Mean Charge   ', &
                                                    'Charge percell']
   character(18), dimension(16), parameter :: fe = [ &
                                              '  Ex2(J)          ', '  Ey2(J)          ', '  Ez2(J)          ', &
                                              '  Ex_max(TV/m)    ', '  Ey_max(TV/m)    ', '  Ez_max(TV/m)    ', &
                                              '  Bx2(J)          ', '  By2(J)          ', '  Bz2(J)          ', &
                                              '  Bx_max(TV/m)    ', '  By_max(TV/m)    ', '  Bz_max(TV/m)    ', &
                                              '  E2(x<X_t)       ', '  B2(x<X_t)       ', '  E2(x>X_t)       ', &
                                              '  B2(x>X_t)       ']
   character(18), dimension(6), parameter :: fe2 = [ &
                                             '  Ex2(J)          ', '  Ey2(J)          ', '  Bz2(J)          ', &
                                             '  Ex_max(TV/m)    ', '  Ey_max(TV/m)    ', '  Bz_max(TV/m)    ']
   character(18), dimension(6), parameter :: feb2 = [ &
                                             '  Ex2(J)          ', '  Ey2(J)          ', '  Bz2(J)          ', &
                                             '  Ex_max(GV/m)    ', '  Ey_max(GV/m)    ', '  Bz_max(GV/m)    ']
   character(18), dimension(16), parameter :: feb = [ &
                                              '  Ex2(J)          ', '  Ey2(J)          ', '  Ez2(J)          ', &
                                              '  Ex_max(GV/m)    ', '  Ey_max(GV/m)    ', '  Ez_max(GV/m)    ', &
                                              '  Bx2(J)          ', '  By2(J)          ', '  Bz2(J)          ', &
                                              '  Bx_max(GV/m)    ', '  By_max(GV/m)    ', '  Bz_max(GV/m)    ', &
                                              '  E2(x<X_t)       ', '  B2(x<X_t)       ', '  E2(x>X_t)       ', &
                                              '  B2(x>X_t)       ']
   character(14), dimension(6), parameter :: lfenv = [ &
                                             '  COM(1)      ', '   COM(2)     ', ' COM(3)       ', &
                                             '  COM(4)      ', '  COM(5)      ', '   COM(6)     ']
   character(18), dimension(5), parameter :: fenv = [ &
                                             '  Env_max         ', '  Centroid        ', '  Env radius      ', &
                                             '  Env_energy      ', '  Env_action      ']
   !character(14),dimension(5), parameter:: flaser=(/&
   ! '  Int_max     ',' Las_energy(J)','  < X_c >     ','   <W_y>      ',&
   ! '   < W_z >    '/)
   character(14), dimension(6), parameter :: flt = ['Ex2(J)        ', &
                                                    'Ey2(J)        ', 'Ez2(J)        ', 'Bx2(J)        ', &
                                                    'By2(J)        ', 'Bz2(J)        ']

   character(12), dimension(4), parameter :: enspect = [ &
                                             'Electron NdE', ' A1-ion NdE ', ' A2-ion NdE ', ' A3-ion NdE ']

   integer :: ik, nfv, npv, nt, color
   integer, parameter :: lun = 10

   nfv = 6
   if (curr_ndim == 3) nfv = 12
   npv = 9
   color = 0
   if (Two_color) color = 1

   ! if (iout<100) write (fname,'(a4,i2)') 'diag' ,idata
   ! if (iout< 10) write (fname,'(a5,i1)') 'diag0',idata

   write (fname, '(a4,i2.2)') 'diag', idata

   open (lun, file='diagnostics/'//fname//'.dat', form='formatted')
   write (lun, *) '    mod_id, dmodel_id,    LP_ord,   der_ord, &
     &    ibeam,     color,   n_field'
   write (lun, '(7i11)') model_id, dmodel_id, lpf_ord, der_ord, ibeam, &
    color, nfield
   write (lun, *) '        Part,        Beam,        Wake, Solid_Target'
   write (lun, '(4L13)') part, beam, wake, solid_target
   write (lun, *) 'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
   write (lun, '(6i6)') ion_min(1), atomic_number(1), ion_min(2), &
    atomic_number(2), iform, str_flag
   write (lun, *) ' xmax       xmin       ymax      ymin      '
   write (lun, '(4e12.4)') xmax, xmin, ymax, ymin
   if (model_id <= 4) then
    write (lun, *) ' lam0       w0x       w0y        energy'
    write (lun, '(4e11.4)') lam0, w0_x, w0_y, lp_energy
    write (lun, *) ' a0        lp_int     lp_pow    energy_on_targ'
    write (lun, '(4e12.4)') a0, lp_intensity, lp_pow, energy_in_targ
    write (lun, *) ' targ_x1  targ_x2     n/nc       el_lp        '
    write (lun, '(4e12.4)') targ_in, targ_end, n_over_nc, el_lp
    if (dmodel_id > 5) then
     write (lun, *) '  lx2        lx3        lx4         dw         lw '
     write (lun, '(5e12.4)') lpx(2:4), lpy(1:2)
    else
     write (lun, *) ' lx1        lx2        lx3        lx4        lx5 '
     write (lun, '(5e12.4)') lpx(1:5)
    end if
    write (lun, *) ' ompe2       nmacro       np_per_cell    '
    write (lun, '(3e12.4)') ompe, nmacro, np_per_cell
    write (lun, *) '    Nx      Ny      Nz    n_cell   Nsp  Nb_las'
    write (lun, '(6i8)') nx, ny, nz, mp_per_cell(1), nsp, nb_laser
    write (lun, *) ' iter, nst, nvar npvar'
    write (lun, '(4i6)') itr, nst, nfv, npv
   end if
   write (lun, *) '========== Fields section======='
   if (nfield < 6) then
    write (lun, '(6a18)') fe2(1:6)
    do ik = 1, nst
     write (lun, '(6e18.10)') favg(1:6, ik)
    end do
   else
    write (lun, '(6a18)') fe(1:6)
    do ik = 1, nst
     write (lun, '(6e18.10)') favg(1:6, ik)
    end do
    write (lun, '(6a18)') fe(7:12)
    do ik = 1, nst
     write (lun, '(6e18.10)') favg(7:12, ik)
    end do
   end if
   if (wake) then
    if (envelope) then
     write (lun, *) '====  the leading pulse integrated variables'
     write (lun, '(5a18)') fenv(1:5)
     do ik = 1, nst
      write (lun, '(5e18.10)') eavg(1:5, ik)
     end do
     if (Two_color) then
      write (lun, *) '====  the injection pulse integrated variables'
      write (lun, '(5a18)') fenv(1:5)
      do ik = 1, nst
       write (lun, '(5e18.10)') eavg1(1:5, ik)
      end do
     end if
    end if
   end if
   if (solid_target) then
    write (lun, *) '====  Field energy on solid targets'
    if (nfield == 6) then
     write (lun, '(6a18)') flt(1:6)
     do ik = 1, nst
      write (lun, '(6e18.10)') eavg(1:6, ik)
     end do
    else
     write (lun, '(3a18)') flt(1:3)
     do ik = 1, nst
      write (lun, '(3e18.10)') eavg(1:3, ik)
     end do
    end if
   end if
   close (lun)

   if (nst > 0) then
    ! if (iout<100) write (fname,'(a4,i2)') 'spec' ,idata
    ! if (iout< 10) write (fname,'(a5,i1)') 'spec0',idata
    write (fname, '(a4,i2.2)') 'spec', idata
    open (lun, file='diagnostics/'//fname//'.dat', form='formatted')
    write (lun, *) 'mod_id,dmodel_id LP_ord,der_ord'
    write (lun, '(4i8)') model_id, dmodel_id, lpf_ord, der_ord
    write (lun, *) 'Z1_i,A1_i,Z2_i,A2_i,iform, str'
    write (lun, '(6i4)') ion_min(1), atomic_number(1), ion_min(2), &
     atomic_number(2), iform, str_flag
    write (lun, *) ' xmax       xmin       ymax      ymin      '
    write (lun, '(4e12.4)') xmax, xmin, ymax, ymin
    if (model_id <= 4) then
     write (lun, *) ' lam0       w0x       w0y        tau'
     write (lun, '(4e12.4)') lam0, w0_x, w0_y, tau_fwhm
     write (lun, *) ' a0        lp_int     lp_pow'
     write (lun, '(3e12.4)') a0, lp_intensity, lp_pow
     write (lun, *) ' targ_x1  targ_x2     n/nc       el_lp        '
     write (lun, '(4e12.4)') targ_in, targ_end, n_over_nc, el_lp
     write (lun, *) &
      ' lx1        lx2          lx3          lx4        lx5 '
     write (lun, '(5e12.4)') lpx(1:5)
     write (lun, *) ' ompe2       nmacro       np_per_cell    '
     write (lun, '(3e12.4)') ompe, nmacro, np_per_cell
    end if
    write (lun, *) '    Nx      Ny      Nz    n_cell   Nsp  Nsb'
    write (lun, '(6i8)') nx, ny, nz, mp_per_cell(1), nsp, nsb
    write (lun, *) ' iter, nst, nvar npvar'
    write (lun, '(4i6)') itr, nst, nfv, npv
    write (lun, *) '             ENERGY SPECTRA            '
    write (lun, '(i4)') ne
    do ik = 1, nsp
     write (lun, *) enspect(ik)
     do nt = 1, nst
      write (lun, *) ' time   emax'
      write (lun, '(2e13.5)') tsp(nt), eksp_max(nt, ik)
      write (lun, *) 'Global  spectral  data  '
      write (lun, '(6e13.5)') nde(1:ne, nt, ik)
      if (solid_target) then
       write (lun, *) 'Front side  selected spectral  data  '
       write (lun, '(6e13.5)') nde_sm(1:ne, nt, ik)
       write (lun, *) 'Rear side   selected spectral  data  '
       write (lun, '(6e13.5)') nde_sp(1:ne, nt, ik)
      end if
     end do
    end do
    close (lun)
   end if
  end subroutine
  !--------------------------
  subroutine en_bdata(nst, it, idata)

   integer, intent(in) :: nst, it, idata
   character(7) :: bfname = '       '
   character(14), dimension(16), parameter :: fb = ['     <X>      ', &
                                                    '     <Y>      ', '     <Z>      ', '     <Px>     ', &
                                                    '     <Py>     ', '     <Pz>     ', '   <msqX>     ', &
                                                    '   <msqY>     ', '   <msqZ>     ', '  <msqPx>     ', &
                                                    '  <msqPy>     ', '  <msqPz>     ', '   <Emysq>    ', &
                                                    '   <Emzsq>    ', '   <Gam>      ', '   DGam/Gam   ']

   integer :: ib, nbvar, ik, color
   integer, parameter :: lun = 10

   color = 0
   if (Two_color) color = 1

   nbvar = 16
   write (bfname, '(a5,i2.2)') 'bdiag', idata
   open (lun, file='diagnostics/'//bfname//'.dat', form='formatted')
   write (lun, *) 'mod_id,dmodel_id LP_ord,der_ord, ibeam,  color'
   write (lun, '(6i6)') model_id, dmodel_id, lpf_ord, der_ord, ibeam, &
    color
   write (lun, *) 'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
   write (lun, '(6i6)') ion_min(1), atomic_number(1), ion_min(2), &
    atomic_number(2), iform, str_flag
   write (lun, *) ' xmax       xmin       ymax      ymin      '
   write (lun, '(4e12.4)') xmax, xmin, ymax, ymin
   write (lun, *) ' ompe2       nmacro       np_per_cell    '
   write (lun, '(3e12.4)') ompe, nmacro, np_per_cell
   write (lun, *) '    Nx      Ny      Nz    n_cell   Nsp  Nbeam'
   write (lun, '(6i8)') nx, ny, nz, mp_per_cell(1), nsp, nsb
   write (lun, *) ' iter, nst, npvar'
   write (lun, '(3i6)') it, nst, nbvar
   write (lun, *) '====================================='
   write (lun, *) 'time'
   write (lun, '(6e13.4)') tbunch(1:nst)
   do ib = 1, nsb
    write (lun, *) ' bunch numbers '
    write (lun, '(6i10)') bunch_number(1:nst, ib)
    write (lun, *) ' bunch charge(pC)'
    write (lun, '(6e13.4)') bcharge(1:nst, ib)
    write (lun, '(6a14)') fb(1:6)
    do ik = 1, nst
     write (lun, '(6e13.4)') bunch_bavg(ik, 1:6, ib)
    end do
    write (lun, '(6a14)') fb(7:12)
    do ik = 1, nst
     write (lun, '(6e13.4)') bunch_bavg(ik, 7:12, ib)
    end do
    write (lun, '(4a14)') fb(13:16)
    do ik = 1, nst
     write (lun, '(4e13.4)') bunch_bavg(ik, 13:16, ib)
    end do
   end do
   close (lun)
  end subroutine

  subroutine en_ionz_data(nst, itrz, data_id)

   integer, intent(in) :: nst, itrz, data_id
   character(12) :: fname = '            '
   character(14), dimension(16), parameter :: fb = ['     <X>      ', &
                                                    '     <Y>      ', '     <Z>      ', '     <Px>     ', &
                                                    '     <Py>     ', '     <Pz>     ', '   <msqX>     ', &
                                                    '   <msqY>     ', '   <msqZ>     ', '  <msqPx>     ', &
                                                    '  <msqPy>     ', '  <msqPz>     ', '   <Emysq>    ', &
                                                    '   <Emzsq>    ', '   <Gam>      ', '   DGam/Gam   ']

   integer :: ik, color, npv
   integer, parameter :: lun = 20

   color = 0
   if (Two_color) color = 1
   npv = 16
   write (fname, '(a10,i2.2)') 'ionz_emitt', data_id
   open (lun, file='diagnostics/'//fname//'.dat', form='formatted')
   write (lun, *) 'mod_id,dmodel_id LP_ord,der_ord, ibeam,  color'
   write (lun, '(6i6)') model_id, dmodel_id, lpf_ord, der_ord, ibeam, &
    color
   write (lun, *) 'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
   write (lun, '(6i6)') ion_min(1), atomic_number(1), ion_min(2), &
    atomic_number(2), iform, str_flag
   write (lun, *) ' xmax       xmin       ymax      ymin      '
   write (lun, '(4e12.4)') xmax, xmin, ymax, ymin
   if (model_id <= 4) then
    write (lun, *) ' lam0       w0x       w0y        energy'
    write (lun, '(4e11.4)') lam0, w0_x, w0_y, lp_energy
    write (lun, *) ' a0        lp_int     lp_pow    energy_on_targ'
    write (lun, '(4e12.4)') a0, lp_intensity, lp_pow, energy_in_targ
    write (lun, *) ' targ_x1  targ_x2     n/nc       el_lp        '
    write (lun, '(4e12.4)') targ_in, targ_end, n_over_nc, el_lp
   end if
   write (lun, *) ' ompe2       nmacro       np_per_cell    '
   write (lun, '(3e12.4)') ompe, nmacro, np_per_cell
   write (lun, *) '    Nx      Ny      Nz    n_cell   Nsp  Nb_las'
   write (lun, '(6i8)') nx, ny, nz, mp_per_cell(1), nsp, nb_laser
   write (lun, *) ' iter, nst, npvar'
   write (lun, '(3i6)') itrz, nst, npv
   write (lun, *) '====================================='
   write (lun, *) 'time'
   write (lun, '(5e11.4)') tionz(1:nst)
   write (lun, *) ' ionization-hg numbers '
   write (lun, '(6i10)') ionz_number(1:nst)
   write (lun, *) ' ionization-hg charge(pC)'
   write (lun, '(6e13.4)') ionz_charge(1:nst)
   write (lun, '(6a14)') fb(1:6)
   do ik = 1, nst
    write (lun, '(6e13.4)') ionz_bavg(ik, 1:6)
   end do
   write (lun, '(6a14)') fb(7:12)
   do ik = 1, nst
    write (lun, '(6e13.4)') ionz_bavg(ik, 7:12)
   end do
   write (lun, '(4a14)') fb(13:16)
   do ik = 1, nst
    write (lun, '(4e13.4)') ionz_bavg(ik, 13:16)
   end do
   close (lun)
  end subroutine

  subroutine en_high_gamma_data(nst, itrz, data_id)

   integer, intent(in) :: nst, itrz, data_id
   character(12) :: fname = '            '
   character(14), dimension(16), parameter :: fb = ['     <X>      ', &
                                                    '     <Y>      ', '     <Z>      ', '     <Px>     ', &
                                                    '     <Py>     ', '     <Pz>     ', '   <msqX>     ', &
                                                    '   <msqY>     ', '   <msqZ>     ', '  <msqPx>     ', &
                                                    '  <msqPy>     ', '  <msqPz>     ', '   <Emysq>    ', &
                                                    '   <Emzsq>    ', '   <Gam>      ', '   DGam/Gam   ']

   integer :: ik, color, npv
   integer, parameter :: lun = 10

   color = 0
   if (Two_color) color = 1
   npv = 16
   write (fname, '(a10,i2.2)') 'higm_emitt', data_id

   open (lun, file='diagnostics/'//fname//'.dat', form='formatted')
   write (lun, *) 'mod_id,dmodel_id LP_ord,der_ord, ibeam,  color'
   write (lun, '(6i6)') model_id, dmodel_id, lpf_ord, der_ord, ibeam, &
    color
   write (lun, *) 'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
   write (lun, '(6i6)') ion_min(1), atomic_number(1), ion_min(2), &
    atomic_number(2), iform, str_flag
   write (lun, *) ' xmax       xmin       ymax      ymin      '
   write (lun, '(4e12.4)') xmax, xmin, ymax, ymin
   if (model_id <= 4) then
    write (lun, *) ' lam0       w0x       w0y        energy'
    write (lun, '(4e11.4)') lam0, w0_x, w0_y, lp_energy
    write (lun, *) ' a0        lp_int     lp_pow    energy_on_targ'
    write (lun, '(4e12.4)') a0, lp_intensity, lp_pow, energy_in_targ
    write (lun, *) ' targ_x1  targ_x2     n/nc       el_lp        '
    write (lun, '(4e12.4)') targ_in, targ_end, n_over_nc, el_lp
   end if
   write (lun, *) ' ompe2       nmacro       np_per_cell    '
   write (lun, '(3e12.4)') ompe, nmacro, np_per_cell
   write (lun, *) '    Nx      Ny      Nz    n_cell   Nsp  Nb_las'
   write (lun, '(6i8)') nx, ny, nz, mp_per_cell(1), nsp, nb_laser
   write (lun, *) ' iter, nst, npvar'
   write (lun, '(3i6)') itrz, nst, npv
   write (lun, *) '====================================='
   write (lun, *) 'time'
   write (lun, '(5e11.4)') tloc(1:nst)
   write (lun, *) ' higamma numbers '
   write (lun, '(5i8)') hgam_number(1:nst)
   write (lun, *) ' higamma charge(pC) '
   write (lun, '(5E13.4)') hgam_charge(1:nst)
   write (lun, '(6a14)') fb(1:6)
   do ik = 1, nst
    write (lun, '(6e13.4)') hgam_bavg(ik, 1:6)
   end do
   write (lun, '(6a14)') fb(7:12)
   do ik = 1, nst
    write (lun, '(6e13.4)') hgam_bavg(ik, 7:12)
   end do
   write (lun, '(4a14)') fb(13:16)
   do ik = 1, nst
    write (lun, '(4e13.4)') hgam_bavg(ik, 13:16)
   end do
   close (lun)
  end subroutine
  !--------------------------
 end module

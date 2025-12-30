
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

 module curr_and_fields_util
  use pstruct_data
  use fstruct_data
  use grid_param
  use mpi_curr_interface
  use mpi_field_interface
  use grid_part_connect
  use grid_fields
  use init_grid_field
  use util, only: write_warning
  
  implicit none
  interface set_lpf_acc
   module procedure set_lpf_acc_new
   module procedure set_lpf_acc_old
  end interface

  interface field_charge_multiply
   module procedure field_charge_multiply_new
   module procedure field_charge_multiply_old
  end interface

  interface curr_accumulate
   module procedure curr_accumulate_new
   module procedure curr_accumulate_old
  end interface
  !===============================
  ! MOVING WINDOW SECTION
  !=============================
 contains
  !============================
  subroutine set_lpf_acc_new(ef, sp_loc, apt, np, nf, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: apt
   integer, intent (in) :: np, nf
   type(memory_pool_t), pointer, intent(in) :: mempool

   ! Uses alternating order quadratic or linear shapes

   select case (ndim)
   case (1)
    call set_part1d_acc(ef, sp_loc, apt, np, nf, mempool)
   case (2)
    call set_part2d_hcell_acc(ef, sp_loc, apt, np, nf, mempool)
   case (3)
    call set_part3d_hcell_acc(ef, sp_loc, apt, np, mempool)
   end select
  end subroutine

  subroutine set_lpf_acc_old(ef, sp_loc, apt, np, nf)

   real(dp), intent(in) :: ef(:, :, :, :)
   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: apt(:, :)
   integer, intent(in) :: np, nf

   ! Uses alternating order quadratic or linear shapes

   select case (ndim)
   case (1)
    call set_part1d_acc(ef, sp_loc, apt, np, nf)
   case (2)
    call set_part2d_hcell_acc(ef, sp_loc, apt, np, nf)
   case (3)
    call set_part3d_hcell_acc(ef, sp_loc, apt, np)
   end select
  end subroutine

  subroutine field_charge_multiply_old(sp_loc, apt, np, ncmp)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: apt(:, :)
   integer, intent(in) :: np, ncmp
   integer :: p, ch

   ch = size(apt, 2)
   !==========================
   do p = 1, np
    wgh_cmp = sp_loc%part(p, ch)
    apt(p, 1:ncmp) = charge*apt(p, 1:ncmp)
   end do
   ! EXIT p-assigned (E,B) fields multiplied by charge
  end subroutine

  subroutine field_charge_multiply_new(sp_loc, apt)

   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: apt
   real(dp) :: ch

   ch = sp_loc%pick_charge()
   !==========================
   call multiply_field_charge(apt, ch)
   ! EXIT p-assigned (E,B) fields multiplied by charge
  end subroutine

  subroutine curr_accumulate_old(sp_loc, pdata, curr, npt)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pdata(:, :), curr(:, :, :, :)
   integer, intent (in) :: npt
   ! real(dp),intent(in) :: dtloc
   !=========================
   ! charge preserving for iform=0, 1
   !iform=0 => (D_xJx,D_yJy,D_zJz) inverted on each particle=> (Jx,Jy,Jz)
   !iform=1    as iform=0
   !iform=2    no charge preserving
   !=========================
   if (npt == 0) return
   if (ndim < 3) then
    if (iform < 2) then
     call esirkepov_2d_curr(sp_loc, pdata, curr, npt)
    else
     call ncdef_2d_curr(sp_loc, pdata, curr, npt)
    end if
    return
   end if
   if (iform < 2) then
    call esirkepov_3d_curr(sp_loc, pdata, curr, npt)
   else
    call ncdef_3d_curr(sp_loc, pdata, curr, npt)
   end if
   !========================
   ! accumulates for each species currents on curr(i1:n1p,j1:n2p,k1:n3p,1:compnent)
   !============================
  end subroutine

  !==============================
  subroutine curr_accumulate_new(sp_loc, pdata, curr, npt, mempool)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent(inout) :: pdata
   real (dp), intent (inout) :: curr(:, :, :, :)
   integer, intent (in) :: npt
   type(memory_pool_t), pointer, intent(in) :: mempool
   ! real(dp),intent(in) :: dtloc
   !=========================
   ! charge preserving for iform=0, 1
   !iform=0 => (D_xJx,D_yJy,D_zJz) inverted on each particle=> (Jx,Jy,Jz)
   !iform=1    as iform=0
   !iform=2    no charge preserving
   !=========================
   if (npt==0) return
   if (ndim<3) then
    if (iform<2) then
     call esirkepov_2d_curr(sp_loc, pdata, curr, npt, mempool)
    else
     call ncdef_2d_curr(sp_loc, pdata, curr, npt, mempool)
    end if
    return
   end if
   if (iform<2) then
    call esirkepov_3d_curr(sp_loc, pdata, curr, npt, mempool)
   else
    call ncdef_3d_curr(sp_loc, pdata, curr, npt, mempool)
   end if
   !========================
   ! accumulates for each species currents on curr(i1:n1p,j1:n2p,k1:n3p,1:compnent)
   !============================
  end subroutine
  !==============================
  subroutine curr_mpi_collect(curr)

   real(dp), intent(inout) :: curr(:, :, :, :)
   integer :: i, j, k, jj, kk
   real(dp) :: dery, derhy, derz, derhz
   !============sums data on ghost points
   if (prl) then
    call fill_curr_yzxbdsdata(curr, curr_ndim)
   end if
   call jc_xyzbd(curr, curr_ndim)
   !=================
   if (iform < 2) then
    do i = 1, ndim
     curr(:, :, :, i) = djc(i)*curr(:, :, :, i)
    end do
   end if
   if (stretch) then
    select case (curr_ndim)
    case (2)
     do k = kz1, kz2
      do j = jy1, jy2
       jj = j - gcy + 1
       dery = loc_yg(jj, 3, imody)
       derhy = loc_yg(jj, 4, imody)
       do i = ix1, ix2
        curr(i, j, k, 1) = dery*curr(i, j, k, 1)
        curr(i, j, k, 2) = derhy*curr(i, j, k, 2)
       end do
      end do
     end do
    case (3)
     do k = kz1, kz2
      kk = k - gcz + 1
      derz = loc_zg(kk, 3, imodz)
      derhz = loc_zg(kk, 4, imodz)
      do j = jy1, jy2
       jj = j - gcy + 1
       dery = loc_yg(jj, 3, imody)
       derhy = loc_yg(jj, 4, imody)
       do i = ix1, ix2
        curr(i, j, k, 1) = dery*derz*curr(i, j, k, 1)
        curr(i, j, k, 2) = derhy*derz*curr(i, j, k, 2)
        curr(i, j, k, 3) = dery*derhz*curr(i, j, k, 3)
       end do
      end do
     end do
    end select
   end if
  end subroutine
  !=================================
  ! END SECTION ON GRID DEFINED PARTICLE VARIABLES
  !================================================
  subroutine pfields_prepare(ef, nc, spr, spl)
   real(dp), intent(inout) :: ef(:, :, :, :)
   integer, intent(in) :: nc, spr, spl
   !===================
   ! Enter fields ef[i1:i2,j1,j2,k1,k2,1:nc)
   ! Exit fields ef[i1:i2,j1,j2,k1,k2,1:nc)
   !===========================
   if (prl) then
    call fill_ebfield_yzxbdsdata(ef, 1, nc, spr, spl)
    !======================
    ! Adds point data => spl to the left spr to the right
    ! Sets periodic BCs for
    ! iby,ibz,ibx =2
    !==================
   end if
   call field_xyzbd(ef, nc)
   ! extends for one point at the box boundaries
   !==================================================
  end subroutine
  !================================
  subroutine advance_lpf_fields(ef, curr, ibd)
   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: curr(:, :, :, :)
   integer, intent(in) :: ibd
   integer :: str, stl, ik
   real(dp) :: dth, dt_lp

   dt_lp = dt_loc
   dth = 0.5*dt_lp
   !=======================
   ! A LPF order Lpf with time-centered source term
   !=============================
   if (prl) then
    str = 1
    stl = 2
    call fill_ebfield_yzxbdsdata(ef, 1, curr_ndim, str, stl)
    ! To fill electric field data
    ! sends stl to the left,
    ! recvs stl points from right at (nyp+stl), (nzp+stl)
   end if
   !============== first substep dt/2 advance of B-field
   call ef_bds(ef, zero_dp, ibd)
   ! Uses upper BCs of E fields: ibd=0 for inflow-outflow
   !                             ibd=1 for symmetric
   if (comoving) then
    call field_xadvect(ef, dth, -vbeam, curr_ndim + 1, nfield, 0)
    ! Solves for one-half step backward advection B field explicit
    ! B^{n} => B^{n}+v_b*Dth[Dx]B^n  v_b >0
   end if
   !==================================
   ! solves B^{n+1/2}= B^n -Dth[rot E]^n
   !============================
   call rote(ef, dth)
   !=============================
   !============== central step for advance of E-field
   if (prl) then
    str = 2
    stl = 1
    call fill_ebfield_yzxbdsdata(ef, curr_ndim + 1, nfield, str, stl)
    ! sends nyp+1-str to the right
    ! recvs str points from left at (1-str)
   end if
   !======================
   call bf_bds(ef, dt_lp, ibd)
   ! Uses lower BCs for B fields: ibd=0 for inflow-outflow
   !                             ibd=1 for symmetric
   if (comoving) then
    call field_xadvect(ef, dth, -vbeam, 1, curr_ndim, 0)
    ! Solves for half-step backward advection E field explicit
    ! E^{n} => E^{n}+v_b*Dth[Dx]E^n
   end if
   !=======================
   ! solves E^{n+1}= E^n +DT[rot B]^{n+1/2}- ompe*DT*J^{n+1/2}
   !==================
   call rotb(ef, dt_lp)
   !===================
   ! adds currents
   do ik = 1, curr_ndim
    ef(ix1:ix2, jy1:jy2, kz1:kz2, ik) = ef(ix1:ix2, jy1:jy2, kz1:kz2, &
                                           ik) - ompe*curr(ix1:ix2, jy1:jy2, kz1:kz2, ik)
   end do
   if (comoving) then
    call field_xadvect(ef, dth, -vbeam, 1, curr_ndim, 2)
    ! Solves for backward advection E field implicit
    ! E^{n+1} => E^{n}+v_b*Dth[Dx]E^{n+1}
   end if
   !============== second substep dt/2 advance of B-field
   if (prl) then
    str = 1
    stl = 2
    call fill_ebfield_yzxbdsdata(ef, 1, curr_ndim, str, stl)
   end if
   ! E field gets stl points from right (nyp+stl), (nzp+stl)
   call ef_bds(ef, dt_lp, ibd)
   ! solves B^{n+1}= B^{n+1/2} -Dth[rot E]^{n+1}
   !===================
   call rote(ef, dth)
   !==============
   if (comoving) then
    call field_xadvect(ef, dth, -vbeam, curr_ndim + 1, nfield, 2)
    ! Solves for one-half step backward advection B field implicit
    ! B^{n+1} => B^{n+1/2}+v_b*Dth[Dx]B^{n+1}
   end if
   !===============
  end subroutine
  !======================
  subroutine advance_lpf_envelope(curr, evf, omg)

   real(dp), intent(inout) :: curr(:, :, :, :), evf(:, :, :, :)
   real(dp), intent(in) :: omg
   integer :: i, j, k
   integer :: str, stl, cind, ib
   !====== enter env(3:4)=A^{n-1} and env(1:2)= A^{n}
   ! enters jc(3)=<wgh*n/gamp> >0
   str = 2
   stl = 2
   !ord=2
   cind = 1 !cind=0 FFT cind=1 grid deriv
   ib = 2 !ib=1 implicit ib=2 optimazid explicit
   !optimized advection scheme
   if (comoving) ib = 0
   call env_bds( evf, str, stl )
   ! In env_bds, envelope boundary points are set to zero
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      curr(i, j, k, 1) = -ompe*curr(i, j, k, 3)*evf(i, j, k, 1)
      curr(i, j, k, 2) = -ompe*curr(i, j, k, 3)*evf(i, j, k, 2)
     end do
    end do
   end do
   !  curr(1:2)=-ompe*chi*env(1:2)  the J_{env} source term
   !==================================
   if (ib == 0) then
    call env_lpf_solve(jc, evf, ib, omg, dt_loc)
   else
    !=================== second order in time full wave equation
    call env_maxw_solve(jc, evf, omg, dt_loc)
   end if
   ! =================================
   if (prl) then
    call fill_ebfield_yzxbdsdata(evf, 1, 2, str, stl)
   end if
  end subroutine
  !========================================================

  subroutine wave_field_left_inject(ef_in, x_left)
   real (dp), intent (inout) :: ef_in(:, :, :, :)
   real (dp), intent (in) :: x_left
   real (dp) :: tnew, xm
   integer :: wmodel_id, ic

   wmodel_id = model_id
   xm = xmn
   if (plane_wave) wmodel_id = 0
   tnew = tnow !Set inflow values [B_z{n}(i1-1/2) E_y{n}(i-1}
   lp_inject = .false.
   do ic = 1, nb_laser
    if (lp_in(ic) < x_left) then
     if (lp_end(ic) >= xm) then
      lp_inject = .true.
      if (model_id<3) call inflow_lp_fields(ef_in, lp_amp, tnew, t0_lp, &
        w0_x, w0_y, xf_loc(ic), oml, wmodel_id, ix1, jy1, jy2, kz1, kz2)
      if (model_id==3) call inflow_cp_fields(ef_in, lp_amp, tnew, t0_lp, &
        w0_x, w0_y, xf_loc(ic), wmodel_id, ix1, jy1, jy2, kz1, kz2)
     end if
    end if
    lp_in(ic) = lp_in(ic) + dt_loc
    lp_end(ic) = lp_end(ic) + dt_loc
   end do
   if (Two_color) then
    if (lp_ionz_in < x_left) then
     if (lp_ionz_end >= xm) then
      lp_inject = .true.
      call inflow_lp_fields(ef_in, lp1_amp, tnew, t1_lp, w1_x, w1_y, xf1, &
        om1, model_id, ix1, jy1, jy2, kz1, kz2)
     end if
    end if
    lp_ionz_in = lp_ionz_in + dt_loc
    lp_ionz_end = lp_ionz_end + dt_loc
   end if
  end subroutine
  !===================================================
  subroutine advect_bunch_fields(fb, curr, v_b)

   real(dp), intent(inout) :: fb(:, :, :, :), curr(:, :, :, :)
   real(dp), intent(in) :: v_b
   real(dp) :: dth
   integer :: ix, iy, iz

   dth = 0.5*dt_loc
   !In 2D nfield=3 nbfield=4=nfield+1   in fb(4)=Jx[i+1/2,j,k] at t=0
   !fb=[Ex,Ey,Ez,Jx,By,Bz]
   !================================================
   call fill_ebfield_xbdsdata(fb, 1, nbfield, 1, 1)
   if (initial_time) then
    call field_xadvect(fb, dth, -v_b, 4, 4, 1)
    fb(:, :, :, 4) = dt_loc*fb(:, :, :, 4)
   end if
   call field_xadvect(fb, dt_loc, v_b, 1, nbfield, 1)
   do iz = kz1, kz2
    do iy = jy1, jy2
     do ix = ix1, ix2
      curr(ix, iy, iz, 1) = curr(ix, iy, iz, 1) - fb(ix, iy, iz, 4)
     end do
    end do
   end do
   ! Subtracts from the longitudinal bunch current the advected initial bunch
   ! current
  end subroutine
  !============================
 end module

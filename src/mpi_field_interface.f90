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
!===================================================
!     Local grid structure under mpi domain decomposition
!==============
! grid [1:n]   np=n+2  extended domain [1:np+2]
! interior [3,np]   ghost [1:2], [np+1:np+2]
!
!
!             overlapping grid points
!====================================================================
!                                     1-----2---- 3--- 4   |      pey+1
!                 1-----2----[3-------np-1--np]--np+1--np+2|    pey
!1-2--------------np-1--np---np+1                          |pey-1
!====================  Fill  extended grid data  ======================================
!      Right(pey+1)     [3:4] ==>      pey [np+1:np+2]  right ghost data
!      Left(pey-1)      [np+1:np+2]==> pey [1:2]        left  ghost data
!===================================

 module mpi_field_interface

  use pstruct_data
  use fstruct_data
  use parallel
  use grid_param

  implicit none
  integer, parameter, private :: x_parity(6) = [-1, 1, 1, -1, 1, 1]
  integer, parameter, private :: y_parity(6) = [1, -1, 1, 1, -1, 1]
  integer, parameter, private :: z_parity(6) = [1, 1, -1, 1, 1, -1]
  integer(hp_int), parameter :: lft = -1, rgt = 1

 contains
  !===========================
  subroutine field_xyzbd(ef, nc)
   real(dp), intent(inout) :: ef(:, :, :, :)
   integer, intent(in) :: nc
   integer :: ik, iy, ix, iz, str, stl
   integer :: i1, i2, j1, j2, k1, k2
   !==========================
   ! enter fields(1:n1,j01-1:j02+1,k01-1:k02+1,nc)
   ! Only for NON-PERIODIC boundaries
   str = 1
   stl = 1

   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2
   i1 = ix1
   i2 = ix2

   if (pex0) then
    if (ibx < 2) then
     do ik = 1, nc
      do iz = k1, k2
       do iy = j1, j2
        ef(ix1 - 1, iy, iz, ik) = 2.*ef(ix1, iy, iz, ik) - &
                                  ef(ix1 + 1, iy, iz, ik)
       end do
      end do
     end do
    end if
    i1 = i1 - 1
   end if
   if (pex1) then
    if (ibx == 0) then
     do ik = 1, nc
      do iz = k1, k2
       do iy = j1, j2
        ef(ix2 + 1, iy, iz, ik) = 2.*ef(ix2, iy, iz, ik) - &
                                  ef(ix2 - 1, iy, iz, ik)
       end do
      end do
     end do
    end if
    if (ibx == 1) then
     do ik = 1, nc
      do iz = k1, k2
       do iy = j1, j2
        ef(ix2 + 1, iy, iz, ik) = x_parity(ik)*ef(ix2, iy, iz, ik)
       end do
      end do
     end do
    end if
    i2 = i2 + 1
   end if
   if (ndim < 2) return

   if (yl_bd) then
    if (iby == 0) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, i2
        ef(ix, jy1 - 1, iz, ik) = 2.*ef(ix, jy1, iz, ik) - &
                                  ef(ix, jy1 + 1, iz, ik)
       end do
      end do
     end do
    end if
    if (iby == 1) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, i2
        ef(ix, jy1 - 1, iz, ik) = y_parity(ik)*ef(ix, jy1 + 1, iz, ik)
       end do
      end do
     end do
    end if
    j1 = j1 - 1
   end if
   if (yr_bd) then
    if (iby == 0) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, i2
        ef(ix, jy2 + 1, iz, ik) = 2.*ef(ix, jy2, iz, ik) - &
                                  ef(ix, jy2 - 1, iz, ik)
       end do
      end do
     end do
    end if
    if (iby == 1) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, i2
        ef(ix, jy2 + 1, iz, ik) = y_parity(ik)*ef(ix, jy2 - 1, iz, ik)
       end do
      end do
     end do
    end if
    j2 = j2 + 1
   end if
   if (ndim < 3) return

   if (zl_bd) then
    if (ibz == 0) then
     do ik = 1, nc
      do iy = j1, j2
       do ix = i1, i2
        ef(ix, iy, kz1 - 1, ik) = 2*ef(ix, iy, kz1, ik) - &
                                  ef(ix, iy, kz1 + 1, ik)
       end do
      end do
     end do
    end if
    if (ibz == 1) then
     do ik = 1, nc
      do iy = j1, j2
       do ix = i1, i2
        ef(ix, iy, kz1 - 1, ik) = z_parity(ik)*ef(ix, iy, kz1 + 1, ik)
       end do
      end do
     end do
    end if
    k1 = k1 - 1
   end if
   if (zr_bd) then
    if (ibz == 0) then
     do ik = 1, nc
      do iy = j1, j2
       do ix = i1, i2
        ef(ix, iy, kz2 + 1, ik) = 2*ef(ix, iy, kz2, ik) - &
                                  ef(ix, iy, kz2 - 1, ik)
       end do
      end do
     end do
    end if
    if (ibz == 1) then
     do ik = 1, nc
      do iy = j1, j2
       do ix = i1, i2
        ef(ix, iy, kz1 + 1, ik) = z_parity(ik)*ef(ix, iy, kz1 - 1, ik)
       end do
      end do
     end do
    end if
    k2 = k2 + 1
   end if
  end subroutine
  !========================================
  subroutine fluid_left_xshift(fld, den_x, den_yz, i1, i2, ic1, ic2, &
                               xsh)

   integer, intent(in) :: i1, i2, ic1, ic2, xsh
   real(dp), intent(inout) :: fld(:, :, :, :)
   real(dp), intent(in) :: den_x(:), den_yz(:, :)
   integer :: n1, ic, ii, ix, j, iy, iz, kk, lenws
   integer :: j1, j2, k1, k2

   if (xsh == 0) return

   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2
   n1 = loc_xgrid(imodx)%ng

   lenws = (ic2 + 1 - ic1)*(k2 + 1 - k1)*(j2 + 1 - j1)*xsh
   if (prlx) then
    !Sends to x-left side xsh data (i1:i1+xsh-1)
    !Recvs from x-right data(i2+1:i2+xsh)       i2=n1p-xsh
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 0, xsh - 1
        ix = i1 + j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    call exchange_bdx_data(aux1, aux2, lenws, lenws, 3, rgt)
   end if
   !
   ! shifts (i1+xsx:i2+xsh=n1p)=>(i1:i2)
   do ic = ic1, ic2
    do iz = k1, k2
     do iy = j1, j2
      do ix = i1, i2
       fld(ix, iy, iz, ic) = fld(ix + xsh, iy, iz, ic)
      end do
     end do
    end do
   end do
   if (xl_bd) then
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       fld(i1 - 1, iy, iz, ic) = fld(i1 + xsh - 1, iy, iz, ic)
       fld(i1, iy, iz, ic) = 0.5*fld(i1, iy, iz, ic) + &
                             0.25*(fld(i1 + 1, iy, iz, ic) + fld(i1 - 1, iy, iz, ic))
      end do
     end do
    end do
   endif
   if (pex1) then
    do ic = ic1, ic2 - 1
     do iz = k1, k2
      do iy = j1, j2
       do ix = i2 + 1, i2 + xsh
        fld(ix, iy, iz, ic) = zero_dp
       end do
      end do
     end do
    end do
   endif
   ! now replaces (i2+1:i2+xsh=n1p)
   if (prlx) then
    if (pex1) aux2(1:lenws) = 0.0
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 1, xsh
        ix = i2 + j
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if
   if (pex1) then
    ic = ic2
    do iz = k1, k2
     do iy = j1, j2
      do ix = i2 + 1, i2 + xsh
       ii = ix + imodx*n1
       fld(ix, iy, iz, ic) = den_x(ii)*den_yz(iy, iz)
      end do
     end do
    end do
   endif

  end subroutine
  !==================================
  subroutine fields_left_xshift(fld, i1, i2, ic1, ic2, xsh)

   integer, intent(in) :: i1, i2, ic1, ic2, xsh
   real(dp) :: fld(:, :, :, :)
   integer :: ic, ix, j, iy, iz, kk, lenws
   integer :: j1, j2, k1, k2

   if (xsh == 0) return
   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2
   lenws = (ic2 + 1 - ic1)*(k2 + 1 - k1)*(j2 + 1 - j1)*xsh
   if (prlx) then
    !Sends to x-left side xsh data (i1:i1+xsh-1)
    !Recvs from x-right data(i2+1:i2+xsh)       i2=n1p-sh
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 0, xsh - 1
        ix = i1 + j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    call exchange_bdx_data(aux1, aux2, lenws, lenws, 3, rgt)
   end if
   !
   ! shifts (i1+xsh:i2+xsh=n1p)=>(i1:i2)
   do ic = ic1, ic2
    do iz = k1, k2
     do iy = j1, j2
      do ix = i1, i2
       fld(ix, iy, iz, ic) = fld(ix + xsh, iy, iz, ic)
      end do
      fld(i2 + 1:i2 + xsh, iy, iz, ic) = 0.0
     end do
    end do
   end do
   ! now replaces (i2+1:i2+xsh=n1p)
   if (prlx) then
    if (pex1) aux2(1:lenws) = 0.0
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 1, xsh
        ix = i2 + j
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if

  end subroutine
  !=================================
  subroutine fill_ebfield_yzxbdsdata(fld, ic1, ic2, str, stl)

   real(dp) :: fld(:, :, :, :)
   integer, intent(in) :: ic1, ic2, str, stl
   integer :: ic, ix, j, iy, iz, kk, iy1, iy2, iz1, iz2, lenws, lenwr
   integer :: i1, i2, j1, j2, k1, k2

   i1 = ix1
   i2 = ix2
   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2

   iy1 = j1
   iy2 = j2
   iz1 = k1
   iz2 = k2

   ! WARNING str <3 , stl>2 allowed
   !=======================
   ! Extends data to the y-left
   ! sends to the right iy=[j2:j2-str+1]
   !recvs from the left iy=[j1-1:j1-str] sign=-1
   !============================================
   if (prly) then
    if (str > 0) then
     kk = 0
     do ic = ic1, ic2
      do iz = k1, k2
       do j = 0, str - 1
        iy = j2 - j
        do ix = i1, i2
         kk = kk + 1
         aux1(kk) = fld(ix, iy, iz, ic)
        end do
       end do
      end do
     end do
     lenws = kk
     lenwr = lenws
     call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, lft)
     if (yl_bd) then
      if (iby < 2) then
       aux2(1:lenwr) = 0.0
      end if
     end if
     kk = 0
     do ic = ic1, ic2
      do iz = k1, k2
       do j = 1, str
        iy = j1 - j
        do ix = i1, i2
         kk = kk + 1
         fld(ix, iy, iz, ic) = aux2(kk)
        end do
       end do
      end do
     end do
     iy1 = j1 - str
    end if
    !=========================
    if (stl > 0) then
     !=======================
     ! Extends data to the y-right, stl>2 allowed
     !Sends to the left stl data[j1:j1+stl-1]
     !Recvs from right [j2+1:j2+stl] f_data sign=+1

     kk = 0
     do ic = ic1, ic2
      do iz = k1, k2
       do j = 0, stl - 1
        iy = j1 + j
        do ix = i1, i2
         kk = kk + 1
         aux1(kk) = fld(ix, iy, iz, ic)
        end do
       end do
      end do
     end do
     lenws = kk
     lenwr = lenws

     !======================= next indx=1 cart dim=1 sign=+1
     call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, rgt)
     if (yr_bd) then
      if (iby < 2) then
       aux2(1:lenwr) = 0.0
      end if
     end if
     kk = 0
     do ic = ic1, ic2
      do iz = k1, k2
       do j = 1, stl
        iy = j + j2
        do ix = i1, i2
         kk = kk + 1
         fld(ix, iy, iz, ic) = aux2(kk)
        end do
       end do
      end do
     end do
     iy2 = j2 + stl
    end if
   end if
   !========================
   if (prlz) then
    !=======================
    ! Extends data to the z-left
    ! sends to the right iz=[k2:k2-str+1]
    !recvs from the left iz=[k1-1:k1-str] sign=-1
    !============================================

    if (str > 0) then
     kk = 0
     do ic = ic1, ic2
      do j = 0, str - 1
       iz = k2 - j
       do iy = iy1, iy2
        do ix = i1, i2
         kk = kk + 1
         aux1(kk) = fld(ix, iy, iz, ic)
        end do
       end do
      end do
     end do
     lenws = kk
     lenwr = lenws
     call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, lft)
     if (zl_bd) then
      if (ibz < 2) then
       aux2(1:lenwr) = 0.0
      end if
     end if
     kk = 0
     do ic = ic1, ic2
      do j = 1, str
       iz = k1 - j
       do iy = iy1, iy2
        do ix = i1, i2
         kk = kk + 1
         fld(ix, iy, iz, ic) = aux2(kk)
        end do
       end do
      end do
     end do
     iz1 = k1 - str
    end if
    if (stl > 0) then
     ! Extends data to the z-right
     !Sends to the left stl data[k1:k1+stl-1]
     !Recvs from right [k2+1:k2+stl] f_data sign=+1
     kk = 0
     do ic = ic1, ic2
      do j = 0, stl - 1
       iz = k1 + j
       do iy = iy1, iy2
        do ix = i1, i2
         kk = kk + 1
         aux1(kk) = fld(ix, iy, iz, ic)
        end do
       end do
      end do
     end do
     lenws = kk
     lenwr = lenws
     call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, rgt)
     if (zr_bd) then
      if (ibz < 2) then
       aux2(1:lenwr) = 0.0
      end if
     end if
     kk = 0
     do ic = ic1, ic2
      do j = 1, stl
       iz = k2 + j
       do iy = iy1, iy2
        do ix = i1, i2
         kk = kk + 1
         fld(ix, iy, iz, ic) = aux2(kk)
        end do
       end do
      end do
     end do
     iz2 = k2 + stl
    end if
   end if
   !===============================
   if (.not. prlx) then
    if (ibx == 2) then
     if (str > 0) then
      do ic = ic1, ic2
       do iz = iz1, iz2
        do iy = iy1, iy2
         do j = 1, str
          fld(i1 - j, iy, iz, ic) = fld(i2 + 1 - j, iy, iz, ic)
         end do
        end do
       end do
      end do
     end if
     if (stl > 0) then
      do ic = ic1, ic2
       do iz = iz1, iz2
        do iy = iy1, iy2
         do j = 1, stl
          fld(i2 + j, iy, iz, ic) = fld(i1 - 1 + j, iy, iz, ic)
         end do
        end do
       end do
      end do
     end if
    end if
    return
   end if
   !====================================
   ! Extends data to the x-left
   ! sends to the right ix=[i2:i2-str+1]
   !recvs from the left ix=[i1-1:i1-str] sign=-1
   !============================================
   if (str > 0) then
    kk = 0
    do ic = ic1, ic2
     do iz = iz1, iz2
      do iy = iy1, iy2
       do j = 0, str - 1
        ix = i2 - j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, lft)
    if (xl_bd) then
     if (ibx < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = ic1, ic2
     do iz = iz1, iz2
      do iy = iy1, iy2
       do j = 1, str
        ix = i1 - j
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if
   !=======================
   if (stl > 0) then
    !=======================
    ! Extends data to the x-right
    !Sends to the left stl data[i1:i1+stl-1]
    !Recvs from right [i2+1:i2+stl] f_data sign=+1

    kk = 0
    do ic = ic1, ic2
     do iz = iz1, iz2
      do iy = iy1, iy2
       do j = 0, stl - 1
        ix = i1 + j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws

    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, rgt)
    if (pex1) then
     if (ibx < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = ic1, ic2
     do iz = iz1, iz2
      do iy = iy1, iy2
       do j = 1, stl
        ix = j + i2
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if
   ! end data transfer
   !===========================
  end subroutine
  !===============================
  subroutine fill_ebfield_xbdsdata(fld, ic1, ic2, str, stl)
   real(dp) :: fld(:, :, :, :)
   integer, intent(in) :: ic1, ic2, str, stl
   integer :: ic, ix, j, iy, iz, kk, lenws, lenwr
   integer :: i1, i2, j1, j2, k1, k2
   !============================
   i1 = ix1
   i2 = ix2
   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2

   if (.not. prlx) then
    if (ibx == 2) then
     if (str > 0) then
      do ic = ic1, ic2
       do iz = k1, k2
        do iy = j1, j2
         do j = 1, str
          fld(i1 - j, iy, iz, ic) = fld(i2 + 1 - j, iy, iz, ic)
         end do
        end do
       end do
      end do
     end if
     if (stl > 0) then
      do ic = ic1, ic2
       do iz = k1, k2
        do iy = j1, j2
         do j = 1, stl
          fld(i2 + j, iy, iz, ic) = fld(i1 - 1 + j, iy, iz, ic)
         end do
        end do
       end do
      end do
     end if
    end if
    return
   end if
   !====================================
   if (str > 0) then
    !Sends to x-right str data(i2+1-str)
    !Recvs from the x-left str data(i1-str) sign=-1
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 0, str - 1
        ix = i2 - j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, lft)
    if (pex0) then
     if (ibx < 2) aux2(1:lenws) = 0.0
    end if
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 1, str
        ix = i1 - j
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if
   if (stl > 0) then
    !Sends to left data(ii1:i1+stl-1)
    !Recvs from the right str data(i2:i2+stl)
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 0, stl - 1
        ix = i1 + j
        kk = kk + 1
        aux1(kk) = fld(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, rgt)
    if (pex1) then
     if (ibx < 2) aux2(1:lenws) = 0.0
    end if
    kk = 0
    do ic = ic1, ic2
     do iz = k1, k2
      do iy = j1, j2
       do j = 1, stl
        ix = i2 + j
        kk = kk + 1
        fld(ix, iy, iz, ic) = aux2(kk)
       end do
      end do
     end do
    end do
   end if
   ! end data transfer
   !===========================
  end subroutine
  !=====================
 end module

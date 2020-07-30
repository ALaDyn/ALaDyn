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
! grid [1:n]   np=n+2  extended domain [1:np+3]
! interior [3,np]   ghost [1:2], [np+1:np+3]
!
!             overlapping grid points
!====================================================================
!                                     1-----2---- 3---  4---- 5   |      pey+1
!                 1-----2----[3-------np-1--np]--np+1--np+2--np+3 |    pey
!1-2--------------np-1--np---np+1                          |pey-1
!----------------------------------------------------------------------
!  On current pey mpi-task
!  data [1,2] recieved form right are added to [np-1,np] data
!  data [np+1:np+3] recieved from right are added to [3:5] data
!===================================

 module mpi_curr_interface

  use pstruct_data
  use fstruct_data
  use parallel
  use grid_param

  implicit none

  integer(hp_int), parameter, private :: rt = 1, lt = -1

 contains
  !====================
  subroutine fill_curr_yzxbdsdata(curr, nc)
   real(dp), intent(inout) :: curr(:, :, :, :)
   integer, intent(in) :: nc
   integer :: s1, s2, r1, r2, y1, y2, z1, z2, x1, x2
   integer :: ic, ix, j, iy, iz, kk, lenws, lenwr
   integer, parameter :: str = 3, stl = 2
   !================ PREDEFINED MAX
   ! enter currents on a five-point extended stencil
   !===========================
   z1 = kz1 - stl
   z2 = kz2 + str
   y1 = jy1 - stl
   y2 = jy2 + str
   if (ndim < 3) then
    z1 = kz1
    z2 = kz2
   end if
   if (ndim < 2) then
    y1 = jy1
    y2 = jy2
   end if
   x1 = ix1 - stl
   x2 = ix2 + str

   lenwr = str*nc*(x2 + 1 - x1)*max(z2 + 1 - z1, y2 + 1 - y1)
   if (size(aux1) < lenwr) then
    deallocate (aux1, aux2)
    allocate (aux1(lenwr))
    allocate (aux2(lenwr))
   end if
   if (prly) then
    !=====================
    ! for stl=2
    ! sends y=[j1-2;j1-1] stl data to left
    !receives from right and adds data on y=[nyc-1:nyc] sign=+1
    !=========================
    s1 = jy1 - stl
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do j = 0, stl - 1
       iy = s1 + j
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, rt)
    r1 = jy2 - stl
    kk = 0
    if (pe1y) then
     if (iby < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do j = 1, stl
       iy = j + r1
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
!========================================
    ! sends y=[nyc+1:nyc+str] str=3 data to the right
    !receives from left and adds data on y=[j1:j1+str-1] sign=-1
    s2 = jy2
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do j = 1, str
       iy = j + s2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, lt)
    !=====================
    r2 = jy1
    kk = 0
    if (pe0y) then
     if (iby < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do j = 0, str - 1
       iy = j + r2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
   end if
   ! The reduced stencil of summed data
   y1 = jy1
   y2 = jy2
   !================
   if (prlz) then
    !================
    ! sends z=[k1-2;k1-1] stl data to left
    ! receives from right and adds data on y=[nzc-1:nzc] sign=+1

    s1 = kz1 - stl
    kk = 0
    do ic = 1, nc
     do j = 0, stl - 1
      iz = s1 + j
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, rt)

    r1 = kz2 - stl
    if (pe1z) then
     if (ibz < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = 1, nc
     do j = 1, stl
      iz = j + r1
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    !
    !================
    ! sends z=[nzc+1:nzc+str] str=3 data to the right
    !receives from left and adds data on z=[k1:k1+str-1] sign=-1
    s2 = kz2
    kk = 0
    do ic = 1, nc
     do j = 1, str
      iz = j + s2
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, lt)
    !================
    r2 = kz1
    if (pe0z) then
     if (ibz < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = 1, nc
     do j = 0, str - 1
      iz = j + r2
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    ! The reduced stencil of summed data
    z1 = kz1
    z2 = kz2
   end if
   !========================== prlx case
   if (prlx) then
    ! sends x=[i1-2;i1-1] stl data to left
    ! receives from right and adds data on x=[nxc-1:nxc] sign=+1
    s1 = ix1 - stl
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do iy = y1, y2
       do j = 0, stl - 1
        ix = s1 + j
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, rt)
    !=====================
    r1 = ix2 - stl
    kk = 0
    if (pex1) then
     if (ibx < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do iy = y1, y2
       do j = 1, stl
        ix = j + r1
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    ! sends x=[nxc+1:nxc+str] str=3 data to the right
    !receives from left and adds data on x=[i1:i1+str-1] sign=-1
    s2 = ix2
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do iy = y1, y2
       do j = 1, str
        ix = j + s2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 3, lt)
    !=====================
    r2 = ix1
    kk = 0
    if (pex0) then
     if (ibx < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do iy = y1, y2
       do j = 0, str - 1
        ix = j + r2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    return
   end if
   !===================== No MPI x-decomposition
   if (ibx == 2) then
    ! data nxc-1:nxc sums to i1-2:i1-1
    ! data i1:i1+2 sums to nxc+1:nxc+3
    s1 = ix1 - stl - 1
    r1 = ix2 - stl
    do ic = 1, nc
     do iz = z1, z2
      do iy = y1, y2
       do j = 1, stl
        curr(r1 + j, iy, iz, ic) = curr(r1 + j, iy, iz, ic) + &
                                   curr(s1 + j, iy, iz, ic)
       end do
       do j = 1, str
        ix = ix1 - 1 + j
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + &
                               curr(r1 + j, iy, iz, ic)
       end do
      end do
     end do
    end do
   end if
   !=============================
  end subroutine
  !===============================
  subroutine fill_ftcurr_yzbdsdata(curr, nc)
   real(dp), intent(inout) :: curr(:, :, :, :)
   integer, intent(in) :: nc
   integer :: s1, s2, r1, r2, y1, y2, z1, z2, x1, x2
   integer :: ic, ix, j, iy, iz, kk, lenws, lenwr
   integer :: j1, j2, k1, k2
   integer, parameter :: str = 3, stl = 2
   !================ PREDEFINED MAX
   ! enter currents on a five-point extended stencil
   !===========================
   k1 = loc_zftgrid(imodz)%p_ind(1)
   k2 = loc_zftgrid(imodz)%p_ind(2)

   j1 = loc_yftgrid(imody)%p_ind(1)
   j2 = loc_yftgrid(imody)%p_ind(2)

   z1 = k1 - stl
   z2 = k2 + str
   y1 = j1 - stl
   y2 = j2 + str
   if (ndim < 3) then
    z1 = k1
    z2 = k2
   end if
   x1 = ix1 - stl
   x2 = ix2 + str

   lenwr = str*nc*(x2 + 1 - x1)*max(z2 + 1 - z1, y2 + 1 - y1)
   if (size(aux1) < lenwr) then
    deallocate (aux1, aux2)
    allocate (aux1(lenwr))
    allocate (aux2(lenwr))
   end if
   if (prly) then
    !=====================
    ! for stl=2
    ! sends y=[j1-2;j1-1] stl data to left
    !receives from right and adds data on y=[nyc-1:nyc] sign=+1
    !=========================
    s1 = j1 - stl
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do j = 0, stl - 1
       iy = s1 + j
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, rt)
    r1 = j2 - stl
    kk = 0
    if (pe1y) then
     if (iby < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do j = 1, stl
       iy = j + r1
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
!========================================
    ! sends y=[nyc+1:nyc+str] str=3 data to the right
    !receives from left and adds data on y=[j1:j1+str-1] sign=-1
    s2 = j2
    kk = 0
    do ic = 1, nc
     do iz = z1, z2
      do j = 1, str
       iy = j + s2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 1, lt)
    !=====================
    r2 = j1
    kk = 0
    if (pe0y) then
     if (iby < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    do ic = 1, nc
     do iz = z1, z2
      do j = 0, str - 1
       iy = j + r2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
   end if
   ! The reduced stencil of summed data
   y1 = j1
   y2 = j2
   !================
   if (prlz) then
    !================
    ! sends z=[k1-2;k1-1] stl data to left
    ! receives from right and adds data on y=[nzc-1:nzc] sign=+1

    s1 = k1 - stl
    kk = 0
    do ic = 1, nc
     do j = 0, stl - 1
      iz = s1 + j
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, rt)

    r1 = k2 - stl
    if (pe1z) then
     if (ibz < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = 1, nc
     do j = 1, stl
      iz = j + r1
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    !
    !================
    ! sends z=[nzc+1:nzc+str] str=3 data to the right
    !receives from left and adds data on z=[k1:k1+str-1] sign=-1
    s2 = k2
    kk = 0
    do ic = 1, nc
     do j = 1, str
      iz = j + s2
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        aux1(kk) = curr(ix, iy, iz, ic)
       end do
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws
    call exchange_bdx_data(aux1, aux2, lenws, lenwr, 2, lt)
    !================
    r2 = k1
    if (pe0z) then
     if (ibz < 2) then
      aux2(1:lenwr) = 0.0
     end if
    end if
    kk = 0
    do ic = 1, nc
     do j = 0, str - 1
      iz = j + r2
      do iy = y1, y2
       do ix = x1, x2
        kk = kk + 1
        curr(ix, iy, iz, ic) = curr(ix, iy, iz, ic) + aux2(kk)
       end do
      end do
     end do
    end do
    ! The reduced stencil of summed data
    z1 = k1
    z2 = k2
   end if
  end subroutine
!=====================================
  subroutine jc_xyzbd(curr, nc)
   real(dp), intent(inout) :: curr(:, :, :, :)
   integer, intent(in) :: nc
   integer :: ix, iy, iz, i0, j2, k2, ik
   integer :: i1, n1, j1, n2, k1, n3
   ! Enter current data on extended ranges:
   !========== Only for Periodic BDs period=n1-1
   i1 = ix1
   n1 = ix2
   j1 = jy1
   n2 = jy2
   k1 = kz1
   n3 = kz2
   j2 = n2
   k2 = n3
   if (ibx == 0) then
    do ik = 1, nc
     curr(i1, j1:j2, k1:k2, ik) = curr(i1, j1:j2, k1:k2, ik) + &
                                  curr(i1 - 1, j1:j2, k1:k2, ik)
     curr(i1 - 1, j1:j2, k1:k2, ik) = 0.0

     curr(n1, j1:j2, k1:k2, ik) = curr(n1, j1:j2, k1:k2, ik) + &
                                  curr(n1 + 1, j1:j2, k1:k2, ik) + curr(n1 + 2, j1:j2, k1:k2, ik)
     curr(n1 + 1:n1 + 2, j1:j2, k1:k2, ik) = 0.0
    end do
    i0 = 1
   end if
   if (ndim < 2) return
   if (pe0y) then
    if (iby == 0) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, n1
        curr(ix, j1, iz, ik) = curr(ix, j1, iz, ik) + &
                               curr(ix, j1 - 1, iz, ik) + curr(ix, j1 - 2, iz, ik)
        curr(ix, j1 - 2:j1 - 1, iz, ik) = 0.0
       end do
      end do
     end do
    end if
    if (iby == 1) then
     !Before norm  r*Jx(j)=rVxrho ==> rEx odd Jx(j-1)=-Jx(j+1)
     !             r*Jr=rVrrho ==> rEr even   Jr(j-1/2)=jr(j+1/2)
     do iz = k1, k2
      do ix = i1, n1
       curr(ix, j1 + 1, iz, 1) = curr(ix, j1 + 1, iz, 1) + &
                                 curr(ix, j1 - 1, iz, 1)
       curr(ix, j1, iz, 2) = curr(ix, j1, iz, 2) - curr(ix, j1 - 1, iz, 2)
      end do
     end do
    end if
   end if
   if (pe1y) then
    if (iby == 0) then
     do ik = 1, nc
      do iz = k1, k2
       do ix = i1, n1
        curr(ix, n2, iz, ik) = curr(ix, n2, iz, ik) + &
                               curr(ix, n2 + 1, iz, ik) + curr(ix, n2 + 2, iz, ik)
        curr(ix, n2 + 1:n2 + 2, iz, ik) = 0.0
       end do
      end do
     end do
    end if
   end if
   if (ndim < 3) return
   if (ibz == 0) then
    if (pe0z) then
     do ik = 1, nc
      do iy = j1, j2
       do ix = i1, n1
        curr(ix, iy, k1, ik) = curr(ix, iy, k1, ik) + &
                               curr(ix, iy, k1 - 1, ik)
        curr(ix, iy, k1 - 1, ik) = 0.0
       end do
      end do
     end do
    end if
    if (pe1z) then
     if (ibz == 0) then
      do ik = 1, nc
       do iy = j1, j2
        do ix = i1, n1
         curr(ix, iy, n3, ik) = curr(ix, iy, n3, ik) + &
                                curr(ix, iy, n3 + 1, ik) + curr(ix, iy, n3 + 2, ik)
         curr(ix, iy, n3 + 1:n3 + 2, ik) = 0.0
        end do
       end do
      end do
     end if
    end if
   end if
  end subroutine
  !=========================
  !
  subroutine den_zyxbd(rho, ik)
   real(dp), intent(inout) :: rho(:, :, :, :)
   integer, intent(in) :: ik
   integer :: i1, i2, j1, j2, k1, k2
   integer :: ix, iy
   ! Enter current data on extended ranges:

   !Enter data on the computational box [i1:n1p][j1:nyp][k1:nzp]

   i1 = ix1
   i2 = ix2
   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2
   if (ndim > 2) then
    if (ibz == 0) then
     if (pe0z) then
      do iy = j1, j2
       do ix = i1, i2
        rho(ix, iy, k1 + 1, ik) = rho(ix, iy, k1 + 1, ik) + &
                                  rho(ix, iy, k1 - 1, ik)
        rho(ix, iy, k1, ik) = rho(ix, iy, k1 + 1, ik)
       end do
      end do
     end if
     if (pe1z) then
      if (ibz < 2) then
       do iy = j1, j2
        do ix = i1, i2
         rho(ix, iy, k2 - 1, ik) = rho(ix, iy, k2 - 1, ik) + &
                                   rho(ix, iy, k2 + 1, ik)
         rho(ix, iy, k2, ik) = rho(ix, iy, k2 - 1, ik)
        end do
       end do
      end if
     end if
    end if
   end if
   !================
   if (ndim > 1) then
    if (pe0y) then
     if (iby == 0) then
      rho(i1:i2, j1 + 1, k1:k2, ik) = rho(i1:i2, j1 + 1, k1:k2, ik) + &
                                      rho(i1:i2, j1 - 1, k1:k2, ik)
      rho(i1:i2, j1, k1:k2, ik) = rho(i1:i2, j1 + 1, k1:k2, ik)
     end if
     if (iby == 1) then
      rho(i1:i2, j1 + 1, k1:k2, ik) = rho(i1:i2, j1 + 1, k1:k2, ik) + &
                                      rho(i1:i2, j1 - 1, k1:k2, ik)
      rho(i1:i2, j1 - 1, k1:k2, ik) = 0.0
     end if
    end if
    if (pe1y) then
     if (iby < 2) then
      rho(i1:i2, j2 - 1, k1:k2, ik) = rho(i1:i2, j2 - 1, k1:k2, ik) + &
                                      rho(i1:i2, j2 + 1, k1:k2, ik)
      rho(i1:i2, j2, k1:k2, ik) = rho(i1:i2, j2 - 1, k1:k2, ik)
     end if
    end if
   end if
   !============== field data on [y_loc]
   if (ibx < 2) then
    if (pex0) then
     rho(i1 + 1, j1:j2, k1:k2, ik) = rho(i1 + 1, j1:j2, k1:k2, ik) + &
                                     rho(i1 - 1, j1:j2, k1:k2, ik)
     rho(i1, j1:j2, k1:k2, ik) = rho(i1 + 1, j1:j2, k1:k2, ik)
    end if
    if (pex1) then
     rho(i2 - 1, j1:j2, k1:k2, ik) = rho(i2 - 1, j1:j2, k1:k2, ik) + &
                                     rho(i2 + 1, j1:j2, k1:k2, ik)
     rho(i2, j1:j2, k1:k2, ik) = rho(i2 - 1, j1:j2, k1:k2, ik)
    end if
   end if
  end subroutine
 end module

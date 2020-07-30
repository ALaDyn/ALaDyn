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

 module prl_fft
  use parallel
  use modern_fft_lib
  implicit none

  real(dp), allocatable :: fp1(:, :, :), fp2(:, :, :), faux1(:), &
                           faux2(:)
  integer, allocatable :: loc_yft_ord(:), loc_zft_ord(:)

 contains
  subroutine mpi_ftw_alloc(n1, n2, n2_loc, n3, n3_loc)
   integer, intent(in) :: n1, n2, n2_loc, n3, n3_loc
   integer :: n1_xloc, n2_xloc, lenw
   !=======================
   ! loc_grid allready define

   n1_xloc = n1/npe_yloc
   n2_xloc = n1/npe_zloc
   lenw = n1_xloc*n2_loc*n3_loc
   allocate (faux1(lenw), faux2(lenw))
   allocate (fp1(n1_xloc, n2, n3_loc))
   allocate (fp2(n2_xloc, n2_loc, n3))

  end subroutine
  !==========================
  subroutine mpi_yzft_ord(ny_ft, nz_ft)
   integer, intent(in) :: ny_ft, nz_ft
   integer :: ip, nh

   allocate (loc_yft_ord(0:npe_yloc - 1))
   allocate (loc_zft_ord(0:npe_zloc - 1))
   nh = npe_yloc/2 - 1
   if (npe_yloc > 4) then
    do ip = 0, nh              ![ x  |   |   |   ]
     loc_yft_ord(ip) = 1
    end do
    do ip = nh + 1, nh + 3
     loc_yft_ord(ip) = loc_yft_ord(ip - 1) + ny_ft
    end do
    ![   | x  |   |   ]   ![   |   | x  |   ] ![   |   |   | x  ]
    do ip = nh + 4, npe_yloc - 1
     loc_yft_ord(ip) = loc_yft_ord(nh + 3)
    end do
   endif
   loc_zft_ord(0:npe_zloc - 1) = 1
   if (npe_zloc > 4) then
    do ip = 0, npe_zloc/2 - 1            ![ x  |   |   |   ]
     loc_zft_ord(ip) = 1
    end do
    do ip = npe_zloc/2, npe_zloc/2 + 2
     loc_zft_ord(ip) = loc_zft_ord(ip - 1) + nz_ft
    end do
    ![   | x  |   |   ]   ![   |   | x  |   ] ![   |   |   | x  ]
    do ip = npe_zloc/2 + 3, npe_zloc - 1
     loc_zft_ord(ip) = loc_zft_ord(npe_zloc/2 + 2)
    end do
   endif
  end subroutine

  subroutine mpi_ftw_dalloc

   if (allocated(faux1)) deallocate (faux1)
   if (allocated(faux2)) deallocate (faux2)
   if (allocated(fp1)) deallocate (fp1)
   if (allocated(fp2)) deallocate (fp2)

  end subroutine

  !===================
  subroutine ft_overset_grid(w_s, w_r, nft1, nft2, nft3)
   real(dp), intent(inout) :: w_s(:, :, :)
   real(dp), intent(out) :: w_r(:, :, :)
   integer, intent(in) :: nft1, nft2, nft3
   integer :: lenw, dd, nhy, nhz
   integer :: j1, j2, k1, k2, jl, jr, kl, kr
   integer :: ybd, zbd, kkr, jjr
   integer ::  tag, ip, per, pes
   !=========== gather on w_r data along y-coordinate using 3 neighbors
   ! imody(+1,+2,+3)-> imody for imody < npe_yloc/2-1
   ! imody(-1,+1,+2)-> imody for imody= npe_yloc/2-1
   ! imody(-2,-1,+1)-> imody for imody= npe_yloc/2
   ! imody(-3,-2,-1)-> imody for imody> npe_yloc/2+1
   !======================================================
   lenw = nft1*nft2*nft3
   dd = 1
   nhy = npe_yloc/2 - 1
   nhz = npe_zloc/2 - 1
   jl = min(imody, 3)
   jr = min(nhy + 3 - imody, 3)
   ybd = min(3, npe_yloc - 1 - imody)
   k1 = loc_zft_ord(imodz)
   k2 = k1 + nft3 - 1
   j1 = loc_yft_ord(imody)
   j2 = j1 + nft2 - 1
   w_r(1:nft1, j1:j2, k1:k2) = w_s(1:nft1, 1:nft2, 1:nft3)
!====================
   !imody=[1,nh+3] sends to ipe=imody-1,imody-2,imody-3  => ipe=[0,nh]
   !
!===================
   !sends to next(ip)  recieves from prev(ip)
   if (imody < nhy + 4) then
    if (imody > 0) then
     do ip = 1, jl
      tag = 200 + ip
      pes = yp_prev(ip)
      call mpi_send(w_s(1, 1, 1), lenw, mpi_sd, pes, tag, comm_col(dd), error)
     end do
    endif
    j1 = loc_yft_ord(imody)
    do ip = 1, jr
     per = yp_next(ip)
     tag = 200 + ip
     call mpi_recv(fp1(1, 1, 1), lenw, &
                   mpi_sd, per, tag, comm_col(dd), status, error)
     j1 = j1 + nft2
     j2 = j1 + nft2 - 1
     w_r(1:nft1, j1:j2, k1:k2) = fp1(1:nft1, 1:nft2, 1:nft3)
    end do
   endif
   !sends imody=nh to (nh+1,nh+2,nh+3) up to
   !imody=npe_yloc-3 to npe_yloc-2,npe_yloc-1) ybd=2
   !imody=npe_yloc-2 to npe_yloc-1)            ybd=1
   if (imody > (nhy - 1)) then
    j1 = loc_yft_ord(imody)
    jjr = imody - nhy
    jjr = min(jjr, 3)
    do ip = 1, ybd
     tag = 200 - ip
     pes = yp_next(ip)
     call mpi_send(w_s(1, 1, 1), lenw, mpi_sd, pes, tag, comm_col(dd), error)
    end do
    j1 = loc_yft_ord(imody)
    do ip = 1, jjr
     tag = 200 - ip
     per = yp_prev(ip)
     call mpi_recv(fp1(1, 1, 1), lenw, &
                   mpi_sd, per, tag, comm_col(dd), status, error)
     j1 = j1 - nft2
     j2 = j1 + nft2 - 1
     w_r(1:nft1, j1:j2, k1:k2) = fp1(1:nft1, 1:nft2, 1:nft3)
    end do
   endif
!================== end prly====================
!=============
   if (prlz) then
    zbd = min(3, npe_zloc - 1 - imodz)
    kl = min(imodz, 3)
    kr = min(nhz + 3 - imodz, 3)
    dd = 2
    j1 = loc_yft_ord(imody)
    j2 = j1 + nft2 - 1
!================= imodz sends to  imodz-1,-2,-3
    if (imodz < nhz + 4) then
     if (imodz > 0) then
      do ip = 1, kl
       tag = 100 + ip
       pes = zp_prev(ip)
       call mpi_send(w_s(1, 1, 1), lenw, mpi_sd, pes, tag, comm_col(dd), error)
      end do
     endif
     k1 = loc_zft_ord(imodz)
     do ip = 1, kr
      tag = 100 + ip
      per = zp_next(ip)
      call mpi_recv(fp1(1, 1, 1), lenw, &
                    mpi_sd, per, tag, comm_col(dd), status, error)
      k1 = k1 + nft3
      k2 = k1 + nft3 - 1
      w_r(1:nft1, j1:j2, k1:k2) = fp1(1:nft1, 1:nft2, 1:nft3)
     end do
    endif
    !sends nhz => (nhz+1,nhz+2,nhz+3)      => npe_zloc-2 > npe_zloc-1 (zbd=1)
    if (imodz > (nhz - 1)) then
     kkr = imodz - nhz
     kkr = min(kkr, 3)
     do ip = 1, zbd
      tag = 100 - ip
      pes = zp_next(ip)
      call mpi_send(w_s(1, 1, 1), lenw, mpi_sd, pes, tag, comm_col(dd), error)
     end do
     k1 = loc_zft_ord(imodz)
     do ip = 1, kkr
      per = zp_prev(ip)
      tag = 100 - ip
      call mpi_recv(fp1(1, 1, 1), lenw, &
                    mpi_sd, per, tag, comm_col(dd), status, error)
      k1 = k1 - nft3
      k2 = k1 + nft3 - 1
      w_r(1:nft1, j1:j2, k1:k2) = fp1(1:nft1, 1:nft2, 1:nft3)
     end do
    endif
   endif
  end subroutine

  subroutine swap_yx_3data(waux, wdata, n1_loc, n2, n3)

   integer, intent(in) :: n1_loc, n2, n3
   real(dp), intent(in) :: waux(:, :, :)
   real(dp), intent(out) :: wdata(:, :, :)

   integer :: pes, per, ip
   integer :: i1, j1, lenws, lenwr, tag, n2_xloc, iy, ix, iz
   integer :: kk
   !-----------------
   !From waux(1:n1,1:n2_xloc) to wdata(1:n1_loc,1:n2)
   n2_xloc = n2/npe_xloc

   do iz = 1, n3
    do iy = 1, n2_xloc
     j1 = iy + n2_xloc*imodx
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imodx
      wdata(ix, j1, iz) = waux(i1, iy, iz)
     end do
    end do
   end do
   if (npe_yloc == 1) return
   lenws = n1_loc*n2_xloc*n3
   lenwr = lenws
   if (size(faux1) < lenwr) then
    deallocate (faux1, faux2)
    allocate (faux1(lenws))
    allocate (faux2(lenwr))
   end if

   do ip = 1, npe_xloc - 1
    pes = xp_next(ip)
    i1 = n1_loc*pes
    per = xp_prev(ip)
    tag = 80 + ip
    kk = 0
    do iz = 1, n3
     do iy = 1, n2_xloc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = waux(ix + i1, iy, iz)
      end do
     end do
    end do
    call mpi_sendrecv(faux1(1), lenws, mpi_sd, pes, tag, faux2(1), &
                      lenwr, mpi_sd, per, tag, comm_col(3), status, error)
    j1 = n2_xloc*per
    kk = 0
    do iz = 1, n3
     do iy = 1, n2_xloc
      do ix = 1, n1_loc
       kk = kk + 1
       wdata(ix, iy + j1, iz) = faux2(kk)
      end do
     end do
    end do
   end do

  end subroutine

  subroutine swap_xy_3data(wp1, wp2, n1_loc, n2_loc, n3_loc)

   integer, intent(in) :: n1_loc, n2_loc, n3_loc
   real(dp), intent(in) :: wp1(:, :, :)
   real(dp), intent(out) :: wp2(:, :, :)

   integer :: pes, per, ip
   integer :: i1, j1, lenws, lenwr, tag, iy, ix, iz
   integer :: kk
   !-----------------
   !From wp1(1:n1_loc,1:n2) to wp2(1:n1,1:n2_loc,ic)

   do iz = 1, n3_loc
    do iy = 1, n2_loc
     j1 = iy + n2_loc*imody
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imody
      wp2(i1, iy, iz) = wp1(ix, j1, iz)
     end do
    end do
   end do
   if (npe_yloc == 1) return
   lenws = n1_loc*n2_loc*n3_loc
   lenwr = lenws
   if (size(faux1) < lenwr) then
    deallocate (faux1, faux2)
    allocate (faux1(lenws))
    allocate (faux2(lenwr))
   end if

   do ip = 1, npe_yloc - 1
    pes = yp_next(ip)
    j1 = n2_loc*pes
    per = yp_prev(ip)
    tag = 80 + ip
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = wp1(ix, iy + j1, iz)
      end do
     end do
    end do
    call mpi_sendrecv(faux1(1), lenws, mpi_sd, pes, tag, faux2(1), &
                      lenwr, mpi_sd, per, tag, comm_col(1), status, error)
    i1 = n1_loc*per
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       wp2(ix + i1, iy, iz) = faux2(kk)
      end do
     end do
    end do
   end do

  end subroutine

  subroutine swap_xz_3data(wp1, wp2, n1_loc, n2_loc, n3_loc)

   integer, intent(in) :: n1_loc, n2_loc, n3_loc
   real(dp), intent(in) :: wp1(:, :, :)
   real(dp), intent(out) :: wp2(:, :, :)

   integer :: pes, per, ip
   integer :: i1, lenw, tag, iy, ix, iz, iz1
   integer :: kk
   !-----------------
   !From wp1(1:n1_loc,1:n3) to wp2(1:n1,1:n3_loc)
   do iz = 1, n3_loc
    iz1 = iz + n3_loc*imodz
    do iy = 1, n2_loc
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imodz
      wp2(i1, iy, iz) = wp1(ix, iy, iz1)
     end do
    end do
   end do
   if (npe_zloc == 1) return
   lenw = n1_loc*n2_loc*n3_loc
   if (size(faux1) < lenw) then
    deallocate (faux1, faux2)
    allocate (faux1(lenw))
    allocate (faux2(lenw))
   end if

   do ip = 1, npe_zloc - 1
    pes = zp_next(ip)
    iz1 = n3_loc*pes
    per = zp_prev(ip)
    tag = 80 + ip
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = wp1(ix, iy, iz + iz1)
      end do
     end do
    end do
    call mpi_sendrecv(faux1(1), lenw, mpi_sd, pes, tag, faux2(1), lenw, &
                      mpi_sd, per, tag, comm_col(2), status, error)
    i1 = n1_loc*per
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       wp2(ix + i1, iy, iz) = faux2(kk)
      end do
     end do
    end do
   end do

  end subroutine

  subroutine swap_yx_3data_inv(wdata, waux, n1_loc, n2, n3)

   integer, intent(in) :: n1_loc, n2, n3
   real(dp), intent(in) :: wdata(:, :, :)
   real(dp), intent(out) :: waux(:, :, :)

   integer :: pes, per, ip
   integer :: i1, j1, lenws, lenwr, tag, iy, ix, iz, n2_xloc
   integer :: kk
   !-----------------
   !enters wdata(n1_loc,n2,n3)
   !From wdata(1:n1_loc,1:n2) to waux(1:n1,1:n2_xloc)
   !n2_xloc=n2/npe_xloc

   n2_xloc = n2/npe_xloc
   kk = 0
   do iz = 1, n3
    do iy = 1, n2_xloc
     j1 = iy + n2_xloc*imodx
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imodx
      kk = kk + 1
      waux(i1, iy, iz) = wdata(ix, j1, iz)
     end do
    end do
   end do
   if (npe_xloc == 1) return
   lenwr = n1_loc*n2_xloc*n3
   lenws = lenwr
   if (size(faux1) < lenwr) then
    deallocate (faux1, faux2)
    allocate (faux1(lenws))
    allocate (faux2(lenwr))
   end if
   do ip = 1, npe_xloc - 1
    pes = xp_next(ip)
    j1 = n2_xloc*pes
    per = xp_prev(ip)
    tag = 40 + ip
    kk = 0
    do iz = 1, n3
     do iy = 1, n2_xloc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = wdata(ix, iy + j1, iz)
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws

    call mpi_sendrecv(faux1(1), lenws, mpi_sd, pes, tag, faux2(1), &
                      lenwr, mpi_sd, per, tag, comm_col(3), status, error)
    i1 = n1_loc*per
    kk = 0
    do iz = 1, n3
     do iy = 1, n2_xloc
      do ix = 1, n1_loc
       kk = kk + 1
       waux(ix + i1, iy, iz) = faux2(kk)
      end do
     end do
    end do
   end do
  end subroutine

  subroutine swap_xy_3data_inv(wp2, wp1, n1_loc, n2_loc, n3_loc)

   integer, intent(in) :: n1_loc, n2_loc, n3_loc
   real(dp), intent(in) :: wp2(:, :, :)
   real(dp), intent(out) :: wp1(:, :, :)

   integer :: pes, per, ip
   integer :: i1, j1, lenws, lenwr, tag, iy, ix, iz
   integer :: kk
   !-----------------
   !From (1:n1,1:n2_loc) to (1:n1_loc,1:n2)

   kk = 0
   do iz = 1, n3_loc
    do iy = 1, n2_loc
     j1 = iy + n2_loc*imody
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imody
      kk = kk + 1
      wp1(ix, j1, iz) = wp2(i1, iy, iz)
     end do
    end do
   end do
   if (npe_yloc == 1) return
   lenwr = n1_loc*n2_loc*n3_loc
   lenws = lenwr
   if (size(faux1) < lenwr) then
    deallocate (faux1, faux2)
    allocate (faux1(lenws))
    allocate (faux2(lenwr))
   end if
   do ip = 1, npe_yloc - 1
    pes = yp_next(ip)
    i1 = n1_loc*pes
    per = yp_prev(ip)
    tag = 40 + ip
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = wp2(ix + i1, iy, iz)
      end do
     end do
    end do
    lenws = kk
    lenwr = lenws

    call mpi_sendrecv(faux1(1), lenws, mpi_sd, pes, tag, faux2(1), &
                      lenwr, mpi_sd, per, tag, comm_col(1), status, error)
    j1 = n2_loc*per
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       wp1(ix, iy + j1, iz) = faux2(kk)
      end do
     end do
    end do
   end do
  end subroutine
  !=====================
  subroutine swap_xz_3data_inv(wp2, wp1, n1_loc, n2_loc, n3_loc)

   integer, intent(in) :: n1_loc, n2_loc, n3_loc
   real(dp), intent(in) :: wp2(:, :, :)
   real(dp), intent(out) :: wp1(:, :, :)

   integer :: pes, per, ip
   integer :: i1, lenw, tag, iy, ix, iz, iz1
   integer :: kk, k1
   !-----------------
   !From (1:n1,1:n3_loc) to (1:n1_loc,1:n3)

   do iz = 1, n3_loc
    k1 = iz + imodz*n3_loc
    do iy = 1, n2_loc
     do ix = 1, n1_loc
      i1 = ix + n1_loc*imodz
      wp1(ix, iy, k1) = wp2(i1, iy, iz)
     end do
    end do
   end do
   if (npe_zloc == 1) return
   lenw = n1_loc*n2_loc*n3_loc
   if (size(faux1) < lenw) then
    deallocate (faux1, faux2)
    allocate (faux1(lenw))
    allocate (faux2(lenw))
   end if
   do ip = 1, npe_zloc - 1
    pes = zp_next(ip)
    i1 = n1_loc*pes
    per = zp_prev(ip)
    tag = 40 + ip
    kk = 0
    do iz = 1, n3_loc
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       faux1(kk) = wp2(ix + i1, iy, iz)
      end do
     end do
    end do

    call mpi_sendrecv(faux1(1), lenw, mpi_sd, pes, tag, faux2(1), lenw, &
                      mpi_sd, per, tag, comm_col(2), status, error)
    k1 = n3_loc*per
    kk = 0
    do iz = 1, n3_loc
     iz1 = iz + k1
     do iy = 1, n2_loc
      do ix = 1, n1_loc
       kk = kk + 1
       wp1(ix, iy, iz1) = faux2(kk)
      end do
     end do
    end do
   end do
  end subroutine
  !====================================
  subroutine pftw2d_sc(w, n1, n2, n2_loc, n3, n3_loc, is, sym)
   real(dp), intent(inout) :: w(:, :, :)
   integer, intent(in) :: n1, n2, n2_loc, n3, n3_loc, is, sym
   integer :: n1_loc
   !performs a 2D FFT sin/cosine`on the (y,z) coordinates for a 3D data (x,y,z)
   !sym=1 for a sine transform
   !sym=2 for a cosine transform

   select case (is)
   case (-1) !
    if (n2 <= 2) return
    if (prly) then
     n1_loc = n1/npe_yloc
     call swap_xy_3data_inv(w, fp1, n1_loc, n2_loc, n3_loc)

     call ftw1d_sc(fp1, n1_loc, n2, n3_loc, is, 2, sym)
     call swap_xy_3data(fp1, w, n1_loc, n2_loc, n3_loc)
    else
     call ftw1d_sc(w, n1, n2, n3_loc, is, 2, sym)
    end if
    !=====================
    if (n3 <= 2) return
    if (npe_zloc > 1) then
     n1_loc = n1/npe_zloc
     call swap_xz_3data_inv(w, fp2, n1_loc, n2_loc, n3_loc)
     call ftw1d_sc(fp2, n1_loc, n2_loc, n3, is, 3, sym)
     call swap_xz_3data(fp2, w, n1_loc, n2_loc, n3_loc)
    else
     call ftw1d_sc(w, n1, n2_loc, n3, is, 3, sym)
    end if
    !======== exit w(loc)
   case (1)
    ! enters w(loc)
    !========================
    if (n3 > 1) then
     if (npe_zloc > 1) then
      n1_loc = n1/npe_zloc
      call swap_xz_3data_inv(w, fp2, n1_loc, n2_loc, n3_loc)
      call ftw1d_sc(fp2, n1_loc, n2_loc, n3, is, 3, sym)
      call swap_xz_3data(fp2, w, n1_loc, n2_loc, n3_loc)
     else
      call ftw1d_sc(w, n1, n2_loc, n3, is, 3, sym)
     end if
    end if
    !=================
    if (n2 > 1) then
     if (prly) then
      n1_loc = n1/npe_yloc
      call swap_xy_3data_inv(w, fp1, n1_loc, n2_loc, n3_loc)
      call ftw1d_sc(fp1, n1_loc, n2, n3_loc, is, 2, sym)
      call swap_xy_3data(fp1, w, n1_loc, n2_loc, n3_loc)
     else
      call ftw1d_sc(w, n1, n2, n3_loc, is, 2, sym)
     end if
    end if
   end select
   !===================
   !exit w(loc)
  end subroutine
  !========================
  subroutine pftw3d_sc(w, n1, n2, n2_loc, n3, n3_loc, is, sym)
   real(dp), intent(inout) :: w(:, :, :)
   integer, intent(in) :: n1, n2, n2_loc, n3, n3_loc, is, sym

   select case (is)
   case (-1)
    call ftw1d_sc(w, n1, n2_loc, n3_loc, is, 1, sym)
    call pftw2d_sc(w, n1, n2, n2_loc, n3, n3_loc, is, sym)
   case (1)
    ! enters w(loc)
    call pftw2d_sc(w, n1, n2, n2_loc, n3, n3_loc, is, sym)
    !========================
    call ftw1d_sc(w, n1, n2_loc, n3_loc, is, 1, sym)
   end select
   !===================
   !exit w(loc)
  end subroutine
  !============================
  subroutine pftw2d(w, n1, n2, n2_loc, n3, n3_loc, is)
   real(dp), intent(inout) :: w(:, :, :)
   integer, intent(in) :: n1, n2, n2_loc, n3, n3_loc, is
   integer :: n1_loc, dir

   select case (is)
   case (-1)
    if (n2 <= 2) return
    dir = 2
    if (prly) then
     n1_loc = n1/npe_yloc
     call swap_xy_3data_inv(w, fp1, n1_loc, n2_loc, n3_loc)
     call ftw1d(fp1, n1_loc, n2, n3_loc, is, dir)
     call swap_xy_3data(fp1, w, n1_loc, n2_loc, n3_loc)
    else
     call ftw1d(w, n1, n2, n3_loc, is, dir)
    end if
    !=====================
    if (n3 <= 2) return
    dir = 3
    if (npe_zloc > 1) then
     n1_loc = n1/npe_zloc
     call swap_xz_3data_inv(w, fp2, n1_loc, n2_loc, n3_loc)
     call ftw1d(fp2, n1_loc, n2_loc, n3, is, dir)
     call swap_xz_3data(fp2, w, n1_loc, n2_loc, n3_loc)
    else
     call ftw1d(w, n1, n2_loc, n3, is, dir)
    end if
    !======== exit w(loc)
   case (1)
    ! enters w(loc)
    !========================
    if (n3 > 1) then
     dir = 3
     if (npe_zloc > 1) then
      n1_loc = n1/npe_zloc
      call swap_xz_3data_inv(w, fp2, n1_loc, n2_loc, n3_loc)
      call ftw1d(fp2, n1_loc, n2_loc, n3, is, dir)
      call swap_xz_3data(fp2, w, n1_loc, n2_loc, n3_loc)
     else
      call ftw1d(w, n1, n2_loc, n3, is, dir)
     end if
    end if
    !=================
    if (n2 > 1) then
     dir = 2
     if (prly) then
      n1_loc = n1/npe_yloc
      call swap_xy_3data_inv(w, fp1, n1_loc, n2_loc, n3_loc)
      call ftw1d(fp1, n1_loc, n2, n3_loc, is, dir)
      call swap_xy_3data(fp1, w, n1_loc, n2_loc, n3_loc)
     else
      call ftw1d(w, n1, n2, n3_loc, is, dir)
     end if
    end if
   end select
   !===================
   !exit w(loc)
  end subroutine
  !====================================
  subroutine pftw3d(w, n1, n2, n2_loc, n3, n3_loc, is)
   real(dp), intent(inout) :: w(:, :, :)
   integer, intent(in) :: n1, n2, n2_loc, n3, n3_loc, is

   select case (is)
   case (-1)

    call ftw1d(w, n1, n2_loc, n3_loc, is, 1)

    call pftw2d(w, n1, n2, n2_loc, n3, n3_loc, is)

   case (1)
    call pftw2d(w, n1, n2, n2_loc, n3, n3_loc, is)

    call ftw1d(w, n1, n2_loc, n3_loc, is, 1)
   end select
   !===================
   !exit w(loc)
  end subroutine
 end module

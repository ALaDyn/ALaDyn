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
! overlapping grid  structure:
!
!====================================================================
!                                     1-----2---- 3--- 4   |      pey+1
!                 1-----2----[3-------np-1--np]--np+1--np+2|    pey
!1-2--------------np-1--np---np+1                          |pey-1
!======================================================================
!      Right(pey+1)     [1:4]     overlap pey [np-1:np+2]
!      Left(pey-1)      [np-1:np+2]overlap pey [1:4]
!===================================

 module set_grid_param

  use common_param
  use grid_param
  use mpi_var

  implicit none

  integer, private :: shift_x, shift_y, shift_z
 contains

  !===========================================================
  ! allocates and defines global (x,y,z)coordinates for uniform or stretched
  ! configurations
  !===========================================================
  subroutine set_grid(n1, n2, n3, ib, x_stretch, y_stretch, xres, yxres, &
    zxres, sh_x, sh_y, sh_z)
   integer, intent (in) :: n1, n2, n3, ib, x_stretch, y_stretch
   real (dp), intent (in) :: xres, yxres, zxres
   integer, intent(in) :: sh_x, sh_y, sh_z
   integer :: i, ns1
   real(dp) :: yy, yyh, smin, smax

   allocate (x(0:n1 + 1), xw(n1 + 1), dx1(0:n1 + 1), y(0:n2 + 1), z(0:n3 + 1), dy1(0:n2 + 1), &
             dz1(0:n3 + 1))
   allocate (dx1h(0:n1 + 1), dy1h(0:n2 + 1), dz1h(0:n3 + 1))
   allocate (xh(0:n1 + 1), yh(0:n2 + 1), zh(0:n3 + 1))
   !-----------------------------------
   ! Reducing guard cells to 1 if there's no dimension in that direction
   shift_x = sh_x
   shift_y = sh_y
   shift_z = sh_z
   if (ndim < 3) then
    shift_z = 1
   end if
   if (ndim < 2) then
    shift_y = 1
   end if

   aph = acos(-1.0)*0.4
   dxi = 1.
   dyi = 1.
   dzi = 1.
   sx_rat = 1.
   sy_rat = 1.
   sz_rat = 1.
   smin = 0.0
   smax = 0.0
   dx = 1.
   if (xres > 0.0) dx = 1./xres
   dx_inv = 1.0/dx
   do i = 0, n1 + 1
    x(i) = dx*real(i - 1, dp) !xminx(1)=0,.....,xmax-dx=x(nx)
    xh(i) = x(i) + 0.5*dx
    dx1(i) = 1.
    dx1h(i) = 1.
   end do
   dxi = dx
   dxi_inv = dx_inv
   ns1 = n1 + 1 - x_stretch
   if (x_stretch > 0) then
    dxi = aph/real(x_stretch, dp)
    dxi_inv = 1./dxi
    lx_s = dx*dxi_inv
    sx_rat = dxi*dx_inv
    smax = x(ns1)
    do i = ns1, n1 + 1
     yy = dxi*real(i - ns1, dp)
     yyh = yy + dxi*0.5
     x(i) = smax + lx_s*tan(yy)
     xh(i) = smax + lx_s*tan(yyh)
     dx1h(i) = cos(yyh)*cos(yyh)
     dx1(i) = cos(yy)*cos(yy)
    end do
   end if
   str_xgrid%sind(1) = x_stretch
   str_xgrid%sind(2) = ns1
   str_xgrid%smin = x(1)
   str_xgrid%smax = x(ns1)
   xw = x
   xmax = x(n1)
   xmin = x(1)
   if (ib == 2) xmax = x(n1 + 1)
   lx_box = xmax - xmin
   str_xgrid%stretched_length = xmax - str_xgrid%smax
   xw_min = xmin
   xw_max = xmax

   dy = 1.
   dy_inv = 1./dy
   dyi = dy
   dyi_inv = 1./dy
   ymin = 0.0
   ymax = 0.0
   y = 0.0
   yh = 0.0
   dy1 = 1.
   dy1h = 1.
   ly_box = 1.
   if (n2 > 1) then
    dy = yxres*dx
    dy_inv = 1./dy
    dyi = dy
    dyi_inv = dy_inv
    do i = 0, n2 + 1
     y(i) = dy*real(i - 1 - n2/2, dp)
     yh(i) = y(i) + 0.5*dy
     dy1(i) = 1.
     dy1h(i) = 1.
    end do
    !============== Stretched grid
    !   index      [1:y_stretch]              [ns1:n2+1]
    !   coordinate [y(1): smin=y(y_stretch+1]   [smax: y(n2+1)=ymax]
    !   unstretched: [y_stretch+1:ns1-1=n2+1-(y_stretch1)]
    !========================
    ns1 = n2 + 1 - y_stretch
    if (y_stretch > 0) then
     dyi = aph/real(y_stretch, dp)
     dyi_inv = 1./dyi
     l_s = dy*dyi_inv
     sy_rat = dyi*dy_inv
     smin = y(y_stretch + 1)
     smax = y(ns1)
     str_ygrid%sind(1) = y_stretch
     str_ygrid%sind(2) = ns1
     str_ygrid%smin = smin
     str_ygrid%smax = smax
     do i = 0, y_stretch
      yy = dyi*real(i - 1 - y_stretch, dp)
      yyh = yy + 0.5*dyi
      y(i) = smin + l_s*tan(yy) !y(xi)=y_s +(Dy/Dxi)*tan(xi) Dy=L_s*dxi uniform
      yh(i) = smin + l_s*tan(yyh)
      dy1h(i) = cos(yyh)*cos(yyh) ! dy(xi)= L_s/cos^2(xi)=(Dy/Dxi)/cos^2(xi)
      dy1(i) = cos(yy)*cos(yy) ! dy1=cos^2(xi)=(dy/dxi)^{-1}*(Dy0/Dxi)
     end do
     do i = ns1, n2 + 1
      yy = dyi*real(i - ns1, dp)
      yyh = yy + dyi*0.5
      y(i) = smax + l_s*tan(yy)
      yh(i) = smax + l_s*tan(yyh)
      dy1h(i) = cos(yyh)*cos(yyh)
      dy1(i) = cos(yy)*cos(yy)
     end do
    end if
    ymin = y(1)
    ymax = y(n2 + 1)
    str_ygrid%stretched_length = ymax - str_ygrid%smax
    ly_box = ymax - ymin
   end if
   dz = 1.
   dz_inv = 1./dz
   dzi = dz
   dzi_inv = 1./dz
   zmin = 0.0
   zmax = 0.0
   z = 0.0
   zh = 0.0
   dz1 = 1.
   dz1h = 1.
   lz_box = 1.
   if (n3 > 1) then
    dz = zxres*dx
    dz_inv = 1./dz
    do i = 0, n3 + 1
     z(i) = dz*real(i - 1 - n3/2, dp)
     zh(i) = z(i) + 0.5*dz
     dz1(i) = 1.
     dz1h(i) = 1.
    end do
    ns1 = n3 + 1 - y_stretch
    if (y_stretch > 0) then
     dzi = aph/real(y_stretch, dp)
     dzi_inv = 1./dzi
     l_s = dz*dzi_inv
     sz_rat = dzi*dz_inv
     smin = z(y_stretch + 1)
     smax = z(ns1)
     str_zgrid%sind(1) = y_stretch
     str_zgrid%sind(2) = ns1
     str_zgrid%smin = smin
     str_zgrid%smax = smax
     do i = 0, y_stretch
      yy = dzi*real(i - 1 - y_stretch, dp)
      yyh = yy + 0.5*dzi
      z(i) = smin + l_s*tan(yy)
      zh(i) = smin + l_s*tan(yyh)
      dz1h(i) = cos(yyh)*cos(yyh)
      dz1(i) = cos(yy)*cos(yy)
     end do
     do i = ns1, n3 + 1
      yy = dzi*real(i - ns1, dp)
      yyh = yy + dzi*0.5
      z(i) = smax + l_s*tan(yy)
      zh(i) = smax + l_s*tan(yyh)
      dz1h(i) = cos(yyh)*cos(yyh)
      dz1(i) = cos(yy)*cos(yy)
     end do
    end if
    zmin = z(1)
    zmax = z(n3 + 1)
    lz_box = zmax - zmin
    str_zgrid%stretched_length = zmax - str_zgrid%smax
   end if
  end subroutine
  !================
  !================
  subroutine mpi_loc_grid(n1_loc, n2_loc, n3_loc, npex, npey, npez)

   integer, intent(in) :: n1_loc, n2_loc, n3_loc, npex, npey, npez
   integer :: p

   allocate (loc_ygrid(0:npey - 1), loc_zgrid(0:npez - 1))
   allocate (loc_xgrid(0:npex - 1))

   loc_xgr_max = n1_loc
   do p = 0, npex - 1
    loc_xgrid(p)%ng = n1_loc
   end do
   loc_ygr_max = n2_loc
   loc_zgr_max = n3_loc
   do p = 0, npey - 1
    loc_ygrid(p)%ng = n2_loc
   end do
   do p = 0, npez - 1
    loc_zgrid(p)%ng = n3_loc
   end do
   allocate (nxh(npex), nyh(npey), nzh(npez))

   allocate (loc_yg(0:loc_ygr_max + 1, 4, 0:npey - 1))
   allocate (loc_zg(0:loc_zgr_max + 1, 4, 0:npez - 1))
   allocate (loc_xg(0:loc_xgr_max + 1, 4, 0:npex - 1))

   allocate (str_indx(0:npey - 1, 0:npez - 1))
   str_indx(0:npey - 1, 0:npez - 1) = 0

  end subroutine

  subroutine set_output_grid(jmp, npex, npey, npez)

   integer, intent(in) :: jmp, npex, npey, npez
   integer :: i1, j1, k1, i2, j2, k2
   integer :: ipe, ix, iix1, iy1, iz1, ngyzx(3), ngout

   do ipe = 0, npex - 1
    i1 = loc_xgrid(ipe)%p_ind(1)
    i2 = loc_xgrid(ipe)%p_ind(2)
    iix1 = 0
    do ix = i1, i2, jmp
     iix1 = iix1 + 1
    end do
    nxh(ipe + 1) = iix1
   end do
   do ipe = 0, npey - 1
    j1 = loc_ygrid(ipe)%p_ind(1)
    j2 = loc_ygrid(ipe)%p_ind(2)
    iy1 = 0
    do ix = j1, j2, jmp
     iy1 = iy1 + 1
    end do
    nyh(ipe + 1) = iy1
   end do
   do ipe = 0, npez - 1
    k1 = loc_zgrid(ipe)%p_ind(1)
    k2 = loc_zgrid(ipe)%p_ind(2)
    iz1 = 0
    do ix = k1, k2, jmp
     iz1 = iz1 + 1
    end do
    nzh(ipe + 1) = iz1
   end do
   ngout = max(nx, ny)
   ngout = max(ngout, nz)
   allocate (gwdata(ngout)) !
   ngyzx(1) = maxval(nyh(1:npey))
   ngyzx(2) = maxval(nzh(1:npez))
   ngyzx(3) = maxval(nxh(1:npex))
   ngout = ngyzx(1)*ngyzx(2)*ngyzx(3)
   if (ngout > 0) then
    allocate (wdata(ngout))
   else
    allocate (wdata(1))
   end if

  end subroutine
!============================
  subroutine set_ftyzgrid(npey, npez)
   integer, intent (in) :: npey, npez
   integer :: i, ii, p, ip, n_loc,last_ind

   ! defines local yftgrid and zftgrid
   loc_yftgrid(0)%gmin = yft(1)
   ip = loc_yftgrid(0)%ng
   n_loc = ip
   loc_yftgrid(0)%gmax = yft(ip+1)
   loc_yftgrid(0)%p_ind(1) = min(shift_y, n_loc)
   loc_yftgrid(0)%p_ind(2) = n_loc + loc_yftgrid(0)%p_ind(1) - 1
   loc_yftgrid(0)%min_cell = 0
   loc_yftgrid(0)%max_cell = loc_yftgrid(0)%min_cell + n_loc - 1
   loc_yftgrid(0)%shift = shift_y

   p = 0
   do i = 1, n_loc + 1
    loc_yft(i, p) = yft(i)
   end do

   if (npey > 1) then
    ip = loc_yftgrid(0)%ng
    if (npey > 2) then
     do p = 1, npey - 2
      n_loc = loc_yftgrid(p - 1)%ng
      n_loc = loc_yftgrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_yft(i, p) = yft(ii)
      end do
      loc_yftgrid(p)%gmin = loc_yftgrid(p - 1)%gmax

      ip = ip + n_loc
      loc_yftgrid(p)%gmax = yft(ip + 1)

      loc_yftgrid(p)%p_ind(1) = shift_y
      loc_yftgrid(p)%p_ind(2) = n_loc + loc_yftgrid(p)%p_ind(1) - 1
      loc_yftgrid(p)%min_cell = loc_yftgrid(p-1)%min_cell + n_loc
      loc_yftgrid(p)%max_cell = loc_yftgrid(p-1)%max_cell + n_loc
      loc_yftgrid(p)%shift = shift_y

     end do
    end if
    p = npey - 1
    n_loc = loc_yftgrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_yft(i, p) = yft(ii)
    end do
    loc_yftgrid(p)%gmin = loc_yftgrid(p - 1)%gmax
    ip = ip + n_loc
    loc_yftgrid(p)%gmax = yft(ip+1)
    loc_yftgrid(p)%p_ind(1) = shift_y
    loc_yftgrid(p)%p_ind(2) = n_loc + loc_yftgrid(p)%p_ind(1) - 1
    loc_yftgrid(p)%min_cell = loc_yftgrid(p-1)%min_cell + n_loc
    loc_yftgrid(p)%max_cell = loc_yftgrid(p-1)%max_cell + n_loc
    loc_yftgrid(p)%shift = shift_y

   end if
!      Now redefine loc_yft coordinate on an extended grid to be overset the
!      strethed grid
   if (npey > 3) then
    n_loc = loc_yftgrid(0)%ng
    last_ind = 0
    do p = 0, npey/2 - 1
     do i = 1, 4*n_loc
      ii = i + last_ind
      loc_yft(i, p) = yft(ii)
     end do
     last_ind = last_ind + n_loc
    enddo
!========================
    last_ind = last_ind - n_loc
    do p = npey/2, npey/2 + 1
     do i = 1, 4*n_loc
      ii = i + last_ind
      loc_yft(i, p) = yft(ii)
     end do
    end do
!==================
    do p = npey/2 + 2, npey - 1
     do i = 1, 4*n_loc
      ii = i + last_ind
      loc_yft(i, p) = yft(ii)
     end do
     last_ind = last_ind + n_loc
    enddo
   end if
!=========================
   loc_zftgrid(0)%gmin = zft(1)
   ip = loc_zftgrid(0)%ng
   n_loc = ip
   loc_zftgrid(0)%gmax = zft(ip+1)
   loc_zftgrid(0)%p_ind(1) = min(shift_z, n_loc)
   loc_zftgrid(0)%p_ind(2) = n_loc + loc_zftgrid(0)%p_ind(1) - 1
   loc_zftgrid(0)%min_cell = 0
   loc_zftgrid(0)%max_cell = loc_zftgrid(0)%min_cell + n_loc - 1
   loc_zftgrid(0)%shift = shift_z

   p = 0
   do i = 1, n_loc + 1
    loc_zft(i, p) = zft(i)
   end do

   if (npez > 1) then
    ip = loc_zftgrid(0)%ng
    if (npez > 2) then
     do p = 1, npez - 2
      n_loc = loc_zftgrid(p - 1)%ng
      n_loc = loc_zftgrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_zft(i, p) = zft(ii)
      end do
      loc_zftgrid(p)%gmin = loc_zftgrid(p - 1)%gmax

      ip = ip + n_loc
      loc_zftgrid(p)%gmax = zft(ip + 1)

      loc_zftgrid(p)%p_ind(1) = shift_z
      loc_zftgrid(p)%p_ind(2) = n_loc + loc_zftgrid(p)%p_ind(1) - 1
      loc_zftgrid(p)%min_cell = loc_zftgrid(p-1)%min_cell + n_loc
      loc_zftgrid(p)%max_cell = loc_zftgrid(p-1)%max_cell + n_loc
      loc_zftgrid(p)%shift = shift_z

     end do
    end if
    p = npez - 1
    n_loc = loc_zftgrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_zft(i, p) = zft(ii)
    end do
    loc_zftgrid(p)%gmin = loc_zftgrid(p - 1)%gmax
    ip = ip + n_loc
    loc_zftgrid(p)%gmax = zft(ip+1)
    loc_zftgrid(p)%p_ind(1) = shift_z
    loc_zftgrid(p)%p_ind(2) = n_loc + loc_zftgrid(p)%p_ind(1) - 1
    loc_zftgrid(p)%min_cell = loc_zftgrid(p-1)%min_cell + n_loc
    loc_zftgrid(p)%max_cell = loc_zftgrid(p-1)%max_cell + n_loc
    loc_zftgrid(p)%shift = shift_z

   end if
   if (npez > 3) then
    n_loc = loc_zftgrid(0)%ng
    last_ind = 0
    do p = 0, npez/2 - 1
     do i = 1, 4*n_loc

      ii = i + last_ind
      loc_zft(i, p) = zft(ii)
     end do
     last_ind = last_ind + n_loc
    enddo
    last_ind = last_ind - n_loc
    do p = npez/2, npez/2 + 1
     do i = 1, 4*n_loc
      ii = i + last_ind
      loc_zft(i, p) = zft(ii)
     end do
    end do
    do p = npez/2 + 2, npez - 1
     do i = 1, 4*n_loc
      ii = i + last_ind
      loc_zft(i, p) = zft(ii)
     end do
     last_ind = last_ind + n_loc
    enddo
   end if

  end subroutine

  subroutine set_fyzxgrid(npey, npez, npex)
   integer, intent (in) :: npey, npez, npex
   integer :: i, ii, p, ip, n_loc

   ! Defines initial local p-grid coordinate and loc n_cell
   ! y-grid decomposed on n2_loc uniform grid size
   loc_ygrid(0)%gmin = y(1)
   ip = loc_ygrid(0)%ng
   n_loc = ip
   loc_ygrid(0)%gmax = y(ip+1)
   loc_ygrid(0)%p_ind(1) = min(shift_y, n_loc)
   loc_ygrid(0)%p_ind(2) = n_loc + loc_ygrid(0)%p_ind(1) - 1
   loc_ygrid(0)%min_cell = 0
   loc_ygrid(0)%max_cell = loc_ygrid(0)%min_cell + n_loc - 1
   loc_ygrid(0)%shift = shift_y

   p = 0
   do i = 0, n_loc + 1
    loc_yg(i, 1, p) = y(i)
    loc_yg(i, 2, p) = yh(i)
    loc_yg(i, 3, p) = dy1(i)
    loc_yg(i, 4, p) = dy1h(i)
   end do

   if (npey > 1) then
    ip = loc_ygrid(0)%ng
    if (npey > 2) then
     do p = 1, npey - 2
      n_loc = loc_ygrid(p - 1)%ng
      loc_yg(0, 1:4, p) = loc_yg(n_loc, 1:4, p - 1)
      n_loc = loc_ygrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_yg(i, 1, p) = y(ii)
       loc_yg(i, 2, p) = yh(ii)
       loc_yg(i, 3, p) = dy1(ii)
       loc_yg(i, 4, p) = dy1h(ii)
      end do
      loc_ygrid(p)%gmin = loc_ygrid(p - 1)%gmax

      ip = ip + n_loc
      loc_ygrid(p)%gmax = y(ip + 1)

      loc_ygrid(p)%p_ind(1) = shift_y
      loc_ygrid(p)%p_ind(2) = n_loc + loc_ygrid(p)%p_ind(1) - 1
      loc_ygrid(p)%min_cell = loc_ygrid(p-1)%min_cell + n_loc
      loc_ygrid(p)%max_cell = loc_ygrid(p-1)%max_cell + n_loc
      loc_ygrid(p)%shift = shift_y

     end do
    end if
    p = npey - 1
    n_loc = loc_ygrid(p - 1)%ng
    loc_yg(0, 1:4, p) = loc_yg(n_loc, 1:4, p - 1)
    n_loc = loc_ygrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_yg(i, 1, p) = y(ii)
     loc_yg(i, 2, p) = yh(ii)
     loc_yg(i, 3, p) = dy1(ii) ! dy1=cos^2(xi) =[dxi/dy] stretched  (=1 y=xi)
     loc_yg(i, 4, p) = dy1h(ii)

    end do
    loc_ygrid(p)%gmin = loc_ygrid(p - 1)%gmax
    ip = ip + n_loc
    loc_ygrid(p)%gmax = y(ip+1)
    loc_ygrid(p)%p_ind(1) = shift_y
    loc_ygrid(p)%p_ind(2) = n_loc + loc_ygrid(p)%p_ind(1) - 1
    loc_ygrid(p)%min_cell = loc_ygrid(p-1)%min_cell + n_loc
    loc_ygrid(p)%max_cell = loc_ygrid(p-1)%max_cell + n_loc
    loc_ygrid(p)%shift = shift_y

   end if
   !=========================
   loc_zgrid(0)%gmin = z(1)
   ip = loc_zgrid(0)%ng
   n_loc = ip
   loc_zgrid(0)%gmax = z(ip+1)
   loc_zgrid(0)%p_ind(1) = min(shift_z, n_loc)
   loc_zgrid(0)%p_ind(2) = n_loc + loc_zgrid(0)%p_ind(1) - 1
   loc_zgrid(0)%min_cell = 0
   loc_zgrid(0)%max_cell = loc_zgrid(0)%min_cell + n_loc - 1
   loc_zgrid(0)%shift = shift_z

   p = 0
   do i = 0, n_loc + 1
    loc_zg(i, 1, p) = z(i)
    loc_zg(i, 2, p) = zh(i)
    loc_zg(i, 3, p) = dz1(i)
    loc_zg(i, 4, p) = dz1h(i)
   end do

   if (npez > 1) then
    ip = loc_zgrid(0)%ng
    if (npez > 2) then
     do p = 1, npez - 2
      n_loc = loc_zgrid(p - 1)%ng
      loc_zg(0, 1:4, p) = loc_zg(n_loc, 1:4, p - 1)
      n_loc = loc_zgrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_zg(i, 1, p) = z(ii)
       loc_zg(i, 2, p) = zh(ii)
       loc_zg(i, 3, p) = dz1(ii)
       loc_zg(i, 4, p) = dz1h(ii)
      end do
      loc_zgrid(p)%gmin = loc_zgrid(p - 1)%gmax
      ip = ip + n_loc
      loc_zgrid(p)%gmax = z(ip+1)
      loc_zgrid(p)%p_ind(1) = shift_z
      loc_zgrid(p)%p_ind(2) = n_loc + loc_zgrid(p)%p_ind(1) - 1
      loc_zgrid(p)%min_cell = loc_zgrid(p-1)%min_cell + n_loc
      loc_zgrid(p)%max_cell = loc_zgrid(p-1)%max_cell + n_loc
      loc_zgrid(p)%shift = shift_z
     end do
    end if
    p = npez - 1
    n_loc = loc_zgrid(p - 1)%ng
    loc_zg(0, 1:4, p) = loc_zg(n_loc, 1:4, p - 1)
    n_loc = loc_zgrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_zg(i, 1, p) = z(ii)
     loc_zg(i, 2, p) = zh(ii)
     loc_zg(i, 3, p) = dz1(ii)
     loc_zg(i, 4, p) = dz1h(ii)
    end do
    loc_zgrid(p)%gmin = loc_zgrid(p - 1)%gmax
    ip = ip + n_loc
    loc_zgrid(p)%gmax = z(ip+1)
    loc_zgrid(p)%p_ind(1) = shift_z
    loc_zgrid(p)%p_ind(2) = n_loc + loc_zgrid(p)%p_ind(1) - 1
    loc_zgrid(p)%min_cell = loc_zgrid(p-1)%min_cell + n_loc
    loc_zgrid(p)%max_cell = loc_zgrid(p-1)%max_cell + n_loc
    loc_zgrid(p)%shift = shift_z
   end if
   !======================
   loc_xgrid(0)%gmin = x(1)
   ip = loc_xgrid(0)%ng
   n_loc = ip
   loc_xgrid(0)%gmax = x(ip+1)
   loc_xgrid(0)%p_ind(1) = min(shift_x, n_loc)
   loc_xgrid(0)%p_ind(2) = n_loc + loc_xgrid(0)%p_ind(1) - 1
   loc_xgrid(0)%min_cell = 0
   loc_xgrid(0)%max_cell = loc_xgrid(0)%min_cell + n_loc - 1
   loc_xgrid(0)%shift = shift_x
   p = 0
   do i = 0, n_loc + 1
    loc_xg(i, 1, p) = x(i)
    loc_xg(i, 2, p) = xh(i)
    loc_xg(i, 3, p) = dx1(i)
    loc_xg(i, 4, p) = dx1h(i)
   end do

   if (npex > 1) then
    ip = loc_xgrid(0)%ng
    if (npex > 2) then
     do p = 1, npex - 2
      n_loc = loc_xgrid(p - 1)%ng
      loc_xg(0, 1:4, p) = loc_xg(n_loc, 1:4, p - 1)
      n_loc = loc_xgrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_xg(i, 1, p) = x(ii)
       loc_xg(i, 2, p) = xh(ii)
       loc_xg(i, 3, p) = dx1(ii)
       loc_xg(i, 4, p) = dx1h(ii)
      end do
      loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
      ip = ip + n_loc
      loc_xgrid(p)%gmax = x(ip+1)
      loc_xgrid(p)%p_ind(1) = shift_x
      loc_xgrid(p)%p_ind(2) = n_loc + loc_xgrid(p)%p_ind(1) - 1
      loc_xgrid(p)%min_cell = loc_xgrid(p-1)%min_cell + n_loc
      loc_xgrid(p)%max_cell = loc_xgrid(p-1)%max_cell + n_loc
      loc_xgrid(p)%shift = shift_x
     end do
    end if
    p = npex - 1
    n_loc = loc_xgrid(p - 1)%ng
    loc_xg(0, 1:4, p) = loc_xg(n_loc, 1:4, p - 1)
    n_loc = loc_xgrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_xg(i, 1, p) = x(ii)
     loc_xg(i, 2, p) = xh(ii)
     loc_xg(i, 3, p) = dx1(ii)
     loc_xg(i, 4, p) = dx1h(ii)
    end do
    loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
    ip = ip + n_loc
    loc_xgrid(p)%gmax = x(ip+1)
    loc_xgrid(p)%p_ind(1) = shift_x
    loc_xgrid(p)%p_ind(2) = n_loc + loc_xgrid(p)%p_ind(1) - 1
    loc_xgrid(p)%min_cell = loc_xgrid(p-1)%min_cell + n_loc
    loc_xgrid(p)%max_cell = loc_xgrid(p-1)%max_cell + n_loc
    loc_xgrid(p)%shift = shift_x
   end if
  end subroutine
  !======================
  subroutine set_fxgrid(npex, sh)
   integer, intent(in) :: npex, sh
   integer :: i, ii, p, ip, n_loc

   loc_xgrid(0)%gmin = x(1)
   ip = loc_xgrid(0)%ng
   n_loc = ip
   loc_xgrid(0)%gmax = x(ip + 1)
   loc_xgrid(0)%p_ind(1) = min(sh, n_loc)
   loc_xgrid(0)%p_ind(2) = n_loc + loc_xgrid(0)%p_ind(1) - 1
   loc_xgrid(0)%shift = sh

   p = 0
   do i = 0, n_loc + 1
    loc_xg(i, 1, p) = x(i)
    loc_xg(i, 2, p) = xh(i)
    loc_xg(i, 3, p) = dx1(i)
    loc_xg(i, 4, p) = dx1h(i)
   end do

   if (npex > 1) then
    ip = loc_xgrid(0)%ng
    if (npex > 2) then
     do p = 1, npex - 2
      n_loc = loc_xgrid(p - 1)%ng
      loc_xg(0, 1:4, p) = loc_xg(n_loc, 1:4, p - 1)
      n_loc = loc_xgrid(p)%ng
      do i = 1, n_loc + 1
       ii = i + ip
       loc_xg(i, 1, p) = x(ii)
       loc_xg(i, 2, p) = xh(ii)
       loc_xg(i, 3, p) = dx1(ii)
       loc_xg(i, 4, p) = dx1h(ii)
      end do
      loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
      ip = ip + n_loc
      loc_xgrid(p)%gmax = x(ip + 1)
      loc_xgrid(p)%p_ind(1) = sh
      loc_xgrid(p)%p_ind(2) = n_loc + loc_xgrid(p)%p_ind(1) - 1
      loc_xgrid(0)%shift = sh
     end do
    end if
    p = npex - 1
    n_loc = loc_xgrid(p - 1)%ng
    loc_xg(0, 1:4, p) = loc_xg(n_loc, 1:4, p - 1)
    n_loc = loc_xgrid(p)%ng
    do i = 1, n_loc + 1
     ii = i + ip
     loc_xg(i, 1, p) = x(ii)
     loc_xg(i, 2, p) = xh(ii)
     loc_xg(i, 3, p) = dx1(ii)
     loc_xg(i, 4, p) = dx1h(ii)
    end do
    loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
    ip = ip + n_loc
    loc_xgrid(p)%gmax = x(ip + 1)
    loc_xgrid(p)%p_ind(1) = sh
    loc_xgrid(p)%p_ind(2) = n_loc + loc_xgrid(p)%p_ind(1) - 1
    loc_xgrid(0)%shift = sh
   end if
  end subroutine

  subroutine set_str_ind(npey, npez, ndm)
   integer, intent(in) :: npey, npez, ndm
   integer :: p, q, ip(4)

   str_indx(0:npey - 1, 0:npez - 1) = 0
   ip = 0
   if (ndm < 3) then
    do p = 0, npey - 1
     if (str_ygrid%smin > loc_ygrid(p)%gmin) ip(1) = p
     if (str_ygrid%smax >= loc_ygrid(p)%gmin) ip(2) = p
    end do

    p = 0
    do q = 0, ip(1)
     str_indx(q, p) = 1 !selects mpi-tasks with stretched y< 0 up to ys_min
    end do
    do q = ip(2), npey - 1
     str_indx(q, p) = 2 !selects mpi-tasks with stretched y>0 up to ys_max
    end do
    return
   end if
   do p = 0, npey - 1
    if (str_ygrid%smin > loc_ygrid(p)%gmin) ip(1) = p
    if (str_ygrid%smax >= loc_ygrid(p)%gmin) ip(2) = p
   end do
   do p = 0, npez - 1
    if (str_zgrid%smin > loc_zgrid(p)%gmin) ip(3) = p
    if (str_zgrid%smax >= loc_zgrid(p)%gmin) ip(4) = p
   end do

   do p = 0, ip(3)
    str_indx(0:npey - 1, p) = 2
    do q = 0, ip(1)
     str_indx(q, p) = 1
    end do
    do q = ip(2), npey - 1
     str_indx(q, p) = 3
    end do
   end do
   do p = ip(3) + 1, ip(4) - 1
    do q = 0, ip(1)
     str_indx(q, p) = 8
    end do
    do q = ip(2), npey - 1
     str_indx(q, p) = 4
    end do
   end do
   do p = ip(4), npez - 1
    str_indx(0:npey - 1, p) = 6
    do q = 0, ip(1)
     str_indx(q, p) = 7
    end do
    do q = ip(2), npey - 1
     str_indx(q, p) = 5
    end do
   end do
  end subroutine
!
  subroutine select_str_to_ft_grid(npey, npez)

   integer, intent(in) :: npey, npez
   integer :: ip, iy, iz, ii, nloc, loc_nft
   real(dp) :: yy, zz

   loc_nft = 4*loc_yftgrid(0)%ng   !the local size of uniform grid
   do ip = 0, npey - 1     !from negative to positive y coordinate
    nloc = loc_ygrid(ip)%ng     !the local size of stretched grid
    do iy = 1, nloc
     yy = loc_yg(iy, 1, ip)
     do ii = 1, loc_nft
      if (yy < loc_yft(ii, ip)) exit
     end do
     yft_ind(iy, ip) = ii - 1
    end do
   end do
   loc_nft = 4*loc_zftgrid(0)%ng
   do ip = 0, npez - 1     !from negative to positive z coordinate
    nloc = loc_zgrid(ip)%ng
    do iz = 1, nloc
     zz = loc_zg(iz, 1, ip)
     do ii = 1, loc_nft
      if (zz < loc_zft(ii, ip)) exit
     end do
     zft_ind(iz, ip) = ii - 1
    end do
   end do
  end subroutine

  subroutine set_loc_grid_param

   xmn = loc_xgrid(imodx)%gmin
   ymn = loc_ygrid(imody)%gmin
   zmn = loc_zgrid(imodz)%gmin
   ix1 = loc_xgrid(imodx)%p_ind(1)
   ix2 = loc_xgrid(imodx)%p_ind(2)
   jy1 = loc_ygrid(imody)%p_ind(1)
   jy2 = loc_ygrid(imody)%p_ind(2)
   kz1 = loc_zgrid(imodz)%p_ind(1)
   kz2 = loc_zgrid(imodz)%p_ind(2)
   gcx = loc_xgrid(imodx)%shift
   gcy = loc_ygrid(imody)%shift
   gcz = loc_zgrid(imodz)%shift
   n_str = 0
   if (stretch) n_str = str_indx(imody, imodz)

   nyp = loc_ygrid(imody)%p_ind(2) !Ny_loc+2
   nzp = loc_zgrid(imodz)%p_ind(2) !Nz_loc+2
   nxp = loc_xgrid(imodx)%p_ind(2) !Nx_loc+2

  end subroutine
  !====================
  subroutine set_ftgrid(str, npe1, npe2, npe3)
   logical, intent(in) :: str
   integer, intent(in) :: npe1, npe2, npe3
   integer :: n1, n2, n3
   integer :: i, ip
   real (dp) :: wkx, wky, wkz

   n1=nx
   n2=ny
   n3=nz
   if(str)then
    n2=nint(dy_inv*ly_box)
    n3=nint(dz_inv*lz_box)
    if(mod(n2,npe2)>0) n2=n2+npe2-mod(n2,npe2)
    if(mod(n3,npe3)>0) n3=n3+npe3-mod(n3,npe3)
   end if
!============= set grid point numbers in common.param
   n1ft = n1
   n2ft = n2
   n3ft = n3
   n1ft_loc = n1ft/npe1
   n2ft_loc = n2ft/npe2
   n3ft_loc = n3ft/npe3
!======= set global ft (y,z) coorinates
   allocate (yft(n2 + 1))
   allocate (zft(n3 + 1))
   do i = 1, n2 + 1
    yft(i) = dy*real(i - 1 - n2/2, dp)
   end do
   do i = 1, n3 + 1
    zft(i) = dz*real(i - 1 - n3/2, dp)
   end do
!=======  set loca locy_ftgrid loc_zftgrid parameters
   allocate (loc_yftgrid(0:npe2 - 1), loc_zftgrid(0:npe3 - 1))
   allocate (loc_yft(1:4*n2ft_loc, 0:npe2 - 1), loc_zft(1:4*n3ft_loc, 0:npe3 - 1))
   allocate (yft_ind(ny_loc, 0:npe2 - 1))
   allocate (zft_ind(nz_loc, 0:npe3 - 1))
   do ip = 0, npe2 - 1
    loc_yftgrid(ip)%ng = n2ft_loc
   end do
   do ip = 0, npe3 - 1
    loc_zftgrid(ip)%ng = n3ft_loc
   end do
!===================
   call set_ftyzgrid(npe2, npe3)
!===================

   yft_min = loc_yftgrid(imody)%gmin
   zft_min = loc_zftgrid(imodz)%gmin

   call select_str_to_ft_grid(nprocy, nprocz)
   ! exit ii=yft_ind(y_str,pe) index  yft(ii) < y_str(pe) < yft(ii+1)
!=======================
   allocate (aky(n2 + 2, 0:2), akz(n3 + 2, 0:2))
   allocate (sky(n2 + 2, 0:2), skz(n3 + 2, 0:2))
   allocate (ak2y(n2 + 2, 0:2), ak2z(n3 + 2, 0:2), ak2x(n1 + 1, 0:2))
   allocate (akx(1:n1 + 1, 0:2), skx(1:n1 + 1, 0:2))
   akx(:, 0:2) = 0.0
   ak2x(:, 0:2) = 0.0
   aky(:, 0:2) = 0.0
   ak2y(:, 0:2) = 0.0
   akz(:, 0:2) = 0.0
   ak2z(:, 0:2) = 0.0
   skx(:, 0:2) = 0.0
   sky(:, 0:2) = 0.0
   skz(:, 0:2) = 0.0
   !================
   !  Sets wave number grid for all configurations
   !=============================================
   !case(0)  ! staggered k-grid
   wkx = 2.*acos(-1.)/lx_box !lxbox=x(n1+1)-x(1)
   wky = 2.*acos(-1.)/ly_box !lybox=y(n2+1)-y(1)
   wkz = 2.*acos(-1.)/lz_box !lzbox=z(n2+1)-z(1)
   do i = 1, n1/2
    akx(i, 0) = wkx*(real(i, dp) - 0.5)
    skx(i, 0) = 2.*sin(0.5*dx*akx(i, 0))/dx
   end do
   ak2x(1:n1, 0) = akx(1:n1, 0)*akx(1:n1, 0)
   if (n2 > 1) then
    do i = 1, n2/2
     aky(i, 0) = wky*(real(i, dp) - 0.5)
     aky(n2 + 1 - i, 0) = -aky(i, 0)
    end do
    ak2y(1:n2, 0) = aky(1:n2, 0)*aky(1:n2, 0)
    do i = 1, n2
     sky(i, 0) = 2.*sin(0.5*dy*aky(i, 0))/dy
    end do
   end if
   if (n3 > 1) then
    do i = 1, n3/2
     akz(i, 0) = wkz*(real(i, dp) - 0.5)
     akz(n3 + 1 - i, 0) = -akz(i, 0)
    end do
    do i = 1, n3
     skz(i, 0) = 2.*sin(0.5*dz*akz(i, 0))/dz
    end do
    ak2z(1:n3, 0) = akz(1:n3, 0)*akz(1:n3, 0)
   end if

   !case(1)    !standard FT k-grid
   do i = 1, n1/2
    akx(i, 1) = wkx*real(i - 1, dp)
    akx(n1 + 2 - i, 1) = -akx(i, 1)
   end do
   ak2x(1:n1, 1) = akx(1:n1, 1)*akx(1:n1, 1)
   do i = 1, n1 + 1
    skx(i, 1) = 2.*sin(0.5*dx*akx(i, 1))/dx
   end do
   if (n2 > 1) then
    do i = 1, n2/2
     aky(i, 1) = wky*real(i - 1, dp)
     aky(n2 + 2 - i, 1) = -aky(i, 1)
     sky(i, 1) = 2.*sin(0.5*dy*aky(i, 1))/dy
    end do
    ak2y(1:n2, 1) = aky(1:n2, 1)*aky(1:n2, 1)
   end if
   if (n3 > 1) then
    do i = 1, n3/2
     akz(i, 1) = wkz*real(i - 1, dp)
     akz(n3 + 2 - i, 1) = -akz(i, 1)
    end do
    do i = 1, n3
     skz(i, 1) = 2.*sin(0.5*dz*akz(i, 1))/dz
    end do
    ak2z(1:n3, 1) = akz(1:n3, 1)*akz(1:n3, 1)
   end if

   !case(2)  ! for the sine/cosine transform
   wkx = acos(-1.0)/lx_box
   wky = acos(-1.0)/ly_box
   wkz = acos(-1.0)/lz_box
   do i = 1, n1 + 1
    akx(i, 2) = wkx*real(i, dp)
    skx(i, 2) = 2.*sin(0.5*dx*akx(i, 2))/dx
   end do
   if (n2 > 1) then
    do i = 1, n2 + 1
     aky(i, 2) = wky*real(i, dp)
     sky(i, 2) = 2.*sin(0.5*dy*aky(i, 2))/dy
    end do
    ak2y(1:n2, 2) = aky(1:n2, 2)*aky(1:n2, 2)
   end if
   if (n3 > 1) then
    do i = 1, n3 + 1
     akz(i, 2) = wkz*real(i, dp)
     skz(i, 2) = 2.*sin(0.5*dz*akz(i, 2))/dz
    end do
    ak2z(1:n3, 2) = akz(1:n3, 2)*akz(1:n3, 2)
   end if
  end subroutine
  !=============================
 end module

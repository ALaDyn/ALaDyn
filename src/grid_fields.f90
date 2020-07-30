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

 module grid_fields

  use grid_field_param
  use parallel

  implicit none
  integer, parameter, private :: x_parity(6) = [-1, 1, 1, -1, 1, 1]
  integer, parameter, private :: y_parity(6) = [1, -1, 1, 1, -1, 1]
  integer, parameter, private :: z_parity(6) = [1, 1, -1, 1, 1, -1]
  integer, dimension(2, 3), protected, private :: COEFF_2
  integer, dimension(2, 2), protected, private :: COEFF_1
  integer, dimension(2, 1), protected, private :: COEFF_0

 contains

  subroutine trid_der1(a, b, c, b1, c1, an, bn, n, ic1, ic2, ord)
   real(dp), intent(in) :: a, b, c, b1, c1, an, bn
   integer, intent(in) :: n, ic1, ic2, ord
   integer :: k, ic
   real(dp) :: bet
   !==========================
   ! Solves
   ! a*ww(i-1)+b*ww(i)+c*ww(i+1)=u(i), i=2,3,..,n-1
   ! at the first row b1*ww(1)+c1*ww(2)=u(1)
   ! at the n-last row an*ww(n-1)+bn*ww(n)=u(n)
   ! first order boundary clusure
   !===============================
   if (ord > 0) then
    do ic = ic1, ic2
     ww0(1, ic) = ww0(1, ic) + ww0(2, ic)
     ww0(n, ic) = ww0(n, ic) + ww0(n - 1, ic)
    end do
   end if
   !===================
   do ic = ic1, ic2
    ww1(1) = 0.0
    bet = b1
    ww0(1, ic) = ww0(1, ic)/bet
    k = 2
    ww1(k) = c1/bet
    bet = b - a*ww1(k)
    ww0(k, ic) = (ww0(k, ic) - a*ww0(k - 1, ic))/bet
    do k = 3, n - 1
     ww1(k) = c/bet
     bet = b - a*ww1(k)
     ww0(k, ic) = (ww0(k, ic) - a*ww0(k - 1, ic))/bet
    end do
    k = n
    ww1(k) = c/bet
    bet = bn - an*ww1(k)
    ww0(k, ic) = (ww0(k, ic) - an*ww0(k - 1, ic))/bet
    do k = n - 1, 1, -1
     ww0(k, ic) = ww0(k, ic) - ww1(k + 1)*ww0(k + 1, ic)
    end do
   end do
  end subroutine
!=================================
  subroutine unif_to_str_field_interp(unif_field, str_field, ic)

   real(dp), intent(in) :: unif_field(:, :, :)
   real(dp), intent(inout) :: str_field(:, :, :, :)
   integer, intent(in) :: ic
   real(dp) :: shy, shz, shy2, shz2
   integer :: i, ii, j, jj, k, kk
   integer :: jsize, ksize
   !=======================================
   jsize = size(unif_field, 2)
   ksize = size(unif_field, 3)
!=====================
   select case (ndim)
   case (2)
    k = kz1
    kk = 1
    do j = jy1, jy2
     jj = yft_ind(j - 2, imody)
     shy = dy_inv*(loc_yg(j - 2, 1, imody) - loc_yft(jj, imody))
     shy2 = 0.5*shy*shy
     shy = 0.5*shy
     do i = ix1, ix2
      ii = i - 2
      str_field(i, j, k, ic) = unif_field(ii, jj, kk) + &
                               shy*(unif_field(ii, jj + 1, kk) - unif_field(ii, jj - 1, kk)) + &
                               shy2*(unif_field(ii, jj + 1, kk) + unif_field(ii, jj - 1, kk) - 2*unif_field(ii, jj, kk))
     end do
    end do
   case (3)
    do k = kz1, kz2
     kk = zft_ind(k - 2, imodz)
     shz = dz_inv*(loc_zg(k - 2, 1, imodz) - loc_zft(kk, imodz))
     shz2 = 0.5*shz*shz
     shz = 0.5*shz
     do j = jy1, jy2
      jj = yft_ind(j - 2, imody)
      shy = dy_inv*(loc_yg(j - 2, 1, imody) - loc_yft(jj, imody))
      shy2 = 0.5*shy*shy
      shy = 0.5*shy
      do i = ix1, ix2
       ii = i - 2
       str_field(i, j, k, ic) = unif_field(ii, jj, kk) + &
                                shy*(unif_field(ii, jj + 1, kk) - unif_field(ii, jj - 1, kk)) + &
                                shz*(unif_field(ii, jj, kk + 1) - unif_field(ii, jj, kk - 1))
       str_field(i, j, k, ic) = str_field(i, j, k, ic) + &
                                shy2*(unif_field(ii, jj + 1, kk) + unif_field(ii, jj - 1, kk) - 2*unif_field(ii, jj, kk))
       str_field(i, j, k, ic) = str_field(i, j, k, ic) + &
                                shz2*(unif_field(ii, jj, kk + 1) + unif_field(ii, jj, kk - 1) - 2*unif_field(ii, jj, kk))
      end do
     end do
    end do
   end select

  end subroutine

  subroutine enforce_continuity(curr)

   real(dp), intent(inout) :: curr(:, :, :, :)
   real(dp) :: aphy, aphz, shy, shz
   integer :: i, ii, j, k, jj, j01, k01
   !===================== 3D Cartesian
   !Solves DJ_x/Dx =-[DJ_y/Dy+DJ_z/Dz +[Rho^{n+1}-Rho^n]/Dt]=
   ! Eneter curr(1)= Drho, curr(2)=J_y*Dt  curr(3)= J_z*Dt
   !=======================================
   aphy = dy_inv
   aphz = dz_inv
   j01 = jy1
   k01 = kz1
   !shy(3)=Dxi/Dy centered on node y_j
   if (ndim == 1) return
   if (pe0y) then
    j = jy1
    shy = loc_yg(j - 1, 3, imody)*aphy
    do k = kz1, kz2
     do i = ix1, ix2
      curr(i, j, k, 1) = curr(i, j, k, 1) + shy*(curr(i, j + 1, k, 2) - curr(i, &
                                                                             j, k, 2))
     end do
    end do
    j01 = jy1 + 1
   end if
   do k = kz1, kz2
    do j = j01, jy2
     jj = j - 2
     shy = loc_yg(jj, 3, imody)*aphy
     do i = ix1, ix2
      curr(i, j, k, 1) = curr(i, j, k, 1) + shy*(curr(i, j, k, 2) - curr(i, j - &
                                                                         1, k, 2))
     end do
    end do
   end do
   !================ ndim >2
   if (ndim == 3) then
    if (pe0z) then
     k = kz1
     shz = loc_zg(k - 1, 3, imodz)*aphz
     do j = jy1, jy2
      do i = ix1, ix2
       curr(i, j, k, 1) = curr(i, j, k, 1) + shz*(curr(i, j, k + 1, 3) - curr(i &
                                                                              , j, k, 3))
      end do
     end do
     k01 = kz1 + 1
    end if
    do k = k01, kz2
     shz = loc_zg(k - 2, 3, imodz)*aphz
     do j = jy1, jy2
      do i = ix1, ix2
       curr(i, j, k, 1) = curr(i, j, k, 1) + shz*(curr(i, j, k, 3) - curr(i, j &
                                                                          , k - 1, 3))
      end do
     end do
    end do
   end if
   !=============== 1D invertion of first derivative
   ww0(:, :) = 0.0
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix2, ix1, -1
      ii = i - 2
      ww0(ii, 1) = ww0(ii + 1, 1) + dx*curr(i + 1, j, k, 1)
     end do
     do i = ix1, ix2
      ii = i - 2
      curr(i, j, k, 1) = ww0(ii, 1)
     end do
    end do
   end do

  end subroutine

  subroutine field_xadvect(ef, dth, v_adv, ic1, ic2, isch)
   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: dth, v_adv
   integer, intent(in) :: ic1, ic2, isch
   integer :: i1, n1p, i, j, k, ii, ic, ind
   real(dp) :: aphx, aphx_exp, aphx_impl, a, b, c, b1, c1, an, bn
   real(dp), dimension(3), parameter :: RDER = [-3., 4., -1.]
   !=====================
   ! APPLIES also for prlx=.true. (MPI x decomposition)
   !=============================================
   ! Solves Df/Dt=-v_adv*Df/Dx    Df/Dx =[f_{i+1}-f_{i-1}]/(2Dx)
   ! forward advection for v_adv > 0
   ! backward advection for v_adv <0
   ! In comoving system in the Maxwell eqs. enters v_adv <0
   !==================
   !Explicit first order advection scheme
   !          E^{n+1}=(1-aphx_exp*D_x)E^n
   !Semi-implicit advection scheme in x-coordinate
   !          E^n+1=E^n-aphx_impl*[D_xE^n+D_xE^{n+1}]
   !          (1+aphx_impl*D_x)E^{n+1}=(1-aphx_impl*D_x)E^n
   !Fully-implicit advection scheme in x-coordinate
   !          (1+aphx_exp*D_x)E^{n+1}=E^n
   !================================
   aphx = dth*dx_inv
   aphx_exp = 0.5*v_adv*aphx !v_adv*dt/2Dx
   aphx_impl = 0.5*aphx_exp !v_adv*(dt/2Dx)/2

   ind = 1
   b1 = 1.
   c1 = 0.0
   bn = 1.
   an = 0.0
   !bn = 1.-2.*aphx_adv
   !cn = 2.*aphx_adv
   !=====================
   i1 = ix1
   n1p = ix2
   !=========================
   if (pe0x) then
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       ef(i1 - 1, j, k, ic) = ef(i1 + 1, j, k, ic)
      end do
     end do
    end do
   end if
   if (pe1x) then
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       ef(n1p + 1, j, k, ic) = ef(n1p, j, k, ic)
      end do
     end do
    end do
   end if
   !===============================
   select case (isch)
   case (0) !pure explicit
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i1, n1p
        ii = i - 2
        ww0(ii, 1) = ef(i, j, k, ic) - aphx_exp*(ef(i + 1, j, k, ic) - ef(i - 1, j &
                                                                          , k, ic))
       end do
       do i = i1, n1p
        ii = i - 2
        ef(i, j, k, ic) = ww0(ii, 1)
       end do
      end do
     end do
    end do
   case (1) !semi-implicit
    a = -aphx_impl
    b = 1.
    c = -a
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i1, n1p
        ii = i - 2
        ww0(ii, 1) = ef(i, j, k, ic) - aphx_impl*(ef(i + 1, j, k, ic) - ef(i - 1, &
                                                                           j, k, ic))
       end do
       call trid_der1(a, b, c, b1, c1, an, bn, ii, 1, 1, 0)
       do i = i1, n1p
        ii = i - 2
        ef(i, j, k, ic) = ww0(ii, 1)
       end do
      end do
     end do
    end do
   case (2) !fully implicit (1+aphx_exp)*Dx]ef=ef
    a = -aphx_exp
    b = 1.
    c = -a
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i1, n1p
        ii = i - 2
        ww0(ii, 1) = ef(i, j, k, ic)
       end do
       call trid_der1(a, b, c, b1, c1, an, bn, ii, 1, 1, 0)
       do i = i1, n1p
        ii = i - 2
        ef(i, j, k, ic) = ww0(ii, 1)
       end do
      end do
     end do
    end do
   end select
  end subroutine
  !==================================
  subroutine pp_lapl(av, source, ic1, ic2)

   real(dp), intent(inout) :: av(:, :, :, :)
   real(dp), intent(inout) :: source(:, :, :, :)

   integer, intent(in) :: ic1, ic2
   integer :: i, j, k, ic, jj, j01, j02, k01, k02, i01, i02
   real(dp) :: dy2_inv, dz2_inv, cf(2), shy, shz, sphy, smhy, sphz, &
               smhz
   real(dp) :: dy4_inv(2), dz4_inv(2)
   !==========================
   ! is=1 adds  is=-1 subtracts the laaplacian term
   !==========================
   dy2_inv = dy_inv*dy_inv
   dz2_inv = dz_inv*dz_inv
   cf(1) = 1.
   cf(2) = .0
   if (der_ord == 4) then
    cf(1) = 4./3. !1-8*hord_der2/3
    cf(2) = -1./12. !2*(2*hord_der2-1)
   end if
   dy4_inv(1) = cf(1)*dy2_inv
   dy4_inv(2) = cf(2)*dy2_inv
   dz4_inv(1) = cf(1)*dz2_inv
   dz4_inv(2) = cf(2)*dz2_inv
   !===========================
   !Second order derivative. At boundaries D^3[av]=0
   i01 = ix1
   i02 = ix2
   j01 = jy1
   j02 = jy2
   k01 = kz1
   k02 = kz2

   do ic = ic1, ic2
    do k = k01, k02
     do j = j01, j02
      jj = j - 2
      shy = dy4_inv(1)*loc_yg(jj, 3, imody)
      sphy = loc_yg(jj, 4, imody)
      smhy = loc_yg(jj - 1, 4, imody)
      do i = i01, i02
       source(i, j, k, ic) = source(i, j, k, ic) + shy*(sphy*(av(i, j + 1, k, ic) - av(i, j, k, ic)) - &
                                                        smhy*(av(i, j, k, ic) - av(i, j - 1, k, ic)))
      end do
     end do
    end do
   end do
   if (der_ord == 4) then
    do ic = ic1, ic2
     do k = k01, k02
      do j = j01, j02
       jj = j - 2
       shy = dy4_inv(2)*loc_yg(jj, 3, imody)
       sphy = loc_yg(jj + 1, 3, imody)
       smhy = loc_yg(jj - 1, 3, imody)
       do i = i01, i02
        source(i, j, k, ic) = source(i, j, k, ic) + shy*(sphy*(av(i, j + 2, k, ic) - av(i, j, k, ic)) - &
                                                         smhy*(av(i, j, k, ic) - av(i, j - 2, k, ic)))
       end do
      end do
     end do
    end do
   end if
   if (ndim < 3) return
   !====================

   do ic = ic1, ic2
    do k = k01, k02
     jj = k - 2
     shz = dz4_inv(1)*loc_zg(jj, 3, imodz)
     sphz = loc_zg(jj, 4, imodz)
     smhz = loc_zg(jj - 1, 4, imodz)
     do j = j01, j02
      do i = i01, i02
       source(i, j, k, ic) = source(i, j, k, ic) + shz*(sphz*(av(i, j, k + 1, ic) - av(i, j, k, ic)) - &
                                                        smhz*(av(i, j, k, ic) - av(i, j, k - 1, ic)))
      end do
     end do
    end do
   end do
   if (der_ord == 4) then
    do ic = ic1, ic2
     do k = k01, k02
      jj = k - 2
      shz = dz4_inv(2)*loc_zg(jj, 3, imodz)
      sphz = loc_zg(jj + 1, 3, imodz)
      smhz = loc_zg(jj - 1, 3, imodz)
      do j = j01, j02
       do i = i01, i02
        source(i, j, k, ic) = source(i, j, k, ic) + shz*(sphz*(av(i, j, k + 2, ic) - av(i, j, k, ic)) - &
                                                         smhz*(av(i, j, k, ic) - av(i, j, k - 2, ic)))
       end do
      end do
     end do
    end do
   end if
   !======================================
  end subroutine
  !========================
  subroutine env_grad(envg)

   real(dp), intent(inout) :: envg(:, :, :, :)
   integer :: i, j, k, i01, i02, j01, j02, k01, k02
   real(dp) :: ax1, ax2, ay1, ay2, az1, az2, shz, shy, shp, shm
   real(dp), parameter :: a_hcd = 13./12., b_hcd = -1./24.
   !=== second or fourth order central flux derivatives
   !==========================
   ! Enters envg(1)= |A|^2/2 exit grad|A|^2/2

   if (der_ord < 4) then
    ax1 = dx_inv
    ay1 = dy_inv
    az1 = dz_inv
    ax2 = 0.
    ay2 = 0.
    az2 = 0.
   else
    ax1 = dx_inv*a_hcd
    ay1 = dy_inv*a_hcd
    az1 = dz_inv*a_hcd
    ax2 = dx_inv*b_hcd
    ay2 = dy_inv*b_hcd
    az2 = dz_inv*b_hcd
   end if
   i01 = ix1
   i02 = ix2
   j01 = jy1
   j02 = jy2
   k01 = kz1
   k02 = kz2
   !================
   if (xl_bd) then
    i = ix1
    do k = kz1, kz2
     do j = jy1, jy2
      envg(i, j, k, 2) = dx_inv*(envg(i + 1, j, k, 1) - envg(i, j, k, 1)) !at i+1/2
     end do
    end do
    i01 = ix1 + 1
   endif
   if (xr_bd) then
    do k = kz1, kz2
     do j = jy1, jy2
      i = ix2 - 1
      envg(i, j, k, 2) = dx_inv*(envg(i + 1, j, k, 1) - envg(i, j, k, 1))
      envg(i + 1, j, k, 2) = dx_inv*(2.*envg(i, j, k, 1) - 3.*envg(i - 1, j, k, 1) + &
                                     envg(i - 2, j, k, 1))
     end do
    end do
    i02 = ix2 - 2
   endif
   do k = kz1, kz2
    do j = jy1, jy2
     do i = i01, i02
      envg(i, j, k, 2) = ax1*(envg(i + 1, j, k, 1) - envg(i, j, k, 1)) !at i+1/2
     end do
    end do
   end do
   if (der_ord == 4) then
    do k = kz1, kz2
     do j = jy1, jy2
      do i = i01, i02
       envg(i, j, k, 2) = envg(i, j, k, 2) + ax2*(envg(i + 2, j, k, 1) - envg(i + 1, j, k, 1) + &
                                                  envg(i, j, k, 1) - envg(i - 1, j, k, 1))
      end do
     end do
    end do
   end if
   if (yr_bd) then
    do k = kz1, kz2
     j = jy2
     shy = loc_yg(j - 2, 4, imody)*dy_inv
     do i = ix1, ix2
      envg(i, j, k, 3) = shy*(2.*envg(i, j, k, 1) - 3.*envg(i, j - 1, k, 1) + envg(i &
                                                                                   , j - 2, k, 1))
     end do
     j = jy2 - 1
     shy = loc_yg(j - 2, 4, imody)*dy_inv
     do i = ix1, ix2
      envg(i, j, k, 3) = shy*(envg(i, j + 1, k, 1) - envg(i, j, k, 1))
     end do
    end do
    j02 = jy2 - 2
   end if
   !===================
   if (yl_bd) then
    j = jy1
    shy = loc_yg(j - 2, 4, imody)*dy_inv
    do k = kz1, kz2
     do i = ix1, ix2
      envg(i, j, k, 3) = shy*(envg(i, j + 1, k, 1) - envg(i, j, k, 1))
     end do
    end do
    j01 = jy1 + 1
   end if
   do k = kz1, kz2
    do j = j01, j02
     shy = loc_yg(j - 2, 4, imody)*ay1
     do i = ix1, ix2
      envg(i, j, k, 3) = shy*(envg(i, j + 1, k, 1) - envg(i, j, k, 1))
     end do
    end do
   end do
   if (der_ord == 4) then
    do k = kz1, kz2
     do j = j01, j02
      shp = loc_yg(j - 1, 4, imody)*ay2
      shm = loc_yg(j - 3, 4, imody)*ay2
      do i = ix1, ix2
       envg(i, j, k, 3) = envg(i, j, k, 3) + shp*(envg(i, j + 2, k, 1) - envg(i, j + 1, k, 1)) + &
                          shm*(envg(i, j, k, 1) - envg(i, j - 1, k, 1))
      end do
     end do
    end do
   end if
   if (ndim == 2) return
   if (zr_bd) then
    k = kz2
    shz = loc_zg(k - 2, 4, imodz)*dz_inv
    do j = jy1, jy2
     do i = ix1, ix2
      envg(i, j, k + 1, 1) = 2.*envg(i, j, k, 1) - envg(i, j, k - 1, 1)
      envg(i, j, k, 4) = shz*(envg(i, j, k + 1, 1) - envg(i, j, k, 1))
     end do
    end do
    k02 = kz2 - 1
   end if
   if (zl_bd) then
    k = kz1
    shz = loc_zg(k - 2, 4, imodz)*dz_inv
    do j = jy1, jy2
     do i = ix1, ix2
      envg(i, j, k - 1, 1) = 2.*envg(i, j, k, 1) - envg(i, j, k + 1, 1)
      envg(i, j, k, 4) = shz*(envg(i, j, k + 1, 1) - envg(i, j, k, 1))
     end do
    end do
    k01 = kz1 + 1
   end if
   !==================
   do k = k01, k02
    shz = loc_zg(k - 2, 4, imodz)*az1
    do j = jy1, jy2
     do i = ix1, ix2
      envg(i, j, k, 4) = shz*(envg(i, j, k + 1, 1) - envg(i, j, k, 1))
     end do
    end do
   end do
   if (der_ord == 4) then
    do k = k01, k02
     shp = loc_zg(k - 1, 4, imodz)*az2
     shm = loc_zg(k - 3, 4, imodz)*az2
     do j = jy1, jy2
      do i = ix1, ix2
       envg(i, j, k, 4) = envg(i, j, k, 4) + shp*(envg(i, j, k + 2, 1) - envg(i, j, k + 1, 1)) + &
                          shm*(envg(i, j, k, 1) - envg(i, j, k - 1, 1))
      end do
     end do
    end do
   end if
  end subroutine
  !====================================
  subroutine env_maxw_solve(curr, evf, om0, dtl)
   real(dp), intent(inout) :: curr(:, :, :, :), evf(:, :, :, :)
   real(dp), intent(in) :: om0, dtl
   integer :: i, j, k, ic
   real(dp) :: dt2, dx1_inv, dhx1_inv, aph_opt(2)
   real(dp) :: kfact, k2_fact, skfact
   real(dp), dimension(0:2), parameter :: LDER = [1.0, -4.0, 3.0]
   !==========================
   ! EXPLICIT INTEGRATION of Maxwell ENVELOPE EVOLUTION EQUATION
   ! See: D.Terzani P. Londrillo " A fast and accurate numerical
   ! implementation of the envelope model for laser–plasma dynamics "
   !         CPC 2019
   !============================
   dt2 = dtl*dtl
   !khfact=2.*sin(0.5*om0*dt_loc)/dt_loc
   !kh2_fact=khfact*khfact
   !khfact=2.*dhx*sin(0.5*om0*dx)
   !kh2_sfact=khfact*khfact

   !kfact=sin(om0*dt_loc)
   kfact = om0*dtl
   k2_fact = 1./(1.+kfact*kfact)
   skfact = om0
   !skfact=dhx*sin(om0*dx)
   dx1_inv = skfact*dx_inv
   dhx1_inv = 2.*dx1_inv
   aph_opt(1) = 1.
   aph_opt(2) = 0.
   if (der_ord == 3) then
    aph_opt(1) = dx1_inv*opt_der1
    aph_opt(2) = dx1_inv*0.5*(1.-opt_der1)
   end if
   ic = 2
   !========Enter  jc(1:2)= - omp2*<q^2*chi*env(1:2)
   !                        chi <q^2*wgh*n/gam_p> >0
   ! Computes the full Laplacian of A^{n}=env(1:2) components
   !========and adds to  jc(1:2)
   call potential_lapl(evf, curr, 1, ic)
   !=====================
   ! =>   jc(1:2)=[D^2-omp^2*chi]A= S(A);
   !=================
   !  Computes D_{x} centered first derivatives of A and adds to S(A)
   call first_ader
   !S_R => S_R -2*k0[D_xA_I]
   !S_I => S_I +2*k0[D_xA_R]
   !
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      curr(i, j, k, 1) = dt2*curr(i, j, k, 1) + 2.*evf(i, j, k, 1) - &
                         evf(i, j, k, 3) + kfact*evf(i, j, k, 4)
      curr(i, j, k, 2) = dt2*curr(i, j, k, 2) + 2.*evf(i, j, k, 2) - &
                         evf(i, j, k, 4) - kfact*evf(i, j, k, 3)
     end do
    end do
   end do

   !====================
   !curr(1)=F_R=dt2*S_R+2*A_R^n-A_R^{n-1}-kfact*A_I^{n-1}
   !curr(2)=F_I=dt2*S_I+2*A_I^n-A_I^{n-1}+kfact*A_R^{n-1}
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      evf(i, j, k, 3) = evf(i, j, k, 1) !A^{n}=> A^{n-1}
      evf(i, j, k, 4) = evf(i, j, k, 2)
      evf(i, j, k, 1) = k2_fact*(curr(i, j, k, 1) - kfact*curr(i, j, k, 2))
      evf(i, j, k, 2) = k2_fact*(curr(i, j, k, 2) + kfact*curr(i, j, k, 1))
     end do
    end do
   end do
  contains
   subroutine first_ader
    !============
    integer :: i01, i02
    ! explicit second order [-2isin(k0dx)*Dx]A and add to S(A)
    i01 = ix1
    i02 = ix2
    if (der_ord < 3) then
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i01, i02
        curr(i, j, k, 1) = curr(i, j, k, 1) - dx1_inv*(evf(i + 1, j, k, 2) - &
                                                       evf(i - 1, j, k, 2))
        curr(i, j, k, 2) = curr(i, j, k, 2) + dx1_inv*(evf(i + 1, j, k, 1) - &
                                                       evf(i - 1, j, k, 1))
       end do
      end do
     end do
    else
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i01, i02
        curr(i, j, k, 1) = curr(i, j, k, 1) - aph_opt(1)*(evf(i + 1, j, k, 2) - &
                                                          evf(i - 1, j, k, 2)) - &
                           aph_opt(2)*(evf(i + 2, j, k, 2) - &
                                       evf(i - 2, j, k, 2))
        curr(i, j, k, 2) = curr(i, j, k, 2) + aph_opt(1)*(evf(i + 1, j, k, 1) - &
                                                          evf(i - 1, j, k, 1)) + &
                           aph_opt(2)*(evf(i + 2, j, k, 1) - &
                                       evf(i - 2, j, k, 1))
       end do
      end do
     end do
    end if
   end subroutine

  end subroutine
  !==================================
  subroutine env_lpf_solve(curr, evf, ib, om0, dtl)
   real(dp), intent(inout) :: curr(:, :, :, :), evf(:, :, :, :)
   integer, intent(in) :: ib
   real(dp), intent(in) :: om0, dtl
   integer :: i, j, k, ii, ic, ic1, n1
   real(dp) :: dhx, dx1_inv, om2, aph1, dx_norm, dx2_norm
   real(dp) :: adv, an, bn, der2_norm
   !==========================
   ! EXPLICIT INTEGRATION of REDUCED ENVELOPE FIELD SOLVER
   !============================
   ! Fourth order first derivative
   ! D_xu= 2/3[u_{i+1}-u_{i-1}]- [u_{i+2}-u_{i-2}]/12
   !====================
   dhx = dx_inv
   om2 = om0*om0
   dx1_inv = 0.5*dx_inv
   aph1 = dx1_inv
   n1 = ix2 + 1 - ix1
   dx_norm = dhx/om0
   dx2_norm = dx_norm*dx_norm
   der2_norm = 0.25*dx2_norm
   !========Enter  jc(1:2)= -om2*<q^2*chi*env(1:2)
   !        chi <q^2*wgh*n/gam_p> >0
   ! Computes the transverse Laplacian of A^{n}=env(1:2) components
   !========and adds to jc(1:2)
   ic = 2
   call pp_lapl(evf, curr, 1, 2)
   !=====================
   do ic = 1, 2
    do k = kz1, kz2
     do j = jy1, jy1
      do i = ix1, ix2
       curr(i, j, k, ic) = -dtl*curr(i, j, k, ic)
      end do
     end do
    end do
   end do
   !=======================================================
   ! =>   jc(1:2)=2*Delta t*S(A)=-dt*[D^2_{pp}-omp^2*chi]A;
   !=================
   !  Computes D_{xi} centered first derivatives of S(A)
   !      ww0(1)= k0*S(A_I) + D_xi[A_R]= F_R
   !      ww0(2)= -k0*S(A_R) + D_xi[A_I]= F_I
   !====================
   call first_der
   !curr(1)=F^R
   !curr(2)=F^I
   !==================
   ! The M operator M=[k0*k0+D_xD_x]X = F   X=DA/Dtau

   !   Explicit inversion
   !M^{-1}=([1-Dx_norm^2]F)/k0*k0
   !===============
   call explicit_mat_inv
   !curr=M^{-1}F
   if (ib > 0) then !fixed coordinate system (x,t)
    !=======================
    !(1+Dt*D_x)A^{n+1}=(1-Dt*D_x)A^{n-1}+ M^{-1}F
    !==================================
    select case (ib) !ib=der-1
    case (1)
     !================= Explicit second order
     adv = dtl*dhx !cfl=dt/dx
     do ic = 1, 2
      ic1 = ic + 2
      do k = kz1, kz2
       do j = jy1, jy2
        do i = ix1, ix2
         curr(i, j, k, ic) = curr(i, j, k, ic) + evf(i, j, k, ic1) - &
                             adv*(evf(i + 1, j, k, ic) - evf(i - 1, j, k, ic))
        end do
        do i = ix1, ix2
         evf(i, j, k, ic1) = evf(i, j, k, ic)
         evf(i, j, k, ic) = curr(i, j, k, ic)
        end do
       end do
      end do
     end do
    case (2) !Explicit  optimized
     !=======================
     ! u^{n+1}=u^{n-1}+adv*(u_{i+1}-u_{i-1})+0.5*adv*(
     !adv=dt_loc*dhx     !cfl=dt/dx
     !========================================
     adv = dtl*dhx
     an = (4.-adv*adv)/3.
     bn = 0.5*adv*(1.-an)
     an = an*adv
     do ic = 1, 2
      ic1 = ic + 2
      do k = kz1, kz2
       do j = jy1, jy2
        do i = ix1, ix2
         curr(i, j, k, ic) = curr(i, j, k, ic) + evf(i, j, k, ic1) - &
                             an*(evf(i + 1, j, k, ic) - evf(i - 1, j, k, ic)) - &
                             bn*(evf(i + 2, j, k, ic) - evf(i - 2, j, k, ic))
        end do
        do i = ix1, ix2
         evf(i, j, k, ic1) = evf(i, j, k, ic)
         evf(i, j, k, ic) = curr(i, j, k, ic)
        end do
       end do
      end do
     end do
    end select
   else !ib=0 comoving coordinate system
    do ic = 1, 2
     ic1 = ic + 2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        curr(i, j, k, ic) = curr(i, j, k, ic) + evf(i, j, k, ic1) !Curr=A^{n-1}+curr
        evf(i, j, k, ic1) = evf(i, j, k, ic) !A^{n-1}=> A^n
        evf(i, j, k, ic) = curr(i, j, k, ic) !A^{n+1}=curr
       end do
      end do
     end do
    end do
   end if
  contains
   subroutine first_der
    !============
    ! explicit second order

    do k = kz1, kz2
     do j = jy1, jy2
      do i = ix1, ix2
       ii = i - 2
       ww0(ii, 1) = om0*curr(i, j, k, 2) + dx1_inv*(curr(i + 1, j, k, 1) - curr &
                                                    (i - 1, j, k, 1))
       ww0(ii, 2) = -om0*curr(i, j, k, 1) + dx1_inv*(curr(i + 1, j, k, 2) - &
                                                     curr(i - 1, j, k, 2))
      end do
      do i = ix1, ix2
       ii = i - 2
       curr(i, j, k, 1) = ww0(ii, 1)
       curr(i, j, k, 2) = ww0(ii, 2)
      end do
     end do
    end do
   end subroutine

   !============================
   subroutine explicit_mat_inv
    integer :: iic
    !================== Uses three-point numerical secon derivative
    do iic = 1, 2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        ii = i - 2
        ww0(ii, 1) = dx2_norm*(curr(i + 1, j, k, iic) - 2.*curr(i, j, k, iic) + curr(i &
                                                                                     - 1, j, k, iic))
       end do
       do i = ix1, ix2
        ii = i - 2
        curr(i, j, k, iic) = (curr(i, j, k, iic) - ww0(ii, 1))/om2
       end do
      end do
     end do
    end do
   end subroutine
   !=======================
  end subroutine
  !========================
  ! END ENV SECTION
  !========== LASER FIELDS SECTION
  !            (E,B) BC in open boundaries (lowest order Yee method
  !==========================================
  subroutine env_bds(ef, ptrght, ptlft, init_ic, end_ic)
   !! Boundary conditions for the envelope field.
   !! Empirically set to be continuous with continuous first derivative.

   real(dp), intent(inout) :: ef(:, :, :, :)
   integer, intent(in) :: ptlft, ptrght
   integer, optional, intent(in) :: init_ic, end_ic

   real(dp) :: shx, shy, shz, smy, smz, alpha
   integer :: i, j, k, iic, i1, i2, j1, j2, k1, k2, point
   integer :: comp1, comp2
   integer, dimension(1, 2):: COEFF
   integer :: stenc

   COEFF_2(1, :) = [3, -3, 1]
   COEFF_2(2, :) = [6, -8, 3]
   COEFF_1(1, :) = [2, -1]
   COEFF_1(2, :) = [3, -2]
   COEFF_0(1, :) = 1
   COEFF_0(2, :) = 1

   comp1 = 1
   comp2 = 2

   if (present(init_ic)) then
    comp1 = init_ic
   endif
   if (present(end_ic)) then
    comp2 = end_ic
   endif
   j1 = jy1
   j2 = jy2
   k1 = kz1
   k2 = kz2
   i1 = ix1
   i2 = ix2

   shx = dx_inv
   stenc = 1
   COEFF = TRANSPOSE(COEFF_0)

   if (xl_bd) then
    if (ibx == 0) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j1, j2
        do point = ptlft, 1, -1
         i = i1 - point
         ef(i, j, k, iic) = DOT_PRODUCT(COEFF(1:stenc, point), ef(i1:(i1 + stenc - 1), j, k, iic))
        end do
       end do
      end do
     end do
    end if
    if (ibx == 1) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j1, j2
        do i = i1 - ptlft, i1 - 1
         ef(i, j, k, iic) = x_parity(iic)*ef(2*i1 - i, j, k, iic)
        end do
       end do
      end do
     end do
    end if
    i1 = i1 - ptlft
   end if

   if (xr_bd) then
    if (ibx == 0) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j1, j2
        do point = 1, ptrght
         i = i2 + point
         ef(i, j, k, iic) = DOT_PRODUCT(COEFF(1:stenc, point), ef(i2:(i2 - stenc + 1), j, k, iic))
        end do
       end do
      end do
     end do
    end if
    if (ibx == 1) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j1, j2
        do i = i2 + 1, i2 + ptrght
         ef(i, j, k, iic) = x_parity(iic)*ef(2*i2 - i, j, k, iic)
        end do
       end do
      end do
     end do
    end if
    i2 = i2 + ptrght
   end if

   if (ndim < 2) return

   if (yl_bd) then
    if (iby == 0) then
     do j = j1 - ptlft, j1 - 1
      shy = loc_yg(j1 - 2, 4, imody)
      smy = loc_yg(j1 - 1, 4, imody)
      alpha = shy/smy
      ef(i1:i2, j, k1:k2, comp1:comp2) = alpha*(ef(i1:i2, j1 + 2, k1:k2, comp1:comp2) - &
                                                ef(i1:i2, j1 + 1, k1:k2, comp1:comp2)) - &
                                         2*ef(i1:i2, j1 + 1, k1:k2, comp1:comp2) + &
                                         3*ef(i1:i2, j1, k1:k2, comp1:comp2)
     end do
    end if
    if (iby == 1) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j1 - ptlft, j1 - 1
        do i = i1, i2
         ef(i, j, k, iic) = y_parity(iic)*ef(i, 2*j1 - j, k, iic)
        end do
       end do
      end do
     end do
    end if
    j1 = j1 - ptlft
   end if

   if (yr_bd) then
    if (iby == 0) then
     do j = j2 + 1, j2 + ptrght
      shy = loc_yg(j2 - 1, 4, imody)
      smy = loc_yg(j2 - 2, 4, imody)
      alpha = shy/smy
      ef(i1:i2, j, k1:k2, comp1:comp2) = alpha*(ef(i1:i2, j2 - 2, k1:k2, comp1:comp2) - &
                                                ef(i1:i2, j2 - 1, k1:k2, comp1:comp2)) - &
                                         2*ef(i1:i2, j2 - 1, k1:k2, comp1:comp2) + &
                                         3*ef(i1:i2, j2, k1:k2, comp1:comp2)
     end do
    end if
    if (iby == 1) then
     do iic = comp1, comp2
      do k = k1, k2
       do j = j2 + 1, j2 + ptrght
        do i = i1, i2
         ef(i, j, k, iic) = y_parity(iic)*ef(i, 2*j2 - j, k, iic)
        end do
       end do
      end do
     end do
    end if
    j2 = j2 + ptrght
   end if

   if (ndim < 3) return

   if (zl_bd) then
    if (ibz == 0) then
     do k = k1 - ptlft, k1 - 1
      shz = loc_zg(k1 - 2, 4, imodz)
      smz = loc_zg(k1 - 1, 4, imodz)
      alpha = shz/smz
      ef(i1:i2, j1:j2, k, comp1:comp2) = alpha*(ef(i1:i2, j1:j2, k1 + 2, comp1:comp2) - &
                                                ef(i1:i2, j1:j2, k1 + 1, comp1:comp2)) - &
                                         2*ef(i1:i2, j1:j2, k1 + 1, comp1:comp2) + &
                                         3*ef(i1:i2, j1:j2, k1, comp1:comp2)
     end do
    end if
    if (ibz == 1) then
     do iic = comp1, comp2
      do k = k1 - ptlft, k1 - 1
       do j = j1, j2
        do i = i1, i2
         ef(i, j, k, iic) = z_parity(iic)*ef(i, j, 2*k1 - k, iic)
        end do
       end do
      end do
     end do
    end if
    k1 = k1 - ptlft
   end if

   if (zr_bd) then
    if (ibz == 0) then
     do k = k2 + 1, k2 + ptrght
      shz = loc_zg(k2 - 1, 4, imodz)
      smz = loc_zg(k2 - 2, 4, imodz)
      alpha = shz/smz
      ef(i1:i2, j1:j2, k, comp1:comp2) = alpha*(ef(i1:i2, j1:j2, k2 - 2, comp1:comp2) - &
                                                ef(i1:i2, j1:j2, k2 - 1, comp1:comp2)) - &
                                         2*ef(i1:i2, j1:j2, k2 - 1, comp1:comp2) + &
                                         3*ef(i1:i2, j1:j2, k2, comp1:comp2)
     end do
    end if
    if (ibz == 1) then
     do iic = comp1, comp2
      do k = k2 + 1, k2 + ptrght
       do j = j1, j2
        do i = i1, i2
         ef(i, j, k, iic) = z_parity(iic)*ef(i, j, 2*k2 - k, iic)
        end do
       end do
      end do
     end do
    end if
    k2 = k2 + ptrght
   end if

  end subroutine

  subroutine bf_bds(ef, dtl, imbd)

   real(dp), intent(inout) :: ef(:, :, :, :)
   integer, intent(in) :: imbd
   real(dp), intent(in) :: dtl

   integer :: i, j, k, ii
   real(dp) :: aphx, aphy, aphz

   !=================
   ! Enter bf(4:6)=[Bx,By,Bz]
   !============================
   !========= Engquist-Majda ABC (=>> Mur) =====================
   !===============
   ! Ey+Bz are right-moving
   !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ey+Bz)=0
   ! first order p=1 Bz=-Ey at x=0 and equal time
   ! Ey-Bz are left-moving
   !at x=L minim. reflection (d/dt+d/dx)^{p-1}(Ey-Bz)=0
   ! first order p=1 Bz=Ey at x=L and equal time
   !============================
   ! B[i1,n1p]=> extended to [i1-1,n1p]
   ! boundaries for E_t=rotB
   !========================
   ! aphx centered as Ey at ii=1
   ii = 1
   if (xl_bd) then
    if (ibx < 2) then
     aphx = loc_xg(1, 3, imodx)*dx_inv*dtl
     do k = kz1, kz2
      do j = jy1, jy2
       ef(ix1 - 1, j, k, nfield) = -(2.*ef(ix1, j, k, 2) + (1.-aphx)*ef(ix1, j, k &
                                                                        , nfield))/(1.+aphx)
      end do
     end do
     if (nfield > 3) then
      !==========================
      !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ez-By)=0
      ! first order p=1 By=Ez at x=0 and equal time
      !==========================
      do k = kz1, kz2
       do j = jy1, jy2
        ef(ix1 - 1, j, k, 5) = (2.*ef(ix1, j, k, 3) - (1.-aphx)*ef(ix1, j, k, 5))/ &
                               (1.+aphx)
       end do
      end do
     end if
    end if
   end if
   if (ndim < 2) return
   !------------------------------------
   !++++++++++++++++++++++++++++++++++++++ (Bz,Ex)
   !at y=-Ly minim. reflection (d/dt-d/dy)^{p-1}(Ex-Bz)=0
   ! first order p=1 Bz=Ex at y=-Ly and equal time
   !========================
   !==============================
   ! aphy centered as Ex j=1 (the Bz derivative)
   ii = 1
   if (iby < 2) then
    if (yl_bd) then
     select case (imbd)
     case (0)
      aphy = loc_yg(ii, 3, imody)*dy_inv*dtl
      do k = kz1, kz2
       do i = ix1, ix2
        ef(i, jy1 - 1, k, nfield) = 2.*ef(i, jy1, k, 1) - &
                                    (1.-aphy)*ef(i, jy1, k, nfield)
        ef(i, jy1 - 1, k, nfield) = ef(i, jy1 - 1, k, nfield)/(1.+aphy)
       end do
      end do
      if (nfield > 3) then
       !================================== (Bx,Ez)
       !at y=-Ly minim. reflection (d/dt-d/dy)^{p-1}(Ez+Bx)=0
       ! first order p=1 Bx=-Ez at y=-Ly and equal time
       !==========================================
       do k = kz1, kz2
        do i = ix1, ix2
         ef(i, jy1 - 1, k, 4) = -2.*ef(i, jy1, k, 3) - &
                                (1.-aphy)*ef(i, jy1, k, 4)
         ef(i, jy1 - 1, k, 4) = ef(i, jy1 - 1, k, 4)/(1.+aphy)
        end do
       end do
      end if
     case (1) !symmetric bds for (Bz,Bx) at ymin
      do k = kz1, kz2
       do i = ix1, ix2
        ef(i, jy1 - 1, k, nfield) = ef(i, jy1, k, nfield)
        !ef(i,j1-1,k,nfield)=2.*ef(i,j1,k,nfield)-ef(i,j1+1,k,nfield)
       end do
      end do
      if (nfield > 3) then
       do k = kz1, kz2
        do i = ix1, ix2
         ef(i, jy1 - 1, k, 4) = ef(i, jy1, k, 4)
         !ef(i,j1-1,k,4)=2.*ef(i,j1,k,4)-ef(i,j1+1,k,4)
        end do
       end do
      end if
     end select
    end if
   end if
   if (ndim < 3) return
   !at z=-Lz minim. reflection (d/dt-d/dz)^{p-1}(Bx-Ey)=0
   ! first order p=1 Bx=Ey at z=-Lz and equal time
   !at z=-Lz minim. reflection (d/dt-d/dz)^{p-1}(By+Ex)=0
   ! first order p=1 By=-Ex at z=-Lz and equal time
   !==============================
   ii = 1
   if (ibz < 2) then
    if (zl_bd) then
     select case (imbd)
     case (0)
      aphz = loc_zg(ii, 3, imodz)*dz_inv*dtl
      do j = jy1, jy2
       do i = ix1, ix2
        ef(i, j, kz1 - 1, 4) = 2.*ef(i, j, kz1, 2) - &
                               (1.-aphz)*ef(i, j, kz1, 4)
        ef(i, j, kz1 - 1, 4) = ef(i, j, kz1 - 1, 4)/(1.+aphz)
        ef(i, j, kz1 - 1, 5) = -2.*ef(i, j, kz1, 1) - &
                               (1.-aphz)*ef(i, j, kz1, 5)
        ef(i, j, kz1 - 1, 5) = ef(i, j, kz1 - 1, 5)/(1.+aphz)
       end do
      end do
     case (1) !symmetric bds for (Nx,By) at zmin
      do j = jy1, jy2
       do i = ix1, ix2
        ef(i, j, kz1 - 1, 4) = ef(i, j, kz1, 4)
        ef(i, j, kz1 - 1, 5) = ef(i, j, kz1, 5)
       end do
      end do
     end select
    end if
   end if
  end subroutine
  !====================================
  subroutine ef_bds(ef, dtl, imbd)
   real(dp), intent(inout) :: ef(:, :, :, :)
   integer, intent(in) :: imbd
   real(dp), intent(in) :: dtl
   integer :: i, j, k, ii
   real(dp) :: aphx, aphy, aphz

   aphx = 1
   aphy = 1
   aphz = 1

   ! Enter ebf(1:3)=[Ex,Ey,Ez]
   ! DATA: ef[1:n1p][1:n2p+1][1:n3p+1] bds are on the right
   !===============
   ! to be used to advance B_t=-rot(E)
   !========= Engquist-Majda ABC (=>> Mur) =====================
   !===============
   ! Ey+Bz are right-moving, Ey-Bz left-moving
   !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ey+Bz)=0
   ! first order p=1 Ey=-Bz at x=0
   !at x=Lx minim. reflection (d/dt+d/dx)^{p-1}(Ey-Bz)=0
   ! first order p=1 Ey=Bz at x=L and equal time level
   !=====================
   ! aphx centered as Bz nx+1/2
   if (ibx < 2) then
    if (xr_bd) then
     ii = ix2 - 2
     select case (ibx)
     case (0)
      aphx = loc_xg(ii, 4, imodx)*dx_inv*dtl
      do k = kz1, kz2
       do j = jy1, jy2
        ef(ix2 + 1, j, k, 2) = (2.*ef(ix2, j, k, nfield) - (1.-aphx)*ef(ix2, j, k &
                                                                        , 2))/(1.+aphx)
       end do
      end do
     case (1) !reflecting  only on the right boundary: (Ey,Ez, By,Bz) symmetric (continuous)
      do k = kz1, kz2
       do j = jy1, jy2
        ef(ix2 + 1, j, k, 2) = ef(ix2, j, k, 2)
       end do
      end do
     end select
     if (nfield > 3) then
      !====================
      !at x=Lx minim. reflection (d/dt+d/dx)^{p-1}(Ez+By)=0
      ! first order p=1 Ez=-Bz at x=L and equal time level
      !===========================
      select case (ibx)
      case (0)
       do k = kz1, kz2
        do j = jy1, jy2
         ef(ix2 + 1, j, k, 3) = -(2.*ef(ix2, j, k, 5) + (1.-aphx)*ef(ix2, j, k, 3) &
                                  )/(1.+aphx)
        end do
       end do
      case (1) !reflecting
       do k = kz1, kz2
        do j = jy1, jy2
         ef(ix2 + 1, j, k, 3) = ef(ix2, j, k, 3)
        end do
       end do
      end select
     end if
    end if
   end if
   !===========================
   if (ndim < 2) return
   !=========================== (Bz,Ex)
   !at y=Ly minim. reflection (d/dt+d/dy)^{p-1}(Ex+Bz)=0
   ! first order p=1 Ex=-Bz at y=Ly and equal time level
   !========================
   ! aphy centered as Bz field ny+1/2
   if (iby < 2) then
    if (yr_bd) then
     select case (imbd)
     case (0)
      ii = jy2 - 2
      aphy = loc_yg(ii, 4, imody)*dy_inv*dtl
      do k = kz1, kz2
       do i = ix1, ix2
        ef(i, jy2 + 1, k, 1) = -(2.*ef(i, jy2, k, nfield) + (1.-aphy)*ef(i, jy2, &
                                                                         k, 1))/(1.+aphy)
       end do
      end do
      if (nfield > 3) then
       !====================== (Bz,Ex)
       !at y=Ly minim. reflection (d/dt+d/dy)^{p-1}(Ez-Bx)=0
       ! first order p=1 Ez=Bx at y=Ly and equal time level
       !================================
       do k = kz1, kz2
        do i = ix1, ix2
         ef(i, jy2 + 1, k, 3) = (2.*ef(i, jy2, k, 4) - (1.-aphy)*ef(i, jy2, k, 3)) &
                                /(1.+aphy)
        end do
       end do
      end if
     case (1) !symmetric bds for (Ex,Ez) at ymax boundary
      do k = kz1, kz2
       do i = ix1, ix2
        ef(i, ix2 + 1, k, 1) = ef(i, ix2, k, 1)
        !ef(i,n2p+1,k,1)=2.*ef(i,n2p,k,1)-ef(i,n2p-1,k,1)
       end do
      end do
      if (nfield > 3) then
       do k = kz1, kz2
        do i = ix1, ix2
         ef(i, ix2 + 1, k, 3) = ef(i, ix2, k, 3)
         !ef(i,n2p+1,k,3)=2.*ef(i,n2p,k,3)-ef(i,n2p-1,k,3)
        end do
       end do
      end if
     end select
    end if
   end if
   !==============================
   if (ndim < 3) return
   !==============================
   !at z=Lz minim. reflection
   ! (d/dt+d/dz)^{p-1}(Ex-By)=0
   ! first order p=1 Ex=By at z=Lz and equal time level
   ! (d/dt+d/dz)^{p-1}(Ey+Bx)=0
   ! first order p=1 Ey=-Bx at z=Lz and equal time level
   !================================
   !========================================
   ! aphz centered as Bx,By at nz+1/2
   if (ibz < 2) then
    if (zr_bd) then
     select case (imbd)
     case (0)
      ii = loc_zgrid(imodz)%ng != kz2-2
      aphz = loc_zg(ii, 4, imodz)*dz_inv*dtl
      do j = jy1, jy2
       do i = ix1, ix2
        ef(i, j, kz2 + 1, 1) = (2.*ef(i, j, kz2, 5) - (1.-aphz)*ef(i, j, kz2, 1))/ &
                               (1.+aphz)
        ef(i, j, kz2 + 1, 2) = -(2.*ef(i, j, kz2, 4) + (1.-aphz)*ef(i, j, kz2, 2)) &
                               /(1.+aphz)
       end do
      end do
     case (1) !symmetric bds for (Ex,Ey) at zmax boundary
      do j = jy1, jy2
       do i = ix1, ix2
        ef(i, j, kz2 + 1, 1) = ef(i, j, kz2, 1)
        ef(i, j, kz2 + 1, 2) = ef(i, j, kz2, 2)
       end do
      end do
     end select
    end if
   end if
  end subroutine
  !=========================================
  subroutine potential_lapl(apf, curr, ic1, ic2)
   real(dp), intent(inout) :: apf(:, :, :, :), curr(:, :, :, :)

   integer, intent(in) :: ic1, ic2
   integer :: i, j, k, ic, i01, i02
   real(dp) :: dx2, cf(2), dx4(2)
   !Computes the Laplacian(apf) and accumulates on the source array curr
   !                 curr=laplcian(apf)+curr
   !========================================
   dx2 = dx_inv*dx_inv !1/(dx*dx)
   i01 = ix1
   i02 = ix2
   !============= ALL FIELDS ic=ic1,ic2
   cf(1) = 1.
   cf(2) = 0.0
   ! Holds opt-second order or fourth order
   ! for second derivative with:
   ! dord=3  hord_der2=-(1-nu*nu)/12  dord=4 hord_der2=-1/12
   if (der_ord > 2) then
    cf(1) = 1.-4.*hord_der2
    cf(2) = hord_der2
   end if
   dx4(1) = cf(1)*dx2
   dx4(2) = cf(2)*dx2
   do ic = ic1, ic2
    do k = kz1, kz2
     do j = jy1, jy2
      do i = i01, i02
       curr(i, j, k, ic) = curr(i, j, k, ic) + dx4(1)*(apf(i + 1, j, k, ic) + &
                                                       apf(i - 1, j, k, ic) - 2.*apf(i, j, k, ic))
      end do
     end do
    end do
   end do
   if (der_ord > 2) then
    do ic = ic1, ic2
     do k = kz1, kz2
      do j = jy1, jy2
       do i = i01, i02
        curr(i, j, k, ic) = curr(i, j, k, ic) + dx4(2)*(apf(i + 2, j, k, ic) + &
                                                        apf(i - 2, j, k, ic) - 2.*apf(i, j, k, ic))
       end do
      end do
     end do
    end do
   end if
   if (ndim > 1) call pp_lapl(apf, curr, ic1, ic2)
  end subroutine
  !===========================
  subroutine rote(ef, dtf)

   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: dtf
   real(dp) :: aphx, aphy, aphz
   real(dp) :: sdhy, sdhz
   integer :: i, j, k, jj, kk
   real(dp) :: aph1, aph2

   ! Enter ef(1:3)=[Ex,Ey,Ez], ef(4:6)=[Bx,By,Bz]
   ! SOLVES B=B-DT*rot[E]
   ! enter boundary fields
   !==================== B=B-dt*rot(E) interior domain==========
   aphx = dtf*dx_inv
   aphy = dtf*dy_inv
   aphz = dtf*dz_inv
   aph1 = aphx*se_coeff(1)
   aph2 = aphx*se_coeff(2)
   !============================
   if (ndim == 1) then
    k = 1
    j = 1
    do i = ix1, ix2
     ef(i, j, k, nfield) = ef(i, j, k, nfield) - &
                           aph1*(ef(i + 1, j, k, 2) - ef(i, j, k, 2)) - &
                           aph2*(ef(i + 2, j, k, 2) - ef(i - 1, j, k, 2))
    end do
    return
   end if
   !=================================
   do k = kz1, kz2
    do j = jy1, jy2
     jj = j - 2
     sdhy = loc_yg(jj, 4, imody)*aphy
     do i = ix1, ix2
      ef(i, j, k, nfield) = ef(i, j, k, nfield) - &
                            aph1*(ef(i + 1, j, k, 2) - ef(i, j, k, 2)) + &
                            sdhy*(ef(i, j + 1, k, 1) - ef(i, j, k, 1)) - &
                            aph2*(ef(i + 2, j, k, 2) - ef(i - 1, j, k, 2))
     end do
    end do
   end do
   if (nfield < 6) return
   if (ndim == 3) then
    do k = kz1, kz2
     kk = k - 2
     sdhz = loc_zg(kk, 4, imodz)*aphz
     do j = jy1, jy2
      jj = j - 2
      sdhy = loc_yg(jj, 4, imody)*aphy
      do i = ix1, ix2
       ef(i, j, k, 4) = ef(i, j, k, 4) - sdhy*(ef(i, j + 1, k, 3) - ef(i, j, k, 3) &
                                               ) + sdhz*(ef(i, j, k + 1, 2) - ef(i, j, k, 2))
       ef(i, j, k, 5) = ef(i, j, k, 5) + &
                        aph1*(ef(i + 1, j, k, 3) - ef(i, j, k, 3)) - &
                        sdhz*(ef(i, j, k + 1, 1) - ef(i, j, k, 1)) + &
                        aph2*(ef(i + 2, j, k, 3) - ef(i - 1, j, k, 3))
      end do
     end do
    end do
   else
    k = 1
    do j = jy1, jy2
     jj = j - 2
     sdhy = loc_yg(jj, 4, imody)*aphy
     do i = ix1, ix2
      ef(i, j, k, 4) = ef(i, j, k, 4) - sdhy*(ef(i, j + 1, k, 3) - ef(i, j, k, 3))
      ef(i, j, k, 5) = ef(i, j, k, 5) + aph1*(ef(i + 1, j, k, 3) - ef(i, j, k, 3)) &
                       + aph2*(ef(i + 2, j, k, 3) - ef(i - 1, j, k, 3))
     end do
    end do
   end if
   !================== interior domains
  end subroutine
  !===============================
  subroutine rotb(ef, dtf)

   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: dtf
   real(dp) :: sdy, sdz, aph1, aph2
   real(dp) :: aphx, aphy, aphz
   integer :: i, j, k, ii, jj, kk
   ! E=E+DT*rot[B]          Two-point Second order derivatives
   !==================== B=B-dt*rot(E) interior domain==========
   ! enter boundary fields
   !=================== interior domains
   aphx = dtf*dx_inv
   aphy = dtf*dy_inv
   aphz = dtf*dz_inv
   aph1 = aphx*se_coeff(1)
   aph2 = aphx*se_coeff(2)
   if (ndim == 1) then
    k = 1
    j = 1
    do i = ix1, ix2
     ef(i, j, k, 2) = ef(i, j, k, 2) - aphx*(ef(i, j, k, nfield) - ef(i - 1, j, k &
                                                                      , nfield))
    end do
   end if
   !=========================== NDIM > 1
   do k = kz1, kz2
    do j = jy1, jy2
     jj = j - 2
     sdy = loc_yg(jj, 3, imody)*aphy
     do i = ix1, ix2
      ii = i - 2
      ef(i, j, k, 1) = ef(i, j, k, 1) + sdy*(ef(i, j, k, nfield) - ef(i, j - 1, k &
                                                                      , nfield))
      ef(i, j, k, 2) = ef(i, j, k, 2) - &
                       aph1*(ef(i, j, k, nfield) - ef(i - 1, j, k, nfield)) - &
                       aph2*(ef(i + 1, j, k, nfield) - ef(i - 2, j, k, nfield))
     end do
    end do
   end do
   if (nfield < 6) return
   if (ndim == 3) then
    do k = kz1, kz2
     kk = k - 2
     sdz = aphz*loc_zg(kk, 3, imodz)
     do j = jy1, jy2
      jj = j - 2
      sdy = aphy*loc_yg(jj, 3, imody)
      do i = ix1, ix2
       ii = i - 2
       ef(i, j, k, 1) = ef(i, j, k, 1) - sdz*(ef(i, j, k, 5) - ef(i, j, k - 1, 5))
       ef(i, j, k, 2) = ef(i, j, k, 2) + sdz*(ef(i, j, k, 4) - ef(i, j, k - 1, 4))
       ef(i, j, k, 3) = ef(i, j, k, 3) + &
                        aph1*(ef(i, j, k, 5) - ef(i - 1, j, k, 5)) - &
                        sdy*(ef(i, j, k, 4) - ef(i, j - 1, k, 4)) + &
                        aph2*(ef(i + 1, j, k, 5) - ef(i - 2, j, k, 5))
      end do
     end do
    end do
   else
    k = 1
    do j = jy1, jy2
     jj = j - 2
     sdy = aphy*loc_yg(jj, 3, imody)
     do i = ix1, ix2
      ii = i - 2
      ef(i, j, k, 3) = ef(i, j, k, 3) + &
                       aph1*(ef(i, j, k, 5) - ef(i - 1, j, k, 5)) - &
                       sdy*(ef(i, j, k, 4) - ef(i, j - 1, k, 4)) + &
                       aph2*(ef(i + 1, j, k, 5) - ef(i - 2, j, k, 5))
     end do
    end do
   end if
  end subroutine
  !=====================================
  !=================================
  subroutine nc_fluid_density_momenta(flx, ef, dt_step, fcomp)
   real(dp), intent(in) :: flx(:, :, :, :)
   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: dt_step
   integer, intent(in) :: fcomp

   integer(kind=4) :: flux_ind
   real(dp) :: aphx, aphy, aphz
   integer :: i, j, k, ic, i01, i02, j01, j02, k01, k02, fcomp_tot
   real(dp) :: shy, shz
   real(dp) :: dw(3), sl(2), sr(2), omgl(2), vv, s0
   real(dp), parameter :: EPS = 1.e-06

   real(dp), dimension(2), parameter :: W03 = [1./3., 2./3.]
   real(dp), dimension(3), parameter :: LDER = [0.5, -2., 1.5]
   real(dp), dimension(3), parameter :: RDER = [-1.5, 2., -0.5]

   ! Fourth order derivatives
   !real(dp), dimension(4), parameter :: LDER4 = [ 1./6., -1., 0.5, &
   !  1./3. ] ![i-2,i+1] stencil
   !real(dp), dimension(4), parameter :: RDER4 = [ -1./3., -0.5, 1., &
   !  -1./6. ] ![i-1,i+2] stencil
   !=========================
   ! Enter primitive variables in flux array flx(Px,Py,Pz,den,vx,vy,vz)
   ! fcomp=curr_ndim+1 components
   flux_ind = 1 !=1 for pure upwind
   !=2 for LxF fluxin density equation
   i01 = ix1
   if (xl_bd) i01 = ix1 + 2
   i02 = ix2
   if (xr_bd) i02 = ix2 - 2
   j01 = jy1
   if (yl_bd) j01 = jy1 + 2
   j02 = jy2
   if (yr_bd) j02 = jy2 - 2
   k01 = kz1
   if (zl_bd) k01 = kz1 + 2
   k02 = kz2
   if (zr_bd) k02 = kz2 - 2

   aphx = dt_step*dx_inv
   aphy = dt_step*dy_inv
   aphz = dt_step*dz_inv
   !===========================
   ! momenta-density
   fcomp_tot = fcomp + 1
   do k = kz1, kz2
    do j = jy1, jy2
     do ic = 1, fcomp_tot
      do i = i01 - 2, i02 + 2
       var(i, ic) = flx(i, j, k, ic)
      end do
     end do
     call weno3_nc(fcomp_tot, i01 - 2, i02 + 2, xl_bd, xr_bd)
     do ic = 1, fcomp !var=momenta
      do i = i01, i02
       ef(i, j, k, ic) = ef(i, j, k, ic) - aphx*ww0(i, ic)
      end do
     end do
    end do
   end do
   !====================
   do k = kz1, kz2
    do i = ix1, ix2
     do ic = 1, fcomp
      do j = j01 - 2, j02 + 2 !Extended range[j1-2,n2p+2] in interior domains
       var(j, ic) = flx(i, j, k, ic)
      end do
     end do
     do j = j01 - 2, j02 + 2
      var(j, fcomp + 1) = flx(i, j, k, fcomp + 2)
     end do
     call weno3_nc(fcomp + 1, j01 - 2, j02 + 2, yl_bd, yr_bd) !rec[flux][j01-1,j02+1]
     do ic = 1, fcomp
      do j = j01, j02
       shy = aphy*loc_yg(j - 2, 3, imody)
       ef(i, j, k, ic) = ef(i, j, k, ic) - shy*ww0(j, ic)
      end do
     end do
    end do
   end do
   if (ndim < 3) return
   do j = jy1, jy2
    do i = ix1, ix2
     do ic = 1, fcomp
      do k = k01 - 2, k02 + 2
       var(k, ic) = flx(i, j, k, ic)
      end do
     end do
     ic = fcomp + 1
     do k = k01 - 2, k02 + 2
      var(k, ic) = flx(i, j, k, fcomp + 3)
     end do
     call weno3_nc(fcomp + 1, k01 - 2, k02 + 2, zl_bd, zr_bd)
     do ic = 1, fcomp
      do k = k01, k02
       shz = aphz*loc_zg(k - 2, 3, imodz)
       ef(i, j, k, ic) = ef(i, j, k, ic) - shz*ww0(k, ic)
      end do
     end do
    end do
   end do
   !=================================
  contains
   subroutine weno3_nc(nc, i1, np, lbd, rbd)
    integer, intent(in) :: nc, i1, np
    logical, intent(in) :: lbd, rbd
    !  enter data [i1,np]
    integer :: ii, iic

    !=======ENTER DATA [i1,np]
    !wl_{i+1/2}  uses stencil [i-1,i,i+1] in range [i=i1+1,np-1]
    !wr_{i+1/2}  uses stencil [i,i+1,i+2] in range [i=i1,np-2]
    !            common interior points [i1+1,np-2
    !            Dw first derivative in range[i1+2,np-2]
    !            L-Boundary    Dw^r[i1+1] uses the [i1:i1+3] stencil for v<0
    !            R-Boundary    Dw^L[np-1] uses the [np-3:np1] stencil
    !===========================================

    iic = nc - 1
    do ii = i1, np
     var(ii, nc + 1) = var(ii, iic)*var(ii, nc) !in den array var(nc+1) => den*v
    end do
    !================= reconstruct nc primitives (Px,Py,Pz,Den,V)
    do iic = 1, nc
     do ii = i1 + 1, np - 1
      dw(1) = var(ii, iic) - var(ii - 1, iic) !DW_{i-1/2}
      dw(2) = var(ii + 1, iic) - var(ii, iic) !DW_{i+1/2}
      omgl(1) = 1./(dw(1)*dw(1) + EPS)
      omgl(2) = 1./(dw(2)*dw(2) + EPS)
      omgl(:) = omgl(:)*omgl(:)
      sl(1) = W03(1)*omgl(1)
      sl(2) = W03(2)*omgl(2)
      sr(1) = W03(2)*omgl(1)
      sr(2) = W03(1)*omgl(2)
      s0 = sl(1) + sl(2)
      wl(ii, iic) = var(ii, iic) + 0.5*(dw(1)*sl(1) + dw(2)*sl(2))/s0
      s0 = sr(1) + sr(2)
      wr(ii - 1, iic) = var(ii, iic) - 0.5*(dw(1)*sr(1) + dw(2)*sr(2))/s0
     end do
    end do
    !===================================
    !upwind boundary derivatives
    if (lbd) then
     do iic = 1, nc - 2
      ii = i1
      ww0(ii, iic) = 0.0
      vv = var(ii, nc)
      if (vv < 0.0) ww0(ii, iic) = vv*(var(ii + 1, iic) - var(ii, iic))
      ii = i1 + 1
      vv = var(ii, nc)
      ww0(ii, iic) = vv*(var(ii, iic) - var(ii - 1, iic))
      if (vv < 0.0) ww0(ii, iic) = vv*dot_product(RDER(1:3), var(ii:ii + 2, iic))
     end do
     iic = nc - 1
     ii = i1
     ww0(ii, iic) = 0.0
     vv = var(ii, nc)
     if (vv < 0.0) ww0(ii, iic) = var(ii + 1, nc + 1) - var(ii, nc + 1)
     ii = i1 + 1
     vv = var(ii, nc)
     ww0(ii, iic) = var(ii, nc + 1) - var(ii - 1, nc + 1)
     if (vv < 0.0) ww0(ii, iic) = dot_product(RDER(1:3), var(ii:ii + 2, nc + 1))
    end if
    if (rbd) then
     do iic = 1, nc - 2
      ii = np - 1
      vv = var(ii, nc)
      ww0(ii, iic) = vv*(var(ii + 1, iic) - var(ii, iic))
      if (vv > 0.0) ww0(ii, iic) = vv*dot_product(LDER(1:3), var(ii - 2:ii, iic))
      ii = np
      vv = var(ii, nc)
      ww0(ii, iic) = 0.0
      if (vv > 0.0) ww0(ii, iic) = vv*(var(ii, iic) - var(ii - 1, iic))
     end do
     iic = nc - 1
     ii = np - 1
     vv = var(ii, nc)
     ww0(ii, iic) = var(ii + 1, nc + 1) - var(ii, nc + 1)
     if (vv > 0.0) ww0(ii, iic) = dot_product(LDER(1:3), var(ii - 2:ii, nc + 1))
     ii = np
     vv = var(ii, nc)
     ww0(ii, iic) = 0.0
     if (vv > 0.0) ww0(ii, iic) = var(ii, nc + 1) - var(ii - 1, nc + 1)
    end if
    !===================================
    !   UPWINDING at interior points
    !          Momenta
    do iic = 1, nc - 2
     do ii = i1 + 1, np - 2
      vv = wr(ii, nc) + wl(ii, nc)
      s0 = sign(one_dp, vv) !s0=1*sign(vv)
      var(ii, iic) = max(0., s0)*wl(ii, iic) - min(0., s0)*wr(ii, iic)
     end do
     do ii = i1 + 2, np - 2
      ww0(ii, iic) = var(ii, nc)*(var(ii, iic) - var(ii - 1, iic))
     end do
    end do
    ! LxF flux for density variable
    !   F=nv=> 1/2(F_L+F_R)-|V_{max}|(den_R-den_L)]
    iic = nc - 1
    do ii = i1 + 1, np - 2
     dw(1) = var(ii - 1, nc)
     dw(2) = var(ii, nc)
     dw(3) = var(ii + 1, nc)
     vv = maxval(abs(dw(1:3)))
     var(ii, iic) = wr(ii, nc)*wr(ii, iic) + wl(ii, nc)*wl(ii, iic) - &
                    vv*(wr(ii, iic) - wl(ii, iic))
     var(ii, iic) = 0.5*var(ii, iic)
    end do
    do ii = i1 + 2, np - 2
     ww0(ii, iic) = var(ii, iic) - var(ii - 1, iic)
    end do
   end subroutine
  end subroutine
!====================================
 end module

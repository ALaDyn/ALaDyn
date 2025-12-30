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

! ===== File kept to store old unused functions (but that may be useful in some future) =====

  subroutine ran_number(idum, ran2)
   integer, intent(inout) :: idum
   real(dp), intent(out) :: ran2

   integer, parameter :: im1 = 2147483563, im2 = 2147483399, &
                         ia1 = 40014, ia2 = 40692, iq1 = 53668, iq2 = 52774, ir1 = 12211, &
                         ir2 = 3791, ntab = 32, imm1 = im1 - 1, ndiv = 1 + int(imm1/ntab)
   real(dp), parameter :: am = 1.0/im1, eps = 1.2e-07, rnmx = 1.0 - eps
   integer :: j, k
   integer, save :: iv(32) = 0, iy = 0, idum2 = 123456789

   if (idum <= 0) then
    idum = max(-idum, 1)
    idum2 = idum
    do j = ntab + 8, 1, -1
     k = idum/iq1
     idum = ia1*(idum - k*iq1) - k*ir1
     if (idum < 0) idum = idum + im1
     if (j <= ntab) iv(j) = idum
    end do
    iy = iv(1)
   end if
   k = idum/iq1
   idum = ia1*(idum - k*iq1) - k*ir1
   if (idum < 0) idum = idum + im1
   k = idum2/iq2
   idum2 = ia2*(idum2 - k*iq2) - k*ir2
   if (idum2 < 0) idum2 = idum2 + im2
   j = 1 + iy/ndiv
   iy = iv(j) - idum2
   iv(j) = idum
   if (iy < 1) iy = iy + imm1
   ran2 = min(am*iy, rnmx)
  end subroutine

  !===========================

  subroutine pbunch_gen(stp, n1, n2, lx, sy, sz, ey, ez, betx, bunch)
   integer, intent(in) :: stp, n1, n2
   real(dp), intent(in) :: lx, sy, sz, ey, ez, betx
   real(dp), intent(inout) :: bunch(:, :)
   integer :: i, j, np
   real(dp) :: sigs(6)
   real(dp) :: ym, zm, pzm, pym
   real(dp) :: v1, v2, rnd

   !Distribute (z,y,pz,py) centered on 0;
   !Distribute (x,px) uniformly

   np = n2 + 1 - n1
   sigs(1) = lx/real(np, dp) !x-distrution
   sigs(2) = sy
   sigs(3) = sz
   sigs(4) = 0.0 !dpz
   sigs(5) = betx*ey/sy
   sigs(6) = betx*ez/sz
   ! =====================
   do i = n1, n2
    bunch(1, i) = sigs(1)*real(i - 1, dp)
    bunch(4, i) = betx
   end do
   !======================= transverse (py,pz) iand (sy,sz) are gaussian
   select case (stp)
   case (1) !Gaussian distributions
    do i = n1, n2
     do j = 2, 5, 3
      do
       call random_number(v1)
       call random_number(v2)
       v1 = 2.0*v1 - 1.0
       v2 = 2.0*v2 - 1.0
       rnd = v1*v1 + v2*v2
       if (rnd < 1.0) exit
      end do
      rnd = sqrt(-2.0*log(rnd)/rnd)
      bunch(j, i) = v1*rnd
      bunch(j + 1, i) = v2*rnd
     end do
    end do
   case (2)
    do i = n1, n2
     do j = 2, 5, 3
      do
       call random_number(v1)
       call random_number(v2)
       v1 = 2.0*v1 - 1.0
       v2 = 2.0*v2 - 1.0
       rnd = v1*v1 + v2*v2
       if (rnd < 1.) exit
      end do
      bunch(j, i) = v1
      bunch(j + 1, i) = v2
     end do
    end do
   end select
   do i = n1, n2
    do j = 2, 5, 3
     bunch(j, i) = sigs(j)*bunch(j, i)
     bunch(j + 1, i) = sigs(j + 1)*bunch(j + 1, i)
    end do
   end do

   ym = 0.0
   zm = 0.0
   pym = 0.0
   pzm = 0.0

   ! Reset centering
   do i = n1, n2
    ym = ym + bunch(2, i)
    zm = zm + bunch(3, i)
    pym = pym + bunch(5, i)
    pzm = pzm + bunch(6, i)
   end do
   !==================== uniform x distribution dx=Lx/np
   ym = ym/real(np, dp)
   zm = zm/real(np, dp)
   pym = pym/real(np, dp)
   pzm = pzm/real(np, dp)

   do i = n1, n2
    bunch(2, i) = bunch(2, i) - ym
    bunch(3, i) = bunch(3, i) - zm
    bunch(5, i) = bunch(5, i) - pym
    bunch(6, i) = bunch(6, i) - pzm
   end do
  end subroutine

  subroutine boxmuller_vector(randnormal, len)
   integer, intent(in) :: len
   real(dp), intent(inout) :: randnormal(len)
   real(dp) :: x, y, s, r
   real(dp) :: mu, std
   integer :: i

   do i = 1, len
    s = 10.
    do while (s >= 1.)
     call random_number(x)
     call random_number(y)
     x = 2.*x - 1.
     y = 2.*y - 1.
     s = x**2 + y**2
    end do
    r = sqrt(-2.*log(s)/s)
    randnormal(i) = x*r
   end do
   !--- convergence to N(0,1) ---!
   mu = sum(randnormal)/(1.*len - 1.)
   std = sqrt(sum((randnormal - mu)**2)/(1.*len - 1.))
   randnormal = (randnormal - mu)/std
  end subroutine

  !Box-Muller with cut in the distribution
  subroutine boxmuller_vector_cut(randnormal, len, cut)
   integer, intent(in) :: len
   real(8), intent(inout) :: randnormal(len)
   real(8), intent(in) :: cut
   real(8) :: x, y, s, r
   integer :: i

   do i = 1, len
100 continue !if the particle is cut :: recalculate particle position
    s = 10.
    do while (s >= 1.)
     call random_number(x)
     call random_number(y)
     x = 2.*x - 1.
     y = 2.*y - 1.
     s = x**2 + y**2
    end do
    r = sqrt(-2.*log(s)/s)
    randnormal(i) = x*r
    if (abs(randnormal(i)) > cut) go to 100
   end do
   randnormal = randnormal - sum(randnormal)/(1.*max(1, size(randnormal)) &
                                              )
  end subroutine
  !=======================================
  subroutine bunch_twissrotation(n1, n2, generated_bunch, alpha_y_t, &
                                 beta_y_t, alpha_z_t, beta_z_t, s_y, s_z, eps_y, eps_z, x_cm, y_cm, &
                                 z_cm)
   integer, intent(in) :: n1, n2
   real(dp), intent(inout) :: generated_bunch(:, :)
   real(dp), intent(in) :: alpha_y_t, beta_y_t, alpha_z_t, beta_z_t
   real(dp), intent(in) :: s_y, s_z, eps_y, eps_z, x_cm, y_cm, z_cm
   integer :: i
   real(dp) :: ay11, ay12, az11, az12

   !twiss-rotation-matrix
   ay11 = sqrt(eps_y*beta_y_t/(eps_y**2 + s_y**2*alpha_y_t**2))
   az11 = sqrt(eps_z*beta_z_t/(eps_z**2 + s_z**2*alpha_z_t**2))
   ay12 = -ay11*alpha_y_t*s_y**2/eps_y
   az12 = -az11*alpha_z_t*s_z**2/eps_z

   !twiss-rotation
   do i = n1, n2
    generated_bunch(2, i) = generated_bunch(2, i) - y_cm
    generated_bunch(2, i) = ay11*generated_bunch(2, i) + &
                            ay12*generated_bunch(5, i) + y_cm
    generated_bunch(3, i) = generated_bunch(3, i) - z_cm
    generated_bunch(3, i) = az11*generated_bunch(3, i) + &
                            az12*generated_bunch(6, i) + z_cm
    generated_bunch(5, i) = generated_bunch(5, i)/ay11
    generated_bunch(6, i) = generated_bunch(6, i)/az11
   end do
  end subroutine

!==========================

  subroutine set_x2_distrib(xb, nxb)
   integer, intent(in) :: nxb
   real(dp), intent(out) :: xb(:)
   real(dp) :: th, uu, du
   integer :: i

   du = 1./real(nxb, dp)
   do i = 1, nxb
    uu = 1.-2*du*(real(i, dp) - 0.5)
    th = acos(uu)/3.
    xb(i) = sqrt(3.)*sin(th) - cos(th)
   end do
  end subroutine

  subroutine set_pden(xp, x0, nx0, rat, id, isp)
   integer, intent(in) :: nx0(:), id, isp
   real(dp), intent(in) :: rat, x0(:)
   real(dp), intent(out) :: xp(:, :)
   integer :: i, i1
   real(dp) :: xx, delt1, delt2, delt3, delt4, alp

   select case (id)
   case (0)
    delt1 = 1./real(nx0(1), dp)
    delt2 = 1./real(nx0(2), dp)
    do i = 1, nx0(1)
     xp(i, isp) = x0(1)*sqrt(delt1*(real(i, dp)))
    end do
    xx = xp(nx0(1), isp)
    do i = 1, nx0(2)
     i1 = i + nx0(1)
     xp(i1, isp) = xx + x0(2)*delt2*(real(i, dp))
    end do

   case (1)
    delt1 = 1./real(nx0(1), dp)
    delt2 = 1./real(nx0(2), dp)
    delt3 = 1./real(nx0(3), dp)
    do i = 1, nx0(1)
     xp(i, isp) = x0(1)*sqrt(delt1*(real(i, dp) - 0.5))
    end do
    do i = 1, nx0(2)
     i1 = i + nx0(1)
     xp(i1, isp) = x0(1) + x0(2)*delt2*(real(i, dp) - 0.5)
    end do
    i1 = nx0(1) + nx0(2)
    do i = 1, nx0(3)
     xp(i + i1, isp) = xp(i1, isp) + x0(3)*(1.-sqrt(1.-delt3*(real(i, dp) - &
                                                              0.5)))
    end do

   case (2)
    delt1 = 1./real(nx0(1), dp)
    delt2 = 1./real(nx0(2), dp)
    delt3 = 1./real(nx0(3), dp)
    delt4 = 1./real(nx0(4), dp)
    alp = 1.-rat*rat
    do i = 1, nx0(1)
     xp(i, isp) = x0(1)*sqrt(delt1*(real(i, dp)))
    end do
    do i = 1, nx0(2)
     i1 = i + nx0(1)
     xp(i1, isp) = xp(nx0(1), isp) + x0(2)*delt2*(real(i, dp))
    end do
    i1 = nx0(1) + nx0(2)
    do i = 1, nx0(3)
     xp(i + i1, isp) = xp(i1, isp) + x0(3)*(1.-sqrt(delt3*(real(nx0(3) - i, &
                                                                dp))))
    end do
    i1 = i1 + nx0(3)
    do i = 1, nx0(4)
     xp(i + i1, isp) = xp(i1, isp) + x0(4)*delt4*real(i, dp)
    end do

   case (3)
    delt1 = 1./real(nx0(1), dp)
    delt2 = 1./real(nx0(2), dp)
    do i = 1, nx0(1)
     xp(i, isp) = x0(1)*(delt1*(real(i, dp)))**(0.2)
    end do
    do i = 1, nx0(2)
     i1 = i + nx0(1)
     xp(i1, isp) = xp(nx0(1), isp) + x0(2)*delt2*(real(i, dp))
    end do
   end select
  end subroutine

  !============================

  subroutine ludcmp(am, n)
   integer, intent(in) :: n
   real(dp), intent(inout) :: am(n, n)
   real(dp), parameter :: epsa = 1.e-06
   integer :: j, i, k, kk0
   real(dp) :: suma, dum

   do j = 1, n
    if (j > 1) then
     do i = 1, j - 1
      suma = am(i, j)
      if (i > 1) then
       kk0 = max(1, i - 2)
       do k = kk0, i - 1
        suma = suma - am(i, k)*am(k, j)
       end do
       am(i, j) = suma
      end if
     end do
    end if
    do i = j, n
     suma = am(i, j)
     if (j > 1) then
      kk0 = max(1, j - 2)
      do k = kk0, j - 1
       suma = suma - am(i, k)*am(k, j)
      end do
      am(i, j) = suma
     end if
    end do
    if (j < n) then
     dum = 1./(am(j, j) + epsa)
     do i = j + 1, n
      am(i, j) = am(i, j)*dum
     end do
    end if
   end do
   do j = 1, n
    am(j, j) = 1./(am(j, j) + epsa)
   end do

  end subroutine

  !=============================

  subroutine trid0(a, b, c, u, n)

   integer, intent(in) :: n
   real(dp), intent(in) :: a(n), b(n), c(n)
   real(dp), intent(inout) :: u(n)
   real(dp) :: g(n), bet
   integer :: k

   if (n == 1) return

   g(1) = 0.0
   bet = b(1)
   u(1) = u(1)/bet
   do k = 2, n
    g(k) = c(k - 1)/bet
    bet = b(k) - a(k)*g(k)
    u(k) = (u(k) - a(k)*u(k - 1))/bet
   end do
   do k = n - 1, 1, -1
    u(k) = u(k) - g(k + 1)*u(k + 1)
   end do
  end subroutine

  !===========================

  subroutine cycl0(a, b, c, x, n)

   integer, intent(in) :: n
   real(dp), intent(in) :: a(n), b(n), c(n)
   real(dp), intent(inout) :: x(n)
   real(dp) :: bb(n), u(n), gm, alp, bet, fact

   alp = c(n)
   bet = a(1)

   gm = -b(1)
   bb(1) = b(1) - gm
   bb(n) = b(n) - alp*bet/gm
   bb(2:n - 1) = b(2:n - 1)
   call trid0(a, bb, c, x, n)
   u(1) = gm
   u(n) = alp
   u(2:n - 1) = 0.0
   call trid0(a, bb, c, u, n)
   fact = (x(1) + bet*x(n)/gm)/(1.+u(1) + bet*u(n)/gm)
   x(1:n) = x(1:n) - fact*u(1:n)

  end subroutine

  subroutine k_fact(k, kfatt)
   integer, intent(in) :: k
   real(dp), intent(out) :: kfatt
   integer :: ck

   kfatt = k
   if (k < 2) return
   do ck = k - 1, 2, -1
    kfatt = kfatt*ck
   end do
  end subroutine

  !===========================
    !=============================================
  subroutine collect_bunch_and_plasma_density(this_bunch, isp)

   !========== bunch density and particles of species isp added on jc(ic)
   !=========================================
   integer, intent (in) :: this_bunch, isp
   real (dp) :: dery, derz
   integer :: np, nb, ik, i, j, k, jj, kk

   do i = 1, 2
    jc(:, :, :, i) = 0.0
   end do
   np = loc_npart(imody, imodz, imodx, isp)

   if (this_bunch==0) then
    do ik = 1, nsb
     nb = loc_nbpart(imody, imodz, imodx, ik)
     if (nb>0) then
      call set_grid_charge(bunch(ik), ebfb, jc, nb, 1)
     end if
    end do
   else
    ik = this_bunch !only the selected bunch density
    nb = loc_nbpart(imody, imodz, imodx, ik)
    if (nb>0) then
     call set_grid_charge(bunch(ik), ebfb, jc, nb, 1)
    end if
   end if
   !=========== bunch data on jc(1)
   !==================== data of isp species on jc(2)
   if (np>0) then
    call set_grid_charge(spec(isp), ebfp, jc, np, 2)
   end if
   if (prl) then
    do i = 1, 2
     call fill_curr_yzxbdsdata(jc, i)
    end do
   end if
   jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) = jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) + &
     jc(ix1:ix2, jy1:jy2, kz1:kz2, 2)
   !============ on jc(1) bunch+ particles
   if (stretch) then
    kk = kz1 - gcz + 1
    do k = kz1, kz2
     derz = loc_zg(kk, 3, imodz)
     jj = jy1 - gcy + 1
     do j = jy1, jy2
      dery = loc_yg(jj, 3, imody)*derz
      do i = ix1, ix2
       jc(i, j, k, 1) = dery*jc(i, j, k, 2)
       jc(i, j, k, 2) = dery*jc(i, j, k, 2)
      end do
      jj = jj + 1
     end do
     kk = kk + 1
    end do
   end if
   !=============================
  end subroutine

  subroutine prl_bden_energy_interp(ic)

   integer, intent (in) :: ic
   real (dp) :: dery, derz
   integer :: np, ik, i, j, k, jj, kk

   !curr_clean
   do i = 1, 2
    jc(:, :, :, i) = 0.0
   end do
   if (ic==0) then !collects all bunch density
    do ik = 1, nsb
     np = loc_nbpart(imody, imodz, imodx, ik)
     if (np>0) then
      call set_grid_den_energy(bunch(ik), ebfb, jc, np)
     end if
    end do
   else
    ik = ic !only the ic-bunch density
    np = loc_nbpart(imody, imodz, imodx, ik)
    if (np>0) then
     call set_grid_den_energy(bunch(ik), ebfb, jc, np)
    end if
   end if
   !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
   if (prl) then
    call fill_curr_yzxbdsdata(jc, 2)
   end if
   !do ik=1,2
   ! call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
   !end do
   jc(:, :, :, 1) = -jc(:, :, :, 1) !positive for electrons

   if (stretch) then
    kk = kz1 - gcz + 1
    do k = kz1, kz2
     derz = loc_zg(kk, 3, imodz)
     jj = jy1 - gcy + 1
     do j = jy1, jy2
      dery = loc_yg(jj, 3, imody)*derz
      do i = ix1, ix2
       jc(i, j, k, 1) = dery*jc(i, j, k, 1)
       jc(i, j, k, 2) = dery*jc(i, j, k, 2)
      end do
      jj = jj + 1
     end do
     kk = kk + 1
    end do
   end if
   !=============================
  end subroutine
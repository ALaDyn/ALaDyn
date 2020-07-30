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

 module fluid_density_momenta

  use mpi_field_interface
  use grid_fields
  implicit none
 contains

  subroutine fluid_curr_accumulate(flx, curr)

   real(dp), intent(inout) :: flx(:, :, :, :), curr(:, :, :, :)
   integer :: i, j, k, ic, str, stl, fdim
   real(dp) :: pp(1:3), den, gam2, ch, gam_inv
   real(dp) :: qx, qy, qz, av2
   real(dp), parameter :: WK1 = 0.5

   stl = 1
   str = 1
   ! Enter fluid variables at t^{n+1/2} and flx(fdim+1)= |a|^2/2 at t^{n+1/2}
   ch = dt_loc*WK1*unit_charge(1)
   fdim = curr_ndim + 1
   if (envelope) then
    do k = kz1, kz2
     do j = jy1, jy2
      do i = ix1, ix2
       av2 = flx(i, j, k, fdim + 1) !time centered |A|^{n+1/2}/2
       den = flx(i, j, k, fdim) !den^{n+1/2}
       pp(1:curr_ndim) = flx(i, j, k, 1:curr_ndim) !p momenta at t^{n+1/2}
       gam2 = 1.+dot_product(pp(1:curr_ndim), pp(1:curr_ndim))
       gam2 = gam2 + av2
       gam_inv = 1./sqrt(gam2)
       do ic = 1, curr_ndim
        flx(i, j, k, ic) = den*gam_inv*pp(ic) !n*v= density flux at t^{n+1/2}
       end do
      end do
     end do
    end do
   else
    do k = kz1, kz2
     do j = jy1, jy2
      do i = ix1, ix2
       den = flx(i, j, k, fdim) !den^{n+1/2}
       pp(1:curr_ndim) = flx(i, j, k, 1:curr_ndim) !p momenta at t^{n+1/2}
       gam2 = 1.+dot_product(pp(1:curr_ndim), pp(1:curr_ndim))
       gam_inv = 1./sqrt(gam2)
       do ic = 1, curr_ndim
        flx(i, j, k, ic) = den*gam_inv*pp(ic) !n*v= density flux at t^{n+1/2}
       end do
      end do
     end do
    end do
   end if
   call fill_ebfield_yzxbdsdata(flx, 1, curr_ndim, str, stl)
   call field_xyzbd(flx, curr_ndim)
   if (pe1x) then
    do k = kz1, kz2
     do j = jy1, jy2
      i = ix2
      flx(i + 1, j, k, 1) = flx(i, j, k, 1)
     end do
    end do
   endif
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      qx = ch*(flx(i, j, k, 1) + flx(i + 1, j, k, 1)) !Dt*Jx(i+1/2,j,k)
      qy = ch*(flx(i, j, k, 2) + flx(i, j + 1, k, 2)) !Dt*Jy(i,j+1/2,k)
      curr(i, j, k, 1) = curr(i, j, k, 1) + qx
      curr(i, j, k, 2) = curr(i, j, k, 2) + qy
     end do
    end do
   end do
   if (curr_ndim == 3) then
    do k = kz1, kz2
     do j = jy1, jy2
      do i = ix1, ix2
       qz = ch*(flx(i, j, k + 1, 3) + flx(i, j, k, 3)) !Dt*Jz(i,j,k+1/2)
       curr(i, j, k, 3) = curr(i, j, k, 3) + qz
      end do
     end do
    end do
   end if
   !In curr(1:curr_ndim) exit  Dt*J^{n+1/2}
  end subroutine
  !============================
  subroutine set_env_momentum_density_flux(uv, ef, curr, eb_tot, flx)
   real(dp), intent(in) :: uv(:, :, :, :), ef(:, :, :, :)
   real(dp), intent(inout) :: curr(:, :, :, :)
   real(dp), intent(out) :: flx(:, :, :, :), eb_tot(:, :, :, :)
   integer :: fdim, ic, i, j, k
   real(dp) :: den, pp(3), gam2, gam_inv
   !================ set density and momenta flux
   !fdim = curr_ndim + 1
   fdim = size(uv, 4)
   flx(:, :, :, 1:fdim) = uv(:, :, :, 1:fdim)
   ! Enter curr(1)= |A|^2/2 and curr(2:4) grad|A|^2/2 at t^n time level
   !=====================

   eb_tot(ix1:ix2, jy1:jy2, kz1:kz2, 1:nfield) = ef(ix1:ix2, jy1:jy2, kz1:kz2, 1:nfield)

   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      den = uv(i, j, k, fdim)
      pp(1:curr_ndim) = uv(i, j, k, 1:curr_ndim) !p momenta
      gam2 = 1.+dot_product(pp(1:curr_ndim), pp(1:curr_ndim))
      gam2 = gam2 + curr(i, j, k, 1)
      gam_inv = 1./sqrt(gam2)
      pp(1:curr_ndim) = gam_inv*pp(1:curr_ndim) !(vx,vy,vz)=pp/gam at time t^n
      curr(i, j, k, 1) = gam_inv*den !n/gam fluid contribution of the sorce of envelope equation
      do ic = 1, curr_ndim
       eb_tot(i, j, k, ic) = eb_tot(i, j, k, ic) + &
                             0.5*gam_inv*curr(i, j, k, ic + 1) !Envelope grad|A|^2/(4*gam_p):wq
       flx(i, j, k, fdim + ic) = pp(ic) !(vx,vy,vz)
      end do
     end do
    end do
   end do
   !Exit flx(1:4)=uv(1:4)=[Px,Py,Pz,den] flx(5:8)=[vx,vy,vz]
   !======================================
  end subroutine
  !====================
  subroutine set_momentum_density_flux(uv, flx)
   real(dp), intent(in) :: uv(:, :, :, :)
   real(dp), intent(inout) :: flx(:, :, :, :)
   integer :: fdim, ic, i, j, k
   real(dp) :: den, pp(3), gam2, gam_inv
   !================ set density and momenta flux
   fdim = curr_ndim + 1
   flx(:, :, :, 1:fdim) = uv(:, :, :, 1:fdim)
   !stores the modentum-density at current time level t^n
   !=====================
   do k = kz1, kz2
    do j = jy1, jy2
     do i = ix1, ix2
      den = uv(i, j, k, fdim)
      pp(1:curr_ndim) = uv(i, j, k, 1:curr_ndim) !p momenta
      gam2 = 1.+dot_product(pp(1:curr_ndim), pp(1:curr_ndim))
      gam_inv = 1./sqrt(gam2)
      pp(1:curr_ndim) = gam_inv*pp(1:curr_ndim) !(vx,vy,vz)=pp/gam at time t^n
      do ic = 1, curr_ndim
       flx(i, j, k, fdim + ic) = gam_inv*uv(i, j, k, ic) !(vx,vy,vz)
      end do
     end do
    end do
   end do
   !Exit non-conservative variables flx(1:4)=uv(1:4)=[Px,Py,Pz,den] flx(5:8)=[vx,vy,vz]
  end subroutine
  !=======================================
  subroutine update_adam_bash_fluid_variables(u, u0, flx, ef)
   real(dp), intent(inout) :: u(:, :, :, :), u0(:, :, :, :)
   real(dp), intent(inout) :: ef(:, :, :, :), flx(:, :, :, :)
   integer :: i, j, k, ic, str, stl, fdim, fldim
   real(dp) :: den, lzf
   real(dp) :: ex, ey, ez, bx, by, bz, vx, vy, vz, b1p, b1m
   real(dp), parameter :: WK1 = 0.5, EPS = 1.e-06
   real(dp) :: abf_0, abf_1
   !===================================
   ! INTEGRATES by a one-step adam-bashfort (dissipative leap-frog)
   !===============================
   !   NON-CONSERVATIVE FORM of RELATIVISTC COLD FLUID
   !==========================================
   !   D_t(p)+v*grad(p)=charge*[E +vxB]
   !   D_t(n) +div(nv) =0
   !   arrays :  q(1:3)=v,  u(1:3)=p   gamm^2=1+p*p
   !===============================
   ! enter ef=total (E,B) fields on staggered grid at t^n time level
   !================================
   lzf = lorentz_fact(1)*unit_charge(1)*dt_loc
   fdim = size(u, 4)
   !fdim=curr_ndim+1
   fldim = size(flx, 4)
   !fldim = 2*curr_ndim + 1 !(five or seven components)
   abf_0 = -0.5
   abf_1 = 1.5
   !================== Enter
   ! flx[Px,Py,Pz,den,vx,vy,vz]^n fldim components
   ! ef[1:nfield] = total (E,B) fields and ponderomotive force
   !===============================================
   str = 1
   stl = 1
   if (prl) then
    !extends flux data to j1-2,j2+2 and k1-2,k2+2
    call fill_ebfield_yzxbdsdata(flx, 1, fldim, 2, 2)
    call fill_ebfield_yzxbdsdata(ef, 1, nfield, str, stl)
    call field_xyzbd(ef, nfield)
   end if
   if (initial_time) then !a one_step update
    u0(:, :, :, :) = zero_dp
    call nc_fluid_density_momenta(flx, u0, dt_loc, fdim)
    ! - F_adv(u) = - grad(Flux)
    call add_lorentz_force !in u_0 is stored Dt*(-F_adv(u)+ F_{Lorentz}) at t^n
    do ic = 1, fdim
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        u(i, j, k, ic) = u(i, j, k, ic) + u0(i, j, k, ic) !updates u^{n+1}=u^{n-1}+2*Dt*F^n
        flx(i, j, k, ic) = 0.5*(flx(i, j, k, ic) + u(i, j, k, ic)) ! (P,den) at t(n+1/2)
       end do
      end do
     end do
    end do
   else !in u_0 enter F^{n-1}
    do ic = 1, fdim
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        u(i, j, k, ic) = u(i, j, k, ic) + abf_0*u0(i, j, k, ic)
       end do
      end do
     end do
    end do
    u0(:, :, :, :) = 0.0
    call nc_fluid_density_momenta(flx, u0, dt_loc, fdim)
    call add_lorentz_force !in u_0 is ftored Dt*(F_adv(u)+ F_{Lorentz}) for next timestep
    do ic = 1, fdim
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        u(i, j, k, ic) = u(i, j, k, ic) + abf_1*u0(i, j, k, ic) !updates u^{n+1}=u^{n}+Dt*(3/2*F^n-1/2F^{n-1})
       end do
       do i = ix1, ix2
        flx(i, j, k, ic) = 0.5*(flx(i, j, k, ic) + u(i, j, k, ic)) ! (P,den) at t(n+1/2)
       end do
      end do
     end do
    end do
   end if
   !==========================
  contains
   subroutine add_lorentz_force
    !in u0() -flux derivatives
    do k = kz1, kz2
     do j = jy1, jy2
      do i = ix1, ix2
       den = 1.
       if (flx(i, j, k, fdim) <= EPS) den = 0.0
       ex = WK1*(ef(i, j, k, 1) + ef(i - 1, j, k, 1)) !Ex(i,j,k)
       ey = WK1*(ef(i, j, k, 2) + ef(i, j - 1, k, 2)) !Ey(i,j,k)
       b1p = WK1*(ef(i, j, k, nfield) + ef(i - 1, j, k, nfield)) !bz(i,j+1/2,k)
       b1m = WK1*(ef(i, j - 1, k, nfield) + ef(i - 1, j - 1, k, nfield)) !bz(i,j-1/2,k)
       bz = WK1*(b1p + b1m) !Bz(i,j,k)
       vx = flx(i, j, k, fdim + 1) !vx^n
       vy = flx(i, j, k, fdim + 2) !vy^n
       u0(i, j, k, 1) = u0(i, j, k, 1) + den*lzf*(ex + vy*bz) !=> u^{n+1}
       u0(i, j, k, 2) = u0(i, j, k, 2) + den*lzf*(ey - vx*bz)
      end do
     end do
    end do
    if (curr_ndim == 3) then
     do k = kz1, kz2
      do j = jy1, jy2
       do i = ix1, ix2
        den = 1.
        if (flx(i, j, k, fdim) <= EPS) den = 0.0
        ez = WK1*(ef(i, j, k, 3) + ef(i, j, k - 1, 3)) !Ez(i,j,k)
        b1p = WK1*(ef(i, j, k, 5) + ef(i - 1, j, k, 5)) !by(i+1/2,j,k+1/2)
        b1m = WK1*(ef(i, j, k - 1, 5) + ef(i - 1, j, k - 1, 5))
        by = WK1*(b1p + b1m) !By(i,j,k)
        b1p = WK1*(ef(i, j, k, 4) + ef(i, j - 1, k, 4)) !bx(i,j+1/2,k+1/2)
        b1m = WK1*(ef(i, j, k - 1, 4) + ef(i, j - 1, k - 1, 4))
        bx = WK1*(b1p + b1m) !Bx(i,j,k)
        vx = flx(i, j, k, fdim + 1) !vx^n
        vy = flx(i, j, k, fdim + 2) !vy^n
        vz = flx(i, j, k, fdim + 3) !vz^n
        u0(i, j, k, 1) = u0(i, j, k, 1) - den*lzf*vz*by
        u0(i, j, k, 2) = u0(i, j, k, 2) + den*lzf*vz*bx
        u0(i, j, k, 3) = u0(i, j, k, 3) + den*lzf*(ez + vx*by - vy*bx)
        !=> u^{n+1}
       end do
      end do
     end do
    end if
   end subroutine
  end subroutine

 end module


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

 module boris_push

  use pstruct_data
  use fstruct_data
  use common_param

  implicit none

 contains
  ! SECTION for Leap-frog integrators in LP regime
  !==========================
  subroutine init_lpf_momenta(sp_loc, pt, np, ic)
   type(species), intent(inout) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   integer, intent(in) :: np, ic
   integer :: p
   real(dp) :: alp, dth_lp, pp(3), vp(3), efp(6), gam2, gam_inv

   dth_lp = 0.5*dt_loc
   alp = dth_lp*lorentz_fact(ic) ! Lfact =1./m
   ! Fields are already multiplied by particle(ic) charge
   !=========================
   ! from p^n to p^{n-1/2}
   !==========================
   select case (curr_ndim)
   case (2)
    do p = 1, np
     efp(1:3) = -alp*pt(p, 1:3) !-DT/2*charge*(Ex,Ey,Bz)^n
     pp(1:2) = sp_loc%part(p, 3:4) !p_{n}
     gam2 = 1.+dot_product(pp(1:2), pp(1:2))
     gam_inv = 1./sqrt(gam2)
     vp(1:2) = pp(1:2)*gam_inv
     sp_loc%part(p, 3) = sp_loc%part(p, 3) + efp(1) + vp(2)*efp(3)
     sp_loc%part(p, 4) = sp_loc%part(p, 4) + efp(2) - vp(1)*efp(3)
    end do
   case (3)
    do p = 1, np
     pp(1:3) = sp_loc%part(p, 4:6)
     efp(1:6) = -alp*pt(p, 1:6)
     gam2 = 1.+dot_product(pp(1:3), pp(1:3))
     gam_inv = 1./sqrt(gam2) !1/gamma
     vp(1:3) = gam_inv*pp(1:3)
     sp_loc%part(p, 4) = sp_loc%part(p, 4) + efp(1) + vp(2)*efp(6) - &
                         vp(3)*efp(5)
     sp_loc%part(p, 5) = sp_loc%part(p, 5) + efp(2) + vp(3)*efp(4) - &
                         vp(1)*efp(6)
     sp_loc%part(p, 6) = sp_loc%part(p, 6) + efp(3) + vp(1)*efp(5) - &
                         vp(2)*efp(4)
    end do
   end select
  end subroutine
  !======================================
  subroutine lpf_momenta_and_positions(sp_loc, pt, np, ic)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)

   integer, intent(in) :: np, ic
   integer :: p, ch
   real(dp) :: alp, dt_lp, dth_lp, bb(3), pp(3), vp(3), vph(3), efp(6), &
               b2, bv, gam02, gam2, gam
   !========================================
   ! uses exact explicit solution for
   ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
   ! v^n=p^n/gamma^n
   !========================================
   !Enter Fields multiplied by particle charge
   dt_lp = dt_loc
   dth_lp = 0.5*dt_lp
   alp = dth_lp*lorentz_fact(ic)
   select case (curr_ndim)
   case (2)
    ch = 5
    do p = 1, np
     pp(1:2) = sp_loc%part(p, 3:4) !p_{n-1/2}
     efp(1:3) = alp*pt(p, 1:3) !q*Lfact*(Ex,Ey,Bz)*Dt/2
     vp(1:2) = pp(1:2) + efp(1:2) !u^{-} in Boris push
     vp(3) = efp(3) !b_z
     gam02 = 1.+dot_product(vp(1:2), vp(1:2)) !gam0 in Boris push
     b2 = vp(3)*vp(3) !b_z*b_z
     gam02 = gam02 - b2
     gam2 = 0.5*(gam02 + sqrt(gam02*gam02 + 4.*b2)) !exact gam^2 solution
     gam = sqrt(gam2)
     !==============================
     !p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     vph(1) = gam2*vp(1) + gam*vp(2)*vp(3)
     vph(2) = gam2*vp(2) - gam*vp(1)*vp(3)
     vph(1:2) = vph(1:2)/(gam2 + b2)
     sp_loc%part(p, 3:4) = 2.*vph(1:2) - pp(1:2)
     !=========== the new momenta
     !        update positions
     pt(p, 3:4) = sp_loc%part(p, 1:2) !old positions stored
     pp(1:2) = sp_loc%part(p, 3:4)
     gam2 = 1.+dot_product(pp(1:2), pp(1:2))
     pt(p, 5) = dt_lp/sqrt(gam2)
     vp(1:2) = pt(p, 5)*pp(1:2) !velocities
     pt(p, 1:2) = vp(1:2) !stores DT*V^{n+1/2}
     sp_loc%part(p, 1:2) = sp_loc%part(p, 1:2) + vp(1:2) !new positions
    end do
   case (3)
    ch = 7
    do p = 1, np
     pp(1:3) = sp_loc%part(p, 4:6)
     efp(1:6) = alp*pt(p, 1:6) !q*Lfact*(E,B) on p-th-particle
     vp(1:3) = pp(1:3) + efp(1:3) !p^{-} in Boris push
     bb(1:3) = efp(4:6)
     gam02 = 1.+dot_product(vp(1:3), vp(1:3)) !the lower order gamma in Boris scheme
     !=============================
     b2 = dot_product(bb(1:3), bb(1:3))
     bv = dot_product(bb(1:3), vp(1:3))
     gam02 = gam02 - b2
     gam2 = 0.5*(gam02 + sqrt(gam02*gam02 + 4.*(b2 + bv*bv))) ! exact solution for gam2=1+p_n*p_n
     gam = sqrt(gam2)
     !============================
     vph(1:3) = gam2*vp(1:3) + bb(1:3)*bv
     vph(1) = vph(1) + gam*(vp(2)*bb(3) - vp(3)*bb(2))
     vph(2) = vph(2) + gam*(vp(3)*bb(1) - vp(1)*bb(3))
     vph(3) = vph(3) + gam*(vp(1)*bb(2) - vp(2)*bb(1))
     vph(1:3) = vph(1:3)/(b2 + gam2) !p_n=(p_{n+1/2)+p_{n-1/2})/2
     !======== advance momenta
     sp_loc%part(p, 4:6) = 2.*vph(1:3) - pp(1:3)
     !==========
     pt(p, 4:6) = sp_loc%part(p, 1:3) !stores old positions
     pp(1:3) = sp_loc%part(p, 4:6)
     gam2 = 1.+dot_product(pp(1:3), pp(1:3))
     pt(p, 7) = dt_lp/sqrt(gam2)
     vp(1:3) = pt(p, 7)*pp(1:3)
     pt(p, 1:3) = vp(1:3) !stores dt*V
     sp_loc%part(p, 1:3) = sp_loc%part(p, 1:3) + vp(1:3) !new positions
    end do
   end select
   !====================
   if (iform < 2) then
    !old charge stored for charge preserving schemes
    do p = 1, np
     pt(p, ch) = sp_loc%part(p, ch)
    end do
   end if
   !In comoving frame vbeam >0
   if (vbeam > 0.) then
    do p = 1, np
     sp_loc%part(p, 1) = sp_loc%part(p, 1) - dt_lp*vbeam
     pt(p, 1) = pt(p, 1) - dt_lp*vbeam !
    end do
   end if
  end subroutine
  !=============================
  subroutine lpf_env_momenta(sp_loc, f_pt, np, ic)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(inout) :: f_pt(:, :)

   integer, intent(in) :: np, ic
   integer :: p
   real(dp) :: bb(3), pp(3), vp(3), vph(3)
   real(dp) :: b2, bv, alp, dt_lp, efp(6)

   dt_lp = dt_loc
   alp = 0.5*dt_lp*lorentz_fact(ic)
   !==========================
   !Enter F_pt(1:2)= q*(E+0.5q*grad[F]/gamp) and F_pt(3)=q*B/gamp     where F=|A|^2/2
   select case (curr_ndim)
   case (2)
    !F_pt(5)=wgh/gamp
    do p = 1, np
     pp(1:2) = sp_loc%part(p, 3:4) !p_{n-1/2}
     efp(1:3) = alp*f_pt(p, 1:3) !Lz_fact*Dt/2
     vp(1:2) = pp(1:2) + efp(1:2) !u^{-}
     bb(1) = efp(3)
     !==============================
     b2 = 1.+bb(1)*bb(1)
     vph(1) = vp(1) + vp(2)*bb(1)
     vph(2) = vp(2) - vp(1)*bb(1)
     vph(1:2) = vph(1:2)/b2 !p_n=(p_{n+1/2)+p_{n-1/2})/2
     sp_loc%part(p, 3:4) = 2.*vph(1:2) - pp(1:2)
     f_pt(p, 1:2) = sp_loc%part(p, 1:2)
    end do
    !F_pt(5)=wgh/gamp unchanged
   case (3)
    !F_pt(7)=wgh/gamp
    do p = 1, np
     pp(1:3) = sp_loc%part(p, 4:6)
     efp(1:6) = alp*f_pt(p, 1:6) !multiply by Lz_fact*Dt/2
     vp(1:3) = efp(1:3) + pp(1:3) !p_{n-1/2}+alp*(E+0.5*F/gamp)
     bb(1:3) = efp(4:6) !alp*B/gamp
     !=============================
     ! The Boris pusher
     !=========================
     b2 = 1.+dot_product(bb(1:3), bb(1:3))
     bv = dot_product(bb(1:3), vp(1:3))
     vph(1) = vp(1) + vp(2)*bb(3) - vp(3)*bb(2) + bb(1)*bv
     vph(2) = vp(2) + vp(3)*bb(1) - vp(1)*bb(3) + bb(2)*bv
     vph(3) = vp(3) + vp(1)*bb(2) - vp(2)*bb(1) + bb(3)*bv
     vph(1:3) = vph(1:3)/b2 !p_n=(p_{n+1/2)+p_{n-1/2})/2
     !======== advance momenta
     sp_loc%part(p, 4:6) = 2.*vph(1:3) - pp(1:3)
     f_pt(p, 1:3) = sp_loc%part(p, 1:3) !stores old positions
    end do
    !F_pt(7)=wgh/gamp unchanged
   end select
  end subroutine
  !======================
  subroutine lpf_env_positions(sp_loc, f_pt, np)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(inout) :: f_pt(:, :)

   integer, intent(in) :: np
   integer :: p, ch
   real(dp) :: pp(3), vp(3)
   real(dp) :: b2, gam2, gam_inv, dt_lp, dth_lp, gam, gam3

   dt_lp = dt_loc
   dth_lp = 0.5*dt_lp
   ch = 5
   !==========================
   select case (curr_ndim)
    !============  enter F_pt(3)=F, F_pt (1:2) Grad[F] where F=|A|^2/2
    !             at time level t^{n+1/2} assigned to the x^n positions
   case (2)
    do p = 1, np
     pp(1:2) = sp_loc%part(p, 3:4) !p^{n+1/2}
     vp(1:2) = f_pt(p, 1:2) !grad[F]
     !=============================
     gam2 = 1.+dot_product(pp(1:2), pp(1:2)) + f_pt(p, 3)
     gam = sqrt(gam2)
     gam3 = gam2*gam
     b2 = 0.25*dot_product(pp(1:2), vp(1:2))
     !-------------------- def gamma_p
     gam_inv = 1./gam
     gam_inv = gam_inv*(1.-dt_lp*b2/gam3)
     !============================
     vp(1:2) = dt_lp*gam_inv*pp(1:2)
     f_pt(p, 3:4) = sp_loc%part(p, 1:2) !old (x,y)^n positions
     f_pt(p, 5) = dt_lp*gam_inv ! dt/gamma
     sp_loc%part(p, 1:2) = sp_loc%part(p, 1:2) + vp(1:2)
     f_pt(p, 1:2) = vp(1:2) ! dt*V^{n+1/2}  velocities
    end do
   case (3)
    !============enter F_pt(4)=F, F_pt (1:3) Grad[F] where F=|A|^2/2 at t^{n+1/2}
    ! assigned at x^n
    ch = 7
    do p = 1, np
     pp(1:3) = sp_loc%part(p, 4:6) !p^{n+1/2}
     vp(1:3) = f_pt(p, 1:3) !grad[F]
     !=============================
     gam2 = 1.+dot_product(pp(1:3), pp(1:3)) + f_pt(p, 4)
     gam = sqrt(gam2)
     gam3 = gam2*gam
     b2 = 0.25*dot_product(pp(1:3), vp(1:3))
     !--------------------
     gam_inv = 1./sqrt(gam2)
     gam_inv = gam_inv*(1.-dt_lp*b2/gam3)
     vp(1:3) = dt_lp*gam_inv*pp(1:3)
     f_pt(p, 4:6) = sp_loc%part(p, 1:3) !old positions
     f_pt(p, 7) = dt_lp*gam_inv ! dt*gam_inv
     sp_loc%part(p, 1:3) = sp_loc%part(p, 1:3) + vp(1:3)
     f_pt(p, 1:3) = vp(1:3) ! dt*V^{n+1/2}  velocities
    end do
   end select
   if (iform < 2) then
    do p = 1, np
     f_pt(p, ch) = sp_loc%part(p, ch)
    end do
   end if
   !====================== vb=-wbet > 0 in comoving x-coordinate
   if (vbeam > 0.0) then
    do p = 1, np
     sp_loc%part(p, 1) = sp_loc%part(p, 1) - dt_lp*vbeam
     f_pt(p, 1) = f_pt(p, 1) - dt_lp*vbeam !new x-position
    end do
   end if
  end subroutine
  !=====================
 end module

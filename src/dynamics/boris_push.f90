
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
  use memory_pool

  implicit none

  interface boris_pusher
   module procedure boris_pusher_new
   module procedure boris_pusher_old
  end interface

  interface higuera_pusher
   module procedure higuera_pusher_new
   module procedure higuera_pusher_old
  end interface

  interface init_lpf_momenta
   module procedure init_lpf_momenta_new
   module procedure init_lpf_momenta_old
  end interface

  interface lpf_momenta_and_positions
   module procedure lpf_momenta_and_positions_new
   module procedure lpf_momenta_and_positions_old
  end interface

  interface lpf_env_momenta
   module procedure lpf_env_momenta_new
   module procedure lpf_env_momenta_old
  end interface

  interface lpf_env_positions
   module procedure lpf_env_positions_new
   module procedure lpf_env_positions_old
  end interface
 contains

  ! SECTION for Leap-frog integrators in LP regime
  !==========================
  subroutine init_lpf_momenta_new(sp_loc, pt, dt_in, np, ic, initial_time_in, mempool)
   type(species_new), intent(inout) :: sp_loc
   type(species_aux), intent(inout) :: pt
   real (dp), intent(in) :: dt_in
   integer, intent(in) :: np, ic
   logical, intent(inout) :: initial_time_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx
   real (dp) :: alp, dth_lp
 
   !======================================
   ! Only executes on the initial timestep
   if (.not. initial_time_in) return

   dth_lp = 0.5*dt_in
   alp = dth_lp*lorentz_fact(ic) ! Lfact =1./m
   ! Fields are already multiplied by particle(ic) charge
   !=========================
   ! from p^n to p^{n-1/2}
   !==========================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !======================================
   ! Do not execute for immobile particles
   !======================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================
   call mp_xx_realloc( mempool%mp_xx_2d_A, np, sp_loc%pick_dimensions(), mempool)
   xx => mempool%mp_xx_2d_A
   !=============================================
   select case (curr_ndim)

   case (2)

    call sp_loc%compute_gamma()

    xx(1:np, 1) = sp_loc%call_component(PX_COMP, lb=1, ub=np) - &
    alp*pt%call_component(EX_COMP, lb=1, ub=np) - &
    alp*pt%call_component(BZ_COMP, lb=1, ub=np) * sp_loc%call_component(PY_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np)

    xx(1:np, 2) = sp_loc%call_component(PY_COMP, lb=1, ub=np) - &
    alp*pt%call_component(EY_COMP, lb=1, ub=np) + &
    alp*pt%call_component(BZ_COMP, lb=1, ub=np) * sp_loc%call_component(PX_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np)

    call sp_loc%set_component(xx(1:np, 1), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component(xx(1:np, 2), PY_COMP, lb=1, ub=np)
    if (sp_loc%istracked()) then
     call pt%set_component(xx(1:np, 1), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component(xx(1:np, 2), OLD_PY_COMP, lb=1, ub=np)
    end if

   case (3)

    call sp_loc%compute_gamma()

    xx(1:np, 1) = sp_loc%call_component(PX_COMP, lb=1, ub=np) - &
    alp*pt%call_component(EX_COMP, lb=1, ub=np) - &
    alp*pt%call_component(BZ_COMP, lb=1, ub=np) * sp_loc%call_component(PY_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np) + &
    alp*pt%call_component(BY_COMP, lb=1, ub=np) * sp_loc%call_component(PZ_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np)

    xx(1:np, 2) = sp_loc%call_component(PY_COMP, lb=1, ub=np) - &
    alp*pt%call_component(EY_COMP, lb=1, ub=np) - &
    alp*pt%call_component(BX_COMP, lb=1, ub=np) * sp_loc%call_component(PZ_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np) + &
    alp*pt%call_component(BZ_COMP, lb=1, ub=np) * sp_loc%call_component(PX_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np)

    xx(1:np, 3) = sp_loc%call_component(PZ_COMP, lb=1, ub=np) - &
    alp*pt%call_component(EZ_COMP, lb=1, ub=np) - &
    alp*pt%call_component(BY_COMP, lb=1, ub=np) * sp_loc%call_component(PX_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np) + &
    alp*pt%call_component(BX_COMP, lb=1, ub=np) * sp_loc%call_component(PY_COMP, lb=1, ub=np) *&
    sp_loc%call_component(INV_GAMMA_COMP, lb=1, ub=np)

    call sp_loc%set_component(xx(1:np, 1), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component(xx(1:np, 2), PY_COMP, lb=1, ub=np)
    call sp_loc%set_component(xx(1:np, 3), PZ_COMP, lb=1, ub=np)
    if (sp_loc%istracked()) then
     call pt%set_component(xx(1:np, 1), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component(xx(1:np, 2), OLD_PY_COMP, lb=1, ub=np)
     call pt%set_component(xx(1:np, 3), OLD_PZ_COMP, lb=1, ub=np)
    end if

   end select

   initial_time_in = .false.
  end subroutine
  !==========================
  subroutine init_lpf_momenta_old(sp_loc, pt, np, ic)
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
  subroutine lpf_momenta_and_positions_new(sp_loc, pt, dt_in, np, ic, mempool)
   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: pt
   real (dp), intent(in) :: dt_in
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool

   !======================================
   ! Do not execute for immobile particles
   !======================================
   if ( .not. sp_loc%ismobile() ) return
   select case(pusher)
   case(HIGUERA)
    call higuera_pusher(sp_loc, pt, dt_in, np, ic, mempool)
   case(BORIS)
    call boris_pusher(sp_loc, pt, dt_in, np, ic, mempool)
   case default
    call higuera_pusher(sp_loc, pt, dt_in, np, ic, mempool)
   end select

  end subroutine

  subroutine boris_pusher_new(sp_loc, pt, dt_in, np, ic, mempool)
   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: pt
   real (dp), intent(in) :: dt_in
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: bb, pp
   integer :: p, nss, nstep, bbdim, narrs
   real (dp) :: alp, dt_lp, dth_lp
   real (dp), pointer, contiguous, dimension(:) :: gam, gam02, b2, bv, aux
   real (dp), pointer, contiguous, dimension(:) :: pyp, pxp, pzp
   !========================================
   ! Boris push 
   ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
   ! v^n=p^n/gamma^n
   !========================================
   !Enter Fields multiplied by particle charge
   dt_lp = dt_in
   dth_lp = 0.5*dt_lp
   ! Substepping variable, 1 by default
   nstep = n_substeps
   alp = dth_lp*lorentz_fact(ic)/nstep
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=============================================
   ! Nullify all pointers
   NULLIFY(bb)
   NULLIFY(pp)
   NULLIFY(gam)
   NULLIFY(gam02)
   NULLIFY(b2)
   NULLIFY(bv)
   NULLIFY(aux)
   NULLIFY(pyp)
   NULLIFY(pxp)
   NULLIFY(pzp)
   !=============================================
   call mp_xx_realloc( mempool%mp_xx_2d_A, np, sp_loc%pick_dimensions(), mempool)
   pp => mempool%mp_xx_2d_A
   !=============================================
   select case (curr_ndim)
   case (2)

    bbdim = 1
    narrs = 2

    ! For some (temporary) optimization,
    ! we store the rank 1 arrays for the computation in the strides
    ! of the usually already allocated 2d array
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim + narrs, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)
    bb => mempool%mp_xx_2d_B(:, 1:bbdim)
    gam02 => mempool%mp_xx_1d_A
    gam => mempool%mp_xx_1d_B
    b2 => mempool%mp_xx_1d_C
    pxp => mempool%mp_xx_2d_B(:, bbdim + 1)
    pyp => mempool%mp_xx_2d_B(:, bbdim + 2)

    pxp(1:np) = sp_loc%px(1:np)
    pyp(1:np) = sp_loc%py(1:np)
    ! If tracked, store old momenta
    if ( sp_loc%istracked() ) then
     call pt%set_component( pxp(1:np), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component( pyp(1:np), OLD_PY_COMP, lb=1, ub=np)
    end if

    ! bb = q*dt*B/2m
    bb(1:np, 1) = pt%aux6(1:np)*alp
    b2(1:np) = bb(1:np, 1) * bb(1:np, 1)

    do nss = 1, nstep
     !u^{-} = p_{n-1/2} + q*Lfact*(Ex,Ey,Bz)*Dt/2 in Boris push
     pp(1:np, 1) = pxp(1:np) + pt%aux1(1:np)*alp
     pp(1:np, 2) = pyp(1:np) + pt%aux2(1:np)*alp

     !gam0 in Boris push gam0^2 = 1 + (u^-)^2
     do p = 1, np
      gam02(p) = 1. + dot_product(pp(p, 1:2), pp(p, 1:2))
     end do

     gam(1:np) = sqrt(gam02(1:np))

     !p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     pxp(1:np) = 2.*(gam02(1:np)*pp(1:np, 1) + &
     gam(1:np)*pp(1:np, 2) * bb(1:np, 1))/ &
     (gam02(1:np) + b2(1:np)) - pxp(1:np)

     pyp(1:np) = 2.*(gam02(1:np)*pp(1:np, 2) - &
     gam(1:np)*pp(1:np, 1) * bb(1:np, 1))/ &
     (gam02(1:np) + b2(1:np)) - pyp(1:np)

    end do

    call sp_loc%set_component( pxp(1:np), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component( pyp(1:np), PY_COMP, lb=1, ub=np)
    
    call sp_loc%compute_gamma()

    !Stores old positions
    call pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call pt%set_component(dt_lp*sp_loc%gamma_inv(1:np), &
     OLD_GAMMA_COMP, lb=1, ub=np)

    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%px(1:np), VX_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%py(1:np), VY_COMP, lb=1, ub=np)

    pp(1:np, 1) = sp_loc%x(1:np) + pt%call_component(VX_COMP, lb=1, ub=np)
    pp(1:np, 2) = sp_loc%y(1:np) + pt%call_component(VY_COMP, lb=1, ub=np)

    call sp_loc%set_component(pp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 2), Y_COMP, lb=1, ub=np)

   case (3)

    bbdim = 3
    narrs = 4

    ! For some (temporary) optimization,
    ! we store the rank 1 arrays for the computation in the strides
    ! of the usually already allocated 2d array
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim + narrs, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)
    call array_realloc_1d( mempool%mp_xx_1d_D, np)
    bb => mempool%mp_xx_2d_B(:, 1:bbdim)
    gam02 => mempool%mp_xx_1d_A
    gam => mempool%mp_xx_1d_B
    b2 => mempool%mp_xx_1d_C
    bv => mempool%mp_xx_1d_D
    aux => mempool%mp_xx_2d_B(:, bbdim + 1)
    pxp => mempool%mp_xx_2d_B(:, bbdim + 2)
    pyp => mempool%mp_xx_2d_B(:, bbdim + 3)
    pzp => mempool%mp_xx_2d_B(:, bbdim + 4)

    pxp(1:np) = sp_loc%px(1:np)
    pyp(1:np) = sp_loc%py(1:np)
    pzp(1:np) = sp_loc%pz(1:np)

    if ( sp_loc%istracked() ) then
     call pt%set_component( pxp(1:np), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component( pyp(1:np), OLD_PY_COMP, lb=1, ub=np)
     call pt%set_component( pzp(1:np), OLD_PZ_COMP, lb=1, ub=np)
    end if

    ! bb = q*dt*B/2m
    bb(1:np, 1) = alp * pt%aux4(1:np)
    bb(1:np, 2) = alp * pt%aux5(1:np)
    bb(1:np, 3) = alp * pt%aux6(1:np)

    do nss = 1, nstep
     !u^{-} = p_{n-1/2} + q*Lfact*(Ex,Ey,Bz)*Dt/2 in Boris push
     pp(1:np, 1) = pxp(1:np) + pt%aux1(1:np)*alp
     pp(1:np, 2) = pyp(1:np) + pt%aux2(1:np)*alp
     pp(1:np, 3) = pzp(1:np) + pt%aux3(1:np)*alp

     do p = 1, np
      b2(p) = dot_product(bb(p, 1:3), bb(p, 1:3))
      bv(p) = dot_product(bb(p, 1:3), pp(p, 1:3))
     end do

     !gam0 in Boris push gam0^2 = 1 + (u^-)^2
     do p = 1, np
      gam02(p) = 1. + dot_product(pp(p, 1:3), pp(p, 1:3))
     end do

     gam(1:np) = sqrt(gam02(1:np))

     !============================================
     ! p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     !============================================

     ! New PX_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 1) + bb(1:np, 1)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 2)*bb(1:np, 3) - &
      pp(1:np, 3)*bb(1:np, 2)))/(b2(1:np) + gam02(1:np))

     pxp(1:np) = 2.*aux(1:np) - pxp(1:np)

     ! New PY_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 2) + bb(1:np, 2)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 3)*bb(1:np, 1) - &
      pp(1:np, 1)*bb(1:np, 3)))/(b2(1:np) + gam02(1:np))
     
     pyp(1:np) = 2.*aux(1:np) - pyp(1:np)

     ! New PZ_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 3) + bb(1:np, 3)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 1)*bb(1:np, 2) - &
      pp(1:np, 2)*bb(1:np, 1)))/(b2(1:np) + gam02(1:np))

     pzp(1:np) = 2.*aux(1:np) - pzp(1:np)

    end do

    call sp_loc%set_component( pxp(1:np), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component( pyp(1:np), PY_COMP, lb=1, ub=np)
    call sp_loc%set_component( pzp(1:np), PZ_COMP, lb=1, ub=np)

    !Updated momenta
    call sp_loc%compute_gamma()

    !Stores old positions
    call pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%z(1:np), OLD_Z_COMP, lb=1, ub=np)
    call pt%set_component(dt_lp*sp_loc%gamma_inv(1:np), &
     OLD_GAMMA_COMP, lb=1, ub=np)

    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%px(1:np), VX_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%py(1:np), VY_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%pz(1:np), VZ_COMP, lb=1, ub=np)

    pp(1:np, 1) = sp_loc%x(1:np) + pt%call_component(VX_COMP, lb=1, ub=np)
    pp(1:np, 2) = sp_loc%y(1:np) + pt%call_component(VY_COMP, lb=1, ub=np)
    pp(1:np, 3) = sp_loc%z(1:np) + pt%call_component(VZ_COMP, lb=1, ub=np)
    
    call sp_loc%set_component(pp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 2), Y_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 3), Z_COMP, lb=1, ub=np)

   end select

   !In comoving frame vbeam >0
   if (vbeam>0.) then
    call sp_loc%set_component(sp_loc%x(1:np) - dt_lp*vbeam, &
    X_COMP, lb=1, ub=np)
    call pt%set_component(pt%x(1:np) - dt_lp*vbeam, OLD_X_COMP, lb=1, ub=np)
   end if
  end subroutine

  subroutine higuera_pusher_new(sp_loc, pt, dt_in, np, ic, mempool)
   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: pt
   real (dp), intent(in) :: dt_in
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: bb, pp
   integer :: p, nss, nstep, bbdim, narrs
   real (dp) :: alp, dt_lp, dth_lp
   real (dp), pointer, contiguous, dimension(:) :: gam, gam02, b2, bv, aux
   real (dp), pointer, contiguous, dimension(:) :: pyp, pxp, pzp
   !========================================
   ! Higuera push
   ! uses exact explicit solution for
   ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
   ! v^n=p^n/gamma^n
   ! Higuera, POP, 2017
   !========================================
   !Enter Fields multiplied by particle charge
   dt_lp = dt_in
   dth_lp = 0.5*dt_lp
   ! Substepping variable, 1 by default
   nstep = n_substeps
   alp = dth_lp*lorentz_fact(ic)/nstep
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=============================================
   ! Nullify all pointers
   NULLIFY(bb)
   NULLIFY(pp)
   NULLIFY(gam)
   NULLIFY(gam02)
   NULLIFY(b2)
   NULLIFY(bv)
   NULLIFY(aux)
   NULLIFY(pyp)
   NULLIFY(pxp)
   NULLIFY(pzp)
   !=============================================
   call mp_xx_realloc( mempool%mp_xx_2d_A, np, sp_loc%pick_dimensions(), mempool)
   pp => mempool%mp_xx_2d_A
   !=============================================
   select case (curr_ndim)
   case (2)
    
    bbdim = 1
    narrs = 2

    ! For some (temporary) optimization,
    ! we store the rank 1 arrays for the computation in the strides
    ! of the usually already allocated 2d array
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim + narrs, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)
    bb => mempool%mp_xx_2d_B(:, 1:bbdim)
    gam02 => mempool%mp_xx_1d_A
    gam => mempool%mp_xx_1d_B
    b2 => mempool%mp_xx_1d_C
    pxp => mempool%mp_xx_2d_B(:, bbdim + 1)
    pyp => mempool%mp_xx_2d_B(:, bbdim + 2)

    pxp(1:np) = sp_loc%px(1:np)
    pyp(1:np) = sp_loc%py(1:np)
    ! If tracked, store old momenta
    if ( sp_loc%istracked() ) then
     call pt%set_component( pxp(1:np), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component( pyp(1:np), OLD_PY_COMP, lb=1, ub=np)
    end if

    ! bb = q*dt*B/2m
    bb(1:np, 1) = pt%aux6(1:np)*alp
    b2(1:np) = bb(1:np, 1) * bb(1:np, 1)

    do nss = 1, nstep
     !u^{-} = p_{n-1/2} + q*Lfact*(Ex,Ey,Bz)*Dt/2 in Higuera push
     pp(1:np, 1) = pxp(1:np) + pt%aux1(1:np)*alp
     pp(1:np, 2) = pyp(1:np) + pt%aux2(1:np)*alp

     !gam0 in Boris push gam0^2 = 1 + (u^-)^2
     do p = 1, np 
      gam02(p) = 1. + dot_product(pp(p, 1:2), pp(p, 1:2))
     end do
    
     !gam0 in Higuera push
     gam02(1:np) = gam02(1:np) - b2(1:np)
    
     !exact gam^2 solution
     gam02(1:np) = 0.5*(gam02(1:np) + sqrt(gam02(1:np) * gam02(1:np) + &
     4.*b2(1:np)))
    
     gam(1:np) = sqrt(gam02(1:np))
    
     !p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     pxp(1:np) = 2.*(gam02(1:np)*pp(1:np, 1) + &
      gam(1:np)*pp(1:np, 2) * bb(1:np, 1))/ &
      (gam02(1:np) + b2(1:np)) - pxp(1:np)

     pyp(1:np) = 2.*(gam02(1:np)*pp(1:np, 2) - &
      gam(1:np)*pp(1:np, 1) * bb(1:np, 1))/ &
      (gam02(1:np) + b2(1:np)) - pyp(1:np)

    end do

    call sp_loc%set_component( pxp(1:np), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component( pyp(1:np), PY_COMP, lb=1, ub=np)

    call sp_loc%compute_gamma()

    !Stores old positions
    call pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call pt%set_component(dt_lp*sp_loc%gamma_inv(1:np), &
     OLD_GAMMA_COMP, lb=1, ub=np)

    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%px(1:np), VX_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%py(1:np), VY_COMP, lb=1, ub=np)

    pp(1:np, 1) = sp_loc%x(1:np) + pt%call_component(VX_COMP, lb=1, ub=np)
    pp(1:np, 2) = sp_loc%y(1:np) + pt%call_component(VY_COMP, lb=1, ub=np)

    call sp_loc%set_component(pp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 2), Y_COMP, lb=1, ub=np)

   case (3)

    bbdim = 3
    narrs = 4

    ! For some (temporary) optimization,
    ! we store the rank 1 arrays for the computation in the strides
    ! of the usually already allocated 2d array
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim + narrs, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)
    call array_realloc_1d( mempool%mp_xx_1d_D, np)
    bb => mempool%mp_xx_2d_B(:, 1:bbdim)
    gam02 => mempool%mp_xx_1d_A
    gam => mempool%mp_xx_1d_B
    b2 => mempool%mp_xx_1d_C
    bv => mempool%mp_xx_1d_D
    aux => mempool%mp_xx_2d_B(:, bbdim + 1)
    pxp => mempool%mp_xx_2d_B(:, bbdim + 2)
    pyp => mempool%mp_xx_2d_B(:, bbdim + 3)
    pzp => mempool%mp_xx_2d_B(:, bbdim + 4)

    pxp(1:np) = sp_loc%px(1:np)
    pyp(1:np) = sp_loc%py(1:np)
    pzp(1:np) = sp_loc%pz(1:np)

    if ( sp_loc%istracked() ) then
     call pt%set_component( sp_loc%px(1:np), OLD_PX_COMP, lb=1, ub=np)
     call pt%set_component( sp_loc%py(1:np), OLD_PY_COMP, lb=1, ub=np)
     call pt%set_component( sp_loc%pz(1:np), OLD_PZ_COMP, lb=1, ub=np)
    end if

    ! bb = q*dt*B/2m
    bb(1:np, 1) = alp * pt%aux4(1:np)
    bb(1:np, 2) = alp * pt%aux5(1:np)
    bb(1:np, 3) = alp * pt%aux6(1:np)

    do nss = 1, nstep
     !u^{-} = p_{n-1/2} + q*Lfact*(Ex,Ey,Bz)*Dt/2 in Higuera push
     pp(1:np, 1) = pxp(1:np) + pt%aux1(1:np)*alp
     pp(1:np, 2) = pyp(1:np) + pt%aux2(1:np)*alp
     pp(1:np, 3) = pzp(1:np) + pt%aux3(1:np)*alp
    
     do p = 1, np
      b2(p) = dot_product(bb(p, 1:3), bb(p, 1:3))
      bv(p) = dot_product(bb(p, 1:3), pp(p, 1:3))
     end do
    
     do p = 1, np
      gam02(p) = 1. + dot_product(pp(p, 1:3), pp(p, 1:3))
     end do
    
     !gam0 in Higuera push
     gam02(1:np) = gam02(1:np) - b2(1:np)
    
     !exact gam^2 solution
     gam02(1:np) = 0.5*(gam02(1:np) + sqrt(gam02(1:np) * gam02(1:np) + &
     4.*(b2(1:np) + bv(1:np) * bv(1:np))))
    
     gam(1:np) = sqrt(gam02(1:np))
    
     !============================================
     ! p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     !============================================

     ! New PX_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 1) + bb(1:np, 1)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 2)*bb(1:np, 3) - &
      pp(1:np, 3)*bb(1:np, 2)))/(b2(1:np) + gam02(1:np))
    
     pxp(1:np) = 2.*aux(1:np) - pxp(1:np)
      
     ! New PY_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 2) + bb(1:np, 2)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 3)*bb(1:np, 1) - &
      pp(1:np, 1)*bb(1:np, 3)))/(b2(1:np) + gam02(1:np))

     pyp(1:np) = 2.*aux(1:np) - pyp(1:np)

     ! New PZ_COMP calculation
     aux(1:np) = gam02(1:np)*pp(1:np, 3) + bb(1:np, 3)*bv(1:np)
     aux(1:np) = (aux(1:np) + gam(1:np)*(pp(1:np, 1)*bb(1:np, 2) - &
      pp(1:np, 2)*bb(1:np, 1)))/(b2(1:np) + gam02(1:np))

     pzp(1:np) = 2.*aux(1:np) - pzp(1:np)

    end do

    call sp_loc%set_component( pxp(1:np), PX_COMP, lb=1, ub=np)
    call sp_loc%set_component( pyp(1:np), PY_COMP, lb=1, ub=np)
    call sp_loc%set_component( pzp(1:np), PZ_COMP, lb=1, ub=np)

    !Updated momenta
    call sp_loc%compute_gamma()

    !Stores old positions
    call pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call pt%set_component( sp_loc%z(1:np), OLD_Z_COMP, lb=1, ub=np)
    call pt%set_component(dt_lp*sp_loc%gamma_inv(1:np), &
     OLD_GAMMA_COMP, lb=1, ub=np)

    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%px(1:np), VX_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%py(1:np), VY_COMP, lb=1, ub=np)
    call pt%set_component( pt%gamma_inv(1:np) * sp_loc%pz(1:np), VZ_COMP, lb=1, ub=np)

    pp(1:np, 1) = sp_loc%x(1:np) + pt%call_component(VX_COMP, lb=1, ub=np)
    pp(1:np, 2) = sp_loc%y(1:np) + pt%call_component(VY_COMP, lb=1, ub=np)
    pp(1:np, 3) = sp_loc%z(1:np) + pt%call_component(VZ_COMP, lb=1, ub=np)

    call sp_loc%set_component(pp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 2), Y_COMP, lb=1, ub=np)
    call sp_loc%set_component(pp(1:np, 3), Z_COMP, lb=1, ub=np)

   end select

   !In comoving frame vbeam >0
   if (vbeam>0.) then
    call sp_loc%set_component(sp_loc%x(1:np) - dt_lp*vbeam, &
    X_COMP, lb=1, ub=np)
    call pt%set_component(pt%x(1:np) - dt_lp*vbeam, OLD_X_COMP, lb=1, ub=np)
   end if
  end subroutine

  !=============================
  subroutine lpf_momenta_and_positions_old(sp_loc, pt, np, ic)
   type (species), intent (inout) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np, ic

   select case(pusher)
   case(HIGUERA)
    call higuera_pusher(sp_loc, pt, np, ic)
   case(BORIS)
    call boris_pusher(sp_loc, pt, np, ic)
   case default
    call higuera_pusher(sp_loc, pt, np, ic)
   end select

  end subroutine

  subroutine boris_pusher_old(sp_loc, pt, np, ic)
   type (species), intent (inout) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)

   integer, intent (in) :: np, ic
   integer :: p, ch
   real (dp) :: alp, dt_lp, dth_lp, bb(3), pp(3), vp(3), vph(3), efp(6), &
     b2, bv, gam2, gam
   !========================================
   ! uses exact explicit solution for
   ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
   ! v^n=p^n/gamma^n
   !========================================
   !Enter Fields multiplied by particle charge
   dt_lp = dt_loc
   dth_lp = 0.5*dt_lp
   alp = dth_lp*lorentz_fact(ic)
   ch = -1
   select case (curr_ndim)
   case (2)
    ch = 5
    do p = 1, np
     pp(1:2) = sp_loc%part(p, 3:4) !p_{n-1/2}
     efp(1:3) = alp*pt(p, 1:3) !q*Lfact*(Ex,Ey,Bz)*Dt/2
     vp(1:2) = pp(1:2) + efp(1:2) !u^{-} in Boris push
     vp(3) = efp(3) !b_z
     gam2 = 1. + dot_product(vp(1:2), vp(1:2)) !gam0 in Boris push
     b2 = vp(3)*vp(3) !b_z*b_z
     gam = sqrt(gam2)
     !==============================
     !p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
     vph(1) = gam2*vp(1) + gam*vp(2)*vp(3)
     vph(2) = gam2*vp(2) - gam*vp(1)*vp(3)
     vph(1:2) = vph(1:2)/(gam2+b2)
     sp_loc%part(p, 3:4) = 2.*vph(1:2) - pp(1:2)
     !=========== the new momenta
     !        update positions
     pt(p, 3:4) = sp_loc%part(p, 1:2) !old positions stored
     pp(1:2) = sp_loc%part(p, 3:4)
     gam2 = 1. + dot_product(pp(1:2), pp(1:2))
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
     gam2 = 1. + dot_product(vp(1:3), vp(1:3)) !the lower order gamma in Boris scheme
     !=============================
     b2 = dot_product(bb(1:3), bb(1:3))
     bv = dot_product(bb(1:3), vp(1:3))
     gam = sqrt(gam2)
     !============================
     vph(1:3) = gam2*vp(1:3) + bb(1:3)*bv
     vph(1) = vph(1) + gam*(vp(2)*bb(3)-vp(3)*bb(2))
     vph(2) = vph(2) + gam*(vp(3)*bb(1)-vp(1)*bb(3))
     vph(3) = vph(3) + gam*(vp(1)*bb(2)-vp(2)*bb(1))
     vph(1:3) = vph(1:3)/(b2+gam2) !p_n=(p_{n+1/2)+p_{n-1/2})/2
     !======== advance momenta
     sp_loc%part(p, 4:6) = 2.*vph(1:3) - pp(1:3)
     !==========
     pt(p, 4:6) = sp_loc%part(p, 1:3) !stores old positions
     pp(1:3) = sp_loc%part(p, 4:6)
     gam2 = 1. + dot_product(pp(1:3), pp(1:3))
     pt(p, 7) = dt_lp/sqrt(gam2)
     vp(1:3) = pt(p, 7)*pp(1:3)
     pt(p, 1:3) = vp(1:3) !stores dt*V 
     sp_loc%part(p, 1:3) = sp_loc%part(p, 1:3) + vp(1:3) !new positions
    end do
   end select
   !====================
   if (iform<2) then
    !old charge stored for charge preserving schemes
    do p = 1, np
     pt(p, ch) = sp_loc%part(p, ch)
    end do
   end if
   !In comoving frame vbeam >0
   if (vbeam>0.) then
    do p = 1, np
     sp_loc%part(p, 1) = sp_loc%part(p, 1) - dt_lp*vbeam
     pt(p, 1) = pt(p, 1) - dt_lp*vbeam ! 
    end do
   end if
  end subroutine

  subroutine higuera_pusher_old(sp_loc, pt, np, ic)
   type (species), intent (inout) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)

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
   ch = -1
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
  subroutine lpf_env_momenta_new(sp_loc, f_pt, np, ic, mempool)

   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: f_pt
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: bb, pp
   integer :: p, bbdim
   real (dp), pointer, contiguous, dimension(:) :: b2, vp, bv
   real (dp) :: alp, dt_lp

   dt_lp = dt_loc
   alp = 0.5*dt_lp*lorentz_fact(ic)
   !==========================
   ! Enter F_pt(1:2)= q*(E+0.5q*grad[F]/gamp) and
   ! F_pt(3)=q*B/gamp     where F=|A|^2/2
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !======================================
   ! Do not execute for immobile particles
   !======================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================
   ! Nullify all pointers
   NULLIFY(bb)
   NULLIFY(pp)
   NULLIFY(b2)
   NULLIFY(bv)
   NULLIFY(vp)
   !=============================================
   call mp_xx_realloc( mempool%mp_xx_2d_A, np, sp_loc%pick_dimensions(), mempool)
   pp => mempool%mp_xx_2d_A
   !=============================================
   select case (curr_ndim)
   case (2)
    !F_pt(5)=wgh/gamp
    bbdim = 1
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    bb => mempool%mp_xx_2d_B
    b2 => mempool%mp_xx_1d_A
    vp => mempool%mp_xx_1d_B

    !u^{-} = p_{n-1/2} + Lz_fact*Dt/2
    pp(1:np, 1) = sp_loc%px(1:np) + f_pt%call_component( FX_COMP, lb=1, ub=np )*alp
    pp(1:np, 2) = sp_loc%py(1:np) + f_pt%call_component( FY_COMP, lb=1, ub=np )*alp

    bb(1:np, 1) = alp * f_pt%call_component( BZ_COMP, lb=1, ub=np )
    b2(1:np) = one_dp + bb(1:np, 1) * bb(1:np, 1)
    !==============================
    
    vp(1:np) = 2*(pp(1:np, 1) + pp(1:np, 2)*bb(1:np, 1))/b2(1:np) - &
     sp_loc%px(1:np)

    if ( sp_loc%istracked() ) then
     call f_pt%set_component( sp_loc%px(1:np), OLD_PX_COMP, lb=1, ub=np)
     call f_pt%set_component( sp_loc%py(1:np), OLD_PY_COMP, lb=1, ub=np)
    end if

    call sp_loc%set_component(vp(1:np), PX_COMP, lb=1, ub=np)
    vp(1:np) = 2*(pp(1:np, 2) - pp(1:np, 1)*bb(1:np, 1))/b2(1:np) - &
     sp_loc%py(1:np)
    call sp_loc%set_component(vp(1:np), PY_COMP, lb=1, ub=np)

    call f_pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call f_pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    !F_pt(5)=wgh/gamp unchanged
   case (3)

    bbdim = 3
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, bbdim, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)
    bb => mempool%mp_xx_2d_B
    b2 => mempool%mp_xx_1d_A
    vp => mempool%mp_xx_1d_B
    bv => mempool%mp_xx_1d_C

    !F_pt(7)=wgh/gamp
    pp(1:np, 1) = sp_loc%px(1:np) + f_pt%call_component( FX_COMP, lb=1, ub=np )*alp
    pp(1:np, 2) = sp_loc%py(1:np) + f_pt%call_component( FY_COMP, lb=1, ub=np )*alp
    pp(1:np, 3) = sp_loc%pz(1:np) + f_pt%call_component( FZ_COMP, lb=1, ub=np )*alp

    bb(1:np, 1) = alp * f_pt%call_component( BX_COMP, lb=1, ub=np )
    bb(1:np, 2) = alp * f_pt%call_component( BY_COMP, lb=1, ub=np )
    bb(1:np, 3) = alp * f_pt%call_component( BZ_COMP, lb=1, ub=np )
    !=============================
    ! The Boris pusher
    !=========================
    do p = 1, np
     b2(p) = 1. + dot_product(bb(p, 1:3), bb(p, 1:3))
     bv(p) = dot_product(bb(p, 1:3), pp(p, 1:3))
    end do
    vp(1:np) = 2*(pp(1:np, 1) + pp(1:np, 2)*bb(1:np, 3) - pp(1:np, 3)*bb(1:np, 2) +&
     bb(1:np, 1)*bv(1:np))/b2(1:np) - sp_loc%px(1:np)

    if ( sp_loc%istracked() ) then
     call f_pt%set_component( sp_loc%px(1:np), OLD_PX_COMP, lb=1, ub=np)
     call f_pt%set_component( sp_loc%py(1:np), OLD_PY_COMP, lb=1, ub=np)
     call f_pt%set_component( sp_loc%pz(1:np), OLD_PZ_COMP, lb=1, ub=np)
    end if

    call sp_loc%set_component(vp(1:np), PX_COMP, lb=1, ub=np)
    vp(1:np) = 2*(pp(1:np, 2) + pp(1:np, 3)*bb(1:np, 1) - pp(1:np, 1)*bb(1:np, 3) +&
     bb(1:np, 2)*bv(1:np))/b2(1:np) - sp_loc%py(1:np)
    call sp_loc%set_component(vp(1:np), PY_COMP, lb=1, ub=np)
    vp(1:np) = 2*(pp(1:np, 3) + pp(1:np, 1)*bb(1:np, 2) - pp(1:np, 2)*bb(1:np, 1) +&
     bb(1:np, 3)*bv(1:np))/b2(1:np) - sp_loc%pz(1:np)
    call sp_loc%set_component(vp(1:np), PZ_COMP, lb=1, ub=np)

    !stores old positions
    call f_pt%set_component( sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call f_pt%set_component( sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call f_pt%set_component( sp_loc%z(1:np), OLD_Z_COMP, lb=1, ub=np)
    !F_pt(7)=wgh/gamp unchanged
   end select
  end subroutine
  !======================
  subroutine lpf_env_momenta_old(sp_loc, f_pt, np, ic)

   type (species), intent (inout) :: sp_loc
   real (dp), intent (inout) :: f_pt(:, :)
   integer, intent (in) :: np, ic

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
  subroutine lpf_env_positions_new(sp_loc, f_pt, np, mempool)

   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: f_pt
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: vp, pp
   integer, intent (in) :: np
   integer :: p
   real (dp), pointer, contiguous, dimension(:) :: ff, b2, gam_inv
   real (dp) :: dt_lp, dth_lp

   dt_lp = dt_loc
   dth_lp = 0.5*dt_lp
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !======================================
   ! Do not execute for immobile particles
   !======================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================
   ! Nullify all pointers
   NULLIFY(vp)
   NULLIFY(pp)
   NULLIFY(ff)
   NULLIFY(b2)
   NULLIFY(gam_inv)
   !=============================================
   call mp_xx_realloc( mempool%mp_xx_2d_A, np, sp_loc%pick_dimensions(), mempool)
   pp => mempool%mp_xx_2d_A
   !=============================================
   !==========================
   select case (curr_ndim)
   !============  enter F_pt(3)=F, F_pt (1:2) Grad[F] where F=|A|^2/2
   !             at time level t^{n+1/2} assigned to the x^n positions
   case (2)

    call mp_xx_realloc( mempool%mp_xx_2d_B, np, 2, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)

    vp => mempool%mp_xx_2d_B
    gam_inv => mempool%mp_xx_1d_A
    b2 => mempool%mp_xx_1d_B
    ff => mempool%mp_xx_1d_C

    pp(1:np, 1) = sp_loc%px(1:np) !p^{n+1/2}
    pp(1:np, 2) = sp_loc%py(1:np) !p^{n+1/2}
    vp(1:np, 1) = f_pt%call_component( GRADF_X_COMP, lb=1, ub=np) !grad[F]_x
    vp(1:np, 2) = f_pt%call_component( GRADF_Y_COMP, lb=1, ub=np) !grad[F]_y
    !=============================
    ff(1:np) = f_pt%call_component( POND_COMP, lb=1, ub=np )
    do p = 1, np
     gam_inv(p) = one_dp + dot_product(pp(p, 1:2), pp(p, 1:2)) + ff(p)
     b2(p) = 0.25*dot_product(pp(p, 1:2), vp(p, 1:2))
    end do
    gam_inv(1:np) = sqrt(gam_inv(1:np))
    !=========================== def gamma_p
    gam_inv(1:np) = one_dp/gam_inv(1:np)
    gam_inv(1:np) = gam_inv(1:np)*(1.-dt_lp*b2(1:np) * &
     gam_inv(1:np)*gam_inv(1:np)*gam_inv(1:np))
    
    call sp_loc%set_component(gam_inv(1:np), INV_GAMMA_COMP, lb=1, ub=np)
    !============================
    call f_pt%set_component(gam_inv(1:np)*dt_lp, OLD_GAMMA_COMP, lb=1, ub=np)
    call f_pt%set_component(sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call f_pt%set_component(sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    vp(1:np, 1) = dt_lp*gam_inv(1:np)*pp(1:np, 1)
    vp(1:np, 2) = dt_lp*gam_inv(1:np)*pp(1:np, 2)
    
    !dt*V^{n+1/2}  velocities
    call f_pt%set_component(vp(1:np, 1), VX_COMP, lb=1, ub=np)
    call f_pt%set_component(vp(1:np, 2), VY_COMP, lb=1, ub=np)
    call sp_loc%set_component(sp_loc%x(1:np) + vp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(sp_loc%y(1:np) + vp(1:np, 2), Y_COMP, lb=1, ub=np)
   case (3)
    !============enter F_pt(4)=F, F_pt (1:3) Grad[F] where F=|A|^2/2 at t^{n+1/2}
    ! assigned at x^n

    call mp_xx_realloc( mempool%mp_xx_2d_B, np, 3, mempool)
    call array_realloc_1d( mempool%mp_xx_1d_A, np)
    call array_realloc_1d( mempool%mp_xx_1d_B, np)
    call array_realloc_1d( mempool%mp_xx_1d_C, np)

    vp => mempool%mp_xx_2d_B
    gam_inv => mempool%mp_xx_1d_A
    b2 => mempool%mp_xx_1d_B
    ff => mempool%mp_xx_1d_C

    pp(1:np, 1) = sp_loc%px(1:np) !p^{n+1/2}
    pp(1:np, 2) = sp_loc%py(1:np) !p^{n+1/2}
    pp(1:np, 3) = sp_loc%pz(1:np) !p^{n+1/2}
    vp(1:np, 1) = f_pt%call_component( GRADF_X_COMP, lb=1, ub=np) !grad[F]_x
    vp(1:np, 2) = f_pt%call_component( GRADF_Y_COMP, lb=1, ub=np) !grad[F]_y
    vp(1:np, 3) = f_pt%call_component( GRADF_Z_COMP, lb=1, ub=np) !grad[F]_y
    !=============================
    ff(1:np) = f_pt%call_component( POND_COMP, lb=1, ub=np )
    do p = 1, np
     gam_inv(p) = one_dp + dot_product(pp(p, 1:3), pp(p, 1:3)) + ff(p)
     b2(p) = 0.25*dot_product(pp(p, 1:3), vp(p, 1:3))
    end do
    gam_inv(1:np) = sqrt(gam_inv(1:np))
    !=============================
    gam_inv(1:np) = one_dp/gam_inv(1:np)
    gam_inv(1:np) = gam_inv(1:np)*(1.-dt_lp*b2(1:np) * &
     gam_inv(1:np)*gam_inv(1:np)*gam_inv(1:np))

    call sp_loc%set_component(gam_inv(1:np), INV_GAMMA_COMP, lb=1, ub=np)

    vp(1:np, 1) = dt_lp*gam_inv(1:np)*pp(1:np, 1)
    vp(1:np, 2) = dt_lp*gam_inv(1:np)*pp(1:np, 2)
    vp(1:np, 3) = dt_lp*gam_inv(1:np)*pp(1:np, 3)

    call f_pt%set_component(sp_loc%x(1:np), OLD_X_COMP, lb=1, ub=np)
    call f_pt%set_component(sp_loc%y(1:np), OLD_Y_COMP, lb=1, ub=np)
    call f_pt%set_component(sp_loc%z(1:np), OLD_Z_COMP, lb=1, ub=np)

    call f_pt%set_component(gam_inv(1:np)*dt_lp, OLD_GAMMA_COMP, lb=1, ub=np)
    !dt*V^{n+1/2}  velocities
    call f_pt%set_component(vp(1:np, 1), VX_COMP, lb=1, ub=np)
    call f_pt%set_component(vp(1:np, 2), VY_COMP, lb=1, ub=np)
    call f_pt%set_component(vp(1:np, 3), VZ_COMP, lb=1, ub=np)
    call sp_loc%set_component(sp_loc%x(1:np) + vp(1:np, 1), X_COMP, lb=1, ub=np)
    call sp_loc%set_component(sp_loc%y(1:np) + vp(1:np, 2), Y_COMP, lb=1, ub=np)
    call sp_loc%set_component(sp_loc%z(1:np) + vp(1:np, 3), Z_COMP, lb=1, ub=np)

   end select

   !====================== vb=-wbet > 0 in comoving x-coordinate
   if (vbeam > zero_dp) then
    call sp_loc%set_component(sp_loc%call_component(X_COMP, lb=1, ub=np) - dt_lp*vbeam, &
     X_COMP, lb=1, ub=np)
    call f_pt%set_component(f_pt%call_component(OLD_X_COMP, lb=1, ub=np) - dt_lp*vbeam, &
     OLD_X_COMP, lb=1, ub=np)
   end if
  end subroutine
  !=====================
  subroutine lpf_env_positions_old(sp_loc, f_pt, np)

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


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

 module env_evolve
  use window, only: lp_window_xshift, comoving_coordinate
  use boris_push
  use curr_and_fields_util
  use mpi_part_interface, only: cell_part_dist
  use ionize, only: set_field_ioniz_wfunction, ionization_cycle, de_inv
  use fluid_density_momenta, only: fluid_curr_accumulate, &
                                   set_env_momentum_density_flux, update_adam_bash_fluid_variables
  use util, only: init_random_seed

  implicit none
  !===============================
 contains
  !============================
  ! ENVELOPE model in LP regime
  !============================
  subroutine env_den_collect(source_in)

   real(dp), intent(inout) :: source_in(:, :, :, :)
   integer :: i, j, k, kk, jj, ic
   real(dp) :: dery, derz

   ic = 1
   if (prl) then
    call fill_curr_yzxbdsdata(source_in, ic)
   end if
   call den_zyxbd(source_in, ic)
   !=======Enters normalized <w*n/gam> > 0
   !==================================
   if (stretch) then
    ic = 1
    if (ndim == 2) then
     k = 1
     do j = jy1, jy2
      jj = j - 2
      dery = loc_yg(jj, 3, imody)
      do i = ix1, ix2
       source_in(i, j, k, ic) = dery*source_in(i, j, k, ic)
      end do
     end do
     return
    end if
    do k = kz1, kz2
     kk = k - 2
     derz = loc_zg(kk, 3, imodz)
     do j = jy1, jy2
      jj = j - 2
      dery = derz*loc_yg(jj, 3, imody)
      do i = ix1, ix2
       source_in(i, j, k, ic) = dery*source_in(i, j, k, ic)
      end do
     end do
    end do
   end if
   !=========================
   !exit in source_in(ic)  the source terms chi() >0 of the envelope equation
   !-----------------------------------------------
  end subroutine
  !=========================
  subroutine env_two_fields_average(evf, ev1f, av, spl_in, spr_in)
   real(dp), intent(in) :: evf(:, :, :, :), ev1f(:, :, :, :)
   real(dp), intent(out) :: av(:, :, :, :)
   integer, intent(in) :: spl_in, spr_in
   integer :: ix, iy, iz
   real(dp) :: ar, ai
   !===================
   do iz = kz1, kz2
    do iy = jy1, jy2
     do ix = ix1, ix2
      ar = 0.5*(evf(ix, iy, iz, 1) + evf(ix, iy, iz, 3)) !A^{n+1/2}=(A^n+1+A^n)/2
      ai = 0.5*(evf(ix, iy, iz, 2) + evf(ix, iy, iz, 4))
      av(ix, iy, iz, 1) = 0.5*(ar*ar + ai*ai)
      ar = 0.5*(ev1f(ix, iy, iz, 1) + ev1f(ix, iy, iz, 3)) !A^{n+1/2}=(A^n+1+A^n)/2
      ai = 0.5*(ev1f(ix, iy, iz, 2) + ev1f(ix, iy, iz, 4))
      av(ix, iy, iz, 1) = av(ix, iy, iz, 1) + 0.5*(ar*ar + ai*ai)
      ! |A|^2/2 at t^{n+1/2}=> gamp^{n+1/2}
      !  NO overlap assumed
     end do
    end do
   end do
   if (prl) call fill_ebfield_yzxbdsdata(av, 1, 1, spr_in, spl_in)
   !call field_xyzbd(av,i1,i2,j1,j2,k1,k2,1,spr_in,spl_in)
   !=====================
  end subroutine
  !===========================
  subroutine env_fields_average(evf, av, spl_in, spr_in)
   real(dp), intent(in) :: evf(:, :, :, :)
   real(dp), intent(out) :: av(:, :, :, :)
   integer, intent(in) :: spl_in, spr_in
   integer :: ix, iy, iz, ord
   real(dp) :: ar, ai
   real(dp), parameter :: frac = one_dp/8.
   !===================
   ord = 2
   ! |A|^2/2 at t^{n+1/2}=> gamp^{n+1/2}
   av(ix1:ix2, jy1:jy2, kz1:kz2, 1) = &
    frac*( &
    (evf(ix1:ix2, jy1:jy2, kz1:kz2, 1) + evf(ix1:ix2, jy1:jy2, kz1:kz2, 3))* &
    (evf(ix1:ix2, jy1:jy2, kz1:kz2, 1) + evf(ix1:ix2, jy1:jy2, kz1:kz2, 3)) + &
    (evf(ix1:ix2, jy1:jy2, kz1:kz2, 2) + evf(ix1:ix2, jy1:jy2, kz1:kz2, 4))* &
    (evf(ix1:ix2, jy1:jy2, kz1:kz2, 2) + evf(ix1:ix2, jy1:jy2, kz1:kz2, 4)))

   if (prl) call fill_ebfield_yzxbdsdata(av, 1, 1, spr_in, spl_in)
   call env_grad(av)
   !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

   if (prl) call fill_ebfield_yzxbdsdata(av, 2, curr_ndim + 1, spr_in, &
                                         spl_in)
   !=====================
  end subroutine
  !===========================
  subroutine env_amp_prepare(envf, av, ord, spl_in, spr_in)
   real(dp), intent(in) :: envf(:, :, :, :)
   real(dp), intent(out) :: av(:, :, :, :)
   integer, intent(in) :: ord, spl_in, spr_in
   integer :: spl, spr
   !real(dp) :: ar,ai
   !===================
   !|A|^2/2 at current t^n time level
   av(ix1:ix2, jy1:jy2, kz1:kz2, 1) = 0.5* &
                                      (envf(ix1:ix2, jy1:jy2, kz1:kz2, 1)*envf(ix1:ix2, jy1:jy2, kz1:kz2, 1) + &
                                       envf(ix1:ix2, jy1:jy2, kz1:kz2, 2)*envf(ix1:ix2, jy1:jy2, kz1:kz2, 2))
   spl = spl_in
   spr = spr_in
   if (spl > 2) spl = 2
   if (spr > 2) spr = 2

   if (prl) call fill_ebfield_yzxbdsdata(av, 1, 1, spr, spl)

   call env_grad(av)
   !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

   if (prl) call fill_ebfield_yzxbdsdata(av, 1, curr_ndim + 1, spr, spl)

   !call field_xyzbd(av,i1,i2,j1,j2,k1,k2,nj_dim,spr,spl)
   !=====================
  end subroutine
  !=============================
  subroutine env_amp_two_fields_prepare(envf, env1f, av, ord, spl_in, &
                                        spr_in)
   real(dp), intent(in) :: envf(:, :, :, :), env1f(:, :, :, :)
   real(dp), intent(out) :: av(:, :, :, :)
   integer, intent(in) :: ord, spl_in, spr_in
   integer :: ix, iy, iz, spl, spr
   !real(dp) :: ar,ai
   !===================
   do iz = kz1, kz2
    do iy = jy1, jy2
     do ix = ix1, ix2
      av(ix, iy, iz, 1) = 0.5*(envf(ix, iy, iz, 1)*envf(ix, iy, iz, 1) + envf(ix &
                                                                              , iy, iz, 2)*envf(ix, iy, iz, 2))
      av(ix, iy, iz, 1) = av(ix, iy, iz, 1) + 0.5*(env1f(ix, iy, iz, 1)* &
                                                   env1f(ix, iy, iz, 1) + env1f(ix, iy, iz, 2)*env1f(ix, iy, iz, 2))
      !|A|^2/2 at current t^n time level
     end do
    end do
   end do
   spl = spl_in
   spr = spr_in
   if (spl > 2) spl = 2
   if (spr > 2) spr = 2

   if (prl) call fill_ebfield_yzxbdsdata(av, 1, 1, spr, spl)

   call env_grad(av)
   !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

   if (prl) call fill_ebfield_yzxbdsdata(av, 2, curr_ndim + 1, spr, spl)

   !call field_xyzbd(av,nj_dim,spr,spl)
   !=====================
  end subroutine
  !=======================================
  subroutine env_lpf2_evolve(it_loc)

   integer, intent(in) :: it_loc
   integer :: np, ic, id_ch
   real(dp) :: ef2_ion, loc_ef2_ion(2)
   logical, parameter :: mw = .false.
   !============================
   ef2_ion = zero_dp
   !====================
   if (prl) call fill_ebfield_yzxbdsdata(ebf, 1, nfield, 2, 2)
   !======================================
   if (ionization) then
    if (it_loc == 0) then
     call init_random_seed(mype)
    end if
    id_ch = nd2 + 1
    if (enable_ionization(1)) then
     if (prl) call pfields_prepare(env, 2, 2, 2)
     do ic = 2, nsp_ionz
      np = loc_npart(imody, imodz, imodx, ic)
      if (np > 0) then
       call set_ion_env_field(env, spec(ic), ebfp, np, oml)
       if (mod(it_loc, 100) == 0) then
        loc_ef2_ion(1) = maxval(ebfp(1:np, id_ch))
        loc_ef2_ion(1) = sqrt(loc_ef2_ion(1))
        ef2_ion = max(loc_ef2_ion(1), ef2_ion)
        if (ef2_ion > lp_max) then
         lp_max = 1.1*ef2_ion
         call set_field_ioniz_wfunction(ion_min(ic - 1), &
                                        atomic_number(ic - 1), ic, ionz_lev, ionz_model, lp_max)
        end if
       end if
       call ionization_cycle(spec(ic), ebfp, np, ic, it_loc, 1, de_inv)
      end if
     end do
    end if
    if (Two_color) then
     if (enable_ionization(2)) then
      if (prl) call pfields_prepare(env1, 2, 2, 2)
      do ic = 2, nsp_ionz
       np = loc_npart(imody, imodz, imodx, ic)
       if (np > 0) then
        call set_ion_env_field(env1, spec(ic), ebfp, np, om1)
        if (mod(it_loc, 100) == 0) then
         loc_ef2_ion(1) = maxval(ebfp(1:np, id_ch))
         loc_ef2_ion(1) = sqrt(loc_ef2_ion(1))
         ef2_ion = max(loc_ef2_ion(1), ef2_ion)
         if (ef2_ion > lp_max) then
          write (6, '(a22,i6,2E11.4)') 'reset high ionz field ', mype, &
           ef2_ion, lp_max
          lp_max = 1.1*ef2_ion
          call set_field_ioniz_wfunction(ion_min(ic - 1), &
                                         atomic_number(ic - 1), ic, ionz_lev, ionz_model, lp_max)
         end if
        end if
        call ionization_cycle(spec(ic), ebfp, np, ic, it_loc, 1, de_inv)
       end if
      end do
     end if
    end if
   end if
   !=================================
   ic = 1
   !===========================
   jc(:, :, :, :) = 0.0
   np = loc_npart(imody, imodz, imodx, ic)
   if (Two_color) then
    call env_amp_two_fields_prepare(env, env1, jc, 2, 2, 2)
   else
    call env_amp_prepare(env, jc, 2, 2, 2)
   end if
   !======================================
   ! exit jc(1)=|a|^2/2 at t^n
   !      jc(2:4)=grad|a|^2/2 at t^n
   ! For two-color |A|= |A_0|+|A_1|
   !======================================
   ebfp(:, :) = 0.0
   call set_env_acc(ebf, jc, spec(ic), ebfp, np, dt_loc)
   !=====================================
   !exit ebfp(1:3)=q*[E+F] ebfp(4:6)=q*B/gamp, ebfp(7)=wgh/gamp at t^n
   !Lorentz force already multiplied by particle charge
   !jc(1:4) not modified
   !====================
   call lpf_env_momenta(spec(ic), ebfp, np, ic)
   ! Updates particle momenta P^{n-1/2} => P^{n+1/2}
   ! stores in ebfp(1:3)=old (x,y,z)^n ebfp(7)=wgh/gamp >0
   !======================
   if (hybrid) then
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++
    call set_env_momentum_density_flux(up, ebf, jc, ebf0, flux)
    !exit jc(1)=q^2*n/gam, jc(2:4) ponderomotive force on a grid
    !ebf0= total fields flux(1:4)=(P,den)^n
    !============================
    call update_adam_bash_fluid_variables(up, up0, flux, ebf0)
    ! In up exit updated momenta-density variables u^{n+1}
    ! in  u0^{n} stores Dt*F(u^n), in flux(1:fdim)=(P,den)^{n+1/2}

    flux(:, :, :, curr_ndim + 2) = jc(:, :, :, 1)

    ! in flux(fdim+1) exit the fluid contribution of the sorce term q^2*n/gam
    ! for the envelope field solver
   end if
   jc(:, :, :, 1) = 0.0
   call set_env_density(ebfp, jc, np, 1)
   call env_den_collect(jc)
   ! in jc(1)the particle contribution of the source term <q^2*n/gamp>
   ! to be added to the fluid contribution if (Hybrid)
   jc(:, :, :, 3) = jc(:, :, :, 1)
   if (hybrid) then
    jc(:, :, :, 3) = jc(:, :, :, 3) + flux(:, :, :, curr_ndim + 2)
   end if
   !===================
   ! in the envelope equation (A^{n-1},A^n)==> (A^n,A^{n+1})
   ! jc(3) = <q^2n/gam>
   ! Jc(1:2)=-ompe*jc(3)*A at level t^n
   !==================================================
   !==================================================
   call advance_lpf_envelope(jc, env, oml)
   !advance (A^n, J^n) => A^{n+1}, A^{n-1}=> A^n
   ! jc(3) not modified
   if (Two_color) call advance_lpf_envelope(jc, env1, om1)
   !advance (A_1^n, J^n) => A_1^{n+1}, A_1^{n-1}=> A_1^n
   !=======================
   if (Two_color) then
    call env_two_fields_average(env, env1, jc, 2, 2)
   else
    call env_fields_average(env, jc, 2, 2)
   end if
   ! In jc(1)= Phi= |A|^2/2 +|A_1|/2 at t^{n+1/2}
   if (hybrid) then
    flux(:, :, :, curr_ndim + 2) = jc(:, :, :, 1)
    !stores in flux()
   end if
   call set_env_grad_interp(jc, spec(ic), ebfp, np, curr_ndim)
   !=============================
   ! Exit p-interpolated |A| field variables
   ! at time level t^{n+1/2} and positions at time t^n
   ! in ebfp(1:3)=grad|A|^2/2 ebfp(4)=|A|^2/2 in 3D
   ! in ebfp(1:2)=grad|A|^2/2 ebfp(3)=|A|^2/2 in 2D
   !=====================================
   call lpf_env_positions(spec(ic), ebfp, np)
   !===========================
   ! ebfp(1:3) dt*V^{n+1/2}  ebfp(4:6) old positions for curr J^{n+1/2}
   ! ebfp(7)=dt*gam_inv
   if (part) call cell_part_dist(mw)
!  particle number has changed
   np = loc_npart(imody, imodz, imodx, ic)
   !=======collects in jc(1:curr_ndim) currents due to electrons
   jc(:, :, :, :) = 0.0
   call curr_accumulate(spec(ic), ebfp, jc, np)
   !===========================
   call curr_mpi_collect(jc)
   if (hybrid) then
    !In flux(1:curr_ndim+1) are stored fluid (P,den) at t^{n+1/2}
    !In flux(curr_ndim+2) is stored |A|^2/2 at t^{n+1/2}

    call fluid_curr_accumulate(flux, jc)

    !Computes fluid contribution => J^{n+1/2} and adds to particle contribution
   end if
   !====================
   ! Jc(1:3) for total curr Dt*J^{n+1/2}
   call advance_lpf_fields(ebf, jc, 0)
   ! (E,B) fields at time t^{n+1}
   !-----------------------------
  end subroutine
  !=============== END ENV PIC SECTION
  subroutine env_run(t_loc, iter_loc)

   real(dp), intent(in) :: t_loc
   integer, intent(in) :: iter_loc

   !=========================
   call env_lpf2_evolve(iter_loc)
   !================================
   !+++++++++++++++++++++++++++++++++
   !for vbeam >0 uses the xw=(x+vbeam*t)
   !x=xi=(xw-vbeam*t) fixed
   !+++++++++++++++++++++++++++++++++
   if (w_speed > 0.0) then ! moves the computational box with w_speed>0.
    if (t_loc >= wi_time) then
     if (t_loc < wf_time) then
      if (mod(iter_loc, w_sh) == 0) then
       call lp_window_xshift(w_sh, iter_loc)
      end if
     end if
    end if
   end if
   if (comoving) then
    if (t_loc >= wi_time) then
     if (t_loc < wf_time) then
      if (mod(iter_loc, w_sh) == 0) then
       call comoving_coordinate(vbeam, w_sh, iter_loc)
      end if
     end if
    end if
   end if
  end subroutine
  !============================
  ! END ENVELOPE MODULE
  !==============================
 end module

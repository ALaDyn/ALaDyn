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

 module pic_out_util

  use grid_part_util
  use mpi_curr_interface
  use mpi_field_interface
  use psolve
  use phys_param, only: electron_mass

  implicit none
  !=====Contains functions to prepare selected output variables=======
 contains
  !=============== Tracking particles============
  subroutine initial_tparticles_select(tx1, ty1)

   real(dp), intent(in) :: tx1, ty1

   integer :: np, p, ik, ndv, ik_max
   integer(hp_int) :: plab, last_ind
   integer, parameter :: tp_numb = 10
   real(dp) :: yy(tp_numb), ym, ymx
   !========================
   ! Define track_tot_nstep
   p = 0
   if (dt_loc > 0.0) p = nint((t_out - t_in)/dt_loc)
   track_tot_nstep = nint(real(p, dp)/real(tkjump, dp))
   ndv = nd2 + 1

   ! Select particles on each mpi_task
   ! xp defined by tx1
   ! yp [-4,-3,-2.-1.,0,1,2,3,4], 9 test particles allocated
   !
   yy(1) = ty1
   do p = 2, tp_numb
    yy(p) = yy(p - 1) + 1.
   end do
   ym = loc_ygrid(imody)%gmin
   ymx = loc_ygrid(imody)%gmax
   np = loc_npart(imody, imodz, imodx, 1)
   ik = 0
   if (allocated(spec(1)%part)) then
    deallocate (spec(1)%part)
    deallocate (ebfp)
    loc_npart(imody, imodz, imodx, 1) = 0
   end if
   select case (ndim)
   case (2)
    do p = 1, tp_numb
     if (ym < yy(p) .and. ymx >= yy(p)) then
      ik = ik + 1
     end if
    end do
    if (ik > 0) then
     loc_npart(imody, imodz, imodx, 1) = ik
     allocate (spec(1)%part(ik, 5))
     np = 0
     do p = 1, tp_numb
      if (ym < yy(p) .and. ymx >= yy(p)) then
       np = np + 1
       spec(1)%part(np, 1) = tx1
       spec(1)%part(np, 2) = yy(p)
       spec(1)%part(np, 3:4) = t0_pl(1)
       part_ind = int(np, hp_int)
       wgh = real(0.0, sp)
       charge = int(-1., hp_int)
       spec(1)%part(np, 5) = wgh_cmp
      end if
     end do
    end if
   case (3)
    return
   end select
   np = loc_npart(imody, imodz, imodx, 1)
   loc_tpart(mype + 1) = np
   call intvec_distribute(np, loc_tpart, npe)
   track_tot_part = sum(loc_tpart(1:npe))
   ik_max = maxval(loc_tpart(1:npe))
   last_ind = 0
   if (mype == 1) last_ind = loc_tpart(1)
   if (mype > 1) last_ind = sum(loc_tpart(1:mype))
   !if(loc_tpart(mype+1)>0)write(6,*)'last particle index',mype,last_ind
   if (np > 0) then
    do p = 1, np
     wgh_cmp = spec(1)%part(p, ndv)
     if (part_ind > 0) part_ind = part_ind + last_ind
     spec(1)%part(p, ndv) = wgh_cmp
    end do
   end if
   allocate (track_aux(2*ndv*ik_max))
   if (.not. allocated(ebfp)) allocate (ebfp(ik_max, ndv))
   if (pe0) then
    allocate (pdata_tracking(ndv, track_tot_part, track_tot_nstep))
    write (6, *) '==== Initial track-Particle data==========='
    write (6, '(a19,i6)') '  tot_track_steps  ', track_tot_nstep
    write (6, '(a19,2i6)') '  tot_track_parts  ', track_tot_part, &
     tp_numb
    write (6, '(a18,i8)') '  short_int size  ', huge(plab)
    write (6, '(a20,e11.4)') '  track memory(MB) ', &
     1.e-06*real(4*ndv*track_tot_nstep*track_tot_part, dp)
   end if
  end subroutine
  !============================================
  subroutine t_particles_collect(time_ind)

   integer, intent(in) :: time_ind

   integer :: np, ik, ik_max, ip, p, ndv, ndvp, ipe, kk, ik1, ik2, nst
   logical :: sr
   real :: xm, ym, zm

   if (time_ind > track_tot_nstep) return
   xm = loc_xgrid(imodx)%gmin
   ym = loc_ygrid(imody)%gmin
   zm = loc_zgrid(imodz)%gmin
   np = loc_npart(imody, imodz, imodx, 1)
   nst = 0
   ndv = nd2 + 1
   ndvp = ndv
   if (stretch) nst = str_indx(imody, imodz)
   !===================
   np = loc_npart(imody, imodz, imodx, 1)
   ik = 0
   !=========== each pe counts track particle number
   do p = 1, np
    wgh_cmp = spec(1)%part(p, ndv)
    if (part_ind > 0) ik = ik + 1
   end do
   loc_tpart(mype + 1) = ik
   call intvec_distribute(ik, loc_tpart, npe)
   ik_max = maxval(loc_tpart(1:npe))
   if (ndvp*ik_max > size(track_aux)) then
    deallocate (track_aux)
    allocate (track_aux(ndvp*ik_max))
   end if
   !========= each pe stores tpart data in track_aux(ndv,loc_tpart)
   kk = 0
   do p = 1, np
    wgh_cmp = spec(1)%part(p, ndv)
    if (part_ind > 0) then
     do ip = 1, ndv
      kk = kk + 1
      track_aux(kk) = spec(1)%part(p, ip)
     end do
    end if
   end do
   !=================
   !  all data are gathered onto pe0
   if (pe0) then
    sr = .false.
    ik1 = 0
    do ipe = 1, npe - 1
     ik = loc_tpart(ipe + 1)
     if (ik > 0) then
      call exchange_1d_grdata(sr, track_aux, ik*ndvp, ipe, ipe + 10)
      !pe0 receives from ipe ik sp_aux data and collects on track array
      kk = 0
      do p = 1, ik
       ik2 = ik1 + p
       do ip = 1, ndv - 1
        kk = kk + 1
        pdata_tracking(ip, ik2, time_ind) = track_aux(kk)
        !pdata_tracking(ip,ik2,time_ind)=track_aux(kk)
       end do
       kk = kk + 1
       wgh_cmp = track_aux(kk)
       pdata_tracking(ndv, ik2, time_ind) = real(part_ind, dp)
      end do
      ik1 = ik2
     end if
    end do
    loc_tpart(1) = ik2
   else
    ik = loc_tpart(mype + 1)
    if (ik > 0) then
     sr = .true.
     call exchange_1d_grdata(sr, track_aux, ik*ndvp, 0, mype + 10) !sends ik data to pe0
    end if
   end if
  end subroutine
  !=================================================
  subroutine fill_density_data(den, ic)
   real(dp), intent(inout) :: den(:, :, :, :)
   integer, intent(in) :: ic
   integer :: i, j, k, iy, iz, n_loc

   n_loc = loc_ygrid(imody)%ng
   do k = kz1, kz2
    do j = jy1, jy2 - 1
     iy = j + imody*n_loc
     if (y(iy) > ymin_t .and. y(iy) < ymax_t) then
      do i = ix1, ix2 - 1
       if (x(i) >= targ_in) den(i, j, k, ic) = den(i, j, k, ic) + 1.
      end do
     end if
    end do
   end do
   if (ndim < 3) return
   n_loc = loc_zgrid(imodz)%ng
   do k = kz1, kz2 - 1
    iz = k + imodz*n_loc
    if (z(iz) > zmin_t .and. z(iz) < zmax_t) then
     do j = jy1, jy2 - 1
      do i = ix1, ix2 - 1
       den(i, j, k, ic) = den(i, j, k, ic) + 1.
      end do
     end do
    end if
   end do
  end subroutine
  !=============================================
  subroutine collect_bunch_and_plasma_density(this_bunch, isp)

   !========== bunch density and particles of species isp added on jc(ic)
   !=========================================
   integer, intent(in) :: this_bunch, isp
   real(dp) :: dery, derz
   integer :: np, nb, ik, i, j, k, jj, kk

   do i = 1, 2
    jc(:, :, :, i) = 0.0
   end do
   np = loc_npart(imody, imodz, imodx, isp)

   if (this_bunch == 0) then
    do ik = 1, nsb
     nb = loc_nbpart(imody, imodz, imodx, ik)
     if (nb > 0) then
      call set_grid_charge(bunch(ik), ebfb, jc, nb, 1)
     end if
    end do
   else
    ik = this_bunch !only the selected bunch density
    nb = loc_nbpart(imody, imodz, imodx, ik)
    if (nb > 0) then
     call set_grid_charge(bunch(ik), ebfb, jc, nb, 1)
    end if
   end if
   !=========== bunch data on jc(1)
   !==================== data of isp species on jc(2)
   if (np > 0) then
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
    kk = 1
    do k = kz1, kz2
     derz = loc_zg(kk, 3, imodz)
     jj = 1
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

   integer, intent(in) :: ic
   real(dp) :: dery, derz
   integer :: np, ik, i, j, k, jj, kk

   !curr_clean
   do i = 1, 2
    jc(:, :, :, i) = 0.0
   end do
   if (ic == 0) then !collects all bunch density
    do ik = 1, nsb
     np = loc_nbpart(imody, imodz, imodx, ik)
     if (np > 0) then
      call set_grid_den_energy(bunch(ik), ebfb, jc, np)
     end if
    end do
   else
    ik = ic !only the ic-bunch density
    np = loc_nbpart(imody, imodz, imodx, ik)
    if (np > 0) then
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
    kk = 1
    do k = kz1, kz2
     derz = loc_zg(kk, 3, imodz)
     jj = 1
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
  !============================
  subroutine prl_den_energy_interp(ic, cmp_out)
   integer, intent(in) :: ic, cmp_out
   real(dp) :: dery, derz, ar, ai
   integer :: np, i, j, k, jj, kk

   !=============================
   ! nden=1 only charge density
   ! nden =2 charge and energy <n(gamma-1)> density
   !=============================
   do i = 1, cmp_out
    jc(:, :, :, i) = 0.0
   end do
   !===========================
   np = loc_npart(imody, imodz, imodx, ic)
   if (part) then
    select case (cmp_out)
    case (1)
     call set_grid_charge(spec(ic), ebfp, jc, np, 1)
     !nden=1 exit density for each ic species
    case (2)
     !nden=2 exit density and energy density for each species
     if (envelope) then
      if (ic == 1) then
       do k = kz1, kz2
        do j = jy1, jy2
         do i = ix1, ix2
          ar = .5*(env(i, j, k, 1) + env(i, j, k, 3))
          ai = .5*(env(i, j, k, 2) + env(i, j, k, 4))
          jc(i, j, k, 3) = 0.5*(ar*ar + ai*ai)
         end do
        end do
       end do
       if (prl) call fill_ebfield_yzxbdsdata(jc, 3, 3, 2, 2)
       call set_grid_env_den_energy(spec(ic), ebfp, jc, np, 3)
      else
       call set_grid_den_energy(spec(ic), ebfp, jc, np) !ic >1 in envelope scheme
      endif
     else
      call set_grid_den_energy(spec(ic), ebfp, jc, np)
      ! in jc(1) is plasma norm density in jc(2) <(gam-1)density> using kinetic
      ! gamma  for each species
     end if
    end select
    if (prl) call fill_curr_yzxbdsdata(jc, cmp_out)
    do kk = 1, cmp_out
     call den_zyxbd(jc, kk)
    end do
    if (ic == 1) jc(:, :, :, 1) = -jc(:, :, :, 1)
    if (cmp_out == 2) jc(:, :, :, 2) = mass(ic)*electron_mass* &
                                       jc(:, :, :, 2)
    !=========== energy density in Mev*n/n_0

    if (stretch) then
     select case (ndim)
     case (2)
      k = 1
      do j = jy1, jy2
       jj = j - 2
       dery = loc_yg(jj, 3, imody)
       do i = ix1, ix2
        jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
       end do
      end do
     case (3)
      do k = kz1, kz2
       kk = k - 2
       derz = loc_zg(kk, 3, imodz)
       do j = jy1, jy2
        jj = j - 2
        dery = loc_yg(jj, 3, imody)*derz
        do i = ix1, ix2
         jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
        end do
       end do
      end do
     end select
    end if
   endif
   !======================
  end subroutine
  !=====================
  subroutine set_wake_potential

   integer :: np, ic, ft_mod, ft_sym
   integer :: i1, i2, j1, j2, k1, k2

   jc(:, :, :, 1:2) = 0.0
   !curr_clean
   do ic = 1, nsp
    np = loc_npart(imody, imodz, imodx, ic)
    if (np > 0) call set_grid_charge_and_jx(spec(ic), ebfp, jc, np)
   end do
   !========= jc(1)=charge density jc(2)= Jx at the same current t^{n} time
   if (prl) then
    call fill_curr_yzxbdsdata(jc, 2)
   end if
   if (nsp == 1) then
    call fill_density_data(jc, 1)
   else
    if (dmodel_id == 3) call fill_density_data(jc, 1)
   end if
   jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) = jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) - &
                                      jc(ix1:ix2, jy1:jy2, kz1:kz2, 2)
   !============== jc(1)=rho-Jx=======================

   ft_mod = 2 !for cosine transform
   ft_sym = 2
   call fft_2d_psolv(jc, jc, ompe, nx, nx_loc, ny, ny_loc, nz, nz_loc, &
                     i1, i2, j1, j2, k1, k2, ft_mod, ft_sym, 0)

   !==================================
  end subroutine
  !============================
 end module

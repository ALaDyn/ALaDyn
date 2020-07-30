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

 module init_beam_part_distrib

  use util
  use psolve
  use array_alloc
  use grid_param
  use mpi_field_interface
  use mpi_curr_interface
  use init_grid_field
  use grid_part_util
  use code_util, only: maxv, mem_psize
  use control_bunch_input
  use phys_param, only: t_unit

  implicit none

  real(dp), allocatable :: bpart(:, :)
  !--------------------------

 contains
  !=========================
  subroutine beam_data(ndm) !generates bunch phase space distribution in
   !                             ebfb(npart,nch)
   integer, intent(in) :: ndm
   integer :: np_tot
   integer :: i, i1, i2, ip
   real(dp) :: cut, xb(5), jp_norm
   integer :: nch
   logical :: data_snd
   !==========================================================
   ! bconf defines only the configuration of bunch charges
   !=======================
   ! default values
   !=======================
   jp_norm = 1.
   np_tot = 0
   do i = 1, nsb
    np_tot = np_tot + nb_tot(i)
   end do
   cut = 3.
   nch = 2*ndm + 1
   allocate (ebfb(np_tot, nch))
   !---!
   i1 = 1
   charge = int(unit_charge(1), hp_int) !the particle charge of each electron bunche
   !===============================
   if (pe0) then                 !only pe0 generates particle distribution
    select case (ndm)
    case (2)
     do ip = 1, nsb
      xb(ip) = xc_bunch(ip)
      wgh = real(jp_norm*jb_norm(ip), sp) !the bunch particle weight
      part_ind = int(ip, hp_int)
      i2 = i1 + nb_tot(ip) - 1
      call bunch_gen(ndm, i1, i2, sxb(ip), syb(ip), syb(ip), gam(ip), &
                     epsy(ip), epsz(ip), cut, dg(ip), ebfb)

      ebfb(i1:i2, 1) = ebfb(i1:i2, 1) + xb(ip) !x-shifting
      ebfb(i1:i2, 2) = ebfb(i1:i2, 2) + yc_bunch(ip) !y-shifting
      ebfb(i1:i2, nch) = wgh_cmp
      i1 = i2 + 1
     end do
    case (3)
     do ip = 1, nsb
      xb(ip) = xc_bunch(ip)
      wgh = real(jp_norm*jb_norm(ip), sp) !the bunch particle weights
      part_ind = int(ip, hp_int)
      i2 = i1 + nb_tot(ip) - 1
!====================
      call bunch_gen(ndm, i1, i2, sxb(ip), syb(ip), syb(ip), gam(ip), &
                     epsy(ip), epsz(ip), cut, dg(ip), ebfb)
!=====================
      ebfb(i1:i2, 1) = ebfb(i1:i2, 1) + xb(ip) !x-shifting
      ebfb(i1:i2, 2) = ebfb(i1:i2, 2) + yc_bunch(ip) !y-shifting
      ebfb(i1:i2, 3) = ebfb(i1:i2, 3) + zc_bunch(ip) !y-shifting
      ebfb(i1:i2, nch) = wgh_cmp
      i1 = i2 + 1
     end do
    end select
    data_snd = .true.            !pe0 sends data to all mpi-task
    do ip = 1, npe - 1
     call exchange_2d_grdata(data_snd, ebfb, nch, np_tot, ip, ip + 10)
    end do
   else
    data_snd = .false.           !current mype mpi-task receves particle data for pe0
    call exchange_2d_grdata(data_snd, ebfb, nch, np_tot, 0, mype + 10)
   end if
   !==============================
  end subroutine
  !===================
  subroutine mpi_beam_ftgrid_distribute(ndm)

   integer, intent(in) :: ndm

   integer :: i, ii, i1, j
   integer :: ic, p, ip, ipp, nb_loc
   real(dp) :: y1, y2, z1, z2, x1, x2
   integer :: nps_loc(nsb), npmax, np_tot
   !========= count bunch particles on each (yz) MPI domain in uniform ftgrid
   np_tot = sum(nb_tot(1:nsb))
   ! ALL MPI tasks do
   x1 = loc_xgrid(imodx)%gmin
   x2 = loc_xgrid(imodx)%gmax
   i1 = 0
   select case (ndm)
!================== 1:   counts local bunch particles of each bunch
   case (2)
    ip = npe_zloc - 1
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ipp = 0, npe_yloc - 1
       y1 = loc_yftgrid(ipp)%gmin
       y2 = loc_yftgrid(ipp)%gmax
       loc_nbpart(ipp, ip, p, ic) = 0
       do j = 1, nb_tot(ic)
        i = i1 + j
        if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
         if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
          loc_nbpart(ipp, ip, p, ic) = loc_nbpart(ipp, ip, p, ic) + 1
         end if
        end if
       end do
      end do
     end do
     i1 = i1 + nb_tot(ic)
    end do
    nb_max = maxval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    nb_min = minval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ipp = 0, npe_yloc - 1
       i = ipp + npe_yloc*(ip + p*npe_zloc)
       if (loc_nbpart(ipp, ip, p, ic) == nb_max) pe_nbmax = i
       if (loc_nbpart(ipp, ip, p, ic) == nb_min) pe_nbmin = i
      end do
     end do
    end do
    !==================
    ! The local particle number of each bunch
    nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
    npmax = maxval(nps_loc(1:nsb))
    npmax = max(npmax, 1)
    if (.not. allocated(bunch(1)%part)) then
     allocate (bunch(1)%part(npmax, nd2 + 1))
    else
     deallocate (bunch(1)%part)
     allocate (bunch(1)%part(npmax, nd2 + 1))
    end if
    !=================================
    !                            2: selected particle coordinates are copied in
    !                            bunch%part array
    nb_loc = nps_loc(1)
    p = imodx
    ip = imodz
    ipp = imody
    y1 = loc_yftgrid(ipp)%gmin
    y2 = loc_yftgrid(ipp)%gmax
    !=========================
    ! Here 2D MPI decomp. allowed
    !===================================
    i1 = 0
    do ic = 1, nsb
     ii = 0
     do i = 1, nb_tot(ic)
      j = i + i1
      if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
       if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
        ii = ii + 1
        bunch(1)%part(ii, 1:nd2 + 1) = ebfb(j, 1:nd2 + 1)
       end if
      end if
     end do
     i1 = i1 + nb_tot(ic)
    end do
!================== 1:   counts local bunch particles of each bunch
   case (3)
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ip = 0, npe_zloc - 1
       z1 = loc_zftgrid(ip)%gmin
       z2 = loc_zftgrid(ip)%gmax
       do ipp = 0, npe_yloc - 1
        y1 = loc_yftgrid(ipp)%gmin
        y2 = loc_yftgrid(ipp)%gmax
        loc_nbpart(ipp, ip, p, ic) = 0
        do j = 1, nb_tot(ic)
         i = i1 + j
         if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
          if (ebfb(i, 3) > z1 .and. ebfb(i, 3) <= z2) then
           if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
            loc_nbpart(ipp, ip, p, ic) = loc_nbpart(ipp, ip, p, ic) + 1
           end if
          end if
         end if
        end do
       end do
      end do
     end do
     i1 = i1 + nb_tot(ic)
    end do
    nb_max = maxval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    nb_min = minval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    !==================
    ! The local MPI task
    nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
    npmax = maxval(nps_loc(1:nsb))
    !==================
    npmax = max(npmax, 1)
    if (.not. allocated(bunch(1)%part)) then
     allocate (bunch(1)%part(npmax, nd2 + 1))
    else
     deallocate (bunch(1)%part)
     allocate (bunch(1)%part(npmax, nd2 + 1))
    end if
    !=================================
    !                            2: selected particle coordinates are copied in
    !                            bunch%part array
    nb_loc = nps_loc(1)
    p = imodx
    ip = imodz
    z1 = loc_zftgrid(ip)%gmin
    z2 = loc_zftgrid(ip)%gmax
    ipp = imody
    y1 = loc_yftgrid(ipp)%gmin
    y2 = loc_yftgrid(ipp)%gmax
    !=========================
    ! Here 3D MPI decomp. allowed
    !===================================
    i1 = 0
    do ic = 1, nsb
     ii = 0
     do i = 1, nb_tot(ic)
      j = i + i1
      if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
       if (ebfb(i, 3) > z1 .and. ebfb(i, 3) <= z2) then
        if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
         ii = ii + 1
         bunch(ic)%part(ii, 1:nd2 + 1) = ebfb(j, 1:nd2 + 1)
        end if
       end if
      end if
     end do
     i1 = i1 + nb_tot(ic)
    end do
   end select
  end subroutine
  !=================================
  subroutine mpi_beam_distribute(ndm)

   integer, intent(in) :: ndm

   integer :: i, ii, i1, j
   integer :: ic, p, ip, ipp, nb_loc
   real(dp) :: y1, y2, z1, z2, x1, x2
   integer :: nps_loc(nsb), npmax, np_tot
   !========= count bunch particles on each (yz) MPI domain
   np_tot = sum(nb_tot(1:nsb))
   ! ALL MPI tasks do
   x1 = loc_xgrid(imodx)%gmin
   x2 = loc_xgrid(imodx)%gmax
   i1 = 0
   select case (ndm)
!================== 1:   counts local bunch particles of each bunch
   case (2)
    ip = npe_zloc - 1
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ipp = 0, npe_yloc - 1
       y1 = loc_ygrid(ipp)%gmin
       y2 = loc_ygrid(ipp)%gmax
       loc_nbpart(ipp, ip, p, ic) = 0
       do j = 1, nb_tot(ic)
        i = i1 + j
        if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
         if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
          loc_nbpart(ipp, ip, p, ic) = loc_nbpart(ipp, ip, p, ic) + 1
         end if
        end if
       end do
      end do
     end do
     i1 = i1 + nb_tot(ic)
    end do
    nb_max = maxval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    nb_min = minval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ipp = 0, npe_yloc - 1
       i = ipp + npe_yloc*(ip + p*npe_zloc)
       if (loc_nbpart(ipp, ip, p, ic) == nb_max) pe_nbmax = i
       if (loc_nbpart(ipp, ip, p, ic) == nb_min) pe_nbmin = i
      end do
     end do
    end do
    !==================
    ! The local particle number of each bunch
    nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
    npmax = maxval(nps_loc(1:nsb))
    npmax = max(npmax, 1)
    if (.not. allocated(bunch(1)%part)) then
     allocate (bunch(1)%part(npmax, nd2 + 1))
    end if
    !=================================
    !              2: selected particle coordinates are copied in
    !                            bunch%part array
    nb_loc = nps_loc(1)
    p = imodx
    ip = imodz
    ipp = imody
    y1 = loc_ygrid(ipp)%gmin
    y2 = loc_ygrid(ipp)%gmax
    !=========================
    ! Here 2D MPI decomp. allowed
    !===================================
    i1 = 0
    do ic = 1, nsb
     ii = 0
     do i = 1, nb_tot(ic)
      j = i + i1
      if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
       if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
        ii = ii + 1
        bunch(1)%part(ii, 1:nd2 + 1) = ebfb(j, 1:nd2 + 1)
       end if
      end if
     end do
     i1 = i1 + nb_tot(ic)
    end do
!================== 1:   counts local bunch particles of each bunch
   case (3)
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ip = 0, npe_zloc - 1
       z1 = loc_zgrid(ip)%gmin
       z2 = loc_zgrid(ip)%gmax
       do ipp = 0, npe_yloc - 1
        y1 = loc_ygrid(ipp)%gmin
        y2 = loc_ygrid(ipp)%gmax
        loc_nbpart(ipp, ip, p, ic) = 0
        do j = 1, nb_tot(ic)
         i = i1 + j
         if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
          if (ebfb(i, 3) > z1 .and. ebfb(i, 3) <= z2) then
           if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
            loc_nbpart(ipp, ip, p, ic) = loc_nbpart(ipp, ip, p, ic) + 1
           end if
          end if
         end if
        end do
       end do
      end do
     end do
     i1 = i1 + nb_tot(ic)
    end do
    nb_max = maxval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    nb_min = minval(loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, &
                               1:nsb))
    do ic = 1, nsb
     do p = 0, npe_xloc - 1
      do ip = 0, npe_zloc - 1
       do ipp = 0, npe_yloc - 1
        i = ipp + npe_yloc*(ip + p*npe_zloc)
        if (loc_nbpart(ipp, ip, p, ic) == nb_max) pe_nbmax = i
        if (loc_nbpart(ipp, ip, p, ic) == nb_min) pe_nbmin = i
       end do
      end do
     end do
    end do
    !==================
    ! The local MPI task
    nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
    npmax = maxval(nps_loc(1:nsb))
    !==================
    npmax = max(npmax, 1)
    if (.not. allocated(bunch(1)%part)) then
     allocate (bunch(1)%part(npmax, nd2 + 1))
    end if
    !=================================
    !                            2: selected particle coordinates are copied in
    !                            bunch%part array
    nb_loc = nps_loc(1)
    p = imodx
    ip = imodz
    z1 = loc_zgrid(ip)%gmin
    z2 = loc_zgrid(ip)%gmax
    ipp = imody
    y1 = loc_ygrid(ipp)%gmin
    y2 = loc_ygrid(ipp)%gmax
    !=========================
    ! Here 3D MPI decomp. allowed
    !===================================
    i1 = 0
    do ic = 1, nsb
     ii = 0
     do i = 1, nb_tot(ic)
      j = i + i1
      if (ebfb(i, 2) > y1 .and. ebfb(i, 2) <= y2) then
       if (ebfb(i, 3) > z1 .and. ebfb(i, 3) <= z2) then
        if (ebfb(i, 1) > x1 .and. ebfb(i, 1) <= x2) then
         ii = ii + 1
         bunch(ic)%part(ii, 1:nd2 + 1) = ebfb(j, 1:nd2 + 1)
        end if
       end if
      end if
     end do
     i1 = i1 + nb_tot(ic)
    end do
   end select
   !=================================
  end subroutine
  !========================
  subroutine beam_model_pot(poten, sx, sy, sz, b_am, i1, i2, j1, j2, k1, &
                            k2)

   real(dp), intent(inout) :: poten(:, :, :, :)
   real(dp), intent(in) :: sx, sy, sz, b_am
   integer, intent(in) :: i1, i2, j1, j2, k1, k2
   integer :: i, j, k, jj, kk
   real(dp) :: r2, brad2, sx2_inv, fact, r2max, pot0
   real(dp) :: xx, yy, zz
   !-----------------------
   brad2 = sy*sy + sz*sz
   r2max = ymax*ymax + zmax*zmax
   pot0 = log(r2max/brad2)
   sx2_inv = 1./(2.*sx*sx)
   do k = k1, k2
    kk = k - 2
    zz = loc_zg(kk, 1, imodz)
    do j = j1, j2
     jj = j - 2
     yy = loc_yg(jj, 1, imody)
     r2 = zz*zz + yy*yy
     if (r2 > brad2) then
      fact = log(r2/brad2) - pot0
     else
      fact = (r2/brad2 - 1.) - pot0
     end if
     do i = i1, i2
      xx = (x(i1) - xc_bunch(1))
      poten(i, j, k, 1) = 0.25*brad2*b_am*fact*exp(-xx*xx*sx2_inv)
     end do
    end do
   end do
  end subroutine
  !=========================
  subroutine beam_inject

   integer :: id_ch, np, nb, ic, ft_mod, ft_sym
   integer :: nps_loc(nsb), nb_loc(1), i1
   integer :: y1, y2, z1, z2
   integer :: n, n1_alc, n2_alc, n3_alc
   real(dp) :: gam2

   gam2 = gam0*gam0
   id_ch = nd2 + 1
   !=======================
   n1_alc = size(ebf, 1)
   n2_alc = size(ebf, 2)
   n3_alc = size(ebf, 3)
   if (.not. allocated(ebf_bunch)) then
    allocate (ebf_bunch(n1_alc, n2_alc, n3_alc, nbfield))
    ebf_bunch(:, :, :, :) = 0.0
   end if
   !=============================
   ! The fields of a moving e-bunch in vacuum
   ! E_x=-(DPhi/Dx)/gamma^2  , E_y=-DPhi/Dy   E_z=-DPhi/Dz
   ! Poisson eq.  D_xE_x+D_yE_y+D_zE_z=omp^2\rho (x-V_b*t_0,y,z)
   ! B_x=0   B_y=-V_b*E_z    B_z= V_b*E_y
   !=========================================
   call init_random_seed(mype)
   call beam_data(ndim)
   ! Generates phase space coordinates for bparticles on ebfb(np_tot,7)
   ! bpart() provisional storage in common to all MPI tasks
   !=======================
   call mpi_beam_distribute(ndim) !local bpart data are stored in bunch(1)%part struct for each MPI task

   nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
   nb = sum(nps_loc(1:nsb))                     !the total bunch particle number
   !on each mpi_task
!=====================================
   if (allocated(spec(1)%part)) then
    np = loc_npart(imody, imodz, imodx, 1)
    if (nb > 0) then
     do n = 1, np
      ebfp(n, 1:id_ch) = spec(1)%part(n, 1:id_ch)
     end do
     deallocate (spec(1)%part)
     allocate (spec(1)%part(np + nb, id_ch))
     do n = 1, np
      spec(1)%part(n, 1:id_ch) = ebfp(n, 1:id_ch)
     end do
     deallocate (ebfp)
     do n = 1, nb
      i1 = n + np
      spec(1)%part(i1, 1:id_ch) = bunch(1)%part(n, 1:id_ch)
     end do
     allocate (ebfp(np + nb, id_ch))
     loc_npart(imody, imodz, imodx, 1) = loc_npart(imody, imodz, imodx, 1) + nb
    endif
   else
    nb_loc(1) = nb
    call p_alloc(nb, nd2 + 1, nb_loc, 1, lpf_ord, 1, 1, mem_psize)
    do n = 1, nb
     spec(1)%part(n, 1:id_ch) = bunch(1)%part(n, 1:id_ch)
    end do
    loc_npart(imody, imodz, imodx, 1) = nb_loc(1)
   end if
   ! Solves for beam potential UNIFORM GRIDS NEEDED
   !==================================================
   jc(:, :, :, 1) = 0.0
   ft_mod = 2 !A sine transform along each coordinate
   ft_sym = 1
   if (stretch) then
    if (ndim > 2) then
     allocate (pot(n1ft + 5, n2ft_loc + 5, n3ft_loc + 5, 1))
    else
     allocate (pot(n1ft + 5, n2ft_loc + 5, n3ft_loc, 1))
    endif
    pot(:, :, :, 1) = 0.0
    call mpi_beam_ftgrid_distribute(ndim) !local bpart data are stored in bunch(1)%part in ftgrid
    nps_loc(1:nsb) = loc_nbpart(imody, imodz, imodx, 1:nsb)
    nb = sum(nps_loc(1:nsb))                     !the total bunch particle number
    if (allocated(ebfb)) deallocate (ebfb)
    allocate (ebfb(nb, nd2 + 1))
    do ic = 1, nsb
     np = loc_nbpart(imody, imodz, imodx, ic)
     call set_charge_on_ftgrid(bunch(1), ebfb, pot, np, 1)
    end do
    if (prl) call fill_ftcurr_yzbdsdata(pot, 1)
    !============================
    !In pot(1) beam density
    !=====================================================
    y1 = loc_yftgrid(imody)%p_ind(1)
    y2 = loc_yftgrid(imody)%p_ind(2)
    z1 = loc_zftgrid(imodz)%p_ind(1)
    z2 = loc_zftgrid(imodz)%p_ind(2)
    !============ ft uniform grid
    if (ndim == 2) call fft_2d_psolv(pot, jc, ompe, n1ft, n1ft_loc, n2ft, n2ft_loc, n3ft, &
                                     n3ft_loc, ix1, ix2, y1, y2, z1, z2, ft_mod, ft_sym, 1)
    if (ndim == 3) call fft_3d_psolv(pot, jc, gam2, ompe, n1ft, n1ft_loc, n2ft, n2ft_loc, n3ft, &
                                     n3ft_loc, ix1, ix2, y1, y2, z1, z2, ft_mod, ft_sym, 1)
    !Solves Laplacian[poten]=ompe*rho in Fourier space using sin() transform
    !Exit beam potential in jc(1)
    !uniform to stretched grid interpolation inside
    !======================================
   else
    do ic = 1, nsb
     np = loc_nbpart(imody, imodz, imodx, ic)
     call set_grid_charge(bunch(1), ebfp, jc, np, 1)
    end do
    if (prl) call fill_curr_yzxbdsdata(jc, 1)
    !============================
    !In jc(1) beam density
    !=====================================================
    y1 = jy1
    y2 = jy2
    z1 = kz1
    z2 = kz2
    if (ndim == 2) call fft_2d_psolv(jc, jc, ompe, nx, nx_loc, ny, ny_loc, nz, &
                                     nz_loc, ix1, ix2, y1, y2, z1, z2, ft_mod, ft_sym, 0)
    if (ndim == 3) call fft_3d_psolv(jc, jc, gam2, ompe, nx, nx_loc, ny, ny_loc, nz, &
                                     nz_loc, ix1, ix2, y1, y2, z1, z2, ft_mod, ft_sym, 0)
    !Solves Laplacian[poten]=ompe*rho in Fourier space using sin() transform
    !Exit beam potential in jc(1)
   endif
   jc(:, :, :, 2) = bet0*jc(:, :, :, 1)
   !==========================
   if (allocated(ebfb)) deallocate (ebfb)
   if (allocated(bunch(1)%part)) deallocate (bunch(1)%part)
   !===========================
   call fill_ebfield_yzxbdsdata(jc, 1, 2, 1, 1)
   call initial_beam_fields(jc, ebf_bunch, gam2, bet0)
   ! generates (Ex,Ey,Ez,By,Bz) bunch fields  Bx=ebf_bunc(4)=0
   ebf_bunch(:, :, :, 4) = 0.0
   !========================================= Collect data
   ebf(:, :, :, 1:nfield) = ebf(:, :, :, 1:nfield) + &
                            ebf_bunch(:, :, :, 1:nfield)
   !========================================
   lp_end(1) = xc_bunch(1) + 2.*sxb(1)
   !=====================================
   if (pe0) then
    !==================
    open (16, file='Initial_bunch_info.dat')
    write (16, '(a27,e11.4)') 'Initial target x-position =', targ_in
    write (16, '(a20,e11.4)') 'Plasma wave-length =', lambda_p
    write (16, *) '-------------------------------------'
    write (16, '(a17,i4)') 'Number of bunches', nsb
    do i1 = 1, nsb
     write (16, *) 'bunch number =', i1
     write (16, '(a25,i6)') 'Bunch particles per cell ', nb_per_cell(i1)
     write (16, '(a23,i8)') 'Bunch particle number  ', nb_tot(i1)
     write (16, '(a15,2e14.6)') 'gamma and beta ', gam(i1), bet0
     write (16, '(a15,2e11.4)') 'Bunch  sizes   ', sxb(i1), syb(i1)
     write (16, '(a23,2e11.4)') 'Transverse emittances= ', epsy(i1), &
      epsz(i1)
     write (16, '(a21,e11.4)') 'Initial xc-position= ', xc_bunch(i1)
     write (16, '(a20,e11.4)') 'B charge    [pC] =  ', bunch_charge(i1)
     write (16, '(a21,e11.4)') 'B_density/P_density  ', rhob(i1)
    end do
    close (16)
   end if
  end subroutine
  !========================
 end module

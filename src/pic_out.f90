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

 module pic_out

  use pstruct_data
  use fstruct_data
  use code_util
  use common_param
  use grid_param
  use parallel

  implicit none

  integer, parameter, private :: par_dim = 20
  integer, private :: int_par(par_dim), part_int_par(par_dim)
  real(sp), private :: real_par(par_dim), part_real_par(par_dim)

  character(13), dimension(20), parameter, private :: rpar = [' time =      ', &
                                                              ' xmin =      ', ' xmax =      ', ' ymin =      ', ' ymax =      ', &
                                                              ' zmin =      ', ' zmax =      ', ' w0_x =      ', ' w0_y =      ', &
                                                              ' a0 =        ', ' lam0 =      ', ' mc2(MeV) =  ', ' n0(e18) =   ', &
                                                              ' np/cell =   ', ' weight =    ', ' mass =      ', ' xmin_out =  ', &
                                                              ' xmax_out =  ', ' ymax_out =  ', ' gam_min =   ']

  character(12), dimension(20), parameter, private :: ipar = [' npe =      ', &
                                                              ' nx =       ', ' ny =       ', ' nz =       ', ' model =    ', &
                                                              ' dmodel =   ', ' nsp =      ', ' curr_ndim =', ' mp/cell =  ', &
                                                              ' ion_ch =   ', ' tsch_ord = ', ' der_ord =  ', ' iform =    ', &
                                                              ' ph_sp_nc = ', ' f_version =', ' i_end =    ', ' nx_loc =   ', &
                                                              ' ny_loc =   ', ' nz_loc =   ', ' pjump  =   ']

 contains

  subroutine endian(iend)
   implicit none
   integer, intent(out) :: iend
   integer, parameter :: ik1 = selected_int_kind(2)
   integer, parameter :: ik4 = selected_int_kind(9)

   iend = 0
   if (btest(transfer(int([1, 0, 0, 0], ik1), 1_ik4), 0)) then
    iend = 1
   else
    iend = 2
   end if
  end subroutine

  subroutine fluid_den_mom_out(fvar, cmp, flcomp)
   real(dp), intent(in) :: fvar(:, :, :, :)
   integer, intent(in) :: cmp, flcomp
   character(9) :: fname = '         '
   character(7), dimension(4), parameter :: flvar = ['Fdenout', &
                                                     'Flpxout', 'Flpyout', 'Flpzout']

   integer :: ix, iy, iz, iq, ipe
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: gr_dim(3), i_end, cmp_name
   integer :: lun, i1, j1, k1
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2
   !========================
   ! ns_index select ion species
   ! cmp select components (density, energy,..)
   ! cmp_loc is the index of output data:  jc(cmp_loc)

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0
   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(fvar(ix, iy, iz, cmp), sp)
     end do
    end do
   end do
   if (cmp == flcomp) then
    cmp_name = 1
   else
    cmp_name = cmp + 1
   end if

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    write (fname, '(a7,i2.2)') flvar(cmp_name), iout
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i14)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    close (10)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fluid density-momenta written on file: '// &
     foldername//'/'//fname//'.bin'
   end if
  end subroutine
  !==================================
  subroutine den_energy_out(ns_ind, cmp, cmp_loc)
   integer, intent(in) :: ns_ind, cmp, cmp_loc
!================
   character(9) :: fname = '         '
   character(7), dimension(1), parameter :: bpot = ['Beampot']
   character(7), dimension(2), parameter :: el1 = ['Edenout', &
                                                   'Elenout']
   character(7), dimension(2), parameter :: pr1 = ['Pdenout', &
                                                   'Prenout']
   character(7), dimension(2), parameter :: io1 = ['H1dnout', &
                                                   'H1enout']
   character(7), dimension(2), parameter :: io2 = ['H2dnout', &
                                                   'H2enout']

   integer :: ix, iy, iz, iq, ipe
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: gr_dim(3), i_end
   integer :: lun, i1, j1, k1
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2
   !========================
   ! ns_index select ion species (electron,ions)
   ! cmp select components name in output script for each species
   ! [Eden,Elen],[H1dn,H1en].....
   ! cmp_loc is the index where output data are stored in jc(cmp_loc) array
   !============================

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0
   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(jc(ix, iy, iz, cmp_loc), sp)
     end do
    end do
   end do

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    select case (ns_ind)
    case (0)
     write (fname, '(a7,i2.2)') bpot, iout
    case (1)
     write (fname, '(a7,i2.2)') el1(cmp), iout
    case (2)
     if (atomic_number(1) == 1) then
      write (fname, '(a7,i2.2)') pr1(cmp), iout
     else
      write (fname, '(a7,i2.2)') io1(cmp), iout
     end if
    case (3)
     if (atomic_number(2) == 1) then
      write (fname, '(a7,i2.2)') pr1(cmp), iout
     else
      write (fname, '(a7,i2.2)') io2(cmp), iout
     end if
    case (4)
     write (fname, '(a7,i2.2)') io2(cmp), iout
    end select
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i14)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    close (10)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Den_Energy_Momenta written on file: '//foldername// &
     '/'//fname//'.bin'
   end if
  end subroutine

  !============================
  subroutine bden_energy_out(cmp_loc)

   integer, intent(in) :: cmp_loc
   character(9) :: fname = '         '

   integer :: ix, iy, iz, ip, iq, ipe
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: i_end, i1, j1, k1
   integer :: lun, gr_dim(3)
   character(4) :: foldername
   integer, parameter :: file_version = 2
   logical :: sd

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 10
   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(jc(ix, iy, iz, cmp_loc), sp)
     end do
    end do
   end do

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    write (fname, '(a7,i2.2)') 'Bdenout', iout
    open (20, file=foldername//'/'//fname//'.dat', form='formatted')
    write (20, *) ' Integer parameters'
    write (20, '(4i14)') int_par
    write (20, *) ' Real parameters'
    write (20, '(4e14.5)') real_par
    close (20)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10

    open (10, file=foldername//'/'//fname//'.bin', form='unformatted') !qui
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (prl) then
    if (mype > 0) then
     gr_dim(1) = nxh(imodx + 1)
     gr_dim(2) = nyh(imody + 1)
     gr_dim(3) = nzh(imodz + 1)
     lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
     sd = .true.
     call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
    else
     sd = .false.
     do ix = 0, npe_xloc - 1
      gr_dim(1) = nxh(ix + 1)
      do ip = 0, npe_zloc - 1
       gr_dim(3) = nzh(ip + 1)
       do iq = 0, npe_yloc - 1
        gr_dim(2) = nyh(iq + 1)
        ipe = iq + npe_yloc*(ip + npe_zloc*ix)
        if (ipe > 0) then
         lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
         call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
         write (lun) gr_dim
         write (lun) wdata(1:lenw)
        end if
       end do
      end do
     end do
    end if
   end if

   if (pe0) then
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Den_Energy_Momenta written on file: '//foldername// &
     '/'//fname//'.bin'
   end if
  end subroutine
  !==========================
  subroutine ext_bfield_out(ef, f_ind)
   real(dp), intent(in) :: ef(:, :, :, :)
   character(8) :: fname = '        '
   integer, intent(in) :: f_ind
   integer :: ix, iy, iz, iq, ipe
   integer :: lun, lenw, kk, nx1, ny1, nz1
   integer :: i1, j1, k1, i_end
   integer :: gr_dim(3)
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
     end do
    end do
   end do

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    select case (f_ind)
    case (1)
     write (fname, '(a6,i2.2)') 'Bx0out', iout
    case (2)
     write (fname, '(a6,i2.2)') 'By0out', iout
    case (3)
     write (fname, '(a6,i2.2)') 'Bz0out', iout
    end select

    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i10)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    close (10)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if
  end subroutine

  subroutine fields_out(ef, f_ind, f_var)
   real(dp), intent(in) :: ef(:, :, :, :)
   character(8) :: fname = '        '
   integer, intent(in) :: f_ind, f_var
   integer :: ix, iy, iz, iq, ipe
   integer :: lun, lenw, kk, nx1, ny1, nz1
   integer :: i1, j1, k1, i_end
   integer :: gr_dim(3)
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
     end do
    end do
   end do

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    select case (f_var)
    case (0)
     write (fname, '(a6,i2.2)') 'Jxfout', iout
    case (1)
     write (fname, '(a6,i2.2)') 'Exfout', iout
    case (2)
     write (fname, '(a6,i2.2)') 'Eyfout', iout
    case (3)
     if (nfield == 3) then
      write (fname, '(a6,i2.2)') 'Bzfout', iout
     else
      write (fname, '(a6,i2.2)') 'Ezfout', iout
     end if
    case (4)
     write (fname, '(a6,i2.2)') 'Bxfout', iout
    case (5)
     write (fname, '(a6,i2.2)') 'Byfout', iout
    case (6)
     write (fname, '(a6,i2.2)') 'Bzfout', iout
    end select

    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i14)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    close (10)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if
  end subroutine

  !==========================

  subroutine fields_out_new(ef, f_ind, var_ind)
   real(dp), intent(in) :: ef(:, :, :, :)
   character(8) :: fname = '        '
   character(12) :: fnamel = '            '
   character(17) :: fname_out = '                 '
   character(21) :: fname_outl = '                     '
   integer, intent(in) :: f_ind, var_ind
   integer :: ix, iy, iz, iq
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: i1, j1, k1, i_end
   integer(offset_kind) :: disp, disp_col
   integer :: num_header_int, gr_dim(3), header(3)
   real(dp), allocatable :: ascii_grid(:)
   integer :: gridsize_x, gridsize_y, gridsize_z
   character(4) :: foldername
   integer, parameter :: file_version = 4

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   gr_dim = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0

   do iz = k1, nzp, jump
    do iy = j1, nyp, jump
     do ix = i1, nxp, jump
      kk = kk + 1
      wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
     end do
    end do
   end do
   !================================
   call endian(i_end)
   ny1 = sum(nyh(1:npe_yloc))
   nz1 = sum(nzh(1:npe_zloc))
   nx1 = sum(nxh(1:npe_xloc))

   real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                     real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                     real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                     real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                     real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                     real(b_charge, sp), real(vbeam, sp)]

   int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, nz1, &
                    loc_nyc_max, jump, ibx, iby, iform, model_id, dmodel_id, nsp, &
                    curr_ndim, mp_per_cell(1), lpf_ord, der_ord, file_version, i_end]

   select case (var_ind)
   case (0)
    write (fname, '(a6,i2.2)') 'dvEout', iout
   case (1)
    write (fname, '(a6,i2.2)') 'Exfout', iout
   case (2)
    write (fname, '(a6,i2.2)') 'Eyfout', iout
   case (3)
    if (nfield == 3) then
     write (fname, '(a6,i2.2)') 'Bzfout', iout
    else
     write (fname, '(a6,i2.2)') 'Ezfout', iout
    end if
   case (4)
    write (fname, '(a6,i2.2)') 'Bxfout', iout
   case (5)
    write (fname, '(a6,i2.2)') 'Byfout', iout
   case (6)
    write (fname, '(a6,i2.2)') 'Bzfout', iout
   case (7)
    write (fname, '(a6,i2.2)') 'Exbout', iout
   case (8)
    write (fname, '(a6,i2.2)') 'Eybout', iout
   case (9)
    if (nfield == 3) then
     write (fname, '(a6,i2.2)') 'Bzbout', iout
    else
     write (fname, '(a6,i2.2)') 'Ezbout', iout
    end if
   case (10)
    write (fname, '(a6,i2.2)') 'Jxbout', iout
   case (11)
    write (fname, '(a6,i2.2)') 'Bybout', iout
   case (12)
    write (fname, '(a6,i2.2)') 'Bzbout', iout
   end select

   if (pe0) then
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i14)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    write (10, *) ' Coordinates'

    gridsize_x = int(nx/jump)
    gridsize_y = int(ny/jump)
    gridsize_z = int(nz/jump)
    allocate (ascii_grid(gridsize_x + 1))
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     ascii_grid(kk) = x(iq)
    end do
    do iq = 1, kk
     write (10, '(es14.5)', advance='no') ascii_grid(iq)
     if (mod(iq, 8) == 0) write (10, *) ''
    end do
    deallocate (ascii_grid)
    allocate (ascii_grid(gridsize_y + 1))
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     ascii_grid(kk) = y(iq)
    end do
    do iq = 1, kk
     write (10, '(es14.5)', advance='no') ascii_grid(iq)
     if (mod(iq, 8) == 0) write (10, *) ''
    end do
    deallocate (ascii_grid)
    allocate (ascii_grid(gridsize_z + 1))
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     ascii_grid(kk) = z(iq)
    end do
    do iq = 1, kk
     write (10, '(es14.5)', advance='no') ascii_grid(iq)
     if (mod(iq, 8) == 0) write (10, *) ''
    end do
    close (10)
    write (6, *) 'Fields parameters written on file: '//foldername// &
     '/'//fname//'.dat'
   end if

   gr_dim(1) = nxh(imodx + 1)
   gr_dim(2) = nyh(imody + 1)
   gr_dim(3) = nzh(imodz + 1)
   lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)

   write (fnamel, '(a8,a1,i3.3)') fname, '_', imodz
   fname_out = foldername//'/'//fname//'.bin'
   fname_outl = foldername//'/'//fnamel//'.bin'
   num_header_int = 3
   header(1:3) = gr_dim(1:3)
   disp = 4*mype*(num_header_int + lenw) ! da usare con mpi_write !assuming that all procs have the same grid size
   disp_col = 4*imody*(num_header_int + lenw) ! con mpi_write_col !assuming that all procs have the same grid size

   call mpi_write_field(wdata, lenw, header, num_header_int, disp, 17, &
                        fname_out)

   if (pe0) then
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if

  end subroutine

  !==========================

  subroutine bfields_out(ef, ef1, f_ind)
   real(dp), intent(in) :: ef(:, :, :, :), ef1(:, :, :, :)
   character(8) :: fname = '        '
   integer, intent(in) :: f_ind
   integer :: ix, iy, iz, iq, ipe
   integer :: lun, lenw, kk, nx1, ny1, nz1
   integer :: i1, j1, k1, i_end
   integer :: gr_dim(3)
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   select case (ibeam)
   case (0)
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
      end do
     end do
    end do
   case (1)
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       if (abs(ef(ix, iy, iz, f_ind) + ef1(ix, iy, iz, f_ind)) > 1d34) then
        write (*, *) 'Error :: overflow in file output'
        write (*, '(A,4I4)') 'index:', ix, iy, iz, f_ind
       end if
       wdata(kk) = real(ef(ix, iy, iz, f_ind) + ef1(ix, iy, iz, f_ind), sp)
      end do
     end do
    end do
   end select

   if (pe0) then
    call endian(i_end)
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, i_end]

    select case (f_ind)
    case (1)
     write (fname, '(a6,i2.2)') 'Exbout', iout
    case (2)
     write (fname, '(a6,i2.2)') 'Eybout', iout
    case (3)
     if (nfield == 3) then
      write (fname, '(a6,i2.2)') 'Bzbout', iout
     else
      write (fname, '(a6,i2.2)') 'Ezbout', iout
     end if
    case (4)
     write (fname, '(a6,i2.2)') 'Jxbout', iout
    case (5)
     write (fname, '(a6,i2.2)') 'Bybout', iout
    case (6)
     write (fname, '(a6,i2.2)') 'Bzbout', iout
    end select

    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Integer parameters'
    write (10, '(4i14)') int_par
    write (10, *) ' Real parameters'
    write (10, '(4e14.5)') real_par
    close (10)
    write (6, *) 'Field data written on file: '//foldername//'/'// &
     fname//'.dat'

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 200)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 200)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if
  end subroutine

  !==========================
  subroutine env_two_fields_out(ef, ef1, f_ind)
   real(dp), intent(in) :: ef(:, :, :, :), ef1(:, :, :, :)
   character(9) :: fname = '         '
   integer, intent(in) :: f_ind
   integer :: ix, iy, iz, iq, ipe
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: gr_dim(3)
   integer :: i1, j1, k1, lun
   logical :: sd
   real(dp) :: a2, avec
   character(4) :: foldername
   integer, parameter :: file_version = 2

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   if (f_ind == 0) then
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       a2 = ef(ix, iy, iz, 1)*ef(ix, iy, iz, 1) + &
            ef(ix, iy, iz, 2)*ef(ix, iy, iz, 2)
       avec = sqrt(a2)
       a2 = ef1(ix, iy, iz, 1)*ef1(ix, iy, iz, 1) + &
            ef1(ix, iy, iz, 2)*ef1(ix, iy, iz, 2)
       avec = avec + sqrt(a2)
       wdata(kk) = real(avec, sp)
      end do
     end do
    end do
   else
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
       wdata(kk) = wdata(kk) + real(ef1(ix, iy, iz, f_ind), sp)
      end do
     end do
    end do
   end if

   if (pe0) then
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, ibeam]

    select case (f_ind)
    case (0)
     write (fname, '(a7,i2.2)') 'Aenvout', iout
    case (1)
     write (fname, '(a7,i2.2)') 'Renvout', iout
    case (2)
     write (fname, '(a7,i2.2)') 'Ienvout', iout
    end select

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if
  end subroutine

  subroutine env_fields_out(ef, f_ind)
   real(dp), intent(in) :: ef(:, :, :, :)
   character(9) :: fname = '         '
   integer, intent(in) :: f_ind
   integer :: ix, iy, iz, iq, ipe
   integer :: lenw, kk, nx1, ny1, nz1
   integer :: gr_dim(3)
   integer :: i1, j1, k1, lun
   logical :: sd
   character(4) :: foldername
   integer, parameter :: file_version = 2
   real(dp) :: a2, avec

   write (foldername, '(i4.4)') iout

   int_par = 0
   real_par = 0.0
   lun = 0

   j1 = jy1
   k1 = kz1
   i1 = ix1

   kk = 0
   if (f_ind < 1) then
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       a2 = ef(ix, iy, iz, 1)*ef(ix, iy, iz, 1) + &
            ef(ix, iy, iz, 2)*ef(ix, iy, iz, 2)
       avec = sqrt(a2)
       wdata(kk) = real(avec, sp)
      end do
     end do
    end do
   else
    do iz = k1, nzp, jump
     do iy = j1, nyp, jump
      do ix = i1, nxp, jump
       kk = kk + 1
       wdata(kk) = real(ef(ix, iy, iz, f_ind), sp)
      end do
     end do
    end do
   end if

   if (pe0) then
    nx1 = sum(nxh(1:npe_xloc))
    ny1 = sum(nyh(1:npe_yloc))
    nz1 = sum(nzh(1:npe_zloc))

    real_par(1:20) = [real(tnow, sp), real(xmin, sp), real(xmax, sp), &
                      real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                      real(w0_x, sp), real(w0_y, sp), real(n_over_nc, sp), real(a0, sp), &
                      real(lam0, sp), real(e0, sp), real(ompe, sp), real(targ_in, sp), &
                      real(targ_end, sp), real(gam0, sp), real(nb_over_np, sp), &
                      real(b_charge, sp), real(vbeam, sp)]

    int_par(1:20) = [npe_yloc, npe_zloc, npe_xloc, nx1, ny1, &
                     loc_nyc_max, nz1, loc_nzc_max, jump, iby, iform, model_id, &
                     dmodel_id, nsp, curr_ndim, mp_per_cell(1), lpf_ord, der_ord, &
                     file_version, ibeam]

    select case (f_ind)
    case (-1)
     write (fname, '(a7,i2.2)') 'aenvout', iout
    case (0)
     write (fname, '(a7,i2.2)') 'Aenvout', iout
    case (1)
     write (fname, '(a7,i2.2)') 'Renvout', iout
    case (2)
     write (fname, '(a7,i2.2)') 'Ienvout', iout
    end select

    gr_dim(1) = nxh(1)
    gr_dim(2) = nyh(1)
    gr_dim(3) = nzh(1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    lun = 10
    open (10, file=foldername//'/'//fname//'.bin', form='unformatted')
    write (10) par_dim
    write (10) int_par
    write (10) real_par
    write (10) gr_dim
    write (10) wdata(1:lenw)
   end if

   if (mype > 0) then
    gr_dim(1) = nxh(imodx + 1)
    gr_dim(2) = nyh(imody + 1)
    gr_dim(3) = nzh(imodz + 1)
    lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
    sd = .true.
    call exchange_pdata(sd, wdata, lenw, pe_min, mype + 100)
   else
    sd = .false.
    do ix = 0, npe_xloc - 1
     gr_dim(1) = nxh(ix + 1)
     do iz = 0, npe_zloc - 1
      gr_dim(3) = nzh(iz + 1)
      do iy = 0, npe_yloc - 1
       gr_dim(2) = nyh(iy + 1)
       ipe = iy + npe_yloc*(iz + npe_zloc*ix)
       if (ipe > 0) then
        lenw = gr_dim(1)*gr_dim(2)*gr_dim(3)
        call exchange_pdata(sd, wdata, lenw, ipe, ipe + 100)
        write (lun) gr_dim
        write (lun) wdata(1:lenw)
       end if
      end do
     end do
    end do
    kk = 0
    do iq = 1, nx, jump
     kk = kk + 1
     gwdata(kk) = real(x(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, ny, jump
     kk = kk + 1
     gwdata(kk) = real(y(iq), sp)
    end do
    write (10) gwdata(1:kk)
    kk = 0
    do iq = 1, nz, jump
     kk = kk + 1
     gwdata(kk) = real(z(iq), sp)
    end do
    write (10) gwdata(1:kk)
    close (10)
    write (6, *) 'Fields written on file: '//foldername//'/'// &
     fname//'.bin'
   end if
  end subroutine
  !================================
  subroutine part_pdata_out(timenow, xmin_out, xmax_out, ymax_out, pid, &
                            jmp)

   character(6), dimension(4), parameter :: part_files = ['Elpout', &
                                                          'H1pout', 'Prpout', 'H2pout']
   character(8) :: fname
   character(17) :: fname_out
   character(12) :: fnamel
   character(21) :: fname_outl
   real(dp), intent(in) :: timenow, xmin_out, xmax_out, ymax_out
   integer, intent(in) :: pid, jmp
   real(sp), allocatable :: pdata(:)
   integer(dp) :: nptot_global_reduced
   integer :: ik, p, q, np, ip, ip_max, nptot
   integer :: lenp, ip_loc(npe), ndv, i_end
   integer(offset_kind) :: disp, disp_col
   real(dp) :: xx, yy, zz
   character(4) :: foldername
   integer, parameter :: file_version = 4

   write (foldername, '(i4.4)') iout

   ndv = nd2 + 2
   np = loc_npart(imody, imodz, imodx, pid)
   ip = 0
   if (np > 0) then
    if (ndim > 2) then
     do p = 1, np, jmp
      yy = spec(pid)%part(p, 2)
      zz = spec(pid)%part(p, 3)
      if (abs(yy) <= ymax_out .and. abs(zz) <= ymax_out) then
       xx = spec(pid)%part(p, 1)
       if (xx >= xmin_out .and. xx <= xmax_out) then
        ip = ip + 1
        do q = 1, nd2 + 1
         ebfp(ip, q) = spec(pid)%part(p, q)
        end do
       end if
      end if
     end do
    else
     zz = 1.
     do p = 1, np, jmp
      yy = spec(pid)%part(p, 2)
      if (abs(yy) <= ymax_out) then
       xx = spec(pid)%part(p, 1)
       if (xx >= xmin_out .and. xx <= xmax_out) then
        ip = ip + 1
        do q = 1, nd2 + 1
         ebfp(ip, q) = spec(pid)%part(p, q)
        end do
       end if
      end if
     end do
    end if
   end if
   ip_loc(mype + 1) = ip

   ip = ip_loc(mype + 1)
   call intvec_distribute(ip, ip_loc, npe)

   ! this differs from nptot_global since it represents just the reduced number of particles
   ! that will be present in the output (should be equal to nptot_global for p_jump=1)!
   nptot_global_reduced = 0
   do ik = 1, npe
    nptot_global_reduced = nptot_global_reduced + ip_loc(ik)
   end do
   if (nptot_global < 1e9) then
    nptot = int(nptot_global_reduced)
   else
    nptot = -1
   end if

   ip_max = ip
   if (pe0) ip_max = maxval(ip_loc(1:npe))
   lenp = ndv*ip_loc(mype + 1)
   allocate (pdata(lenp))
   ik = 0
   do p = 1, ip_loc(mype + 1)
    do q = 1, nd2
     ik = ik + 1
     pdata(ik) = real(ebfp(p, q), sp)
    end do
    wgh_cmp = ebfp(p, nd2 + 1)
    ik = ik + 1
    pdata(ik) = wgh
    ik = ik + 1
    pdata(ik) = real(charge, sp)
   end do
   if (ik /= lenp) write (6, '(a16,3i8)') 'wrong pdata size', mype, lenp, &
    ik

   call endian(i_end)
   part_real_par(1:20) = [real(timenow, sp), real(xmin, sp), real(xmax, sp), &
                          real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                          real(w0_x, sp), real(w0_y, sp), real(a0, sp), real(lam0, sp), &
                          real(e0, sp), real(n0_ref, sp), real(np_per_cell, sp), &
                          real(j0_norm, sp), real(mass(pid), sp), real(xmin_out, sp), &
                          real(xmax_out, sp), real(ymax_out, sp), real(gam_min, sp)]

   part_int_par(1:20) = [npe, nx, ny, nz, model_id, dmodel_id, nsp, &
                         curr_ndim, mp_per_cell(pid), ion_min(1), lpf_ord, der_ord, iform, ndv, &
                         file_version, i_end, nx_loc, ny_loc, nz_loc, pjump]

   write (fname, '(a6,i2.2)') part_files(pid), iout !serve sempre
   write (fnamel, '(a6,i2.2,a1,i3.3)') part_files(pid), iout, '_', imodz !usare con mpi_write_part_col
   fname_out = foldername//'/'//fname//'.bin'
   fname_outl = foldername//'/'//fnamel//'.bin'
   disp = 0
   disp_col = 0
   if (pe0) then
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Real parameters'
    do q = 1, 20
     write (10, '(a13,e11.4)') rpar(q), part_real_par(q)
    end do
    write (10, *) ' Integer parameters'
    do p = 1, 20
     write (10, '(a12,i8)') ipar(p), part_int_par(p)
    end do
    write (10, *) ' Number of particles in the output box'
    write (10, '(4i20)') nptot_global_reduced
    close (10)
    write (6, *) 'Particles param written on file: '//foldername// &
     '/'//fname//'.dat'
   else
    disp = mype + ndv*sum(ip_loc(1:mype)) ! da usare con mpi_write_part
   end if

   if (mod(mype, npe_yloc) > 0) disp_col = ndv*sum(ip_loc(imodz*npe_yloc + 1: &
                                                          mype)) ! da usare con mpi_write_part_col

   disp = disp*4 ! sia gli int che i float sono di 4 bytes
   disp_col = disp_col*4

   call mpi_write_part(pdata, lenp, ip, disp, 17, fname_out)

   if (allocated(pdata)) deallocate (pdata)
   if (pe0) then
    write (6, *) 'Particles data written on file: '//foldername// &
     '/'//fname//'.bin'
    write (6, *) ' Output logical flag ', l_force_singlefile_output
   end if
  end subroutine

!==========================
  subroutine part_high_gamma_out(gam_in, timenow)

   character(8), dimension(1), parameter :: part_files = ['E_hg_out']
   character(10) :: fname
   character(19) :: fname_out
   real(dp), intent(in) :: gam_in, timenow
   real(sp), allocatable :: pdata(:)
   integer(dp) :: nptot_global_reduced
   integer :: id_ch, ik, p, q, ip, ip_max, nptot
   integer :: jmp, ne, lenp, ip_loc(npe), ndv, i_end
   integer(offset_kind) :: disp
   real(dp) :: gam, pp(3)
   character(4) :: foldername
   integer, parameter :: file_version = 4

   write (foldername, '(i4.4)') iout
   jmp = 1
   id_ch = nd2 + 1
   ndv = nd2 + 2
   ne = loc_npart(imody, imodz, imodx, 1)
   select case (nd2)
   case (4)
    ip = 0
    if (ne > 0) then
     do p = 1, ne
      pp(1:2) = spec(1)%part(p, 3:4)
      gam = sqrt(1.+pp(1)*pp(1) + pp(2)*pp(2))
      if (gam > gam_in) then
       ip = ip + 1
       do q = 1, nd2 + 1
        ebfp(ip, q) = spec(1)%part(p, q)
       end do
      end if
     end do
    end if
   case (6)
    ip = 0
    if (ne > 0) then
     do p = 1, ne
      pp(1:3) = spec(1)%part(p, 4:6)
      gam = sqrt(1.+pp(1)*pp(1) + pp(2)*pp(2) + pp(3)*pp(3))
      if (gam > gam_in) then
       ip = ip + 1
       do q = 1, nd2 + 1
        ebfp(ip, q) = spec(1)%part(p, q)
       end do
      end if
     end do
    end if
   end select
   ip_loc(mype + 1) = ip

   ip = ip_loc(mype + 1)
   call intvec_distribute(ip, ip_loc, npe)
   nptot_global_reduced = 0
   !nptot_global_reduced=sum(ip_loc(1:npe))
   do ik = 1, npe
    nptot_global_reduced = nptot_global_reduced + ip_loc(ik)
   end do
   if (nptot_global < 1e9) then
    nptot = int(nptot_global_reduced)
   else
    nptot = -1
   end if
   ip_max = ip
   if (pe0) ip_max = maxval(ip_loc(1:npe))
   lenp = ndv*ip
   ik = max(1, lenp)
   allocate (pdata(lenp))
   ik = 0
   do p = 1, ip
    do q = 1, nd2
     ik = ik + 1
     pdata(ik) = real(ebfp(p, q), sp)
    end do
    wgh_cmp = ebfp(p, nd2 + 1)
    ik = ik + 1
    pdata(ik) = wgh
    ik = ik + 1
    pdata(ik) = real(charge, sp)
   end do
   if (ik /= lenp) write (6, '(a16,3i8)') 'wrong pdata size', mype, lenp, &
    ik

   int_par = 0
   call endian(i_end)

   part_real_par(1:20) = [real(timenow, sp), real(xmin, sp), real(xmax, sp), &
                          real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                          real(w0_x, sp), real(w0_y, sp), real(a0, sp), real(lam0, sp), &
                          real(e0, sp), real(n0_ref, sp), real(np_per_cell, sp), &
                          real(wgh_ion, sp), real(mass(1), sp), real(xp0_out, sp), &
                          real(xp1_out, sp), real(yp_out, sp), real(gam_in, sp)]

   part_int_par(1:20) = [npe, nx, ny, nz, model_id, dmodel_id, nsp, &
                         curr_ndim, mp_per_cell(1), ion_min(1), lpf_ord, der_ord, iform, &
                         ndv, file_version, i_end, nx_loc, ny_loc, nz_loc, 0]

   write (fname, '(a8,i2.2)') part_files(1), iout !serve sempre
   fname_out = foldername//'/'//fname//'.bin'
   disp = 0
   if (pe0) then
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Real parameters'
    do q = 1, 20
     write (10, '(a13,e11.4)') rpar(q), part_real_par(q)
    end do
    write (10, *) ' Integer parameters'
    do p = 1, 20
     write (10, '(a12,i8)') ipar(p), part_int_par(p)
    end do
    write (10, *) ' Number of particles in the output box'
    write (10, '(4i20)') nptot_global_reduced
    close (10)
    write (6, *) 'Particles param written on file: '//foldername// &
     '/'//fname//'.dat'
   else
    disp = mype + ndv*sum(ip_loc(1:mype)) ! da usare con mpi_write_part
   end if

   disp = disp*4 ! sia gli int che i float sono di 4 bytes
   call mpi_write_part(pdata, lenp, ip, disp, 19, fname_out)
   if (allocated(pdata)) deallocate (pdata)
   if (pe0) then
    write (6, *) 'Particles data written on file: '//foldername// &
     '/'//fname//'.bin'
   end if
  end subroutine

  subroutine part_bdata_out(timenow, pid, jmp)

   character(11), dimension(1), parameter :: part_files = ['E_bunch_out']
   character(13) :: fname
   character(22) :: fname_out
   real(dp), intent(in) :: timenow
   integer, intent(in) :: pid, jmp
   real(sp), allocatable :: pdata(:)
   integer(dp) :: nptot_global_reduced
   integer :: id_ch, ik, p, q, ip, ip_max, nptot
   integer :: ne, lenp, ip_loc(npe), ndv, i_end
   integer(offset_kind) :: disp
   character(4) :: foldername
   integer, parameter :: file_version = 4

   write (foldername, '(i4.4)') iout
   id_ch = nd2 + 1
   ndv = nd2 + 2
   ne = loc_npart(imody, imodz, imodx, 1)
   ip = 0
   if (ne > 0) then
    do p = 1, ne, jmp
     wgh_cmp = spec(1)%part(p, id_ch)
     if (part_ind == pid) then
      ip = ip + 1
      do q = 1, nd2 + 1
       ebfp(ip, q) = spec(1)%part(p, q)
      end do
     end if
    end do
   end if
   ip_loc(mype + 1) = ip

   ip = ip_loc(mype + 1)
   call intvec_distribute(ip, ip_loc, npe)
   nptot_global_reduced = 0
   !nptot_global_reduced=sum(ip_loc(1:npe))
   do ik = 1, npe
    nptot_global_reduced = nptot_global_reduced + ip_loc(ik)
   end do
   if (nptot_global < 1e9) then
    nptot = int(nptot_global_reduced)
   else
    nptot = -1
   end if
   ip_max = ip
   if (pe0) ip_max = maxval(ip_loc(1:npe))
   lenp = ndv*ip
   ik = max(1, lenp)
   allocate (pdata(lenp))
   ik = 0
   do p = 1, ip
    do q = 1, nd2
     ik = ik + 1
     pdata(ik) = real(ebfp(p, q), sp)
    end do
    wgh_cmp = ebfp(p, nd2 + 1)
    ik = ik + 1
    pdata(ik) = wgh
    ik = ik + 1
    pdata(ik) = real(charge, sp)
   end do
   if (ik /= lenp) write (6, '(a16,3i8)') 'wrong pdata size', mype, lenp, &
    ik

   int_par = 0
   call endian(i_end)

   part_real_par(1:20) = [real(timenow, sp), real(xmin, sp), real(xmax, sp), &
                          real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                          real(w0_x, sp), real(w0_y, sp), real(a0, sp), real(lam0, sp), &
                          real(e0, sp), real(n0_ref, sp), real(np_per_cell, sp), &
                          real(wgh_ion, sp), real(mass(1), sp), real(xp0_out, sp), &
                          real(xp1_out, sp), real(yp_out, sp), real(gam_min, sp)]

   part_int_par(1:20) = [npe, nx, ny, nz, model_id, dmodel_id, nsp, &
                         curr_ndim, mp_per_cell(1), ion_min(1), lpf_ord, der_ord, iform, ndv, &
                         file_version, i_end, nx_loc, ny_loc, nz_loc, pjump]

   write (fname, '(a11,i2.2)') part_files(1), iout !serve sempre
   fname_out = foldername//'/'//fname//'.bin'
   disp = 0
   if (pe0) then
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Real parameters'
    do q = 1, 20
     write (10, '(a13,e11.4)') rpar(q), part_real_par(q)
    end do
    write (10, *) ' Integer parameters'
    do p = 1, 20
     write (10, '(a12,i8)') ipar(p), part_int_par(p)
    end do
    write (10, *) ' Number of particles in the output box'
    write (10, '(4i20)') nptot_global_reduced
    close (10)
    write (6, *) 'Particles param written on file: '//foldername// &
     '/'//fname//'.dat'
   else
    disp = mype + ndv*sum(ip_loc(1:mype)) ! da usare con mpi_write_part
   end if

   disp = disp*4 ! sia gli int che i float sono di 4 bytes
   call mpi_write_part(pdata, lenp, ip, disp, 22, fname_out)
   if (allocated(pdata)) deallocate (pdata)
   if (pe0) then
    write (6, *) 'Particles data written on file: '//foldername// &
     '/'//fname//'.bin'
   end if
  end subroutine
  !==============================================
  subroutine part_ionz_out(timenow)

   character(8), dimension(1), parameter :: part_files = ['Eionzout']
   character(10) :: fname
   character(19) :: fname_out
   real(dp), intent(in) :: timenow
   real(sp), allocatable :: pdata(:)
   integer(dp) :: nptot_global_reduced
   integer :: id_ch, ik, p, q, ip, ip_max, nptot
   integer :: jmp, ne, lenp, ip_loc(npe), ndv, i_end
   integer(offset_kind) :: disp
   real(sp) :: ch_ion
   character(4) :: foldername
   integer, parameter :: file_version = 4

   write (foldername, '(i4.4)') iout
   jmp = 1
   id_ch = nd2 + 1
   ndv = nd2 + 2
   ch_ion = real(wgh_ion, sp)
   ne = loc_npart(imody, imodz, imodx, 1)
   ip = 0
   if (ne > 0) then
    do p = 1, ne
     wgh_cmp = spec(1)%part(p, id_ch)
     if (part_ind < 0) then
      ip = ip + 1
      do q = 1, nd2 + 1
       ebfp(ip, q) = spec(1)%part(p, q)
      end do
     end if
    end do
   end if
   ip_loc(mype + 1) = ip

   ip = ip_loc(mype + 1)
   call intvec_distribute(ip, ip_loc, npe)
   nptot_global_reduced = 0
   !nptot_global_reduced=sum(ip_loc(1:npe))
   do ik = 1, npe
    nptot_global_reduced = nptot_global_reduced + ip_loc(ik)
   end do
   if (nptot_global < 1e9) then
    nptot = int(nptot_global_reduced)
   else
    nptot = -1
   end if
   ip_max = ip
   if (pe0) ip_max = maxval(ip_loc(1:npe))
   lenp = ndv*ip
   ik = max(1, lenp)
   allocate (pdata(lenp))
   ik = 0
   do p = 1, ip
    do q = 1, nd2
     ik = ik + 1
     pdata(ik) = real(ebfp(p, q), sp)
    end do
    wgh_cmp = ebfp(p, nd2 + 1)
    ik = ik + 1
    pdata(ik) = wgh
    ik = ik + 1
    pdata(ik) = real(charge, sp)
   end do
   if (ik /= lenp) write (6, '(a16,3i8)') 'wrong pdata size', mype, lenp, &
    ik

   int_par = 0
   call endian(i_end)

   part_real_par(1:20) = [real(timenow, sp), real(xmin, sp), real(xmax, sp), &
                          real(ymin, sp), real(ymax, sp), real(zmin, sp), real(zmax, sp), &
                          real(w0_x, sp), real(w0_y, sp), real(a0, sp), real(lam0, sp), &
                          real(e0, sp), real(n0_ref, sp), real(np_per_cell, sp), &
                          real(wgh_ion, sp), real(mass(1), sp), real(xp0_out, sp), &
                          real(xp1_out, sp), real(yp_out, sp), real(gam_min, sp)]

   part_int_par(1:20) = [npe, nx, ny, nz, model_id, dmodel_id, nsp, &
                         curr_ndim, mp_per_cell(1), ion_min(1), lpf_ord, der_ord, iform, &
                         ndv, file_version, i_end, nx_loc, ny_loc, nz_loc, pjump]

   write (fname, '(a8,i2.2)') part_files(1), iout !serve sempre
   fname_out = foldername//'/'//fname//'.bin'
   disp = 0
   if (pe0) then
    open (10, file=foldername//'/'//fname//'.dat', form='formatted')
    write (10, *) ' Real parameters'
    do q = 1, 20
     write (10, '(a13,e11.4)') rpar(q), part_real_par(q)
    end do
    write (10, *) ' Integer parameters'
    do p = 1, 20
     write (10, '(a12,i8)') ipar(p), part_int_par(p)
    end do
    write (10, *) ' Number of particles in the output box'
    write (10, '(4i20)') nptot_global_reduced
    close (10)
    write (6, *) 'Particles param written on file: '//foldername// &
     '/'//fname//'.dat'
   else
    disp = mype + ndv*sum(ip_loc(1:mype)) ! da usare con mpi_write_part
   end if

   disp = disp*4 ! sia gli int che i float sono di 4 bytes
   call mpi_write_part(pdata, lenp, ip, disp, 19, fname_out)
   if (allocated(pdata)) deallocate (pdata)
   if (pe0) then
    write (6, *) 'Particles data written on file: '//foldername// &
     '/'//fname//'.bin'
   end if
  end subroutine
  !================================

 end module

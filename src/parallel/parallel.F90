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

 module parallel
  use mpi_var
  use common_param
  use base_species, only: scalars
  use util, only: init_random_seed

#if !defined (_CRESCO)
#define ENABLE_MPI_LONG_INT
#endif

#if defined (FORCE_OLD_MPI)
  implicit none
  include 'mpif.h'
#else
  use mpi
  implicit none
#endif

  type :: communicator_T
   integer, private :: world_comm
   !! Stores the MPI_COMM_WORLD
   contains
    procedure, public :: setworld
    procedure, public :: getworld
  end type

  integer, parameter :: offset_kind = mpi_offset_kind, &
                        whence = mpi_seek_set

  integer :: mpi_err

  real(dp), allocatable :: fp0x(:, :, :, :), fp1x(:, :, :, :)

  integer :: status(mpi_status_size), error, mpi_sd

  type(communicator_T) :: communicator
  
  contains

 !=== Subroutine for communicator type ===

 subroutine setworld( this, comm_in )
  class(communicator_T), intent(inout) :: this
  integer, intent(in) :: comm_in
  
  this%world_comm = comm_in
  
 end subroutine
 
 pure function getworld( this ) result( comm_out )
  class(communicator_T), intent(in) :: this
  integer :: comm_out

  comm_out = this%world_comm

 end function

 !==================
 subroutine check_decomposition

   if (npe_yz > 0) then
    nprocy = npe_yz
    if (nz > 1) then
     nprocz = npe_yz
    else
     nprocz = 1
    end if
    nprocx = mpi_size/nprocy/nprocz
   end if
   if (nprocx < 0 .or. nprocy < 0 .or. nprocz < 0 .or. &
       nprocx*nprocy*nprocz /= mpi_size) then
    if (mpi_rank == 0) then
     write (6, *) 'Invalid MPI decomposition'
     write (6, *) 'mpi_size =', mpi_size
     write (6, *) 'nprocx =', nprocx, 'nprocy =', nprocy, 'nprocz =', &
      nprocz
     stop 674
    end if
   end if
  end subroutine

  subroutine start_parallel(ncmp, p_ind, b_ind)
   integer, intent(in) :: ncmp, p_ind, b_ind
   integer :: ipe, pen, pkind, bkind

   call mpi_init(error)
   call mpi_comm_size(mpi_comm_world, mpi_size, error)
   call mpi_comm_rank(mpi_comm_world, mpi_rank, error)

   call communicator%setworld( mpi_comm_world )

   call check_decomposition
   !===================================
   npe_xloc = nprocx
   npe_yloc = nprocy
   npe_zloc = nprocz
   npe = mpi_size

   pe_min = 0
   pe_max = npe - 1

   mype = mpi_rank
   pe0 = (mype == 0)
   pe1 = (mype == pe_max)

   prl = (npe > 1)
   prlx = (npe_xloc > 1)
   prly = (npe_yloc > 1)
   prlz = (npe_zloc > 1)

   comm = mpi_comm_world

   mpi_sd = mpi_double_precision

   call mpi_type_contiguous(ncmp + 1, mpi_sd, partype, error)
   call mpi_type_commit(partype, error)

   !================
   ndims = 3
   dims(1) = npe_yloc
   dims(2) = npe_zloc
   dims(3) = npe_xloc
   !================
   imodzx = mype/npe_yloc
   imody = mod(mype, npe_yloc)
   imodz = mod(imodzx, npe_zloc)
   imodx = imodzx/npe_zloc

   imodyx = imody + npe_yloc*npe_zloc*imodx
   imodyz = imody + npe_yloc*imodz
   !======================
   !=================

   !================= MPI cartesiantopology with (imody,imodz,imodx) coordinates
   ! mype=imody+npe_yloc*imodzx
   ! imodzx=imodz+npe_zloc*imodx
   !----------------------------
   ! mype=imodyx+npe_yloc*imodz
   ! imodyx=imody+npe_yloc*npe_zloc*imodx
   !===================
   ! mype=imodyz+npe_yloc*npe_zloc*imodx
   col_or(1) = dims(1)*(imodz + dims(2)*imodx)
   ! imodyz=imody+npe_yloc*imodz
   col_or(2) = imody + dims(1)*dims(2)*imodx
   !==========================
   col_or(3) = imody + dims(1)*imodz
   !======================
   call mpi_comm_split(comm, col_or(1), imody, comm_col(1), error)
   call mpi_comm_rank(comm_col(1), coor(1), error)
   call mpi_comm_split(comm, col_or(2), imodz, comm_col(2), error)
   call mpi_comm_rank(comm_col(2), coor(2), error)
   call mpi_comm_split(comm, col_or(3), imodx, comm_col(3), error)
   call mpi_comm_rank(comm_col(3), coor(3), error)
   !imodzx=>> all pes in the (imodz,imodx) plane for given imody
   !imodyx=> all pes in the (imody,imodx) plane for given imodz
   !all pes in the (imody,imodz) plane for given imodx
   !============ for diagnostic
   pe0y = imody == 0
   pe1y = imody == npe_yloc - 1
   pe0z = imodz == 0
   pe1z = imodz == npe_zloc - 1
   pex0 = imodx == 0
   pex1 = imodx == npe_xloc - 1
   !===========================
   pe_min_y = 0
   pe_max_y = npe_yloc - 1
   pe_min_z = 0
   pe_max_z = npe_zloc - 1
   pe_min_x = 0
   pe_max_x = npe_xloc - 1
   !===========================
   xl_bd = .false.
   xr_bd = .false.
   yl_bd = .false.
   yr_bd = .false.
   zl_bd = .false.
   zr_bd = .false.
   if (pex0) xl_bd = .true.
   if (pex1) xr_bd = .true.
   if (pe0y) yl_bd = .true.
   if (pe1y) yr_bd = .true.
   if (pe0z) zl_bd = .true.
   if (pe1z) zr_bd = .true.
   pe0x = pex0
   pe1x = pex1
   ! Logical idensification of mpi boundary coordinates
   pkind = max(1, p_ind)
   bkind = max(1, b_ind)
   !====================
   allocate (loc_npart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, 1:pkind))
   loc_npart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, 1:pkind) = 0
   !========================================
   allocate (loc_ne_ionz(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1))
   loc_ne_ionz(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1) = 0
   allocate (loc_tpart(npe))
   loc_tpart(1:npe) = 0

   allocate (loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, 1:bkind))
   loc_nbpart(0:npe_yloc - 1, 0:npe_zloc - 1, 0:npe_xloc - 1, 1:bkind) = 0

   allocate (yp_next(npe_yloc), yp_prev(npe_yloc))
   allocate (zp_next(npe_zloc), zp_prev(npe_zloc))
   allocate (xp_next(npe_xloc), xp_prev(npe_xloc))

   yp_next(1) = imody
   yp_prev(1) = imody
   if (npe_yloc > 1) then
    do ipe = 1, npe_yloc - 1
     pen = imody + ipe
     yp_next(ipe) = mod(pen, npe_yloc)
     !============================
     pen = imody - ipe
     if (pen < 0) pen = pen + npe_yloc
     yp_prev(ipe) = pen
    end do
   end if
   zp_next(1) = imodz
   zp_prev(1) = imodz
   if (npe_zloc > 1) then
    do ipe = 1, npe_zloc - 1
     pen = imodz + ipe
     zp_next(ipe) = mod(pen, npe_zloc)

     pen = imodz - ipe
     if (pen < 0) pen = pen + npe_zloc
     zp_prev(ipe) = pen
    end do
   end if
   xp_next(1) = imodx
   xp_prev(1) = imodx
   if (prlx) then
    do ipe = 1, npe_xloc - 1
     pen = imodx + ipe
     xp_next(ipe) = mod(pen, npe_xloc)
     !============= output arrays
     pen = imodx - ipe
     if (pen < 0) pen = pen + npe_xloc
     xp_prev(ipe) = pen
    end do
   end if

   !==========================================
   ! INITIALIZE THE RANDOM SEED FOR EVERY MYPE
   !==========================================

   call init_random_seed(mype)
   !call processor_grid_diag

  end subroutine

  subroutine call_barrier( comm_in )
   integer, intent(in), optional :: comm_in
   integer :: comm_barr

   if ( present(comm_in) ) then
    comm_barr = comm_in
   else
    comm_barr = comm
   end if

   call MPI_BARRIER( comm_barr, error )

  end subroutine

  subroutine mpi_write_dp(buf, bufsize, disp, fout)

   real (dp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout
   !===========================
   integer :: ierr, thefile
   !========================
   call mpi_file_open(comm, fout, mpi_mode_wronly + mpi_mode_create, &
                      mpi_info_null, thefile, ierr)

   call mpi_file_write_at(thefile, disp, buf, bufsize, mpi_sd, &
                          mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)

  end subroutine
  !======== each process acces thefile and writes at disp(byte) coordinate
  subroutine mpi_write_row_dp(buf, bufsize, disp, fout)

   real (dp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !=======================================
   call mpi_file_open(comm_col(2), fout, mpi_mode_wronly+mpi_mode_create &
     , mpi_info_null, thefile, ierr)

   call mpi_file_write_at(thefile, disp, buf, bufsize, mpi_sd, &
     mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine mpi_write_col_dp(buf, bufsize, disp, fout)

   real (dp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !===================
   call mpi_file_open(comm_col(1), fout, mpi_mode_wronly+mpi_mode_create &
     , mpi_info_null, thefile, ierr)

   call mpi_file_write_at(thefile, disp, buf, bufsize, &
     mpi_double_precision, mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine mpi_write_col_str(buf, bufsize, disp, fout)

   character(LEN=*), intent (in), dimension(:) :: buf
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !===================
   call mpi_file_open(comm_col(1), fout, mpi_mode_wronly+mpi_mode_create &
     , mpi_info_null, thefile, ierr)

   call mpi_file_write_at(thefile, disp, buf, bufsize, &
    mpi_character, mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)

  end subroutine

  subroutine mpi_read_col_dp(buf, bufsize, disp, fout)

   real (dp), intent (inout) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !========================
   call mpi_file_open(comm_col(1), fout, mpi_mode_rdonly, mpi_info_null, &
     thefile, ierr)

   call mpi_file_read_at(thefile, disp, buf, bufsize, mpi_sd, &
     mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)

  end subroutine

  subroutine mpi_read_dp(buf, bufsize, disp, fout)

   real (dp), intent (inout) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !=======================================
   call mpi_file_open(comm, fout, mpi_mode_rdonly, mpi_info_null, &
     thefile, ierr)

   call mpi_file_read_at(thefile, disp, buf, bufsize, mpi_sd, &
     mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine mpi_write_part(buf, bufsize, loc_np, disp, fout)

   real (sp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize, loc_np
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !===============================
   call mpi_file_open(comm, fout, mpi_mode_wronly+mpi_mode_create, &
     mpi_info_null, thefile, ierr)

   call mpi_file_set_view(thefile, disp, mpi_real, mpi_real, 'native', &
                          mpi_info_null, ierr)

   call mpi_file_write(thefile, loc_np, 1, mpi_integer, &
                       mpi_status_ignore, ierr)
   call mpi_file_write(thefile, buf, bufsize, mpi_real, &
                       mpi_status_ignore, ierr)
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine mpi_write_part_col(buf, bufsize, disp, fout)

   real (sp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout
   !========================
   integer :: ierr, thefile
   !========================
   call mpi_file_open(comm_col(1), fout, mpi_mode_wronly+mpi_mode_create &
     , mpi_info_null, thefile, ierr)

   call mpi_file_set_view(thefile, disp, mpi_real, mpi_real, 'native', &
                          mpi_info_null, ierr)

   call mpi_file_write(thefile, buf, bufsize, mpi_real, &
                       mpi_status_ignore, ierr)
   !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
   call mpi_file_close(thefile, ierr)
   !please disable any binary output in Windows/IFORT if it doesn't work for you
  end subroutine

  subroutine mpi_write_field(buf, bufsize, header, header_size, disp, &
    fout)

   real (sp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize, header_size, header(:)
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout

   integer :: ierr, thefile
   !========================
   call mpi_file_open(comm, fout, mpi_mode_wronly+mpi_mode_create, &
     mpi_info_null, thefile, ierr)

   call mpi_file_set_view(thefile, disp, mpi_real, mpi_real, 'native', &
                          mpi_info_null, ierr)

   call mpi_file_write(thefile, header, header_size, mpi_integer, &
                       mpi_status_ignore, ierr)
   !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
   call mpi_file_write(thefile, buf, bufsize, mpi_real, &
                       mpi_status_ignore, ierr)
   !please disable any binary output in Windows/IFORT if it doesn't work for you
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine mpi_write_field_col(buf, bufsize, header, header_size, &
    disp, fout)
   !========================
   real (sp), intent (in) :: buf(:)
   integer, intent (in) :: bufsize, header_size, header(:)
   integer (offset_kind), intent (in) :: disp
   character (LEN=*), intent (in) :: fout
   !different from mpi_write_field because of the different communicator in
   integer :: ierr, thefile
   !mpi_file_open
   call mpi_file_open(comm_col(1), fout, mpi_mode_wronly+mpi_mode_create &
     , mpi_info_null, thefile, ierr)

   call mpi_file_set_view(thefile, disp, mpi_real, mpi_real, 'native', &
                          mpi_info_null, ierr)

   call mpi_file_write(thefile, header, header_size, mpi_integer, &
                       mpi_status_ignore, ierr)
   !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
   call mpi_file_write(thefile, buf, bufsize, mpi_real, &
                       mpi_status_ignore, ierr)
   !please disable any binary output in Windows/IFORT if it doesn't work for you
   call mpi_file_close(thefile, ierr)
  end subroutine

  subroutine End_parallel

   call MPI_FINALIZE(error)

  end subroutine

  subroutine exchange_idata(sr, idat, lenw, ipe, tag)
   integer, intent(in) :: lenw, ipe, tag
   integer, intent(inout) :: idat(:)
   logical, intent(in) :: sr

   if (sr) then
    !=======================
    call mpi_send(idat(1), lenw, mpi_integer, ipe, tag, comm, error)
    !=====================================
   else

    call mpi_recv(idat(1), lenw, mpi_integer, ipe, tag, comm, status, &
                  error)
   end if

  end subroutine

  subroutine exchange_2d_idata(sr, idat, n1, n2, ipe, tag)
   logical, intent(in) :: sr
   integer, intent(inout) :: idat(:, :)
   integer, intent(in) :: n1, n2, ipe, tag
   integer :: lenw

   lenw = n1*n2
   if (sr) then

    call mpi_send(idat(1, 1), lenw, mpi_integer, ipe, tag, comm, error)

   else

    call mpi_recv(idat(1, 1), lenw, mpi_integer, ipe, tag, comm, status, &
                  error)
   end if

  end subroutine

  subroutine exchange_3d_sp_data(sr, dat0, n1, n2, n3, ipe, tag)
   integer, intent(in) :: n1, n2, n3, ipe, tag
   real(sp), intent(inout) :: dat0(:, :, :)
   logical, intent(in) :: sr
   integer :: lenw

   lenw = n1*n2*n3
   if (sr) then

    call mpi_send(dat0(1, 1, 1), lenw, mpi_real, ipe, tag, comm, error)
    !====================
   else

    call mpi_recv(dat0(1, 1, 1), lenw, mpi_real, ipe, tag, comm, status, &
                  error)
   end if

  end subroutine

  subroutine exchange_1d_grdata(sr, dat0, lenw, ipe, tag)
   logical, intent(in) :: sr
   real(dp), intent(inout) :: dat0(:)
   integer, intent(in) :: lenw, ipe, tag

   if (sr) then

    call mpi_send(dat0(1), lenw, mpi_sd, ipe, tag, comm, error)

   else
    !====================
    call mpi_recv(dat0(1), lenw, mpi_sd, ipe, tag, comm, status, error)
   end if

  end subroutine

  subroutine exchange_2d_grdata(sr, dat0, n1, n2, ipe, tag)
   logical, intent(in) :: sr
   real(dp), intent(inout) :: dat0(:, :)
   integer, intent(in) :: n1, n2, ipe, tag
   integer :: lenw

   lenw = n1*n2
   if (sr) then

    call mpi_send(dat0(1, 1), lenw, mpi_sd, ipe, tag, comm, error)

   else

    call mpi_recv(dat0(1, 1), lenw, mpi_sd, ipe, tag, comm, status, &
                  error)
   end if

  end subroutine
!====================
  subroutine exchange_3d_grdata(sr, dat0, lenw, dir, ipe)
   integer, intent(in) :: lenw, dir, ipe
   real(dp), intent(inout) :: dat0(:, :, :)
   logical, intent(in) :: sr
   integer :: tag

   tag = 10 + ipe
   if (sr) then

    call mpi_send(dat0(1, 1, 1), lenw, mpi_sd, ipe, tag, comm_col(dir), &
                  error)
    !=========================
   else

    call mpi_recv(dat0(1, 1, 1), lenw, mpi_sd, ipe, tag, comm_col(dir), &
                  status, error)
   end if
  end subroutine

  subroutine exchange_grdata(sr, dat0, lenw, dir, ipe)
   integer, intent(in) :: lenw, dir, ipe
   real(dp), intent(inout) :: dat0(:, :, :, :)
   logical, intent(in) :: sr
   integer :: tag

   tag = 10 + ipe
   if (sr) then

    call mpi_send(dat0(1, 1, 1, 1), lenw, mpi_sd, ipe, tag, comm_col(dir), &
                  error)
    !=========================
   else

    call mpi_recv(dat0(1, 1, 1, 1), lenw, mpi_sd, ipe, tag, comm_col(dir), &
                  status, error)
   end if

  end subroutine

  subroutine sr_part_properties(part_prop_send, part_prop_rcv, ns, nr, dir, side)
   type (scalars), intent(in) :: part_prop_send
   type (scalars), intent(inout) :: part_prop_rcv
   integer, intent (in) :: ns, nr, dir, side
   real (dp) :: dat0(3), dat1(3)
   integer :: tag, pes, per

   tag = 30 + dir
   select case (dir)
   case (1)
    if (side>0) then
     pes = yp_prev(side)
     per = yp_next(side)
    else
     pes = yp_next(-side)
     per = yp_prev(-side)
    end if
   case (2)
    if (side>0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side>0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select
   if (ns*nr>0) then

    dat0(1) = part_prop_send%pick_temperature()
    dat0(2) = part_prop_send%pick_charge()
    dat0(3) = part_prop_send%pick_dimensions()

    call mpi_sendrecv(dat0(1), 3, mpi_sd, pes, tag, dat1(1), 3, &
      mpi_sd, per, tag, comm_col(dir), status, error)

    call part_prop_rcv%set_temperature( dat1(1) )
    call part_prop_rcv%set_charge( dat1(2) )
    call part_prop_rcv%set_dimensions( int(dat1(3)) )

   else
    if (ns>0) then

     dat0(1) = part_prop_send%pick_temperature()
     dat0(2) = part_prop_send%pick_charge()
     dat0(3) = part_prop_send%pick_dimensions()

     call mpi_send(dat0(1), 3, mpi_sd, pes, tag, &
      comm_col(dir), error)
    end if
    if (nr>0) then

     call mpi_recv(dat1(1), 3, mpi_sd, per, tag, &
      comm_col(dir), status, error)
     
     call part_prop_rcv%set_temperature( dat1(1) )
     call part_prop_rcv%set_charge( dat1(2) )
     call part_prop_rcv%set_dimensions( int(dat1(3)) )
    end if
   end if

  end subroutine

  subroutine realvec_distribute(rs, rv, nproc)
   integer, intent(in) :: nproc
   real(dp), intent(in) :: rs
   real(dp) :: rv(:), rc
   integer :: ipe

   if (.not. pe0) then
    call mpi_send(rs, 1, mpi_real, pe_min, 20 + mype, comm, error)
   else
    rv(1) = rs
    do ipe = 1, nproc - 1
     call mpi_recv(rc, 1, mpi_real, ipe, 20 + ipe, comm, status, error)
     rv(ipe + 1) = rc
    end do
   end if

  end subroutine

  subroutine intvec_distribute(ns, nc, nproc)
   integer, intent(in) :: ns, nproc
   integer, intent(inout) :: nc(:)
   integer :: ipe, nr

   if (.not. pe0) then
    call mpi_send(ns, 1, mpi_integer, pe_min, mype, comm, error)
   else
    nc(1) = ns
    if (.not. prl) return
    do ipe = 1, nproc - 1
     call mpi_recv(nr, 1, mpi_integer, ipe, ipe, comm, status, error)
     nc(ipe + 1) = nr
    end do
   end if
   call MPI_BCAST(nc, nproc, mpi_integer, pe_min, comm, error)

  end subroutine

  subroutine sr_idata(ns, nr, dir, side)
   integer, intent(in) :: ns, dir, side
   integer, intent(out) :: nr
   integer :: pes, per, tag

   nr = 0
   tag = 100 + dir
   select case (dir)
   case (1)
    if (side > 0) then
     pes = yp_prev(side)
     per = yp_next(side) !=============
    else
     pes = yp_next(-side)
     per = yp_prev(-side) !sends to left ns data
    end if
   case (2)
    if (side > 0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side > 0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select
   call mpi_sendrecv(ns, 1, mpi_integer, pes, tag, nr, 1, mpi_integer, &
                     per, tag, comm_col(dir), status, error)
   !receives from right nr daata
  end subroutine
  !sends to right ns data
  subroutine sr_pdata(sdata, rdata, ns, nr, dir, side)
   real(dp), intent(in) :: sdata(:)
   real(dp), intent(out) :: rdata(:)
   integer, intent(in) :: ns, nr, dir, side
   integer :: tag, pes, per
   !receives form left nr data
   tag = 1000 + dir
   select case (dir)
   case (1)
    if (side > 0) then
     pes = yp_prev(side)
     per = yp_next(side)
    else
     pes = yp_next(-side)
     per = yp_prev(-side)
    end if
   case (2)
    if (side > 0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side > 0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select
   if (ns*nr > 0) then
    call mpi_sendrecv(sdata(1), ns, mpi_sd, pes, tag, rdata(1), nr, &
                      mpi_sd, per, tag, comm_col(dir), status, error)
   else
    if (ns > 0) call mpi_send(sdata(1), ns, mpi_sd, pes, tag, &
                              comm_col(dir), error)
    if (nr > 0) call mpi_recv(rdata(1), nr, mpi_sd, per, tag, &
                              comm_col(dir), status, error)
   end if
  end subroutine

  subroutine sr_vidata(sidat, ridat, n2, n3, dir, side)
   integer, intent(in) :: n2, n3, dir, side
   integer, intent(in) :: sidat(n2, n3)
   integer, intent(out) :: ridat(n2, n3)
   integer :: tag, pes, per, nq
   !==================
   tag = 10 + dir
   nq = n2*n3
   select case (dir)
   case (1)
    if (side > 0) then
     pes = yp_prev(side)
     per = yp_next(side)
    else
     pes = yp_next(-side)
     per = yp_prev(-side)
    end if
   case (2)
    if (side > 0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side > 0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select

   call mpi_sendrecv(sidat(1, 1), nq, mpi_integer, pes, tag, ridat(1, 1), &
                     nq, mpi_integer, per, tag, comm_col(dir), status, error)
   !====================
  end subroutine

  subroutine exchange_pdata(sr, pdata, lenw, ipe, tag)
   integer, intent(in) :: lenw, ipe, tag
   logical, intent(in) :: sr
   real(sp), intent(inout) :: pdata(:)

   if (sr) then
    call mpi_send(pdata(1), lenw, mpi_real, pe_min, tag, comm, error)

   else

    call mpi_recv(pdata(1), lenw, mpi_real, ipe, tag, comm, status, &
                  error)
   end if

  end subroutine

  subroutine exchange_rdata(buff, sr, lenw, ipe, dir, tag)
   real(dp), intent(inout) :: buff(:)
   logical, intent(in) :: sr
   integer, intent(in) :: lenw, ipe, dir, tag

   if (sr) then
    call mpi_send(buff(1), lenw, mpi_sd, ipe, tag, comm_col(dir), error)
   else
    call mpi_recv(buff(1), lenw, mpi_sd, ipe, tag, comm_col(dir), &
                  status, error)
   end if

  end subroutine

  subroutine exchange_rdata_int(buff, sr, lenw, ipe, dir, tag)
   integer, intent(inout) :: buff(:)
   logical, intent(in) :: sr
   integer, intent(in) :: lenw, ipe, dir, tag

   if (sr) then
    call mpi_send(buff(1), lenw, mpi_integer, ipe, tag, comm_col(dir), error)
   else
    call mpi_recv(buff(1), lenw, mpi_integer, ipe, tag, comm_col(dir), &
                  status, error)
   end if

  end subroutine
  !=======================
  subroutine vint_2d_bcast(mydat, n1, n2)
   integer, intent(in) :: n1, n2
   integer, intent(inout), dimension(:, :) :: mydat
   integer :: nt

   nt = n1*n2
   call MPI_BCAST(mydat(1, 1), nt, mpi_integer, pe_min, comm, error)

  end subroutine

  subroutine vint_bcast(mydat, nt)
   integer, intent(in) :: nt
   integer, intent(inout) :: mydat(nt)

   call MPI_BCAST(mydat, nt, mpi_integer, pe_min, comm, error)

  end subroutine

  subroutine int_bcast(mydat)
   integer, intent(in) :: mydat

   call MPI_BCAST(mydat, 1, mpi_integer, pe_min, comm, error)

  end subroutine

  subroutine all_gather_dpreal(rv_send, rv_recv, dir, nt)
   real(dp), intent(inout) :: rv_send(:), rv_recv(:)
   integer, intent(in) :: dir, nt

   call mpi_allgather(rv_send, nt, mpi_sd, rv_recv, nt, mpi_sd, &
                      comm_col(dir), error)
  end subroutine
  subroutine allreduce_dpreal(ib, rv_loc, rv, nt)

   integer, intent(in) :: ib, nt
   real(dp), intent(in) :: rv_loc(:)
   real(dp), intent(out) :: rv(:)

   if (prl) then
    select case (ib)
    case (-1) !min

     call MPI_ALLREDUCE(rv_loc, rv, nt, mpi_sd, mpi_min, comm, error)
    case (0) !sum

     call MPI_ALLREDUCE(rv_loc, rv, nt, mpi_sd, mpi_sum, comm, error)
    case (1) !max

     call MPI_ALLREDUCE(rv_loc, rv, nt, mpi_sd, mpi_max, comm, error)
    end select

   else
    rv = rv_loc
   end if

  end subroutine

#ifdef ENABLE_MPI_LONG_INT
  ! WARNING: unsupported on some architecture!!
  subroutine allreduce_big_int(n0, n1)

   integer(dp), intent(in) :: n0
   integer(dp), intent(out) :: n1

   if (prl) then

    call mpi_allreduce(n0, n1, 1, mpi_long_int, mpi_sum, comm, error)
   end if
   !===========================
  end subroutine
#endif

  subroutine allreduce_sint(ib, dt0, dt_tot)

   integer, intent(in) :: ib
   integer, intent(in) :: dt0
   integer, intent(out) :: dt_tot

   if (prl) then
    select case (ib)
    case (-1) !---------------------------------------------

     call MPI_ALLREDUCE(dt0, dt_tot, 1, mpi_integer, mpi_min, comm, error)
    case (0) 

     call MPI_ALLREDUCE(dt0, dt_tot, 1, mpi_integer, mpi_sum, comm, error)
    case (1) 

     call MPI_ALLREDUCE(dt0, dt_tot, 1, mpi_integer, mpi_max, comm, error)
    end select
   else
    dt_tot = dt0
   end if

  end subroutine

  subroutine allreduce_vint(ib, dt0, dt_tot, nt)

   integer, intent(in) :: ib, nt
   integer, intent(in) :: dt0(nt)
   integer, intent(out) :: dt_tot(nt)

   if (prl) then
    select case (ib)
    case (-1) !max

     call MPI_REDUCE(dt0, dt_tot, nt, mpi_integer, mpi_min, pe_min, comm, &
                     error)
    case (0)
     !==============================
     call MPI_REDUCE(dt0, dt_tot, nt, mpi_integer, mpi_sum, pe_min, comm, &
                     error)
    case (1)

     call MPI_REDUCE(dt0, dt_tot, nt, mpi_integer, mpi_max, pe_min, comm, &
                     error)
    end select
    call MPI_BCAST(dt_tot, nt, mpi_integer, pe_min, comm, error)
   else
    dt_tot = dt0
   end if

  end subroutine

  subroutine bcast_grdata(dat0, n1, n2, n3, nc)
   integer, intent(in) :: n1, n2, n3, nc
   real(dp), intent(inout) :: dat0(:, :, :, :)
   integer :: lenw

   lenw = n1*n2*n3*nc

   call MPI_BCAST(dat0(1, 1, 1, 1), lenw, mpi_sd, pe_min, comm, error)

  end subroutine

  subroutine bcast_realv_sum(ib, dt_prl, dt_tot, nt)

   logical, intent(in) :: ib
   integer, intent(in) :: nt
   real(dp), intent(in) :: dt_prl(nt)
   real(dp), intent(out) :: dt_tot(nt)

   if (prl) then

    call MPI_REDUCE(dt_prl, dt_tot, nt, mpi_sd, mpi_sum, pe_min, comm, &
                    error)
    if (ib) call MPI_BCAST(dt_tot, nt, mpi_sd, pe_min, comm, error)
   else
    dt_tot = dt_prl
   end if

  end subroutine

  subroutine bcast_int_sum(dt_prl, dt_tot)

   integer, intent(in) :: dt_prl
   integer, intent(out) :: dt_tot

   if (prl) then

    call MPI_REDUCE(dt_prl, dt_tot, 1, mpi_integer, mpi_sum, pe_min, comm, &
                    error)
    call MPI_BCAST(dt, 1, mpi_integer, pe_min, comm, error)
   else
    dt = dt_prl
   end if
  end subroutine

  subroutine real_bcast(dt_tot, ndt)

   integer, intent(in) :: ndt
   real(dp) :: dt_tot(ndt)

   call MPI_BCAST(dt_tot, ndt, mpi_sd, pe_min, comm, error)

  end subroutine

  subroutine local_to_global_grdata(buff1, buff2, lenws, ip, dir)
   real(dp), intent(in) :: buff1(:)
   real(dp), intent(out) :: buff2(:)
   integer, intent(in) :: lenws, ip, dir
   integer pes, per, tag

   tag = 250 + ip
   select case (dir)
   case (1)
    per = yp_prev(ip)
    pes = yp_next(ip)
   case (2)
    per = zp_prev(ip)
    pes = zp_next(ip)
   case (3)
    per = xp_prev(ip)
    pes = xp_next(ip)
   end select
   call mpi_sendrecv(buff1(1), lenws, mpi_sd, pes, tag, buff2(1), lenws, &
                     mpi_sd, per, tag, comm_col(dir), status, error)

  end subroutine

  subroutine exchange_bd_3d_data(buff1, lenws, buff2, lenwr, dir, side)
   real(dp), intent(in) :: buff1(:, :, :)
   real(dp), intent(out) :: buff2(:, :, :)
   integer, intent(in) :: lenws, lenwr, dir
   integer(hp_int), intent(in) :: side
   integer pes, per, tag

   !===============================
   ! side > recievies next sends left
   tag = 610

   select case (dir)
   case (1)
    if (side > 0) then
     pes = yp_prev(side)
     per = yp_next(side)
    else
     pes = yp_next(-side)
     per = yp_prev(-side)
    end if
   case (2)
    if (side > 0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side > 0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select
   call mpi_sendrecv(buff1(1, 1, 1), lenws, mpi_sd, pes, tag, buff2(1, 1, 1), lenwr, &
                     mpi_sd, per, tag, comm_col(dir), status, error)

  end subroutine

  subroutine exchange_bdx_data(buff1, buff2, lenws, lenwr, dir, side)
   real(dp), intent(in) :: buff1(:)
   real(dp), intent(out) :: buff2(:)
   integer, intent(in) :: lenws, lenwr, dir
   integer(hp_int), intent(in) :: side
   integer pes, per, tag

   !===============================
   tag = 610

   select case (dir)
   case (1)
    if (side > 0) then
     pes = yp_prev(side)
     per = yp_next(side)
    else
     pes = yp_next(-side)
     per = yp_prev(-side)
    end if
   case (2)
    if (side > 0) then
     pes = zp_prev(side)
     per = zp_next(side)
    else
     pes = zp_next(-side)
     per = zp_prev(-side)
    end if
   case (3)
    if (side > 0) then
     pes = xp_prev(side)
     per = xp_next(side)
    else
     pes = xp_next(-side)
     per = xp_prev(-side)
    end if
   end select
   call mpi_sendrecv(buff1(1), lenws, mpi_sd, pes, tag, buff2(1), lenwr, &
                     mpi_sd, per, tag, comm_col(dir), status, error)

  end subroutine

  subroutine processor_grid_diag
   !============================
   integer :: i
   character(10) :: fname = '          '
   integer, parameter :: lun = 10

   if (mype < 10) then
    write (fname, '(a9,i1)') 'proc_map0', mype
   else
    write (fname, '(a8,i2)') 'proc_map', mype
   end if
   open (lun, file=fname//'.dat', form='formatted')
   write (lun, *) '== local carteisan ranks'
   write (lun, *) imody, imodz, imodx
   write (lun, *) coor(1:3)
   i = 1
   write (lun, *) '====== y-neighbors========='
   write (lun, *) yp_next(i), yp_prev(i)
   write (lun, *) '====== z-neighbors========='
   write (lun, *) zp_next(i), zp_prev(i)
   write (lun, *) '====== x-neighbors========='
   write (lun, *) xp_next(i), xp_prev(i)
   write (lun, *) '======== comm_col==========='
   write (lun, *) comm_col(1:3)
   close (lun)

  end subroutine
  !================== side <0 receives from left   side >0 receives from right
 end module

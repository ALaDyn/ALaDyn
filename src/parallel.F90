 !*****************************************************************************************************!
 !                            Copyright 2008-2018  The ALaDyn Collaboration                            !
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
 use precision_def
 use mpi_var
 use grid_and_particles

#if !defined (__INTEL_COMPILER)
 use mpi
 use fft_lib
 implicit none
#else
#if !defined (_CRESCO)
#define ENABLE_MPI_LONG_INT
#endif
 use fft_lib
 implicit none
 include 'mpif.h'
#endif


 integer, parameter :: offset_kind = MPI_OFFSET_KIND

 integer :: mpi_err
 integer,allocatable :: loc_npart(:,:,:,:),loc_nbpart(:,:,:,:)
 integer,allocatable :: loc_ne_ionz(:,:,:),loc_tpart(:)
 integer,allocatable :: yp_next(:),yp_prev(:)
 integer,allocatable :: zp_next(:),zp_prev(:)
 integer,allocatable :: xp_next(:),xp_prev(:)
 integer,allocatable :: nxh(:),nyh(:),nzh(:)
 !-------------------
 real(dp),allocatable :: fp0x(:,:,:,:),fp1x(:,:,:,:)
 real(dp),allocatable :: fp1(:,:,:),fp2(:,:,:),faux1(:),faux2(:)
 real(dp),allocatable :: aux1(:),aux2(:)

 integer :: comm,status(mpi_status_size),error,mpi_sd
 logical :: pex0,pex1
 integer :: coor(3),comm_col(3),color(3)

 contains


 subroutine check_decomposition
 if (npe_yz>0) then
  nprocy=npe_yz
  if (nz>1) then
   nprocz=npe_yz
  else
   nprocz=1
  endif
  nprocx=mpi_size/nprocy/nprocz
 endif
 if (nprocx < 0 .or. nprocy < 0 .or. nprocz < 0 .or. nprocx*nprocy*nprocz /= mpi_size) then
  if (mpi_rank == 0) then
   write(6,*) 'Invalid MPI decomposition'
   write(6,*) 'mpi_size =',mpi_size
   write(6,*) 'nprocx =',nprocx,'nprocy =',nprocy,'nprocz =',nprocz
   STOP 674
  endif
 endif
 end subroutine check_decomposition



 subroutine start_parallel(ncmp,pkind,bkind,model)
 integer,intent(in) :: ncmp,pkind,bkind,model
 integer :: ipe,pen

 call mpi_init(error)
 call mpi_comm_size(mpi_comm_world,mpi_size,error)
 call mpi_comm_rank(mpi_comm_world,mpi_rank,error)

 call check_decomposition

 npe_xloc=nprocx
 npe_yloc=nprocy
 npe_zloc=nprocz
 npe=mpi_size

 mype=mpi_rank
 pe0=(mype==0)
 pe1=(mype==pe_max)

 prl=(npe>1)
 prlx=(npe_xloc>1)
 prly=(npe_yloc>1)
 prlz=(npe_zloc>1)

 pe_min=0
 pe_max=npe-1

 comm=mpi_comm_world

 mpi_sd=mpi_double_precision
 !================
 call mpi_type_contiguous(ncmp+1,mpi_sd,partype,error)
 call mpi_type_commit(partype,error)
 !================

 !======================
 ndims=3
 dims(1)=npe_yloc
 dims(2)=npe_zloc
 dims(3)=npe_xloc
 !=================
 imodzx=mype/npe_yloc
 imody=mod(mype,npe_yloc)
 imodz=mod(imodzx,npe_zloc)
 imodx=imodzx/npe_zloc

 imodyx=imody+npe_yloc*npe_zloc*imodx
 imodyz=imody+npe_yloc*imodz
 !================= MPI cartesiantopology with (imody,imodz,imodx) coordinates
 ! mype=imody+npe_yloc*imodzx
 ! imodzx=imodz+npe_zloc*imodx
 !----------------------------
 ! mype=imodyx+npe_yloc*imodz
 ! imodyx=imody+npe_yloc*npe_zloc*imodx
 !===================
 ! mype=imodyz+npe_yloc*npe_zloc*imodx
 ! imodyz=imody+npe_yloc*imodz
 !==========================
 !======================
 color(1)=dims(1)*(imodz+dims(2)*imodx)
 !imodzx=>> all pes in the (imodz,imodx) plane for given imody
 color(2)=imody+dims(1)*dims(2)*imodx
 !imodyx=> all pes in the (imody,imodx) plane for given imodz
 color(3)=imody+dims(1)*imodz
 !all pes in the (imody,imodz) plane for given imodx
 call mpi_comm_split(comm,color(1),imody,comm_col(1),error)
 call mpi_comm_rank(comm_col(1),coor(1),error)
 call mpi_comm_split(comm,color(2),imodz,comm_col(2),error)
 call mpi_comm_rank(comm_col(2),coor(2),error)
 call mpi_comm_split(comm,color(3),imodx,comm_col(3),error)
 call mpi_comm_rank(comm_col(3),coor(3),error)
 !============ for diagnostic
 !===========================
 ! Logical idensification of mpi boundary coordinates
 !====================
 pe0y=imody==0
 pe1y=imody==npe_yloc-1
 pe0z=imodz==0
 pe1z=imodz==npe_zloc-1
 pex0=imodx==0
 pex1=imodx==npe_xloc-1
 !========================================
 allocate(loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:pkind))
 loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:pkind)=0
 allocate(loc_ne_ionz(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1))
 loc_ne_ionz(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1)=0
 allocate(loc_tpart(npe))
 loc_tpart(1:npe)=0
!============================
if(pe0) THEN
  write(*,*) 'model model model',model
  write(*,*) 'model_id model_id model_id',model_id
  write(*,*) 'dmodel_id dmodel_id dmodel_id',dmodel_id
endif
 if(model >4)then
   if(pe0) write(*,*) 'im ready to allocate',npe_yloc,npe_zloc,npe_xloc,bkind
  allocate(loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:bkind))
  loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:bkind)=0
   if(pe0) write(*,*) 'i have allocated'
 endif

 allocate(yp_next(npe_yloc),yp_prev(npe_yloc))
 allocate(zp_next(npe_zloc),zp_prev(npe_zloc))
 allocate(xp_next(npe_xloc),xp_prev(npe_xloc))
 !============= output arrays
 allocate(nxh(npe_xloc),nyh(npe_yloc),nzh(npe_zloc))
 !====================
 yp_next(1)=imody
 yp_prev(1)=imody
 if(npe_yloc >1)then
  do ipe=1,npe_yloc-1
   pen=imody+ipe
   yp_next(ipe)=mod(pen,npe_yloc)

   pen=imody-ipe
   if(pen < 0)pen=pen+npe_yloc
   yp_prev(ipe)=pen
  end do
 endif
 zp_next(1)=imodz
 zp_prev(1)=imodz
 if(npe_zloc >1)then
  do ipe=1,npe_zloc-1
   pen=imodz+ipe
   zp_next(ipe)=mod(pen,npe_zloc)

   pen=imodz-ipe
   if(pen < 0)pen=pen+npe_zloc
   zp_prev(ipe)=pen
  end do
 endif
 xp_next(1)=imodx
 xp_prev(1)=imodx
 if(prlx)then
  do ipe=1,npe_xloc-1
   pen=imodx+ipe
   xp_next(ipe)=mod(pen,npe_xloc)

   pen=imodx-ipe
   if(pen < 0)pen=pen+npe_xloc
   xp_prev(ipe)=pen
  end do
 endif
 !call processor_grid_diag

 end subroutine start_parallel

 !===========================

 subroutine mpi_valloc(n1_loc,n2_loc,n3_loc,nvd,ifrm,vsize)
 ! subroutine mpi_valloc(n1,n1_loc,n2,n2_loc,n3_loc,nvd,ifrm,vsize)
 ! integer,intent(in) :: n1,n2
 integer,intent(in) :: n1_loc,n2_loc,n3_loc,nvd,ifrm
 integer,intent(inout) :: vsize
 integer :: lenw,n3m,stl

 stl=3
 if(ifrm <2)stl=5
 n3m=max(n2_loc,n3_loc)
 lenw=nvd*(n1_loc+2)*n3m*stl
 allocate(aux1(lenw),aux2(lenw))
 aux1=0.0
 aux2=0.0
 vsize=vsize+2*lenw

 end subroutine mpi_valloc

 !========================

 subroutine mpi_write_part(buf,bufsize,loc_np,disp,nchar,fout)

 real(sp),intent(in) :: buf(:)
 integer,intent(in) :: bufsize,loc_np,nchar
 integer(offset_kind),intent(in) :: disp
 character(nchar),intent(in) :: fout

 integer :: ierr,thefile

 call mpi_file_open(comm, fout, &
  mpi_mode_wronly + mpi_mode_create, &
  mpi_INFO_NULL,thefile,ierr)

 !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
 !please disable any binary output in Windows/IFORT if it doesn't work for you
 call mpi_file_set_view(thefile, disp, mpi_real, &
  mpi_real, 'native', mpi_info_null, ierr)

 call mpi_file_write(thefile,loc_np,1, mpi_integer, &
  mpi_status_ignore, ierr)
 call mpi_file_write(thefile, buf, bufsize, mpi_real, &
  mpi_status_ignore, ierr)
 call mpi_file_close(thefile, ierr)

 end subroutine mpi_write_part
 !========================
 subroutine mpi_write_part_col(buf,bufsize,disp,nchar,fout)

 real(sp),intent(in) :: buf(:)
 integer,intent(in) :: bufsize,nchar
 integer(offset_kind),intent(in) :: disp
 character(nchar),intent(in) :: fout

 integer :: ierr,thefile

 call mpi_file_open(comm_col(1), fout, &
  mpi_mode_wronly + mpi_mode_create, &
  mpi_INFO_NULL,thefile,ierr)

 !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
 !please disable any binary output in Windows/IFORT if it doesn't work for you
 call mpi_file_set_view(thefile, disp, mpi_real, &
  mpi_real, 'native', &
  mpi_info_null, ierr)

 call mpi_file_write(thefile, buf, bufsize, mpi_real, &
  mpi_status_ignore, ierr)

 call mpi_file_close(thefile, ierr)

 end subroutine mpi_write_part_col
 !========================
 subroutine mpi_write_field(buf,bufsize,header,header_size,disp,nchar,fout)

 real(sp),intent(in) :: buf(:)
 integer,intent(in) :: bufsize,nchar,header_size,header(:)
 integer(offset_kind),intent(in) :: disp
 character(nchar),intent(in) :: fout

 integer :: ierr,thefile

 call mpi_file_open(comm, fout, &
  mpi_mode_wronly + mpi_mode_create, &
  mpi_INFO_NULL,thefile,ierr)

 !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
 !please disable any binary output in Windows/IFORT if it doesn't work for you
 call mpi_file_set_view(thefile, disp, mpi_real, &
  mpi_real, 'native', &
  mpi_info_null, ierr)

 call mpi_file_write(thefile,header,header_size, mpi_integer, &
  mpi_status_ignore, ierr)

 call mpi_file_write(thefile, buf, bufsize, mpi_real, &
  mpi_status_ignore, ierr)

 call mpi_file_close(thefile, ierr)
 end subroutine mpi_write_field
 !========================
 subroutine mpi_write_field_col(buf,bufsize,header,header_size,disp,nchar,fout)
 !different from mpi_write_field because of the different communicator in
 !mpi_file_open

 real(sp),intent(in) :: buf(:)
 integer,intent(in) :: bufsize,nchar,header_size,header(:)
 integer(offset_kind),intent(in) :: disp
 character(nchar),intent(in) :: fout

 integer :: ierr,thefile

 call mpi_file_open(comm_col(1), fout, &
  mpi_mode_wronly + mpi_mode_create, &
  mpi_INFO_NULL,thefile,ierr)

 !mpi_file_set_view is broken on Windows 10 with Intel MPI 5 (don't have money to upgrade MPI-Lib and check)
 !please disable any binary output in Windows/IFORT if it doesn't work for you
 call mpi_file_set_view(thefile, disp, mpi_real, &
  mpi_real, 'native', &
  mpi_info_null, ierr)

 call mpi_file_write(thefile,header,header_size, mpi_integer, &
  mpi_status_ignore, ierr)

 call mpi_file_write(thefile, buf, bufsize, mpi_real, &
  mpi_status_ignore, ierr)

 call mpi_file_close(thefile, ierr)
 end subroutine mpi_write_field_col
 !========================
 subroutine mpi_xinv_data_alloc(n1_loc,n2_loc,n3_loc,nf)
 integer,intent(in) :: n1_loc,n2_loc,n3_loc,nf

 allocate(fp0x(n1_loc,n2_loc,n3_loc,nf))
 allocate(fp1x(n1_loc,n2_loc,n3_loc,nf))

 end subroutine mpi_xinv_data_alloc

 subroutine mpi_ftw_alloc(n1,n2,n2_loc,n3,n3_loc)
 integer,intent(in) :: n1,n2,n2_loc,n3,n3_loc
 integer :: n1_xloc,n2_xloc,lenw
 !=======================
 ! loc_grid allready define


 n1_xloc=n1/npe_yloc
 n2_xloc=n1/npe_zloc
 lenw=n1_xloc*n2_loc*n3_loc
 allocate(faux1(lenw),faux2(lenw))
 allocate(fp1(n1_xloc,n2,n3_loc))
 allocate(fp2(n2_xloc,n2_loc,n3))

 end subroutine mpi_ftw_alloc
 !==========================
 subroutine mpi_ftw_dalloc
 if(allocated(faux1))deallocate(faux1)
 if(allocated(faux2))deallocate(faux2)
 if(allocated(fp1))deallocate(fp1)
 if(allocated(fp2))deallocate(fp2)

 end subroutine mpi_ftw_dalloc

 subroutine end_parallel

 call mpi_finalize(error)

 end subroutine end_parallel
 !----------------
 !=====================================
 subroutine exchange_idata(sr,idat,lenw,ipe,tag)
 integer,intent(in) :: lenw,ipe,tag
 integer,intent(inout) :: idat(:)
 logical,intent(in) :: sr

 if(sr)then

  call mpi_send(idat(1),lenw,mpi_integer,ipe,tag, &
   comm,error)

 else

  call mpi_recv(idat(1),lenw,mpi_integer,ipe,tag, &
   comm,status,error)
 endif

 end subroutine exchange_idata
 !====================
 subroutine exchange_3d_sp_data(sr,dat0,n1,n2,n3,ipe,tag)
 integer,intent(in) :: n1,n2,n3,ipe,tag
 real(sp),intent(inout) :: dat0(:,:,:)
 logical,intent(in) :: sr
 integer :: lenw

 lenw=n1*n2*n3
 if(sr)then

  call mpi_send(dat0(1,1,1),lenw,mpi_real,ipe,tag, &
   comm,error)

 else

  call mpi_recv(dat0(1,1,1),lenw,mpi_real,ipe,tag, &
   comm,status,error)
 endif

 end subroutine exchange_3d_sp_data
 !====================
 subroutine exchange_2d_grdata(sr,dat0,n1,n2,ipe,tag)
 logical,intent(in) :: sr
 real(dp),intent(inout) :: dat0(:,:)
 integer,intent(in) :: n1,n2,ipe,tag
 integer :: lenw

 lenw=n1*n2
 if(sr)then

  call mpi_send(dat0(1,1),lenw,mpi_sd,ipe,tag, &
   comm,error)

 else

  call mpi_recv(dat0(1,1),lenw,mpi_sd,ipe,tag, &
   comm,status,error)
 endif

 end subroutine exchange_2d_grdata
 !---------------------------------
 subroutine exchange_grdata(sr,dat0,lenw,dir,ipe)
 integer,intent(in) :: lenw,dir,ipe
 real(dp),intent(inout) :: dat0(:,:,:,:)
 logical,intent(in) :: sr
 integer :: tag

 tag=10+ipe
 if(sr)then

  call mpi_send(dat0(1,1,1,1),lenw,mpi_sd,ipe,tag, &
   comm_col(dir),error)

 else

  call mpi_recv(dat0(1,1,1,1),lenw,mpi_sd,ipe,tag, &
   comm_col(dir),status,error)
 endif

 end subroutine exchange_grdata

 subroutine realvec_distribute(rs,rv,nproc)
 integer,intent(in) :: nproc
 real(dp),intent(in) :: rs
 real(dp) :: rv(:),rc
 integer :: ipe

 if(.not.pe0)then
  call mpi_send(rs,1,mpi_real,pe_min,20+mype,comm,error)
 else
  rv(1)=rs
  do ipe=1,nproc-1
   call mpi_recv(rc,1,mpi_real,ipe,20+ipe,comm,status,error)
   rv(ipe+1)=rc
  end do
 endif

 end subroutine realvec_distribute

 subroutine intvec_distribute(ns,nc,nproc)
 integer,intent(in) :: ns,nproc
 integer,intent(inout) :: nc(:)
 integer :: ipe,nr

 if(.not.pe0)then
  call mpi_send(ns,1,mpi_integer,pe_min,mype,comm,error)
 else
  nc(1)=ns
  do ipe=1,nproc-1
   call mpi_recv(nr,1,mpi_integer,ipe,ipe,comm,status,error)
   nc(ipe+1)=nr
  end do
 endif

 call mpi_bcast(nc,nproc,mpi_integer,pe_min,comm,error)

 end subroutine intvec_distribute
 !=============
 subroutine intvec_row_distribute(nsy,nsz,nsx)
 integer,intent(in) :: nsy,nsz,nsx
 integer :: ipe,ip,q,ix,ns(3),nr(3)

 ns(1)=nsy
 ns(2)=nsz
 ns(3)=nsx
 if(.not.pe0)then
  call mpi_send(ns,3,mpi_integer,pe_min,20+mype,comm,error)
 else
  nyh(1)=ns(1)
  nzh(1)=ns(2)
  nxh(1)=ns(3)
  do ix=0,npe_xloc-1
   do ip=0,npe_zloc-1
    do q=0,npe_yloc-1
     ipe=q+npe_yloc*(ip+npe_zloc*ix)
     if(ipe>0)then
      call mpi_recv(nr,3,mpi_integer,ipe,20+ipe,comm,status,error)
      nyh(q+1)=nr(1)
      nzh(ip+1)=nr(2)
      nxh(ix+1)=nr(3)
     endif
    end do
   end do
  end do
 endif

 !call mpi_bcast(ngm,2,mpi_integer,pe_min,comm,error)

 end subroutine intvec_row_distribute
 !================
 subroutine sr_idata(ns,nr,dir,side)
 integer,intent(in) :: ns,dir,side
 integer,intent(out) :: nr
 integer :: pes,per,tag

 nr=0
 tag=100+dir
 select case(dir)
 case(1)
  if(side >0)then
   pes=yp_prev(side)      !sends to left ns data
   per=yp_next(side)      !receives from right nr daata
  else
   pes=yp_next(-side)     !sends to right ns data
   per=yp_prev(-side)     !receives form left nr data
  endif
 case(2)
  if(side >0)then
   pes=zp_prev(side)
   per=zp_next(side)
  else
   pes=zp_next(-side)
   per=zp_prev(-side)
  endif
 case(3)
  if(side >0)then
   pes=xp_prev(side)
   per=xp_next(side)
  else
   pes=xp_next(-side)
   per=xp_prev(-side)
  endif
 end select
 call mpi_sendrecv(ns,1,mpi_integer,pes,tag, &
  nr,1,mpi_integer,per,tag, &
  comm_col(dir),status,error)

 end subroutine sr_idata
 !==================
 subroutine sr_pdata(sdata,rdata,ns,nr,dir,side)
 real(dp),intent(in) :: sdata(:)
 real(dp),intent(out) :: rdata(:)
 integer,intent(in) :: ns,nr,dir,side
 integer :: tag,pes,per

 tag=1000+dir
 select case(dir)
 case(1)
  if(side >0)then
   pes=yp_prev(side)
   per=yp_next(side)
  else
   pes=yp_next(-side)
   per=yp_prev(-side)
  endif
 case(2)
  if(side >0)then
   pes=zp_prev(side)
   per=zp_next(side)
  else
   pes=zp_next(-side)
   per=zp_prev(-side)
  endif
 case(3)
  if(side >0)then
   pes=xp_prev(side)
   per=xp_next(side)
  else
   pes=xp_next(-side)
   per=xp_prev(-side)
  endif
 end select
 if(ns*nr >0)then
  call mpi_sendrecv(sdata(1),ns,mpi_sd,pes,tag, &
   rdata(1),nr,mpi_sd,per,tag, &
   comm_col(dir),status,error)
 else
  if(ns >0)call mpi_send(sdata(1),ns,mpi_sd,pes,tag, &
   comm_col(dir),error)
  if(nr>0)call mpi_recv(rdata(1),nr,mpi_sd,per,tag, &
   comm_col(dir),status,error)
 endif
 end subroutine sr_pdata
 !====================
 subroutine sr_vidata(sidat,ridat,n2,n3,dir,side)
 integer,intent(in) :: n2,n3,dir,side
 integer,intent(in) :: sidat(n2,n3)
 integer,intent(out) :: ridat(n2,n3)
 integer :: tag,pes,per,nq

 tag=10+dir
 nq=n2*n3
 select case(dir)
 case(1)
  if(side >0)then
   pes=yp_prev(side)
   per=yp_next(side)
  else
   pes=yp_next(-side)
   per=yp_prev(-side)
  endif
 case(2)
  if(side >0)then
   pes=zp_prev(side)
   per=zp_next(side)
  else
   pes=zp_next(-side)
   per=zp_prev(-side)
  endif
 case(3)
  if(side >0)then
   pes=xp_prev(side)
   per=xp_next(side)
  else
   pes=xp_next(-side)
   per=xp_prev(-side)
  endif
 end select

 call mpi_sendrecv(sidat(1,1),nq,mpi_integer,pes,tag, &
  ridat(1,1),nq,mpi_integer,per,tag, &
  comm_col(dir),status,error)

 end subroutine sr_vidata

 subroutine exchange_pdata(sr,pdata,lenw,ipe,tag)
 integer,intent(in) :: lenw,ipe,tag
 logical,intent(in) :: sr
 real(sp),intent(inout) :: pdata(:)

 if(sr)then
  call mpi_send(pdata(1),lenw,mpi_real,pe_min,tag, &
   comm,error)

 else

  call mpi_recv(pdata(1),lenw,mpi_real,ipe,tag, &
   comm,status,error)
 endif

 end subroutine exchange_pdata
 !----------------------------
 subroutine exchange_rdata(sr,lenw,ipe,dir,tag)
 integer,intent(in) :: lenw,ipe,dir,tag
 logical,intent(in) :: sr

 if(sr)then
  call mpi_send(aux1(1),lenw,mpi_sd,ipe,tag, &
   comm_col(dir),error)
 else
  call mpi_recv(aux2(1),lenw,mpi_sd,ipe,tag, &
   comm_col(dir),status,error)
 endif

 end subroutine exchange_rdata

 subroutine vint_bcast(mydat,nt)
 integer,intent(in) :: nt
 integer,intent(inout) :: mydat(nt)

 call mpi_bcast(mydat,nt,mpi_integer,pe_min,comm,error)

 end subroutine vint_bcast

 subroutine int_bcast(mydat)
 integer,intent(in) :: mydat

 call mpi_bcast(mydat,1,mpi_integer,pe_min,comm,error)

 end subroutine int_bcast
 ! ***************************************************************************
 subroutine all_gather_dpreal(rv_send,rv_recv,dir,nt)
 real(dp),intent(inout) :: rv_send(:),rv_recv(:)
 integer,intent(in) :: dir,nt
 call mpi_allgather(&
  rv_send,nt,mpi_sd,rv_recv,nt,mpi_sd,comm_col(dir),error)
 end subroutine all_gather_dpreal
 subroutine allreduce_dpreal(ib,rv_loc,rv,nt)

 integer,intent(in) :: ib,nt
 real(dp),intent(in) :: rv_loc(:)
 real(dp),intent(out) :: rv(:)

 if(prl)then
  select case(ib)
  case(-1)             !min

   call mpi_allreduce(rv_loc,rv,nt,mpi_sd,mpi_min,comm,error)
  case(0)             !sum

   call mpi_allreduce(rv_loc,rv,nt,mpi_sd,mpi_sum,comm,error)
  case(1)             !max

   call mpi_allreduce(rv_loc,rv,nt,mpi_sd,mpi_max,comm,error)
  end select
  !call mpi_bcast(rv,nt,mpi_sd,pe_min,comm,error)
 else
  rv=rv_loc
 endif

 end subroutine allreduce_dpreal


#ifdef ENABLE_MPI_LONG_INT
 !---------------------------------------------
 ! WARNING: unsupported on some architecture!!
 subroutine allreduce_big_int(n0,n1)

 integer(dp),intent(in) :: n0
 integer(dp),intent(out) ::n1

 if(prl)then

  call mpi_allreduce(n0,n1,1,mpi_long_int,mpi_sum,comm,error)
 endif

 end subroutine allreduce_big_int
 !---------------------------------------------
#endif


 subroutine allreduce_sint(ib,dt0,dt)

 integer,intent(in) :: ib
 integer,intent(in) :: dt0
 integer,intent(out) :: dt

 if(prl)then
  select case(ib)
  case(-1)             !min

   call mpi_reduce(dt0,dt,1,mpi_integer,mpi_min,pe_min,comm,error)
  case(0)             !sum

   call mpi_reduce(dt0,dt,1,mpi_integer,mpi_sum,pe_min,comm,error)
  case(1)             !max

   call mpi_reduce(dt0,dt,1,mpi_integer,mpi_max,pe_min,comm,error)
  end select
  call mpi_bcast(dt,1,mpi_integer,pe_min,comm,error)
 else
  dt=dt0
 endif

 end subroutine allreduce_sint
 !----------------------------------------
 subroutine allreduce_vint(ib,dt0,dt,nt)

 integer,intent(in) :: ib,nt
 integer,intent(in) :: dt0(nt)
 integer,intent(out) :: dt(nt)

 if(prl)then
  select case(ib)
  case(-1)             !min

   call mpi_reduce(dt0,dt,nt,mpi_integer,mpi_min,pe_min,comm,error)
  case(0)             !sum

   call mpi_reduce(dt0,dt,nt,mpi_integer,mpi_sum,pe_min,comm,error)
  case(1)             !max

   call mpi_reduce(dt0,dt,nt,mpi_integer,mpi_max,pe_min,comm,error)
  end select
  call mpi_bcast(dt,nt,mpi_integer,pe_min,comm,error)
 else
  dt=dt0
 endif

 end subroutine allreduce_vint

 subroutine bcast_grdata(dat0,n1,n2,n3,nc)
 integer,intent(in) :: n1,n2,n3,nc
 real(dp),intent(inout) :: dat0(:,:,:,:)
 integer :: lenw

 lenw=n1*n2*n3*nc

 call mpi_bcast(dat0(1,1,1,1),lenw,mpi_sd,pe_min,comm,error)

 end subroutine bcast_grdata

 subroutine bcast_realv_sum(ib,dt_prl,dt,nt)

 logical,intent(in) :: ib
 integer,intent(in) :: nt
 real(dp),intent(in) :: dt_prl(nt)
 real(dp),intent(out) :: dt(nt)

 if(prl)then

  call mpi_reduce(dt_prl,dt,nt,mpi_sd,mpi_sum,pe_min,comm,error)
  if(ib)call mpi_bcast(dt,nt,mpi_sd,pe_min,comm,error)
 else
  dt=dt_prl
 endif

 end subroutine bcast_realv_sum

 subroutine bcast_int_sum(dt_prl,dt)

 integer,intent(in) :: dt_prl
 integer,intent(out) :: dt

 if(prl)then

  call mpi_reduce(dt_prl,dt,1,mpi_integer,mpi_sum,pe_min,comm,error)
  call mpi_bcast(dt,1,mpi_integer,pe_min,comm,error)
 else
  dt=dt_prl
 endif
 end subroutine bcast_int_sum

 subroutine real_bcast(dt,ndt)

 integer,intent(in) :: ndt
 real(dp) :: dt(ndt)

 call mpi_bcast(dt,ndt,mpi_sd,pe_min,comm,error)

 end subroutine real_bcast
 !===============================
 subroutine local_to_global_grdata(lenws,ip,dir)
 integer,intent(in) :: lenws,ip,dir
 integer pes,per,tag


 tag=200+ip
 select case(dir)
 case(1)
  per=yp_prev(ip)
  pes=yp_next(ip)
 case(2)
  per=zp_prev(ip)
  pes=zp_next(ip)
 case(3)
  per=xp_prev(ip)
  pes=xp_next(ip)
 end select
 call mpi_sendrecv(aux1(1),lenws,mpi_sd,pes,tag, &
  aux2(1),lenws,mpi_sd,per,tag, &
  comm_col(dir),status,error)

 end subroutine local_to_global_grdata
 !============================
 subroutine exchange_bdx_data(lenws,lenwr,dir,side)
 integer,intent(in) :: lenws,lenwr,dir,side
 integer pes,per,tag


 tag=200
 !================== side <0 receives from left   side >0 receives from right
 select case(dir)
 case(1)
  if(side >0)then
   pes=yp_prev(side)
   per=yp_next(side)
  else
   pes=yp_next(-side)
   per=yp_prev(-side)
  endif
 case(2)
  if(side >0)then
   pes=zp_prev(side)
   per=zp_next(side)
  else
   pes=zp_next(-side)
   per=zp_prev(-side)
  endif
 case(3)
  if(side >0)then
   pes=xp_prev(side)
   per=xp_next(side)
  else
   pes=xp_next(-side)
   per=xp_prev(-side)
  endif
 end select
 call mpi_sendrecv(aux1(1),lenws,mpi_sd,pes,tag, &
  aux2(1),lenwr,mpi_sd,per,tag, &
  comm_col(dir),status,error)

 end subroutine exchange_bdx_data
 !===================
 subroutine swap_yx_3data(waux,wdata,n1_loc,n2,n3)

 integer,intent(in) :: n1_loc,n2,n3
 real(dp),intent(in) :: waux(:,:,:)
 real(dp),intent(out) :: wdata(:,:,:)

 integer :: pes,per,ip
 integer :: i1,j1,lenws,lenwr,tag,n2_xloc,iy,ix,iz
 integer :: kk
 !-----------------
 !From waux(1:n1,1:n2_xloc) to wdata(1:n1_loc,1:n2)
 n2_xloc=n2/npe_xloc

 do iz=1,n3
  do iy=1,n2_xloc
   j1=iy+n2_xloc*imodx
   do ix=1,n1_loc
    i1=ix+n1_loc*imodx
    wdata(ix,j1,iz)=waux(i1,iy,iz)
   end do
  end do
 end do
 if(npe_yloc==1)return
 lenws=n1_loc*n2_xloc*n3
 lenwr=lenws
 if(size(faux1)<lenwr)then
  deallocate(faux1,faux2)
  allocate(faux1(lenws))
  allocate(faux2(lenwr))
 endif

 do ip=1,npe_xloc-1
  pes=xp_next(ip)
  i1=n1_loc*pes
  per=xp_prev(ip)
  tag=80+ip
  kk=0
  do iz=1,n3
   do iy=1,n2_xloc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=waux(ix+i1,iy,iz)
    end do
   end do
  end do
  call mpi_sendrecv(faux1(1),lenws,mpi_sd,pes,tag, &
   faux2(1),lenwr,mpi_sd,per,tag, &
   comm_col(3),status,error)
  j1=n2_xloc*per
  kk=0
  do iz=1,n3
   do iy=1,n2_xloc
    do ix=1,n1_loc
     kk=kk+1
     wdata(ix,iy+j1,iz)=faux2(kk)
    end do
   end do
  end do
 end do

 end subroutine swap_yx_3data

 subroutine swap_xy_3data(wp1,wp2,n1_loc,n2_loc,n3_loc)

 integer,intent(in) :: n1_loc,n2_loc,n3_loc
 real(dp),intent(in) :: wp1(:,:,:)
 real(dp),intent(out) :: wp2(:,:,:)

 integer :: pes,per,ip
 integer :: i1,j1,lenws,lenwr,tag,iy,ix,iz
 integer :: kk
 !-----------------
 !From wp1(1:n1_loc,1:n2) to wp2(1:n1,1:n2_loc,ic)

 do iz=1,n3_loc
  do iy=1,n2_loc
   j1=iy+n2_loc*imody
   do ix=1,n1_loc
    i1=ix+n1_loc*imody
    wp2(i1,iy,iz)=wp1(ix,j1,iz)
   end do
  end do
 end do
 if(npe_yloc==1)return
 lenws=n1_loc*n2_loc*n3_loc
 lenwr=lenws
 if(size(faux1)<lenwr)then
  deallocate(faux1,faux2)
  allocate(faux1(lenws))
  allocate(faux2(lenwr))
 endif

 do ip=1,npe_yloc-1
  pes=yp_next(ip)
  j1=n2_loc*pes
  per=yp_prev(ip)
  tag=80+ip
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=wp1(ix,iy+j1,iz)
    end do
   end do
  end do
  call mpi_sendrecv(faux1(1),lenws,mpi_sd,pes,tag, &
   faux2(1),lenwr,mpi_sd,per,tag, &
   comm_col(1),status,error)
  i1=n1_loc*per
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     wp2(ix+i1,iy,iz)=faux2(kk)
    end do
   end do
  end do
 end do

 end subroutine swap_xy_3data

 subroutine swap_xz_3data(wp1,wp2,n1_loc,n2_loc,n3_loc)

 integer,intent(in) :: n1_loc,n2_loc,n3_loc
 real(dp),intent(in) :: wp1(:,:,:)
 real(dp),intent(out) :: wp2(:,:,:)

 integer :: pes,per,ip
 integer :: i1,lenw,tag,iy,ix,iz,iz1
 integer :: kk
 !-----------------
 !From wp1(1:n1_loc,1:n3) to wp2(1:n1,1:n3_loc)
 do iz=1,n3_loc
  iz1=iz+n3_loc*imodz
  do iy=1,n2_loc
   do ix=1,n1_loc
    i1=ix+n1_loc*imodz
    wp2(i1,iy,iz)=wp1(ix,iy,iz1)
   end do
  end do
 end do
 if(npe_zloc==1)return
 lenw=n1_loc*n2_loc*n3_loc
 if(size(faux1)<lenw)then
  deallocate(faux1,faux2)
  allocate(faux1(lenw))
  allocate(faux2(lenw))
 endif

 do ip=1,npe_zloc-1
  pes=zp_next(ip)
  iz1=n3_loc*pes
  per=zp_prev(ip)
  tag=80+ip
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=wp1(ix,iy,iz+iz1)
    end do
   end do
  end do
  call mpi_sendrecv(faux1(1),lenw,mpi_sd,pes,tag, &
   faux2(1),lenw,mpi_sd,per,tag, &
   comm_col(2),status,error)
  i1=n1_loc*per
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     wp2(ix+i1,iy,iz)=faux2(kk)
    end do
   end do
  end do
 end do

 end subroutine swap_xz_3data

 subroutine swap_yx_3data_inv(wdata,waux,n1_loc,n2,n3)

 integer,intent(in) :: n1_loc,n2,n3
 real(dp),intent(in) :: wdata(:,:,:)
 real(dp),intent(out) :: waux(:,:,:)

 integer :: pes,per,ip
 integer :: i1,j1,lenws,lenwr,tag,iy,ix,iz,n2_xloc
 integer :: kk
 !-----------------
 !enters wdata(n1_loc,n2,n3)
 !From wdata(1:n1_loc,1:n2) to waux(1:n1,1:n2_xloc)
 !n2_xloc=n2/npe_xloc

 n2_xloc=n2/npe_xloc
 kk=0
 do iz=1,n3
  do iy=1,n2_xloc
   j1=iy+n2_xloc*imodx
   do ix=1,n1_loc
    i1=ix+n1_loc*imodx
    kk=kk+1
    waux(i1,iy,iz)=wdata(ix,j1,iz)
   end do
  end do
 end do
 if(npe_xloc==1)return
 lenwr=n1_loc*n2_xloc*n3
 lenws=lenwr
 if(size(faux1)<lenwr)then
  deallocate(faux1,faux2)
  allocate(faux1(lenws))
  allocate(faux2(lenwr))
 endif
 do ip=1,npe_xloc-1
  pes=xp_next(ip)
  j1=n2_xloc*pes
  per=xp_prev(ip)
  tag=40+ip
  kk=0
  do iz=1,n3
   do iy=1,n2_xloc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=wdata(ix,iy+j1,iz)
    end do
   end do
  end do
  lenws=kk
  lenwr=lenws

  call mpi_sendrecv(faux1(1),lenws,mpi_sd,pes,tag, &
   faux2(1),lenwr,mpi_sd,per,tag, &
   comm_col(3),status,error)
  i1=n1_loc*per
  kk=0
  do iz=1,n3
   do iy=1,n2_xloc
    do ix=1,n1_loc
     kk=kk+1
     waux(ix+i1,iy,iz)=faux2(kk)
    end do
   end do
  end do
 end do
 end subroutine swap_yx_3data_inv

 subroutine swap_xy_3data_inv(wp2,wp1,n1_loc,n2_loc,n3_loc)

 integer,intent(in) :: n1_loc,n2_loc,n3_loc
 real(dp),intent(in) :: wp2(:,:,:)
 real(dp),intent(out) :: wp1(:,:,:)

 integer :: pes,per,ip
 integer :: i1,j1,lenws,lenwr,tag,iy,ix,iz
 integer :: kk
 !-----------------
 !From (1:n1,1:n2_loc) to (1:n1_loc,1:n2)

 kk=0
 do iz=1,n3_loc
  do iy=1,n2_loc
   j1=iy+n2_loc*imody
   do ix=1,n1_loc
    i1=ix+n1_loc*imody
    kk=kk+1
    wp1(ix,j1,iz)=wp2(i1,iy,iz)
   end do
  end do
 end do
 if(npe_yloc==1)return
 lenwr=n1_loc*n2_loc*n3_loc
 lenws=lenwr
 if(size(faux1)<lenwr)then
  deallocate(faux1,faux2)
  allocate(faux1(lenws))
  allocate(faux2(lenwr))
 endif
 do ip=1,npe_yloc-1
  pes=yp_next(ip)
  i1=n1_loc*pes
  per=yp_prev(ip)
  tag=40+ip
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=wp2(ix+i1,iy,iz)
    end do
   end do
  end do
  lenws=kk
  lenwr=lenws

  call mpi_sendrecv(faux1(1),lenws,mpi_sd,pes,tag, &
   faux2(1),lenwr,mpi_sd,per,tag, &
   comm_col(1),status,error)
  j1=n2_loc*per
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     wp1(ix,iy+j1,iz)=faux2(kk)
    end do
   end do
  end do
 end do
 end subroutine swap_xy_3data_inv
 !=====================
 subroutine swap_xz_3data_inv(wp2,wp1,n1_loc,n2_loc,n3_loc)

 integer,intent(in) :: n1_loc,n2_loc,n3_loc
 real(dp),intent(in) :: wp2(:,:,:)
 real(dp),intent(out) :: wp1(:,:,:)

 integer :: pes,per,ip
 integer :: i1,lenw,tag,iy,ix,iz,iz1
 integer :: kk,k1
 !-----------------
 !From (1:n1,1:n3_loc) to (1:n1_loc,1:n3)

 do iz=1,n3_loc
  k1=iz+imodz*n3_loc
  do iy=1,n2_loc
   do ix=1,n1_loc
    i1=ix+n1_loc*imodz
    wp1(ix,iy,k1)=wp2(i1,iy,iz)
   end do
  end do
 end do
 if(npe_zloc==1)return
 lenw=n1_loc*n2_loc*n3_loc
 if(size(faux1)<lenw)then
  deallocate(faux1,faux2)
  allocate(faux1(lenw))
  allocate(faux2(lenw))
 endif
 do ip=1,npe_zloc-1
  pes=zp_next(ip)
  i1=n1_loc*pes
  per=zp_prev(ip)
  tag=40+ip
  kk=0
  do iz=1,n3_loc
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     faux1(kk)=wp2(ix+i1,iy,iz)
    end do
   end do
  end do

  call mpi_sendrecv(faux1(1),lenw,mpi_sd,pes,tag, &
   faux2(1),lenw,mpi_sd,per,tag, &
   comm_col(2),status,error)
  k1=n3_loc*per
  kk=0
  do iz=1,n3_loc
   iz1=iz+k1
   do iy=1,n2_loc
    do ix=1,n1_loc
     kk=kk+1
     wp1(ix,iy,iz1)=faux2(kk)
    end do
   end do
  end do
 end do
 end subroutine swap_xz_3data_inv
 !=====================
 subroutine processor_grid_diag

 integer :: i
 character(10) :: fname='          '
 integer,parameter:: lun=10

 if(mype <10)then
  write (fname,'(a9,i1)')'proc_map0',mype
 else
  write (fname,'(a8,i2)')'proc_map',mype
 endif
 open(lun,file=fname//'.dat',form='formatted')
 write(lun,*)'== local carteisan ranks'
 write(lun,*)imody,imodz,imodx
 write(lun,*)coor(1:3)
 i=1
 write(lun,*)'====== y-neighbors========='
 write(lun,*)yp_next(i),yp_prev(i)
 write(lun,*)'====== z-neighbors========='
 write(lun,*)zp_next(i),zp_prev(i)
 write(lun,*)'====== x-neighbors========='
 write(lun,*)xp_next(i),xp_prev(i)
 write(lun,*)'======== comm_col==========='
 write(lun,*)comm_col(1:3)
 close(lun)
 !
 end subroutine processor_grid_diag
 !====================================
 subroutine pftw2d_sc(w,n1,n2,n2_loc,n3,n3_loc,is,sym)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n2_loc,n3,n3_loc,is,sym
 integer :: n1_loc
 !performs a 2D FFT sin/cosine`on the (y,z) coordinates for a 3D data (x,y,z)
 !sym=1 for a sine transform
 !sym=2 for a cosine transform

 select case(is)
 case(-1)         !
  if(n2 <=2)return
  if(prly)then
   n1_loc=n1/npe_yloc
   call swap_xy_3data_inv(w,fp1,n1_loc,n2_loc,n3_loc)

   call ftw1d_sc(fp1,n1_loc,n2,n3_loc,is,2,sym)
   call swap_xy_3data(fp1,w,n1_loc,n2_loc,n3_loc)
  else
   call ftw1d_sc(w,n1,n2,n3_loc,is,2,sym)
  endif
  !=====================
  if(n3<=2)return
  if(npe_zloc >1) then
   n1_loc=n1/npe_zloc
   call swap_xz_3data_inv(w,fp2,n1_loc,n2_loc,n3_loc)
   call ftw1d_sc(fp2,n1_loc,n2_loc,n3,is,3,sym)
   call swap_xz_3data(fp2,w,n1_loc,n2_loc,n3_loc)
  else
   call ftw1d_sc(w,n1,n2_loc,n3,is,3,sym)
  endif
  !======== exit w(loc)
 case(1)
  ! enters w(loc)
  !========================
  if(n3>1)then
   if(npe_zloc >1)then
    n1_loc=n1/npe_zloc
    call swap_xz_3data_inv(w,fp2,n1_loc,n2_loc,n3_loc)
    call ftw1d_sc(fp2,n1_loc,n2_loc,n3,is,3,sym)
    call swap_xz_3data(fp2,w,n1_loc,n2_loc,n3_loc)
   else
    call ftw1d_sc(w,n1,n2_loc,n3,is,3,sym)
   endif
  endif
  !=================
  if(n2 >1)then
   if(prly)then
    n1_loc=n1/npe_yloc
    call swap_xy_3data_inv(w,fp1,n1_loc,n2_loc,n3_loc)
    call ftw1d_sc(fp1,n1_loc,n2,n3_loc,is,2,sym)
    call swap_xy_3data(fp1,w,n1_loc,n2_loc,n3_loc)
   else
    call ftw1d_sc(w,n1,n2,n3_loc,is,2,sym)
   endif
  endif
 end select
 !===================
 !exit w(loc)
 end subroutine pftw2d_sc
!========================
 subroutine pftw3d_sc(w,n1,n2,n2_loc,n3,n3_loc,is,sym)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n2_loc,n3,n3_loc,is,sym
 integer :: n1_loc

 select case(is)
 case(-1)
  call ftw1d_sc(w,n1,n2_loc,n3_loc,is,1,sym)
  call pftw2d_sc(w,n1,n2,n2_loc,n3,n3_loc,is,sym)
 case(1)
  ! enters w(loc)
  call pftw2d_sc(w,n1,n2,n2_loc,n3,n3_loc,is,sym)
  !========================
   call ftw1d_sc(w,n1,n2_loc,n3_loc,is,1,sym)
 end select
 !===================
 !exit w(loc)
 end subroutine pftw3d_sc
 !============================
 subroutine pftw2d(w,n1,n2,n2_loc,n3,n3_loc,is)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n2_loc,n3,n3_loc,is
 integer :: n1_loc,dir

 select case(is)
 case(-1)
  if(n2 <=2)return
  dir=2
  if(prly)then
   n1_loc=n1/npe_yloc
   call swap_xy_3data_inv(w,fp1,n1_loc,n2_loc,n3_loc)
   call ftw1d(fp1,n1_loc,n2,n3_loc,is,dir)
   call swap_xy_3data(fp1,w,n1_loc,n2_loc,n3_loc)
  else
   call ftw1d(w,n1,n2,n3_loc,is,dir)
  endif
  !=====================
  if(n3<=2)return
  dir=3
  if(npe_zloc >1) then
   n1_loc=n1/npe_zloc
   call swap_xz_3data_inv(w,fp2,n1_loc,n2_loc,n3_loc)
   call ftw1d(fp2,n1_loc,n2_loc,n3,is,dir)
   call swap_xz_3data(fp2,w,n1_loc,n2_loc,n3_loc)
  else
   call ftw1d(w,n1,n2_loc,n3,is,dir)
  endif
  !======== exit w(loc)
 case(1)
  ! enters w(loc)
  !========================
  if(n3>1)then
   dir=3
   if(npe_zloc >1)then
    n1_loc=n1/npe_zloc
    call swap_xz_3data_inv(w,fp2,n1_loc,n2_loc,n3_loc)
    call ftw1d(fp2,n1_loc,n2_loc,n3,is,dir)
    call swap_xz_3data(fp2,w,n1_loc,n2_loc,n3_loc)
   else
    call ftw1d(w,n1,n2_loc,n3,is,dir)
   endif
  endif
  !=================
  if(n2 >1)then
   dir=2
   if(prly)then
    n1_loc=n1/npe_yloc
    call swap_xy_3data_inv(w,fp1,n1_loc,n2_loc,n3_loc)
    call ftw1d(fp1,n1_loc,n2,n3_loc,is,dir)
    call swap_xy_3data(fp1,w,n1_loc,n2_loc,n3_loc)
   else
    call ftw1d(w,n1,n2,n3_loc,is,dir)
   endif
  endif
 end select
 !===================
 !exit w(loc)
 end subroutine pftw2d
!====================================
 subroutine pftw3d(w,n1,n2,n2_loc,n3,n3_loc,is)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n2_loc,n3,n3_loc,is
 integer :: n1_loc

 select case(is)
 case(-1)

  call ftw1d(w,n1,n2_loc,n3_loc,is,1)

  call pftw2d(w,n1,n2,n2_loc,n3,n3_loc,is)

 case(1)
  call pftw2d(w,n1,n2,n2_loc,n3,n3_loc,is)

  call ftw1d(w,n1,n2_loc,n3_loc,is,1)
 end select
 !===================
 !exit w(loc)
 end subroutine pftw3d
 end module parallel
 !================================================================

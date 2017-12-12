 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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

 module pstruct_data
 use precision_def
 use struct_def

 implicit none

 type(species) :: spec(4),bunch(5),electron_ionz
 real(dp),allocatable :: ebfp(:,:),ebfb(:,:)
 real(dp),allocatable :: ebfp0(:,:),ebfp1(:,:)
 real(dp),allocatable :: xpt(:,:),ypt(:,:),zpt(:,:),wghpt(:,:)
 real(dp),allocatable :: loc_ypt(:,:),loc_zpt(:,:),loc_wghy(:,:),loc_wghz(:,:)
 real(dp),allocatable :: loc_xpt(:,:),loc_wghx(:,:)
 real(sp),allocatable :: pdata_tracking(:,:,:)

 !=====================

 contains
 !--------------------------
 !DIR$ ATTRIBUTES INLINE :: p_realloc
 subroutine p_realloc(pdata,npt_new,ndv)
 type(species),intent(inout)  :: pdata
 integer,intent(in) :: npt_new,ndv
 integer :: AllocStatus, DeallocStatus

 if(allocated(pdata%part))then
  if(size(pdata%part,1) < npt_new)then
   deallocate(pdata%part,STAT=DeallocStatus)
   if(DeallocStatus==0)allocate(pdata%part(1:npt_new,1:ndv),STAT=AllocStatus)
  endif
 else
  allocate(pdata%part(1:npt_new,1:ndv),STAT=AllocStatus)
 endif
 pdata%part(:,:)=0.0
 end subroutine p_realloc
 !========================
 subroutine v_realloc(vdata,npt_new,ndv)
 real(dp),allocatable,intent(inout) :: vdata(:,:)
 integer,intent(in) :: npt_new,ndv
 integer :: AllocStatus, DeallocStatus

 if(allocated(vdata))then
  if(size(vdata,1) < npt_new)then
   deallocate(vdata,STAT=DeallocStatus)
   allocate(vdata(1:npt_new,1:ndv),STAT=AllocStatus)
  endif
 else
  allocate(vdata(1:npt_new,1:ndv),STAT=AllocStatus)
 endif
 vdata(:,:)=0.0
 end subroutine v_realloc
 !===========================
 subroutine p_alloc(npt_max,ncmp,np_s,ns,lp,mid,r_type,msize)

 integer,intent(in) :: npt_max,ncmp,np_s(:),ns,lp,mid,r_type
 integer,intent(inout) :: msize
 integer :: nsize,ic,npt,AllocStatus

 npt=1
 select case(r_type)
 case(1)             !Plasma particles
  nsize=0
  do ic=1,ns
   npt=max(np_s(ic),1)
   allocate(spec(ic)%part(npt,ncmp),STAT=AllocStatus)
   nsize=nsize+ncmp*npt
   spec(ic)%part(1:npt,1:ncmp)=0.0
  enddo
  if(mid>0)then
   allocate(ebfp(npt_max,ncmp),STAT=AllocStatus)
   nsize=nsize+ncmp*npt_max
   ebfp(1:npt_max,1:ncmp)=0.0
  endif
  if(lp>2)then
   allocate(ebfp0(npt_max,ncmp),STAT=AllocStatus)
   nsize=nsize+ncmp*npt
   ebfp0(1:npt_max,1:ncmp)=0.0
   if(lp ==4)then
    allocate(ebfp1(npt_max,ncmp),STAT=AllocStatus)
    nsize=nsize+ncmp*npt
    ebfp1(1:npt_max,1:ncmp)=0.0
   endif
  endif
  msize=msize+nsize
 case(2)             !bunch particles
  nsize=0
  do ic=1,ns
   npt=np_s(ic)
   if(npt>0)allocate(bunch(ic)%part(npt,ncmp),STAT=AllocStatus)
   nsize=nsize+ncmp*npt
   bunch(ic)%part(1:npt,1:ncmp)=0.0
  enddo
  if(mid>0)then
   allocate(ebfb(npt_max,ncmp),STAT=AllocStatus)
   nsize=nsize+ncmp*npt_max
   ebfb(1:npt_max,1:ncmp)=0.0
  endif
  msize=msize+nsize
 end select
 end subroutine p_alloc
 !--------------------------
 end module pstruct_data

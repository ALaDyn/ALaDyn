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

 module fstruct_data

 use precision_def

 implicit none

 real(dp),allocatable :: ebf(:,:,:,:),ebf_bunch(:,:,:,:),jc(:,:,:,:)
 real(dp),allocatable :: pot(:,:,:,:),fluid_x_profile(:)
 real(dp),allocatable :: ebf0(:,:,:,:),ebf1(:,:,:,:)
 real(dp),allocatable :: ebf0_bunch(:,:,:,:),ebf1_bunch(:,:,:,:),jb(:,:,:,:)
 integer,allocatable :: sp_count(:,:)
 real(dp),allocatable :: env(:,:,:,:),env1(:,:,:,:)
 real(dp),allocatable :: up(:,:,:,:),up0(:,:,:,:),up1(:,:,:,:),flux(:,:,:,:)
 real(dp),allocatable :: ub(:,:,:,:),ub0(:,:,:,:),ub1(:,:,:,:)
 !--------------------------

 contains

 !--------------------------

 subroutine v_alloc(n1,n2,n3,ncomp,njc,ndm,ns,ifluid,lp,oder,envlp,color,comv,fsize)

 integer,intent(in) ::n1,n2,n3,ncomp,njc,ndm,ns,ifluid,lp,oder
 logical,intent(in) :: envlp,color,comv
 integer,intent(inout) ::fsize
 integer :: njdim,ng,ng0,n1p,n2p,n3p,shx,AllocStatus,fsize_loc
 !==============
 ! ns =nsp for active ionization
 !======================
 !extended grid [1:n1+3]  interior [shx,n1]  
 !overlapping grid [n1-1,n1+shx]=> 1,shx+2  [1,2] <= [n1-1,n1],[shx,shx+2]=>[n1+1,n1+3]
 
 shx=3
 n1p=n1+shx          
 n2p=n2+shx
 n3p=n3
 ng0=1+(n1-2)*(n2-2)
 if(ndm==3)then
  ng0=1+(n1-2)*(n2-2)*(n3-2)
  n3p=n3+shx
 endif
 ng=n1p*n2p*n3p
 fsize_loc=0

 ! allocates common arrays
 allocate(ebf(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
 allocate(jc(n1p,n2p,n3p,njc),STAT=AllocStatus)
 fsize_loc=fsize_loc+ng*ncomp+ng*njc
 ebf=0.0
 jc=0.0
 if(comv)then
  if(lp <3)then     !to handle backward advected fields
   allocate(ebf0(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
   fsize_loc=fsize_loc+ng*ncomp
   ebf0=0.0
  endif
 endif
 if(ifluid==2)then
  if(lp <3)then     !for 2th order lpf in fluid variables
   allocate(ebf0(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
   fsize_loc=fsize_loc+ng*ncomp
   ebf0=0.0
  endif
 endif
 if(ns>0)then
  allocate(sp_count(ng0,ns),STAT=AllocStatus)
  sp_count=0
  fsize_loc=fsize_loc+ns*ng0
 endif
 if(lp>2)then
  allocate(ebf0(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
  ebf0=0.0
  fsize_loc=fsize_loc+ng*ncomp
  if(lp >3)then
   allocate(ebf1(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
   fsize_loc=fsize_loc+ng*ncomp
   ebf1=0.0
  endif
 else
  if(oder==4)then
   allocate(ebf0(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
   allocate(ebf1(n1p,n2p,n3p,ncomp),STAT=AllocStatus)
   allocate(jb(n1p,n2p,n3p,njc),STAT=AllocStatus)
   ebf0=0.0
   ebf1=0.0
   jb=0.0
  endif
 endif
 if(envlp)then
  njdim=4
  if(lp >3) njdim=6
  allocate(env(n1p,n2p,n3p,njdim),STAT=AllocStatus)
  env=0.0
  fsize_loc=fsize_loc+njdim*ng
  if(color)then
   allocate(env1(n1p,n2p,n3p,njdim),STAT=AllocStatus)
   env1=0.0
   fsize_loc=fsize_loc+njdim*ng
  endif
 endif
 fsize=fsize+fsize_loc  !sums all over memory alloc
 end subroutine v_alloc

 !--------------------------

 subroutine bv_alloc(n1,n2,n3,bcomp,ndm,ibch,fsize)

 integer,intent(in) ::n1,n2,n3,bcomp,ndm,ibch
 integer,intent(inout) ::fsize
 integer :: ng,n1p,n2p,n3p,shx,AllocStatus

 shx=3
 n1p=n1+shx       !x-grid ix=1,2 bd, 3:nx+2=n1p data n1p+1 bd
 n2p=n2+shx
 n3p=n3
 if(ndm==3)n3p=n3+shx
 ng=n1p*n2p*n3p
 allocate(ebf_bunch(n1p,n2p,n3p,bcomp),STAT=AllocStatus)
 allocate(jb(n1p,n2p,n3p,ndm),STAT=AllocStatus)
 ebf_bunch=0.0
 jb=0.0

 fsize=fsize+ng*(bcomp+ndm)
 if(ibch>0)then
  allocate(ebf1_bunch(n1p,n2p,n3p,bcomp),STAT=AllocStatus)
  ebf1_bunch=0.0
  fsize=fsize+ng*bcomp
 endif
 ! In 3D  nbcomp=6    in 2D nbcomp=nfield+1=4
 end subroutine bv_alloc
 !==================
 subroutine fluid_alloc(n1,n2,n3,fcomp,ndm,lp,fsize)

 integer,intent(in) ::n1,n2,n3,fcomp,ndm,lp
 integer,intent(inout) ::fsize
 integer :: ng,n1p,n2p,n3p,shx,flcomp,AllocStatus

 shx=3
 n1p=n1+shx       !x-grid ix=1,2 bd, 3:n1+2=n1p data n1+1 bd
 n2p=n2+shx       !overlapping grid y=1,3 = n2-1,n2+1  y=n2+1=shx
 n3p=n3
 flcomp=2*fcomp-1
 if(ndm==3)n3p=n3+shx
 ng=n1p*n2p*n3p
 allocate(up(n1p,n2p,n3p,fcomp),STAT=AllocStatus)
 allocate(up0(n1p,n2p,n3p,fcomp),STAT=AllocStatus)
 allocate(flux(n1p,n2p,n3p,flcomp),STAT=AllocStatus)
 up=0.0
 flux=0.0
 up0=0.0
 fsize=fsize+ng*(2*fcomp+flcomp)
 if(lp >2)then
  allocate(up1(n1p,n2p,n3p,fcomp),STAT=AllocStatus)
  up1=0.0
  fsize=fsize+ng*fcomp
 endif
 end subroutine fluid_alloc
 !============ external B-field allocated
 subroutine bext_alloc(n1,n2,n3,bcomp,ndm,fsize)

 integer,intent(in) ::n1,n2,n3,bcomp,ndm
 integer,intent(inout) ::fsize
 integer :: ng,n1p,n2p,n3p,shx,AllocStatus
 n1p=n1+shx       !x-grid ix=1,2 bd, 3:n1+2=n1p data n1+1 bd
 n2p=n2+shx       !overlapping grid y=1,3 = n2-1,n2+1  y=n2+1=shx
 n3p=n3
 allocate(ebf0_bunch(n1p,n2p,n3p,bcomp),STAT=AllocStatus)
 ebf0_bunch=0.0
 fsize=fsize+bcomp*ng
 end subroutine bext_alloc

 !--------------------------

 subroutine v_dalloc

 if(allocated(ebf))deallocate(ebf)
 if(allocated(ebf0))deallocate(ebf0)
 if(allocated(ebf1))deallocate(ebf1)
 if(allocated(jc))deallocate(jc)

 end subroutine v_dalloc

 !--------------------------

 end module fstruct_data

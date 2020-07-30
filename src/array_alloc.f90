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
!     Local grid structure under mpi domain decomposition
!==============
! grid [1:n]   np=n+2  extended domain [1:np+2]
! interior [3,np]   ghost [1:2], [np+1:np+2]
! overlapping grid  structure:
!
!====================================================================
!                                     1-----2---- 3--- 4   |      pey+1
!                 1-----2----[3-------np-1--np]--np+1--np+2|    pey
!1-2--------------np-1--np---np+1                          |pey-1
!======================================================================
!      Right(pey+1)     [1:4]     overlap pey [np-1:np+2]
!      Left(pey-1)      [np-1:np+2]overlap pey [1:4]
!===================================
 module array_alloc

  use pstruct_data
  use fstruct_data
  implicit none

 contains

  subroutine mpi_buffer_alloc(n1_loc, n2_loc, n3_loc, nvd)
   integer, intent(in) :: n1_loc, n2_loc, n3_loc, nvd
   integer :: lenw, n3m, stl

   stl = 5
   n3m = max(n2_loc, n3_loc)
   lenw = nvd*(n1_loc + 2)*n3m*stl
   allocate (aux1(lenw), aux2(lenw))
   aux1 = 0.0
   aux2 = 0.0

  end subroutine

  subroutine v_alloc(n1, n2, n3, ncomp, njc, ndm, ifluid, lp, oder, &
                     envlp, color, comv, fsize)

   integer, intent(in) :: n1, n2, n3, ncomp, njc, ndm, ifluid, lp, oder
   logical, intent(in) :: envlp, color, comv
   integer, intent(inout) :: fsize
   integer :: njdim, ng, ng0, n1p, n2p, n3p, allocstatus, fsize_loc

   n1p = n1 + ihx
   n2p = n2 + ihx
   n3p = n3
   ng0 = 1 + (n1 - 2)*(n2 - 2)
   if (ndm == 3) then
    ng0 = 1 + (n1 - 2)*(n2 - 2)*(n3 - 2)
    n3p = n3 + ihx
   end if
   ng = n1p*n2p*n3p
   fsize_loc = 0

   ! allocates common arrays
   allocate (ebf(n1p, n2p, n3p, ncomp), stat=allocstatus)
   allocate (jc(n1p, n2p, n3p, njc), stat=allocstatus)
   fsize_loc = fsize_loc + ng*ncomp + ng*njc
   ebf = 0.0
   jc = 0.0
   if (comv) then
    if (lp < 3) then !to handle backward advected fields
     allocate (ebf0(n1p, n2p, n3p, ncomp), stat=allocstatus)
     fsize_loc = fsize_loc + ng*ncomp
     ebf0 = 0.0
    end if
   end if
   if (ifluid == 2) then
    if (lp < 3) then !for 2th order lpf in fluid variables
     if (.not. allocated(ebf0)) then
      allocate (ebf0(n1p, n2p, n3p, ncomp), stat=allocstatus)
      fsize_loc = fsize_loc + ng*ncomp
      ebf0 = 0.0
     end if
    end if
   end if
   if (lp > 2) then
    allocate (ebf0(n1p, n2p, n3p, ncomp), stat=allocstatus)
    ebf0 = 0.0
    fsize_loc = fsize_loc + ng*ncomp
    if (lp > 3) then
     allocate (ebf1(n1p, n2p, n3p, ncomp), stat=allocstatus)
     fsize_loc = fsize_loc + ng*ncomp
     ebf1 = 0.0
    end if
   end if
   !=============ENV allocation section
   if (envlp) then
    njdim = 4
    allocate (env(n1p, n2p, n3p, njdim), stat=allocstatus)
    env = 0.0
    fsize_loc = fsize_loc + njdim*ng
    if (lp == 4) then !RK4 for env solver
     allocate (env0(n1p, n2p, n3p, 2*njdim), stat=allocstatus)
     env0 = 0.0
     fsize_loc = fsize_loc + 2*njdim*ng
    end if
    if (color) then
     allocate (env1(n1p, n2p, n3p, njdim), stat=allocstatus)
     env1 = 0.0
     fsize_loc = fsize_loc + njdim*ng
    end if
   end if
   fsize = fsize + fsize_loc !sums all over memory alloc
  end subroutine

  !--------------------------

  subroutine bv_alloc(n1, n2, n3, bcomp, ndm, ibch, fsize)

   integer, intent(in) :: n1, n2, n3, bcomp, ndm, ibch
   integer, intent(inout) :: fsize
   integer :: ng, n1p, n2p, n3p, allocstatus

   n1p = n1 + ihx !x-grid ix=1,2 bd, 3:nx+2=n1p data n1p+1 bd
   n2p = n2 + ihx
   n3p = n3
   if (ndm == 3) n3p = n3 + ihx
   ng = n1p*n2p*n3p
   allocate (ebf_bunch(n1p, n2p, n3p, bcomp), stat=allocstatus)
   allocate (jb(n1p, n2p, n3p, ndm), stat=allocstatus)
   ebf_bunch = 0.0
   jb = 0.0

   fsize = fsize + ng*(bcomp + ndm)
   if (ibch > 0) then
    allocate (ebf1_bunch(n1p, n2p, n3p, bcomp), stat=allocstatus)
    ebf1_bunch = 0.0
    fsize = fsize + ng*bcomp
   end if
   ! In 3D  nbcomp=6    in 2D nbcomp=nfield+1=4
  end subroutine
  !==================
  subroutine fluid_alloc(n1, n2, n3, fcomp, ndm, lp, fsize)

   integer, intent(in) :: n1, n2, n3, fcomp, ndm, lp
   integer, intent(inout) :: fsize
   integer :: ng, n1p, n2p, n3p, flcomp, allocstatus

   n1p = n1 + ihx !x-grid ix=1,2 bd, 3:n1+2=n1p data n1+1 bd
   n2p = n2 + ihx !overlapping grid y=1,3 = n2-1,n2+1  y=n2+1=ihx
   n3p = n3
   flcomp = 2*fcomp - 1
   if (ndm == 3) n3p = n3 + ihx
   ng = n1p*n2p*n3p
   allocate (up(n1p, n2p, n3p, fcomp), stat=allocstatus)
   allocate (up0(n1p, n2p, n3p, fcomp), stat=allocstatus)
   allocate (flux(n1p, n2p, n3p, flcomp), stat=allocstatus)
   allocate (fluid_yz_profile(n2p, n3p), stat=allocstatus)
   up = 0.0
   flux = 0.0
   up0 = 0.0
   fluid_yz_profile = 0.0
   fsize = fsize + ng*(2*fcomp + flcomp) + n2p*n3p
   if (lp > 2) then
    allocate (up1(n1p, n2p, n3p, fcomp), stat=allocstatus)
    up1 = 0.0
    fsize = fsize + ng*fcomp
   end if
  end subroutine
  !============ external B-field allocated
  subroutine bext_alloc(n1, n2, n3, bcomp, fsize)

   integer, intent(in) :: n1, n2, n3, bcomp
   integer, intent(inout) :: fsize
   integer :: ng, n1p, n2p, n3p, allocstatus

   n1p = n1 + ihx !x-grid ix=1,2 bd, 3:n1+2=n1p data n1+1 bd
   n2p = n2 + ihx !overlapping grid y=1,3 = n2-1,n2+1  y=n2+1=ihx
   n3p = n3
   ng = n1p*n2p*n3p
   allocate (ebf0_bunch(n1p, n2p, n3p, bcomp), stat=allocstatus)
   ebf0_bunch = 0.0
   fsize = fsize + bcomp*ng
  end subroutine
  !===========================
  subroutine p_alloc(npt_max, ncmp, np_s, ns, lp, mid, r_type, msize)

   integer, intent(in) :: npt_max, ncmp, np_s(:), ns, lp, mid, r_type
   integer, intent(inout) :: msize
   integer :: nsize, ic, npt, allocstatus

   npt = 1
   select case (r_type)
   case (1) !Plasma particles
    nsize = 0
    do ic = 1, ns
     npt = max(np_s(ic), 1)
     allocate (spec(ic)%part(npt, ncmp), stat=allocstatus)
     nsize = nsize + ncmp*npt
     spec(ic)%part(1:npt, 1:ncmp) = 0.0
    end do
    if (mid > 0) then
     allocate (ebfp(npt_max, ncmp), stat=allocstatus)
     nsize = nsize + ncmp*npt_max
     ebfp(1:npt_max, 1:ncmp) = 0.0
    end if
    if (lp > 2) then
     allocate (ebfp0(npt_max, ncmp), stat=allocstatus)
     nsize = nsize + ncmp*npt
     ebfp0(1:npt_max, 1:ncmp) = 0.0
     if (lp == 4) then
      allocate (ebfp1(npt_max, ncmp), stat=allocstatus)
      nsize = nsize + ncmp*npt
      ebfp1(1:npt_max, 1:ncmp) = 0.0
     end if
    end if
    msize = msize + nsize
   case (2) !bunch particles are inside spec(1)%npart
    nsize = 0
    msize = msize + nsize
    return
   end select
  end subroutine
  !============================
  !DIR$ ATTRIBUTES INLINE :: p_realloc
  subroutine p_realloc(pdata, npt_new, ndv)
   type(species), intent(inout) :: pdata
   integer, intent(in) :: npt_new, ndv
   integer :: allocstatus, deallocstatus

   if (allocated(pdata%part)) then
    if (size(pdata%part, 1) < npt_new) then
     deallocate (pdata%part, stat=deallocstatus)
     if (deallocstatus == 0) allocate (pdata%part(1:npt_new, 1:ndv), &
                                       stat=allocstatus)
    end if
   else
    allocate (pdata%part(1:npt_new, 1:ndv), stat=allocstatus)
   end if
   pdata%part(:, :) = 0.0
  end subroutine
  !========================
  subroutine v_realloc(vdata, npt_new, ndv)
   real(dp), allocatable, intent(inout) :: vdata(:, :)
   integer, intent(in) :: npt_new, ndv
   integer :: allocstatus, deallocstatus

   if (allocated(vdata)) then
    if (size(vdata, 1) < npt_new) then
     deallocate (vdata, stat=deallocstatus)
     allocate (vdata(1:npt_new, 1:ndv), stat=allocstatus)
    end if
   else
    allocate (vdata(1:npt_new, 1:ndv), stat=allocstatus)
   end if
   vdata(:, :) = 0.0
  end subroutine
  !===========================
 end module


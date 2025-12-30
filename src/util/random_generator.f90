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

 module random_generator

  use precision_def
  use code_util
  use array_util

  implicit none

  interface gasdev
   module procedure gasdev_array 
   module procedure gasdev_real 
  end interface

  contains

  subroutine init_random_seed(myrank)
   integer, intent (in) :: myrank
   integer, allocatable :: seed(:)
   integer :: i, n, un, istat, dt(8), pid, t(2), s
   integer (8) :: count, tms

   i = 0
   call random_seed(size=n)
   allocate (seed(n))

   if (.not. l_disable_rng_seed) then
    un = 123
    ! First try if the OS provides a random number generator
    open (unit=un, file='/dev/urandom', access='stream', &
      form='unformatted', action='read', status='old', iostat=istat)
    if (istat==0) then
     read (un) seed
     close (un)
    else
     ! Fallback to XOR:ing the current time and pid. The PID is
     ! useful in case one launches multiple instances of the same
     ! program in parallel.
     call system_clock(count)
     if (count/=0) then
      t = transfer(count, t)
     else
      call date_and_time(values=dt)
      tms = (dt(1)-1970)*365_8*24*60*60*1000 + dt(2)*31_8*24*60*60*1000 &
        + dt(3)*24*60*60*60*1000 + dt(5)*60*60*1000 + dt(6)*60*1000 + &
        dt(7)*1000 + dt(8)
      t = transfer(tms, t)
     end if
     s = ieor(t(1), t(2))
     pid = myrank + 1099279 ! Add a prime
     s = ieor(s, pid)
     if (n>=3) then
      seed(1) = t(1) + 36269
      seed(2) = t(2) + 72551
      seed(3) = pid
      if (n>3) then
       seed(4:) = s + 37* [ (i,i=0,n-4) ]
      end if
     else
      seed = s + 37* [ (i,i=0,n-1) ]
     end if
    end if
   else
    seed = myrank
   end if
   call random_seed(put=seed)
  end subroutine
  !========================

  subroutine gasdev_real(dev)

   real (dp), intent (out) :: dev
   real (dp) :: v1, v2, rsq
   real (dp), save :: g
   logical, save :: gaus_store = .false.

   if (gaus_store) then
    dev = g
    gaus_store = .false.
   else
    do
     call random_number(v1)
     call random_number(v2)
     v1 = 2.0*v1 - 1.0
     v2 = 2.0*v2 - 1.0
     rsq = v1*v1 + v2*v2
     if (rsq<1.0) exit
    end do
    rsq = sqrt(-2.0*log(rsq)/rsq)
    dev = v1*rsq
    g = v2*rsq
    gaus_store = .true.
   end if
  end subroutine

  !===============================

  subroutine gasdev_array(dev)

   !
   !=======================================================================
   !                                                                      !
   !  Return in harvest a normally distributed deviate with zero mean     !
   !  and unit variance, using RAN1 as the source of uniform deviates.    !
   !                                                                      !
   !  Vector version adapted from Numerical Recipes.                      !
   !                                                                      !
   !  Press, W.H., S.A. Teukolsky, W.T. Vetterling, and B.P. Flannery,    !
   !     1996:  Numerical Recipes in Fortran 90,  The Art of Parallel     !
   !     Scientific Computing, 2nd Edition, Cambridge Univ. Press.        !
   !                                                                      !
   !=======================================================================
   real(dp), dimension(:), intent(out) :: dev
   logical, save :: gaus_stored = .TRUE.
   logical, dimension(SIZE(dev)) :: mask
   integer, save :: last_allocated = 0
   integer :: m, n, ng, nn
   real(dp), dimension(SIZE(dev)) :: rsq, v1, v2
   real(dp), allocatable, dimension(:), save :: g
   !
   !-----------------------------------------------------------------------
   !  Compute a normally distributed vector deviate.
   !-----------------------------------------------------------------------
   !
   !  We have an extra deviate handy, so return it, and unset the flag.
   !
   n = SIZE(dev)
   if (n /= last_allocated) then
    if (last_allocated /= 0) deallocate (g)
    allocate ( g(n) )
    last_allocated = n
    gaus_stored = .FALSE.
   end if
   !
   !  We do not have an extra deviate handy, so pick two uniform numbers
   !  in the square extending from -1 to +1 in each direction.
   !
   if (gaus_stored) then
    dev = g
    gaus_stored = .FALSE.
   else
    ng = 1
    do
     if (ng > n) exit
     call random_number(v1(ng:n))
     call random_number(v2(ng:n))
     v1(ng:n) = 2*one_dp*v1(ng:n) - one_dp
     v2(ng:n) = 2*one_dp*v2(ng:n) - one_dp
     !
     !  See if they are in the unit circle, and if they are not, try again.
     !
     rsq(ng:n) = v1(ng:n)**2 + v2(ng:n)**2
     mask(ng:n) = ((rsq(ng:n) > zero_dp).and.(rsq(ng:n) < one_dp))
     call array_copy(PACK(v1(ng:n), mask(ng:n)), v1(ng:), nn, m)
     v2(ng:ng+nn-1) = PACK(v2(ng:n), mask(ng:n))
     rsq(ng:ng+nn-1) = PACK(rsq(ng:n), mask(ng:n))
     ng=ng+nn
    end do
    !
    !  Make the Box-Muller transformation to get two normal deviates.
    !  Return one and save the other for next time.
    !
    rsq = sqrt(-2*one_dp*log(rsq)/rsq)
    dev = v1*rsq
    g = v2*rsq
    gaus_stored = .TRUE.
   end if

   return

  end subroutine



 end module
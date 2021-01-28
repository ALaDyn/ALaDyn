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

 module mkl_random_generator

  use precision_def
  use code_util
  use mkl_vsl

  implicit none

  type (VSL_STREAM_STATE), save :: stream
  integer, parameter :: method = VSL_RNG_METHOD_GAUSSIAN_BOXMULLER
  integer, parameter :: brng = VSL_BRNG_MCG31
  integer, private :: stat, errstat

  interface gasdev
   module procedure gasdev_array 
   module procedure gasdev_real 
  end interface
  contains

  subroutine init_random_seed(myrank)
   integer, intent (in) :: myrank
   integer :: seed
   integer :: i, un, istat, dt(8), pid, t(2), s
   integer(8) :: count, tms

   i = 0

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
     seed = s
    end if
   else
    seed = myrank
   end if
   errstat = vslNewStream( stream, brng, seed )
  end subroutine

  subroutine gasdev_real(dev)
   real (dp), intent (out) :: dev
   real (dp), parameter :: mean = zero_dp
   real (dp), parameter :: sigma = one_dp
   real (dp) :: r(1)
   stat = vdRngGaussian( method, stream, 1, r, mean, sigma )
   dev = r(1)
  end subroutine
  
  subroutine gasdev_array(dev)
   real (dp), intent (inout), dimension(:) :: dev
   real (dp), parameter :: mean = zero_dp
   real (dp), parameter :: sigma = one_dp
   integer :: n

   n = size(dev)
   stat = vdRngGaussian( method, stream, n, dev, mean, sigma )

  end subroutine

 end module

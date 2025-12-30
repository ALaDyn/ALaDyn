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

 module util

  use precision_def
  use code_util
#if defined(USE_MKL)
  use mkl_random_generator
#else
  use random_generator
#endif
  use warnings

  implicit none
  private
  public :: gasdev, init_random_seed, bunch_gen, write_warning, endian
  public :: trapezoidal_integration, simpson_integration

 contains

  subroutine sort(part, np)

   real(dp), intent(inout) :: part(:)
   integer, intent(in) :: np
   integer :: ir, i, j, k, l, jstack
   integer, parameter :: m = 7, nstack = 50
   real(dp) :: a
   integer :: istack(nstack)

   jstack = 0
   ir = np
   l = 1
   do
    if (ir - l < m) then
     do j = l + 1, ir
      a = part(j)
      do i = j - 1, l, -1
       if (part(i) <= a) exit
       part(i + 1) = part(i)
      end do
      part(i + 1) = a
     end do
     if (jstack == 0) return
     ir = istack(jstack)
     l = istack(jstack - 1)
     jstack = jstack - 2
    else
     k = (l + ir)/2
     call swap(k, l + 1)

     if (part(l) > part(ir)) call swap(l, ir)
     if (part(l + 1) > part(ir)) call swap(l + 1, ir)
     if (part(l) > part(l + 1)) call swap(l, l + 1)

     i = l + 1
     j = ir
     a = part(l + 1)
     do
      do
       i = i + 1
       if (part(i) >= a) exit
      end do
      do
       j = j - 1
       if (part(j) <= a) exit
      end do
      if (j < i) exit
      call swap(i, j)
     end do
     part(l + 1) = part(j)
     part(j) = a
     jstack = jstack + 2
     if (jstack > nstack) exit
     if (ir - i + 1 >= j - l) then
      istack(jstack) = ir
      istack(jstack - 1) = i
      ir = j - 1
     else
      istack(jstack) = j - 1
      istack(jstack - 1) = l
      l = i
     end if
    end if
   end do

  contains

   subroutine swap(i1, i2)
    integer, intent(in) :: i1, i2
    real(dp) :: temp

    temp = part(i1)
    part(i1) = part(i2)
    part(i2) = temp

   end subroutine

  end subroutine

  !=========================

  subroutine vsort(part, np, ndv, dir)

   real(dp), intent(inout) :: part(:, :)
   integer, intent(in) :: np, ndv, dir
   integer :: ir, i, j, k, l, jstack
   integer, parameter :: m = 7, nstack = 50
   real(dp) :: a(ndv), temp(ndv)
   integer :: istack(nstack)

   jstack = 0
   ir = np
   l = 1
   do
    if (ir - l < m) then
     do j = l + 1, ir
      a(1:ndv) = part(1:ndv, j)
      do i = j - 1, l, -1
       if (part(dir, i) <= a(dir)) exit
       part(1:ndv, i + 1) = part(1:ndv, i)
      end do
      part(1:ndv, i + 1) = a(1:ndv)
     end do
     if (jstack == 0) return
     ir = istack(jstack)
     l = istack(jstack - 1)
     jstack = jstack - 2
    else
     k = (l + ir)/2
     call swap(k, l + 1, ndv)

     if (part(dir, l) > part(dir, ir)) call swap(l, ir, ndv)
     if (part(dir, l + 1) > part(dir, ir)) call swap(l + 1, ir, ndv)
     if (part(dir, l) > part(dir, l + 1)) call swap(l, l + 1, ndv)

     i = l + 1
     j = ir
     a(1:ndv) = part(1:ndv, l + 1)
     do
      do
       i = i + 1
       if (part(dir, i) >= a(dir)) exit
      end do
      do
       j = j - 1
       if (part(dir, j) <= a(dir)) exit
      end do
      if (j < i) exit
      call swap(i, j, ndv)
     end do
     part(1:ndv, l + 1) = part(1:ndv, j)
     part(1:ndv, j) = a(1:ndv)
     jstack = jstack + 2
     if (jstack > nstack) exit
     if (ir - i + 1 >= j - l) then
      istack(jstack) = ir
      istack(jstack - 1) = i
      ir = j - 1
     else
      istack(jstack) = j - 1
      istack(jstack - 1) = l
      l = i
     end if
    end if
   end do

  contains

   subroutine swap(i1, i2, nd)
    integer, intent(in) :: i1, i2, nd

    temp(1:nd) = part(1:nd, i1)
    part(1:nd, i1) = part(1:nd, i2)
    part(1:nd, i2) = temp(1:nd)

   end subroutine

  end subroutine

  !===========================
  subroutine bunch_gen(ndm, n1, n2, sx, sy, sz, gm, ey, ez, cut, dg, &
                       bunch)
   integer, intent(in) :: ndm, n1, n2
   real(dp), intent(in) :: sx, sy, sz, gm, ey, ez, cut, dg
   real(dp), intent(inout) :: bunch(:, :)
   integer :: i, j, np
   real(dp) :: sigs(6)
   real(dp) :: xm, ym, zm, pxm, pym, pzm
   real(dp) :: v1, v2, rnd, a, np_norm

   !============= ey,ez are emittances (in mm-microns)
   ! FIX emittances are ALWAYS a dimension times an angle...
   ! so this (in mm-micron) doesn't make any sense
   ! dg=d(gamma)/gamma (%)
   ! dp_y=ey/s_y dp_z=ez/s_z dp_x=d(gamma)
   !=============================================

   !Distribute (x,y,z,px,py,pz) centered on 0; px=> px+gamma

   select case (ndm)
   case (2)
    sigs(1) = sx
    sigs(2) = sy
    sigs(3) = sqrt(3.0)*0.01*dg*gm !dpz
    sigs(4) = ey/sy
    do i = n1, n2
     do
      call random_number(v1)
      call random_number(v2)
      v1 = 2.0*v1 - 1.0
      v2 = 2.0*v2 - 1.0
      rnd = v1*v1 + v2*v2
      if (rnd < 1.0) exit
     end do
     rnd = sqrt(-2.0*log(rnd)/rnd)
     bunch(i, 2) = v1*rnd
     bunch(i, 4) = v2*rnd
     call gasdev(rnd)
     bunch(i, 1) = rnd
     do
      call random_number(rnd)
      rnd = 2.*rnd - 1.
      a = cut*rnd
      if (a*a < 1.) exit
     end do
     bunch(i, 3) = a
    end do
    do j = 1, 4
     bunch(n1:n2, j) = sigs(j)*bunch(n1:n2, j)
    end do
    bunch(n1:n2, 3) = bunch(n1:n2, 3) + gm
    xm = 0.0
    ym = 0.0
    pxm = 0.0
    pym = 0.0
    ! Reset centering
    xm = sum(bunch(n1:n2, 1))
    ym = sum(bunch(n1:n2, 2))
    pxm = sum(bunch(n1:n2, 3))
    pym = sum(bunch(n1:n2, 4))

    np = n2 + 1 - n1
    xm = xm/real(np, dp)
    ym = ym/real(np, dp)
    pxm = pxm/real(np, dp)
    pym = pym/real(np, dp)
    do i = n1, n2
     bunch(i, 1) = bunch(i, 1) - xm
     bunch(i, 2) = bunch(i, 2) - ym
     bunch(i, 3) = bunch(i, 3) - (pxm - gm)
     bunch(i, 4) = bunch(i, 4) - pym
    end do
   case (3)
    sigs(1) = sx
    sigs(2) = sy
    sigs(3) = sz
    sigs(4) = sqrt(3.0)*0.01*dg*gm !dpz
    sigs(5) = ey/sy
    sigs(6) = ez/sz
    do j = 2, 5, 3
     do i = n1, n2
      do
       call random_number(v1)
       call random_number(v2)
       v1 = 2.0*v1 - 1.0
       v2 = 2.0*v2 - 1.0
       rnd = v1*v1 + v2*v2
       if (rnd < 1.0) exit
      end do
      rnd = sqrt(-2.0*log(rnd)/rnd)
      bunch(i, j) = v1*rnd
      bunch(i, j + 1) = v2*rnd
     end do
    enddo
    j = 1
    do i = n1, n2
     call gasdev(rnd)
     bunch(i, j) = rnd
     do
      call random_number(rnd)
      rnd = 2.*rnd - 1.
      a = cut*rnd
      if (a*a < 1.) exit
     end do
     bunch(i, j + 3) = a
    end do
!======================
    do j = 1, 6
     do i = n1, n2
      bunch(i, j) = sigs(j)*bunch(i, j)
     end do
    end do
    bunch(n1:n2, 4) = bunch(n1:n2, 4) + gm

    xm = 0.0
    ym = 0.0
    zm = 0.0
    pxm = 0.0
    pym = 0.0
    pzm = 0.0

    ! Reset centering
    np = n2 + 1 - n1
    np_norm = 1./real(np, dp)
    xm = np_norm*sum(bunch(n1:n2, 1))
    ym = np_norm*sum(bunch(n1:n2, 2))
    zm = np_norm*sum(bunch(n1:n2, 3))
    pxm = np_norm*sum(bunch(n1:n2, 4))
    pym = np_norm*sum(bunch(n1:n2, 5))
    pzm = np_norm*sum(bunch(n1:n2, 6))

    do i = n1, n2
     bunch(i, 1) = bunch(i, 1) - xm
     bunch(i, 2) = bunch(i, 2) - ym
     bunch(i, 3) = bunch(i, 3) - zm
     bunch(i, 4) = bunch(i, 4) - (pxm - gm)
     bunch(i, 5) = bunch(i, 5) - pym
     bunch(i, 6) = bunch(i, 6) - pzm
    end do
   end select
  end subroutine

  subroutine endian(iend)
   implicit none
   integer, intent (out) :: iend
   integer, parameter :: ik1 = selected_int_kind(2)
   integer, parameter :: ik4 = selected_int_kind(9)

   iend = 0
   if (btest(transfer(int([1,0,0,0],ik1),1_ik4),0)) then
    iend = 1
   else
    iend = 2
   end if
  end subroutine

  !=========================================
  ! Numerical integration methods
  !=========================================

  subroutine trapezoidal_integration(dx_in, field_in, result_in, lb_in, ub_in, jlb, jub, klb, kub)
    real(dp), intent(in) :: dx_in
    real(dp), allocatable, dimension(:, :, :), intent(in) :: field_in
    real(dp), allocatable, dimension(:, :, :), intent(inout) :: result_in
    integer, intent(in) :: lb_in, ub_in, jlb, jub, klb, kub
    integer :: i

    do i = ub_in, lb_in, -1
     result_in( i, jlb:jub, klb:kub) = - 0.5 * dx_in * &
      (field_in( i + 1, jlb:jub, klb:kub ) + field_in( i, jlb:jub, klb:kub)) + &
      result_in( i + 1, jlb:jub, klb:kub )
    end do
 
   end subroutine

  subroutine simpson_integration(dx_in, field_in, result_in, lb_in, ub_in, jlb, jub, klb, kub)
    real(dp), intent(in) :: dx_in
    real(dp), allocatable, dimension(:, :, :), intent(in) :: field_in
    real(dp), allocatable, dimension(:, :, :), intent(inout) :: result_in
    integer, intent(in) :: lb_in, ub_in, jlb, jub, klb, kub
    integer :: i
    real(dp), parameter :: one_third = one_dp/3.

    do i = ub_in, lb_in, -1
     result_in( i, jlb:jub, klb:kub) = - one_third * dx_in * &
      (field_in( i, jlb:jub, klb:kub ) + 4*field_in( i + 1, jlb:jub, klb:kub) + field_in( i + 2, jlb:jub, klb:kub )) + &
      result_in( i + 2, jlb:jub, klb:kub )
    end do
 
   end subroutine

 end module

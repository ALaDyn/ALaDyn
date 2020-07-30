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

 module modern_fft_lib

  use precision_def
  use, intrinsic :: iso_c_binding

  implicit none
  include 'fftw3.f03'

  !=== Real arrays for the r2r transforms
  real(C_DOUBLE), pointer :: in1_1d(:)
  !! Array for the FFT along x
  real(C_DOUBLE), pointer :: in2_1d(:)
  !! Array for the FFT along y
  real(C_DOUBLE), pointer :: in3_1d(:)
  !! Array for the FFT along z
  type(C_PTR) :: data1_sc, data2_sc, data3_sc
  !! C Pointers assigned to the arrays

  !=== Plans that define the sine and cosine transform ===
  type(C_PTR) :: plan1_s, plan1_c
  !! Plans of the sine and cosine FFT along x
  type(C_PTR) :: plan2_s, plan2_c
  !! Plans of the sine and cosine FFT along y
  type(C_PTR) :: plan3_s, plan3_c
  !! Plans of the sine and cosine FFT along z

  !=== Old plans kept for backward compatibility
  !=== (with the old_ftw1d_sc)
  type(C_PTR) :: plan1, iplan1
  type(C_PTR) :: plan2, iplan2
  type(C_PTR) :: plan3, iplan3

  !=== Old variables
  real(C_DOUBLE), pointer :: w1_re(:), w2_re(:), w3_re(:)
  complex(C_DOUBLE_COMPLEX), pointer :: w1_cplx(:), w2_cplx(:), w3_cplx(:)
  type(C_PTR) :: data1, data2, data3
  real(dp), allocatable :: cw(:, :), w1_st(:)
  real(dp), allocatable :: cfhx(:), sfhx(:)
  real(dp), allocatable :: cfhy(:), sfhy(:)
  real(dp), allocatable :: cfhz(:), sfhz(:)

 contains

  pure function logical_dimension(kind_flag, N) result(log_dim)
  !! Returns the logical dimension given the FFT Kind.
  !! To be generalized for other applications than
  !! FFTW_RODFT00 and FFTW_REDFT00

   integer(C_INTPTR_T), intent(in) :: kind_flag
   integer, intent(in) :: N
   integer :: log_dim
   select case (kind_flag)
   case (FFTW_RODFT00)
    log_dim = 2*(N + 1)
   case (FFTW_REDFT00)
    log_dim = 2*(N - 1)
   case default
    log_dim = 2*N
   end select
  end function

  pure function determine_kind(sym_index) result(kind)
  !! Converts the integer flag `sym` into the proper FFT kind
   integer, intent(in) :: sym_index
   integer(C_INTPTR_T) :: kind

   select case (sym_index)
   case (1)
    kind = FFTW_RODFT00
   case (2)
    kind = FFTW_REDFT00
   end select

  end function
  subroutine ftw_init(n1, n2, n3, ind_ft)
   !! Initialization of all the FFT variables and plans
   integer, intent(in) :: n1, n2, n3, ind_ft
   integer :: i, nm
   real(dp) :: wk

   !!$PRAGMA C( DFFTW_PLAN_DFT_R2C_1D, DFFTW_PLAN_DFT_C2R_1D )

   select case (ind_ft)
   case (0)
    nm = max(n1, n2)
    data1 = fftw_alloc_complex(int(n1/2 + 1, C_SIZE_T))
    call c_f_pointer(data1, w1_re, [2*(n1/2 + 1)])
    call c_f_pointer(data1, w1_cplx, [n1/2 + 1])
    allocate (w1_st(nm + 1), cfhx(n1), sfhx(n1))
    wk = acos(-1.0)/real(n1, dp)
    do i = 1, n1
     cfhx(i) = cos(real(i - 1, dp)*wk)
     sfhx(i) = sin(real(i - 1, dp)*wk)
    end do
    w1_re(:) = 0.0
    w1_cplx(:) = 0.0
    plan1 = fftw_plan_dft_r2c_1d(n1, w1_re, w1_cplx, fftw_estimate)
    iplan1 = fftw_plan_dft_c2r_1d(n1, w1_cplx, w1_re, fftw_estimate)

    data2 = fftw_alloc_complex(int(n2/2 + 1, C_SIZE_T))
    call c_f_pointer(data2, w2_re, [2*(n2/2 + 1)])
    call c_f_pointer(data2, w2_cplx, [n2/2 + 1])
    allocate (cfhy(n2), sfhy(n2))
    wk = acos(-1.0)/real(n2, dp)
    do i = 1, n2
     cfhy(i) = cos(real(i - 1, dp)*wk)
     sfhy(i) = sin(real(i - 1, dp)*wk)
    end do
    w2_re(:) = 0.0
    w2_cplx(:) = 0.0
    plan2 = fftw_plan_dft_r2c_1d(n2, w2_re, w2_cplx, fftw_estimate)
    iplan2 = fftw_plan_dft_c2r_1d(n2, w2_cplx, w2_re, fftw_estimate)

    data3 = fftw_alloc_complex(int(n3/2 + 1, C_SIZE_T))
    call c_f_pointer(data3, w3_re, [2*(n3/2 + 1)])
    call c_f_pointer(data3, w3_cplx, [n3/2 + 1])
    allocate (cfhz(n3), sfhz(n3))
    wk = acos(-1.0)/real(n3, dp)
    do i = 1, n3
     cfhz(i) = cos(real(i - 1, dp)*wk)
     sfhz(i) = sin(real(i - 1, dp)*wk)
    end do
    w3_re(:) = 0.0
    w3_cplx(:) = 0.0
    plan3 = fftw_plan_dft_r2c_1d(n3, w3_re, w3_cplx, fftw_estimate)
    iplan3 = fftw_plan_dft_c2r_1d(n3, w3_cplx, w3_re, fftw_estimate)

   case (1)
    data1 = fftw_alloc_complex(int(n1/2 + 1, C_SIZE_T))
    call c_f_pointer(data1, w1_re, [2*(n1/2 + 1)])
    call c_f_pointer(data1, w1_cplx, [n1/2 + 1])
    w1_re(:) = 0.0
    w1_cplx(:) = 0.0
    plan1 = fftw_plan_dft_r2c_1d(n1, w1_re, w1_cplx, fftw_estimate)
    iplan1 = fftw_plan_dft_c2r_1d(n1, w1_cplx, w1_re, fftw_estimate)

    data2 = fftw_alloc_complex(int(n2/2 + 1, C_SIZE_T))
    call c_f_pointer(data2, w2_re, [2*(n2/2 + 1)])
    call c_f_pointer(data2, w2_cplx, [n2/2 + 1])
    w2_re(:) = 0.0
    w2_cplx(:) = 0.0
    plan2 = fftw_plan_dft_r2c_1d(n2, w2_re, w2_cplx, fftw_estimate)
    iplan2 = fftw_plan_dft_c2r_1d(n2, w2_cplx, w2_re, fftw_estimate)

    data3 = fftw_alloc_complex(int(n3/2 + 1, C_SIZE_T))
    call c_f_pointer(data3, w3_re, [2*(n3/2 + 1)])
    call c_f_pointer(data3, w3_cplx, [n3/2 + 1])
    w3_re(:) = 0.0
    w3_cplx(:) = 0.0
    plan3 = fftw_plan_dft_r2c_1d(n3, w3_re, w3_cplx, fftw_estimate)
    iplan3 = fftw_plan_dft_c2r_1d(n3, w3_cplx, w3_re, fftw_estimate)
   case (2) !for sin/cos transforms
    data1 = fftw_alloc_complex(int(n1 + 1, C_SIZE_T))
    call c_f_pointer(data1, w1_re, [2*(n1 + 1)])
    call c_f_pointer(data1, w1_cplx, [n1 + 1])
    w1_re(:) = 0.0
    w1_cplx(:) = 0.0
    plan1 = fftw_plan_dft_r2c_1d(2*n1, w1_re, w1_cplx, fftw_estimate)
    iplan1 = fftw_plan_dft_c2r_1d(2*n1, w1_cplx, w1_re, fftw_estimate)

    data2 = fftw_alloc_complex(int(n2 + 1, C_SIZE_T))
    call c_f_pointer(data2, w2_re, [2*(n2 + 1)])
    call c_f_pointer(data2, w2_cplx, [n2 + 1])
    w2_re(:) = 0.0
    w2_cplx(:) = 0.0
    plan2 = fftw_plan_dft_r2c_1d(2*n2, w2_re, w2_cplx, fftw_estimate)
    iplan2 = fftw_plan_dft_c2r_1d(2*n2, w2_cplx, w2_re, fftw_estimate)

    data3 = fftw_alloc_complex(int(n3 + 1, C_SIZE_T))
    call c_f_pointer(data3, w3_re, [2*(n3 + 1)])
    call c_f_pointer(data3, w3_cplx, [n3 + 1])
    w3_re(:) = 0.0
    w3_cplx(:) = 0.0
    plan3 = fftw_plan_dft_r2c_1d(2*n3, w3_re, w3_cplx, fftw_estimate)
    iplan3 = fftw_plan_dft_c2r_1d(2*n3, w3_cplx, w3_re, fftw_estimate)

    !===== New version based on r2r transforms =====
    data1_sc = fftw_alloc_complex(int(n1, C_SIZE_T))
    call c_f_pointer(data1_sc, in1_1d, [n1])

    in1_1d(:) = zero_dp
    plan1_s = fftw_plan_r2r_1d(n1, in1_1d, in1_1d, FFTW_RODFT00, &
                               FFTW_ESTIMATE)
    plan1_c = fftw_plan_r2r_1d(n1, in1_1d, in1_1d, FFTW_REDFT00, &
                               FFTW_ESTIMATE)

    data2_sc = fftw_alloc_complex(int(n2, C_SIZE_T))
    call c_f_pointer(data2_sc, in2_1d, [n2])

    in2_1d(:) = zero_dp
    plan2_s = fftw_plan_r2r_1d(n2, in2_1d, in2_1d, FFTW_RODFT00, &
                               FFTW_ESTIMATE)
    plan2_c = fftw_plan_r2r_1d(n2, in2_1d, in2_1d, FFTW_REDFT00, &
                               FFTW_ESTIMATE)

    data3_sc = fftw_alloc_complex(int(n3, C_SIZE_T))
    call c_f_pointer(data3_sc, in3_1d, [n3])

    in3_1d(:) = zero_dp
    plan3_s = fftw_plan_r2r_1d(n3, in3_1d, in3_1d, FFTW_RODFT00, &
                               FFTW_ESTIMATE)
    plan3_c = fftw_plan_r2r_1d(n3, in3_1d, in3_1d, FFTW_RODFT00, &
                               FFTW_ESTIMATE)

   end select
  end subroutine

  subroutine ftw_end
   !! Routines that ends all the FFTs. It destroys the existing plans
   !! and deallocates the arrays.
   if (allocated(w1_st)) deallocate (w1_st)
   if (allocated(cw)) deallocate (cw)
   call fftw_destroy_plan(plan1)
   call fftw_destroy_plan(plan2)
   call fftw_destroy_plan(plan3)
   call fftw_destroy_plan(iplan1)
   call fftw_destroy_plan(iplan2)
   call fftw_destroy_plan(iplan3)
   call fftw_destroy_plan(plan1_s)
   call fftw_destroy_plan(plan2_s)
   call fftw_destroy_plan(plan3_s)
   call fftw_destroy_plan(plan1_c)
   call fftw_destroy_plan(plan2_c)
   call fftw_destroy_plan(plan3_c)
   call fftw_free(data1)
   call fftw_free(data2)
   call fftw_free(data3)
   call fftw_free(data1_sc)
   call fftw_free(data2_sc)
   call fftw_free(data3_sc)

  end subroutine

  subroutine ftw1d_st(w, n1, n2, n3, is, dir)
   real(dp), intent(inout) :: w(:, :, :)
   !! WARNING: Not used in the code, need to be checked
   integer, intent(in) :: n1, n2, n3, is, dir
   integer :: ii, ix, iy, iz, i1, i2, n1_tr, n2_tr
   real(dp) :: sc, wrr, wir, wri, wii

   !!$PRAGMA C( DFFTW_EXECUTE )

   !=============================
   !staggered (k_x,k_y,k_z)
   !===================
   select case (dir)
   case (1)
    if (is < 0) then
     sc = 1./real(n1, dp)
     do iz = 1, n3
      do iy = 1, n2
       do ix = 1, n1
        w1_re(ix) = cfhx(ix)*w(ix, iy, iz)
       end do
       call fftw_execute_dft_r2c(plan1, w1_re, w1_cplx)
       w1_st(1:n1) = w1_cplx(1:n1)
       do ix = 1, n1
        w1_re(ix) = sfhx(ix)*w(ix, iy, iz)
       end do
       call fftw_execute_dft_r2c(plan1, w1_re, w1_cplx)
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = i2 - 1
        w(i1, iy, iz) = sc*(w1_st(i1) + w1_cplx(i2))
        w(i2, iy, iz) = sc*(w1_st(i2) - w1_cplx(i1))
       end do
      end do
     end do
    else
     n1_tr = n1/2 + n1/4
     do iz = 1, n3
      do iy = 1, n2
       w(n1_tr + 1:n1, iy, iz) = 0.0
       w1_cplx(n1 + 1:n1 + 2) = 0.0
       w1_cplx(1:n1) = w(1:n1, iy, iz)
       call fftw_execute_dft_c2r(iplan1, w1_cplx, w1_re)
       w1_st(1:n1) = w1_re(1:n1)
       w1_cplx(1:n1 + 2) = 0.0
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = i2 - 1
        w1_cplx(i1) = -w(i2, iy, iz)
        w1_cplx(i2) = w(i1, iy, iz)
       end do
       call fftw_execute_dft_c2r(iplan1, w1_cplx, w1_re)
       do ix = 1, n1
        w(ix, iy, iz) = cfhx(ix)*w1_st(ix) + sfhx(ix)*w1_re(ix)
       end do
      end do
     end do
    end if
   case (2)
    allocate (cw(n1, n2))
    if (is < 0) then
     sc = 1./real(n2, dp)
     do iz = 1, n3
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do ii = i1, i2
        do iy = 1, n2
         w2_re(iy) = cfhy(iy)*w(ii, iy, iz)
        end do
        call fftw_execute_dft_r2c(plan2, w2_re, w2_cplx)
        do iy = 1, n2
         cw(ii, iy) = sc*w2_cplx(iy)
         w2_re(iy) = sfhy(iy)*w(ii, iy, iz)
        end do
        call fftw_execute_dft_r2c(plan2, w2_re, w2_cplx)
        do iy = 1, n2/2
         cw(ii, 2*iy - 1) = cw(ii, 2*iy - 1) + sc*w2_cplx(2*iy)
         cw(ii, 2*iy) = cw(ii, 2*iy) - sc*w2_cplx(2*iy - 1)
        end do
       end do
      end do
      !=========== reordering as 2D fft
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do iy = 1, n2/2
        wrr = cw(i1, 2*iy - 1)
        wri = cw(i1, 2*iy)
        wir = cw(i2, 2*iy - 1)
        wii = cw(i2, 2*iy)
        w(i1, iy, iz) = wrr - wii
        w(i2, iy, iz) = wri + wir
        w(i1, n2 + 1 - iy, iz) = wrr + wii
        w(i2, n2 + 1 - iy, iz) = wir - wri
       end do
      end do
     end do
    else
     n2_tr = n2/2 + n2/4
     do iz = 1, n3
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do iy = 1, n2/2
        cw(i1, 2*iy - 1) = 0.5*(w(i1, iy, iz) + w(i1, n2 + 1 - iy, iz)) !wrr
        cw(i1, 2*iy) = 0.5*(w(i2, iy, iz) - w(i2, n2 + 1 - iy, iz)) !wri
        cw(i2, 2*iy - 1) = 0.5*(w(i2, iy, iz) + w(i2, n2 + 1 - iy, iz)) !wir
        cw(i2, 2*iy) = 0.5*(w(i1, n2 + 1 - iy, iz) - w(i1, iy, iz)) !wii
       end do
      end do
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do ii = i1, i2
        cw(ii, n2_tr + 1:n2) = 0.0
        w2_cplx(n2 + 1:n2 + 2) = 0.0
        do iy = 1, n2
         w2_cplx(iy) = cw(ii, iy)
        end do
        call fftw_execute_dft_c2r(iplan2, w2_cplx, w2_re)
        do iy = 1, n2
         w(ii, iy, iz) = cfhy(iy)*w2_re(iy)
        end do
        w2_cplx(n2 + 1:n2 + 2) = 0.0
        do iy = 1, n2/2
         w2_cplx(2*iy - 1) = -cw(ii, 2*iy)
         w2_cplx(2*iy) = cw(ii, 2*iy - 1)
        end do
        call fftw_execute_dft_c2r(iplan2, w2_cplx, w2_re)
        do iy = 1, n2
         w(ii, iy, iz) = w(ii, iy, iz) + sfhy(iy)*w2_re(iy)
        end do
       end do
      end do
     end do
    end if
    if (allocated(cw)) deallocate (cw)
   case (3)
    allocate (cw(n1, n3))
    if (is < 0) then
     sc = 1./real(n3, dp)
     do iy = 1, n2
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do ii = i1, i2
        do iz = 1, n3
         w3_re(iz) = cfhz(iz)*w(ii, iy, iz)
        end do
        call fftw_execute_dft_r2c(plan3, w3_re, w3_cplx)
        do iz = 1, n3
         cw(ii, iz) = sc*w3_cplx(iz)
         w3_re(iz) = sfhz(iz)*w(ii, iy, iz)
        end do
        call fftw_execute_dft_r2c(plan3, w3_re, w3_cplx)
        do iz = 1, n3/2
         cw(ii, 2*iz - 1) = cw(ii, 2*iz - 1) + sc*w3_cplx(2*iz)
         cw(ii, 2*iz) = cw(ii, 2*iz) - sc*w3_cplx(2*iz - 1)
        end do
       end do
      end do
      !=========== reordering as 2D fft
      do iz = 1, n3/2
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = i2 - 1
        wrr = cw(i1, 2*iz - 1)
        wri = cw(i1, 2*iz)
        wir = cw(i2, 2*iz - 1)
        wii = cw(i2, 2*iz)
        w(i1, iy, iz) = wrr - wii
        w(i2, iy, iz) = wri + wir
        w(i1, iy, n3 + 1 - iz) = wrr + wii
        w(i2, iy, n3 + 1 - iz) = wir - wri
       end do
      end do
     end do
    else
     n2_tr = n3/2 + n3/4
     ! First reorders
     do iy = 1, n2
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do iz = 1, n3/2
        cw(i1, 2*iz - 1) = 0.5*(w(i1, iy, iz) + w(i1, iy, n3 + 1 - iz)) !wrr
        cw(i1, 2*iz) = 0.5*(w(i2, iy, iz) - w(i2, iy, n3 + 1 - iz)) !wri
        cw(i2, 2*iz - 1) = 0.5*(w(i2, iy, iz) + w(i2, iy, n3 + 1 - iz)) !wir
        cw(i2, 2*iz) = 0.5*(w(i1, iy, n3 + 1 - iz) - w(i1, iy, iz)) !wii
       end do
      end do
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do ii = i1, i2
        cw(ii, n2_tr + 1:n3) = 0.0
        w3_cplx(n3 + 1:n3 + 2) = 0.0
        do iz = 1, n3
         w3_cplx(iz) = cw(ii, iz)
        end do
        call fftw_execute_dft_c2r(iplan3, w3_cplx, w3_re)
        do iz = 1, n3
         w(ii, iy, iz) = cfhz(iz)*w3_re(iz)
        end do
        w3_cplx(n3 + 1:n3 + 2) = 0.0
        do iz = 1, n3/2
         w3_cplx(2*iz - 1) = -cw(ii, 2*iz)
         w3_cplx(2*iz) = cw(ii, 2*iz - 1)
        end do
        call fftw_execute_dft_c2r(iplan3, w3_cplx, w3_re)
        do iz = 1, n3
         w(ii, iy, iz) = w(ii, iy, iz) + sfhz(iz)*w3_re(iz)
        end do
       end do
      end do
     end do
    end if
    if (allocated(cw)) deallocate (cw)
   end select
  end subroutine
  !====================
  subroutine ftw1d(w, n1, n2, n3, is, dir)
   !! WARNING: Still to be checked
   real(dp), intent(inout) :: w(:, :, :)

   integer, intent(in) :: n1, n2, n3, is, dir
   integer :: ix, iy, iz, n1_tr, n2_tr
   integer :: i1, i2, j1, j2
   real(dp) :: sc, wrr, wir, wri, wii

   !!$PRAGMA C( DFFTW_EXECUTE )

   select case (dir)
   case (1)
    if (is < 0) then
     sc = 1./real(n1, dp)
     do iz = 1, n3
      do iy = 1, n2
       w1_re(1:n1) = w(1:n1, iy, iz)
       call fftw_execute_dft_r2c(plan1, w1_re, w1_cplx)
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = 2*ix - 1
        w(i1, iy, iz) = sc*real(w1_cplx(ix))
        w(i2, iy, iz) = sc*imag(w1_cplx(ix))
       end do
      end do
     end do
    else
     n1_tr = n1
     do iz = 1, n3
      do iy = 1, n2
       w1_re(n1 + 1:n1 + 2) = 0.0
       do ix = 1, n1_tr/2
        i2 = 2*ix
        i1 = 2*ix - 1
        w1_cplx(ix) = cmplx(w(i1, iy, iz), w(i2, iy, iz))
       end do
       call fftw_execute_dft_c2r(iplan1, w1_cplx, w1_re)
       w(1:n1, iy, iz) = w1_re(1:n1)
      end do
     end do
    end if
   case (2)
    allocate (cw(n1, n2))
    if (is < 0) then
     sc = 1./real(n2, dp)
     do iz = 1, n3
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       w2_re(1:n2) = w(i1, 1:n2, iz)
       call fftw_execute_dft_r2c(plan2, w2_re, w2_cplx)
       do iy = 1, n2/2
        j2 = 2*iy
        j1 = 2*iy - 1
        cw(i1, j1) = sc*real(w2_cplx(iy))
        cw(i1, j2) = sc*imag(w2_cplx(iy))
       end do
       w2_re(1:n2) = w(i2, 1:n2, iz)
       call fftw_execute_dft_r2c(plan2, w2_re, w2_cplx)
       do iy = 1, n2
        cw(i2, iy) = sc*w2_cplx(iy)
       end do
      end do
      !=========== reordering as 2D fft
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       w(i1:i2, 1:n2, iz) = 0.
       w(i1:i2, 1, iz) = cw(i1:i2, 1)
       do iy = 2, n2/2
        wrr = cw(i1, 2*iy - 1)
        wri = cw(i1, 2*iy)
        wir = cw(i2, 2*iy - 1)
        wii = cw(i2, 2*iy)
        w(i1, iy, iz) = wrr - wii
        w(i2, iy, iz) = wri + wir
        w(i1, n2 + 2 - iy, iz) = wrr + wii
        w(i2, n2 + 2 - iy, iz) = wir - wri
       end do
      end do
     end do
    else
     n2_tr = n2/2
     do iz = 1, n3
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do iy = 1, n2
        cw(i1:i2, iy) = w(i1:i2, iy, iz)
       end do
       w2_re(1:n2 + 2) = 0.0
       w2_cplx(1) = cw(i1, 1)
       do iy = 2, n2_tr
        w2_cplx(2*iy - 1) = 0.5*(cw(i1, iy) + cw(i1, n2 + 2 - iy))
        w2_cplx(2*iy) = 0.5*(cw(i2, iy) - cw(i2, n2 + 2 - iy))
       end do
       call fftw_execute_dft_c2r(iplan2, w2_cplx, w2_re)
       do iy = 1, n2
        w(i1, iy, iz) = w2_re(iy)
       end do
       w2_re(1:n2 + 2) = 0.0
       w2_cplx(1) = cw(i2, 1)
       do iy = 2, n2_tr
        w2_cplx(2*iy - 1) = 0.5*(cw(i2, iy) + cw(i2, n2 + 2 - iy))
        w2_cplx(2*iy) = 0.5*(cw(i1, n2 + 2 - iy) - cw(i1, iy))
       end do
       call fftw_execute_dft_c2r(iplan2, w2_cplx, w2_re)
       do iy = 1, n2
        w(i2, iy, iz) = w2_re(iy)
       end do
      end do
     end do
    end if
    if (allocated(cw)) deallocate (cw)
   case (3)
    allocate (cw(n1, n3))
    if (is < 0) then
     sc = 1./real(n3, dp)
     do iy = 1, n2
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       do iz = 1, n3
        w3_re(iz) = w(i1, iy, iz)
       end do
       call fftw_execute_dft_r2c(plan3, w3_re, w3_cplx)
       do iz = 1, n3
        cw(i1, iz) = sc*w3_cplx(iz)
        w3_re(iz) = w(i2, iy, iz)
       end do
       call fftw_execute_dft_r2c(plan3, w3_re, w3_cplx)
       do iz = 1, n3
        cw(i2, iz) = sc*w3_cplx(iz)
       end do
      end do
      !=========== reordering as 2D fft
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       w(i1:i2, iy, 1) = cw(i1:i2, 1)
      end do
      do iz = 2, n3/2
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = i2 - 1
        wrr = cw(i1, 2*iz - 1)
        wri = cw(i1, 2*iz)
        wir = cw(i2, 2*iz - 1)
        wii = cw(i2, 2*iz)
        w(i1, iy, iz) = wrr - wii
        w(i2, iy, iz) = wri + wir
        w(i1, iy, n3 + 2 - iz) = wrr + wii
        w(i2, iy, n3 + 2 - iz) = wir - wri
       end do
      end do
     end do
    else
     n2_tr = n3/2
     do iy = 1, n2
      do iz = 1, n3
       do ix = 1, n1/2
        i2 = 2*ix
        i1 = i2 - 1
        cw(i1:i2, iz) = w(i1:i2, iy, iz)
       end do
      end do
      do ix = 1, n1/2
       i2 = 2*ix
       i1 = i2 - 1
       w3_re(1:n3 + 2) = 0.0
       w3_cplx(1) = cw(i1, 1)
       do iz = 2, n2_tr
        w3_cplx(2*iz - 1) = 0.5*(cw(i1, iz) + cw(i1, n3 + 2 - iz)) !wrr
        w3_cplx(2*iz) = 0.5*(cw(i2, iz) - cw(i2, n3 + 2 - iz)) !wri
       end do
       call fftw_execute_dft_c2r(iplan3, w3_cplx, w3_re)
       do iz = 1, n3
        w(i1, iy, iz) = w3_re(iz)
       end do
       w3_re(1:n3 + 2) = 0.0
       w3_cplx(1) = cw(i2, 1)
       do iz = 2, n2_tr
        w3_cplx(2*iz - 1) = 0.5*(cw(i2, iz) + cw(i2, n3 + 2 - iz)) !wir
        w3_cplx(2*iz) = 0.5*(cw(i1, n3 + 2 - iz) - cw(i1, iz)) !wii
       end do
       call fftw_execute_dft_c2r(iplan3, w3_cplx, w3_re)
       do iz = 1, n3
        w(i2, iy, iz) = w3_re(iz)
       end do
      end do
     end do
    end if
    if (allocated(cw)) deallocate (cw)
   end select
  end subroutine

  !==========================
  subroutine ftw1d_sc(w, n1, n2, n3, is, dir, sym)
   real(dp), intent(inout) :: w(:, :, :)

   integer, intent(in) :: n1, n2, n3, is, dir, sym
   integer :: ix, iy, iz
   real(dp) :: sc
   integer :: ndb
   integer(C_INTPTR_T) :: kind

   !!$PRAGMA C( DFFTW_EXECUTE )
   kind = determine_kind(sym)
   select case (dir)
   case (1)
    in1_1d(:) = zero_dp
    ndb = logical_dimension(kind, n1 + 1)
    if (is < 0) then !grid to Fourier space
     sc = 1./real(ndb, dp)
     if (kind == FFTW_RODFT00) then !sin
      do iz = 1, n3
       do iy = 1, n2
        in1_1d(1:n1) = w(1:n1, iy, iz)
        call fftw_execute_r2r(plan1_s, in1_1d, in1_1d)
        w(1:n1, iy, iz) = sc*in1_1d(1:n1)
       end do
      end do
     else if (kind == FFTW_REDFT00) then
      do iz = 1, n3 !cos
       do iy = 1, n2
        in1_1d(1:n1) = w(1:n1, iy, iz)
        call fftw_execute_r2r(plan1_c, in1_1d, in1_1d)
        w(1:n1, iy, iz) = sc*in1_1d(1:n1)
       end do
      end do
     end if
    else
     if (kind == FFTW_RODFT00) then
      do iz = 1, n3
       do iy = 1, n2
        in1_1d(:) = zero_dp
        in1_1d(1:n1) = w(1:n1, iy, iz)
        call fftw_execute_r2r(plan1_s, in1_1d, in1_1d)
        w(1:n1, iy, iz) = in1_1d(1:n1)
       end do
      end do
     else if (kind == FFTW_REDFT00) then
      do iz = 1, n3
       do iy = 1, n2
        in1_1d(:) = zero_dp
        in1_1d(1:n1) = w(1:n1, iy, iz)
        call fftw_execute_r2r(plan1_c, in1_1d, in1_1d)
        w(1:n1, iy, iz) = in1_1d(1:n1)
       end do
      end do
     end if
    end if
   case (2)
    in2_1d(:) = zero_dp
    ndb = logical_dimension(kind, n2)
    if (is < 0) then
     sc = 1./real(ndb, dp)
     do iz = 1, n3
      do ix = 1, n1
       in2_1d(1:n2) = w(ix, 1:n2, iz)
       call fftw_execute_r2r(plan2_s, in2_1d, in2_1d)
       w(ix, 1:n2, iz) = sc*in2_1d(1:n2)
      end do
     end do
    else
     do iz = 1, n3
      do ix = 1, n1
       in2_1d(:) = zero_dp
       in2_1d(1:n2) = w(ix, 1:n2, iz)
       call fftw_execute_r2r(plan2_s, in2_1d, in2_1d)
       w(ix, 1:n2, iz) = in2_1d(1:n2)
      end do
     end do
    end if
   case (3)
    in3_1d(:) = zero_dp
    ndb = logical_dimension(kind, n3)
    if (is < 0) then
     sc = 1./real(ndb, dp)
     do iy = 1, n2
      do ix = 1, n1
       in3_1d(:) = zero_dp
       in3_1d(1:n3) = w(ix, iy, 1:n3)
       call fftw_execute_r2r(plan3_s, in3_1d, in3_1d)
       w(ix, iy, 1:n3) = sc*in3_1d(1:n3)
      end do
     end do
    else
     do iy = 1, n2
      do ix = 1, n1
       in3_1d(1:n3) = w(ix, iy, 1:n3)
       call fftw_execute_r2r(plan3_s, in3_1d, in3_1d)
       w(ix, iy, 1:n3) = in3_1d(1:n3)
      end do
     end do
    end if
   end select
  end subroutine

 end module

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

 module pic_dump

  use array_alloc
  use code_util
  use common_param
  use grid_param
  use parallel
  use, intrinsic :: iso_c_binding

  implicit none

  real (dp), allocatable, private :: send_buff(:), recv_buff(:)

  interface restart
   module procedure restart_new
   module procedure restart_old
  end interface
  
  interface particles_dump
   module procedure particles_dump_new
   module procedure particles_dump_old
  end interface
  
  interface particles_restart
   module procedure particles_restart_new
   module procedure particles_restart_old
  end interface
    
  interface dump_data
   module procedure dump_data_new
   module procedure dump_data_old
  end interface

  contains

   subroutine particles_dump_properties( spec_in, spec_aux_in, lenw, max_npt_size_in, array_in)
    type(species_new), intent(in), dimension(:) :: spec_in
    type(species_aux), intent(in), dimension(:) :: spec_aux_in
    integer, intent(inout), dimension(:) :: lenw
    integer, intent(in) :: max_npt_size_in
    real(dp), allocatable, dimension(:), intent(inout) :: array_in
    real(dp), allocatable, dimension(:) :: properties_spec, properties_spec_aux
    integer :: ic, kk, size_prop, size_prop_aux

    lenw(:) = 0
    if(max_npt_size_in <= 0) return

    ! Here we assume that for every species properties_size is the same
    ic = 1
    size_prop = spec_in(ic)%properties_array_size()
    size_prop_aux = spec_aux_in(ic)%properties_array_size()
    lenw(1:npe) = lenw(1:npe) + (size_prop + size_prop_aux)*nsp

    call array_realloc_1d( array_in, lenw(mype + 1) )
    kk = 0
    do ic = 1, nsp
    
     call spec_in(ic)%dump_properties_to_array(properties_spec)
     array_in((kk + 1):(kk + size_prop)) = properties_spec
     kk = kk + size_prop
     
     call spec_aux_in(ic)%dump_properties_to_array(properties_spec_aux)
     array_in((kk + 1):(kk + size_prop_aux)) = properties_spec_aux
     kk = kk + size_prop_aux

    end do

   end subroutine

   subroutine particles_dump_new( spec_in, spec_aux_in, lenw, ip_loc_in, max_npt_size_in, array_in)
   type(species_new), intent(in), dimension(:) :: spec_in
   type(species_aux), intent(in), dimension(:) :: spec_aux_in
   integer, intent(inout), dimension(:) :: lenw
   integer, intent(in), dimension(:) :: ip_loc_in
   integer, intent(inout) :: max_npt_size_in
   real(dp), allocatable, dimension(:), intent(inout) :: array_in
   real(dp), allocatable, dimension(:) :: temp_buff
   integer :: size_spec, size_spec_aux, tot, ic, np, kk

   lenw(:) = 0
   if(max_npt_size_in <= 0) return
   
   ! Here we assume that for every species spec%total_size is the same
   ic = 1
   size_spec = spec_in(ic)%total_size()
   size_spec_aux = spec_aux_in(ic)%total_size()
   lenw(1:npe) = lenw(1:npe) + (size_spec + size_spec_aux)* ip_loc_in(1:npe)
   
   call array_realloc_1d( array_in, lenw(mype + 1) )

   kk = 0
   do ic = 1, nsp
    if ( .not. spec_in(ic)%empty ) then
     np = spec_in(ic)%how_many()
     tot = np*spec_in(ic)%total_size()
     call spec_in(ic)%flatten_into(temp_buff)
     array_in( (kk + 1): (kk + tot) ) = temp_buff(1:tot)
     kk = kk + tot
     
     tot = np*spec_aux_in(ic)%total_size()
     call spec_aux_in(ic)%flatten_into(temp_buff)
     array_in( (kk + 1): (kk + tot) ) = temp_buff(1:tot)
     kk = kk + tot
    end if
   end do

   max_npt_size_in = maxval(lenw(1:npe))
   
  end subroutine

  subroutine particles_dump_old( spec_in, lenw, ip_loc_in, max_npt_size_in, array_in )
   type(species), intent(in), dimension(:) :: spec_in
   integer, intent(inout), dimension(:) :: lenw
   integer, intent(in), dimension(:) :: ip_loc_in
   integer, intent(inout) :: max_npt_size_in
   real(dp), allocatable, dimension(:), intent(inout) :: array_in
   integer :: ic, np, i, j, kk, ndv

   ndv = nd2 + 1
   if (max_npt_size_in <= 0) return
   lenw(1:npe) = ndv*ip_loc_in(1:npe)
   max_npt_size_in = maxval(lenw(1:npe))
   kk = 0
   call array_realloc_1d( array_in, lenw(mype + 1) )
   do ic = 1, nsp
    np = loc_npart(imody, imodz, imodx, ic)
    if (np>0) then
     do j = 1, ndv
      do i = 1, np
       kk = kk + 1
       array_in(kk) = spec_in(ic)%part(i, j)
      end do
     end do
    end if
   end do

  end subroutine

  subroutine particles_restart_properties( spec_in, spec_aux_in, array_in )
   type(species_new), intent(inout), dimension(:) :: spec_in
   type(species_aux), intent(inout), dimension(:) :: spec_aux_in
   real(dp), dimension(:), intent(in) :: array_in
   integer :: ic, kk, size_prop, size_prop_aux

   ic = 1
   size_prop = spec_in(ic)%properties_array_size()
   size_prop_aux = spec_aux_in(ic)%properties_array_size()

   kk = 0

   do ic = 1, nsp

    call spec_in(ic)%read_properties_from_array(array_in(kk+1:kk+size_prop))
    kk = kk + size_prop

    call spec_aux_in(ic)%read_properties_from_array(array_in(kk+1:kk+size_prop_aux))
    kk = kk + size_prop_aux

   end do

  end subroutine

  subroutine particles_restart_new( spec_in, spec_aux_in, local_particles, array_in )
   type(species_new), intent(inout), dimension(:) :: spec_in
   type(species_aux), intent(inout), dimension(:) :: spec_aux_in
   integer, allocatable, dimension(:, :, :, :), intent(in) :: local_particles
   real(dp), dimension(:), intent(in) :: array_in
   integer :: ic, np, kk, spec_size, aux_size
   integer :: tot, tot_aux

   ic = 1
   spec_size = spec_in(ic)%total_size()
   aux_size = spec_aux_in(ic)%total_size()
   kk = 0

   do ic = 1, nsp
    np = local_particles( imody, imodz, imodx, ic )
    tot = np*spec_size
    tot_aux = np*aux_size

    call spec_in(ic)%redistribute(array_in(kk+1:kk+tot), np, spec_in(ic)%pick_properties(), aux_in=.false.)
    kk = kk + tot
    call spec_aux_in(ic)%redistribute(array_in(kk+1:kk+tot_aux), np, spec_in(ic)%pick_properties(), aux_in=.true.)
    kk = kk + tot_aux

   end do
  end subroutine

  subroutine particles_restart_old()
  end subroutine

  subroutine dump_data_new(it_loc, tloc, spec_in, spec_aux_in)
   type(species_new), dimension(:), intent(in) :: spec_in
   type(species_aux), dimension(:), intent(in) :: spec_aux_in
   integer, intent (in) :: it_loc
   real (dp), intent (in) :: tloc
   character (9) :: fname_comm
   character (14) :: fname_comm_mype
   character (9) :: fname_yz
   character (9) :: fname_ebf
   character (9) :: fname_env
   character (9) :: fname_fl
   character (9) :: fname_part
   character (9) :: fname_names
   character (8) :: fname_prop
   !=== BE CAREFUL: FILE NAMES HAVE BEEN INITIALIZED TO ONLY ALLOW A MAXIMUM
   ! 99 CORES ALONG Z. IF MORE ARE NEEDED, IT IS NECESSARY TO CHANGE FROM
   ! CHARACTER(11) TO CHARACTER(12) (OR MORE) ===
   character (14) :: name_prop
   character (30) :: fnamel_prop
   character (9) :: name_file
   character (25) :: fnamel_name
   character (15) :: part_file
   character (31) :: fnamel_part
   character (15) :: ebf_file
   character (31) :: fnamel_ebf
   character (15) :: env_file
   character (31) :: fnamel_env
   character (15) :: fl_file
   character (31) :: fnamel_fl
   character (31) :: fnamel_yz
   character (11) :: foldername
   integer (offset_kind) :: disp_col, disp
   integer :: max_npt_size, disp_coord
   integer :: ic, lun, i, j, k, kk, lenbuff
   integer :: nxf_loc, nyf_loc, nzf_loc, ndv
   integer :: npt_arr(npe, nsp), ip_loc(npe)
   integer :: loc_grid_size(npe), loc2d_grid_size(npe), lenw(npe)
   integer :: grid_size_max, grid2d_size_max
   integer :: env_cp, env1_cp, fl_cp, ebf_cp
   real (dp) :: rdata(10)
   integer :: ndata(10)
   !==============
   write (fname_comm, '(a9)') 'Comm-data'
   write (fname_yz, '(a9)') 'Dist-wgyz'
   write (fname_ebf, '(a9)') 'EB-fields'
   write (fname_env, '(a9)') 'ENVfields'
   write (fname_fl, '(a9)') 'FL-fields'
   write (fname_part, '(a9)') 'Particles'
   write (fname_names, '(a9)') 'Specnames'
   write (fname_prop, '(a8)') 'SpecProp'
   write (foldername, '(a11)') 'dumpRestart'
   !================field array sizes
   nxf_loc = size(ebf, 1)
   nyf_loc = size(ebf, 2)
   nzf_loc = size(ebf, 3)
   ebf_cp = size(ebf, 4)

   !======== Write files name ===========
   write (fname_comm_mype, '(a9, i5.5)') fname_comm, mype
   write (name_prop, '(a8,2i3.3)') fname_prop, imodx, imodz
   fnamel_prop = foldername //'/' // name_prop // '.bin' 
   write (name_file, '(a9)') fname_names
   fnamel_name = foldername //'/' // name_file // '.bin' 
   write (part_file, '(a9,2i3.3)') fname_part, imodx, imodz
   fnamel_part = foldername //'/' // part_file // '.bin' 
   write (ebf_file, '(a9,2i3.3)') fname_ebf, imodx, imodz
   fnamel_ebf = foldername //'/' // ebf_file // '.bin'
   write (env_file, '(a9,2i3.3)') fname_env, imodx, imodz
   fnamel_env = foldername //'/' // env_file // '.bin'
   write (fl_file, '(a9,2i3.3)') fname_fl, imodx, imodz
   fnamel_fl = foldername //'/' // fl_file // '.bin'
   fnamel_yz = foldername //'/' // fname_yz // '.bin'
   !======== End write files name ======
   loc_grid_size(mype+1) = nxf_loc*nyf_loc*nzf_loc !allowing for different grid sizes among mpi_tasks
   loc2d_grid_size(mype+1) = nyf_loc*nzf_loc
   lenbuff = ebf_cp
   if (envelope) then
    env1_cp = 0
    env_cp = size(env, 4)
    if (Two_color) env1_cp = env_cp
    lenbuff = max(lenbuff, env_cp+env1_cp)
   end if
   grid2d_size_max = 0
   if (hybrid) then
    fl_cp = size(up, 4)
    lenbuff = max(lenbuff, 2*fl_cp)
    kk = loc2d_grid_size(mype+1)
    call intvec_distribute(kk, loc2d_grid_size, npe)
    grid2d_size_max = maxval(loc2d_grid_size(1:npe))
   end if
   !===============================
   kk = loc_grid_size(mype+1)
   call intvec_distribute(kk, loc_grid_size, npe)
   grid_size_max = maxval(loc_grid_size(1:npe))
   lenbuff = lenbuff*grid_size_max + grid2d_size_max
   !===============================
   ndv = nd2+1
   do i = 1, nsp
    kk = loc_npart(imody, imodz, imodx, i)
    call intvec_distribute(kk, ip_loc, npe)
    npt_arr(1:npe, i) = ip_loc(1:npe)
   end do
   do i = 1, npe
    ip_loc(i) = sum(npt_arr(i, 1:nsp))
   end do
   max_npt_size = ndv*maxval(ip_loc(1:npe))
   lenbuff = max(lenbuff, max_npt_size)
   !=========================
   ndata = 0
   rdata = 0.0
   !====================
   rdata(1) = tloc
   rdata(2) = j0_norm
   rdata(3) = ompe
   rdata(4) = targ_in
   rdata(5) = targ_end
   rdata(6) = lp_in(1)
   rdata(7) = xp0_out
   rdata(8) = xp1_out
   rdata(9) = x(1)

   ndata(1) = it_loc
   ndata(2) = nxf_loc
   ndata(3) = nyf_loc
   ndata(4) = nzf_loc
   ndata(5) = nptx_max
   ndata(6) = size(x)
   ndata(7) = nxf
   ndata(8) = nd2
   !==========================
   lun = 10
   open (lun, file=foldername //'/'//fname_comm_mype//'.bin', form='unformatted', &
     status='unknown')
   write (lun) rdata(1:10)
   write (lun) ndata(1:10)
   write (lun) nptx(1:nsp) !the index of particles inside the box
   write (lun) sptx_max(1:nsp) !the max index inside the target
   !=====================
   if (targ_end > xmax) then
    do i = 1, nsp
     do j = 1, nptx_max
      write (lun) xpt(j, i), wghpt(j, i)
     end do
    end do
    if (hybrid) then
     if (nxf > 0) then
      write (lun) fluid_x_profile(1:nxf)
     end if
    end if
   end if
   write (lun) npt_arr(1:npe, 1:nsp)
   write (lun) loc_npty(1:nsp)
   write (lun) loc_nptz(1:nsp)
   !==================
   close (lun)
   if (pe0) write (6, *) 'End write Common data'
   !===========================
   allocate (send_buff(lenbuff)) !to be used for all mpi_write()
   !=====================
   !=== PARTICLES DUMP SECTION ====

   if ( max_npt_size > 0 ) then
    call particles_dump_properties( spec_in, spec_aux_in, lenw, max_npt_size, send_buff)
    disp_col = 0
    disp_coord = imodz*npe_yloc + npe_zloc*npe_yloc*imodx
    if (mod(mype, npe_yloc)>0) disp_col = sum(lenw(disp_coord + 1:mype))
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(mype+1), disp_col, &
    fnamel_prop)

    call particles_dump( spec_in, spec_aux_in, lenw, ip_loc, max_npt_size, send_buff )
    if (pe0) then
     open( unit = lun, file=fnamel_name, status='unknown', form='unformatted')
     do ic = 1, nsp
      write (lun) spec_in(ic)%pick_name()
     end do
     close(lun)
    end if
    disp_col = 0
    disp_coord = imodz*npe_yloc + npe_zloc*npe_yloc*imodx
    if (mod(mype, npe_yloc)>0) disp_col = sum(lenw(disp_coord + 1:mype))
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(mype+1), disp_col, &
    fnamel_part)

    if (pe0) write (6, *) 'Particles data dumped'
   end if
   !=== END PARTICLES DUMP SECTION ===

   !=== ELECTROMAGNETIC FIELD DUMP SECTION ===
   lenw(1:npe) = ebf_cp*loc_grid_size(1:npe)
   kk = 0
   do ic = 1, ebf_cp
    do k = 1, nzf_loc
     do j = 1, nyf_loc
      do i = 1, nxf_loc
       kk = kk + 1
       send_buff(kk) = ebf(i, j, k, ic)
      end do
     end do
    end do
   end do

   disp = lenw(1 + mype)
   disp_col = imody*disp
   disp_col = 8*disp_col
   call mpi_write_col_dp(send_buff, lenw(1+mype), disp_col, &
   fnamel_ebf)
   if (pe0) write (6, *) 'Electromagnetic fields data dumped'
   !=== END ELECTROMAGNETIC FIELD DUMP SECTION ===

   !=== ENVELOPE FIELD DUMP SECTION ===
   if (envelope) then
    lenw(1:npe) = (env_cp+env1_cp)*loc_grid_size(1:npe)
    kk = 0
    do ic = 1, env_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = env(i, j, k, ic)
       end do
      end do
     end do
    end do
    if (Two_color) then
     do ic = 1, env1_cp
      do k = 1, nzf_loc
       do j = 1, nyf_loc
        do i = 1, nxf_loc
         kk = kk + 1
         send_buff(kk) = env1(i, j, k, ic)
        end do
       end do
      end do
     end do
    end if
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(mype+1), disp_col, &
    fnamel_env)
    if (pe0) write (6, *) 'Envelope field data dumped'
   end if
   !=== END ENVELOPE FIELD DUMP SECTION ===

   !=== FLUID DUMP SECTION ===
   if (hybrid) then
    lenw(1:npe) = 2*fl_cp*loc_grid_size(1:npe) + loc2d_grid_size(1:npe)
    kk = 0
    do k = 1, nzf_loc
     do j = 1, nyf_loc
      kk = kk + 1
      send_buff(kk) = fluid_yz_profile(j, k)
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = up(i, j, k, ic)
       end do
      end do
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = up0(i, j, k, ic)
       end do
      end do
     end do
    end do
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(1+mype), disp_col, &
    fnamel_fl)
    if (pe0) write (6, *) 'Fluid density and momentum data dumped'
   end if

   !=== END FLUID DUMP SECTION ===
   !============== write (y,z,wghyz) initial part distribution
   if (part) then
    kk = 0
    do ic = 1, nsp
     if (loc_npty(ic)>0) then
      do i = 1, loc_npty(ic)
       kk = kk + 1
       send_buff(kk) = loc_ypt(i, ic)
      end do
     end if
    end do
    do ic = 1, nsp
     if (loc_nptz(ic)>0) then
      do j = 1, loc_nptz(ic)
       kk = kk + 1
       send_buff(kk) = loc_zpt(j, ic)
      end do
     end if
    end do
    !===============================
    do ic = 1, nsp
     if (loc_nptz(ic)>0) then
      do j = 1, loc_nptz(ic)
       if (loc_npty(ic)>0) then
        do i = 1, loc_npty(ic)
         kk = kk + 1
         send_buff(kk) = loc_wghyz(i, j, ic)
        end do
       end if
      end do
     end if
    end do
    call intvec_distribute(kk, lenw, npe)
    disp = 0
    if (mype>0) disp = sum(lenw(1:mype))
    disp = 8*disp
    call mpi_write_dp(send_buff, lenw(mype+1), disp, fnamel_yz)
    if (pe0) write (6, *) &
      'Incoming plasma target transverse distribution data dumped'
   end if
   deallocate (send_buff)
   !====================
   unix_time_last_dump = unix_time_now
   if (pe0) write (6, *) 'END TOTAL DUMP WRITE'
  end subroutine

  subroutine dump_data_old(it_loc, tloc, spec_in, spec_aux_in)
   type(species), dimension(:), intent(in) :: spec_in
   real(dp), allocatable, dimension(:, :) ::spec_aux_in
   integer, intent (in) :: it_loc
   real (dp), intent (in) :: tloc
   character (9) :: fname = '         '
   character (9) :: fname_yz = '         '
   character (9) :: fname_ebf = '         '
   character (9) :: fname_env = '         '
   character (9) :: fname_fl = '         '
   character (9) :: fname_part = '         '
   !=== BE CAREFUL: FILE NAMES HAVE BEEN INITIALIZED TO ONLY ALLOW A MAXIMUM
   ! 99 CORES ALONG Z. IF MORE ARE NEEDED, IT IS NECESSARY TO CHANGE FROM
   ! CHARACTER(11) TO CHARACTER(12) (OR MORE) ===
   character (11) :: part_file = '           '
   character (27) :: fnamel_part = '                           '
   character (11) :: ebf_file = '           '
   character (27) :: fnamel_ebf = '                           '
   character (11) :: env_file = '           '
   character (27) :: fnamel_env = '                           '
   character (11) :: fl_file = '           '
   character (27) :: fnamel_fl = '                           '
   character (27) :: fnamel_yz = '                           '
   character (11) :: foldername = '           '
   integer (offset_kind) :: disp_col, disp
   integer :: max_npt_size
   integer :: ic, lun, i, j, k, kk, ipe, lenbuff
   integer :: nxf_loc, nyf_loc, nzf_loc, ndv
   integer :: npt_arr(npe, nsp), ip_loc(npe)
   integer :: loc_grid_size(npe), loc2d_grid_size(npe), lenw(npe)
   integer :: grid_size_max, grid2d_size_max
   integer :: env_cp, env1_cp, fl_cp, ebf_cp
   real(dp) :: rdata(10)
   integer :: ndata(10)
   integer :: dist_npy(npe_yloc, nsp), dist_npz(npe_zloc, nsp)
   logical :: sd
   !==============
   write (fname, '(a9)') 'Comm-data'
   write (fname_yz, '(a9)') 'Dist-wgyz'
   write (fname_ebf, '(a9)') 'EB-fields'
   write (fname_env, '(a9)') 'ENVfields'
   write (fname_fl, '(a9)') 'FL-fields'
   write (fname_part, '(a9)') 'Particles'
   write (foldername, '(a11)') 'dumpRestart'
   !================field array sizes
   nxf_loc = size(ebf, 1)
   nyf_loc = size(ebf, 2)
   nzf_loc = size(ebf, 3)
   ebf_cp = size(ebf, 4)

   !======== Write files name ===========
   write (part_file, '(a9,i2.2)') fname_part, imodz
   fnamel_part = foldername //'/' // part_file // '.bin' 
   write (ebf_file, '(a9,i2.2)') fname_ebf, imodz
   fnamel_ebf = foldername //'/' // ebf_file // '.bin'
   write (env_file, '(a9,i2.2)') fname_env, imodz
   fnamel_env = foldername //'/' // env_file // '.bin'
   write (fl_file, '(a9,i2.2)') fname_fl, imodz
   fnamel_fl = foldername //'/' // fl_file // '.bin'
   fnamel_yz = foldername //'/' // fname_yz // '.bin'
   !======== End write files name ======
   loc_grid_size(mype+1) = nxf_loc*nyf_loc*nzf_loc !allowing for different grid sizes among mpi_tasks
   loc2d_grid_size(mype+1) = nyf_loc*nzf_loc
   lenbuff = ebf_cp
   if (envelope) then
    env1_cp = 0
    env_cp = size(env, 4)
    if (Two_color) env1_cp = env_cp
    lenbuff = max(lenbuff, env_cp + env1_cp)
   end if
   grid2d_size_max = 0
   if (hybrid) then
    fl_cp = size(up, 4)
    lenbuff = max(lenbuff, 2*fl_cp)
    kk = loc2d_grid_size(mype + 1)
    call intvec_distribute(kk, loc2d_grid_size, npe)
    grid2d_size_max = maxval(loc2d_grid_size(1:npe))
   end if
   !===============================
   kk = loc_grid_size(mype + 1)
   call intvec_distribute(kk, loc_grid_size, npe)
   grid_size_max = maxval(loc_grid_size(1:npe))
   lenbuff = lenbuff*grid_size_max + grid2d_size_max
   !===============================
   ndv = nd2 + 1
   do i = 1, nsp
    kk = loc_npart(imody, imodz, imodx, i)
    call intvec_distribute(kk, ip_loc, npe)
    npt_arr(1:npe, i) = ip_loc(1:npe)
   end do
   do i = 1, npe
    ip_loc(i) = sum(npt_arr(i, 1:nsp))
   end do
   max_npt_size = ndv*maxval(ip_loc(1:npe))
   lenbuff = max(lenbuff, max_npt_size)
   !===============================
   dist_npy(:, :) = 0
   dist_npz(:, :) = 0
   dist_npy(imody + 1, 1:nsp) = loc_npty(1:nsp)
   dist_npz(imodz + 1, 1:nsp) = loc_nptz(1:nsp)
   !===============================
   if (.not. pe0y) then
    sd = .true.
    call exchange_rdata_int(loc_npty, sd, nsp, pe_min_y, 1, 100 + imody)
   else
    sd = .false.
    do ipe = 1, npe_yloc - 1
     call exchange_rdata_int(loc_npty, sd, nsp, ipe, 1, 100 + ipe)
     dist_npy(ipe + 1, 1:nsp) = loc_npty(1:nsp)
    end do
   end if
   if (.not. pe0z) then
    sd = .true.
    call exchange_rdata_int(loc_nptz, sd, nsp, pe_min_z, 2, 100 + imodz)
   else
    sd = .false.
    do ipe = 1, npe_zloc - 1
     call exchange_rdata_int(loc_nptz, sd, nsp, ipe, 2, 100 + ipe)
     dist_npz(ipe + 1, 1:nsp) = loc_nptz(1:nsp)
    end do
   end if
   !=========================
   ndata = 0
   rdata = 0.0
   !====================
   rdata(1) = tloc
   rdata(2) = j0_norm
   rdata(3) = ompe
   rdata(4) = targ_in
   rdata(5) = targ_end
   rdata(6) = lp_in(1)
   rdata(7) = xp0_out
   rdata(8) = xp1_out
   rdata(9) = x(1)

   ndata(1) = it_loc
   ndata(2) = nxf_loc
   ndata(3) = nyf_loc
   ndata(4) = nzf_loc
   ndata(5) = nptx_max
   ndata(6) = size(x)
   ndata(7) = nxf
   ndata(8) = nd2
   !==========================
   !==============
   lun = 10
   if (pe0) then
    open (lun, file=foldername //'/'//fname//'.bin', form='unformatted', &
      status='unknown')
    write (lun) rdata(1:10)
    write (lun) ndata(1:10)
    write (lun) nptx(1:nsp) !the index of particles inside the box
    write (lun) sptx_max(1:nsp) !the max index inside the target
    !=====================
    if (targ_end > xmax) then
     do i = 1, nsp
      do j = 1, nptx_max
       write (lun) xpt(j, i), wghpt(j, i)
      end do
     end do
     if (hybrid) then
      if (nxf > 0) then
       write (lun) fluid_x_profile(1:nxf)
      end if
     end if
    end if
    write (lun) npt_arr(1:npe, 1:nsp)
    write (lun) dist_npy(1:npe_yloc, 1:nsp)
    write (lun) dist_npz(1:npe_zloc, 1:nsp)
    !==================
    close (lun)
   end if !end pe0 write on fname
   if (pe0) write (6, *) 'End write Common data'
   !===========================
   allocate (send_buff(lenbuff)) !to be used for all mpi_write()
   !=====================
   !=== PARTICLES DUMP SECTION ====

   call particles_dump( spec_in, lenw, ip_loc, max_npt_size, send_buff )

   if ( max_npt_size > 0 ) then
    if (mod(mype,npe_yloc)>0) disp_col = sum(lenw(imodz*npe_yloc+1:mype))
    disp_col = 8*disp_col
    if ( disp_col > 0 ) then
     call mpi_write_col_dp(send_buff, lenw(mype+1), disp_col, &
     fnamel_part)
    end if
    if (pe0) write (6, *) 'Particles data dumped'
   end if
   !=== END PARTICLES DUMP SECTION ===

   !=== ELECTROMAGNETIC FIELD DUMP SECTION ===
   lenw(1:npe) = ebf_cp*loc_grid_size(1:npe)
   kk = 0
   do ic = 1, ebf_cp
    do k = 1, nzf_loc
     do j = 1, nyf_loc
      do i = 1, nxf_loc
       kk = kk + 1
       send_buff(kk) = ebf(i, j, k, ic)
      end do
     end do
    end do
   end do

   disp = lenw(1+mype)
   disp_col = imody*disp
   disp_col = 8*disp_col
   call mpi_write_col_dp(send_buff, lenw(1+mype), disp_col, &
   fnamel_ebf)
   if (pe0) write (6, *) 'Electromagnetic fields data dumped'
   !=== END ELECTROMAGNETIC FIELD DUMP SECTION ===

   !=== ENVELOPE FIELD DUMP SECTION ===
   if (envelope) then
    lenw(1:npe) = (env_cp+env1_cp)*loc_grid_size(1:npe)
    kk = 0
    do ic = 1, env_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = env(i, j, k, ic)
       end do
      end do
     end do
    end do
    if (Two_color) then
     do ic = 1, env1_cp
      do k = 1, nzf_loc
       do j = 1, nyf_loc
        do i = 1, nxf_loc
         kk = kk + 1
         send_buff(kk) = env1(i, j, k, ic)
        end do
       end do
      end do
     end do
    end if
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(mype+1), disp_col, &
    fnamel_env)
    if (pe0) write (6, *) 'Envelope field data dumped'
   end if
   !=== END ENVELOPE FIELD DUMP SECTION ===

   !=== FLUID DUMP SECTION ===
   if (hybrid) then
    lenw(1:npe) = 2*fl_cp*loc_grid_size(1:npe) + loc2d_grid_size(1:npe)
    kk = 0
    do k = 1, nzf_loc
     do j = 1, nyf_loc
      kk = kk + 1
      send_buff(kk) = fluid_yz_profile(j, k)
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = up(i, j, k, ic)
       end do
      end do
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, nzf_loc
      do j = 1, nyf_loc
       do i = 1, nxf_loc
        kk = kk + 1
        send_buff(kk) = up0(i, j, k, ic)
       end do
      end do
     end do
    end do
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_write_col_dp(send_buff, lenw(1+mype), disp_col, &
    fnamel_fl)
    if (pe0) write (6, *) 'Fluid density and momentum data dumped'
   end if

   !=== END FLUID DUMP SECTION ===
   !============== write (y,z,wghyz) initial part distribution
   if (part) then
    kk = 0
    do ic = 1, nsp
     if (loc_npty(ic)>0) then
      do i = 1, loc_npty(ic)
       kk = kk + 1
       send_buff(kk) = loc_ypt(i, ic)
      end do
     end if
    end do
    do ic = 1, nsp
     if (loc_nptz(ic)>0) then
      do j = 1, loc_nptz(ic)
       kk = kk + 1
       send_buff(kk) = loc_zpt(j, ic)
      end do
     end if
    end do
    !===============================
    do ic = 1, nsp
     if (loc_nptz(ic)>0) then
      do j = 1, loc_nptz(ic)
       if (loc_npty(ic)>0) then
        do i = 1, loc_npty(ic)
         kk = kk + 1
         send_buff(kk) = loc_wghyz(i, j, ic)
        end do
       end if
      end do
     end if
    end do
    call intvec_distribute(kk, lenw, npe)
    disp = 0
    if (mype>0) disp = sum(lenw(1:mype))
    disp = 8*disp
    call mpi_write_dp(send_buff, lenw(mype+1), disp, fnamel_yz)
    if (pe0) write (6, *) &
      'Incoming plasma target transverse distribution data dumped'
   end if
   deallocate (send_buff)
   !====================
   unix_time_last_dump = unix_time_now
   if (pe0) write (6, *) 'END TOTAL DUMP WRITE'
  end subroutine
  !==============================================================
  ! RESTART SECTION
  !==============================================================
  subroutine restart_new(it_loc, tloc, spec_in, spec_aux_in)
   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (out) :: it_loc
   real (dp), intent (out) :: tloc
   character (9) :: fname_comm
   character (14) :: fname_comm_mype
   character (9) :: fname_yz
   character (9) :: fname_ebf
   character (9) :: fname_env
   character (9) :: fname_fl
   character (9) :: fname_part
   character (9) :: fname_names
   character (8) :: fname_prop
   character (14) :: name_prop
   character (30) :: fnamel_prop
   character (9) :: name_file
   character (25) :: fnamel_name
   character (15) :: part_file
   character (31) :: fnamel_part
   character (15) :: ebf_file
   character (31) :: fnamel_ebf
   character (15) :: env_file
   character (31) :: fnamel_env
   character (15) :: fl_file
   character (31) :: fnamel_fl
   character (31) :: fnamel_yz
   character (11) :: foldername
   character (100) :: name_buff
   integer (offset_kind) :: disp_col, disp
   integer :: max_npt_size, ipe, npt_arr(npe, nsp), disp_coord, init_part(nsp)
   integer :: ndv, ic, lun, i, j, k, kk, lenw(npe), lenbuff
   integer :: ip_loc(npe), loc_grid_size(npe), loc2d_grid_size(npe)
   integer :: grid_size_max, grid2d_size_max
   integer :: env_cp, env1_cp, fl_cp, ebf_cp
   integer :: ndata(10), nps_loc(4), n1_old
   integer :: n1_loc, n2_loc, n3_loc, nypt_max, nzpt_max
   integer :: size_spec, size_spec_aux, size_prop, size_prop_aux
   real (dp) :: rdata(10), x0_new
   logical, allocatable, dimension(:) :: mobilearray
   logical(c_bool) :: isempty

   !==============
   write (fname_comm, '(a9)') 'Comm-data'
   write (fname_ebf, '(a9)') 'EB-fields'
   write (fname_env, '(a9)') 'ENVfields'
   write (fname_fl, '(a9)') 'FL-fields'
   write (fname_part, '(a9)') 'Particles'
   write (fname_names, '(a9)') 'Specnames'
   write (fname_prop, '(a8)') 'SpecProp'
   write (foldername, '(a11)') 'dumpRestart'
   write (fname_yz, '(a9)') 'Dist-wgyz'
   !==============       Already defined data
   n1_loc = size(ebf, 1)
   n2_loc = size(ebf, 2)
   n3_loc = size(ebf, 3)
   ebf_cp = size(ebf, 4)
   !======== Write files name ===========
   write (fname_comm_mype, '(a9, i5.5)') fname_comm, mype
   write (name_prop, '(a8,2i3.3)') fname_prop, imodx, imodz
   fnamel_prop = foldername //'/' // name_prop // '.bin' 
   write (name_file, '(a9)') fname_names
   fnamel_name = foldername //'/' // name_file // '.bin' 
   write (part_file, '(a9,2i3.3)') fname_part, imodx, imodz
   fnamel_part = foldername //'/' // part_file // '.bin' 
   write (ebf_file, '(a9,2i3.3)') fname_ebf, imodx, imodz
   fnamel_ebf = foldername //'/' // ebf_file // '.bin'
   write (env_file, '(a9,2i3.3)') fname_env, imodx, imodz
   fnamel_env = foldername //'/' // env_file // '.bin'
   write (fl_file, '(a9,2i3.3)') fname_fl, imodx, imodz
   fnamel_fl = foldername //'/' // fl_file // '.bin'
   fnamel_yz = foldername //'/' // fname_yz // '.bin'
   !======== End write files name ======


   !======== Check if dumpRestart folder contains files =====
   call check_folder_empty(isempty, foldername)
   if( isempty ) then
    if (pe0) then
     write(6, *) 'dumpRestart folder does not contain any file. Exiting.'
    end if
    call call_barrier
    call End_parallel
    call exit(0)
   end if
   !=========================================================

   loc_grid_size(mype+1) = n1_loc*n2_loc*n3_loc
   loc2d_grid_size(mype+1) = n2_loc*n3_loc
   lenbuff = ebf_cp
   if (envelope) then
    env1_cp = 0
    env_cp = size(env, 4)
    if (Two_color) env1_cp = size(env1, 4)
    lenbuff = max(lenbuff, env_cp+env1_cp)
   end if
   grid2d_size_max = 0
   if (hybrid) then
    fl_cp = size(up, 4)
    lenbuff = max(lenbuff, 2*fl_cp)
    kk = loc2d_grid_size(mype+1)
    call intvec_distribute(kk, loc2d_grid_size, npe)
    grid2d_size_max = maxval(loc2d_grid_size(1:npe))
   end if
   kk = loc_grid_size(mype+1)
   call intvec_distribute(kk, loc_grid_size, npe)
   grid_size_max = maxval(loc_grid_size(1:npe))
   lenbuff = lenbuff*grid_size_max + grid2d_size_max
   !===================
   if (pe0) write (6, *) 'Max size of recieve buffer', lenbuff
   lun = 10
   open (lun, file='dumpRestart/'//fname_comm_mype//'.bin', form='unformatted', &
     status='unknown')

   read (lun) rdata(1:10)
   read (lun) ndata(1:10)
   read (lun) nptx(1:nsp)
   read (lun) sptx_max(1:nsp)
   it_loc = ndata(1)
   nptx_max = ndata(5)
   n1_old = ndata(6)
   nxf = ndata(7)
   ndv = ndata(8) + 1
   !=========================
   tloc = rdata(1)
   targ_in = rdata(4)
   targ_end = rdata(5)
   lp_in(1) = rdata(6)
   x0_new = rdata(9)
   !=============================
   if (targ_end > xmax + x0_new) then
    allocate (xpt(nptx_max,nsp))
    allocate (wghpt(nptx_max,nsp))
    do i = 1, nsp
     do j = 1, nptx_max
      read (lun) xpt(j, i), wghpt(j, i)
     end do
    end do
    if (hybrid) then
     if (nxf>0) then
      allocate (fluid_x_profile(nxf))
      read (lun) fluid_x_profile(1:nxf)
     end if
    end if
   end if
   !==================== dumped by pe0 even if no particles are present
   read (lun) npt_arr(1:npe, 1:nsp)
   read (lun) loc_npty(1:nsp)
   read (lun) loc_nptz(1:nsp)
   close (lun)
   !=========================
   if (x0_new > 0.0) then
    x = x + x0_new
    xh = xh + x0_new
    xmin = xmin + x0_new
    xmax = xmax + x0_new
    loc_xgrid(imodx)%gmin = loc_xgrid(imodx)%gmin + x0_new
    loc_xgrid(imodx)%gmax = loc_xgrid(imodx)%gmax + x0_new
    xp0_out = xp0_out + x0_new
    xp1_out = xp1_out + x0_new
    xmn = loc_xgrid(imodx)%gmin
   end if
   do i = 1, npe
    ip_loc(i) = sum(npt_arr(i, 1:nsp))
   end do
   ipe = 0
   do i = 0, npe_xloc - 1
    do j = 0, npe_zloc - 1
     do k = 0, npe_yloc - 1
      loc_npart(k, j, i, 1:nsp) = npt_arr(ipe+1, 1:nsp)
      ipe = ipe + 1
     end do
    end do
   end do
   nypt_max = maxval(loc_npty(1:nsp))
   nzpt_max = maxval(loc_nptz(1:nsp))
   allocate (loc_ypt(nypt_max,nsp))
   allocate (loc_zpt(nzpt_max,nsp))
   allocate (loc_wghyz(nypt_max,nzpt_max,nsp))
   ! x() defined on the grid module starting from x(1)=0.0
   !============================================
   allocate (recv_buff(lenbuff))
   recv_buff(:) = 0.0
   !=== FLUID RESTART SECTION ===
   if (hybrid) then
    lenw(1:npe) = 2*fl_cp*loc_grid_size(1:npe) + loc2d_grid_size(1:npe)
    !==========================
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
    fnamel_fl)
    kk = 0
    do k = 1, n3_loc
     do j = 1, n2_loc
      kk = kk + 1
      fluid_yz_profile(j, k) = recv_buff(kk)
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        up(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        up0(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    if (pe0) write (6, *) 'Fluid density and momentum data read'
   end if
   !=== END FLUID RESTART SECTION ===

   !=== ENVELOPE RESTART SECTION ===
   if (envelope) then
    lenw(1:npe) = (env_cp+env1_cp)*loc_grid_size(1:npe)
    !==================
    disp = lenw(1+mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
    fnamel_env)
    !======================
    kk = 0
    do ic = 1, env_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        env(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    if (Two_color) then
     do ic = 1, env1_cp
      do k = 1, n3_loc
       do j = 1, n2_loc
        do i = 1, n1_loc
         kk = kk + 1
         env1(i, j, k, ic) = recv_buff(kk)
        end do
       end do
      end do
     end do
    end if
    if (pe0) write (6, *) 'Envelope field data read'
   end if
   !=== END ENVELOPE RESTART SECTION ===

   !=== FIELD RESTART SECTION ===
   lenw(1:npe) = ebf_cp*loc_grid_size(1:npe)
   !=========================
   disp = lenw(1+mype)
   disp_col = imody*disp
   disp_col = 8*disp_col

   call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
   fnamel_ebf)
   !===========================
   kk = 0
   do ic = 1, ebf_cp
    do k = 1, n3_loc
     do j = 1, n2_loc
      do i = 1, n1_loc
       kk = kk + 1
       ebf(i, j, k, ic) = recv_buff(kk)
      end do
     end do
    end do
   end do
   if (pe0) write (6, *) 'Electromagnetic fields data read'
   !=== END FIELD RESTART SECTION===
   
   !=== PARTICLES RESTART SECTION===
   do i = 1, nsp
    nps_loc(i) = maxval(npt_arr(1:npe,i))
   end do
   np_max = maxval(nps_loc(1:nsp))
   init_part(1:nsp) = 0
   if(np_max >0)then                    !READS particles (if any)
    allocate(mobilearray(nsp), source=.false.)
    call p_alloc(spec_in, spec_aux_in, 0, ndv, init_part(1:nsp), nsp, lpf_ord, 1, &
      mem_psize, mobilearray)
    !=======================
    ic = 1
    lenw(1:npe) = 0
    size_prop = spec_in(ic)%properties_array_size()
    size_prop_aux = spec_aux_in(ic)%properties_array_size()
    lenw(1:npe) = (size_prop + size_prop_aux)*nsp
    disp_col = 0
    disp_coord = imodz*npe_yloc + npe_yloc*npe_zloc*imodx
    if (mod(mype, npe_yloc)>0) disp_col = sum(lenw(disp_coord + 1:mype))
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
    fnamel_prop)
    call particles_restart_properties(spec_in, spec_aux_in, recv_buff)
    kk = 1
    do ic = 1, nsp
     open (lun, file=fnamel_name, form='unformatted', status='unknown')
     read(lun) name_buff
     call spec_in(ic)%set_name(name_buff)
    end do
    close(lun)
    
    ic = 1
    lenw(1:npe) = 0
    size_spec = spec_in(ic)%total_size()
    size_spec_aux = spec_aux_in(ic)%total_size()
    lenw(1:npe) = lenw(1:npe) + (size_spec + size_spec_aux)* ip_loc(1:npe)
    disp_col = 0
    disp_coord = imodz*npe_yloc + npe_yloc*npe_zloc*imodx
    if (mod(mype, npe_yloc)>0) disp_col = sum(lenw(disp_coord + 1:mype))
    disp_col = 8*disp_col
    max_npt_size = (size_spec + size_spec_aux)*maxval(ip_loc(1:npe))
    lenbuff = max(lenbuff, max_npt_size)
    call array_realloc_1d(recv_buff, lenbuff)
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
    fnamel_part)
    
    !==============================
    call particles_restart(spec_in, spec_aux_in, loc_npart, recv_buff)
   end if
   !=================================
    kk = 0
    do ic = 1, nsp
     if (loc_npty(ic) > 0) then
      do i = 1, loc_npty(ic)
       kk = kk + 1
      end do
     end if
    end do
    do ic = 1, nsp
     if (loc_nptz(ic) > 0) then
      do j = 1, loc_nptz(ic)
       kk = kk + 1
      end do
     end if
    end do
    do ic = 1, nsp
     if (loc_nptz(ic) > 0) then
      do j = 1, loc_nptz(ic)
       if (loc_npty(ic) > 0) then
        do i = 1, loc_npty(ic)
         kk = kk + 1
        end do
       end if
      end do
     end if
    end do
    call intvec_distribute(kk, lenw, npe)
    np_max=maxval(lenw(1:npe))
    if(np_max >0)then
    
     disp = 0
     if (mype>0) disp = sum(lenw(1:mype))
     disp = 8*disp
     call mpi_read_dp(recv_buff, lenw(mype+1), disp, fnamel_yz)
     kk = 0
     do ic = 1, nsp
      do i = 1, loc_npty(ic)
       kk = kk + 1
       loc_ypt(i, ic) = recv_buff(kk)
      end do
     end do
     do ic = 1, nsp
      do j = 1, loc_nptz(ic)
       kk = kk + 1
       loc_zpt(j, ic) = recv_buff(kk)
      end do
     end do
     do ic = 1, nsp
      do j = 1, loc_nptz(ic)
       do i = 1, loc_npty(ic)
        kk = kk + 1
        loc_wghyz(i, j, ic) = recv_buff(kk)
       end do
      end do
     end do
    end if !end of part read
    if (pe0) write (6, *) 'Particles data read'
   !============================================
   deallocate (recv_buff)
   !===============================
   if (pe0) write (6, *) 'END TOTAL DUMP READ'
  end subroutine
  !===========================
  subroutine restart_old(it_loc, tloc, spec_in, spec_aux_in)
   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (out) :: it_loc
   real (dp), intent (out) :: tloc
   character (9) :: fname = '         '
   character (9) :: fname_yz = '         '
   character (9) :: fname_ebf = '         '
   character (9) :: fname_env = '         '
   character (9) :: fname_fl = '         '
   character (9) :: fname_part = '         '
   character (11) :: fnamel_part = '           '
   character (11) :: fnamel_ebf = '           '
   character (11) :: fnamel_env = '           '
   character (11) :: fnamel_fl = '           '
   character (11) :: foldername = '           '
   character (25) :: fname_out = '                         '
   character (27) :: fnamel_out = '                           '
   integer (offset_kind) :: disp_col, disp
   integer :: max_npt_size, ipe, npt_arr(npe, nsp)
   integer :: k1, ndv, np, ic, lun, i, j, k, kk, lenw(npe), lenbuff, &
              k2, k3
   integer :: ip_loc(npe), loc_grid_size(npe), loc2d_grid_size(npe)
   integer :: grid_size_max, grid2d_size_max
   integer :: env_cp, env1_cp, fl_cp, ebf_cp
   integer :: ndata(10), nps_loc(4), n1_old
   integer :: n1_loc, n2_loc, n3_loc, nypt_max, nzpt_max
   integer :: dist_npy(npe_yloc, nsp), dist_npz(npe_zloc, nsp)
   real(dp) :: rdata(10), x0_new
   logical :: sd

   !==============
   write (fname, '(a9)') 'Comm-data'
   write (fname_ebf, '(a9)') 'EB-fields'
   write (fname_env, '(a9)') 'ENVfields'
   write (fname_fl, '(a9)') 'FL-fields'
   write (fname_part, '(a9)') 'Particles'
   write (foldername, '(a11)') 'dumpRestart'
   write (fname_yz, '(a9)') 'Dist-wgyz'
   !==============       Already defined data
   n1_loc = size(ebf, 1)
   n2_loc = size(ebf, 2)
   n3_loc = size(ebf, 3)
   ebf_cp = size(ebf, 4)
   !===================
   loc_grid_size(mype + 1) = n1_loc*n2_loc*n3_loc
   loc2d_grid_size(mype + 1) = n2_loc*n3_loc
   lenbuff = ebf_cp
   if (envelope) then
    env1_cp = 0
    env_cp = size(env, 4)
    if (Two_color) env1_cp = size(env1, 4)
    lenbuff = max(lenbuff, env_cp + env1_cp)
   end if
   grid2d_size_max = 0
   if (hybrid) then
    fl_cp = size(up, 4)
    lenbuff = max(lenbuff, 2*fl_cp)
    kk = loc2d_grid_size(mype + 1)
    call intvec_distribute(kk, loc2d_grid_size, npe)
    grid2d_size_max = maxval(loc2d_grid_size(1:npe))
   end if
   kk = loc_grid_size(mype + 1)
   call intvec_distribute(kk, loc_grid_size, npe)
   grid_size_max = maxval(loc_grid_size(1:npe))
   lenbuff = lenbuff*grid_size_max + grid2d_size_max
   !===================
   if (pe0) write (6, *) 'Max size of recieve buffer', lenbuff
   lun = 10
   if (pe0) then
    open (lun, file='dumpRestart/'//fname//'.bin', form='unformatted', &
          status='unknown')

    read (lun) rdata(1:10)
    read (lun) ndata(1:10)
    read (lun) nptx(1:nsp)
    read (lun) sptx_max(1:nsp)
    it_loc = ndata(1)
    nptx_max = ndata(5)
    n1_old = ndata(6)
    nxf = ndata(7)
    ndv = ndata(8) + 1
    !=========================
    tloc = rdata(1)
    targ_in = rdata(4)
    targ_end = rdata(5)
    lp_in(1) = rdata(6)
    x0_new = rdata(9)
    !=============================
    if (targ_end > xmax + x0_new) then
     allocate (xpt(nptx_max, nsp))
     allocate (wghpt(nptx_max, nsp))
     do i = 1, nsp
      do j = 1, nptx_max
       read (lun) xpt(j, i), wghpt(j, i)
      end do
     end do
     if (hybrid) then
      if (nxf > 0) then
       allocate (fluid_x_profile(nxf))
       read (lun) fluid_x_profile(1:nxf)
      end if
     end if
    end if
!==================== dumped by pe0 even if no particles are present
    read (lun) npt_arr(1:npe, 1:nsp)
    read (lun) dist_npy(1:npe_yloc, 1:nsp)
    read (lun) dist_npz(1:npe_zloc, 1:nsp)
    close (lun)
   end if !end pe0 read on fname
   !========================= distribute comm data
   kk = size(rdata)
   k1 = size(ndata)
   k2 = size(nptx)
   k3 = size(sptx_max)
   call vint_bcast(ndata, k1)
   call vint_bcast(nptx, k2)
   call vint_bcast(sptx_max, k3)
   call real_bcast(rdata, kk)
   it_loc = ndata(1)
   nptx_max = ndata(5)
   n1_old = ndata(6)
   nxf = ndata(7)
   ndv = ndata(8) + 1
   !=========================
   tloc = rdata(1)
   targ_in = rdata(4)
   targ_end = rdata(5)
   lp_in(1) = rdata(6)
   x0_new = rdata(9)
   if (x0_new > 0.0) then
    x = x + x0_new
    xh = xh + x0_new
    xmin = xmin + x0_new
    xmax = xmax + x0_new
    loc_xgrid(imodx)%gmin = loc_xgrid(imodx)%gmin + x0_new
    loc_xgrid(imodx)%gmax = loc_xgrid(imodx)%gmax + x0_new
    xp0_out = xp0_out + x0_new
    xp1_out = xp1_out + x0_new
    xmn = loc_xgrid(imodx)%gmin
   end if
   if (targ_end > xmax) then
    if (mype > 0) then
     allocate (xpt(nptx_max, nsp))
     allocate (wghpt(nptx_max, nsp))
     if (hybrid) then
      if (nxf > 0) allocate (fluid_x_profile(nxf))
     end if
    end if
    if (pe0) then
     sd = .true.
     do ipe = 1, npe - 1
      call exchange_2d_grdata(sd, xpt, nptx_max, nsp, ipe, ipe + 100)
      call exchange_2d_grdata(sd, wghpt, nptx_max, nsp, ipe, ipe + 400)
     end do
    else
     sd = .false.
     call exchange_2d_grdata(sd, xpt, nptx_max, nsp, pe_min, mype + 100)
     call exchange_2d_grdata(sd, wghpt, nptx_max, nsp, pe_min, mype + 400)
    end if
    !===========================
    if (hybrid) then
     if (nxf > 0) then
      if (pe0) then
       sd = .true.
       do ipe = 1, npe - 1
        call exchange_1d_grdata(sd, fluid_x_profile, nxf, ipe, ipe + 10)
       end do
      else
       sd = .false.
       call exchange_1d_grdata(sd, fluid_x_profile, nxf, pe_min, mype + 10)
      end if
     end if
    end if
   end if
   !Pe0 distributes npart => npt(npe,nsp)
   call vint_2d_bcast(npt_arr, npe, nsp)
   do i = 1, npe
    ip_loc(i) = sum(npt_arr(i, 1:nsp))
   end do
   max_npt_size = ndv*maxval(ip_loc(1:npe))
   lenbuff = max(lenbuff, max_npt_size)
   ipe = 0
   do i = 0, npe_xloc - 1
    do j = 0, npe_zloc - 1
     do k = 0, npe_yloc - 1
      loc_npart(k, j, i, 1:nsp) = npt_arr(ipe + 1, 1:nsp)
      ipe = ipe + 1
     end do
    end do
   end do
   !========== distributes npty,nptz initial particle distribution
   call vint_2d_bcast(dist_npy, npe_yloc, nsp)
   call vint_2d_bcast(dist_npz, npe_zloc, nsp)
   loc_npty(1:nsp) = dist_npy(imody + 1, 1:nsp)
   loc_nptz(1:nsp) = dist_npz(imodz + 1, 1:nsp)
   nypt_max = maxval(loc_npty(1:nsp))
   nzpt_max = maxval(loc_nptz(1:nsp))
   allocate (loc_ypt(nypt_max, nsp))
   allocate (loc_zpt(nzpt_max, nsp))
   allocate (loc_wghyz(nypt_max, nzpt_max, nsp))
   ! x() defined on the grid module starting from x(1)=0.0
   !---------- Particle read
   !============================================
   allocate (recv_buff(lenbuff))
   recv_buff(:) = 0.0
   !=== FLUID RESTART SECTION ===
   if (hybrid) then
    write (fnamel_fl, '(a9,i2.2)') 'FL-fields', imodz
    fnamel_out = 'dumpRestart/'//fnamel_fl//'.bin'
    lenw(1:npe) = 2*fl_cp*loc_grid_size(1:npe) + loc2d_grid_size(1:npe)
    !==========================
    disp = lenw(1 + mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
      fnamel_out)
    kk = 0
    do k = 1, n3_loc
     do j = 1, n2_loc
      kk = kk + 1
      fluid_yz_profile(j, k) = recv_buff(kk)
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        up(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    do ic = 1, fl_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        up0(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    if (pe0) write (6, *) 'Fluid density and momentum data read'
   end if
   !=== END FLUID RESTART SECTION ===

   !=== ENVELOPE RESTART SECTION ===
   if (envelope) then
    write (fnamel_env, '(a9,i2.2)') 'ENVfields', imodz
    fnamel_out = 'dumpRestart/'//fnamel_env//'.bin'
    lenw(1:npe) = (env_cp + env1_cp)*loc_grid_size(1:npe)
    !==================
    disp = lenw(1 + mype)
    disp_col = imody*disp
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
      fnamel_out)
    !======================
    kk = 0
    do ic = 1, env_cp
     do k = 1, n3_loc
      do j = 1, n2_loc
       do i = 1, n1_loc
        kk = kk + 1
        env(i, j, k, ic) = recv_buff(kk)
       end do
      end do
     end do
    end do
    if (Two_color) then
     do ic = 1, env1_cp
      do k = 1, n3_loc
       do j = 1, n2_loc
        do i = 1, n1_loc
         kk = kk + 1
         env1(i, j, k, ic) = recv_buff(kk)
        end do
       end do
      end do
     end do
    end if
    if (pe0) write (6, *) 'Envelope field data read'
   end if
   !=== END ENVELOPE RESTART SECTION ===

   !=== FIELD RESTART SECTION ===
   write (fnamel_ebf, '(a9,i2.2)') 'EB-fields', imodz
   fnamel_out = 'dumpRestart/'//fnamel_ebf//'.bin'
   lenw(1:npe) = ebf_cp*loc_grid_size(1:npe)
   !=========================
   disp = lenw(1 + mype)
   disp_col = imody*disp
   disp_col = 8*disp_col

   call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
     fnamel_out)
   !===========================
   kk = 0
   do ic = 1, ebf_cp
    do k = 1, n3_loc
     do j = 1, n2_loc
      do i = 1, n1_loc
       kk = kk + 1
       ebf(i, j, k, ic) = recv_buff(kk)
      end do
     end do
    end do
   end do
   if (pe0) write (6, *) 'Electromagnetic fields data read'
   !=== END FIELD RESTART SECTION===

   do i = 1, nsp
    nps_loc(i) = maxval(npt_arr(1:npe, i))
   end do
   np_max = maxval(nps_loc(1:nsp))
   if (np_max > 0) then                    !READS particles (if any)
    write (fnamel_part, '(a9,i2.2)') 'Particles', imodz
    fnamel_out = 'dumpRestart/' // fnamel_part // '.bin'
    call p_alloc(spec_in, spec_aux_in, np_max, ndv, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
    lenw(1:npe) = ndv*ip_loc(1:npe)
    !=======================
    disp_col = 0
    if (mod(mype, npe_yloc) > 0) disp_col = sum(lenw(imodz*npe_yloc + 1:mype))
    disp_col = 8*disp_col
    call mpi_read_col_dp(recv_buff, lenw(1+mype), disp_col, &
      fnamel_out)
    !==============================
    kk = 0
    do ic = 1, nsp
     np = loc_npart(imody, imodz, imodx, ic)
     if (np > 0) then
      do j = 1, ndv
       do i = 1, np
        kk = kk + 1
        spec_in(ic)%part(i, j) = recv_buff(kk)
       end do
      end do
     end if
    end do
   end if
   !=================================
   fname_out = 'dumpRestart/'//fname_yz//'.bin'
   kk = 0
   do ic = 1, nsp
    if (loc_npty(ic) > 0) then
     do i = 1, loc_npty(ic)
      kk = kk + 1
     end do
    end if
   end do
   do ic = 1, nsp
    if (loc_nptz(ic) > 0) then
     do j = 1, loc_nptz(ic)
      kk = kk + 1
     end do
    end if
   end do
   do ic = 1, nsp
    if (loc_nptz(ic) > 0) then
     do j = 1, loc_nptz(ic)
      if (loc_npty(ic) > 0) then
       do i = 1, loc_npty(ic)
        kk = kk + 1
       end do
      end if
     end do
    end if
   end do
   call intvec_distribute(kk, lenw, npe)
   np_max = maxval(lenw(1:npe))
   if (np_max > 0) then

    disp = 0
    if (mype > 0) disp = sum(lenw(1:mype))
    disp = 8*disp
    call mpi_read_dp(recv_buff, lenw(mype + 1), disp, fname_out)
    kk = 0
    do ic = 1, nsp
     do i = 1, loc_npty(ic)
      kk = kk + 1
      loc_ypt(i, ic) = recv_buff(kk)
     end do
    end do
    do ic = 1, nsp
     do j = 1, loc_nptz(ic)
      kk = kk + 1
      loc_zpt(j, ic) = recv_buff(kk)
     end do
    end do
    do ic = 1, nsp
     do j = 1, loc_nptz(ic)
      do i = 1, loc_npty(ic)
       kk = kk + 1
       loc_wghyz(i, j, ic) = recv_buff(kk)
      end do
     end do
    end do
   end if !end of part read
   if (pe0) write (6, *) 'Particles data read'
   !============================================
   deallocate (recv_buff)
   !===============================
   if (pe0) write (6, *) 'END TOTAL DUMP READ'
  end subroutine
  !===========================

 end module

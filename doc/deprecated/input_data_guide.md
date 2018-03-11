# ALaDyn `input.data` guide

Following here is a brief description of the `input.data` file required for the simulation parameters definition, through an example. This method is currently deprecated, in favour of the new input namelist.

## Lines 1-2-3-4 / Grid parameters

```fortran
7168,   3584,    1,     3500
  nx,     ny,   nz,  ny_part
100.0,    2.0
  k0, yx_rat
```

+ `nx` is the number of grid points in the *x* direction
  + for PWFA simulations, it is required that this number contains the number of cpus of the *y* and *z* domains defined by `pey` in order to be sure that FFTs are working as expected
+ `ny` is the number of points in the *y* direction
  + in order to be sure that the simulation is working as expected, it is better to *always* use a number that contains `pey`
+ `nz` is the number of points in the *z* direction; for a 2D simulaton, use `nz = 1`
  + in order to be sure that the 3D simulation is working as expected, it is better to *always* use here a number that contains the number of cpu assigned to split the *z* domain (`mpi_ntot/pey`)
+ `nypart` is the transverse size of the target, expressed in number of grid cell filled with particles (if the simulation is a 3D one, the target is a square in the *xy* plane)
+ `k0` defines the resolution, being the numbero of points per μm along *x*, so that Δx = 1/k0 [μm]; please remember to set a value small enough to solve the skin depth
+ `yxrat` is the ratio between the resolution along *x* and *y*: Δy = yxrat/k0 [μm]; the resolution along *z* is the same as for *y*.

With those parameters, the full box size (in μm) is: `Lx = nx / k0`, `Ly = yxrat * ny / k0`, `Lz = yxrat * nz / k0`

## Lines 5-6 / Numerical schemes

```fortran
  2,   2,   0,     0
LPf, Der, str, iform
```

+ `Lpf` is the leapfrog order used in the integration scheme
+ `Der` is the order of the finite difference scheme
+ `str` has three possible different values:
  + `0` for uniform grid
  + `1` to enable stretching along transverse axes
  + `2` to enable stretching along all axes, but for *x* it is only on the right side of the box (need testing)
+ `iform` has two possible different values:
  + `0`: (D<sub>x</sub>J<sub>x</sub>,D<sub>y</sub>J<sub>y</sub>,D<sub>z</sub>J<sub>z</sub>) to be inverted on a grid (to be preferred) (energy conservation not guaranteed)
  + `1`: only (D<sub>x</sub>J<sub>x</sub>) to be inverted on a grid
  + `2`: no charge preserving (use energy conservation algorithm)

## Lines 7-8 / Driver and target models

```fortran
  1,    3,     1
mdl, dmdl, ibeam
```

+ `mdl` has five possible different values
  + `1` laser is p-polarized
  + `2` laser is s-polarized
  + `3` laser is circularly polarized
  + `4` laser is described by its envelope approximation model
  + `5` the driver is an electron beam. Its description must be given in a separate namelist file
  + `6` the driver is a proton bunch. Its description must be given in a separate namelist file
+ `dmdl` has five possible different values
  + `1` the simulation is done using just two species, electrons and protons
  + `2` four species, e, p, C, Al, used in this way: [e+p]foam + [e+Al]bulk + [e+C+p]contaminants (Al is in reality a generic atom, defined by `Z_i` and `A_i`, while C is hard coded: we use Al in this readme since it is the common experimental choice, while for C it is an experimental mandatory choice - but it will be lifted in the future)
  + `3` three species, e, p, C, in one layer: [e+C+p*n]bulk (mind the *n*, managed with `nsb`) (C is in reality a generic atom, defined by `Z_i` and `A_i`: we use C in this readme since it is the common experimental choice)
  + `4` three species, e, p, Al, used in this way: [e+p]foam + [e+Al]bulk + [e+p]contaminants
  + `5` three species, e, p, Al, identic to case `4`, but with the contaminant layer *mass limited*
+ `ibeam`:
    for PWFA (model_id #5/#6):
  + `0`
  + `1`
  + `2`

## Lines 9-10 / Number and type of particle species

```fortran
  0,   0,   0,   3,   1
ibx, iby, ibz, nsp, nsb
```

+ `nsp` is the number of species (be careful and coherent with `dmdl`)
+ `nsb`, used with `dmdl=3`, defines the ratio *n* between the number of C ions and protons, to form *CH<sub>n</sub>*

## Lines 11-12 / Ionization status and schemes

```fortran
    4,         11,        13,               26.98
ion_min(1),ion_max(1),atomic_number(1),mass_number(1)
    1,         1,          1,         1.0
ion_min(2),ion_max(2),atomic_number(2),mass_number(2)
    0,       0
ionz_lev,ionz_model,
```

+ `ion_min`
+ `ion_max`
+ `atomic_number`
+ `mass_number`
+ `ionz_lev`
+ `ionz_model`

## Lines 13-14 / Initial temperatures of the species

```fortran
 0.0003 ,    0.0 ,    0.0 ,    0.0
t0_pl(1),t0_pl(2),t0_pl(3),t0_pl(4)
```

## Lines 15-16-17-18 / Number of particles per cell

```fortran
   12, 4, 4, 4, 4, 4
np_xc
    9, 3, 4, 4, 4, 4
np_yc
```

*First two are the number of electrons and ions per cell along x/y in the bulk, second two numbers refer to the front layer and third two to the contaminants. A special case is `dmdl=3`, in which the first indicates the number of electrons, the second the number of protons (to be multiplies by `nsb`) and the third is the number of ions; in this case the other numbers are not important.*

Describing the previous example, it means that, along x:

+ n<sub>e</sub><sup>x</sup> = 9, n<sub>i</sub><sup>x</sup> = 4, in the central layer (bulk)
+ n<sub>e</sub><sup>x</sup> = 3, n<sub>i</sub><sup>x</sup> = 3, in the frontal layer (foam)
+ n<sub>e</sub><sup>x</sup> = 6, n<sub>i</sub><sup>x</sup> = 6, in the contaminant layer

**n<sub>e</sub><sup>x</sup> = 0 in the first `np_xc` position just means to ignore the plasma and do just a laser propagation simulation.**

+ `np_yc`: the same as `np_xc`, this describes the number of particles per cell along transverse directions (valid also for *z* for 3D simulations)

## Lines 19-20 / Laser parameters

```fortran
16.5, 16.5, 33.0, 6.2,  3.0,  0.8
  tf,   xc,   wx,  wy,   a0, lam0
```

**these lines are valid only for `ibeam=1`**

+ `xc` is the position (in μm) along *x* of the central point of the laser envelope
+ `tf` is the distance (in μm) after which the laser is focused; so the focus is at `xf = xc + tf`
+ `wx` is twice the longitudinal waist, corresponding to the total laser length, which is twice the FWHM. For example, if we have a 40 fs FWHM Ti:Sa laser, we should write for `wx=33 μm` because:

```fortran
    (40 fs/3.333)*2*1.37 = 33 μm
    40/3.3333 is done to convert fs to μm
    *2 is to convert from FWHM to full length
    *1.37 is to convert from laser intensity to fields: usually we receive from experiments the FWHM of the laser intensity, but we set the length of fields and the FWHM of a cos4 has to be converted to the FWHM of a cos2: FWHM(cos4)=FWHM(cos2)/1.37
```

+ `wy` is the transverse waist FWHM (in μm)
+ `a` is the laser adimensional parameter: a<sub>0</sub>=eA/(m<sub>e</sub> c<sup>2</sup>)
+ `lam0` is the laser wavelength (in μm)

## Lines 21-22 / Target shape parameters

```fortran
0.0, 0.0, 2.0, 0.0, 0.08, 0.0, 0.01
 lx
```

+ `lx(1)` is the length of the upstream layer (foam or preplasma), having density `n1/nc`
+ `lx(2)` is the length of the ramp (linear or exponential depending on the `mdl`) connecting the upstream layer with the central one (made with bulk particles)
+ `lx(3)` is the length of the central layer (bulk), having density `n2/nc`
+ `lx(4)` is the length of the ramp (linear), connecting the bulk with the contaminants (made with bulk particles)
+ `lx(5)` is the length of the downstream layer (contaminants), having density `n3/nc`
+ `lx(6)` is the angle *α* of incidence, between the laser axis and the target plane
+ `lx(7)` is the offset between the end of the laser and the beginning of the target (if zero, the target starts right at the end of the laser pulse). The offset is calculated *before* laser rotation, so mind the transverse size if `lx(6) ≠ 0`, in order to avoid laser initialization *inside the target*.

## Lines 23-24 / Target shape parameters

```fortran
0.0, 0.0
 ly
```

+ `ly(1)` defines the wire size
+ `ly(2)` defines the interwire size

## Lines 25-26 / Target density parameters

```fortran
 100.0,   1.0,  10.0
 n2/nc, n1/nc, n3/nc
```

+ `n2/nc` is the density in the central layer (bulk)
+ `n1/nc` is the density in the upstream layer (foam/preplasma)
+ `n3/nc` is the density in the downstream layer (contaminants)

## Lines 27-28 / Moving window

```fortran
    20, 100.0, 150.0,     1.0
wnd_sh,  w_in, w_end, w_speed
```

+ `wsh` is the number of time steps after which the moving-window is called, repeatedly
+ `win` is the absolute time at which the window starts moving
+ `wend` is the absolute time at which the window stops moving
+ `wspeed` is the β speed of the moving window

## Lines 29-30 / Output manager

```fortran
  10,  200,     3,    1,     0,     0
nout, iene, nvout, nden, npout, nbout
```

+ `nout` is the number of binary outputs during the relative time of the simulation
+ `iene` is the number of text outputs during the relative time of the simulation
+ `nvout` is the number of *fields* written. For a 2D P-polarized case, we have just 3 fields, *E<sub>x</sub>*, *E<sub>y</sub>* and *B<sub>z</sub>*; in all the other cases there are 6 fields with these IDs: 1=*E<sub>x</sub>*, 2=*E<sub>y</sub>*, 3=*E<sub>z</sub>*, 4=*B<sub>x</sub>*, 5=*B<sub>y</sub>*, 6=*B<sub>z</sub>*. At each `nout` step, every field with `ID ≤ nf` will be dumped.
+ `nden` can have three different values; every output is divided by species on different files:
  + `1`: writes *only* the particle density *n* on the grid
  + `2`: writes *also* the energy density *n ･ gamma* on the grid
  + `3`: writes *also* the currents *J* on the grid
+ `npout`
  + `1`: writes *only* the electron phase space
  + `2`: writes *only* the proton phase space
  + in general it writes *only* the phase space of the *n-th* species, with `n=npv`; if `n=npv>nps`, it writes the phase spaces of all the particle species
+ `nbout`

## Lines 31-32 / Output subsampling

```fortran
  1,    1
jmp, pjmp
```

+ `jmp`: jump on grid points; if `jmp=2`, it means that each processor is going to write only one point every two. Mind that it's extremely important that `jmp` is a divisor of the number of grid points *per processor*, otherwise the grid output will be corrupted
+ `pjmp`: jump on particles; if `pjmp=2`, it means that each processor is going to write the phase space of just one particle every two.

## Lines 33-34 / Particle box to be dumped

```fortran
 0.0, 100.0,  20.0
 xp0,   xp1, ypmax
```

+ In the dump we will find only particles contained inside the box defined by `xp0 < x < xp1`
+ In the dump we will find only particles contained in the box `|{y,z}| < ypmax`

## Lines 35-36 / Simulation time settings

```fortran
100.0,  0.8
 tmax,  cfl
```

+ `tmax` is the relative time (in μm, because it is multiplied by *c*) that the simulation is going to be evolved. Being relative, it's going to be added to the time eventually already reached before the restart. To obtain the time in fs, you have to divide it by 0.299792458 [speed of light in μm/fs]
+ `cfl` is the Courant–Friedrichs–Lewy parameter ( [Wikipedia CFL-parameter page](http://en.wikipedia.org/wiki/Courant%E2%80%93Friedrichs%E2%80%93Lewy_condition) )

## Lines 37-38 / Restart manager and processor settings

```fortran
  0,     0,    0,  64
new, id_ew, dump, pey
```

+ `new`: identifies if a simulation is new `new=0` (data is recreated from initial conditions) or if it is a restart `new=1` (dump are going to be read)
+ `id_ew` identifies the starting number of the output files (if `new=0`) or the last one written in the previous step (if `new=1`)
+ `dump`
  + `1`: each processor will dump a binary file for every `nout` in order to enable restart
  + `0`: dumps are suppressed
+ `pey` identifies the number of processors used for 2D simulations, or how many processors are going to be used for the *y* domains (`mpi_ntot/pey` is the number of processors used for the *z* domains) for 3D simulations.

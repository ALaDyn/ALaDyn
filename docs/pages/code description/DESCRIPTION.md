# The ALaDyn PIC Code

## Overview

`ALaDyn` is a Particle in Cell (PIC) code designed to investigate three main physical regimes:  

- Laser-plasma interaction in under-dense gas targets for electron acceleration (LWFA).  
- Beam-plasma interaction in under-dense gas targets for electron acceleration (PWFA).  
- Laser-plasma interaction in over-dense solid targets for proton(ions) acceleration and related phenomenologies.  

Recent developments and applications are reported in the attached references.

## PIC numerical model

A PIC method is based on a hybrid formal setting, whereby plasma particles are represented
on a Lagrangian framework whereas self-consistent fields are represented on the Eulerian framework
given by Maxwell equations.  
As most other PIC codes, `ALaDyn` discretize particle and field dynamical equations
by centred finite differences on a staggered  space and time grid (Yee's module) using one step
second order leap-frog integrator.  
To connect Lagrangian particles to Eulerian fields collocated on the spatial grid, finite order
B-splines are used. B-splines are local polynomials with compact support, allowing to
represent delta-like point particles on a grid for charge deposition and, by converse,
to assign field grid data to a point particle.  
Energy preserving PIC schemes do not satisfy local charge conservation and the related
Poisson equation. By converse, using one of the many numerical recipes to enforce the
continuity equations, energy conservation is heavily damaged.  
`ALaDyn` code implements both charge or energy preserving schemes, letting the user
to choose, depending on the problem at hand.  
Besides the standard leap-frog integrator,
`ALaDyn` also implements a fourth order in space and time Runge-Kutta integrator. This scheme
requires larger computational resources, of course, but can be of help to
improve on accuracy and reduce dispersive effects of wave propagation.  
The code implements also reduced models based on:

- Envelope (two-scale) approximation of the laser fields and of the particle dynamics;
- A cold fluid approximation of the wake fields.

Reduced models are well tested only for (quasi) linear regimes. Serious problems
arise for non linear dynamics.

In all the above configurations field induced ionization (tunnelling) are also implemented
and can be activated if requested by the user.
All ionization models are based on ADK scheme plus barrier suppression (BSI) for higher
*Z* ions. For solid targets, impact (collisional) ionization is under development.

## Implementation

`ALaDyn` is almost completely written in `Fortran 90`, but a couple of utility modules are written in `C++`.
`C` can also be easily used to extend code functionalities.
`Fortran 90` is the most popular computational language in PIC codes, probably because of
the higher efficiency in handling multidimensional arrays on a grid.
Finite difference integration allows to exploit efficient parallelism by domain decomposition
using MPI technique to distribute the computational work among CPU units.

`ALaDyn` has been successfully ported to many HPC architectures, both in Italy at CINECA and in Europe through
PRACE Partnerships. From the CINECA IBM-SP6 system in 2011, to the test system at CINECA based on IBM/BGP in 2012,
then CINECA FERMI in 2014 and MARCONI in 2016, `ALaDyn` run on a multitude of HPC architectures, always extracting
top range performances.

A new version has been recently released open source on the web, with a GPLv3 license.
In part it has been rewritten from scratch and it is the basis for future development.
Sources can be found, together with other codes, in our organization GitHub page at
[github.com/ALaDyn](https://github.com/ALaDyn)

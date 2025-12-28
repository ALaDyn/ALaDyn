#!/usr/bin/env python3
"""
Example TNSA (Target Normal Sheath Acceleration) configuration in Python.

This configuration demonstrates a typical TNSA simulation with a solid
target and high-intensity laser pulse.
"""

import sys
import os

# Add scripts directory to path
script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'scripts')
sys.path.insert(0, script_dir)

from aladyn_config import (
    ALaDynConfig, GridConfig, SimulationConfig, TargetConfig,
    LaserConfig, MovingWindowConfig, OutputConfig, MPIConfig
)

# Define the configuration
config = ALaDynConfig(
    # Grid configuration - high resolution for solid target
    grid=GridConfig(
        nx=8000,
        ny=3000,
        nz=1,           # 2D simulation
        ny_targ=2500,
        k0=200.0,       # 200 points per μm (high resolution)
        yx_rat=1.0,
        zx_rat=1.0,
    ),
    
    # Simulation parameters
    simulation=SimulationConfig(
        LPf_ord=2,      # Leap-frog integration
        der_ord=2,      # Standard Yee scheme
        str_flag=1,     # Light stretching for boundaries
        iform=0,        # Esirkepov charge conservation
        model_id=1,     # p-polarized laser
        dmodel_id=3,    # Preplasma model
        ibx=0,          # Open boundaries in x
        iby=0,          # Open boundaries in y
        ibz=0,          # Open boundaries in z
        ibeam=0,
    ),
    
    # Target description - solid target with preplasma
    target=TargetConfig(
        nsp=3,              # 3 species (electrons + 2 ion species)
        nsb=0,              # No bunches
        ionz_lev=0,
        ionz_model=4,
        ion_min=[4, 1, 1],   # Ionization states
        ion_max=[11, 1, 1],
        atomic_number=[13, 1, 1],  # Aluminum + Hydrogen
        mass_number=[26.98, 1.0, 1.0],
        t0_pl=[0.005, 0.0, 0.0, 0.0],  # Low temperature
        np_per_xc=[6, 6, 6, 1, 1, 1],  # 6 particles per cell (solid)
        np_per_yc=[6, 6, 6, 1, 1, 1],
        concentration=[1],
        lpx=[0., 5., 1., 0.5, 0.0, 0.0, 0.5],  # Thin target with preplasma
        lpy=[0.0, 0.0],
        n0_ref=100.0,    # Solid density (100 nc)
        np1=0.0,
        np2=0.0,
        r_c=0.0,
    ),
    
    # Laser parameters - high intensity for TNSA
    laser=LaserConfig(
        G_prof=True,        # Gaussian profile
        nb_laser=1,         # Single laser
        t0_lp=0.,
        xc_lp=5.,          # Laser focused at target front
        tau_fwhm=30.,      # Short pulse (30 fs)
        w0_y=3.0,          # Tight focus (3 μm)
        a0=10.0,           # High intensity (a0=10)
        lam0=0.8,          # Wavelength (μm)
        y0_cent=[0.0],
        z0_cent=[0.0],
        incid_angle=0.0,   # Normal incidence
        Enable_ionization=[True],
        lp_delay=[0.0],
    ),
    
    # Moving window - not typically used for TNSA
    moving_window=MovingWindowConfig(
        w_sh=0,            # No window movement
        wi_time=0.,
        wf_time=1000.0,
        w_speed=1.0,
    ),
    
    # Output configuration
    output=OutputConfig(
        nouts=4,
        iene=20,           # Energy diagnostics every 20 steps
        nvout=2,           # Vector field output
        nden=1,            # Density output
        npout=1,           # Particle output
        nbout=0,
        jump=1,
        pjump=1,
        gam_min=1.0,
        xp0_out=0.,
        xp1_out=10.0,      # Focus on target region
        yp_out=15.,
        tmax=10.,          # Short simulation time
        cfl=0.8,           # CFL condition
        new_sim=0,
        id_new=0,
        dump=0,
        L_env_modulus=True,
    ),
    
    # MPI configuration
    mpi=MPIConfig(
        nprocx=2,          # 2 processors in x
        nprocy=20,         # 20 processors in y
        nprocz=1,
    ),
)

if __name__ == "__main__":
    # Generate the namelist file
    from config_to_namelist import config_to_namelist
    
    output_file = sys.argv[1] if len(sys.argv) > 1 else "input_tnsa.nml"
    config_to_namelist(config, output_file)

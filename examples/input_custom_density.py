#!/usr/bin/env python3
"""
Example LWFA configuration with custom plasma density profile.

This example demonstrates how to use Python to define custom plasma
density profiles, which is much more intuitive than the Fortran namelist
approach. This makes it easier to experiment with different target
configurations.
"""

import sys
import os
import numpy as np

# Add scripts directory to path
script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'scripts')
sys.path.insert(0, script_dir)

from aladyn_config import (
    ALaDynConfig, GridConfig, SimulationConfig, TargetConfig,
    LaserConfig, MovingWindowConfig, OutputConfig, MPIConfig
)


def custom_plasma_density(x, y, z):
    """
    Custom plasma density profile function.
    
    This example creates a density profile with:
    - A linear upramp from x=50 to x=100 μm
    - A flat-top region from x=100 to x=400 μm
    - A linear downramp from x=400 to x=450 μm
    - A transverse Gaussian profile
    
    Args:
        x, y, z: Position arrays (in μm)
        
    Returns:
        Density array (normalized to critical density)
    """
    # Initialize density
    density = np.zeros_like(x)
    
    # Longitudinal profile
    # Upramp region (50 to 100 μm)
    upramp_mask = (x >= 50) & (x < 100)
    density[upramp_mask] = (x[upramp_mask] - 50) / 50.0
    
    # Flat-top region (100 to 400 μm)
    flattop_mask = (x >= 100) & (x <= 400)
    density[flattop_mask] = 1.0
    
    # Downramp region (400 to 450 μm)
    downramp_mask = (x > 400) & (x <= 450)
    density[downramp_mask] = (450 - x[downramp_mask]) / 50.0
    
    # Transverse Gaussian profile
    r_transverse = np.sqrt(y**2 + z**2)
    transverse_profile = np.exp(-(r_transverse / 20.0)**2)
    
    # Combine longitudinal and transverse profiles
    density *= transverse_profile
    
    return density


def parabolic_channel_density(x, y, z, r0=20.0, delta_n=0.01):
    """
    Plasma density with a parabolic channel.
    
    This creates a uniform plasma with a parabolic density channel
    suitable for guiding laser pulses over long distances.
    
    Args:
        x, y, z: Position arrays (in μm)
        r0: Channel radius (μm)
        delta_n: Channel depth (fraction of background density)
        
    Returns:
        Density array (normalized to critical density)
    """
    # Base uniform density
    density = np.ones_like(x)
    
    # Longitudinal extent
    mask = (x >= 50) & (x <= 450)
    
    # Parabolic channel
    r = np.sqrt(y**2 + z**2)
    channel_profile = 1.0 - delta_n * (r / r0)**2
    channel_profile = np.maximum(channel_profile, 0.0)  # Ensure non-negative
    
    density[mask] *= channel_profile[mask]
    
    return density


# Define the configuration
config = ALaDynConfig(
    # Grid configuration
    grid=GridConfig(
        nx=5000,
        ny=4200,
        nz=1,           # 2D simulation
        ny_targ=3200,
        k0=50.0,        # 50 points per μm
        yx_rat=2.0,
        zx_rat=1.0,
    ),
    
    # Simulation parameters
    simulation=SimulationConfig(
        LPf_ord=2,      # Leap-frog integration
        der_ord=2,      # Standard Yee scheme
        str_flag=0,     # Uniform grid
        iform=0,        # Esirkepov charge conservation
        model_id=1,     # p-polarized laser
        dmodel_id=1,    # Uniform target (custom density in post-processing)
        ibx=0,          # Open boundaries in x
        iby=0,          # Open boundaries in y
        ibz=0,          # Open boundaries in z
        ibeam=1,
    ),
    
    # Target description
    # Note: For custom density, we still need to specify the basic parameters.
    # The custom density function can be used for post-processing or 
    # pre-initialization of particle distributions.
    target=TargetConfig(
        nsp=1,              # 1 species (electrons)
        nsb=0,              # No bunches
        ionz_lev=0,
        ionz_model=4,
        ion_min=[1, 1, 1],
        ion_max=[1, 1, 1],
        atomic_number=[1, 1, 1],  # Hydrogen
        mass_number=[1.0, 1.0, 1.0],
        t0_pl=[0.01, 0.0, 0.0, 0.0],  # Temperature
        np_per_xc=[2, 1, 1, 1, 1, 1],  # 2 particles per cell in x
        np_per_yc=[2, 1, 1, 1, 1, 1],  # 2 particles per cell in y
        concentration=[1],
        lpx=[0., 50., 400., 100., 0.0, 0.0, 15.0],  # Plasma profile in x
        lpy=[0.0, 0.0],  # Plasma profile in y
        n0_ref=1.0,      # Reference density
        np1=0.0,
        np2=0.0,
        r_c=0.0,
    ),
    
    # Laser parameters
    laser=LaserConfig(
        G_prof=True,        # Gaussian profile
        nb_laser=1,         # Single laser
        t0_lp=0.,
        xc_lp=50.,         # Laser center position
        tau_fwhm=40.,      # Pulse duration (FWHM)
        w0_y=20.0,         # Spot size
        a0=2.0,            # Normalized vector potential
        lam0=0.8,          # Wavelength (μm)
        y0_cent=[0.0],
        z0_cent=[0.0],
        incid_angle=0.0,
        Enable_ionization=[True],
        lp_delay=[0.0],
    ),
    
    # Moving window
    moving_window=MovingWindowConfig(
        w_sh=10,           # Window shift
        wi_time=0.,
        wf_time=1000.0,
        w_speed=1.0,       # Speed of light
    ),
    
    # Output configuration
    output=OutputConfig(
        nouts=4,
        iene=40,           # Energy diagnostics every 40 steps
        nvout=2,           # Vector field output
        nden=1,            # Density output
        npout=1,           # Particle output
        nbout=0,
        jump=1,
        pjump=1,
        gam_min=1.0,
        xp0_out=0.,
        xp1_out=60.0,
        yp_out=50.,
        tmax=45.,          # Maximum simulation time
        cfl=0.8,           # CFL condition
        new_sim=0,
        id_new=0,
        dump=0,
        L_env_modulus=True,
    ),
    
    # MPI configuration
    mpi=MPIConfig(
        nprocx=1,
        nprocy=40,         # 40 processors in y
        nprocz=1,
    ),
    
    # Attach custom density function
    custom_density_function=custom_plasma_density,
)


def visualize_density_profile():
    """
    Visualize the custom plasma density profile.
    
    This function creates plots of the density profile to help
    understand and verify the plasma configuration.
    """
    try:
        import matplotlib.pyplot as plt
    except ImportError:
        print("matplotlib not available for visualization")
        return
    
    # Create coordinate arrays
    dx = 1.0 / config.grid.k0
    dy = config.grid.yx_rat / config.grid.k0
    
    x = np.linspace(0, config.grid.nx * dx, 1000)
    y = np.linspace(-config.grid.ny * dy / 2, config.grid.ny * dy / 2, 500)
    
    X, Y = np.meshgrid(x, y)
    Z = np.zeros_like(X)
    
    # Calculate density
    density = config.custom_density_function(X, Y, Z)
    
    # Create figure with two subplots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))
    
    # 2D density map
    im = ax1.pcolormesh(X, Y, density, shading='auto', cmap='viridis')
    ax1.set_xlabel('x (μm)')
    ax1.set_ylabel('y (μm)')
    ax1.set_title('Custom Plasma Density Profile')
    plt.colorbar(im, ax=ax1, label='n/nc')
    
    # Longitudinal lineout (at y=0)
    ax2.plot(x, density[len(y)//2, :], 'b-', linewidth=2)
    ax2.set_xlabel('x (μm)')
    ax2.set_ylabel('n/nc')
    ax2.set_title('Longitudinal Density Profile (y=0)')
    ax2.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('custom_plasma_density.png', dpi=150)
    print("Density profile saved to custom_plasma_density.png")


if __name__ == "__main__":
    # Generate the namelist file
    from config_to_namelist import config_to_namelist
    
    output_file = sys.argv[1] if len(sys.argv) > 1 else "input_custom.nml"
    config_to_namelist(config, output_file)
    
    # Optionally visualize the density profile
    if "--visualize" in sys.argv:
        visualize_density_profile()

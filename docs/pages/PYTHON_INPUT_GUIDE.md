# ALaDyn Python Input Guide

## Introduction

ALaDyn now supports **Python-based input configuration**, providing a modern, intuitive alternative to traditional Fortran namelists. This approach offers several advantages:

- **Easier to understand and write**: Use familiar Python syntax instead of Fortran namelist format
- **Better validation**: Automatic parameter checking with helpful error messages
- **Custom plasma density functions**: Define complex plasma profiles using Python functions
- **Type safety**: Python type hints help prevent configuration errors
- **IDE support**: Get autocomplete and inline documentation in modern editors
- **Reusability**: Share common configurations using Python imports

## Quick Start

### 1. Install Python (if needed)

Python 3.6 or later is required. Most systems already have Python installed.

```bash
python3 --version
```

### 2. Create a Python Configuration File

Here's a minimal example (`my_simulation.py`):

```python
#!/usr/bin/env python3
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'scripts'))

from aladyn_config import *

config = ALaDynConfig(
    grid=GridConfig(
        nx=1000,
        ny=500,
        nz=1,
        k0=50.0,
    ),
    simulation=SimulationConfig(
        model_id=1,
        dmodel_id=1,
    ),
    target=TargetConfig(
        nsp=1,
        n0_ref=1.0,
    ),
    laser=LaserConfig(
        a0=2.0,
        lam0=0.8,
        w0_y=20.0,
    ),
    moving_window=MovingWindowConfig(),
    output=OutputConfig(
        tmax=50.0,
    ),
    mpi=MPIConfig(
        nprocy=4,
    ),
)
```

### 3. Generate Namelist File

Convert the Python configuration to a Fortran namelist:

```bash
cd examples
python3 my_simulation.py output.nml
```

Or use the converter directly:

```bash
cd scripts
python3 config_to_namelist.py ../examples/my_simulation.py ../input.nml
```

### 4. Run ALaDyn

Run ALaDyn as usual with the generated namelist:

```bash
mpirun -np 4 ./ALaDyn
```

The code will read `input.nml` automatically.

## Configuration Sections

### GridConfig

Defines the computational grid:

```python
grid=GridConfig(
    nx=5000,        # Grid points in x
    ny=4200,        # Grid points in y
    nz=1,           # Grid points in z (1 for 2D)
    ny_targ=3200,   # Target size in cells
    k0=50.0,        # Resolution (points per μm)
    yx_rat=2.0,     # y/x resolution ratio
    zx_rat=1.0,     # z/x resolution ratio
)
```

**Key parameters:**
- `k0`: Controls spatial resolution. Higher values = finer grid
- Domain size: `Lx = nx/k0`, `Ly = ny*yx_rat/k0`, `Lz = nz*zx_rat/k0` (in μm)

### SimulationConfig

Controls the simulation algorithm:

```python
simulation=SimulationConfig(
    LPf_ord=2,      # 2=leap-frog, 4=RK4
    der_ord=2,      # Finite difference order (2, 3, or 4)
    model_id=1,     # 1=p-pol, 2=s-pol, 3=circular, 4=envelope
    dmodel_id=1,    # 1=uniform, 3=preplasma, 4=foam, etc.
    iform=0,        # 0=charge conserving
)
```

### TargetConfig

Defines the plasma target:

```python
target=TargetConfig(
    nsp=1,                      # Number of species
    atomic_number=[1, 1, 1],    # Z for each species
    mass_number=[1.0, 1.0, 1.0],# Mass for each species
    t0_pl=[0.01, 0.0, 0.0, 0.0],# Temperature (mc²)
    np_per_xc=[2, 1, 1, 1, 1, 1],  # Particles per cell
    np_per_yc=[2, 1, 1, 1, 1, 1],
    lpx=[0., 50., 400., 100., 0., 0., 15.],  # Longitudinal profile
    n0_ref=1.0,                 # Density (in nc)
)
```

**Longitudinal profile (`lpx`):**
- `lpx[0]`: Empty space before plasma
- `lpx[1]`: Upramp length
- `lpx[2]`: Flat-top length
- `lpx[3]`: Downramp length
- `lpx[4]`: Empty space after plasma
- `lpx[5]`: Reserved
- `lpx[6]`: Pre-plasma scale length

### LaserConfig

Laser pulse parameters:

```python
laser=LaserConfig(
    a0=2.0,         # Normalized vector potential
    lam0=0.8,       # Wavelength (μm)
    tau_fwhm=40.0,  # Pulse duration (fs)
    w0_y=20.0,      # Spot size (μm)
    xc_lp=50.0,     # Focal position
)
```

**Laser intensity:** The intensity is related to `a0` by:
- `I [W/cm²] ≈ 1.37 × 10¹⁸ × a0² / lam0²`

### OutputConfig

Control output and diagnostics:

```python
output=OutputConfig(
    tmax=45.0,      # Simulation time (1/ωp)
    cfl=0.8,        # CFL condition (< 1.0)
    iene=40,        # Energy output every N steps
    nvout=2,        # Field output interval
    nden=1,         # Density output interval
    npout=1,        # Particle output interval
)
```

### MPIConfig

Parallel execution setup:

```python
mpi=MPIConfig(
    nprocx=1,       # Processors in x
    nprocy=40,      # Processors in y
    nprocz=1,       # Processors in z
)
```

**Total processors:** `nprocx × nprocy × nprocz`

## Examples

### Example 1: LWFA Simulation

See `examples/input_lwfa.py` for a complete LWFA configuration.

```bash
cd examples
python3 input_lwfa.py
```

### Example 2: PWFA Simulation

See `examples/input_pwfa.py` for a complete PWFA configuration with beam injection.

```bash
cd examples
python3 input_pwfa.py
```

### Example 3: Custom Plasma Density

The Python approach makes it easy to define custom plasma density profiles:

```python
import numpy as np

def my_density(x, y, z):
    """Custom density with upramp and channel."""
    # Longitudinal profile
    density = np.where(x < 100, x/100.0, 1.0)
    
    # Add parabolic channel
    r = np.sqrt(y**2 + z**2)
    density *= (1.0 - 0.01 * (r/20.0)**2)
    
    return density

config = ALaDynConfig(
    # ... other parameters ...
    custom_density_function=my_density,
)
```

See `examples/input_custom_density.py` for a complete example with visualization.

## Advanced Features

### Configuration Validation

The Python system automatically validates parameters:

```python
# This will raise an error
grid = GridConfig(
    nx=-100,  # ERROR: negative value
)

# This will raise an error
laser = LaserConfig(
    model_id=99,  # ERROR: invalid model
)
```

### Configuration Summary

Print a human-readable summary:

```python
config.validate()
print(config.summary())
```

Output:
```
ALaDyn Configuration Summary
============================

Grid:
  Domain size: 100.00 × 84.00 × 1.00 μm³
  Grid points: 5000 × 4200 × 1
  Resolution: Δx=0.0200, Δy=0.0200, Δz=1.0000 μm
...
```

### Sharing Common Configurations

Use Python imports to share configurations:

```python
# common_config.py
from aladyn_config import GridConfig

STANDARD_GRID = GridConfig(
    nx=5000,
    ny=4200,
    k0=50.0,
)

# my_simulation.py
from common_config import STANDARD_GRID

config = ALaDynConfig(
    grid=STANDARD_GRID,
    # ... other parameters ...
)
```

### Programmatic Configuration

Generate multiple configurations programmatically:

```python
# Scan over laser intensities
for a0 in [1.0, 2.0, 3.0, 4.0]:
    config = ALaDynConfig(
        # ... parameters ...
        laser=LaserConfig(a0=a0, ...),
    )
    config_to_namelist(config, f"input_a0_{a0:.1f}.nml")
```

## Backward Compatibility

**The original Fortran namelist format still works!** You can continue using `.nml` files:

```fortran
&GRID
  nx = 5000,
  ny = 4200,
  ...
/
```

Both formats are supported, giving you flexibility in how you configure simulations.

## Migration from Namelist

To convert an existing `.nml` file to Python:

1. Look at the example files (`input_lwfa.py`, `input_pwfa.py`)
2. Copy the relevant section from your `.nml` file
3. Convert the Fortran syntax to Python:
   - `&SECTION` → `SectionConfig(`
   - `parameter = value,` → `parameter=value,`
   - `.true.` → `True`
   - `.false.` → `False`
   - `(1, 2, 3)` → `[1, 2, 3]`

## Troubleshooting

### Import Error

If you get `ModuleNotFoundError: No module named 'aladyn_config'`:

```python
# Add this at the top of your script
import sys
import os
sys.path.insert(0, '/path/to/ALaDyn/scripts')
```

### Invalid Configuration

If `config.validate()` fails, check the error message for details about which parameter is invalid.

### Comparing Output

To verify the generated namelist matches your expectations:

```bash
# Generate from Python
python3 my_config.py output.nml

# Compare with original
diff output.nml original.nml
```

## Getting Help

- Check the examples in `examples/`
- Read the docstrings: `help(GridConfig)`
- See the traditional namelist guide: `docs/pages/NAMELIST_GUIDE.md`
- Ask on the ALaDyn Telegram channel

## Summary

The Python input system makes ALaDyn more accessible by:

✅ Using familiar Python syntax  
✅ Providing automatic validation  
✅ Enabling custom plasma density functions  
✅ Supporting modern development tools  
✅ Maintaining full backward compatibility  

Start with the examples and adapt them to your needs!

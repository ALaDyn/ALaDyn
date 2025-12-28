# ALaDyn Python Input Scripts

This directory contains Python modules for configuring ALaDyn simulations using Python instead of traditional Fortran namelists.

## Files

- **`aladyn_config.py`**: Core configuration module with dataclasses for all simulation parameters
- **`config_to_namelist.py`**: Converter that generates Fortran namelist files from Python configurations

## Quick Usage

1. Create a Python configuration file (see `../examples/input_*.py` for examples)
2. Run it to generate a namelist:
   ```bash
   python3 my_config.py output.nml
   ```
3. Or use the converter directly:
   ```bash
   python3 config_to_namelist.py ../examples/input_lwfa.py ../input.nml
   ```

## Features

- **Type-safe configuration** with validation
- **Custom plasma density functions** using NumPy
- **Automatic parameter checking**
- **Full compatibility** with existing Fortran code
- **Human-readable configuration** with comments and structure

## Documentation

See `../docs/pages/PYTHON_INPUT_GUIDE.md` for complete documentation.

## Examples

- `../examples/input_lwfa.py` - Laser Wakefield Acceleration
- `../examples/input_pwfa.py` - Plasma Wakefield Acceleration
- `../examples/input_custom_density.py` - Custom plasma density profiles

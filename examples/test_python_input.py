#!/usr/bin/env python3
"""
Simple test script to validate the Python input management system.

This script tests the basic functionality of the ALaDyn Python configuration
system by generating namelist files and validating their structure.
"""

import sys
import os

# Add scripts directory to path
script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'scripts')
sys.path.insert(0, script_dir)

from aladyn_config import *
from config_to_namelist import config_to_namelist


def test_basic_config():
    """Test a minimal configuration."""
    print("Testing basic configuration...")
    
    config = ALaDynConfig(
        grid=GridConfig(nx=1000, ny=500, nz=1, k0=50.0),
        simulation=SimulationConfig(),
        target=TargetConfig(),
        laser=LaserConfig(),
        moving_window=MovingWindowConfig(),
        output=OutputConfig(tmax=10.0),
        mpi=MPIConfig(),
    )
    
    # Validate
    try:
        config.validate()
        print("✓ Basic configuration validated successfully")
    except Exception as e:
        print(f"✗ Validation failed: {e}")
        return False
    
    # Generate namelist
    try:
        import tempfile
        with tempfile.NamedTemporaryFile(mode='w', suffix='.nml', delete=False) as f:
            test_file = f.name
        
        config_to_namelist(config, test_file)
        print(f"✓ Namelist generated successfully: {test_file}")
        
        # Check file exists and has content
        if os.path.exists(test_file) and os.path.getsize(test_file) > 0:
            print("✓ Namelist file is valid")
            os.remove(test_file)
            return True
        else:
            print("✗ Namelist file is empty or missing")
            return False
    except Exception as e:
        print(f"✗ Namelist generation failed: {e}")
        return False


def test_validation():
    """Test parameter validation."""
    print("\nTesting parameter validation...")
    
    # Test invalid grid
    try:
        grid = GridConfig(nx=-100, ny=500, nz=1, k0=50.0)
        print("✗ Validation should have caught negative nx")
        return False
    except ValueError:
        print("✓ Correctly caught invalid grid parameter")
    
    # Test invalid model_id
    try:
        sim = SimulationConfig(model_id=99)
        print("✗ Validation should have caught invalid model_id")
        return False
    except ValueError:
        print("✓ Correctly caught invalid simulation parameter")
    
    return True


def test_lwfa_example():
    """Test loading the LWFA example."""
    print("\nTesting LWFA example...")
    
    try:
        # Import the LWFA example
        import importlib.util
        lwfa_path = os.path.join(os.path.dirname(__file__), 'input_lwfa.py')
        spec = importlib.util.spec_from_file_location("lwfa_config", lwfa_path)
        lwfa_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(lwfa_module)
        
        if hasattr(lwfa_module, 'config'):
            lwfa_module.config.validate()
            print("✓ LWFA example loaded and validated successfully")
            return True
        else:
            print("✗ LWFA example missing 'config' object")
            return False
    except Exception as e:
        print(f"✗ LWFA example failed: {e}")
        return False


def test_pwfa_example():
    """Test loading the PWFA example."""
    print("\nTesting PWFA example...")
    
    try:
        # Import the PWFA example
        import importlib.util
        pwfa_path = os.path.join(os.path.dirname(__file__), 'input_pwfa.py')
        spec = importlib.util.spec_from_file_location("pwfa_config", pwfa_path)
        pwfa_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(pwfa_module)
        
        if hasattr(pwfa_module, 'config'):
            pwfa_module.config.validate()
            print("✓ PWFA example loaded and validated successfully")
            return True
        else:
            print("✗ PWFA example missing 'config' object")
            return False
    except Exception as e:
        print(f"✗ PWFA example failed: {e}")
        return False


def main():
    """Run all tests."""
    print("=" * 60)
    print("ALaDyn Python Input Management System - Test Suite")
    print("=" * 60)
    
    tests = [
        test_basic_config,
        test_validation,
        test_lwfa_example,
        test_pwfa_example,
    ]
    
    results = []
    for test in tests:
        try:
            result = test()
            results.append(result)
        except Exception as e:
            print(f"✗ Test crashed: {e}")
            results.append(False)
    
    print("\n" + "=" * 60)
    passed = sum(results)
    total = len(results)
    print(f"Test Results: {passed}/{total} passed")
    
    if passed == total:
        print("✓ All tests passed!")
        return 0
    else:
        print("✗ Some tests failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())

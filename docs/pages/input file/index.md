title: Write an input file

# How to define the parameters for the simulations

Currently it is possible to provide the input parametrs either as a JSON file or as a namelist (`.nml`) file.

- [Here](./docs/input file/JSON_GUIDE.md) you can find a guide to fill a JSON file.
- [Here](./docs/input file/NAMELIST_GUIDE.md) you can find a guide to fill an namelist.

@note
We are currently switching to a full support of a JSON input file.
For this reason, it is still possible to provide an `input.nml` file,
but this feature is now **deprecated** and it will be dropped in a future
release.
@endnote

## Convert a namelist into a JSON file

Here is a quick way to directly convert an old namelist into an equivalent JSON file using python.
Before proceeding, please ensure that both the `json` and the `f90nml` packages are installed in your python distribution.

```python
import f90nml as nml
import json

nmlist = nml.read('input.nml')
jsondictionary = nmlist.todict()

with open('input.json', 'w') as fp:
    json.dump(jsondictionary, fp, indent=4)
```

## AGH Fortran project 3
### Run Python
``` bash
F77=gfortran
F90=gfortran
CC=gcc
f2py -c -m  src/matrix_lib.F90
```
Rename generated file in src to library.so
Then run testpy.py

### Build
You need opencoarrays, caf, cafrun in your PATH
```  bash
    make clean
    make all
```

Then run version you need from out/

### Docs

To generate documentation

``` bash
    doxygen DoxygenConfigFortran
```

### Plots
Plots are in jupyter nootebook files
They compare times of execution for sequentail and parallel versions
### Scripts
- test.sh - generates data for ploting


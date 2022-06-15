#!/bin/bash
fpm build --flag "-I/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/include" --link-flag "-L/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/lib64"

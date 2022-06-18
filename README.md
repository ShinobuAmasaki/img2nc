# img2nc

Convert planetary DEM[^1] data into NetCDF

For those who want to draw a topographic map of the Moon.



## Abstract

This software has the following roles to:

- Download the DEM data of **SLDEM2013**
- Read label files and Combine image data
- Output NetCDF files 

**SLDEM2013** is one of the high-resolutional lunar DEM and is able to be obtained from [SELENE Data Archive](https://darts.isas.jaxa.jp/planet/pdap/selene/index.html.en) provided by ISAS/JAXA[^2]. Check the license of the provider before acquiring the data, and comply with it when using the data.

This project managed by Fortran Package Manager (FPM) so we recommend to use it to build this software. 

This software depends on:

- GNU Fortran (gfortran)

- [**Unidata NetCDF Fortran Library**](https://www.unidata.ucar.edu/software/netcdf/).

- wget (to download DEM data)

[^1]: Digital Elevation Model（数値標高地図）
[^2]: [Institute of Space and Astronautical Science, Japan Aerospace Exploration Agency - 宇宙航空研究開発機構　宇宙科学研究所](https://www.isas.jaxa.jp/)

_____________

We have checked the operation on the following Linux OS:

- Ubuntu Desktop 22.04 LTS

- openSUSE Linux Tumbleweed

  

## Installation

1. Install NetCDF Fortran Library (cf. [the Documentation](https://docs.unidata.ucar.edu/netcdf-fortran/current/))
2. Build with `fpm`

```bash
$ git clone 
$ cd img2nc
$ fpm build --flag "-I/<netcdf-fortran-include-dir>" --link-flag "-L/<netcdf-fortran-library-dir>"
```

Then, you can find the executable file on `img2nc/build/gfortran_XXX/img2nc` 

#### e.g. on Ubuntu 22.04 LTS

We will show you the installation procedure in Ubuntu 22.04 LTS as an example.

``` bash
$ sudo apt install libnetcdff-dev gfortran
$ ldconfig -p | grep libnetcdff
		libnetcdff.so.7 (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so.7
        libnetcdff.so (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so
```

Memo: the library path is "`/lib/x86_64-linux-gnu`"

```bash
# Get fpm
$ wget https://github.com/fortran-lang/fpm/releases/download/v0.5.0/fpm-0.5.0-linux-x86_64
$ chmod +x fpm-0.5.0-linux-x86_64
# 	and make a symbolic link in a PATH dir.

# Build
$ fpm build --flag "-I/usr/include" --link-flag "-L/lib/x86_64-linux-gnu"
```



## Usage

As an example, let's draw  Lambert crater at 21 deg. west longitude and 26 deg. north latitude on the front side of the Moon.

#### Downloading DEM

First, determine the drawing range and download the DEM data. Specify the latitudes and longitudes of the coordinates as command line arguments when executing the shell script: `west/east/south/north`. 

The longitude is specified in the range of 0 to 360 with the eastern direction as the positive with respect to the center of the Moon. The Latitude is specified in the range of -90 to 90 with the north direction as positive and the south direction as negative based on the Moon equator[^3].

Here, execute the shell script `dl_sldem2013` with following command and arguments.

```
$ ./dl_sldem2013 -d ./dat 338/341/24/27
```

Specify the directory to download with `-d` option argument.

[^3]: It is not yet possible to specify a range that intersects the 0 deg. longitude line.

#### Execution

Execute the command `img2nc`.

```
$ img2nc -d ./dat -o out.nc -r 338/341/24/27
```

You can see how to use the optional arguments with `img2nc -h`.

After the execution, you will get the NetCDF file `out.nc`.

#### Drawing

You can draw a topographic map using `out.nc`, such as by GMT[^4].

```bash
#!/bin/bash
#This script depends on GMT6
dpi=100
gmt set PROJ_ELLIPSOID = Moon
gmt begin lambart png
	gmt basemap -JM8i -R338/341/24/27 -Bafg -BWeSn
	gmt makecpt -Cviridis -T-5000/1000/1000 -Z
	gmt grdgradient out.nc -Ggrad.grd -A310 -Ne0.6
	gmt grdimage out.nc -JM8i -C -E$dpi -Igrad.grd
gmt end
```

Executing this shell script, following figure `lambert.png` is outputted. See [GMT Documentation 6.3.0](https://docs.generic-mapping-tools.org/latest/)  on how to use GMT.

![lambart](https://user-images.githubusercontent.com/100006043/174430799-5b3f654a-1a47-48d0-ac9e-32976f05390c.png)

[^4]: Generic Mapping Tools: https://www.generic-mapping-tools.org/



## Future works

In the future, we would like to implement the following feature:

- Patch for Intel Fortran Compiler (ifort)

- Parallel processing


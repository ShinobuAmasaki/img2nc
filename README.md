# img2nc

Convert planetary DEM[^1] data into NetCDF

For those who want to draw a topographic map of the Moon.

![lambart](https://user-images.githubusercontent.com/100006043/174430799-5b3f654a-1a47-48d0-ac9e-32976f05390c.png)

![Mars-globe](https://github.com/ShinobuAmasaki/img2nc/blob/d64e171d2bb1ebde617e00c03fc34523bf31dc6e/mars-global.png?raw=true)

## Abstract
This software has the following roles to:

- Download the DEM data of **SLDEM2013**
- Read label files and Combine image data
- Output NetCDF files 

**SLDEM2013** is one of the high-resolutional lunar DEM and is able to be obtained from [SELENE Data Archive](https://darts.isas.jaxa.jp/planet/pdap/selene/index.html.en) provided by ISAS/JAXA[^2]. Check the license of the provider before acquiring the data, and comply with it when using the data.

This project managed by [**Fortran Package Manager (FPM)**](https://github.com/fortran-lang/fpm) so we recommend to use it to build this software. 

This software depends on:
- GNU Fortran (gfortran) or Intel Fortran Compiler (ifort, ifx)
- MPI Library (with ROMIO feature enabled.)
- [**Unidata NetCDF Fortran Library**](https://www.unidata.ucar.edu/software/netcdf/).
- wget (to download DEM data)

### Operation check
We have checked the operation on the following Linux OS:
- Ubuntu Desktop 22.04 LTS (gfortran/OpenMPI, Intel Fortran)
- Gentoo Linux (gfortran/OpenMPI)


## Installation
1. Install compiler, MPI library and FPM.

2. Install NetCDF Fortran Library (cf. [the Documentation](https://docs.unidata.ucar.edu/netcdf-fortran/current/))

	If we use gfortran, we may be able to use a package manager to install this. Using Intel Compiler, however, we have to build the NetCDF library ourself with it.

3. Download the tarball of this project.

   ```bash
	$ wget https://github.com/ShinobuAmasaki/img2nc/archive/refs/tags/v3.0.0.tar.gz
	$ tar xzf v3.0.0.tar.gz
	$ cd img2nc-3.0.0
	```

4. Build with `fpm`

	```bash
	$ fpm build --compiler mpif90 \
			 --flag "-I/<netcdf-fortran-include>" \
			 --link-flag "-L/<netcdf-fortran-lib-dir>"
	```

5. Install into any directory
   
	```bash
	$ fpm install --prefix <directory>
	```

Then, we can find the executable file on `<directory>/bin/img2nc` 

### e.g. on Ubuntu 22.04 LTS
We will look the installation procedure in Ubuntu 22.04 LTS as an example.

``` bash
$ sudo apt install gfortran openmpi-bin libnetcdff-dev

$ locate mpi_f08
/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi/mpi_f08.mod

$ ldconfig -p | grep libnetcdff
		libnetcdff.so.7 (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so.7
        libnetcdff.so (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so
```

Memo: the library path is "`/lib/x86_64-linux-gnu`" and the module directory path is "`/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi`".


```bash
# Get fpm
$ wget https://github.com/fortran-lang/fpm/releases/download/v0.5.0/fpm-0.5.0-linux-x86_64
$ chmod +x fpm-0.5.0-linux-x86_64
# 	and make a symbolic link in a PATH dir.

# Build
$ fpm build --compiler mpif90 \
	 --flag "-I/usr/include -I/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi" \
	 --link-flag "-L/lib/x86_64-linux-gnu"
```

## Usage - Moon: SLDEM2013
As an example, let's draw  Lambert crater at 21 deg. west longitude and 26 deg. north latitude on the front side of the Moon.

### Downloading DEM
First, determine the drawing range and download the DEM data. Specify the latitudes and longitudes of the coordinates as command line arguments when executing the shell script: `west/east/south/north`. 

The longitude is specified in the range of 0 to 360 with the eastern direction as the positive, or the range of -180 to 180 with the eastern direction as the positive and the western direction as negative with respect to the center of the Moon. The Latitude is specified in the range of -90 to 90 with the north direction as positive and the south direction as negative based on the Moon equator[^3].

Here, execute the shell script `dl_sldem2013` with following command and arguments.

```
$ ./dl_sldem2013 -d ./dat 338/341/24/27
```

Specify the directory to download with `-d` option argument.


### Execution
Execute the command `img2nc`.

```
$ img2nc -d ./dat -o out.nc -r 338/341/24/27
```

We can see how to use the optional arguments with `img2nc -h`.

After the execution, we will get the NetCDF file `out.nc`.

#### Parallel Processing
Execute `img2nc` with `mpiexec`.

```
$ mpiexec -n 9 img2nc -d ./dat -o out.nc -r 339/341/24/27
```

Specify the number of processor elements with `-n` or `-np` option which must be less than or equal to the product of longitude width and latitude width. For example in the above range specification, $(341 - 339) \times (27 - 24) = 3 \times 3 = 9$.


### Drawing
We can draw a topographic map using `out.nc`, such as by GMT[^4].

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

Executing this shell script, the beginning figure `lambert.png` is outputted.
See [GMT Documentation](https://docs.generic-mapping-tools.org/latest/) on how to use GMT.

## Usage - Mars: MOLA-MEGDR

Since version 3.1, support for Mars DEM has been introduced.
The basic usage is similar to that of lunar data, but in this case, the `img2nc-mola` command is used.

This section will guide you through the process of visualizing the entire globe of Mars in Mercator projection.

To download the DEM, you can use the `dl_mola-megdr.sh` script. The MOLA-MEGDR(meg128) dataset
divides the global map into 16 regions. As of version 3.1, the script allows downloading the entire dataset only:

```
$ ./dl_mola-megdr.sh -d mola-megdr
```

Execute the NetCDF file creation in parallel using 16 processes.
Note that the `-p` option allows you to specify the resolution in pixels per degree for latitude and longitude:

```
$ mpiexec -n 16 img2nc-mola -d mola-megdr -o mola.nc -p 16 -180/180/-88/88
```

By running the `mars-global-mercator.sh` script in the `script` directory, you can use GMT to generate an image in Mercator projection based on the previousely created `mola.nc` file:

```
./script/mars-global-mercator.sh
```

These steps will produce an image representing the entire Mars globe:

![Mars-globe](https://github.com/ShinobuAmasaki/img2nc/blob/d64e171d2bb1ebde617e00c03fc34523bf31dc6e/mars-global.png?raw=true)

## Future works
In the future, we would like to implement the following feature:

- ✅ Coarse vision
- ✅ Specifying Resolution per Degree
- ✅ Support Intel Fortran Compiler
- ✅ Parallel processing
- ✅ Parallel I/O (MPI I/O based)
- ✅ High Degree of Parallelization
- ✅ Negative Longitude
- ✅ Support MOLA data (Mars DEM)
- ✅ Trimming (for MOLA only)
- Asynchronous I/O

[^1]: Digital Elevation Model（数値標高地図）
[^2]: [Institute of Space and Astronautical Science, Japan Aerospace Exploration Agency - 宇宙航空研究開発機構　宇宙科学研究所](https://www.isas.jaxa.jp/)
[^3]: It is not yet possible to specify a range that intersects the 0 deg. longitude line.
[^4]: Generic Mapping Tools: https://www.generic-mapping-tools.org/

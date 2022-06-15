FC = gfortran
INCLUDE	=	-I/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/include
LDFLAGS	=	-L/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/lib64
LIBS	=	-lnetcdff
WORN	=	-Wall
FFLAGS	=	$(INCLUDE) $(LDFLAGS) $(LIBS) $(WORN)

img2nc:	read_lbl.o read_img.o img2nc.o main.f90
	$(FC) -o img2nc $(FFLAGS) read_lbl.o read_img.o img2nc.o main.f90

read_lbl.o: read_lbl.f90
	$(FC) -c $(WORN) read_lbl.f90

read_img.o: read_img.f90 read_lbl.o
	$(FC) -c $(WORN) read_img.f90

img2nc.o: img2nc.f90
	$(FC) -c $(FFLAGS) img2nc.f90

test_1: read_lbl.o read_img.o img2nc.o test_1.f90
	$(FC) -o a.out $(FFLAGS) read_lbl.o read_img.o img2nc.o test_1.f90

four_tiles: read_lbl.o read_img.o img2nc.o four_tiles.f90
	$(FC) -o a.out $(FFLAGS) read_lbl.o read_img.o img2nc.o four_tiles.f90

clean:
	rm -rf *.o *.mod a.out img2nc

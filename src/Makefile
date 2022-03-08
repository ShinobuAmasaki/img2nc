FC = gfortran
INCLUDE	=	-I/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/include
LDFLAGS	=	-L/usr/lib/hpc/gnu11/netcdf-fortran/4.5.3/lib64
LIBS	=	-lnetcdff
WORN	=	-Wall
FFLAGS	=	$(INCLUDE) $(LDFLAGS) $(LIBS) $(WORN)

read_img.o: read_img.f90
	$(FC) -c $(WORN) read_img.f90

string_operation.o: string_operation.f90
	$(FC) -c $(WORN) string_operation.f90

img2nc.o: img2nc.f90
	$(FC) -c $(FFLAGS) img2nc.f90

main:	read_img.o string_operation.o img2nc.o main.f90
	$(FC) -o a.out $(FFLAGS) read_img.o string_operation.o img2nc.o main.f90

clean:
	rm -rf *.o *.mod
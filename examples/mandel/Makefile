.SUFFIXES:
.SUFFIXES: .o .f90

FC = mpxlf90_r
FFLAGS = -O -g
FINCLUDE = -I/usr/local/include -I/usr/local/lib   -L/usr/local/lib -lnetcdf

OBJ = mandel2D.o madelio.o

mandel: $(OBJ)
	$(FC) $(FINCLUDE) $(FFLAGS)  $(OBJ) -o $@

.f90.o:
	$(FC) -c $(FINCLUDE) $(FFLAGS) $*.f90

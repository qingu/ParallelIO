.SUFFIXES:
.SUFFIXES: .o .f90

include ../../pio/Makefile.conf


OBJ = mandel2D.o mandelio.o

mandel: $(OBJ)
	$(MPIFC) $(FOPTS) $(INCLUDES) $(FFLAGS)  $(OBJ) -o $@ -L$(PIO_PATH) -lpio -L$(PIO_PATH)/../timing $(LDLIBS) 

.f90.o:
	$(MPIFC) -c $(INCLUDES) $(FOPTS) $(FFLAGS) -I$(PIO_PATH) $*.f90


mandel2D.o: mandelio.o 

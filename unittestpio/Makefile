# Makefile for PIO unit tests

include ../pio/Makefile.conf

SRCS_FC = driver.F90      \
          global_vars.F90 \
          basic_tests.F90

OUTFILES = piotest_bin.ieee   \
           piotest_bin2.ieee  \
           piotest_netcdf.nc  \
           piotest_pnetcdf.nc

# Dependencies
DEPSUF = .d
DEPENDS := $(SRCS_FC:.F90=$(DEPSUF))
ifneq ($(INCLUDE_DEPENDS),)
  include $(DEPENDS)
endif
FDEPENDS = ../pio/fdepends.awk


# mod and object files
MODSUF = .mod
MODFILES = $(SRCS_FC:.F90=$(MODSUF))
OBJS = $(SRCS_FC:.F90=.o)
EXE = piotest

all: depends
	@$(MAKE) $(EXE) INCLUDE_DEPENDS=TRUE

depends: $(DEPENDS)
	@echo "Done updating dependencies"

$(EXE): $(OBJS)
	$(MPIFC) -o $(EXE) $(OBJS) -L../pio -lpio $(LDLIBS)

.SUFFIXES: .o .F90 $(DEPSUF)

%$(DEPSUF): %.F90
	@echo 'Making dependencies for' $< '-->' $@
	@$(AWK) -f $(FDEPENDS) -v NAME=$(basename $<) -v SUF=$(suffix $<) $< > $@

%.o: %.F90
	$(MPIFC) $(FFLAGS) -c $< -I../pio $(CPPDEFS)

clean:
	$(RM) $(EXE) $(OBJS) $(MODFILES) $(DEPENDS) $(OUTFILES)

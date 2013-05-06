# Makefile for PIO unit tests

include ../pio/Makefile.conf

SRCS_FC = driver.F90 \
          netcdf_test.F90

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
	$(MPIFC) -o $(EXE) $(OBJS) -L../pio -lpio

.SUFFIXES: .o .F90 $(DEPSUF)

%$(DEPSUF): %.F90
	@echo 'Making dependencies for' $< '-->' $@
	@$(AWK) -f $(FDEPENDS) -v NAME=$(basename $<) -v SUF=$(suffix $<) $< > $@

%.o: %.F90
	$(MPIFC) -c $< -I../pio

clean:
	$(RM) $(EXE) $(OBJS) $(MODFILES) $(DEPENDS)

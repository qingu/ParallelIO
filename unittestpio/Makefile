# Makefile for PIO unit tests

include ../pio/Makefile.conf
DEPSUF = .d

SRCS_FC = driver.F90

DEPENDS := $(SRCS_FC:.F90=$(DEPSUF))
OBJS = $(SRCS_FC:.F90=.o)
EXE = piotest

all: depends
	@$(MAKE) $(EXE)

depends: $(DEPENDS)
	@echo "Done updating dependencies"

$(EXE): $(OBJS)
	$(MPIFC) -o $(EXE) $(OBJS)

.SUFFIXES: .o .F90 $(DEPSUF)

%$(DEPSUF): %.F90
	@echo 'Making dependencies for' $< '-->' $@
	@$(AWK) -f $(DEPENDS) -v NAME=$(basename $<) -v SUF=$(suffix $<) $< > $@

.F90.o:
	$(MPIFC) -c $<

clean:
	$(RM) $(EXE) $(OBJS) $(DEPENDS)

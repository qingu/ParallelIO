#
# Defined externally (Makefile.conf and Makefile.arch)
#
#   INCLUDES
#   LIBS
#   MPICC
#   MPIF90
#   COPTS
#   FOPTS
#   CFLAGS
#   FFLAGS
#   AWK
#   AR
#

@TOPBILDDIR@
@TOPSRCDIR@

RM=/bin/rm -f
FDEPENDS=$(TOPSOURCEDIR)/pio/fdepends.awk
DEPSUF = .d
MODSUF = .mod
CPPSUF = .f90


if PIOARCH_DEFAULT
  PIOARCH=conf
endif
include $(TOPBUILDDIR)/Makefile.$(PIOARCH)

SRCS_C=
SRCS_F90= kinds_mod.F90 \
          check_mod.F90 \
          utils_mod.F90 \
          gdecomp_mod.F90 \
	  namelist_mod.F90 \
          testpio.F90

SRCS2_C=
SRCS2_F90= kinds_mod.F90 \
           gdecomp_mod.F90 \
           namelist_mod.F90 \
           utils_mod.F90 \
	   testfiletype.F90 

SRCS3_F90= tst_pio.F90 \
	   kinds_mod.F90 \
           namelist_mod.F90 \
           utils_mod.F90

SRCS3_C=

@SRCS4@

OBJS=  $(SRCS_C:.c=.o)       \
       $(SRCS_F90:.F90=.o)

OBJS2=  $(SRCS2_C:.c=.o)     \
       $(SRCS2_F90:.F90=.o)

OBJS3=  $(SRCS3_C:.c=.o)     \
       $(SRCS3_F90:.F90=.o)

@OBJS4@

# File foo.DEPSUF will contain make dependency for foo.F90
@DEPENDS@


#
#
#

EXE=testpio
EXE2=testfiletype
EXE3=tst_pio

LEVEL=0

default: depends
	$(MAKE) -f Makefile LEVEL=1 $(EXE)

all: depends
	$(MAKE) -f Makefile LEVEL=1 $(EXE)
#	$(MAKE) -f Makefile LEVEL=1 $(EXE2)
	$(MAKE) -f Makefile LEVEL=1 $(EXE3)
	@BUILDEXE4@

depends: $(DEPENDS)
	@echo "Done updating dependencies"

PIODIR = $(TOPBUILDDIR)/pio
PIOINC = -I$(PIODIR)
# Optionally include timing libraries and directory in lib and include flags
@TIMINGLIBS@
PIOLIBA = $(PIODIR)/libpio.a

$(EXE): $(OBJS) $(PIOLIBA)
	$(MPIFC) $(FFLAGS) -o $@ $(OBJS) $(PIOLIB) $(LDLIBS)

$(EXE2): $(OBJS2) $(PIOLIBA)
	$(MPIFC) $(FFLAGS) -o $@ $(OBJS2) $(PIOLIB) $(LDLIBS)

$(EXE3): $(OBJS3) $(PIOLIBA)
	$(MPIFC) $(FFLAGS) -o $@ $(OBJS3) $(PIOLIB) $(LDLIBS)

@EXE4@

#
# Suffix rules
#

@SUFFIXRULES@

# For each file foo.F90, make the depency file foo.d
@MKDEPEND@

if EXPLICITCPP
SRCS_CPP= $(SRCS_F90:.F90=$(CPPSUF))
.F90.o:
	@if [ -w $*.f90 ] ; then echo "ERROR: file $*.f90 is writable - the .f90 suffix is reserved for temporary cpp output" ; exit 1; fi
	$(RM) $*.f90
	$(CPP) $(CPPFLAGS) $(CFLAGS) $(COPTS) $(INCLUDES) -o $*.f90 $*.F90 
	chmod a-w $*.f90
	$(MPIFC) -c $(FFLAGS) $(FOPTS) $(INCLUDES) $(PIOINC) $*.f90
else
.F90.o:
	$(MPIFC) -c $(FPPDEFS) $(FFLAGS) $(FOPTS) $(INCLUDES) $(PIOINC) $*.F90
endif


.c.o:
	$(MPICC) -c $(CFLAGS) $(COPTS) $(INCLUDES) $(PIOINC) $*.c


TESTDATA= foo.binr foo.i4.nc foo.r4.binr foo.r8.bin  foo.r8.nc \
          foo.i4.binr  foo.nc  foo.r4.nc foo.r8.binr

clean:
	$(RM) $(EXE) $(EXE2) $(EXE3) $(EXE4)
	$(RM) $(OBJS) $(OBJS2) $(OBJS3) $(OBJS4) *.mod 
	$(RM) $(DEPENDS) $(TESTDATA) $(SRCS_CPP)


#
# Automatically generated module dependencies
#

@INCLUDEDEPENDS@


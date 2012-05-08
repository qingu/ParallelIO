#
# Defined externally in Makefile.conf
#
#   INCLUDES
#   LIBS
#   MPICC
#   MPIFC
#   COPTS
#   FOPTS
#   CFLAGS
#   FFLAGS
#   AWK
#   AR

@TOPBILDDIR@
@TOPSRCDIR@

RM=/bin/rm -f
FDEPENDS=$(TOPSOURCEDIR)/pio/fdepends.awk
DEPSUF = .d
MODSUF = .mod

if PIOARCH_DEFAULT
  PIOARCH=conf
endif
include $(TOPBUILDDIR)/Makefile.$(PIOARCH)
export PIOARCH

SRCS_FC = pio_cpp_binding.F90
#pio_kinds.F90        \
#          box_rearrange.F90    \
#          calcdecomp.F90       \
#          calcdisplace_mod.F90 \
#          iompi_mod.F90        \
#          ionf_mod.F90         \
#          nf_mod.F90           \
#          alloc_mod.F90        \
#          pio_mpi_utils.F90    \
#          pio_msg_mod.F90      \
#          pio_support.F90      \
#          pio_types.F90        \
#          pio_utils.F90        \
#          piolib_mod.F90       \
#          pionfread_mod.F90    \
#          pionfwrite_mod.F90   \
#          rearrange.F90        


TEMPLATES_FC = 
#pio_sizes.F90.in

TEMPSRCFC = $(TEMPLATES_FC:.in=)
SRCS_FC += $(TEMPSRCFC)

PIOBUILDDIR = $(TOPBUILDDIR)/pio
PIOSOURCEDIR = $(TOPSOURCEDIR)/pio

# This probably needs to be a pointer to actual modules
PIOMODS = $(PIOBUILDDIR)/libpio.a

VPATH = $(PIOBUILDDIR)

OBJS = $(SRCS_FC:.F90=.o)

@MODFILES@

# File foo.DEPSUF will contain make dependency for foo.F90
# This depends on SRCS_FC so has to come after
#@DEPENDS@

# Include pio subdirectory
@ADDPIOINC@

LIB= libpiocpp.a

LDPATH = -L$(builddir) -L$(TOPBUILDDIR)/pio
MPILIBS = -lmpi_f90 -lmpi
NETCDFLIBS = -lnetcdf -lpnetcdf -lhdf5
LDOPTS = $(LDPATH)  -lpiocpp -lpio $(MPILIBS) $(NETCDFLIBS) -lgfortran

LEVEL=0

#
# Suffix rules
#

@SUFFIXRULES@

# For each file foo.F90, make the depency file foo.d
@MKDEPEND@

%.o: %.F90
	$(MPIFC) -c $(FPPDEFS) $(FCFLAGS) $(FOPTS) $(INCLUDES) $<

%.o: %.cpp
	$(MPICXX) -c $(CPPDEFS) $(CXXFLAGS) $(COPTS) $(INCLUDES) $<

$(TEMPSRCFC): $(TEMPLATE_FC)
	$(PERL) $(PIOSOURCEDIR)/genf90.pl $< > $*.F90

all: depends
	@$(MAKE) LEVEL=1 $(LIB)


depends: $(DEPENDS)
	@echo "Done updating dependencies"

$(LIB): $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)

test: testpio_cpp

testpio_cpp: testpio_cpp.o namelist.o
	$(MPICXX) $(CXXFLAGS) $(COPTS) $(LDLIBS) -o $@ $<

clean:
	$(RM) $(LIB) $(OBJS) $(MODFILES) $(DEPENDS) $(SRCS_CPP)

predistclean: clean

predist: predistclean

##
## Testing
##

# check_kinds depends on pio_kinds.mod
CHECK_KINDS_SRC = check_kinds.F90

# size_of_types depends on pio_types.mod
SIZE_OF_TYPES_SRC = size_of_types.F90

##
## Automatically generated module dependencies
##

@INCLUDEDEPENDS@

##
## Explicit Dependencies
##
#pio_kinds.o : pio_kinds.F90
#box_rearrange.o : box_rearrange.F90
#calcdecomp.o : calcdecomp.F90
#calcdisplace_mod.o : calcdisplace_mod.F90
#iompi_mod.o : iompi_mod.F90
#ionf_mod.o : ionf_mod.F90
#nf_mod.o : nf_mod.F90
#alloc_mod.o : alloc_mod.F90
#pio_mpi_utils.o : pio_mpi_utils.F90
#pio_msg_mod.o : pio_msg_mod.F90
#pio_support.o : pio_support.F90
#pio_types.o : pio_types.F90
#pio_utils.o : pio_utils.F90
#piolib_mod.o : piolib_mod.F90
#pionfread_mod.o : pionfread_mod.F90
#pionfwrite_mod.o : pionfwrite_mod.F90
#rearrange.o : rearrange.F90
pio_cpp_binding.o : pio_cpp_binding.F90
pio_cpp_binding.o : $(PIOBUILDDIR)/pio_kinds.mod
pio_cpp_binding.o : $(PIOBUILDDIR)/pio_types.mod
pio_cpp_binding.o : $(PIOBUILDDIR)/piolib_mod.mod

testpio_cpp.o: testpio_cpp.cpp pio.h $(LIB)
testpio_cpp.o: kinds.h
testpio_cpp.o: namelist.o namelist.o
#testpio_cpp.o:

#pio_sizes.o : pio_sizes.F90.in

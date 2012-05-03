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

if PIOARCH_DEFAULT
  PIOARCH=conf
endif
include ../Makefile.$(PIOARCH)
export PIOARCH

SRCS_FC = pio_kinds.F90        \
          box_rearrange.F90    \
          calcdecomp.F90       \
          calcdisplace_mod.F90 \
          iompi_mod.F90        \
          ionf_mod.F90         \
          nf_mod.F90           \
          alloc_mod.F90        \
          pio_mpi_utils.F90    \
          pio_msg_mod.F90      \
          pio_support.F90      \
          pio_types.F90        \
          pio_utils.F90        \
          piolib_mod.F90       \
          pionfread_mod.F90    \
          pionfwrite_mod.F90   \
          rearrange.F90        \
          pio_cpp_binding.F90

TEMPLATES_FC = pio_sizes.F90.in

TEMPSRCFC = $(TEMPLATES_FC:.in=)
SRCS_FC += $(TEMPSRCFC)

PIODIR = $(SRCDIR)/pio

# This could probably use sprucing up
PIOMODS = $(PIODIR)/libpio.a

VPATH = $(PIODIR)

OBJS = $(SRCS_FC:.F90=.o)

@MODFILES@

# check_kinds depends on pio_kinds.mod
CHECK_KINDS_SRC = check_kinds.F90

# size_of_types depends on pio_types.mod
SIZE_OF_TYPES_SRC = size_of_types.F90

# Include pio subdirectory
@ADDPIOINC@

LIB= libpiocpp.a

LEVEL=0

#
# Suffix rules
#

SUFFIXES = .o .cpp .F90 $(DEPSUF)

.F90.o:
	$(MPIFC) -c $(FPPDEFS) $(FCFLAGS) $(FOPTS) $(INCLUDES) $<

.cpp.o:
	$(MPICXX) -c $(CPPDEFS) $(CXXFLAGS) $(COPTS) $(INCLUDES) $<

$(TEMPSRCFC): $(TEMPLATE_FC)
	$(PERL) $(SRCDIR)/pio/genf90.pl $< > $*.F90

all: depends
	@$(MAKE) LEVEL=1 $(LIB)


depends: $(PIOMODS)
	@echo "Done updating dependencies"

$(LIB): $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODFILES) $(DEPENDS) $(SRCS_CPP)

predistclean: clean

predist: predistclean

# Dependencies
pio_kinds.o : pio_kinds.F90
box_rearrange.o : box_rearrange.F90
calcdecomp.o : calcdecomp.F90
calcdisplace_mod.o : calcdisplace_mod.F90
iompi_mod.o : iompi_mod.F90
ionf_mod.o : ionf_mod.F90
nf_mod.o : nf_mod.F90
alloc_mod.o : alloc_mod.F90
pio_mpi_utils.o : pio_mpi_utils.F90
pio_msg_mod.o : pio_msg_mod.F90
pio_support.o : pio_support.F90
pio_types.o : pio_types.F90
pio_utils.o : pio_utils.F90
piolib_mod.o : piolib_mod.F90
pionfread_mod.o : pionfread_mod.F90
pionfwrite_mod.o : pionfwrite_mod.F90
rearrange.o : rearrange.F90
pio_cpp_binding.o : pio_cpp_binding.F90

pio_sizes.o : pio_sizes.F90.in

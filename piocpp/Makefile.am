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

SRCS = alloc_mod.f90        \
       box_rearrange.f90    \
       calcdecomp.f90       \
       calcdisplace_mod.f90 \
       iompi_mod.f90        \
       ionf_mod.f90         \
       nf_mod.f90           \
       pio_cpp_binding.f90  \
       pio_kinds.f90        \
       pio_mpi_utils.f90    \
       pio_msg_mod.f90      \
       pio_sizes.f90        \
       pio_support.f90      \
       pio_types.f90        \
       pio_utils.f90        \
       piolib_mod.f90       \
       pionfread_mod.f90    \
       pionfwrite_mod.f90   \
       rearrange.f90

OBJS = $(SRCSC:.f90=.o

# check_kinds depends on pio_kinds.mod
CHECK_KINDS_SRC = check_kinds.f90

# size_of_types depends on pio_types.mod
SIZE_OF_TYPES_SRC = size_of_types.f90


LIB= libpiocpp.a

LEVEL=0

all: depends
	@$(MAKE) LEVEL=1 $(LIB)


depends: $(DEPENDS)
	@echo "Done updating dependencies"

$(LIB): $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)

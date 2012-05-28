#define __PIO_FILE__ "pio.cpp"
// ---------------------------------------------------------------------

//  procedures for translating MPI communicators before calling into Fortran
#include <mpi.h>
#include "pio.h"


// ---------------------------------------------------------------------
// the real PIO_init function prototypes
// ---------------------------------------------------------------------

extern "C" {

// subroutine pio_cpp_init_intracom(comp_rank, comp_comm, num_iotasks, num_aggregator, stride, rearr, iosystem, base) bind(c)

void pio_cpp_init_intracom_int(int comp_rank,
                               int comp_comm,
                               int num_tasks,
                               int num_aggregator,
                               int stride,
                               int rearr,
                               iosystem_desc_t iosystem,
                               int base);

// subroutine pio_cpp_init_intercom(component_count, peer_comm, comp_comms, io_comm, iosystem) bind(c)

void pio_cpp_init_intercom_int(int component_count,
                               int peer_comm,
                               int *comp_comms,
                               int io_comm,
                               iosystem_desc_t *iosystem);
} // extern "C"

void pio_cpp_init_intracom(int comp_rank,
                           MPI_Comm comp_comm,
                           int num_tasks,
                           int num_aggregator,
                           int stride,
                           int rearr,
                           iosystem_desc_t iosystem,
                           int base) {
  pio_cpp_init_intracom_int(comp_rank,
                            MPI_Comm_c2f(comp_comm),
                            num_tasks,
                            num_aggregator,
                            stride,
                            rearr,
                            iosystem,
                            base);
}

void pio_cpp_init_intercom(int component_count,
                           MPI_Comm peer_comm,
                           MPI_Comm* comp_comms,
                           MPI_Comm io_comm,
                           iosystem_desc_t *iosystems) {
  int *int_comp_comms;

  int_comp_comms = (int *)malloc(sizeof(int) * component_count);
  if (int_comp_comms != (int *)NULL) {
    for (int i = 0; i < component_count; i++) {
      int_comp_comms[i] = MPI_Comm_c2f(comp_comms[i]);
    }
    pio_cpp_init_intercom_int(component_count,
                              MPI_Comm_c2f(peer_comm),
                              int_comp_comms,
                              MPI_Comm_c2f(io_comm),
                              iosystems);
  }
}

// Initialize PIOSYSTEM_DESC_NULL
// NB: This should be the only place outside of pio_kinds.h which knows
//     that an iossytem_desc_t is a pointer to an integer
static int nullint = -1;
const iosystem_desc_t PIOSYSTEM_DESC_NULL = &nullint;

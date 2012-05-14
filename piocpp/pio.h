#ifndef __PIO_H_INCLUDED_
#define __PIO_H_INCLUDED_

#include "pio_kinds.h"

// ---------------------------------------------------------------------
// the pio function prototypes
// ---------------------------------------------------------------------

extern "C" {

// subroutine pio_cpp_init_intracom(comp_rank, comp_comm, num_iotasks, num_aggregator, stride, rearr, iosystem, base) bind(c)

void pio_cpp_init_intracom(int comp_rank,
                            int comp_comm,
                            int num_tasks,
                            int num_aggregator,
                            int stride,
                            int rearr,
                            iosystem_desc_t iosystem,
                            int base);

// subroutine pio_cpp_init_intercom(component_count, peer_comm, comp_comms, io_comm, iosystem) bind(c)

void pio_cpp_init_intercom(int component_count,
                            int peer_comm,
                            int* comp_comms,
                            int io_comm,
                            iosystem_desc_t *iosystem);

// subroutine pio_cpp_finalize(iosystem, ierr) bind(c)

void pio_cpp_finalize(iosystem_desc_t iosystem,
                       int* ierror);

// subroutine pio_cpp_initdecomp_dof_i8(iosystem, basepiotype, dims, ndims, compdof, ncompdof, &
//                                       iodesc, iostart, niostart, iocount, niocount) bind(c)

void pio_cpp_initdecomp_dof_i8(iosystem_desc_t iosystem,
                                int basepiotype,
                                int* dims,
                                int ndims,
                                int* compdof,
                                int ncompdof,
                                io_desc_t iodesc,
                                int* iostart,
                                int niostart,
                                int* iocount,
                                int niocount);

// function pio_cpp_openfile(iosystem, file, iotype, fname, mode) result(ierr) bind(c)

int pio_cpp_openfile(iosystem_desc_t iosystem,
                      file_desc_t file,
                      int iotype,
                      char* fname,
                      int mode);

// subroutine pio_cpp_syncfile(file) bind(c)

void pio_cpp_syncfile(file_desc_t file);

// function pio_cpp_createfile(iosystem, file, iotype, fname, amode_in) result(ierr) bind(c)

int pio_cpp_createfile(iosystem_desc_t iosystem,
                        file_desc_t file,
                        int iotype,
                        char* fname,
                        int amode_in);

// subroutine pio_cpp_closefile(file) bind(c)

void pio_cpp_closefile(file_desc_t file);

// subroutine pio_cpp_setiotype(file, iotype, rearr) bind(c)

void pio_cpp_setiotype(file_desc_t file,
                        int iotype,
                        int rearr);

// function pio_cpp_numtoread(iodesc) result(num) bind(c)

int pio_cpp_numtoread(io_desc_t iodesc);

// function pio_cpp_numtowrite(iodesc) result(num) bind(c)

int pio_cpp_numtowrite(io_desc_t iodesc);

// subroutine pio_cpp_setframe(vardesc, frame) bind(c)

void pio_cpp_setframe(var_desc_t vardesc,
                       int frame);

// subroutine pio_cpp_advanceframe(vardesc) bind(c)

void pio_cpp_advanceframe(var_desc_t vardesc);

// subroutine pio_cpp_setdebuglevel(level) bind(c)

void pio_cpp_setdebuglevel(int level);

// subroutine pio_cpp_seterrorhandlingf(file, method) bind(c)

void pio_cpp_seterrorhandlingf(file_desc_t file,
                                int method);

// subroutine pio_cpp_seterrorhandlingi(ios, method) bind(c)

void pio_cpp_seterrorhandlingi(void* ios,
                                int method);

// function pio_cpp_get_local_array_size(iodesc) result(siz) bind(c)

int pio_cpp_get_local_array_size(io_desc_t iodesc);

// subroutine pio_cpp_freedecomp_ios(ios_handle, iodesc) bind(c)

void pio_cpp_freedecomp_ios(void* ios,
                             io_desc_t iodesc);

// subroutine pio_cpp_freedecomp_file(file, iodesc) bind(c)

void pio_cpp_freedecomp_file(file_desc_t file,
                              io_desc_t iodesc);

// subroutine pio_cpp_dupiodesc(src, dest) bind(c)

void pio_cpp_dupiodesc(void* src,
                        void* dest);

// subroutine pio_cpp_getnumiotasks(iosystem, numiotasks) bind(c)

void pio_cpp_getnumiotasks(iosystem_desc_t iosystem,
                            int* numiotasks);

// subroutine pio_cpp_set_hint(iosystem, hint, hintval) bind(c)

void pio_cpp_set_hint(iosystem_desc_t iosystem,
                       void* hint,
                       void* hint_val);

// function pio_cpp_getnum_ost(iosystem) result(numost) bind(c)

int pio_cpp_getnum_ost(iosystem_desc_t iosystem);

// subroutine pio_cpp_setnum_ost(iosystem, numost) bind(c)

void pio_cpp_setnum_ost(iosystem_desc_t iosystem,
                         int numost);

// function pio_cpp_file_is_open(file) result(is_open) bind(c)

int pio_cpp_file_is_open(file_desc_t file);

} // extern "C"
// ---------------------------------------------------------------------

#endif // __PIO_H_INCLUDED_

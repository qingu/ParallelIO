#ifndef __PIO_H_INCLUDED_
#define __PIO_H_INCLUDED_

#include <mpi.h>
#include "pio_kinds.h"

// ---------------------------------------------------------------------
// the pio function prototypes
// ---------------------------------------------------------------------

extern "C" {

/////////////////////////////////
//
//  PIOlib Interface Functions
//
/////////////////////////////////

// subroutine pio_cpp_init_intracom(comp_rank, comp_comm, num_iotasks,        &
//                                  num_aggregator, stride, rearr, iosystem,  &
//                                  base) bind(c)

void pio_cpp_init_intracom(int comp_rank,
                           MPI_Comm comp_comm,
                           int num_tasks,
                           int num_aggregator,
                           int stride,
                           int rearr,
                           pio_iosystem_desc_t *iosystem,
                           int base);

// subroutine pio_cpp_init_intercom(component_count, peer_comm, comp_comms,  &
//                                  io_comm, iosystem) bind(c)

void pio_cpp_init_intercom(int component_count,
                           MPI_Comm peer_comm,
                           MPI_Comm* comp_comms,
                           MPI_Comm io_comm,
                           pio_iosystem_desc_t **iosystems);

// subroutine pio_cpp_finalize(iosystem, ierr) bind(c)

void pio_cpp_finalize(pio_iosystem_desc_t *iosystem,
                       int* ierror);

// We present two versions of this routine in order to allow the C
// programmer to leave out the optional arguments
// We use would like to use MPI_Offset for the size of arrays since PIO_OFFSET
// is of type MPI_Offset. However, in order to use the C interoperability
// features, we are limited so we choose the 64-bit integer version

// subroutine pio_cpp_initdecomp_dof_io(iosystem, basepiotype, dims, ndims,   &
//                                      compdof, ncompdof, iodesc, iostart,   &
//                                      niostart, iocount, niocount) bind(c)

void pio_cpp_initdecomp_dof_io(pio_iosystem_desc_t *iosystem,
                               int basepiotype,
                               int* dims, int ndims,
                               int64_t* compdof, int ncompdof,
                               pio_io_desc_t iodesc,
                               int64_t* iostart, int niostart,
                               int64_t* iocount, int niocount);

// subroutine pio_cpp_initdecomp_dof(iosystem, basepiotype, dims, ndims,      &
//                                      compdof, ncompdof, iodesc) bind(c)

void pio_cpp_initdecomp_dof(pio_iosystem_desc_t *iosystem,
                            int basepiotype,
                            int* dims, int ndims,
                            int64_t* compdof, int ncompdof,
                            pio_io_desc_t iodesc);

#if 0
// This routine is not yet implemented
// subroutine pio_cpp_initdecomp_dof_dof(iosystem, basepiotype, dims, ndims,  &
//                                       compdof, ncompdof, iodesc,           &
//                                       iodof, niodof) bind(c)

void pio_cpp_initdecomp_dof_dof(pio_iosystem_desc_t *iosystem,
                                int basepiotype,
                                int* dims, int ndims,
                                int* compdof, int ncompdof,
                                pio_io_desc_t iodesc,
                                int* iodof, int niodof);
#endif // Not implemented

// function pio_cpp_openfile(iosystem, file, iotype, fname, mode)             &
//          result(ierr) bind(c)

int pio_cpp_openfile(pio_iosystem_desc_t *iosystem,
                     pio_file_desc_t file,
                     int iotype,
                     const char *fname,
                     int mode);

// subroutine pio_cpp_syncfile(file) bind(c)

void pio_cpp_syncfile(pio_file_desc_t file);

// function pio_cpp_createfile(iosystem, file, iotype, fname, amode_in)       &
//          result(ierr) bind(c)

int pio_cpp_createfile(pio_iosystem_desc_t *iosystem,
                       pio_file_desc_t file,
                       int iotype,
                       const char *fname,
                       int amode_in);

// subroutine pio_cpp_closefile(file) bind(c)

void pio_cpp_closefile(pio_file_desc_t file);

// subroutine pio_cpp_setiotype(file, iotype, rearr) bind(c)

void pio_cpp_setiotype(pio_file_desc_t file,
                       int iotype,
                       int rearr);

// function pio_cpp_numtoread(iodesc) result(num) bind(c)

int pio_cpp_numtoread(pio_io_desc_t iodesc);

// function pio_cpp_numtowrite(iodesc) result(num) bind(c)

int pio_cpp_numtowrite(pio_io_desc_t iodesc);

// subroutine pio_cpp_setframe(vardesc, frame) bind(c)

void pio_cpp_setframe(pio_var_desc_t vardesc,
                      int frame);

// subroutine pio_cpp_advanceframe(vardesc) bind(c)

void pio_cpp_advanceframe(pio_var_desc_t vardesc);

// subroutine pio_cpp_setdebuglevel(level) bind(c)

void pio_cpp_setdebuglevel(int level);

// subroutine pio_cpp_seterrorhandlingf(file, method) bind(c)

void pio_cpp_seterrorhandlingf(pio_file_desc_t file,
                               int method);

// subroutine pio_cpp_seterrorhandlingi(ios, method) bind(c)

void pio_cpp_seterrorhandlingi(void* ios,
                               int method);

// function pio_cpp_get_local_array_size(iodesc) result(siz) bind(c)

int pio_cpp_get_local_array_size(pio_io_desc_t iodesc);

// subroutine pio_cpp_freedecomp_ios(ios_handle, iodesc) bind(c)

void pio_cpp_freedecomp_ios(void* ios,
                            pio_io_desc_t iodesc);

// subroutine pio_cpp_freedecomp_file(file, iodesc) bind(c)

void pio_cpp_freedecomp_file(pio_file_desc_t file,
                             pio_io_desc_t iodesc);

// subroutine pio_cpp_dupiodesc(src, dest) bind(c)

void pio_cpp_dupiodesc(void* src,
                       void* dest);

// subroutine pio_cpp_getnumiotasks(iosystem, numiotasks) bind(c)

void pio_cpp_getnumiotasks(pio_iosystem_desc_t *iosystem,
                           int* numiotasks);

// subroutine pio_cpp_set_hint(iosystem, hint, hintval) bind(c)

void pio_cpp_set_hint(pio_iosystem_desc_t *iosystem,
                      void* hint,
                      void* hint_val);

// function pio_cpp_getnum_ost(iosystem) result(numost) bind(c)

int pio_cpp_getnum_ost(pio_iosystem_desc_t *iosystem);

// subroutine pio_cpp_setnum_ost(iosystem, numost) bind(c)

void pio_cpp_setnum_ost(pio_iosystem_desc_t *iosystem,
                        int numost);

// function pio_cpp_file_is_open(file) result(is_open) bind(c)

int pio_cpp_file_is_open(pio_file_desc_t file);

/////////////////////////////////
//
//  NetCDF Interface Functions
//
/////////////////////////////////

// function pio_cpp_inquire(File, nDimensions, nVariables,                    &
//                          nAttributes, unlimitedDimID) result(ierr)

int pio_cpp_inquire(pio_file_desc_t file, int nDimensions, int nVariables,
                    int nAttributes, int unlimitedDimID);

// function pio_cpp_inq_att_vid(file,varid,name,xtype,len) result(ierr)

int pio_cpp_inq_att_vid(pio_file_desc_t file, int varid,
                        const char *name, int len);

// function pio_cpp_def_dim(file, name, len, dimid) result(ierr)

int pio_cpp_def_dim(pio_file_desc_t file,
                    const char *name,
                    int len,
                    int *dimid);

// function pio_cpp_enddef(file) result(ierr) bind(c)

int pio_cpp_enddef(pio_file_desc_t file);

// function pio_cpp_def_var_0d(file, name, type, vardesc) result(ierr) bind(c)

int pio_cpp_def_var_0d(pio_file_desc_t file,
                       const char *name,
                       int type,
                       pio_var_desc_t vardesc);

// function pio_cpp_def_var_md(file, name, type, dimds, vardesc)              &
//          result(ierr) bind(c)

int pio_cpp_def_var_md(pio_file_desc_t file,
                       const char *name,
                       int type,
                       int *dimds, int ndimds,
                       pio_var_desc_t vardesc);

///////////////////////////////////////////
//
//  darray read/write interface functions
//
///////////////////////////////////////////

// subroutine pio_cpp_read_darray_1d_int(file, varDesc, ioDesc, array,       &
//                                       narray, iostat) bind(c)

void pio_cpp_read_darray_1d_int(void *file,
                                void *varDesc,
                                void *ioDesc,
                                int *array,
                                int narray,
                                int *iostat);

// subroutine pio_cpp_read_darray_1d_real(file, varDesc, ioDesc, array,       &
//                                        narray, iostat) bind(c)

void pio_cpp_read_darray_1d_real(void *file,
                                 void *varDesc,
                                 void *ioDesc,
                                 float *array,
                                 int narray,
                                 int *iostat);

// subroutine pio_cpp_read_darray_1d_double(file, varDesc, ioDesc, array,     &
//                                          narray, iostat) bind(c)

void pio_cpp_read_darray_1d_double(void *file,
                                   void *varDesc,
                                   void *ioDesc,
                                   double *array,
                                   int narray,
                                   int *iostat);

// subroutine pio_cpp_write_darray_1d_int(file, varDesc, ioDesc, array,       &
//                                        narray, iostat) bind(c)

void pio_cpp_write_darray_int(void *file,
                              void *varDesc,
                              void *ioDesc,
                              int *array,
                              int *shape,
                              int rank,
                              int *iostat);

// subroutine pio_cpp_write_darray_1d_int_fill(file, varDesc, ioDesc, array,  &
//                                             narray, iostat) bind(c)

void pio_cpp_write_darray_1d_int_fill(void *file,
                                      void *varDesc,
                                      void *ioDesc,
                                      int *array,
                                      int narray,
                                      int *iostat,
                                      int fillval);

// subroutine pio_cpp_write_darray_1d_real(file, varDesc, ioDesc, array,      &
//                                         narray, iostat) bind(c)

void pio_cpp_write_darray_1d_real(void *file,
                                  void *varDesc,
                                  void *ioDesc,
                                  float *array,
                                  int narray,
                                  int *iostat);

// subroutine pio_cpp_write_darray_1d_real_fill(file, varDesc, ioDesc, array, &
//                                              narray, iostat, fillval)      &
//                                              bind(c)

void pio_cpp_write_darray_1d_real_fill(void *file,
                                       void *varDesc,
                                       void *ioDesc,
                                       float *array,
                                       int narray,
                                       int *iostat,
                                       float fillval);

// subroutine pio_cpp_write_darray_1d_double(file, varDesc, ioDesc, array,    &
//                                           narray, iostat) bind(c)

void pio_cpp_write_darray_1d_double(void *file,
                                    void *varDesc,
                                    void *ioDesc,
                                    double *array,
                                    int narray,
                                    int *iostat);

// subroutine pio_cpp_write_darray_1d_double_fill(file, varDesc, ioDesc,      &
//                                                array, narray, iostat,      &
//                                                fillval) bind(c)

void pio_cpp_write_darray_1d_double_fill(void *file,
                                         void *varDesc,
                                         void *ioDesc,
                                         double *array,
                                         int narray,
                                         int *iostat,
                                         double fillval);

} // extern "C"
// ---------------------------------------------------------------------

#endif // __PIO_H_INCLUDED_

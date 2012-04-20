#ifndef PIO_H
#define PIO_H

// ---------------------------------------------------------------------

// the size of the PIO Fortran derived types

// sizeof( iosystem_desc_t)

#define SIZE_IOSYSTEM_DESC  144

// sizeof( file_desc_t)

#define SIZE_FILE_DESC   24

// sizeof( io_desc_t)

#define SIZE_IO_DESC  472

// sizeof( var_desc_t)

#define SIZE_VAR_DESC   16

// ---------------------------------------------------------------------

// the pio function prototypes

// subroutine pio_cpp_init_intracom( comp_rank, comp_comm, num_iotasks, num_aggregator, stride, rearr, iosystem, base) bind( c)

extern "C" void pio_cpp_init_intracom( int comp_rank, int comp_comm, int num_tasks, int num_aggregator,
                                       int stride, int rearr, void* iosystem, int base);

// subroutine pio_cpp_init_intercom( component_count, peer_comm, comp_comms, io_comm, iosystem) bind( c)

extern "C" void pio_cpp_init_intercom( int component_count, int peer_comm, int* comp_comms, int io_comm, void* iosystem);

// subroutine pio_cpp_finalize( iosystem, ierr) bind( c)

extern "C" void pio_cpp_finalize( void* iosystem, int* ierror);

// subroutine pio_cpp_initdecomp_dof_i8( iosystem, basepiotype, dims, ndims, compdof, ncompdof, &
//                                       iodesc, iostart, niostart, iocount, niocount) bind( c)

extern "C" void pio_cpp_initdecomp_dof_i8( void* iosystem, int basepiotype, int* dims, int ndims, int* compdof, int ncompdof,
                                           void* iodesc, int* iostart, int niostart, int* iocount, int niocount);

// function pio_cpp_openfile( iosystem, file, iotype, fname, mode) result( ierr) bind( c)

extern "C" int pio_cpp_openfile( void* iosystem, void* file, int iotype, char* fname, int mode);

// subroutine pio_cpp_syncfile( file) bind( c)

extern "C" void pio_cpp_syncfile( void* file);

// function pio_cpp_createfile( iosystem, file, iotype, fname, amode_in) result( ierr) bind( c)

extern "C" int pio_cpp_createfile( void* iosystem, void* file, int iotype, char* fname, int amode_in);

// subroutine pio_cpp_closefile( file) bind( c)

extern "C" void pio_cpp_closefile( void* file);

// subroutine pio_cpp_setiotype( file, iotype, rearr) bind( c)

extern "C" void pio_cpp_setiotype( void* file, int iotype, int rearr);

// function pio_cpp_numtoread( iodesc) result( num) bind( c)

extern "C" int pio_cpp_numtoread( void* iodesc);

// function pio_cpp_numtowrite( iodesc) result( num) bind( c)

extern "C" int pio_cpp_numtowrite( void* iodesc);

// subroutine pio_cpp_setframe( vardesc, frame) bind( c)

extern "C" void pio_cpp_setframe( void* vardesc, int frame);

// subroutine pio_cpp_advanceframe( vardesc) bind( c)

extern "C" void pio_cpp_advanceframe( void* vardesc);

// subroutine pio_cpp_setdebuglevel( level) bind( c)

extern "C" void pio_cpp_setdebuglevel( int level);

// subroutine pio_cpp_seterrorhandlingf( file, method) bind( c)

extern "C" void pio_cpp_seterrorhandlingf( void* file, int method);

// subroutine pio_cpp_seterrorhandlingi( ios, method) bind( c)

extern "C" void pio_cpp_seterrorhandlingi( void* ios, int method);

// function pio_cpp_get_local_array_size( iodesc) result( siz) bind( c)

extern "C" int pio_cpp_get_local_array_size( void* iodesc);

// subroutine pio_cpp_freedecomp_ios( ios, iodesc) bind( c)

extern "C" void pio_cpp_freedecomp_ios( void* ios, void* iodesc);

// subroutine pio_cpp_freedecomp_file( file, iodesc) bind( c)

extern "C" void pio_cpp_freedecomp_file( void* file, void* iodesc);

// subroutine pio_cpp_dupiodesc( src, dest) bind( c)

extern "C" void pio_cpp_dupiodesc( void* src, void* dest);

// subroutine pio_cpp_getnumiotasks( iosystem, numiotasks) bind( c)

extern "C" void pio_cpp_getnumiotasks( void* iosystem, int* numiotasks);

// subroutine pio_cpp_set_hint( iosystem, hint, hintval) bind( c)

extern "C" void pio_cpp_set_hint( void* iosystem, void* hint, void* hint_val);

// function pio_cpp_getnum_ost( iosystem) result( numost) bind( c)

extern "C" int pio_cpp_getnum_ost( void* iosystem);

// subroutine pio_cpp_setnum_ost( iosystem, numost) bind( c)

extern "C" void pio_cpp_setnum_ost( void* iosystem, int numost);

// function pio_cpp_file_is_open( file) result( is_open) bind( c)

extern "C" int pio_cpp_file_is_open( void* file);

// ---------------------------------------------------------------------

// PIO_H

#endif

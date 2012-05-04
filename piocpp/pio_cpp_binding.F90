! ---------------------------------------------------------------------

!  procedures for a cpp binding to pio

module pio_cpp_binding

implicit none

!  explicit export

private

public :: pio_cpp_init_intracom
public :: pio_cpp_init_intercom
public :: pio_cpp_finalize
public :: pio_cpp_initdecomp_dof_i8
public :: pio_cpp_openfile
public :: pio_cpp_syncfile
public :: pio_cpp_createfile
public :: pio_cpp_closefile
public :: pio_cpp_setiotype
public :: pio_cpp_numtoread
public :: pio_cpp_numtowrite
public :: pio_cpp_setframe
public :: pio_cpp_advanceframe
public :: pio_cpp_setdebuglevel
public :: pio_cpp_seterrorhandlingf
public :: pio_cpp_seterrorhandlingi
public :: pio_cpp_get_local_array_size
public :: pio_cpp_freedecomp_ios
public :: pio_cpp_freedecomp_file
public :: pio_cpp_dupiodesc
public :: pio_cpp_getnumiotasks
public :: pio_cpp_set_hint
public :: pio_cpp_getnum_ost
public :: pio_cpp_setnum_ost
public :: pio_cpp_file_is_open

!  constants

integer, parameter :: max_string_len = 10000

! ---------------------------------------------------------------------

!  library

contains

! ---------------------------------------------------------------------

!  remove the null that ends a C string

pure subroutine f_chars( fc, cs)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_char

!  dummy arguments

character( kind= c_char, len= *), intent( in) :: cs
character( len= *), intent( out) :: fc

!  local

   integer :: i

!  text

continue

   convert_kind: do i = 1, min( len( fc), len( cs))

      fc( i: i) = char( ichar( cs( i: i)))

   end do convert_kind

return

end subroutine f_chars

! ---------------------------------------------------------------------

!  return the length of the character data in a C string

pure function c_len( cs) result( cl)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_char, c_null_char

!  result

integer :: cl

!  dummy arguments

character( kind= c_char, len= *), intent( in) :: cs

!  text

continue

   cl = index( cs, c_null_char) - 1

return

end function c_len

! ---------------------------------------------------------------------

!  the extern "C" functions()

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_init_intracom( int comp_rank, int comp_comm, int num_tasks, int num_aggregator,
!                                         int stride, int rearr, void* iosystem, int base);

subroutine pio_cpp_init_intracom( comp_rank, comp_comm, num_iotasks, num_aggregator, stride, rearr, iosystem, base) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_init

!  dummy arguments

integer( c_int), value :: comp_rank
integer( c_int), value :: comp_comm
integer( c_int), value :: num_iotasks
integer( c_int), value :: num_aggregator
integer( c_int), value :: stride
integer( c_int), value :: rearr
type( c_ptr), value :: iosystem
integer( c_int), value :: base

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

!  text

continue

!  convert the C pointer to a Fortran pointer

   call c_f_pointer( iosystem, iosystem_desc)

!  call the Fortran procedure

   call pio_init( int( comp_rank, i4), int( comp_comm, i4), int( num_iotasks, i4), int( num_aggregator, i4), &
                  int( stride, i4), int( rearr, i4), iosystem_desc, int( base, i4))

return

end subroutine pio_cpp_init_intracom

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_init_intercom( int component_count, int peer_comm, int* comp_comms, int io_comm, void* iosystem);

subroutine pio_cpp_init_intercom( component_count, peer_comm, comp_comms, io_comm, iosystem) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_init

!  dummy arguments

integer( c_int), value :: component_count
integer( c_int), value :: peer_comm
integer( c_int), dimension( component_count), intent( in) :: comp_comms
integer( c_int), value :: io_comm
type( c_ptr), value :: iosystem

!  local

   type( iosystem_desc_t), dimension( :), pointer :: iosystem_desc

!  text

continue

!  convert the C pointer to a Fortran pointer

   call c_f_pointer( iosystem, iosystem_desc, shape= [ component_count ])

!  call the Fortran procedure

   call pio_init( int( component_count), int( peer_comm), int( comp_comms), int( io_comm), iosystem_desc)

return

end subroutine pio_cpp_init_intercom

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_finalize( void* iosystem, int* ierror);

subroutine pio_cpp_finalize( iosystem, ierr) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_finalize

!  dummy arguments

type( c_ptr), value :: iosystem
integer( c_int), intent( out) :: ierr

!  local

   integer( i4) :: ierror

   type( iosystem_desc_t), pointer :: iosystem_desc

!  text

continue

!  convert the C pointer to a Fortran pointer

   call c_f_pointer( iosystem, iosystem_desc)

!  call the Fortran procedure

   call pio_finalize( iosystem_desc, ierror)

!  convert the arguments back to C

   ierr = int( ierror, c_int)

!  return to the cpp caller

return

end subroutine pio_cpp_finalize

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_initdecomp_dof_i8( void* iosystem, int basepiotype, int* dims, int ndims, int* compdof, int ncompdof,
!                                             void* iodesc, int* iostart, int niostart, int* iocount, int niocount);

subroutine pio_cpp_initdecomp_dof_i8( iosystem, basepiotype, dims, ndims, compdof, ncompdof, &
                                      iodesc, iostart, niostart, iocount, niocount) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4, pio_offset

!  import pio types

use :: pio_types, only: iosystem_desc_t, io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_initdecomp

!  dummy arguments

type( c_ptr), value :: iosystem
integer( c_int), value :: basepiotype
type( c_ptr), value :: dims
integer( c_int), value :: ndims
type( c_ptr), value :: compdof
integer( c_int), value :: ncompdof
type( c_ptr), value :: iodesc
type( c_ptr), value :: iostart
integer( c_int), value :: niostart
type( c_ptr), value :: iocount
integer( c_int), value :: niocount

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc
   integer( c_int), dimension( :), pointer :: as_dims
   integer( c_int), dimension( :), pointer :: as_compdof
   type( io_desc_t), pointer :: iodesc_desc
   integer( c_int), dimension( :), pointer :: as_iostart
   integer( c_int), dimension( :), pointer :: as_iocount

!  text

continue

!  convert the C pointer to a Fortran pointer

   call c_f_pointer( iosystem, iosystem_desc)
   call c_f_pointer( dims, as_dims, shape= [ ndims ])
   call c_f_pointer( compdof, as_compdof, shape= [ ncompdof ])
   call c_f_pointer( iodesc, iodesc_desc)
   call c_f_pointer( iostart, as_iostart, shape= [ niostart ])
   call c_f_pointer( iocount, as_iocount, shape= [ niocount ])

!  call the Fortran procedure

   call pio_initdecomp( iosystem_desc, int( basepiotype, i4), int( as_dims, i4), int( as_compdof, pio_offset), &
                        iodesc_desc, int( as_iostart, pio_offset), int( as_iocount, pio_offset))

!  return to the cpp caller

return

end subroutine pio_cpp_initdecomp_dof_i8

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_openfile( void* iosystem, void* file, int iotype, char* fname, int mode);

function pio_cpp_openfile( iosystem, file, iotype, fname, mode) result( ierr) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t, file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_openfile

!  function result

integer( c_int) :: ierr

!  dummy arguments

type( c_ptr), value :: iosystem
type( c_ptr), value :: file
integer( c_int), value :: iotype
type( c_ptr), value :: fname
integer( c_int), value :: mode

!  local

   integer :: ierror

   type( iosystem_desc_t), pointer :: iosystem_desc
   type( file_desc_t), pointer :: file_desc
   character( kind= c_char, len= max_string_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
   character( len= :), allocatable :: filename
#else
   character( len= max_string_len) :: filename
#endif

   integer :: clen

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)
   call c_f_pointer( file, file_desc)
   call c_f_pointer( fname, c_filename)

!  convert the C string to Fortran characters

   clen = c_len( c_filename)

#ifdef ALLOC_CHARLEN_OK
   allocate( filename, mold= filename( 1: clen))
#endif
   call f_chars( filename, c_filename( 1: clen))

!  call the Fortran procedure

   ierror = pio_openfile( iosystem_desc, file_desc, int( iotype), filename, int( mode))

!  convert the arguments back to C

   ierr = int( ierror, c_int)

!  return to the cpp caller

return

end function pio_cpp_openfile

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_syncfile( void* file);

subroutine pio_cpp_syncfile( file) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_syncfile

!  dummy arguments

type( c_ptr), value :: file

!  local

   type( file_desc_t), pointer :: file_desc

!  text

continue

!  convert the C pointer to a Fortran pointer

   call c_f_pointer( file, file_desc)

!  call the Fortran procedure

   call pio_syncfile( file_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_syncfile

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_createfile( void* iosystem, void* file, int iotype, char* fname, int amode_in);

function pio_cpp_createfile( iosystem, file, iotype, fname, amode_in) result( ierr) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t, file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_createfile

!  function result

integer( c_int) :: ierr

!  dummy arguments

type( c_ptr), value :: iosystem
type( c_ptr), value :: file
integer( c_int), value :: iotype
type( c_ptr), value :: fname
integer( c_int), value :: amode_in

!  local

   integer :: ierror

   type( iosystem_desc_t), pointer :: iosystem_desc
   type( file_desc_t), pointer :: file_desc
   character( kind= c_char, len= max_string_len), pointer :: c_filename
#if ALLOC_CHARLEN_OK
   character( len= :), allocatable :: filename
#else
   character( len= max_string_len) :: filename
#endif

   integer :: clen

!  text

continue

!  convert the C pointers to Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)
   call c_f_pointer( file, file_desc)
   call c_f_pointer( fname, c_filename)

!  convert the C string to Fortran characters

   clen = c_len( c_filename)

#if ALLOC_CHARLEN_OK
   allocate( filename, mold= filename( 1: clen))
#endif

   call f_chars( filename, c_filename( 1: clen))

!  call the Fortran procedure

   ierror = pio_createfile( iosystem_desc, file_desc, int( iotype), filename, int( amode_in))

!  convert the arguments back to C

   ierr = int( ierror, c_int)

!  return to the cpp caller

return

end function pio_cpp_createfile

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_closefile( void* file);

subroutine pio_cpp_closefile( file) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_closefile

!  dummy arguments

type( c_ptr), value :: file

!  local

   type( file_desc_t), pointer :: file_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( file, file_desc)

!  call the Fortran procedure

   call pio_closefile( file_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_closefile

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_setiotype( void* file, int iotype, int rearr);

subroutine pio_cpp_setiotype( file, iotype, rearr) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio types

use :: pio_types, only: file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_setiotype

!  dummy arguments

type( c_ptr), value :: file
integer( c_int), value :: iotype
integer( c_int), value :: rearr

!  local

   type( file_desc_t), pointer :: file_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( file, file_desc)

!  call the Fortran procedure

   call pio_setiotype( file_desc, int( iotype, i4), int( rearr, i4))

!  return to the cpp caller

return

end subroutine pio_cpp_setiotype

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_numtoread( void* iodesc);

function pio_cpp_numtoread( iodesc) result( num) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_numtoread

!  function result

integer( c_int) :: num

!  dummy arguments

type( c_ptr), value :: iodesc

!  local

   integer :: n

   type( io_desc_t), pointer :: io_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iodesc, io_desc)

!  call the Fortran procedure

   n = pio_numtoread( io_desc)

!  convert the arguments back to C

   num = int( n, c_int)

!  return to the cpp caller

return

end function pio_cpp_numtoread

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_numtowrite( void* iodesc);

function pio_cpp_numtowrite( iodesc) result( num) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_numtowrite

!  function result

integer( c_int) :: num

!  dummy arguments

type( c_ptr), value :: iodesc

!  local

   integer :: n

   type( io_desc_t), pointer :: io_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iodesc, io_desc)

!  call the Fortran procedure

   n = pio_numtowrite( io_desc)

!  convert the arguments back to C

   num = int( n, c_int)

!  return to the cpp caller

return

end function pio_cpp_numtowrite

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_setframe( void* vardesc, int frame);

subroutine pio_cpp_setframe( vardesc, frame) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: pio_offset

!  import pio types

use :: pio_types, only: var_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_setframe

!  dummy arguments

type( c_ptr), value :: vardesc
integer( c_int), value :: frame

!  local

   type( var_desc_t), pointer :: var_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( vardesc, var_desc)

!  call the Fortran procedure

   call pio_setframe( var_desc, int( frame, pio_offset))

!  return to the cpp caller

return

end subroutine pio_cpp_setframe

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_advanceframe( void* vardesc);

subroutine pio_cpp_advanceframe( vardesc) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: var_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_advanceframe

!  dummy arguments

type( c_ptr), value :: vardesc

!  local

   type( var_desc_t), pointer :: var_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( vardesc, var_desc)

!  call the Fortran procedure

   call pio_advanceframe( var_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_advanceframe

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_setdebuglevel( int level);

subroutine pio_cpp_setdebuglevel( level) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio procedure signatures

use :: piolib_mod, only: pio_setdebuglevel

!  dummy arguments

integer( c_int), value :: level

!  text

continue

!  call the Fortran procedure

   call pio_setdebuglevel( int( level, i4))

!  return to the cpp caller

return

end subroutine pio_cpp_setdebuglevel

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_seterrorhandlingf( void* file, int method);

subroutine pio_cpp_seterrorhandlingf( file, method) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_seterrorhandling

!  dummy arguments

type( c_ptr), value :: file
integer( c_int), value :: method

!  local

   type( file_desc_t), pointer :: file_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( file, file_desc)

!  call the Fortran procedure

   call pio_seterrorhandling( file_desc, int( method))

!  return to the cpp caller

return

end subroutine pio_cpp_seterrorhandlingf

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_seterrorhandlingi( void* ios, int method);

subroutine pio_cpp_seterrorhandlingi( ios, method) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_seterrorhandling

!  dummy arguments

type( c_ptr), value :: ios
integer( c_int), value :: method

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( ios, iosystem_desc)

!  call the Fortran procedure

   call pio_seterrorhandling( iosystem_desc, int( method))

!  return to the cpp caller

return

end subroutine pio_cpp_seterrorhandlingi

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_get_local_array_size( void* iodesc);

function pio_cpp_get_local_array_size( iodesc) result( siz) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_get_local_array_size

!  function result

integer( c_int) :: siz

!  dummy arguments

type( c_ptr), value :: iodesc

!  local

   integer :: s

   type( io_desc_t), pointer :: io_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iodesc, io_desc)

!  call the Fortran procedure

   s = pio_get_local_array_size( io_desc)

!  convert the arguments back to C

   siz = int( s, c_int)

!  return to the cpp caller

return

end function pio_cpp_get_local_array_size

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_freedecomp_ios( void* ios, void* iodesc);

subroutine pio_cpp_freedecomp_ios( ios, iodesc) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t, io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_freedecomp

!  dummy arguments

type( c_ptr), value :: ios
type( c_ptr), value :: iodesc

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc
   type( io_desc_t), pointer :: io_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( ios, iosystem_desc)
   call c_f_pointer( iodesc, io_desc)

!  call the Fortran procedure

   call pio_freedecomp( iosystem_desc, io_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_freedecomp_ios

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_freedecomp_file( void* file, void* iodesc);

subroutine pio_cpp_freedecomp_file( file, iodesc) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: file_desc_t, io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_freedecomp

!  dummy arguments

type( c_ptr), value :: file
type( c_ptr), value :: iodesc

!  local

   type( file_desc_t), pointer :: file_desc
   type( io_desc_t), pointer :: io_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( file, file_desc)
   call c_f_pointer( iodesc, io_desc)

!  call the Fortran procedure

   call pio_freedecomp( file_desc% iosystem, io_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_freedecomp_file

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_dupiodesc( void* src, void* dest);

subroutine pio_cpp_dupiodesc( src, dest) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: io_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_dupiodesc

!  dummy arguments

type( c_ptr), value :: src
type( c_ptr), value :: dest

!  local

   type( io_desc_t), pointer :: src_desc
   type( io_desc_t), pointer :: dest_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( src, src_desc)
   call c_f_pointer( dest, dest_desc)

!  call the Fortran procedure

   call pio_dupiodesc( src_desc, dest_desc)

!  return to the cpp caller

return

end subroutine pio_cpp_dupiodesc

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_getnumiotasks( void* iosystem, int* numiotasks);

subroutine pio_cpp_getnumiotasks( iosystem, numiotasks) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_getnumiotasks

!  dummy arguments

type( c_ptr), value :: iosystem
integer( c_int) :: numiotasks

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

   integer( i4) :: nt

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)

!  call the Fortran procedure

   call pio_getnumiotasks( iosystem_desc, nt)

!  convert the arguments back to C

   numiotasks = int( nt, c_int)

!  return to the cpp caller

return

end subroutine pio_cpp_getnumiotasks

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_set_hint( void* iosystem, void* hint, void* hintval);

subroutine pio_cpp_set_hint( iosystem, hint, hintval) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_set_hint

!  dummy arguments

type( c_ptr), value :: iosystem
type( c_ptr), value :: hint
type( c_ptr), value :: hintval

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

   character( kind= c_char, len= max_string_len), pointer :: c_hint
   character( kind= c_char, len= max_string_len), pointer :: c_hintval

#if ALLOC_CHARLEN_OK
   character( len= :), allocatable :: hint_str
   character( len= :), allocatable :: hintval_str
#else
   character( len= max_string_len) :: hint_str
   character( len= max_string_len) :: hintval_str
#endif

   integer :: clen

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)
   call c_f_pointer( hint, c_hint)
   call c_f_pointer( hintval, c_hintval)

!  convert the C string to Fortran characters

   clen = c_len( c_hint)
#if ALLOC_CHARLEN_OK
   allocate( hint_str, mold= hint_str( 1: clen))
#endif
   call f_chars( hint_str, c_hint( 1: clen))

   clen = c_len( c_hintval)
#if ALLOC_CHARLEN_OK
   allocate( hintval_str, mold= hintval_str( 1: clen))
#endif
   call f_chars( hintval_str, c_hintval( 1: clen))

!  call the Fortran procedure

   call pio_set_hint( iosystem_desc, hint_str, hintval_str)

!  return to the cpp caller

return

end subroutine pio_cpp_set_hint

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_getnum_ost( void* iosystem);

function pio_cpp_getnum_ost( iosystem) result( numost) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_getnum_ost

!  function result

integer( c_int) :: numost

!  dummy arguments

type( c_ptr), value :: iosystem

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

   integer :: n

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)

!  call the Fortran procedure

   n = pio_getnum_ost( iosystem_desc)

!  convert the arguments back to C

   numost = int( n, c_int)

!  return to the cpp caller

return

end function pio_cpp_getnum_ost

! ---------------------------------------------------------------------

!  extern "C" void pio_cpp_setnum_ost( void* iosystem, int numost);

subroutine pio_cpp_setnum_ost( iosystem, numost) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

!  import pio kinds

use :: pio_kinds, only: i4

!  import pio types

use :: pio_types, only: iosystem_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_setnum_ost

!  dummy arguments

type( c_ptr), value :: iosystem
integer( c_int) :: numost

!  local

   type( iosystem_desc_t), pointer :: iosystem_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( iosystem, iosystem_desc)

!  call the Fortran procedure

   call pio_setnum_ost( iosystem_desc, int( numost, i4))

!  return to the cpp caller

return

end subroutine pio_cpp_setnum_ost

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_file_is_open( void* file);

function pio_cpp_file_is_open( file) result( is_open) bind( c)

!  bind to C

use, intrinsic :: iso_c_binding, only: c_bool, c_ptr, c_f_pointer

!  import pio types

use :: pio_types, only: file_desc_t

!  import pio procedure signatures

use :: piolib_mod, only: pio_file_is_open

!  function result

logical( c_bool) :: is_open

!  dummy arguments

type( c_ptr), value :: file

!  local

   logical :: o

   type( file_desc_t), pointer :: file_desc

!  text

continue

!  convert the C pointers to a Fortran pointers

   call c_f_pointer( file, file_desc)

!  call the Fortran procedure

   o = pio_file_is_open( file_desc)

!  convert the arguments back to C

   is_open = logical( o, c_bool)

!  return to the cpp caller

return

end function pio_cpp_file_is_open

! ---------------------------------------------------------------------

end module pio_cpp_binding

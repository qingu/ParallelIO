#define __PIO_FILE__ "nf_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for a cpp binding to PIO's NetCDF interface functions
module nf_cpp_binding

  use pio_kinds, only: i4, r4, r8, pio_offset

  implicit none

   !  explicit export

  private

   ! public interface

  public :: pio_cpp_def_dim
!   public :: PIO_enddef
!   public :: PIO_redef
!   public :: PIO_inquire
!   public :: PIO_inq_dimid
!   public :: PIO_inq_dimname
!   public :: PIO_inq_dimlen
!   public :: PIO_inquire_dimension
!   public :: PIO_copy_att
  public :: pio_cpp_def_var_0d
  public :: pio_cpp_def_var_md
!   public :: pio_inq_attname
!   public :: pio_inq_att
!   public :: pio_inq_attlen
!   public :: pio_inq_varid
!   public :: pio_inq_varname
!   public :: pio_inq_vartype
!   public :: pio_inq_varndims
!   public :: pio_inq_vardimid
!   public :: pio_inq_varnatts
!   public :: pio_inquire_variab

  !  constants

 contains

!  the extern "C" functions()

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_def_dim(pio_file_desc_t file, const char *name,      &
!                                int len, int *dimid);

function pio_cpp_def_dim(file, name, len, dimid) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_dim
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: len
  integer(c_int), intent(out) :: dimid

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif

  integer :: clen
  integer :: dim_id

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_dim(file_desc, trim(filename), int(len), dim_id)

  !  convert the arguments back to C
  dimid = int(dim_id, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_def_dim

!  extern "C" int pio_cpp_def_var_0d(pio_file_desc_t file, const char *name,  &
!                                    int type, pio_var_desc_t vardesc);

function pio_cpp_def_var_0d(file, name, type, vardesc) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: var_desc_t, file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_var
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: type
  type(c_ptr), value :: vardesc

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif
  type(Var_desc_t), pointer :: var_desc

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)
  call c_f_pointer(vardesc, var_desc)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_var(file_desc, trim(filename), int(type), var_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_def_var_0d

! extern "C" int pio_cpp_def_var_0d(pio_file_desc_t file, const char *name,   &
!                                   int type, int *dimds, int ndimds,         &
!                                   pio_var_desc_t vardesc);

function pio_cpp_def_var_md(file, name, type, dimds, ndimds, vardesc)         &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: var_desc_t, file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_var
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: type
  type(c_ptr), value :: dimds
  integer(c_int), value :: ndimds
  type(c_ptr), value :: vardesc

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif
  integer(c_int), dimension(:), pointer :: as_dimds
  type(Var_desc_t), pointer :: var_desc

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)
  call c_f_pointer(dimds, as_dimds, shape= [ ndimds ])
  call c_f_pointer(vardesc, var_desc)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_var(file_desc, trim(filename), int(type),                  &
                       int(as_dimds), var_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_def_var_md

! ---------------------------------------------------------------------
end module nf_cpp_binding

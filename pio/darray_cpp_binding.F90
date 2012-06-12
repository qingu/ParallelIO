#define __PIO_FILE__ "darray_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for a cpp binding to PIO's read/write darray interface functions
module darray_cpp_binding

  use pio_kinds, only: i4, r4, r8, pio_offset

  !  explicit export

  private

  ! public interface
  ! read/write routines
  public :: pio_cpp_read_darray_1d_int
  public :: pio_cpp_read_darray_1d_real
  public :: pio_cpp_read_darray_1d_double
  public :: pio_cpp_write_darray_1d_int
  public :: pio_cpp_write_darray_1d_real
  public :: pio_cpp_write_darray_1d_double

  !  constants

  integer, parameter :: max_string_len = 1024
  integer, parameter :: max_path_len = 1024

contains

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_1d_int(void *file, void *varDesc,      &
!                                             void *ioDesc, int *array,       &
!                                             int narray, int *iostat);

subroutine pio_cpp_read_darray_1d_int(file, varDesc, ioDesc, array, narray,   &
                                      iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(i4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call read_darray_1d_int(file_desc, var_desc, io_desc,                       &
                          int(as_array, i4), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_read_darray_1d_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_1d_real(void *file, void *varDesc,     &
!                                              void *ioDesc, int *array,      &
!                                              int narray, int *iostat);

subroutine pio_cpp_read_darray_1d_real(file, varDesc, ioDesc, array, narray,  &
                                       iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call read_darray_1d_real(file_desc, var_desc, io_desc,                      &
                           int(as_array, r4), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end subroutine pio_cpp_read_darray_1d_real

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_1d_double(void *file, void *varDesc,   &
  !                                              void *ioDesc, int *array,    &
!                                                int narray, int *iostat);

subroutine pio_cpp_read_darray_1d_double(file, varDesc, ioDesc, array,        &
                                         narray, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r8), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call read_darray_1d_real(file_desc, var_desc, io_desc,                      &
                           int(as_array, r8), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end subroutine pio_cpp_read_darray_1d_double

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_int(void *file,                    &
!                                              void *varDesc,                 &
!                                              void *ioDesc, int *array,      &
!                                              int narray, int *iostat);

subroutine pio_cpp_write_darray_1d_int(file, varDesc, ioDesc, array,          &
                                       narray, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(i4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_int(file_desc, var_desc, io_desc,                      &
                           int(as_array, i4), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_int_fill(void *file,               &
!                                                   void *varDesc,            &
!                                                   void *ioDesc, int *array, &
!                                                   int narray, int *iostat,  &
!                                                   int fillval);

subroutine pio_cpp_write_darray_1d_int_fill(file, varDesc, ioDesc, array,     &
                                            narray, iostat, fillval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat
  integer(c_int), value :: fillval

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(i4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_int(file_desc, var_desc, io_desc,                      &
                           int(as_array, i4), iostatus, fillval)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_int_fill

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_float(void *file,                  &
!                                                void *varDesc,               &
!                                                void *ioDesc, float *array,  &
!                                                int narray, int *iostat);

subroutine pio_cpp_write_darray_1d_real(file, varDesc, ioDesc, array,         &
                                        narray, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_real(file_desc, var_desc, io_desc,                    &
                            real(as_array, r4), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_real

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_real_fill(void *file,              &
!                                                    void *varDesc,           &
!                                                    void *ioDesc,            &
!                                                    float *array,            &
!                                                    int narray, int *iostat, &
!                                                    float fillval);

subroutine pio_cpp_write_darray_1d_real_fill(file, varDesc, ioDesc, array,    &
                                             narray, iostat, fillval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_float, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat
  real(c_float), value :: fillval

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r4), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_real(file_desc, var_desc, io_desc,                     &
                            real(as_array, r4), iostatus, real(fillval, r4))

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_real_fill

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_double(void *file,                 &
!                                                 void *varDesc,              &
!                                                 void *ioDesc,               &
!                                                 double *array,              &
!                                                 int narray, int *iostat);

subroutine pio_cpp_write_darray_1d_double(file, varDesc, ioDesc, array,       &
                                          narray, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r8), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_double(file_desc, var_desc, io_desc,                   &
                              real(as_array, r8), iostatus)

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_double

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_1d_double_fill(void *file,            &
!                                                      void *varDesc,         &
!                                                      void *ioDesc,          &
!                                                      double *array,         &
!                                                      int narray,            &
!                                                      int *iostat,           &
!                                                      double fillval);

subroutine pio_cpp_write_darray_1d_double_fill(file, varDesc, ioDesc, array,  &
                                               narray, iostat, fillval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: varDesc
  type(c_ptr), value :: ioDesc
  type(c_ptr), value :: array
  integer(c_int), value :: narray
  integer(c_int), intent(out) :: iostat
  real(c_double), value :: fillval

  !  local
  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  type(io_desc_t), pointer :: iodesc_desc
  integer(r8), dimension(:), pointer :: as_array

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(array, as_array, shape= [ narray ])

  !  call the Fortran procedure
  call write_darray_1d_double(file_desc, var_desc, io_desc,                   &
                              real(as_array, r8), iostatus, real(fillval, r8))

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_1d_double_fill

! ---------------------------------------------------------------------
end module darray_cpp_binding

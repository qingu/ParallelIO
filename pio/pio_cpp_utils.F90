#define __PIO_FILE__ "pio_cpp_utils.F90"
! ---------------------------------------------------------------------

!  utility procedures for use by cpp binding functions
module pio_cpp_utils

  use pio_kinds, only: i4, r4, r8, pio_offset

  implicit none

   !  explicit export

  private

   ! public interface

  public :: f_chars
  public :: c_len
  public :: max_string_len
  public :: max_path_len

  !  constants

  integer, parameter :: max_string_len = 1024
  integer, parameter :: max_path_len = 1024

 contains

! ---------------------------------------------------------------------

!  remove the null that ends a C string

pure subroutine f_chars(fc, cs)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char

  !  dummy arguments

  character(kind= c_char, len= *), intent(in) :: cs
  character(len= *), intent(out) :: fc

  !  local
  integer :: i

  !  text
  continue

  convert_kind: do i = 1, min(len(fc), len(cs))
     fc(i: i) = char(ichar(cs(i: i)))
  end do convert_kind

  return

end subroutine f_chars

! ---------------------------------------------------------------------

!  return the length of the character data in a C string

pure function c_len(cs) result(cl)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_null_char

  !  result
  integer :: cl

  !  dummy arguments
  character(kind= c_char, len= *), intent(in) :: cs

  !  text
  continue

  cl = index(cs, c_null_char) - 1

  return

end function c_len

end module pio_cpp_utils

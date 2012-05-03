module pio_sizes

!  import C kind type parameters

use, intrinsic :: iso_c_binding, only: c_char

!  import pio types

use :: pio_types, only: iosystem_desc_t, file_desc_t, io_desc_t, var_desc_t

private

!  types whose sizes we need

type( iosystem_desc_t) :: ios
type( file_desc_t) :: file
type( io_desc_t) :: iod
type( var_desc_t) :: var

!  sizeof returns sizes in bytes

character( kind= c_char, len= 1) :: c

!  computed sizes

integer, save, public :: size_ios = storage_size( ios) !/ storage_size( c)
integer, save, public :: size_file = 0!storage_size( file) / storage_size( c)
integer, save, public :: size_iod = 0!storage_size( iod) / storage_size( c)
integer, save, public :: size_var = 0!storage_size( var) / storage_size( c)

end module pio_sizes

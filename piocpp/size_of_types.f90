
!  rewrite the pio.h file with the correct sizes of PIO derived types

program size_of_types

!  import C kind type parameters

use, intrinsic :: iso_c_binding, only: c_char

!  import pio types

use :: pio_types, only: iosystem_desc_t, file_desc_t, io_desc_t, var_desc_t

!  units and files

integer, parameter :: h_unit = 10
character( len= *), parameter :: h_name = 'pio.h'

integer, parameter :: scratch_unit = h_unit + 1

!  strings to seek

character( len= *), parameter :: iosystem_str = 'SIZE_IOSYSTEM_DESC'
character( len= *), parameter :: file_str = 'SIZE_FILE_DESC'
character( len= *), parameter :: iod_str = 'SIZE_IO_DESC'
character( len= *), parameter :: var_str = 'SIZE_VAR_DESC'

!  line lengths and formats

integer, parameter :: line_len = 160
character( len= *), parameter :: a_fmt = '( a)'
character( len= *), parameter :: i_fmt = '( i4)'

!  types whose sizes we need

type( iosystem_desc_t) :: ios
type( file_desc_t) :: file
type( io_desc_t) :: iod
type( var_desc_t) :: var

!  sizeof returns sizes in bytes

character( kind= c_char, len= 1) :: c

!  computed sizes

integer :: size_ios
integer :: size_file
integer :: size_iod
integer :: size_var

!  lines from the header file

character( len= line_len) :: line

character( len= line_len) :: line_ios
character( len= line_len) :: line_file
character( len= line_len) :: line_iod
character( len= line_len) :: line_var

!  indexes of lines and strings

integer :: idx

integer :: pos

integer :: rec
integer :: rec_ios
integer :: rec_file
integer :: rec_iod
integer :: rec_var

!  must check iostat to detect end-of-file

integer :: iostatus

!  text

continue

!  compute C sizes

   size_ios = storage_size( ios) / storage_size( c)
   size_file = storage_size( file) / storage_size( c)
   size_iod = storage_size( iod) / storage_size( c)
   size_var = storage_size( var) / storage_size( c)

!  open the header file and a scratch file to store the first pass

   open( unit= h_unit, file= h_name, status= 'old', position= 'rewind')

   open( unit= scratch_unit, status= 'scratch', form= 'formatted')

!  count records

   rec = 0

!  loop through the header file line-by-line

   read_h: do

!  read a line

      read( unit= h_unit, fmt= a_fmt, iostat= iostatus) line

!  exit at end-of-file

      if( iostatus < 0 ) exit read_h

!  count records

      rec = rec + 1

!  hold in the scratch file

      write( unit= scratch_unit, fmt= a_fmt) line

!  seek the iosystem string

      idx = index( line, iosystem_str)

!  if found

      got_iosystem: if( idx > 0 )then

!  mark the record number and update the line with the computed size

         rec_ios = rec

         line_ios = line

         pos = idx + len( iosystem_str) + 1
         write( unit= line_ios( pos: pos + 3), fmt= i_fmt) size_ios

         cycle read_h

      end if got_iosystem

!  seek the file string

      idx = index( line, file_str)

!  if found

      got_file: if( idx > 0 )then

!  mark the record number and update the line with the computed size

         rec_file = rec

         line_file = line

         pos = idx + len( file_str) + 1
         write( unit= line_file( pos: pos + 3), fmt= i_fmt) size_file

         cycle read_h

      end if got_file

!  seek the iodesc string

      idx = index( line, iod_str)

!  if found

      got_iodesc: if( idx > 0 )then

!  mark the record number and update the line with the computed size

         rec_iod = rec

         line_iod = line

         pos = idx + len( iod_str) + 1
         write( unit= line_iod( pos: pos + 3), fmt= i_fmt) size_iod

         cycle read_h

      end if got_iodesc

!  seek the var string

      idx = index( line, var_str)

!  if found

      got_var: if( idx > 0 )then

!  mark the record number and update the line with the computed size

         rec_var = rec

         line_var = line

         pos = idx + len( var_str) + 1
         write( unit= line_var( pos: pos + 3), fmt= i_fmt) size_var

         cycle read_h

      end if got_var

   end do read_h

!  delete the original header

   close( unit= h_unit, status= 'delete')

!  prepare to re-read the scratch file

   rewind( unit= scratch_unit)

!  start a new header

   open( unit= h_unit, file= h_name, status= 'new')

!  count records

   rec = 0

!  write the new header

   write_h: do

!  read the scratch file

      read( unit= scratch_unit, fmt= a_fmt, iostat= iostatus) line

!  exit at end-of-file

      if( iostatus < 0 ) exit write_h

!  count records

      rec = rec + 1

!  when at the iosystem record

      special_line: if( rec == rec_ios )then

!  write the updated record

         write( unit= h_unit, fmt= a_fmt) trim( line_ios)

!  when at the file record

      else if( rec == rec_file )then special_line

!  write the updated record

         write( unit= h_unit, fmt= a_fmt) trim( line_file)

!  when at the iodesc record

      else if( rec == rec_iod )then special_line

!  write the updated record

         write( unit= h_unit, fmt= a_fmt) trim( line_iod)

!  when at the var record

      else if( rec == rec_var )then special_line

!  write the updated record

         write( unit= h_unit, fmt= a_fmt) trim( line_var)

!  when at any other record

      else special_line

!  write the original record

         write( unit= h_unit, fmt= a_fmt) trim( line)

      end if special_line

   end do write_h

!  keep the new header

   close( unit= h_unit, status= 'keep')
   close( unit= scratch_unit)

!  write a summary for our fans

   write( unit= *, fmt= *) 'C size of iosystem_desc_t', size_ios

   write( unit= *, fmt= *) 'C size of file_desc_t', size_file

   write( unit= *, fmt= *) 'C size of io_desc_t', size_iod

   write( unit= *, fmt= *) 'C size of var_desc_t', size_var

!  quit happy

stop 'normal exit'

end program size_of_types

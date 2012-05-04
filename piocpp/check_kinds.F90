
!  report whether PIO kinds and C kinds interoperate

program check_kinds

!  import the C kinds

use, intrinsic :: iso_c_binding, only: c_bool, c_int, c_long, c_float, c_double

!  import the PIO kinds

use :: pio_kinds, only: log_kind, int_kind, i4, i8, r4, r8

!  record whether the kinds all match

   logical :: all_ok

!  text

continue

!  every PIO kind must match its C counterpart

   all_ok = log_kind == c_bool &
      .and. int_kind == c_int &
      .and. i4 == c_int &
      .and. i8 == c_long &
      .and. r4 == c_float &
      .and. r8 == c_double

!  report yea or nay

   all_checks: if( all_ok )then

      write( unit= *, fmt= *) 'SUCCESS: all kinds match'

   else all_checks

      write( unit= *, fmt= *) 'FAILURE: not all kinds match!'

      write( unit= *, fmt= '( 15x, a10, a10)') 'PIO kind', 'C kind'
      write( unit= *, fmt= '( a15, i10, i10)') 'log_kind:', log_kind, c_bool
      write( unit= *, fmt= '( a15, i10, i10)') 'kind( c_bool):', kind( .true._c_bool), c_bool
      write( unit= *, fmt= '( a15, i10, i10)') 'int_kind:', int_kind, c_int
      write( unit= *, fmt= '( a15, i10, i10)') 'i4:', i4, c_int
      write( unit= *, fmt= '( a15, i10, i10)') 'i8:', i8, c_long
      write( unit= *, fmt= '( a15, i10, i10)') 'r4:', r4, c_float
      write( unit= *, fmt= '( a15, i10, i10)') 'r8:', r8, c_double

   end if all_checks

!  report completion

stop 'complete check_kind'

!  done

end program check_kinds

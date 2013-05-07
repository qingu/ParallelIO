module basic_tests

  use pio_types
  use piolib_mod
  use nf_mod
  use global_vars

  Implicit None
  private
  save

  public :: test_create

  Contains

    Function test_create(test_id)
    ! test_create():
    ! * Create an empty file, close it, test that it can be opened
    ! * Do it again with PIO_CLOBBER mode
    ! * Do it again with PIO_NOCLOBBER mode, check for error

      ! Input / Output Vars
      integer, intent(in) :: test_id
      integer             :: test_create

      ! Local Vars
      character(len=str_len) :: filename
      integer                :: iotype, fails, ret_val

      fails = 0

      filename = fnames(test_id)
      iotype   = iotypes(test_id)

      ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename)
      if (ret_val.ne.0) fails = fails+1
      if ((iotype.eq.PIO_iotype_netcdf).or.(iotype.eq.PIO_iotype_pnetcdf)) then
        ret_val = PIO_enddef(pio_file)
        if (ret_val.ne.0) fails = fails+1
      end if
      call PIO_closefile(pio_file)
      test_create = fails

    End Function test_create

end module basic_tests

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
    ! Routines tested: PIO_createfile, PIO_openfile, PIO_closefile
    ! Also checks PIO_enddef if netcdf or pnetcdf tests are enabled

      ! Input / Output Vars
      integer, intent(in) :: test_id
      integer             :: test_create

      ! Local Vars
      character(len=str_len) :: filename
      integer                :: iotype, fails, ret_val

      fails = 0

      filename = fnames(test_id)
      iotype   = iotypes(test_id)

      ! Original create file
      ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename)
      if (ret_val.ne.0) then
        ! File not created successfully 
        fails = fails+1
      else
        ! File created
        if ((iotype.eq.PIO_iotype_netcdf).or.(iotype.eq.PIO_iotype_pnetcdf)) then
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) fails = fails+1
        end if
        call PIO_closefile(pio_file)

        ! Test opening of file
        ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, filename, PIO_nowrite)
        if (ret_val.ne.0) then
          ! Error opening file
          fails = fails+1
        else
          ! File opened
          call PIO_closefile(pio_file)
        end if
      end if

      ! Recreate file with CLOBBER (netcdf / pnetcdf only)
      if ((iotype.eq.PIO_iotype_netcdf).or.(iotype.eq.PIO_iotype_pnetcdf)) then
        ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename, PIO_CLOBBER)
        if (ret_val.ne.0) then
          ! File not created successfully
          fails = fails+1
        else
          ! File created 
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) fails = fails+1
          call PIO_closefile(pio_file)
        end if
      end if

      ! Recreate file with NOCLOBBER
      if ((iotype.eq.PIO_iotype_netcdf).or.(iotype.eq.PIO_iotype_pnetcdf)) then
        ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename, PIO_NOCLOBBER)
        if (ret_val.eq.0) then
          ! File created (bad, expect an error because of NOCLOBBER)
          fails = fails+1
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) fails = fails+1
          call PIO_closefile(pio_file)
        end if
      end if

      test_create = fails

    End Function test_create

end module basic_tests

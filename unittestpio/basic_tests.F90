module basic_tests

  use pio
  use pio_types
  use piolib_mod
  use nf_mod
  use global_vars

  Implicit None
  private
  save

  public :: test_create
  public :: test_open

  Contains

    Function test_create(test_id)
    ! test_create():
    ! * Create an empty file, close it, test that it can be opened
    ! * For netcdf / pnetcdf:
    !   - Create same file with PIO_CLOBBER mode
    !   - Try to create same file with PIO_NOCLOBBER mode, check for error
    ! Routines used in test: PIO_createfile, PIO_openfile, PIO_closefile
    ! Also uses PIO_enddef for [p]netcdf tests

      ! Input / Output Vars
      integer, intent(in) :: test_id
      logical             :: test_create ! True => success

      ! Local Vars
      character(len=str_len) :: filename
      integer                :: iotype, ret_val
      logical                :: passed

      passed = .true.

      filename = fnames(test_id)
      iotype   = iotypes(test_id)

      ! Original create file
      ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename)
      if (ret_val.ne.0) then
        ! File not created successfully 
        passed = .false.
      else
        ! File created
        if (is_netcdf(iotype)) then
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) passed = .false.
        end if
        call PIO_closefile(pio_file)

        ! Test opening of file
        ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, filename, PIO_nowrite)
        if (ret_val.ne.0) then
          ! Error opening file
          passed = .false.
        else
          ! File opened
          call PIO_closefile(pio_file)
        end if
      end if

      ! Recreate file with CLOBBER (netcdf / pnetcdf only)
      if (is_netcdf(iotype)) then
        ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename, PIO_CLOBBER)
        if (ret_val.ne.0) then
          ! File not created successfully
          passed = .false.
        else
          ! File created 
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) passed = .false.
          call PIO_closefile(pio_file)
        end if
      end if

      ! Recreate file with NOCLOBBER
      if (is_netcdf(iotype)) then
        ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename, PIO_NOCLOBBER)
        if (ret_val.eq.0) then
          ! File created (bad, expect an error because of NOCLOBBER)
          passed = .false.
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) passed = .false.
          call PIO_closefile(pio_file)
        end if
      end if

      test_create = passed

    End Function test_create

    Function test_open(test_id)
    ! test_open():
    ! * Try to open file that doesn't exist, check for error
    ! * Open a file with PIO_write, write something, close
    ! * Open a file with PIO_nowrite, try to write, check for error
    ! * For netcdf / pnetcdf:
    !   - Try to open non-netcdf file, check for error
    ! Routines used in test: PIO_initdecomp, PIO_openfile, PIO_write_darray,
    !                        PIO_closefile, PIO_freedecomp
    ! Also uses PIO_createfile for binary tests
    !           PIO_redef, PIO_def_dim, PIO_def_var, PIO_enddef for [p]netcdf tests

      ! Input / Output Vars
      integer, intent(in) :: test_id
      logical             :: test_open

      ! Local Vars
      character(len=str_len) :: filename
      integer                :: iotype, ret_val
      logical                :: passed

      ! Data used to test writing
      integer,          dimension(3) :: data_to_write, compdof
      integer,          dimension(1) :: dims
      type(io_desc_t)                :: iodesc_nCells
      integer                        :: pio_dim
      type(var_desc_t)               :: pio_var

      passed = .true.
      dims(1) = 3*ntasks
      compdof = 3*my_rank+(/1,2,3/)  ! Where in the global array each task writes
      data_to_write = my_rank

      call PIO_initdecomp(pio_iosystem, PIO_int, dims, compdof, iodesc_nCells)

      filename = fnames(test_id)
      iotype   = iotypes(test_id)

      ! Open file that doesn't exist
      ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, "FAKE.FILE", PIO_nowrite)
      if (ret_val.eq.0) then
        if (master_task) write(*,"(A)") "ERROR: Successfully opened file that doesn't exist"
        passed = .false.
      end if

      ! Open existing file, write data to it (for binary file, need to create new file)
      if (is_netcdf(iotype)) then
        ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, filename, PIO_write)
      else
        ret_val = PIO_createfile(pio_iosystem, pio_file, iotype, filename)
      end if
      if (ret_val.ne.0) then
        ! Error opening file
        if (master_task) write(*,"(A)") "Error opening file with PIO_write"
        passed = .false.
      else
        ! File opened
        if (is_netcdf(iotype)) then
          ret_val = PIO_redef(pio_file)
          if (ret_val.ne.0) then
            if (master_task) write(*,"(A)") "ERROR entering definition mode"
            passed = .false.
          end if
          ret_val = PIO_def_dim(pio_file, 'N', 3*ntasks, pio_dim)
          if (ret_val.ne.0) then
            if (master_task) write(*,"(A)") "ERROR defining pio_dim"
            passed = .false.
          end if
          ret_val = PIO_def_var(pio_file, 'foo', PIO_int, &
                                (/pio_dim/), pio_var)
          if (ret_val.ne.0) then
            if (master_task) write(*,"(A)") "ERROR defining pio_var"
            passed = .false.
          end if
          ret_val = PIO_enddef(pio_file)
          if (ret_val.ne.0) then
            if (master_task) write(*,"(A)") "ERROR in PIO_enddef"
            passed = .false.
          end if
        end if
        call PIO_write_darray(pio_file, pio_var, iodesc_nCells, data_to_write, ret_val)
        if (ret_val.ne.0) then
          if (master_task) write(*,"(A)") "ERROR writing data"
          passed = .false.
        end if
        call PIO_closefile(pio_file)
      end if

      ! Open existing file with PIO_nowrite, try to write (netcdf only)
      if (is_netcdf(iotype)) then
        ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, filename, PIO_nowrite)
        if (ret_val.ne.0) then
          ! Error opening file
          if (master_task) write(*,"(A)") "Error opening file with PIO_write"
          passed = .false.
        else
          call PIO_write_darray(pio_file, pio_var, iodesc_nCells, 1+data_to_write, ret_val)
          if (ret_val.eq.0) then
            if (master_task) write(*,"(A,x,A)") "ERROR wrote data to file opened", &
                                                "with PIO_nowrite"
            passed = .false.
          end if
          call PIO_closefile(pio_file)
        end if
      end if

      ! Try to open standard binary file as netcdf (if iotype = netcdf)
      if (is_netcdf(iotype)) then
        ret_val = PIO_openfile(pio_iosystem, pio_file, iotype, &
                               "not_netcdf.ieee", PIO_nowrite)
        if (ret_val.eq.0) then
          if (master_task) write(*,"(A,x,A)") "ERROR: Successfully opened", &
                                              "non-netcdf file as netcdf"
          passed = .false.
        end if
      end if

      call PIO_freedecomp(pio_iosystem, iodesc_nCells)
      test_open = passed

    End Function test_open

    Function is_netcdf(iotype)

      integer, intent(in) :: iotype
      logical             :: is_netcdf

      is_netcdf =  (iotype.eq.PIO_iotype_netcdf) .or. &
                   (iotype.eq.PIO_iotype_pnetcdf)

    End Function is_netcdf

end module basic_tests

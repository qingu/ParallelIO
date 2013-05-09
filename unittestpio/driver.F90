Program pio_unit_test_driver

  use global_vars
  use basic_tests
  use piolib_mod

  Implicit None

  ! local variables
  character(len=str_len) :: err_msg
  integer :: fail_cnt, test_cnt, ios, test_id, ierr, test_val 
  logical :: ltest_bin, ltest_bin_direct, ltest_netcdf, ltest_pnetcdf
  namelist/piotest_nml/ltest_bin,        &
                       ltest_bin_direct, &
                       ltest_netcdf,     &
                       ltest_pnetcdf,    &
                       stride

  ! Set up MPI
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, ntasks , ierr)
  master_task = my_rank.eq.0

  if (master_task) then
    write(*,"(A,I0,A)") "Running unit tests with ", ntasks, " MPI tasks."
    ltest_bin        = .false.
    ltest_bin_direct = .false.
    ltest_netcdf     = .false.
    ltest_pnetcdf    = .false.
    stride           = 1

    open(615, file="input.nl")
    read(615, nml=piotest_nml, iostat=ios)
    if (ios.ne.0) then
      print*, "ERROR reading input.nl, exiting!"
    end if
    close(615)

    if (stride.gt.ntasks) then
      stride = ntasks
      write(*,"(A,x,A,I0)") "WARNING: stride value in namelist is larger than", &
                         "than number of MPI tasks, reducing stride to ", stride
    end if

! Ignore namelist values if PIO not built with correct options
! (i.e. don't test pnetcdf if not built with pnetcdf)
#ifndef USEMPIIO
    if (ltest_bin.or.ltest_bin_direct) then
      write(*,"(A,x,A)") "WARNING: can not test binary files because PIO", &
                         "was not compiled with -DUSEMPIIO"
      ltest_bin        = .false.
      ltest_bin_direct = .false.
    end if
#endif
#ifndef _NETCDF
    if (ltest_netcdf) then
      write(*,"(A,x,A)") "WARNING: can not test netcdf files because PIO", &
                         "was not compiled with -D_NETCDF"
      ltest_netcdf     = .false.
    end if
#endif
#ifndef _PNETCDF
    if (ltest_pnetcdf) then
      write(*,"(A,x,A)") "WARNING: can not test pnetcdf files because PIO", &
                         "was not compiled with -D_PNETCDF"
      ltest_pnetcdf    = .false.
    end if
#endif
    write(*,"(A)") "------"

  end if

  call MPI_Bcast(ios,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  if (ios.ne.0) then
    call MPI_Abort(MPI_COMM_WORLD)
  end if

  ltest(BINARY)  = ltest_bin
  ltest(BINDIR)  = ltest_bin_direct
  ltest(NETCDF)  = ltest_netcdf
  ltest(PNETCDF) = ltest_pnetcdf
  call MPI_Bcast(ltest,ntest,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
  call MPI_Bcast(stride,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  niotasks = ntasks/stride

  ! Set up PIO
  call PIO_init(my_rank,        & ! MPI rank
                MPI_COMM_WORLD, & ! MPI communicator
                niotasks,       & ! Number of iotasks (ntasks/stride)
                0,              & ! num_aggregator (?)
                stride,         & ! Stride
                PIO_rearr_box,  & ! rearr
                pio_iosystem)     ! iosystem

  call PIO_seterrorhandling(pio_iosystem, PIO_BCAST_ERROR)

  fail_cnt = 0
  test_cnt = 0

  do test_id=1,ntest
    if (ltest(test_id)) then
      ! Make sure i is a valid test number
      select case (test_id)
        case (BINARY)
          if (master_task) &
            write(*,"(A)") "Testing PIO's binary input / output:"
        case (BINDIR)
          if (master_task) &
            write(*,"(A)") "Testing PIO's direct binary input / output:"
        case (NETCDF)
          if (master_task) &
            write(*,"(A)") "Testing PIO's netcdf input / output:"
        case (PNETCDF)
          if (master_task) &
            write(*,"(A)") "Testing PIO's pnetcdf input / output:"
        case DEFAULT
          if (master_task) &
            write(*,"(A,I0)") "Error, not configured for test #", test_id
          call MPI_Abort(MPI_COMM_WORLD)
      end select

      ! test_create()
      if (master_task) write(*,"(3x,A,x)",advance="no") "testing PIO_createfile..."
      call parse(test_create(test_id, err_msg), fail_cnt, err_msg)

      ! test_open()
      if (master_task) write(*,"(3x,A,x)", advance="no") "testing PIO_openfile..."
      call parse(test_open(test_id, err_msg), fail_cnt, err_msg)

      if (master_task) write(*,*) ""

    end if ! ltest(test_id)

  end do

  if (master_task) then
    write(*,"(A,I0)") "Total failure count: ", fail_cnt
    if (fail_cnt.eq.0) then
      write(*,"(A)") "PASSED unit testing."
    else
      write(*,"(A)") "FAILED unit testing."
    end if
  end if

  call PIO_finalize(pio_iosystem, ierr)
  call MPI_Finalize(ierr)

  Contains

    Subroutine parse(test_passed, fail_counter, err_msg)

      logical,          intent(in)    :: test_passed
      integer,          intent(inout) :: fail_counter
      character(len=*), intent(in)    :: err_msg

      if (master_task) then
        if (test_passed) then
          write(*,"(A)") "success!"
        else
          write(*,"(A)") "FAILURE: " // trim(err_msg)
          fail_counter = fail_counter+1
        end if
      end if

    End Subroutine parse

End Program pio_unit_test_driver

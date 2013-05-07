Program pio_unit_test_driver

  use netcdf_test

  Implicit None

  integer, parameter :: str_len = 255
  character(len=str_len), dimension(3) :: test_file_name = (/"piotest_bin.ieee  ", &
                                                             "piotest_netcdf.nc ",  &
                                                             "piotest_pnetcdf.nc"/)

  integer, parameter :: BINARY=1, NETCDF=2, PNETCDF=3
     
  integer nfail, ntest, ios
  logical :: ltest_bin, ltest_netcdf, ltest_pnetcdf

  namelist/piotest_nml/ltest_bin, ltest_netcdf, ltest_pnetcdf

  ltest_bin     = .false.
  ltest_netcdf  = .false.
  ltest_pnetcdf = .false.

  open(615, file="input.nl")
  read(615, nml=piotest_nml, iostat=ios)
  if (ios.ne.0) then
    print*, "ERROR reading input.nl, exiting!"
    stop 1
  end if
  close(615)

  nfail = 0
  ntest = 0

  if (ltest_bin) then
    write(*,"(A)") "Testing PIO's ability to create / read / write a binary file."
  end if

  if (ltest_netcdf) then
    write(*,"(A)") "Testing PIO's ability to create / read / write a netcdf file."
    nfail = test_nccreate(trim(test_file_name(NETCDF)))
  end if

  if (ltest_netcdf) then
    write(*,"(A,x,A)") "Testing PIO's ability to create / read / write a", &
                       "netcdf file using the pnetcdf library."
  end if

End Program pio_unit_test_driver

Program pio_unit_test_driver

  Use netcdf_test

  Implicit None

  integer, parameter :: str_len = 255
  character(len=str_len), dimension(3) :: test_file_name = (/"piotest_bin.ieee  ", &
                                                             "piotest_netcdf.nc ",  &
                                                             "piotest_pnetcdf.nc"/)

  integer, parameter :: BINARY=1, NETCDF=2, PNETCDF=3
     
  integer nfail, ntest
  logical :: ltest_bin, ltest_netcdf, ltest_pnetcdf

  namelist/piotest_nml/ltest_bin, ltest_netcdf, ltest_pnetcdf

  ltest_bin = .false.
  ltest_netcdf = .false.
  ltest_pnetcdf = .false.

  read(*, nml=piotest_nml)

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

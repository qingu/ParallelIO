module global_vars

  use pio_types

  Implicit None
  public
  save

  include 'mpif.h' ! _EXTERNAL

  integer, parameter :: str_len = 255, ntest=4
  integer, parameter :: BINARY =1, &
                        BINDIR =2, &
                        NETCDF =3, &
                        PNETCDF=4

  ! MPI Variables
  integer :: my_rank, ntasks
  logical :: master_task

  ! PIO Variables
  integer               :: stride, niotasks
  type(iosystem_desc_t) :: pio_iosystem
  type(file_desc_t)     :: pio_file

  ! Arguments for the different tests
  character(len=str_len), dimension(ntest) :: fnames = (/"piotest_bin.ieee  ", &
                                                         "piotest_bin2.ieee ", &
                                                         "piotest_netcdf.nc ", &
                                                         "piotest_pnetcdf.nc"/)
  integer, dimension(ntest) :: iotypes = (/PIO_iotype_pbinary,        &
                                           PIO_iotype_direct_pbinary, &
                                           PIO_iotype_netcdf,         &
                                           PIO_iotype_pnetcdf/)
  logical, dimension(ntest) :: ltest

end module global_vars

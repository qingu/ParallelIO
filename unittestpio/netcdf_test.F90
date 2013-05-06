module netcdf_test

  Use pio_types

  Implicit None
  private
  save

  public :: test_nccreate 

  Contains

    Function test_nccreate(filename)

      character(len=*), intent(in) :: filename
      integer test_nccreate

      write(*,"(x,A,x,a)") "This test will create ", filename
      ! Just checking to make sure using pio_types is working properly
      print*, "PIO_CLOBBER = ", PIO_CLOBBER
      print*, "PIO_NOCLOBBER = ", PIO_NOCLOBBER
      test_nccreate = 0

    End Function test_nccreate

end module netcdf_test

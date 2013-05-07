module basic_tests

  use pio_types

  Implicit None
  private
  save

  public :: test_create 

  Contains

    Function test_create(filename)

      character(len=*), intent(in) :: filename
      integer test_create

      write(*,"(x,A,x,a)") "This test will create ", filename
      ! Just checking to make sure using pio_types is working properly
      print*, "PIO_CLOBBER = ", PIO_CLOBBER
      print*, "PIO_NOCLOBBER = ", PIO_NOCLOBBER
      test_create = 0

    End Function test_create

end module basic_tests

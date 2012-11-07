#define __PIO_FILE__ "pio_buffer.F90"
module pio_buffer
#ifndef NO_MPIMOD
  use mpi   ! _EXTERNAL
#endif
  use pio_support, only : CheckMPIReturn, Debug, piodie
  use pio_kinds, only : pio_offset
  use pio_types, only : file_desc_t

#ifdef _PNETCDF
  use pnetcdf    ! _EXTERNAL
#endif

  implicit none
#ifdef NO_MPIMOD
  include 'mpif.h'   ! _EXTERNAL
#endif
  private

#ifdef PIO_MANAGE_BUFFER
!>
!! @private
!<
  interface add_data_to_buffer
! TYPE real,int,double
     module procedure add_data_to_buffer_{TYPE}
  end interface

#endif
  logical :: DebugBuffers=.true.

  integer :: total_buffsize=0
  integer(kind=pio_offset) :: pio_buffer_size_limit= 100000000   ! 100MB default

  public :: pio_set_buffer_size_limit
  public :: pio_buffer_add_request
  public :: pio_buffer_attach
  public :: pio_buffer_flush
  public :: pio_buffer_check
contains

  subroutine pio_set_buffer_size_limit(newlimit)
    integer, intent(in) :: newlimit 

    if(newlimit<0) then
       call piodie(__PIO_FILE__,__LINE__,' bad value to buffer_size_limit',newlimit)
    end if
    pio_buffer_size_limit=newlimit

  end subroutine pio_set_buffer_size_limit

  
  subroutine pio_buffer_add_request(File, request)
    use pio_types, only : file_desc_t, max_buffered_requests
    type(file_desc_t) :: File
    integer, intent(in) :: request
    integer :: maxrequestcnt
    integer :: mpierr

    if(request/=MPI_REQUEST_NULL) then
       File%request_cnt=File%request_cnt+1

       if(File%request_cnt>=MAX_BUFFERED_REQUESTS) then
          call piodie(__FILE__,__LINE__,"maximum outstanding pio buffer limit reached")
       endif


       File%requests(File%request_cnt)=request
    end if


    if(DebugBuffers) then
       call MPI_ALLREDUCE(File%request_cnt,maxrequestcnt,1,MPI_INTEGER,MPI_MAX,File%iosystem%io_comm, mpierr)
       call CheckMPIreturn('close_mpiio: after call to file_close: ',mpierr)        

       if(maxrequestcnt==MAX_BUFFERED_REQUESTS) then
          ! Flush write buffer here
       end if
    end if


  end subroutine pio_buffer_add_request
  
#ifdef PIO_MANAGE_BUFFER
  ! TYPE real,int,double  
  subroutine add_data_to_buffer_{TYPE} (File, IOBUF, request)
    use pio_types, only : io_data_list
    type(file_desc_t) :: File
    {VTYPE}, pointer :: IOBUF(:)
    integer, intent(in) :: request
    integer :: cnt, mpierr, maxbuffsize, this_buffsize
    type(io_data_list), pointer :: ptr
#ifdef MEMCHK	
    call GPTLget_memusage(msize, rss, mshare, mtext, mstack)
    if(rss>lastrss) then
       lastrss=rss
       print *,__PIO_FILE__,__LINE__,'mem=',rss
    end if
#endif

    if(.not. associated(File%data_list_top)) then
       allocate(file%data_list_top)
       ptr => file%data_list_top
    else
       ptr => file%data_list_top
       do while(associated(ptr%next))
          ptr => ptr%next
       end do
       
       allocate(ptr%next)
       ptr=>ptr%next
       nullify(ptr%next)
    end if
    ptr%request = request
    File%request_cnt=File%request_cnt+1
    ptr%data_{TYPE} => IOBUF
    this_buffsize = size(iobuf)*sizeof(iobuf(1))
    file%buffsize=file%buffsize+this_buffsize
    total_buffsize = total_buffsize+this_buffsize
    
    call MPI_ALLREDUCE(total_buffsize,maxbuffsize,1,MPI_INTEGER,MPI_MAX,file%iosystem%io_comm, mpierr)

    if(maxbuffsize > pio_buffer_size_limit) then
       call darray_write_complete(File)
    endif

    if(debug) print *,__FILE__,__LINE__,'buffsize = ',file%buffsize,file%request_cnt
#ifdef MEMCHK	
    call GPTLget_memusage(msize, rss, mshare, mtext, mstack)
    if(rss>lastrss) then
       lastrss=rss
       print *,__PIO_FILE__,__LINE__,'mem=',rss
    end if
#endif

  end subroutine add_data_to_buffer_{TYPE}
#endif
  subroutine pio_buffer_attach(File)
    implicit none
    type(file_desc_t) :: File
    integer :: ierr
#ifdef _PNETCDF
    ierr = nfmpi_buffer_attach(File%fh, pio_buffer_size_limit)
#endif

  end subroutine pio_buffer_attach


  subroutine pio_buffer_flush(File)
    type(file_desc_t) :: File
    integer, pointer :: status(:)
    integer :: ierr
#ifdef _PNETCDF
    allocate(status(file%request_cnt))
    ierr  = nfmpi_wait_all(file%fh, file%request_cnt, file%requests, status)
    deallocate(status)
    file%request_cnt=0
#endif
    
  end subroutine pio_buffer_flush


  subroutine pio_buffer_check(File)
    type(file_desc_t) :: File
    integer :: ierr

    integer(kind=pio_offset) :: bufsize, maxbufsize

#ifdef _PNETCDF

    ierr = nfmpi_inq_buffer_usage(File%fh,bufsize)

    call MPI_ALLREDUCE(bufsize,maxbufsize,1,MPI_INTEGER8,MPI_MAX,file%iosystem%io_comm, ierr)

    if(maxbufsize > pio_buffer_size_limit) then
       call pio_buffer_flush(File)
    endif
    


#endif    


  end subroutine pio_buffer_check





end module pio_buffer

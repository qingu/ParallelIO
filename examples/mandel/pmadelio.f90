subroutine chunk1 (xsegment, ysegment, x1, rank, startx, starty, endx, endy)

implicit none

integer, intent(in)::xsegment, ysegment, rank, x1
integer:: startx, starty, endx, endy

starty = rank/x1
starty = starty*ysegment+1

startx = mod(rank, x1)
startx = startx*xsegment+1

endx = startx + xsegment -1
endy = starty + ysegment-1 


!write (*,*) 'subroutine values for starty, endy, startx, endx',starty,endy, startx, endx

 
end subroutine chunk1


module mandelio
  use pio
  implicit none
  public :: write_target_netcdf 
contains
  subroutine write_target_netcdf(xsegment, ysegment,x1, rank,ntasks, startx,endx,starty,endy,globalx,globaly,field)
    use netcdf
    use mpi 
   
    implicit none
    
    integer, intent(in) :: rank, ntasks, startx, endx, starty, endy,globalx,globaly
    integer, intent(in), pointer :: field(:,:)
    integer :: xsegment, ysegment, x1
    integer :: ierr, ncid, dimx, dimy, varid, status,i,n
    integer :: starts(startx,starty),value
    integer :: strt(2), cnt(2)
    integer stat(MPI_STATUS_SIZE)
    integer :: aggregator, iotype

    type (iosystem_desc_t), intent(out):: iosystem
    type (file_desc_t), intent(out):: file
    type (var_desc_t), intent(inout) :: variableid 
 
    call PIO_init(rank, MPI_COMM_WORLD,ntasks,aggregator, 1,&PIO_rearr_none, iosystem) 

    value = (endx-startx+1)*(endy-starty+1)
       
    call PIO_createfile(iosystem,file,PIO_iotype_pnetcdf,'pmandel.nc')

    call PIO_def_dim(file, 'plon', globalx, dimx)

    call PIO_def_dim(file, 'plat', globaly, dimy)
    
    call  PIO_def_var_md(file, 'pmandelbrot', PIO_int, (/dimx,dimy/), variableid)
    
    call PIO_enddef(file) 
  
    call chunk1(xsegment, ysegment,x1,rank,strt(1),strt(2),endx,endy)

    call PIO_initdecomp_1dof_bin(iosystem, PIO_int,(/globalx,globaly/),   
	!write (*,*)'Values for startx, endx, starty, endy', startx,endx, starty, endy 
       
    cnt = (/endx-startx+1,endy-starty+1/)

	
!       ierr = nf90_put_var(ncid, varid, field, start=strt, count=cnt)
!       if(ierr /= nf90_noerr) then 
!          print *,'after nf90_put_var for rank ZERO: ',nf90_strerror(ierr) 
!       endif

!          call mpi_recv(field,value,MPI_INT, i, 123+i,MPI_COMM_WORLD,stat, ierr )
!          write (*,*)'After call,  startx, endx, starty, endy, i ', startx,endx, starty, endy, i, minval(field),maxval(field) 

       call pio_close(file)
!      call mpi_send(field, value, MPI_INT, 0, 123+rank, MPI_COMM_WORLD, ierr )

  end subroutine write_target_netcdf

end module mandelio

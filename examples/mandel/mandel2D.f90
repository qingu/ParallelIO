	subroutine chunk(xsegment,ysegment, x1, rank, xstart, ystart, xend, yend)
	implicit none
	integer,intent(in):: xsegment, ysegment, rank, x1
	integer:: xstart, ystart, xend, yend
	
	ystart = rank/x1 
	ystart = ystart*ysegment+1
		
	xstart = mod(rank, x1)
	xstart = xstart*xsegment+1

	xend = xstart + xsegment-1
	yend = ystart + ysegment-1
	
	end subroutine chunk



	program mandel2d
        use mandelio
!	use mpi
	
	implicit none
        include 'mpif.h'
	integer, parameter:: width = 400, height = 400
	integer :: num_tasks, rank, ierr, rc, xsegment, ysegment, xprocs, xstart, ystart, xend, yend
	integer, parameter:: x1 = 4, y1 = 2, x2 = 4, y2 = 4
	integer, parameter:: escape_radius = 400
	integer, parameter:: n_max = 1000
	integer :: i, j, n, temp, distance
	complex :: c,z
	double precision :: start_time, end_time, time,compute_start, compute, io_start, io, total
	integer, pointer :: result(:,:)

	call MPI_INIT(ierr)

	if(ierr .ne. MPI_SUCCESS) then
		print *,'Error starting MPI. Terminating'
		call MPI_ABORT(MPI_COMM_WORLD,rc, ierr)
	end if

	call MPI_COMM_SIZE(MPI_COMM_WORLD, num_tasks, ierr)

	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	
	if(num_tasks .EQ. 8) then
	xsegment = width/x1
	ysegment = height/y1
	end if

	if(num_tasks .EQ. 16) then
	xsegment = width/x2
	ysegment = height/y2
	end if
	
	!ystart = rank/x1
	!ystart = ystart*ysegment+1
	
	!xstart = mod(rank,x1)
	!xstart = xstart*xsegment+1
	
	!xend = xstart + xsegment-1
	!yend = ystart + ysegment-1
	
	call chunk(xsegment, ysegment, x1, rank,xstart, ystart, xend, yend)
	compute_start = MPI_Wtime()
        allocate(result(xsegment,ysegment))

	

	do 20 i = 1, xsegment
		do 10 j= 1,ysegment
		c = CMPLX((i+xstart-1)*(4.0/real(width))-2,(j+ystart-1)*(4.0/real(height))-2)
		z = CMPLX(0,0)
		n =0
		z = z*z
		
		DO WHILE(ABS(z)<escape_radius .AND. (n<=n_max))
		
		z = z**2 +c
		n = n+1
		
		end do
		
		result(i,j)=n
		
		10 continue
20	continue

	compute = MPI_Wtime()-compute_start
	!time = start_time - end_time
	
	io_start = MPI_Wtime()	

!        do j=ystart,yend
!           do i=xstart,xend
!              result(i-xstart+1,j-ystart+1) = rank*10000000  +(j-1)*height+i
!           end do
!        end do


	 call write_target_netcdf(xsegment, ysegment,x1, rank,num_tasks,xstart,xend,ystart,yend, width, height,result) 
	io = MPI_Wtime()-io_start


        total = io + compute	
	 
	
	print *, 'Rank', rank, 'computation time', compute,'io time ',io,'total time',total
	deallocate(result)

	call MPI_FINALIZE(ierr) 
	end program mandel2d

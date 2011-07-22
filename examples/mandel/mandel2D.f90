	subroutine chunk(xsegment,ysegment, x1, rank, xstart, ystart, xend, yend)
	implicit none
	integer, intent(in):: xsegment, ysegment, rank, x1
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
       !integer, parameter:: width = 400, height = 400
        integer :: width, height,x1,y1,x2,y2,x3, y3,x4,y4	
        integer :: num_tasks, rank, ierr, rc, xsegment, ysegment, xprocs, xstart, ystart, xend, yend
	!integer, parameter:: x1 = 4, y1 = 2, x2 = 4, y2 = 4
	integer, parameter:: n_max = 1000
	integer :: i, j, n, temp, distance, stride, iotasks, escape_radius, squareroot
	complex :: c,z
	double precision :: start_time, end_time, time,compute_start, compute, io_start, io, total,min, max, avg
	integer, pointer :: result(:,:)
        
        !Namelist group values
        namelist/values/ height, width, stride, iotasks, escape_radius

        open (1, file='mandel.nml', status='old')
        read(1, nml=values)
        close(1)

	call MPI_INIT(ierr)

	if(ierr .ne. MPI_SUCCESS) then
		print *,'Error starting MPI. Terminating'
		call MPI_ABORT(MPI_COMM_WORLD,rc, ierr)
	end if

	call MPI_COMM_SIZE(MPI_COMM_WORLD, num_tasks, ierr)
        

	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      
       x1 =floor(sqrt(real(num_tasks)))
      
       if (mod(x1, 2) .ne. 0)  then
       squareroot = num_tasks/2
       x1 = floor(sqrt(real(squareroot)))
       x1 = 2 * x1 
       !print *, 'not divisible by 2'
       end if


      ! if (rank > mod (width, x1))then
      ! print *, 'here'
      ! x1 = x1 + 1
      ! end if
       
      ! print *, 'the value after mod is ', squareroot, 'value of width',  width, 'value of x1', x1 

       y1 = num_tasks/x1
       !print *, ' The new values for x1',x1, 'y1 ', y1 
       !print *, 'The squareroot', squareroot, 'number of tasks', num_tasks
          
     
        if(num_tasks .EQ. 8) then
        x1 = 4
        y1 = 2 
	end if

	if(num_tasks .EQ. 16) then
        x1 = 4
        y1 = 4	
	end if
 
!	if(num_tasks .EQ. 32) then
!        x1 = 8
!        y1 = 4	
!	end if
       	
       	
	if(num_tasks .EQ. 64) then
        x1 = 8
        y1 = 8	
	end if
       	
	if(num_tasks .EQ. 128) then
        x1 =16 
        y1 =8 
	end if
 
	if(num_tasks .EQ. 256) then
        x1 = 16 
        y1 = 16	
	end if
       
	if(num_tasks .EQ. 512) then
        x1 = 16 
        y1 = 32	
	end if
       
        xsegment = width/x1
	ysegment = height/y1
       	
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
	
	
	!print *, 'Rank', rank, 'computation time', compute,'io time ',io,'total time',total
	io_start = MPI_Wtime()	

	call write_target_netcdf(xsegment, ysegment,x1, rank,num_tasks,xstart,xend,ystart,yend, width, height,result) 
        
        !print *, 'Values for xsegment, ysegment, x1, rank, num_tasks, xstart, xend, ystart, yend, width, height, result', xsegment, ysegment, x1, rank, num_tasks,xstart, xend, ystart, yend, width, height
        io = MPI_Wtime()-io_start


        total = io + compute	
	
        call MPI_Reduce(compute, min, 1, MPI_double,MPI_MIN, 0, MPI_COMM_WORLD, ierr) 
        
        call MPI_barrier(MPI_COMM_WORLD, ierr)
        
        call MPI_Reduce(compute, max, 1, MPI_double,MPI_MAX,0, MPI_COMM_WORLD, ierr) 

        call MPI_barrier(MPI_COMM_WORLD, ierr)

        call MPI_Reduce (compute, avg, 1, MPI_double, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

         avg = avg/num_tasks

	print *, 'Rank', rank, 'computation time', compute,'io time ',io,'total time',total


        if (rank ==0) then
        print *, 'the value for minimum' , min
        print *, 'the max value is', max
        print *, 'the average is ', avg
        end if
      
        deallocate(result)
        	
        call MPI_FINALIZE(ierr) 
	
        end program mandel2d

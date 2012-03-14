program Test_Lib
	use pio
	use pio_kinds
	!use piovdc
	implicit none
	
	include 'mpif.h'
	type (Var_desc_t)	:: var_handle_no_comp
	type (VDC_Var_desc_t)	:: var_handle
	character		:: vdf_path*100, binary_path*100
	type (File_desc_t)	:: file_handle, file_handle_no_comp
	type (IOsystem_desc_t)	:: iosystem
	type (io_desc_t)	:: iodesc
	integer(i4)		:: rank, ierr, iostat,  dim_ids(3), nioprocs, nprocs, dims(3), dpp, n, bsize(3)
	integer(i4),allocatable		:: compdof(:)
	real (r4),  allocatable :: array(:), read_array(:)
#ifdef 	DEBUG
	double precision	:: start, end, temp
#endif
	
	!set locals for vdc compression
	dims = (/2048, 2048, 2048/)
	vdf_path = '/lustre/janus_scratch/ypolius/piovdc/libbench.vdf'
	binary_path = '/lustre/janus_scratch/ypolius/piovdc/benchdata.nc'
	nioprocs = 64
	
	!call PIO_SetDebugLevel(1)

	call MPI_init(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'Initiating PIO...'
	endif
#endif

	call PIO_init(rank, MPI_COMM_WORLD, nioprocs, nioprocs, 1, PIO_rearr_box, iosystem, 0, dims)		


#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'PIO Initiated procs: ', nioprocs
	endif
#endif
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

	dpp = product(dims) / nprocs !INT(temp_dpp)
	
	if(allocated(array)) then
		deallocate(array)
	endif

	allocate(array(dpp))
	allocate(read_array(dpp))

#ifdef DEBUG
	if(rank .eq. 0 ) then
		print *, 'Allocated write and read arrays'
		print *, 'File type', file_handle%iotype
	endif
#endif
	if(allocated(compdof)) then
	    deallocate(compdof)
	endif

	allocate(compdof(dpp))

#ifdef DEBUG	
	if(rank .eq. 0 ) then
	    print *, 'Allocated compdof'
	    print *, 'Filling compdof array...dpp-comp: ', dpp, ' dpp-io: ', product(dims)/nioprocs, ' dims: ' , dims, ' int limit: ' , huge(dims(1))
	endif
#endif

	do n = 1, dpp
	    compdof(n) =  n + rank * dpp !INT(REAL(n) + REAL(rank) * REAL(dpp))
	    array(n) = 53.0
		if(compdof(n) .lt. 0) then
			print *, ' n: ' , n , ' compdof(n): ' , compdof(n), ' array(n): ', array(n)
		end if
	end do

#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'Filled compdof array'
	    print *, 'Filled data array'
	endif
#endif
	
#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'Initializing decomposition...'
	endif
#endif

#ifdef DEBUG
	start = MPI_WTIME()
#endif
	call PIO_initdecomp(iosystem, PIO_real, dims, compdof, iodesc)

#ifdef DEBUG
	if(rank .eq. 0) then 
	    print *, 'Decomposition initialized'
	    print *, 'Creating vdf file'
	endif
        print *, 'Rank: ', rank, 'Decomposition rearrangment runtime: ' , MPI_WTIME() - start
#endif
	ierr = PIO_CreateFile(iosystem, file_handle, PIO_iotype_vdc2, vdf_path, PIO_clobber)

#ifdef DEBUG
	if(rank .eq. 0) then 
	    print *, 'VDF file created'
	    print *, 'Opening vdf var for writing...(vx0)'
	endif
#endif
	!VDC WRITING DOES NOT REQUIRE CREATING DIMS, THERE ARE ALWAYS 3, dims = (/x, y, z/)
	iostat = PIO_def_var(file_handle, 'vx' , PIO_real,var_handle)

#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'Opened var for writing'
	endif
#endif

	ierr = PIO_enddef(file_handle)

#ifdef DEBUG
	if(rank .eq. 0 ) then
	    print *, 'Ended VDF definition'
	endif
	start = MPI_WTIME()
#endif

	call PIO_write_darray(file_handle, var_handle, iodesc,  array, iostat, 0)

#ifdef DEBUG
	print *, 'Rank: ', rank, ' vdc write time: ', MPI_WTIME() - start
#endif

#ifdef DEBUG
	if(rank .eq. 0) then
		print *, 'Attempting to read back data'
	endif
	start = MPI_WTIME()
#endif
	call PIO_read_darray(file_handle, var_handle, iodesc,  read_array, iostat, 0)

#ifdef DEBUG
	print *, 'Rank: ', rank, ' vdc read time: ' , MPI_WTIME() - start
#endif
	ierr = PIO_CreateFile(iosystem, file_handle_no_comp, PIO_iotype_pnetcdf, binary_path, PIO_clobber)
	iostat = PIO_def_dim(file_handle_no_comp, 'z', dims(3), dim_ids(3))
	iostat = PIO_def_dim(file_handle_no_comp, 'y', dims(2), dim_ids(2))
	iostat = PIO_def_dim(file_handle_no_comp, 'x', dims(1), dim_ids(1))
	iostat = PIO_def_var(file_handle_no_comp, 'vx', PIO_real, dim_ids, var_handle_no_comp)
	ierr = PIO_enddef(file_handle_no_comp)

#ifdef DEBUG
	start = MPI_WTIME()
#endif

	call pio_write_darray(file_handle_no_comp, var_handle_no_comp, iodesc, array, iostat)

#ifdef DEBUG

	print *, 'Rank: ', rank , ' pure NC write_darray runtime: ' , MPI_WTIME() - start

#endif

	!clean up PIO and MPI
	call PIO_CloseFile(file_handle_no_comp)

#ifdef DEBUG
	if(rank .eq. 0 ) then
		print *, 'Closed PIO file'
	endif
#endif

	call PIO_Finalize(iosystem, ierr)

#ifdef DEBUG
	if(rank .eq. 0 ) then
		print *, 'Finalized PIO'	
	endif
#endif

	deallocate(compdof)
	deallocate(array)

	!force non-io tasks to barrier until io is complete

	call MPI_Finalize(ierr)
	stop
endprogram

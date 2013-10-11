module pio_timeseries
  use pio_kinds, only : char_len
  use pio_types, only : iosystem_desc_t, file_desc_t, timeseries_var_t, timeseries_file_t, var_desc_t
  use pio_support, only: piodie
  implicit none
  integer, parameter :: MAX_FILES=10, MAX_VARS=500
  
contains
  subroutine pio_timeseries_init(iosys, nlfilename)
#ifndef NO_MPIMOD
    use mpi            ! _EXTERNAL
#else
    include 'mpif.h'   ! _EXTERNAL
#endif
    type(iosystem_desc_t) :: iosys
    character(len=*), optional :: nlfilename

    logical :: exists
    character(len=char_len) :: nlfile, filename, include_vars(MAX_VARS), exclude_vars(MAX_VARS)
    integer :: i, varcnt, ierr, found_vars
    integer, parameter :: stderr=6

    type(timeseries_file_t), pointer :: timeseries
    logical, save :: initialized=.false.

    namelist /pio_timeseries/ filename, include_vars, exclude_vars
    filename = ' '
    include_vars = ' '
    exclude_vars = ' '

    if(iosys%io_rank==0) then
       if(present(nlfilename)) then
          print *,' Reading pio_timeseries namelist from', trim(nlfilename)
          nlfile=nlfilename
       else	
          nlfile='timeseries.nl'
       end if
       inquire(file=nlfile, exist=exists)
       if(exists) then
          open(27, file=nlfile, status='old')
          read(27, pio_timeseries)
          close(27)
       end if
    end if
    call mpi_bcast(filename,char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)
    call mpi_bcast(include_vars,MAX_VARS*char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)
    call mpi_bcast(exclude_vars,MAX_VARS*char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)

    allocate(timeseries)
    timeseries%filename = filename
   
    varcnt=0
    if(len_trim(include_vars(1)) > 0 .and. len_trim(exclude_vars(1)) > 0) then
       call piodie(__FILE__,__LINE__,'only one of include_vars and exclude_vars can be used')       
    endif
    do i=1,MAX_VARS
       if(len_trim(exclude_vars(i))>0) then
          varcnt=varcnt+1
       else  if(len_trim(include_vars(i))>0) then
          varcnt=varcnt-1
       endif
    end do
    if(varcnt> 0) then
       allocate(timeseries%exclude_vars(varcnt))
       timeseries%exclude_vars = exclude_vars(1:varcnt)
    else if(varcnt<0) then
       allocate(timeseries%varlist)
       timeseries%varlist%name = include_vars(1)
       do i=2,-varcnt
          call add_var_to_timeseries(timeseries%varlist, include_vars(i))
       enddo
    else
       deallocate(timeseries)
    end if
    if(associated(timeseries)) then
       if(associated(iosys%timeseries_list)) then
          call add_timeseries_to_list(iosys%timeseries_list,timeseries)
       else
          iosys%timeseries_list => timeseries
       endif
    endif

  end subroutine pio_timeseries_init
  subroutine add_var_to_timeseries(varlist, var, vardesc)
    type(timeseries_var_t), intent(inout), target :: varlist
    character(len=*), intent(in) :: var
    type(var_desc_t), target, optional :: vardesc
    
    type(timeseries_var_t), pointer :: avar
    
    avar => varlist

    do while(associated(avar%next))
       avar => avar%next
    enddo
    
    allocate(avar%next)
    avar => avar%next
    avar%name = var
    if(present(vardesc)) then
       avar%vardesc => vardesc
    endif
    
    

  end subroutine add_var_to_timeseries
        
  subroutine add_timeseries_to_list(timeseries_list,timeseries)
    type(timeseries_file_t), target :: timeseries_list
    type(timeseries_file_t), pointer :: timeseries
    type(timeseries_file_t), pointer :: tptr
    integer :: i, its
    
   tptr => timeseries_list
   do while(associated(tptr%next))
      tptr => tptr%next
   enddo
   tptr%next => timeseries


  end subroutine add_timeseries_to_list

  logical function check_if_timeseries_file(File) result(match)
    type(file_desc_t) :: file

    type(timeseries_file_t), pointer :: tptr
    integer :: i
    type(timeseries_var_t), pointer :: varlist

    match=.false.

    if( associated(file%tsfile)) then
       tptr => file%tsfile
       match=.true.
    else
       if(associated(file%iosystem%timeseries_list)) then
          tptr => file%iosystem%timeseries_list
          do while(associated(tptr))
             ! Does the filename substring from the namelist match this filename
             i=index(trim(file%filepath),trim(tptr%filename)) 
             if(i > 0) then
                file%tsfile=>tptr
                match=.true.
                exit
             endif
             tptr=>tptr%next
          end do
       endif
    endif
  end function check_if_timeseries_file

  logical function check_if_timeseries_var(tsFile, vardesc,dimids) result(match)
    type(timeseries_file_t), intent(inout), target :: tsfile
    type(var_desc_t), intent(in), target :: vardesc
    integer, intent(in) :: dimids(:)

    type(timeseries_var_t), pointer :: varlist
    
    match=.false.
! A variable is only a candidate if the unlimited dim is defined

    if(any(dimids == tsfile%unlim_dimid)) then
! If the exclude_vars list is present and this variable isn't on it, add it to the linked list.
       if(allocated(tsFile%exclude_vars)) then
          if(.not. any(tsFile%exclude_vars .eq. vardesc%name)) then
             if(allocated(tsfile%varlist)) then
                call add_var_to_timeseries(tsFile%varlist, vardesc%name, vardesc)
             else
                allocate(tsfile%varlist)
                tsfile%varlist%vardesc=>vardesc
             endif
             match=.true.
          endif
       else             
! if the exclude varlist isn't present then check to see if the variable is on the include varlist
          varlist => tsFile%varlist
          do while(associated(varlist))
             if(varlist%name .eq. vardesc%name) then
                match=.true.
                varlist%vardesc=>vardesc
                exit
             endif
             varlist=>varlist%next
          end do
       endif
    endif

  end function check_if_timeseries_var
end module pio_timeseries

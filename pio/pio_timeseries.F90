module pio_timeseries
  use pio_kinds, only : char_len
  use pio_types, only : iosystem_desc_t, file_desc_t, timeseries_var_t, timeseries_file_t, var_desc_t
  implicit none
  integer, parameter :: MAX_FILES=10, MAX_VARS=500
  
!
! action values are
! 
  integer, parameter :: action_none   =0
  integer, parameter :: action_include=1
  integer, parameter :: action_exclude=2
  integer, parameter :: action_global =3
  


  type(timeseries_file_t), target :: timeseries_list

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
    character(len=char_len) :: nlfile, filename, include_vars(MAX_VARS), exclude_vars(MAX_VARS), global_vars(MAX_VARS)
    integer :: i, varcnt, ierr, found_vars
    integer, parameter :: stderr=6

    type(timeseries_file_t), pointer :: timeseries
    logical, save :: initialized=.false.

    namelist /pio_timeseries/ filename, include_vars, exclude_vars, global_vars
!    if(initialized) return
!    initialized=.true.
    filename = ' '
    include_vars = ' '
    exclude_vars = ' '
    global_vars = ' '

    if(iosys%io_rank==0) then
       if(present(nlfilename)) then
          print *,' Reading pio_timeseries namelist from', trim(nlfilename)
          nlfile=nlfilename
       else	
          nlfile='timeseries.nl'
       end if
       inquire(file=nlfile, exist=exists)
       if(exists) then
          print *,' Reading pio_timeseries namelist from', trim(nlfile)
          open(27, file=nlfile, status='old')
          read(27, pio_timeseries)
          close(27)
       end if
    end if
    call mpi_bcast(filename,char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)
    call mpi_bcast(include_vars,MAX_VARS*char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)
    call mpi_bcast(exclude_vars,MAX_VARS*char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)
    call mpi_bcast(global_vars,MAX_VARS*char_len, mpi_character, iosys%ioroot, iosys%union_comm, ierr)

    allocate(timeseries)
    timeseries%filename = filename
   
    varcnt=0
    do i=1,MAX_VARS
       if(len_trim(include_vars(i))>0 .or. &
            len_trim(exclude_vars(i))>0 .or. &
            len_trim(global_vars(i))>0) then
          varcnt=varcnt+1
       else
          exit
       endif
    end do
    if(varcnt> 0) then
       allocate(timeseries%varlist(varcnt))
       found_vars = 0

       call timeseries_var_action(timeseries%varlist, include_vars, action_include, found_vars)
       call timeseries_var_action(timeseries%varlist, exclude_vars, action_exclude, found_vars)
       call timeseries_var_action(timeseries%varlist, global_vars, action_global, found_vars)
       call add_timeseries_to_list(timeseries_list,timeseries)
    else
       deallocate(timeseries)
    end if


    contains 
      subroutine timeseries_var_action(varlist, action_list, action, found_vars)
        type(timeseries_var_t), intent(inout) :: varlist(:)
        character(len=*), intent(inout) :: action_list(:)
        integer, intent(in) :: action
        integer, intent(inout) :: found_vars

        integer :: i, j

        do i=1,MAX_VARS
           do j=1,found_vars
              if(varlist(j)%name .eq. action_list(i)) then
                 write(stderr,*) 'Name in list matches name in use', varlist(j)%name, action_list(i)
                 action_list(i) = ' '
              end if
           end do

           if(len_trim(action_list(i))>0) then
              varlist(i)%name = action_list(i)
              varlist(i)%action = action
              found_vars=found_vars+1
           endif
        end do
        

      end subroutine timeseries_var_action

  end subroutine pio_timeseries_init

  subroutine add_timeseries_to_list(timeseries_list,timeseries)
    type(timeseries_file_t), target :: timeseries_list
    type(timeseries_file_t), pointer :: timeseries
    type(timeseries_file_t), pointer :: tptr
    integer :: i, its
    
   tptr => timeseries_list
   ! Copy to first instance
   if(.not.allocated(tptr%varlist)) then
      its = size(timeseries%varlist)
      allocate(tptr%varlist(its))
      tptr%filename=timeseries%filename
      do i=1,its
         tptr%varlist(i)%action = timeseries%varlist(i)%action
         tptr%varlist(i)%name   = timeseries%varlist(i)%name
      end do
      deallocate(timeseries%varlist)
      deallocate(timeseries)
   else
      ! Add after previous instance
      do while(associated(tptr%next))
         tptr => tptr%next
      end do
      tptr%next => timeseries
   endif

  end subroutine add_timeseries_to_list

  logical function check_if_timeseries(File, vardesc,dimids) result(match)
    type(file_desc_t) :: file
    type(var_desc_t), target :: vardesc
    integer, intent(in) :: dimids(:)

    type(timeseries_file_t), pointer :: tptr
    integer :: i
    type(timeseries_var_t), pointer :: varlist(:)
    logical :: fmatch

    match=.false.
    if(any(dimids==file%unlim_dimid)) then
       if( associated(file%tsfile)) then
          tptr => file%tsfile
          fmatch=.true.
       else
          tptr => timeseries_list
          do while(associated(tptr%next))
             i=index(trim(file%filepath),trim(tptr%filename)) 
             if(i > 0) then
                file%tsfile=>tptr
                fmatch=.true.
                exit
             endif
             tptr=>tptr%next
          end do
       endif
       
       if(fmatch) then
          varlist => tptr%varlist
          do i=1,size(varlist)
             if(varlist(i)%name .eq. vardesc%name .and. (varlist(i)%action==action_include &
                  .or. varlist(i)%action==action_global)) then
                match=.true.
                varlist(i)%vardesc=>vardesc
                exit
             endif
          end do
       endif
    endif
  end function check_if_timeseries


end module pio_timeseries

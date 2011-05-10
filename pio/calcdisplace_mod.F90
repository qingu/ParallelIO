MODULE calcdisplace_mod

 use pio_kinds, only: i4
 use pio_support, only : piodie
 
 private
 public :: GCDblocksize,array_gcd
 public :: calcdisplace
CONTAINS

subroutine calcdisplace(bsize,dof,displace)
   implicit none
   integer(i4), intent(in) :: bsize    ! length of contigious blocks of numbers
   integer(i4), intent(in) :: dof(:)   ! degree of freedom on which to setup the displacement array
   integer(i4), intent(inout) :: displace(:)  ! array of mpi displacments

  integer :: numblocks,lenblocks,i,ii,dis

   numblocks = size(displace)
   lenblocks = bsize
  do i=1,numblocks
      ii = (i-1)*lenblocks+1
      dis = dof(ii)-1
      dis = dis/lenblocks   
      displace(i) = dis
  enddo
   do i=1,numblocks-1	
      if(displace(i+1) .lt. displace(i)) then
         print *,'calcdisplace: error with displacement arrays',i,displace(i:i+1),numblocks,size(dof),dof(numblocks)
!         call piodie( _FILE_,__LINE__)
      endif
   enddo

end subroutine calcdisplace

SUBROUTINE GCDblocksize(arr_in,bsize)
implicit none

 integer,intent(in) ,dimension(:)  :: arr_in  !arr_in = rindex array from box_rearrange
 integer,intent(out)               :: bsize   ! the gcd of the block length array

! Locals
 integer,dimension(:),allocatable   :: del_arr,loc_arr,blk_len
 integer :: i,j,k,n,numblks,numtimes,tloc


n = size(arr_in)

allocate(del_arr(n-1))

del_arr = 0    ! forward diff of the input array to deterine where contiguous blocks end.


do  i = 1,n-1  ! compute foward diff of the elements; if =1, still in a contiguous block,
               ! if /= 1 , the end of a block has been reached. 

  del_arr(i) = (arr_in(i+1) - arr_in(i))


end do

numblks  = count( del_arr /= 1) + 1   ! the number of contiguous blocks.
numtimes = count( del_arr /= 1) 
allocate(loc_arr(numtimes))

j=0

do i = 1, n-1

   if ( del_arr(i) == 1 ) cycle

   tloc = i

   j = j+1

   loc_arr(j) = tloc

end do 

allocate(blk_len(numblks))
blk_len(1) = loc_arr(1)

do k = 2,numblks-1           ! computes the the length of each block by differencing the 
                             ! eleemts of the res array. 

  blk_len(k)  = loc_arr(k) - loc_arr(k-1)

end do

blk_len(numblks) = n - sum(blk_len(1:numblks-1)) ! computes the length of the last block

!DBG print*,"blk_len=",blk_len(1:10)
!DBG print*,'blk_len size=',size(blk_len)


call array_gcd(blk_len,bsize) ! call to compute the gcd of the blk_len array.    
!DBG print*,'bsize=', bsize
deallocate(del_arr,loc_arr,blk_len)

end SUBROUTINE GCDblocksize


SUBROUTINE array_gcd(ain,bsize)
implicit none


integer, intent(in),dimension(:) :: ain
integer, intent(out):: bsize

! locals
integer,allocatable :: gcr(:) 
integer :: i,j,a_min,x,index,n


n = size(ain) 
allocate(gcr(n))
gcr = 0
! First check, if an element is 1, then 1 is the gcd (i.e bsize)

do i=1,n

 if ( ain(i) == 1 ) then
  bsize = 1
 end if 
end do  
!

! Find and set the min value.
i=1
a_min = ain(1)
 
do j = i+1,n
  if ( ain(j) < a_min ) then 
   a_min = ain(j)

  end if
end do  

  

!print*,"amin = ", a_min

! Now compute the gcd between a_min and the rest of the array elements as long as a_min /= 1
! otherwise gcd = a_min = 1.
! Done by calling the external function that is below.

do i = 1,n

!print*,"ain = ",ain
gcr(i) = gcd(a_min,ain(i))


!print*,"gcr(i) = ", gcr

end do

!
! Now look for the smallest value in the gcr array and and assign it to bsize.
!
i=1
bsize = gcr(i)

 do j = i+1,n
  if ( gcr(j) < bsize ) then
  bsize = gcr(j)
  end if
 end do

deallocate(gcr)

end SUBROUTINE array_gcd

integer FUNCTION gcd(u,v)
implicit none

integer,intent(in) :: u,v

! locals
integer :: x,a,b

a = u
b = v

  do 

   x = mod(a,b)
   if ( x == 0 ) EXIT
    gcd = b 

     a = b 
     b = x 

     gcd = x 

  end do

 gcd = b 

end FUNCTION gcd 

END MODULE calcdisplace_mod

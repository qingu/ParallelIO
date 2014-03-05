#!/bin/bash 

module switch intel intel/13.1.2
module rm netcdf
module load netcdf/4.3.0
module load cmake/2.8.10.2


export CC=icc
export FC=ifort

piodir=`pwd`/../pio
rm -fr  /glade/scratch/$USER/piotest.serial
mkdir /glade/scratch/$USER/piotest.serial
cd /glade/scratch/$USER/piotest.serial
rm -fr CMakeCache.txt  CMakeFiles  */CMakeCache.txt  */CMakeFiles 
if [ ! -f CMakeCache.txt ]; then
cmake --debug-trycompile   -D CMAKE_VERBOSE_MAKEFILE=1 \
   -D NETCDF_DIR=$NETCDF \
   -D PIO_FILESYSTEM_HINTS=gpfs \
   -D CMAKE_Fortran_FLAGS="-g -traceback" \
   -D ADDITIONAL_INCS="/glade/p/work/kpaul/installs/MCT/include" \
   -D ADDITIONAL_LIBS="-L/glade/p/work/kpaul/installs/MCT/lib -lmpi-serial" \
 $piodir 
fi

gmake

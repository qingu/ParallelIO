#include <iostream>
#include <cstring>
#include <cstdio>
#include <vector>
#include <sstream>
#include <algorithm>
#include <mpi.h>
#include <cerrno>
#include <vapor/CFuncs.h>
#include <vapor/MetadataVDC.h>
#include <vapor/WaveCodecIO.h>

using namespace VetsUtil;
using namespace VAPoR;

void idx_to_coords (int idx, int *max_dims, int *coords, bool reverse) {
//translate index in X,Y,Z array to X,Y,Z coordinates
//  assumes that bounds start at 0,0,0 
//  i.e. {0, 0, 0, max_dims[0], max_dims[1], max_dims[2]}

	if (reverse == false) {
	//X, Y, Z
	coords[2] = idx/(max_dims[0] * max_dims[1]);
	idx -= coords[2] * (max_dims[0] * max_dims[1]);
	coords[1] = idx/max_dims[0];
	idx -= coords[1] * max_dims[0];
	coords[0] = idx;
	} else {
	//Z, Y, X
	coords[0] = idx/(max_dims[0] * max_dims[1]);
	idx -= coords[0] * (max_dims[0] * max_dims[1]);
	coords[1] = idx/max_dims[0];
	idx -= coords[1] * max_dims[0];
	coords[2] = idx;
	}
}

void xform_and_write(const float *data, int *start_block, int *end_block, MPI_Comm IOComm, int *ts, int *lod, int *verbose, char* vdf, char* name){
  //Obtain MPI Rank for timing
   int rank;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
 
  double starttime = MPI_Wtime();
  double iotime;
  double xformtime;
  MetadataVDC metadata(vdf);
  WaveCodecIO *wcwriter = new WaveCodecIO(metadata);
  wcwriter->SetIOComm(IOComm);
  wcwriter->SetCollectiveIO(false);
  const size_t sblock[3] = {start_block[0],start_block[1],start_block[2]};
  const size_t eblock[3] = {end_block[0],end_block[1],end_block[2]};

  //Write out data array
  if (wcwriter->GetErrCode() != 0) perror("wcwriter ctor failed\n");
  if (wcwriter->OpenVariableWrite((size_t)*ts, name, 1, *lod) < 0) perror("openVar failed\n");
  if (wcwriter->BlockWriteRegion(data, sblock, eblock) < 0) perror("writeRegion failed\n");

  wcwriter->CloseVariable();

  if (wcwriter->GetErrCode() != 0) perror("closeVar failed\n");

  iotime = wcwriter->GetWriteTimer();
  xformtime = wcwriter->GetXFormTimer();
  if(*verbose && rank == 0){
    std::cout << "Elapsed vdf time: " << MPI_Wtime() - starttime  << std::endl;
    std::cout << "IOtime (NetCDF): " << iotime << std::endl;
    std::cout << "Xformtime: " << xformtime << std::endl;
  }
  delete wcwriter;
}

extern "C" void write_vdc2_var(float *array, int *start, int *len, int *ndims, int *IO_Comm_f, int *bsize, int *ts, int *lod, int* verbose, char *vdf, char *name, int vdf_len, int name_len) {
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if(rank == 0 && *verbose)
    std::cout << "Calling VDC2 transforms" << std::endl;
  
  MPI_Comm IO_Comm = MPI_Comm_f2c(*IO_Comm_f);

  MyBase::SetErrMsgFilePtr(stderr);

  MyBase::SetDiagMsg("@ndims: %d array[0]: %f\n", ndims, array[0]);
  for(int i=0; i<*ndims; i++)
    	MyBase::SetDiagMsg("@start[%d]: %d, len[%d]: %d\n", i, start[i], i, len[i]);
	
    int start_block[]= { start[0] / bsize[0],
	                 start[1] / bsize[1],
	                 start[2] / bsize[2]
                           },
    end_block[]  = { (start[0] + len[0]) / bsize[0] - 1,
	             (start[1] + len[1]) / bsize[1] - 1,
	             (start[2] + len[2]) / bsize[2] - 1
                     };

    MyBase::SetDiagMsg("@st: %d %d %d, en: %d %d %d\n", 
		       start_block[0], start_block[1], start_block[2],
		       end_block[0], end_block[1], end_block[2]);
    xform_and_write(array, start_block, end_block, IO_Comm, ts, lod, verbose, vdf, name);
    if(*verbose && rank == 0)
      std::cout << "VDC2 transforms complete" << std::endl;
}



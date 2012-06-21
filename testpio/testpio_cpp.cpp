#include <iostream>
#include <cstdlib>
#include <sys/stat.h>
#include <unistd.h>
#include <getopt.h>
#include <mpi.h>

#include "pio.h"
#include "pio_types.h"
#include "namelist.h"

#define CHECK_MPI_FUNC(_rval, _fname) \
  printMPIErr((_rval), (_fname), __FILE__, (__LINE__ - 1))

#define myname 'testpio'
#define TestR8CaseName 'r8_test'
#define TestR4CaseName 'r4_test'
#define TestI4CaseName 'i4_test'
#define TestComboCaseName 'combo_test'
#define MBYTES 1.0e-6

#ifdef _MPISERIAL
#define MPI_Comm_c2f(_comm) (1)
#endif // _MPISERIAL

#define __DEBUG
#ifdef __DEBUG
// Debug version
#define PRINTPOS  std::cout << METHODNAME << " called at " << __FILE__        \
                            << ":" << __LINE__ << std::endl
#define PRINTMSG(_msg)  std::cout << METHODNAME << " at " << __FILE__         \
                                  << ":" << __LINE__ << _msg << std::endl
#define PRINTMSGTSK(_msg) PRINTMSG("(" << my_task << ") " << _msg)
#else
// Non-debug version
#define PRINTPOS
#define PRINTMSG(_msg)
#define PRINTMSGTSK(_msg)
#endif

// Calculate the zero-based local index offset for the given coordinates
// We are assuming Fortran array order
static inline int getLocalOffset(int i, int j, int k, int dimi, int dimj) {
  return (i + ((j + (k * dimj)) * dimi));
}
// Get a local value from a local (Fortran order) array
#define GET_LOCAL_VALUE(_arr,_i,_j,_k,_dimi,_dimj)                            \
        (*((_arr) + getLocalOffset((_i), (_j), (_k), (_dimi), (_dimj))))
// Set a local value from a local (Fortran order) array
#define SET_LOCAL_VALUE(_arr,_i,_j,_k,_dimi,_dimj,_val)                       \
     (*((_arr) + getLocalOffset((_i), (_j), (_k), (_dimi), (_dimj))) = (_val))
// Calculate the one-based global offset for a set of local indices
// The offset is calculated for Fortran order
static inline int getGlobalOffset(int i, int j, int k, int *dims,
                                  int64_t blkStart, int64_t blkSize) {
  return (i + 1 + ((j + (k * dims[1])) * dims[0]) + (blkStart - 1));
}

#undef METHODNAME
#define METHODNAME "printMPIErr"
static void printMPIErr(int err, std::string fname,
                        std::string file, int line) {
  std::string errName;
  switch(err) {
  case MPI_SUCCESS:
    errName = "MPI_SUCCESS";
    break;
#ifndef _MPISERIAL
  case MPI_ERR_BUFFER:
    errName = "MPI_ERR_BUFFER";
    break;
  case MPI_ERR_COUNT:
    errName = "MPI_ERR_COUNT";
    break;
  case MPI_ERR_TYPE:
    errName = "MPI_ERR_TYPE";
    break;
  case MPI_ERR_TAG:
    errName = "MPI_ERR_TAG";
    break;
  case MPI_ERR_COMM:
    errName = "MPI_ERR_COMM";
    break;
  case MPI_ERR_RANK:
    errName = "MPI_ERR_RANK";
    break;
  case MPI_ERR_REQUEST:
    errName = "MPI_ERR_REQUEST";
    break;
  case MPI_ERR_ROOT:
    errName = "MPI_ERR_ROOT";
    break;
  case MPI_ERR_GROUP:
    errName = "MPI_ERR_GROUP";
    break;
  case MPI_ERR_OP:
    errName = "MPI_ERR_OP";
    break;
  case MPI_ERR_TOPOLOGY:
    errName = "MPI_ERR_TOPOLOGY";
    break;
  case MPI_ERR_DIMS:
    errName = "MPI_ERR_DIMS";
    break;
  case MPI_ERR_ARG:
    errName = "MPI_ERR_ARG";
    break;
  case MPI_ERR_UNKNOWN:
    errName = "MPI_ERR_UNKNOWN";
    break;
  case MPI_ERR_TRUNCATE:
    errName = "MPI_ERR_TRUNCATE";
    break;
  case MPI_ERR_OTHER:
    errName = "MPI_ERR_OTHER";
    break;
  case MPI_ERR_INTERN:
    errName = "MPI_ERR_INTERN";
    break;
  case MPI_ERR_IN_STATUS:
    errName = "MPI_ERR_IN_STATUS";
    break;
  case MPI_ERR_PENDING:
    errName = "MPI_ERR_PENDING";
    break;
  case MPI_ERR_ACCESS:
    errName = "MPI_ERR_ACCESS";
    break;
  case MPI_ERR_AMODE:
    errName = "MPI_ERR_AMODE";
    break;
  case MPI_ERR_ASSERT:
    errName = "MPI_ERR_ASSERT";
    break;
  case MPI_ERR_BAD_FILE:
    errName = "MPI_ERR_BAD_FILE";
    break;
  case MPI_ERR_BASE:
    errName = "MPI_ERR_BASE";
    break;
  case MPI_ERR_CONVERSION:
    errName = "MPI_ERR_CONVERSION";
    break;
  case MPI_ERR_DISP:
    errName = "MPI_ERR_DISP";
    break;
  case MPI_ERR_DUP_DATAREP:
    errName = "MPI_ERR_DUP_DATAREP";
    break;
  case MPI_ERR_FILE_EXISTS:
    errName = "MPI_ERR_FILE_EXISTS";
    break;
  case MPI_ERR_FILE_IN_USE:
    errName = "MPI_ERR_FILE_IN_USE";
    break;
  case MPI_ERR_FILE:
    errName = "MPI_ERR_FILE";
    break;
  case MPI_ERR_INFO_KEY:
    errName = "MPI_ERR_INFO_KEY";
    break;
  case MPI_ERR_INFO_NOKEY:
    errName = "MPI_ERR_INFO_NOKEY";
    break;
  case MPI_ERR_INFO_VALUE:
    errName = "MPI_ERR_INFO_VALUE";
    break;
  case MPI_ERR_INFO:
    errName = "MPI_ERR_INFO";
    break;
  case MPI_ERR_IO:
    errName = "MPI_ERR_IO";
    break;
  case MPI_ERR_KEYVAL:
    errName = "MPI_ERR_KEYVAL";
    break;
  case MPI_ERR_LOCKTYPE:
    errName = "MPI_ERR_LOCKTYPE";
    break;
  case MPI_ERR_NAME:
    errName = "MPI_ERR_NAME";
    break;
  case MPI_ERR_NO_MEM:
    errName = "MPI_ERR_NO_MEM";
    break;
  case MPI_ERR_NOT_SAME:
    errName = "MPI_ERR_NOT_SAME";
    break;
  case MPI_ERR_NO_SPACE:
    errName = "MPI_ERR_NO_SPACE";
    break;
  case MPI_ERR_NO_SUCH_FILE:
    errName = "MPI_ERR_NO_SUCH_FILE";
    break;
  case MPI_ERR_PORT:
    errName = "MPI_ERR_PORT";
    break;
  case MPI_ERR_QUOTA:
    errName = "MPI_ERR_QUOTA";
    break;
  case MPI_ERR_READ_ONLY:
    errName = "MPI_ERR_READ_ONLY";
    break;
  case MPI_ERR_RMA_CONFLICT:
    errName = "MPI_ERR_RMA_CONFLICT";
    break;
  case MPI_ERR_RMA_SYNC:
    errName = "MPI_ERR_RMA_SYNC";
    break;
  case MPI_ERR_SERVICE:
    errName = "MPI_ERR_SERVICE";
    break;
  case MPI_ERR_SIZE:
    errName = "MPI_ERR_SIZE";
    break;
  case MPI_ERR_SPAWN:
    errName = "MPI_ERR_SPAWN";
    break;
  case MPI_ERR_UNSUPPORTED_DATAREP:
    errName = "MPI_ERR_UNSUPPORTED_DATAREP";
    break;
  case MPI_ERR_UNSUPPORTED_OPERATION:
    errName = "MPI_ERR_UNSUPPORTED_OPERATION";
    break;
  case MPI_ERR_WIN:
    errName = "MPI_ERR_WIN";
    break;
  case MPI_ERR_LASTCODE:
    errName = "MPI_ERR_LASTCODE";
    break;
#endif // _MPISERIAL
  default:
    errName = "MPI_ERROR";
  }
  if (err != MPI_SUCCESS) {
    std::cout << "Call to " << fname << " returned " << errName
              << " at " << file << ":" << line << std::endl;
  }
}

static struct option long_options[] =
  { { "num_iotasks",                 required_argument, 0,           'i' },
    { "num_aggregators",             required_argument, 0,           'a' },
    { "rerr_type",                   required_argument, 0,           'r' },
    { "stride",                      required_argument, 0,           's' },
    { "base",                        required_argument, 0,           'b' },
    { "help",                        no_argument,       0,           'h' },
    { 0,                             0,                 0,            0  }
  };

#undef METHODNAME
#define METHODNAME "main"
int main(int argc, char *argv[]) {
  int copt = 1;
  int option_index = 0;

  int nprocs;
  int num_iotasks;
  int my_task;
  int master_task;
  bool is_master_task;
  int rval;
  int iostat;
  int indx;
  int mode;
  int ip;
  int numPhases ;
  int num_aggregators;
  int stride = 1;
  int base = 0;
  int rearr_type = PIO_rearr_box;
  int iotype;
  int localrc;
  char filename[FNAME_LEN];

  pio_iosystem_desc_t PIOSYS;
  pio_iosystem_desc_t piosystems[1];
  pio_file_desc_t     File, File_r8, File_r4, File_i4;

  pio_io_desc_t IOdesc_r8;
  pio_io_desc_t IOdesc_r4;
  pio_io_desc_t IOdesc_i4;

  int    *testArray_i4  = (int *)NULL;
  int    *checkArray_i4 = (int *)NULL;
  float  *testArray_r4  = (float *)NULL;
  float  *checkArray_r4 = (float *)NULL;;
  double *testArray_r8  = (double *)NULL;;
  double *checkArray_r8 = (double *)NULL;;;

  pio_var_desc_t varDesc_i4;
  pio_var_desc_t varDesc_r4;
  pio_var_desc_t varDesc_r8;

  int ncDims[3];

  //  gdecomp_type gdecomp;
  bool    progOK = true;
  bool    fileOpen = false;
  int64_t *compdof;
  int64_t blockSize;
  int     arrayNumElem;
  int64_t peNumElem;
  int64_t numBlocks;
  int64_t blockStart;
  int     gDims3D[3];
  int     totaldims;

  // Initialize MPI

#ifndef _MPISERIAL
  rval = MPI_Init(&argc, &argv);
  CHECK_MPI_FUNC(rval, "MPI_Init");
#endif // ! _MPISERIAL

#ifdef _MPISERIAL
  my_task = 0;
#else
  rval = MPI_Comm_rank(MPI_COMM_WORLD, &my_task);
  CHECK_MPI_FUNC(rval, "MPI_Comm_rank");
#endif // _MPISERIAL

#ifdef _MPISERIAL
  nprocs = 1;
#else
  rval = MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
  CHECK_MPI_FUNC(rval, "MPI_Comm_size");
#endif // _MPISERIAL

  // IO Namelist data
  io_nml namelist = io_nml(nprocs);

  master_task = 0;
  is_master_task = (my_task == master_task);
  num_iotasks = std::max((nprocs / 2), 1);
  num_aggregators = num_iotasks;
  bool numaggset = false;

  // Process input args (if any)
  while (copt >= 0) {
    copt = getopt_long(argc, argv, "a:b:hi:r:s:",
		       long_options, &option_index);
    switch (copt) {
    case 'a':
      num_aggregators = atoi(optarg);
      numaggset = true;
      break;
    case 'b':
      base = atoi(optarg);
      break;
    case 'i':
      num_iotasks = atoi(optarg);
      break;
    case 'r':
      rearr_type = atoi(optarg);
      break;
    case 's':
      stride = atoi(optarg);
      break;
    case 'h':
      if (is_master_task) {
        std::cout << "Usage: " << argv[0] << std::endl;
        std::cout << "Optional arguments: " << std::endl;
        for (int i = 0;
             i < (sizeof(long_options) / sizeof(struct option)); i++) {
          std::cout << "  " << long_options[i].name << std::endl;
        }
      }
#ifndef _MPISERIAL
      rval = MPI_Finalize();
      CHECK_MPI_FUNC(rval, "MPI_Finalize");
#endif // ! _MPISERIAL
      return 0;
      break;
    case '?':
      // No action, getopt_long already complained
      break;
    default:
      if (copt >= 0) {
	std::cerr << "ERROR: Bad option, " << copt << std::endl;
      }
      // copt < 0 is normal termination
    }
  }
  if (!numaggset) {
    num_aggregators = num_iotasks;
  }

  std::cout << "My rank is " << my_task << "/" << nprocs << ": "
            << "I am" << (is_master_task ? "" : " not")
            << " the master task." << std::endl;
  if (is_master_task) {
    std::cout << "Configuring " << num_iotasks << " iotasks and "
              << num_aggregators << " MPI aggegrators" << std::endl;
  }

  pio_cpp_setdebuglevel(3);
  PIOSYS = PIO_IOSYSTEM_DESC_NULL;
  PRINTMSG(" Calling pio_cpp_init_intracom, PIOSYS = " << PIOSYS);
  pio_cpp_init_intracom(my_task, MPI_COMM_WORLD, num_iotasks,
                        num_aggregators, stride, rearr_type, &PIOSYS,
                        base);
  pio_cpp_seterrorhandlingi(&PIOSYS, PIO_RETURN_ERROR);
  PRINTMSG(" After pio_cpp_init_intracom, PIOSYS = " << PIOSYS);

  iotype = namelist.iotype;
  if (strlen(namelist.fname1) > 0) {
    strcpy(filename, namelist.fname1);
  } else {
    strcpy(filename, "FileI4.nc");
  }
  // Allocate PIO data structure memory
  IOdesc_i4 = (pio_io_desc_t)malloc(PIO_SIZE_IO_DESC);
  if ((pio_io_desc_t)NULL == IOdesc_i4) {
    PRINTMSG(" failed to allocate pio IO desc");
    progOK = false;
  }
  // Allocate a file descriptor
  if (progOK) {
    File_i4 = (pio_file_desc_t)malloc(PIO_SIZE_FILE_DESC);
    if ((pio_file_desc_t)NULL == File_i4) {
      PRINTMSG(" failed to allocate pio file desc");
      progOK = false;
    } else {
      PRINTMSG(" allocated pio file desc, addr = " << (void *)File_i4);
    }
  }
  // Allocate a variable descriptor
  if (progOK) {
    varDesc_i4 = (pio_var_desc_t)malloc(PIO_SIZE_VAR_DESC);
    if ((pio_var_desc_t)NULL == varDesc_i4) {
      PRINTMSG(" failed to allocate pio variable desc");
      progOK = false;
    } else {
      PRINTMSG(" allocated pio variable desc, addr = " << (void *)varDesc_i4);
    }
  }


  if (progOK) {
    // Decomposition -- set up a problem
    int nMod;
    gDims3D[0] = namelist.nx_global;
    gDims3D[1] = namelist.ny_global;
    gDims3D[2] = namelist.nz_global;
/////////////////////////
//// Debug values
/////////////////////////
    gDims3D[0] = 8;
    gDims3D[1] = 16;
    gDims3D[2] = 2;
/////////////////////////
    totaldims = gDims3D[0]*gDims3D[1]*gDims3D[2];
    blockSize = gDims3D[1] * gDims3D[2];
    arrayNumElem = gDims3D[0] * blockSize;
    nMod = gDims3D[0] % nprocs;
    if (my_task < nMod) {
      numBlocks = ((gDims3D[0] / nprocs) + 1);
      blockStart = (my_task * numBlocks) + 1; // Fortran based
    } else {
      numBlocks = (gDims3D[0] / nprocs);
      blockStart = ((nMod * (numBlocks + 1)) +
                    ((my_task - nMod) * numBlocks) + 1); // Fortran based
    }
    peNumElem = blockSize * numBlocks;
#if 0
    std::cout << "PE(" << my_task << "): numBlocks = " << numBlocks
              << ", blockStart = " << blockStart << ", peNumElem = "
              << peNumElem << ", range = ("
              << getGlobalOffset(0, 0, 0, gDims3D, blockStart, blockSize)
              << ", "
              << getGlobalOffset((numBlocks - 1),
                                 (gDims3D[1] - 1), (gDims3D[2] - 1),
                                 gDims3D, blockStart, blockSize)
              << ")" << std::endl;
#endif
    testArray_i4 = (int *)malloc(peNumElem * sizeof(int));
    if ((int *)NULL == testArray_i4) {
      PRINTMSG(" failed to allocate integer test array");
      progOK = false;
    }
  }
  if (progOK) {
    compdof = (int64_t *)malloc(peNumElem * sizeof(int64_t));
    if ((int64_t *)NULL == compdof) {
      PRINTMSG(" failed to allocate compdof");
      progOK = false;
    }
  }
  if (progOK) {
    // Fill the array and the DOF
    for (int i = 0; i < numBlocks; i++) {
      for (int j = 0; j < gDims3D[1]; j++) {
        for (int k = 0; k < gDims3D[2]; k++) {
          SET_LOCAL_VALUE(testArray_i4, i, j, k, numBlocks, gDims3D[1],
                          (getLocalOffset(i, j, k, numBlocks, gDims3D[1]) +
                           (my_task * 10000000)));
          SET_LOCAL_VALUE(compdof, i, j, k, numBlocks, gDims3D[1],
                          getGlobalOffset(i, j, k, gDims3D,
                                          blockStart, blockSize));
        }
      }
    }

#if 0
    if (0 == my_task) {
      for (int i = 0; i < peNumElem; i++) {
        PRINTMSGTSK("compdof(" << i << ") = " << compdof[i]);
      }
    }
    rval = MPI_Barrier(MPI_COMM_WORLD);
    CHECK_MPI_FUNC(rval, "MPI_Barrier");
    if (1 == my_task) {
      for (int i = 0; i < peNumElem; i++) {
        PRINTMSGTSK("compdof(" << i << ") = " << compdof[i]);
      }
    }
#endif

    // Calculate a decomposition
    PRINTMSGTSK("Calling pio_cpp_initdecomp_dof");
    pio_cpp_initdecomp_dof(&PIOSYS, PIO_int, gDims3D, 3,
                           compdof, peNumElem, IOdesc_i4);
    free(compdof);
    compdof = (int64_t *)NULL;
  }

  // See if the file already exists
  if (progOK) {
    struct stat info;
    if (stat(filename, &info) != -1) {
      if (is_master_task) {
        PRINTMSG(" WARNING: file, " << filename << ", exists; overwriting");
      }
    }

    // Create the file
    PRINTMSG(" Calling pio_cpp_createfile");
    localrc = pio_cpp_createfile(&PIOSYS, File_i4, iotype,
                                 filename, PIO_CLOBBER);
    if (PIO_noerr != localrc) {
      if (is_master_task) {
        char errmsg[512];
        sprintf(errmsg,
                " ERROR: Attempt to create file, \"%s\", return code = %d\n",
                filename, localrc);
        PRINTMSG(errmsg);
      }
      progOK = false;
    } else {
      fileOpen = true;
    }
  }

  // Create the variable
  if (progOK && fileOpen) {
    std::string axes[3] = { "x", "y", "z" };
    PRINTMSGTSK("Calling pio_cpp_def_dim");
    for (int i = 0; i < 3; i++) {
      localrc = pio_cpp_def_dim(File_i4, axes[i].c_str(),
                                gDims3D[i], &ncDims[i]);
      if (PIO_noerr != localrc) {
        char errmsg[512];
        sprintf(errmsg,
                " ERROR: Attempt to create dimension %d, return code = %d\n",
                i, localrc);
        PRINTMSG(errmsg);
        progOK = false;
        break;
      }
    }
  }
  if (progOK && fileOpen) {
    PRINTMSGTSK("Calling pio_cpp_def_var_md");
    localrc = pio_cpp_def_var_md(File_i4, "testArray_i4", PIO_int,
                                 ncDims, 3, varDesc_i4);
    if (PIO_noerr != localrc) {
      char errmsg[512];
      sprintf(errmsg,
              " ERROR: Attempt to create NetCDF var, return code = %d\n",
              localrc);
      PRINTMSG(errmsg);
      progOK = false;
    }
  }
  if (progOK && fileOpen) {
    PRINTMSGTSK("Calling pio_cpp_enddef");
    localrc = pio_cpp_enddef(File_i4);
    if (PIO_noerr != localrc) {
      char errmsg[512];
      sprintf(errmsg,
              " ERROR: Attempt to end NetCDF def mode, return code = %d\n",
              localrc);
      PRINTMSG(errmsg);
      progOK = false;
    }
  }

  // Write the file
  if (progOK && fileOpen) {
    rval = MPI_Barrier(MPI_COMM_WORLD);
    CHECK_MPI_FUNC(rval, "MPI_Barrier");
    PRINTMSGTSK("Calling pio_cpp_write_darray_1d_int");
    pio_cpp_write_darray_int(File_i4, varDesc_i4, IOdesc_i4,
                             testArray_i4, gDims3D, 3, &localrc);
    rval = MPI_Barrier(MPI_COMM_WORLD);
    CHECK_MPI_FUNC(rval, "MPI_Barrier");
  }

  // Close the file
  if (fileOpen) {
    PRINTMSGTSK("Calling pio_cpp_syncfile");
    pio_cpp_syncfile(File_i4);
  }

  // Close the file
  if (fileOpen) {
    PRINTMSGTSK("Calling pio_cpp_closefile");
    pio_cpp_closefile(File_i4);
    fileOpen = false;
  }

  // Try reading in the file
  if (progOK) {
    checkArray_i4 = (int *)malloc(peNumElem * sizeof(int));
    if ((int *)NULL == checkArray_i4) {
      PRINTMSG(" failed to allocate integer check array");
      progOK = false;
    }
  }
  if (progOK) {
    // Open the file
    PRINTMSG(" Calling pio_cpp_openfile");
    localrc = pio_cpp_openfile(&PIOSYS, File_i4, iotype,
                               filename, PIO_NOWRITE);
    if (PIO_noerr != localrc) {
      if (is_master_task) {
        char errmsg[512];
        sprintf(errmsg,
                " ERROR: Attempt to open file, \"%s\", return code = %d\n",
                filename, localrc);
        PRINTMSG(errmsg);
      }
      progOK = false;
    } else {
      fileOpen = true;
    }
  }

  // Check that our variable is in the file,
  if (progOK && fileOpen) {
    int nDim;
    int nVar;
    int nAtt;
    int unlim;
    PRINTMSGTSK("pio_cpp_inquire");
    localrc = pio_cpp_inquire(File_i4, &nDim, &nVar, &nAtt, &unlim);
    if (PIO_noerr != localrc) {
      if (is_master_task) {
        char errmsg[512];
        sprintf(errmsg,
                " ERROR: pio_cpp_inquire failed, return code = %d\n", localrc);
        PRINTMSG(errmsg);
      }
      progOK = false;
    } else {
      PRINTMSGTSK("filename, nDim = " << nDim << ", nVar = " << nVar
                  << ", nAtt = " << nAtt << ", unlim dim = " << unlim);
      char name[FNAME_LEN];
      for (int i = 1; i <= nVar; i++) {
        PRINTMSGTSK("pio_cpp_inq_varname_vid");
        localrc = pio_cpp_inq_varname_vid(File_i4, i, name);
        if (PIO_noerr != localrc) {
          if (is_master_task) {
            char errmsg[512];
            sprintf(errmsg,
                    " ERROR: pio_cpp_inq_varname_vid failed, "
                    "return code = %d\n", localrc);
            PRINTMSG(errmsg);
          }
        } else {
          PRINTMSGTSK("variable #" << i << " = " << name);
        }
      }
    }
  }

  if (progOK && fileOpen) {
    int varid;
    PRINTMSGTSK("pio_cpp_inq_varid_vid");
    localrc = pio_cpp_inq_varid_vid(File_i4, "testArray_i4", &varid);
    if (PIO_noerr != localrc) {
      if (is_master_task) {
        char errmsg[512];
        sprintf(errmsg,
                " ERROR: Attempt to find id of, \"%s\", return code = %d\n",
                "testArray_i4", localrc);
        PRINTMSG(errmsg);
      }
      progOK = false;
    } else {
      PRINTMSGTSK("testArray_i4 vid = " << varid <<
                  ", calling pio_cpp_read_darray_int");
      pio_cpp_read_darray_int(File_i4, varDesc_i4, IOdesc_i4,
                              checkArray_i4, gDims3D, 3, &localrc);
      if (PIO_noerr != localrc) {
        if (is_master_task) {
          char errmsg[512];
          sprintf(errmsg,
                  " ERROR: read of test array failed, return code = %d\n",
                  localrc);
          PRINTMSG(errmsg);
          progOK = false;
        } else {
        }
      }
    }
  }

  // Close the file
  if (fileOpen) {
    PRINTMSGTSK("Calling pio_cpp_closefile");
    pio_cpp_closefile(File_i4);
    fileOpen = false;
  }

  // Cleanup

  if ((pio_io_desc_t)NULL != IOdesc_i4) {
    // Free any decompositions
    PRINTMSGTSK("Calling pio_cpp_freedecomp_ios");
    pio_cpp_freedecomp_ios(&PIOSYS, IOdesc_i4);
    free(IOdesc_i4);
    IOdesc_i4 = (pio_io_desc_t)NULL;
  }

  if (progOK) {
    pid_t foo = getpid();
    PRINTMSGTSK(foo);
  }
  // Finalize PIO (always call this)
  rval = MPI_Barrier(MPI_COMM_WORLD);
  CHECK_MPI_FUNC(rval, "MPI_Barrier");

  PRINTMSGTSK("Calling pio_cpp_finalize");
  pio_cpp_finalize(&PIOSYS, &rval);
  if (rval != PIO_noerr) {
    std::cerr << "ERROR: pio_cpp_finalize returned " << rval << std::endl;
  }

  if ((pio_io_desc_t)NULL != IOdesc_i4) {
    free(IOdesc_i4);
    IOdesc_i4 = (pio_io_desc_t)NULL;
  }
  if ((pio_file_desc_t)NULL != File_i4) { 
    free(File_i4);
    File_i4 = (pio_file_desc_t)NULL;
  }
  if ((pio_var_desc_t)NULL != varDesc_i4) {
    free(varDesc_i4);
    varDesc_i4 = (pio_var_desc_t)NULL;
  }
  if ((int *)NULL != testArray_i4) {
    free(testArray_i4);
    testArray_i4 = (int *)NULL;
  }
  if ((int *)NULL != checkArray_i4) {
    free(checkArray_i4);
    checkArray_i4 = (int *)NULL;
  }
  if ((float *)NULL != testArray_r4) {
    free(testArray_r4);
    testArray_r4 = (float *)NULL;
  }
  if ((float *)NULL != checkArray_r4) {
    free(checkArray_r4);
    checkArray_r4 = (float *)NULL;
  }
  if ((double *)NULL != testArray_r8) {
    free(testArray_r8);
    testArray_r8 = (double *)NULL;
  }
  if ((double *)NULL != checkArray_r8) {
    free(checkArray_r8);
    checkArray_r8 = (double *)NULL;
  }
  if ((int64_t *)NULL != compdof) {
    free(compdof);
    compdof = (int64_t *)NULL;
  }

#ifndef _MPISERIAL
  rval = MPI_Finalize();
  CHECK_MPI_FUNC(rval, "MPI_Finalize");
#endif // ! _MPISERIAL
  return 0;
}

#if 0


  integer(i4) :: cbad, ivar
  integer(i4) :: i,j,is,ie,itmp,it,n,i1,j1,k1

  character(6) :: ew_type,ns_type
  character(len=10) :: varname

  integer(i4) :: varid,dimid_x,dimid_y,dimid_z

  integer(kind=PIO_OFFSET),parameter :: one = 1

  integer, parameter :: ntest = 5
  integer(i4), dimension(ntest),parameter :: num_agg =(/ 8,12,16,24,32/)

  integer(i4),pointer :: test_i4wr(:),test_i4rd(:),diff_i4(:)
  integer(i4),pointer :: test_i4i(:),test_i4j(:),test_i4k(:),test_i4m(:),test_i4dof(:)
  real(r4),   pointer :: test_r4wr(:),test_r4rd(:),diff_r4(:)
  real(r8),   pointer :: test_r8wr(:),test_r8rd(:),diff_r8(:)

  logical, parameter :: TestR8    = .true.
  logical, parameter :: TestR4    = .false.
  logical, parameter :: TestInt   = .false.
  logical, parameter :: TestCombo = .false.
  logical, parameter :: CheckArrays = .true.  ! turn off the array check for maximum memory usage testing

  logical :: writePhase, readPhase
  logical, parameter :: splitPhase = .true.
  integer :: numPhase

  real(r8) :: lsum,lsum2,gsum
  real(r8) :: st,et  ! start/end times for timing
  real(r8) :: dt_write_r8, dt_write_r4, dt_write_i4 ! individual write times
  real(r8) :: dt_read_r8, dt_read_r4, dt_read_i4 ! individual read times
  ! Arrays to hold globally reduced read/write times--one element per time trial
  real(r8), dimension(:), pointer :: gdt_write_r8, gdt_write_r4, gdt_write_i4 
  real(r8), dimension(:), pointer :: gdt_read_r8, gdt_read_r4, gdt_read_i4

  integer(i4) :: lLength    ! local number of words in the computational decomposition 

  integer(i4), parameter ::  nml_in = 10
  character(len=*), parameter :: nml_filename = 'testpio_in'

  integer(i4)  :: nml_error
  integer(i4)  :: sdof,sdof_sum,sdof_min,sdof_max

  ! memory tracking stuff
  integer(i4)  :: msize,mrss,mrss0,mrss1,mrss2
  real(r8)     :: mb_blk
  real(r8),allocatable :: mem_tmp(:)
  integer(i4),allocatable :: lmem(:),gmem(:,:)

  integer(i4), pointer  :: compDOF(:), ioDOF(:)
  integer(i4), pointer  :: ioDOFR(:),ioDOFW(:)

  integer(i4) :: startIO(3),countIO(3), &
       startCOMP(3), countCOMP(3), &
       start(3), count(3)
  integer(i4) :: lenblocks, glenr8, glenr4, gleni4
  integer(kind=PIO_OFFSET) :: startpio(3), countpio(3)

  character(len=80) :: fname, fname_r8,fname_r4,fname_i4
  logical, parameter :: Debug = .false.
  integer :: mpi_comm_compute, mpi_comm_io, mpi_icomm_cio

  character(len=3) :: citer


  if(Debug)    print *,'testpio: before call to t_initf'
#ifdef TIMING
  !---------------------------------------------------------------
  ! timing library
  !---------------------------------------------------------------
  if(Debug)    print *,'testpio: point #1'
  call t_initf(nml_filename, logprint=.false., logunit=6, &
       mpicom=MPI_COMM_WORLD, MasterTask=log_master_task)
  if(Debug)    print *,'testpio: point #2'
  call t_startf('testpio_total')

  if(Debug)    print *,'testpio: after call to t_startf'
  !-----------------------------------------------------------------------------
  ! Memory test
  !-----------------------------------------------------------------------------
  call get_memusage(msize,mrss0)
  if(Debug)    print *,'testpio: after get_memusage #3'
  allocate(mem_tmp(1024*1024))    ! 1 MWord, 8 MB
  mem_tmp = -1.0
  call get_memusage(msize,mrss1)
  if(Debug)    print *,'testpio: after get_memusage #4'
  deallocate(mem_tmp)
  call get_memusage(msize,mrss2)
  if(Debug)    print *,'testpio: after get_memusage #5'
  mb_blk = 0.0_r8
  if(Debug)    print *,'testpio: after get_memusage #6'
  if (mrss1 - mrss0 > 0) then
     mb_blk = (8.0_r8)/((mrss1-mrss0)*1.0_r8)
  endif
  if (my_task == master_task) then
     write(*,*) myname,' 8 MB memory   alloc in MB is ',(mrss1-mrss0)*mb_blk
     write(*,*) myname,' 8 MB memory dealloc in MB is ',(mrss1-mrss2)*mb_blk
     write(*,*) myname,' Memory block size conversion in bytes is ',mb_blk*1024_r8*1024.0_r8
  endif
#endif

  !----------------------------------------------------------------
  ! Read in namelist and set File IO Type and Format accordingly...
  !----------------------------------------------------------------

  if(Debug)    print *,'testpio: before call to readTestPIO_Namelist'
  if(my_task == master_task) then 
     call ReadTestPIO_Namelist(nml_in, nprocs, nml_filename, myname, nml_error)
  endif
  if(Debug) print *,'testpio: before call to broadcast_namelist'
  call MPI_barrier(MPI_COMM_WORLD,ierr)
  call Broadcast_Namelist(myname, my_task, master_task, MPI_COMM_WORLD, ierr)
  if(Debug) print *,'testpio: after call to broadcast_namelist'

  !-------------------------------------
  ! Checks
  !-------------------------------------

#if !defined(BGx) 
  if (num_iotasks <= 0) then
     write(*,*) trim(myname),' ERROR: ioprocs invalid num_iotasks=',num_iotasks
     call piodie(__FILE__,__LINE__)
  endif
#endif

  ! ----------------------------------------------------------------
  ! if stride is and num_iotasks is incompatible than reset stride
  ! ----------------------------------------------------------------
  if (base + num_iotasks * (stride-1) > nprocs-1) then
     write(*,*) trim(myname),' ERROR: num_iotasks, base and stride too large', &
          ' base=',base,' num_iotasks=',num_iotasks,' stride=',stride,' nprocs=',nprocs
     call piodie(__FILE__,__LINE__)
  endif

  !--------------------------------------
  ! Initalizes the parallel IO subsystem 
  !--------------------------------------
  call PIO_setDebugLevel(DebugLevel)

  if(Debug)    print *,'testpio: before call to PIO_init'

  if(async) then
#ifdef BGx
	call piodie(__FILE__,__LINE__,'async option not currently supported')
!     allocate(&PIOSYS)
!     call PIO_init(my_task, MPI_COMM_WORLD, num_iotasks, num_aggregator, stride, &
!          rearr_type, &PIOSYS, base, async=.true.,mpi_comm_compute=mpi_comm_compute)
#else
     call split_comm(mpi_comm_world,nprocs, num_iotasks, stride, base, &
          mpi_comm_compute, mpi_comm_io, mpi_icomm_cio)
     call PIO_init(1, mpi_comm_world, (/mpi_comm_compute/), mpi_comm_io, PIOSYSTEMS)
     PIOSYS => PIOSYSTEMS(1)

#endif
     call MPI_COMM_RANK(MPI_COMM_COMPUTE,my_task,ierr)
     call MPI_COMM_SIZE(MPI_COMM_COMPUTE,nprocs,ierr)

  else
     mpi_comm_compute = mpi_comm_world
     allocate(PIOSYS)

     call PIO_init(my_task, MPI_COMM_COMPUTE, num_iotasks, num_aggregator, stride, &
          rearr_type, PIOSYS, base)
  end if
  if(Debug)    print *,'testpio: after call to PIO_init', piosys%num_tasks,piosys%io_comm

  !! ** Set PIO/MPI filesystem hints **

  if (set_mpi_values /= 0) then
     if (trim(mpi_cb_buffer_size) /= '') then
        call PIO_set_hint(PIOSYS, 'cb_buffer_size', trim(mpi_cb_buffer_size))
     end if
  end if

  if (set_romio_values /= 0) then
     if (trim(romio_cb_write) /= '') then
        call PIO_set_hint(PIOSYS, 'romio_cb_write', trim(romio_cb_write))
     end if

     if (trim(romio_cb_read) /= '') then
        call PIO_set_hint(PIOSYS, 'romio_cb_read', trim(romio_cb_read))
     end if

     !! NCH: Not sure if the following applies to non-XFS file systems...

     if (trim(romio_direct_io) /= '') then
        call PIO_set_hint(PIOSYS, 'direct_read', trim(romio_direct_io))
        call PIO_set_hint(PIOSYS, 'direct_write', trim(romio_direct_io))
     end if
  end if

  if (set_ibm_io_values /= 0) then
     if (trim(ibm_io_buffer_size) /= '') then
        call PIO_set_hint(PIOSYS, 'IBM_io_buffer_size', &
                          trim(ibm_io_buffer_size))
     end if

     if (trim(ibm_io_largeblock_io) /= '') then
        call PIO_set_hint(PIOSYS, 'IBM_largeblock_io', &
                          trim(ibm_io_largeblock_io))
     end if

     if (trim(ibm_io_sparse_access) /= '') then
        call PIO_set_hint(PIOSYS, 'IBM_sparse_access', &
                          trim(ibm_io_sparse_access))
     end if
  end if
  if(set_lustre_values /= 0) then 
        call PIO_setnum_OST(PIOSYS,lfs_ost_count)
  endif

  !-----------------------------------------
  ! Compute compDOF based on namelist input
  !-----------------------------------------
!  write(rd_buffer,('(i9)')) 64*1024*1024
!  call PIO_set_hint(PIOSYS,'cb_buffer_size',trim(adjustl(rd_buffer)))
!  call PIO_set_hint(PIOSYS,'romio_cb_write','enable')
!  call PIO_set_hint(PIOSYS,'romio_cb_read','disable')

  startCOMP = 0

  if(index(casename,'CAM')==1) then
     call camlike_decomp_generator(gdims3d(1),gdims3d(2),gdims3d(3),my_task,nprocs,npr_yz,compDOF)
  elseif(index(casename,'MPAS')==1) then 
!     print *,'testpio: before call to mpas_decomp_generator: (',TRIM(part_input),') gdims3d: ',gdims3d
     call mpas_decomp_generator(gdims3d(1),gdims3d(2),gdims3d(3),my_task,part_input,compDOF)
  else  if (trim(compdof_input) == 'namelist') then
     if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #1'
     call gdecomp_read_nml(gdecomp,nml_filename,'comp',PIOSYS%comp_rank,PIOSYS%num_tasks,gDims3D(1:3))
     if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #2'

     call gdecomp_DOF(gdecomp,PIOSYS%comp_rank,compDOF,start,count)
     if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #3'
  else
     call pio_readdof(trim(compdof_input),compDOF,MPI_COMM_COMPUTE,75)
     sdof = size(compDOF)
     start = gDims3D(1:3)   ! min tmp
     count = 0         ! max tmp
     do n = 1,sdof
        call c1dto3d(compdof(n),gDims3D(1),gDims3D(2),gDims3D(3),i1,j1,k1)
        start(1) = min(start(1),i1)
        start(2) = min(start(2),j1)
        start(3) = min(start(3),k1)
        count(1) = max(count(1),i1)
        count(2) = max(count(2),j1)
        count(3) = max(count(3),k1)
     enddo
     do n = 1,3
        count(n) = max(count(n)-start(n)+1,0)
     enddo
     if (count(1)*count(2)*count(3) == sdof) then
        ! start and count seem consistent with compDOF
     else
        ! start and count NOT consistent with compDOF, zero them out
        start = 0
        count = 0
     endif
  endif
  startCOMP(1:3) = start(1:3)
  countCOMP(1:3) = count(1:3)
  if (trim(compdof_output) /= 'none') then
     call pio_writedof(trim(compdof_output),compDOF,MPI_COMM_COMPUTE,75)
  endif

  sdof = size(compDOF)
  call MPI_REDUCE(sdof,sdof_sum,1,MPI_INTEGER,MPI_SUM,master_task,MPI_COMM_COMPUTE,ierr)
  call CheckMPIReturn('Call to MPI_REDUCE SUM',ierr,__FILE__,__LINE__)
  call MPI_REDUCE(sdof,sdof_min,1,MPI_INTEGER,MPI_MIN,master_task,MPI_COMM_COMPUTE,ierr)
  call CheckMPIReturn('Call to MPI_REDUCE MIN',ierr,__FILE__,__LINE__)
  call MPI_REDUCE(sdof,sdof_max,1,MPI_INTEGER,MPI_MAX,master_task,MPI_COMM_COMPUTE,ierr)
  call CheckMPIReturn('Call to MPI_REDUCE MAX',ierr,__FILE__,__LINE__)
  if (my_task == master_task) then
     write(6,*) trim(myname),' total nprocs = ',nprocs
     write(6,*) trim(myname),' compDOF sum/min/max = ',sdof_sum,sdof_min,sdof_max
  endif

  if (mod(my_task,((nprocs/32)+1)) == 0) then
     if(Debug)       write(6,*) trim(myname),' my_task,sdof,start,count = ',my_task,sdof,start,count
  endif

  !--------------------------------
  ! calculate ioDOF
  !--------------------------------

  startIO = 0
  countIO = 0
  if (trim(rearr) == 'none') then
     ioDOF   => compDOF
     startIO(1:3) = startCOMP(1:3)
     countIO(1:3) = countCOMP(1:3)
  elseif (trim(rearr) == 'box') then
     ! do nothing
     if (trim(iodof_input) == 'namelist') then
        if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #4'
        call gdecomp_read_nml(gdecomp,nml_filename,'io',PIOSYS%io_rank,PIOSYS%num_iotasks,gDims3D(1:3))
        if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #5'
        call gdecomp_DOF(gdecomp,PIOSYS%io_rank,ioDOF,start,count)
        if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #6'
        startIO(1:3) = start(1:3)
        countIO(1:3) = count(1:3)
     endif
  elseif (trim(rearr) == 'mct') then
     call gdecomp_read_nml(gdecomp,nml_filename,'io',PIOSYS%io_rank,PIOSYS%num_iotasks,gDims3D(1:3))
     call gdecomp_DOF(gdecomp,PIOSYS%io_rank,ioDOF,start,count)
     startIO(1:3) = start(1:3)
     countIO(1:3) = count(1:3)
  else
     call piodie(__FILE__,__LINE__,' rearr '//trim(rearr)//' not supported')
  endif

  ioDOFR => ioDOF
  ioDOFW => ioDOF
  startpio = startIO
  countpio = countIO
  lenblocks = countIO(1)

  if(Debug) print *,'comp_rank,io_rank: ',piosys%comp_rank,piosys%io_rank,' ioDOF ',ioDOF
  if(Debug) print *,'comp_rank: ',PIOSYS%comp_rank,SIZE(compDOF),SIZE(ioDOF)

  !-----------------------------------------------------
  ! number of words on each computational processor owns
  !-----------------------------------------------------

  lLength = size(compDOF)

  !----------------------
  ! allocate and set test arrays 
  !----------------------

  if(TestR8 .or. TestCombo) then 
     call alloc_check(test_r8wr,lLength,'testpio:test_r8wr')
  endif
  if(TestR4 .or. TestCombo) then 
     call alloc_check(test_r4wr,lLength,'testpio:test_r4wr' )
  endif
  if(TestInt .or. TestCombo) then 
     call alloc_check(test_i4wr,lLength,'testpio:test_i4wr')
  endif
  if(TestInt) then 
    call alloc_check(test_i4i ,lLength,'testpio:test_i4i ')
    call alloc_check(test_i4j ,lLength,'testpio:test_i4j ')
    call alloc_check(test_i4k ,lLength,'testpio:test_i4k ')
    call alloc_check(test_i4m ,lLength,'testpio:test_i4m ')
    call alloc_check(test_i4dof,lLength,'testpio:test_i4dof')
  endif

  do n = 1,lLength
     call c1dto3d(compdof(n),gDims3D(1),gDims3D(2),gDims3D(3),i1,j1,k1)
     if(TestInt) then 
	test_i4dof(n) = compdof(n)
        test_i4i(n) = i1
        test_i4j(n) = j1
        test_i4k(n) = k1
        test_i4m(n) = my_task
     endif
     if(TestR8 .or. TestCombo) then 
         test_r8wr(n) = 10.0_r8*cos(20.*real(i1,kind=r8)/real(gDims3D(1),kind=r8))* &
          cos(10.*real(j1,kind=r8)/real(gDims3D(2),kind=r8))* &
          (1.0+1.0*real(j1,kind=r8)/real(gDims3D(2),kind=r8))* &
          cos(25.*real(k1,kind=r8)/real(gDims3D(3),kind=r8))
     endif
     if(TestR4 .or. TestCombo) then 
         test_r4wr(n) = 10.0_r4*cos(20.*real(i1,kind=r4)/real(gDims3D(1),kind=r4))* &
          cos(10.*real(j1,kind=r4)/real(gDims3D(2),kind=r4))* &
          (1.0+1.0*real(j1,kind=r4)/real(gDims3D(2),kind=r4))* &
          cos(25.*real(k1,kind=r4)/real(gDims3D(3),kind=r4))
     endif
     if(TestInt .or. TestCombo) then 
         test_i4wr(n) = nint(10.0_r8*cos(20.*real(i1,kind=r8)/real(gDims3D(1),kind=r8))* &
          cos(10.*real(j1,kind=r8)/real(gDims3D(2),kind=r8))* &
          (1.0+1.0*real(j1,kind=r8)/real(gDims3D(2),kind=r8))* &
          cos(25.*real(k1,kind=r8)/real(gDims3D(3),kind=r8))*1000.0_r8)
     endif
  enddo
  if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #10'


  if(TestR8  .or. TestCombo)  call alloc_check(test_r8rd,lLength,'testpio:test_r8rd')
  if(TestInt .or. TestCombo) call alloc_check(test_i4rd,lLength,'testpio:test_i4rd')
  if(TestR4  .or. TestCombo)  call alloc_check(test_r4rd,lLength,'testpio:test_r4rd')

  if(TestR8  .or. TestCombo) test_r8rd(:) = 1000.00
  if(TestR4  .or. TestCombo) test_r4rd(:) = 1000.00
  if(TestInt .or. TestCombo) test_i4rd(:) = 1000

  if(Debug) then
     write(*,'(a,2(a,i8))') myname,':: Before call to OpenFile().  comp_rank=',piosys%comp_rank, &
          ' io_rank=',piosys%io_rank
  endif

  !--------------------------------
  ! allocate arrays for holding globally-reduced timing information
  !--------------------------------

  call alloc_check(gdt_write_r8, maxiter, ' testpio:gdt_write_r8 ')
  call alloc_check(gdt_read_r8, maxiter, ' testpio:gdt_read_r8 ')
  call alloc_check(gdt_write_r4, maxiter, ' testpio:gdt_write_r4 ')
  call alloc_check(gdt_read_r4, maxiter, ' testpio:gdt_read_r4 ')
  call alloc_check(gdt_write_i4, maxiter, ' testpio:gdt_write_i4 ')
  call alloc_check(gdt_read_i4, maxiter, ' testpio:gdt_read_i4 ')
  if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #11'

  if(splitPhase) then 
    numPhases = 2
  else
    numPhases = 1
  endif
  do ip=1,numPhases
     if(numPhases == 1) then 
        readPhase = .true.
        writePhase = .true.
     else
        if(ip == 1) then 
	   writePhase = .true.
	   readPhase = .false.
	else
	   writePhase = .false.
	   readPhase = .true.
        endif
     endif
     if(log_master_task) print *,'{write,read}Phase:  ',writePhase,readPhase
     do it=1,maxiter

     !-------------------------------------------------------
     ! Explain the distributed array decomposition to PIOlib
     !-------------------------------------------------------

        if (trim(rearr) == 'box') then
           !JMD print *,__FILE__,__LINE__,gdims3d,minval(compdof),maxval(compdof)
           
           if (trim(iodof_input) == 'namelist') then
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #7'
              if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double,  gDims3D,compDOF,IOdesc_r8,startpio,countpio)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #7.1'
              if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,    gDims3D,compDOF,IOdesc_r4,startpio,countpio)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #7.2'
              if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,     gDims3D,compDOF,IOdesc_i4,startpio,countpio)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8'
           else
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.1'
              if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double,  gDims3D,compDOF,IOdesc_r8)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.2'
              if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,    gDims3D,compDOF,IOdesc_r4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.3'
              if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,     gDims3D,compDOF,IOdesc_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.4'
           endif
        else
           if(iofmtd.eq.'nc') then ! netCDF
              if (num_iodofs == 1) then
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.5'
                 if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double, gDims3D,lenblocks,compDOF,ioDOF,startpio,countpio,IOdesc_r8)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.6'
                 if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,   gDims3D,lenblocks,compDOF,ioDOF,startpio,countpio,IOdesc_r4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.7'
                 if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,    gDims3D,lenblocks,compDOF,ioDOF,startpio,countpio,IOdesc_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.8'
              elseif (num_iodofs == 2) then
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.9'
                 if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double, gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,startpio,countpio,IOdesc_r8)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.10'
                 if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,   gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,startpio,countpio,IOdesc_r4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.11'
                 if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,    gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,startpio,countpio,IOdesc_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.12'
              else
                 call piodie(__FILE__,__LINE__,' num_iodofs not 1 or 2')
              endif
           else
              ! tcraig: there are cases where lenblocks is not valid here like different size IO blocks
              if (num_iodofs == 1) then
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.13'
                 if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double, gDims3D,lenblocks,compDOF,ioDOF,IOdesc_r8)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.14'
                 if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,   gDims3D,lenblocks,compDOF,ioDOF,IOdesc_r4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.15'
                 if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,    gDims3D,lenblocks,compDOF,ioDOF,IOdesc_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.16'
              elseif (num_iodofs == 2) then
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.17'
                 if(TestR8 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_double, gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,IOdesc_r8)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.18'
                 if(TestR4 .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_real,   gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,IOdesc_r4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.19'
                 if(TestInt .or. TestCombo) call PIO_initDecomp(PIOSYS,PIO_int,    gDims3D,lenblocks,compDOF,ioDOFR,ioDOFW,IOdesc_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #8.20'
              else
                 call piodie(__FILE__,__LINE__,' num_iodofs not 1 or 2')
              endif
           endif
        endif
        if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9'
        
        if(Debug) then
           write(*,'(a,2(a,i8))') myname,':: After call to initDecomp.  comp_rank=',piosys%comp_rank, &
                ' io_rank=',piosys%io_rank
        endif

        call PIO_getnumiotasks(PIOSYS,num_iotasks)
        !------------
        ! Open file{s} 
        !------------
        write(citer,'(i3.3)') it
        
        fname    = TRIM(dir)//'foo.'//citer//'.'//TRIM(Iofmtd)
        fname_r8 = TRIM(dir)//'foo.r8.'//citer//'.'//TRIM(Iofmtd)
        fname_r4 = TRIM(dir)//'foo.r4.'//citer//'.'//TRIM(Iofmtd)
        fname_i4 = TRIM(dir)//'foo.i4.'//citer//'.'//TRIM(Iofmtd)
        !   print *, __FILE__,__LINE__,'>',fname,'<'
        !   print *, __FILE__,__LINE__,'>',fname_r8,'<'
        !   print *, __FILE__,__LINE__,'>',fname_i4,'<'
        !   print *, __FILE__,__LINE__,'>',fname_r4,'<'
#if defined(_NETCDF) || defined(_PNETCDF)
        mode = pio_64bit_offset
#else
        mode = 0
#endif

        if(writePhase) then 
           if(TestCombo) then
              if(Debug) write(*,'(2a,i8)') myname,':: Combination Test:  Creating File...it=',it
              ierr = PIO_CreateFile(PIOSYS,File,iotype,trim(fname), mode)
              call check_pioerr(ierr,__FILE__,__LINE__,' combo createfile')
           endif

           if(TestR8) then
              if (Debug) write(*,'(2a,i8)') myname,':: REAL*8 Test:  Creating File...it=',it
              ierr = PIO_CreateFile(PIOSYS,File_r8,iotype,trim(fname_r8), mode)
              call check_pioerr(ierr,__FILE__,__LINE__,' r8 createfile')
           endif

           if(TestR4) then
              if(Debug) write(*,'(2a,i8)') myname,':: REAL*4 Test:  Creating File...,it=',it
              ierr = PIO_CreateFile(PIOSYS,File_r4,iotype,trim(fname_r4), mode)
              call check_pioerr(ierr,__FILE__,__LINE__,' r4 createfile')
           endif

           if(TestInt) then
              if(Debug) write(*,'(2a,i8)') myname,':: INTEGER*4 Test:  Creating File...,it=',it
              ierr = PIO_CreateFile(PIOSYS,File_i4,iotype,trim(fname_i4), mode)
              call check_pioerr(ierr,__FILE__,__LINE__,' i4 createfile')
           endif

           ! Set Frame to '1' in the PIO descriptor file
           
           allocate(vard_r8(nvars), vard_r4(nvars))
           
           do ivar=1,nvars
              call PIO_SetFrame(vard_r8(ivar),one)
              call PIO_SetFrame(vard_r4(ivar),one)
           end do

           call PIO_SetFrame(vard_i4,one)
           call PIO_SetFrame(vard_r8c,one)
           call PIO_SetFrame(vard_r4c,one)
           call PIO_SetFrame(vard_i4c,one)
           call PIO_SetFrame(vard_i4i,one)
           call PIO_SetFrame(vard_i4j,one)
           call PIO_SetFrame(vard_i4k,one)
           call PIO_SetFrame(vard_i4m,one)
           call PIO_SetFrame(vard_i4dof,one)

           !---------------------------
           ! Code specifically for netCDF files 
           !---------------------------
           if(iotype == iotype_pnetcdf .or. & 
                iotype == iotype_netcdf .or. &
                iotype == PIO_iotype_netcdf4p .or. &
                iotype == PIO_iotype_netcdf4c) then

              if(TestR8) then 
                 !-----------------------------------
                 ! for the single record real*8 file 
                 !-----------------------------------
                 call WriteHeader(File_r8,nx_global,ny_global,nz_global,dimid_x,dimid_y,dimid_z)

                 do ivar = 1, nvars
                    write(varname,'(a,i5.5)') 'field',ivar
                    iostat = PIO_def_var(File_r8,varname,PIO_double,(/dimid_x,dimid_y,dimid_z/),vard_r8(ivar))
                    call check_pioerr(iostat,__FILE__,__LINE__,' r8 defvar')
                 end do
                 iostat = PIO_enddef(File_r8)
                 call check_pioerr(iostat,__FILE__,__LINE__,' r8 enddef')
              endif

              if(TestR4) then 
                 !-----------------------------------
                 ! for the single record real*4 file 
                 !-----------------------------------
                 call WriteHeader(File_r4,nx_global,ny_global,nz_global,dimid_x,dimid_y,dimid_z)

                 do ivar = 1, nvars
                    write(varname,'(a,i5.5)') 'field',ivar
                    iostat = PIO_def_var(File_r4,varname,PIO_real,(/dimid_x,dimid_y,dimid_z/),vard_r4(ivar))
                    call check_pioerr(iostat,__FILE__,__LINE__,' r4 defvar')
                 end do
                 iostat = PIO_enddef(File_r4)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4 enddef')
              endif

              if(TestInt) then 
                 !-----------------------------------
                 ! for the single record integer file 
                 !-----------------------------------
                 call WriteHeader(File_i4,nx_global,ny_global,nz_global,dimid_x,dimid_y,dimid_z)
                 
                 iostat = PIO_def_var(File_i4,'field',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4 defvar')
                 iostat = PIO_def_var(File_i4,'fi',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4i)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4i defvar')
                 iostat = PIO_def_var(File_i4,'fj',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4j)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4j defvar')
                 iostat = PIO_def_var(File_i4,'fk',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4k)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4k defvar')
                 iostat = PIO_def_var(File_i4,'my_task',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4m)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4m defvar')
                 iostat = PIO_def_var(File_i4,'fdof',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4dof)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4dof defvar')
                 iostat = PIO_enddef(File_i4)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4 enddef')
              endif
        
              if(TestCombo) then 
                 !-----------------------------------
                 ! for the multi record file 
                 !-----------------------------------
                 call WriteHeader(File,nx_global,ny_global,nz_global,dimid_x,dimid_y,dimid_z)
                 iostat = PIO_def_var(File,'field_r8',PIO_double,(/dimid_x,dimid_y,dimid_z/),vard_r8c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo r8 defvar')
                 iostat = PIO_def_var(File,'field_r4',PIO_real,(/dimid_x,dimid_y,dimid_z/),vard_r4c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo r4 defvar')
                 iostat = PIO_def_var(File,'field_i4',PIO_int,(/dimid_x,dimid_y,dimid_z/),vard_i4c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo i4 defvar')
                 iostat = PIO_enddef(File)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo enddef')
              endif

           endif ! if(iotype == iotype_pnetcdf .or. iotype == iotype_netcdf ) then

           if(Debug) then
              write(*,'(a,2(a,i8))') myname,':: After call to OpenFile.  comp_rank=',piosys%comp_rank, &
                   ' io_rank=',piosys%io_rank
           endif

           !-------------------------
           ! Time the parallel write 
           !-------------------------

           if(TestR8) then
              dt_write_r8 = 0.
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.0.1'
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.0.2'
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.0.3'
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_write')
#endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.0.4'
              do ivar=1,nvars
                 call PIO_write_darray(File_r8,vard_r8(ivar), iodesc_r8, test_r8wr, iostat)
                 call check_pioerr(iostat,__FILE__,__LINE__,' r8 write_darray')
              end do
#ifdef TIMING
              call t_stopf('testpio_write')
#endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.1'
              call PIO_CloseFile(File_r8)
              et = MPI_Wtime()
              dt_write_r8 = dt_write_r8 + (et - st)/nvars
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #9.2'
           endif

           if(TestR4) then
              dt_write_r4 = 0.
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_write')
#endif
              do ivar=1,nvars
                 call PIO_write_darray(File_r4,vard_r4(ivar),iodesc_r4, test_r4wr,iostat)
                 call check_pioerr(iostat,__FILE__,__LINE__,' r4 write_darray')
              end do
#ifdef TIMING
              call t_stopf('testpio_write')
#endif
              call PIO_CloseFile(File_r4)
              et = MPI_Wtime()
              dt_write_r4 = dt_write_r4 + (et - st)/nvars
           endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #13'

           if(TestInt) then 
              dt_write_i4 = 0.
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_write')
#endif
              call PIO_write_darray(File_i4,vard_i4,iodesc_i4,test_i4wr,iostat)
#ifdef TIMING
              call t_stopf('testpio_write')
#endif
              et = MPI_Wtime()
              dt_write_i4 = dt_write_i4 + et - st
              call check_pioerr(iostat,__FILE__,__LINE__,' i4 write_darray')
              call PIO_write_darray(File_i4,vard_i4i,iodesc_i4,test_i4i,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' i4i write_darray')
              call PIO_write_darray(File_i4,vard_i4j,iodesc_i4,test_i4j,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' i4j write_darray')
              call PIO_write_darray(File_i4,vard_i4k,iodesc_i4,test_i4k,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' i4k write_darray')
              call PIO_write_darray(File_i4,vard_i4m,iodesc_i4,test_i4m,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' i4m write_darray')
              call PIO_write_darray(File_i4,vard_i4dof,iodesc_i4,test_i4dof,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' i4dof write_darray')
              call PIO_CloseFile(File_i4) 
           endif

           if(TestCombo) then 
              call PIO_write_darray(File,vard_r8c,iodesc_r8,test_r8wr,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo r8 write_darray')
              call PIO_write_darray(File,vard_r4c,iodesc_r4, test_r4wr,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo r4 write_darray')
              call PIO_write_darray(File,vard_i4c,iodesc_i4, test_i4wr,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo i4 write_darray')
              call PIO_CloseFile(File)
           endif

           if(Debug) then
              write(*,'(a,2(a,i8))') myname,':: After calls to PIO_write_darray.  comp_rank=',piosys%comp_rank, & 
                   ' io_rank=',piosys%io_rank,piosys%io_comm
              
           endif

        endif
        call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
        call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #14'


        if (readPhase) then 
           !-------------------------------------
           ! Open the file back up and check data
           !-------------------------------------
           
           if(TestR8) then 
              ierr = PIO_OpenFile(PIOSYS, File_r8, iotype, fname_r8)
              call check_pioerr(ierr,__FILE__,__LINE__,' r8 openfile')
           endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #15'
           
           if(TestR4) then 
              ierr = PIO_OpenFile(PIOSYS,File_r4,iotype, fname_r4)
              call check_pioerr(ierr,__FILE__,__LINE__,' r4 openfile')
           endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #16'

           if(TestInt) then
              ierr = PIO_OpenFile(PIOSYS,File_i4,iotype, fname_i4)
              call check_pioerr(ierr,__FILE__,__LINE__,' int openfile')
           endif

           !   if(TestCombo) ierr = PIO_OpenFile(PIOSYS,File,iotype,fname)
           if(Debug) then
              write(*,'(2a,i8)') myname,':: After calls to PIO_OpenFile.  my_task=',my_task
           endif
           
           if(Debug) print *,__FILE__,__LINE__

           if(iotype == iotype_pnetcdf .or. &
                iotype == iotype_netcdf ) then
              do ivar=1,nvars
                 if(TestR8) then 
                    iostat = PIO_inq_varid(File_r8,'field00001',vard_r8(ivar))
                    call check_pioerr(iostat,__FILE__,__LINE__,' r8 inq_varid')
                 endif

                 if(TestR4) then 
                    iostat = PIO_inq_varid(File_r4,'field00001',vard_r4(ivar))
                    call check_pioerr(iostat,__FILE__,__LINE__,' r4 inq_varid')
                 endif
              end do
              if(TestInt) then 
                 iostat = PIO_inq_varid(File_i4,'field',vard_i4)
                 call check_pioerr(iostat,__FILE__,__LINE__,' i4 inq_varid')
              endif

           endif ! if((iotype == iotype_pnetcdf) .or (iotype == iotype_netcdf))...
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #17'
           if(Debug) print *,__FILE__,__LINE__
           do ivar=1,nvars
              call PIO_SetFrame(vard_r8(ivar),one)
              call PIO_SetFrame(vard_r4(ivar),one)
           end do
           call PIO_SetFrame(vard_i4,one)
           call PIO_SetFrame(vard_r8c,one)
           call PIO_SetFrame(vard_r4c,one)
           call PIO_SetFrame(vard_i4c,one)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #18'

           !-------------------------
           ! Time the parallel  read
           !-------------------------
           if(TestR8) then 
              dt_read_r8 = 0.
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_read')
#endif
           if(Debug) print *,__FILE__,__LINE__
              do ivar=1,nvars
                 call PIO_read_darray(File_r8,vard_r8(ivar),iodesc_r8,test_r8rd,iostat)
                 call check_pioerr(iostat,__FILE__,__LINE__,' r8 read_darray')
              enddo
           if(Debug) print *,__FILE__,__LINE__
#ifdef TIMING
              call t_stopf('testpio_read')
#endif
              et = MPI_Wtime()
              dt_read_r8 = dt_read_r8 + (et - st)/nvars        
              call check_pioerr(iostat,__FILE__,__LINE__,' r8 read_darray')
           endif

           if(TestR4) then
              dt_read_r4 = 0.
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_read')
#endif
              do ivar=1,nvars
                 call PIO_read_darray(File_r4,vard_r4(ivar),iodesc_r4,test_r4rd,iostat)
                 call check_pioerr(iostat,__FILE__,__LINE__,' r4 read_darray')
              enddo
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #19'
#ifdef TIMING
              call t_stopf('testpio_read')
#endif
              et = MPI_Wtime()
              dt_read_r4 = dt_read_r4 + (et - st)/nvars
              call check_pioerr(iostat,__FILE__,__LINE__,' r4 read_darray')
           endif

           if(TestInt) then
              dt_read_i4 = 0.
              call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
              call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)
              st = MPI_Wtime()
#ifdef TIMING
              call t_startf('testpio_read')
#endif
              call PIO_read_darray(File_i4,vard_i4,iodesc_i4, test_i4rd,iostat)
#ifdef TIMING
              call t_stopf('testpio_read')
#endif
              et = MPI_Wtime()
              dt_read_i4 = dt_read_i4 + et - st
              call check_pioerr(iostat,__FILE__,__LINE__,' i4 read_darray')
           endif

           !-------------------------------
           ! Print the maximum memory usage 
           !-------------------------------
!           call alloc_print_usage(0,'testpio: after calls to PIO_read_darray')

#ifdef TESTMEM
!           stop 
#endif

           if(Debug) then
              write(*,'(a,2(a,i8))') myname,':: After PIO_read_darray tests, my_task=', &
                   my_task,', it=',it
           endif

     !-------------------
     ! close the file up 
     !-------------------
           if(TestR8) call PIO_CloseFile(File_r8)
           if(TestR4) call PIO_CloseFile(File_r4)
           if(TestInt) call PIO_CloseFile(File_i4)
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #20'

           call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
           call CheckMPIReturn('Call to MPI_BARRIER()',ierr,__FILE__,__LINE__)

!           if(Debug) then
!              write(*,*) myname,':: my_task=',my_task,'test_r8wr= ',test_r8wr
!              if(TestR8 .or. TestCombo) write(*,*) myname,':: my_task=',my_task,'test_r8rd= ',test_r8rd
!           endif

     !-----------------------------
     ! Perform correctness testing 
     !-----------------------------
           if(TestR8 .and. CheckArrays) then
              call checkpattern(mpi_comm_compute, fname_r8,test_r8wr,test_r8rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern r8 test')
           endif
     
           if( TestR4 .and. CheckArrays) then
              call checkpattern(mpi_comm_compute, fname_r4,test_r4wr,test_r4rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern r4 test')
           endif

           if(TestInt .and. CheckArrays) then
              call checkpattern(mpi_comm_compute, fname_i4, test_i4wr,test_i4rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern i4 test')
           endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #21'

           if(TestCombo .and. CheckArrays) then 

              !-------------------------------------
              !  Open up and read the combined file 
              !-------------------------------------
              
              ierr = PIO_OpenFile(PIOSYS,File,iotype,fname)
              call check_pioerr(ierr,__FILE__,__LINE__,' combo test read openfile')
              
              if(iofmtd(1:2).eq.'nc') then
                 iostat = PIO_inq_varid(File,'field_r8',vard_r8c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo test r8 inq_varid')
                 iostat = PIO_inq_varid(File,'field_r4',vard_r4c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo test r4 inq_varid')
                 iostat = PIO_inq_varid(File,'field_i4',vard_i4c)
                 call check_pioerr(iostat,__FILE__,__LINE__,' combo test i4 inq_varid')
              endif
           
              call PIO_read_darray(File,vard_r8c,iodesc_r8,test_r8rd,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo test r4 pio_read_darray')
              call PIO_read_darray(File,vard_r4c,iodesc_r4,test_r4rd,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo test r4 pio_read_darray')
              call PIO_read_darray(File,vard_i4c,iodesc_i4,test_i4rd,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' combo test i4 pio_read_darray')

              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #22'
              call PIO_CloseFile(File)
           
              !-----------------------------
              ! Check the combined file 
              !-----------------------------
              call checkpattern(mpi_comm_compute, fname,test_r8wr,test_r8rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern test_r8 ')
              
              call checkpattern(mpi_comm_compute, fname,test_r4wr,test_r4rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern test_r4 ')
        
              call checkpattern(mpi_comm_compute, fname,test_i4wr,test_i4rd,lLength,iostat)
              call check_pioerr(iostat,__FILE__,__LINE__,' checkpattern test_i4 ')
              
           endif
     !---------------------------------------
     ! Print out the performance measurements 
     !---------------------------------------
           call MPI_Barrier(MPI_COMM_COMPUTE,ierr)
        endif

              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #23'
        if(TestR8) then
           ! Maximum read/write times
           if(readPhase)  call GetMaxTime(dt_read_r8, gdt_read_r8(it), MPI_COMM_COMPUTE, ierr)
           if(writePhase) call GetMaxTime(dt_write_r8, gdt_write_r8(it), MPI_COMM_COMPUTE, ierr)
        endif

        if(TestR4) then
           ! Maximum read/write times
           if(readPhase)  call GetMaxTime(dt_read_r4, gdt_read_r4(it), MPI_COMM_COMPUTE, ierr)
           if(writePhase) call GetMaxTime(dt_write_r4, gdt_write_r4(it), MPI_COMM_COMPUTE, ierr)
        endif
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #24'
        
        if(TestInt) then
           ! Maximum read/write times
           if(readPhase)  call GetMaxTime(dt_read_i4, gdt_read_i4(it), MPI_COMM_COMPUTE, ierr)
           if(writePhase) call GetMaxTime(dt_write_i4, gdt_write_i4(it), MPI_COMM_COMPUTE, ierr)
        endif


        if(TestR8 .or. TestCombo) glenr8=iodesc_r8%glen
        if(TestR4 .or. TestCombo) glenr4=iodesc_r4%glen
        if(TestInt .or. TestCombo) gleni4=iodesc_i4%glen
        if(TestR8 .or. TestCombo) call pio_freedecomp(PIOSYS, iodesc_r8)
        if(TestR4 .or. TestCombo) call pio_freedecomp(PIOSYS, iodesc_r4)
        if(TestInt .or. TestCombo) call pio_freedecomp(PIOSYS, iodesc_i4)
     enddo ! do it=1,maxiter
              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #25'

  enddo ! do ip=1,numphase


  !--------------------------------
  ! Clean up initialization memory 
  !   note: make sure DOFs are not used later
  !--------------------------------
  if (PIOSYS%comp_rank >= 0) call dealloc_check(compDOF)
  if (trim(rearr) == 'mct') then
     if (PIOSYS%io_rank >= 0) call dealloc_check(ioDOF)
  endif

  !----------------------------------
  ! Print summary bandwidth statistics 
  !----------------------------------

              if(Debug)       print *,'iam: ',PIOSYS%comp_rank,'testpio: point #26'
  if(TestR8 .and. (piosys%io_rank == 0) ) then
     call WriteTimeTrialsStats(casename,TestR8CaseName, fname_r8, glenr8, gdt_read_r8, gdt_write_r8, maxiter) 
  endif

  if(TestR4 .and. (piosys%io_rank == 0) ) then
     call WriteTimeTrialsStats(casename,TestR4CaseName, fname_r4, glenr4, gdt_read_r4, gdt_write_r4, maxiter) 
  endif

  if(TestInt .and. (piosys%io_rank == 0) ) then
     call WriteTimeTrialsStats(casename,TestI4CaseName, fname_i4, gleni4, gdt_read_i4, gdt_write_i4, maxiter) 
  endif

  !-------------------------------
  ! Print timers and memory usage 
  !-------------------------------

#ifdef TIMING
  call t_stopf('testpio_total')
  call t_prf('timing.testpio',MPI_COMM_COMPUTE)
  call t_finalizef()
  call get_memusage(msize,mrss)
  allocate(lmem(2),gmem(2,0:nprocs-1))
  lmem(1) = msize
  lmem(2) = mrss
  call mpi_gather(lmem,2,MPI_INTEGER,gmem,2,MPI_INTEGER,0,MPI_COMM_COMPUTE,ierr)
  if (my_task == master_task) then
     do n = 0,nprocs-1
        write(*,'(2a,i8,a,2f10.2)') myname,' my_task=',n,' : (hw, usage) memory (MB) = ',gmem(1,n)*mb_blk,gmem(2,n)*mb_blk
     enddo
!     indx = MAXLOC(gmem(1,:),dim=1) - 1 ! offset the location of the maximum memory usage by one
     indx = MAXLOC(gmem(2,:),dim=1) - 1
     write(*,'(2a,i8,a,2f10.2)') myname,' my_task=',indx,' : (hw, usage) MAX memory (MB) = ',gmem(1,indx)*mb_blk,gmem(2,indx)*mb_blk
  endif
  deallocate(lmem,gmem)
#endif

  call MPI_Barrier(MPI_COMM_COMPUTE,ierr)

  if (my_task == master_task) then
     print *,' '
     print *,'testpio completed successfully'
     print *,' '
  endif

  call PIO_finalize(PIOSYS,ierr)
  call MPI_Finalize(ierr)
  call CheckMPIReturn('Call to MPI_FINALIZE()',ierr,__FILE__,__LINE__)

  !=============================================================================
contains
  !=============================================================================

  subroutine GetMaxTime(dtLocal, gdtMax, comm, ierror)

    implicit none

    real(r8),    intent(IN)  :: dtLocal
    real(r8),    intent(OUT) :: gdtMax
    integer(i4), intent(IN)  :: comm
    integer(i4), intent(OUT) :: ierror
    real(r8) :: local_temp
#ifdef _MPISERIAL
    local_temp = dtlocal
#else
    local_temp = max(dtlocal, MPI_Wtick())
#endif
    call MPI_Allreduce(Local_temp, gdtMax, 1,MPI_DOUBLE_PRECISION, MPI_MAX, comm, ierror)

    call CheckMPIReturn('Call to MPI_ALLREDUCE()',ierror,__FILE__,__LINE__)

  end subroutine GetMaxTime

  !=============================================================================

  subroutine WriteStats(CaseName, FileName, glen, trialNo, dtRead, dtWrite)

    implicit none

    character(len=*),  intent(IN) :: CaseName
    character(len=80), intent(IN) :: FileName
    integer(i4),       intent(in) :: glen
    integer(i4),       intent(IN) :: trialNo
    real(r8),          intent(IN) :: dtRead
    real(r8),          intent(IN) :: dtWrite

    character(len=*), parameter :: myname_=myname//'::WriteStats'
    integer :: datumSize

    select case(CaseName)
    case(TestR8CaseName)
       datumSize = r8
    case(TestR4CaseName)
       datumSize = r4
    case(TestI4CaseName)
       datumSize = i4
    case(TestComboCaseName)
       write(*,'(4a)') myname_,':: Case ',CaseName,' not supported.  Returning without writing output.'
    case default
       write(*,'(4a)') myname_,':: Case ',CaseName,' not supported.  Returning without writing output.'
    end select

    print *,'-----------------------------------------'
    print *,myname,':: Timings for ',CaseName,' Trial Number=',trialNo
    print *,'Total Procs: ',nprocs,' IO Procs: ',num_iotasks, &
         ' Aggregators: ',num_aggregator,' Stride: ',stride
    print *,'Record bytes: ',INT(glen*datumSize,kind=i8), &
         INT(glen*datumSize,kind=i8)
    print *,'-----------------------------------------'
    print *,'Type of I/O performed: ',Iofmtd
    print *,' File name: ',FileName
    print *,'-----------------------------------------'
    print *, CaseName,' Trial No. ',trialNo,' Read time: ',dtRead, &
         'Read Mbytes/sec: ',MBYTES*glen*datumSize/dtRead
    print *, CaseName,' Trial No. ',trialNo,' Write time: ',dtWrite, &
         'Write Mbytes/sec: ',MBYTES*glen*datumSize/dtWrite
    print *,'-----------------------------------------'

  end subroutine WriteStats

  !=============================================================================

  subroutine WriteTimeTrialsStats(casename,TestName, FileName, glen, ReadTimes, WriteTimes, nTrials) 

    implicit none

    character(len=*),          intent(IN) :: casename
    character(len=*),          intent(IN) :: TestName
    character(len=80),         intent(IN) :: FileName
    integer(i4),          intent(IN) :: glen
    real(r8),    dimension(:), pointer ::    ReadTimes
    real(r8),    dimension(:), pointer ::    WriteTimes
    integer(i4),               intent(IN) :: nTrials

    character(len=*), parameter :: myname_=myname//'::WriteTimeTrialsStats'

    real(r8), parameter :: tiny = 1.e-10
    real(r8) :: ReadBWAvg, ReadBWStdDev, ReadBWStdErrMean, ReadBWMin, ReadBWMax
    real(r8) :: WriteBWAvg, WriteBWStdDev, WriteBWStdErrMean, WriteBWMin, WriteBWMax
    real(r8) :: WriteTimeAvg,ReadTimeAvg
    real(r8) :: TotalMBytes
    integer ::  datumSize, i, nDOF
    real(r8), dimension(:), pointer :: ReadBW, WriteBW

    if(nTrials .ne. size(ReadTimes)) then
       write(*,'(a,2(a,i8))') myname_,':: ERROR--nTrials = ',nTrials,' size(ReadTimes)=',size(ReadTimes)
       call piodie(__FILE__,__LINE__)
    endif

    if(nTrials .ne. size(WriteTimes)) then
       write(*,'(a,2(a,i8))') myname_,':: ERROR--nTrials = ',nTrials,' size(WriteTimes)=',size(WriteTimes)
       call piodie(__FILE__,__LINE__)
    endif

    select case(TestName)
    case(TestR8CaseName)
       datumSize = r8
    case(TestR4CaseName)
       datumSize = r4
    case(TestI4CaseName)
       datumSize = i4
    case(TestComboCaseName)
       datumSize = 0
    case default
       write(*,'(4a)') myname_,':: TestName ',TestName,' not supported.  Returning without writing output.'
       return
    end select

    TotalMBytes = MBYTES * glen * datumSize

    write(*,*)'-----------------------------------------'
    write(*,*) 'Timing Output :: case = ',trim(casename),'  test = ',trim(TestName)
    write(*,'(5a,g14.6)') &
         '  I/O format = ',trim(Iofmtd),'  File name = ',trim(FileName), &
         '  Record Size Mbytes = ',TotalMBytes
    write(*,*) '  Trials = ',nTrials,'  Total Procs = ',nprocs
    write(*,*) '  IO Procs=',num_iotasks,'  Aggregators=',num_aggregator,&
         '  Base=',base,'  Stride=',stride
    do i = 1,nTrials
       if(writetimes(i)>0) then
          write(*,101) 'n=',i,'  write (Mb/sec)=',TotalMBytes/WriteTimes(i), &
               '  write_time(sec)=',WriteTimes(i), &
               trim(casename),trim(TestName)
       else
          write(*,101) 'n=',i,'  write (Mb/sec)=',TotalMBytes, &
               '?  write_time(sec)=',WriteTimes(i), &
               trim(casename),trim(TestName)
       end if
    enddo
    if (nTrials > 1) write(*,*) '         -------------------'
    do i = 1,nTrials
	if(Readtimes(i)>0) then
           write(*,101) 'n=',i,'   read (Mb/sec)=',TotalMBytes/ReadTimes(i), &
                '   read_time(sec)=',ReadTimes(i), &
                trim(casename),trim(TestName)
        else
           write(*,101) 'n=',i,'   read (Mb/sec)=',TotalMBytes, &
                '?   read_time(sec)=',ReadTimes(i), &
                trim(casename),trim(TestName)
        end if
    enddo

101 format(3x,a,i5,a,f9.1,a,e12.4,2x,a,2x,a)

    if (nTrials > 1) then

       call alloc_check(ReadBW, nTrials, myname_//':: ReadBW')
       call alloc_check(WriteBW, nTrials, myname_//':: WriteBW')

       ! Compute mean read/write bandwidths
       ReadBWAvg = 0.
       ReadTimeAvg = 0.
       WriteBWAvg = 0.
       WriteTimeAvg = 0.
       do i=1,nTrials
          ReadBW(i) = TotalMBytes / (ReadTimes(i) + tiny)
          WriteBW(i) = TotalMBytes / (WriteTimes(i) + tiny)
          ReadBWAvg = ReadBWAvg + ReadBW(i)
          ReadTimeAvg = ReadTimeAvg + 1.0e3*ReadTimes(i)
          WriteBWAvg = WriteBWAvg + WriteBW(i)
          WriteTimeAvg = WriteTimeAvg + 1.0e3*WriteTimes(i)
       enddo

       ReadBWAvg = ReadBWAvg / float(nTrials)
       ReadTimeAvg = ReadTimeAvg / float(nTrials)
       WriteBWAvg = WriteBWAvg / float(nTrials)
       WriteTimeAvg = WriteTimeAvg / float(nTrials)

       ! Compute Standard Deviation and Std Error of the Mean
       ReadBWStdDev = 0.
       WriteBWStdDev = 0.
       do i=1,nTrials
          ReadBWStdDev = ReadBWStdDev + (ReadBWAvg - ReadBW(i))**2
          WriteBWStdDev = WriteBWStdDev + (WriteBWAvg - WriteBW(i))**2
       enddo

       ! Compute std. deviation and std. error of the mean
       nDOF = max(1,nTrials-1) ! sample number of degrees-of-freedom
       ReadBWStdDev = sqrt( ReadBWStdDev / float(nDOF) )
       WriteBWStdDev = sqrt( WriteBWStdDev / float(nDOF) )
       ReadBWStdErrMean = ReadBWStdDev / sqrt( float(nTrials) )
       WriteBWStdErrMean = WriteBWStdDev / sqrt( float(nTrials) )

       ! Determine minimum and maximum BW values
       ReadBWMin = minval(ReadBW)
       ReadBWMax = maxval(ReadBW)
       WriteBWMin = minval(WriteBW)
       WriteBWMax = maxval(WriteBW)

       write(*,*) '         -------------------'
       write(*,*) '  Summary BW Stats (MB/sec) for ',nTrials,' trials of ',trim(casename),' ',trim(TestName)
       write(*,102)  'write avg=',WriteBWAvg,' +/-',WriteBWStderrMean, &
            ' min=',WriteBWMin,' max=',WriteBWMax,' stddev=',WriteBWStdDev, &
            trim(casename),trim(TestName)
       write(*,102)  'read  avg=',ReadBWAvg ,' +/-',ReadBWStderrMean, &
            ' min=',ReadBWMin ,' max=',ReadBWMax ,' stddev=',ReadBWStdDev, &
            trim(casename),trim(TestName)
       write(*,103)   'Write Time Avg (usec) =',WriteTimeAvg
       write(*,103)   'Read Time Avg (usec) =',ReadTimeAvg

102    format(3x,5(a,f9.1),1x,a,1x,a)
103    format(3x,a,f9.1)

       call dealloc_check(ReadBW, myname_//':: ReadBW')
       call dealloc_check(WriteBW, myname_//':: WriteBW')

    endif    ! (nTrials > 1)
    write(*,*) '-----------------------------------------'

  end subroutine WriteTimeTrialsStats

  !=======================================================================
#ifdef TIMING
  subroutine get_memusage(msize,mrss)

    integer :: msize,mrss

    integer :: mshare,mtext,mdatastack
    integer :: ierr
    integer :: GPTLget_memusage

    ierr = GPTLget_memusage (msize, mrss, mshare, mtext, mdatastack)

  end subroutine get_memusage
#endif
  !=============================================================================

  subroutine c1dto3d(gindex,nx,ny,nz,i,j,k)

    implicit none
    integer,intent(in) :: gindex,nx,ny,nz
    integer,intent(out) :: i,j,k

    k = (gindex                          - 1) / (nx*ny) + 1
    j = (gindex - (k-1)*nx*ny            - 1) / (nx)    + 1
    i = (gindex - (k-1)*nx*ny - (j-1)*nx - 1)           + 1

  end subroutine c1dto3d

  !=============================================================================
  subroutine check_pioerr(ierr, file, line, str1, str2)

    implicit none
    integer(i4),intent(in) :: ierr
    character(len=*),intent(in) :: file
    integer(i4),intent(in) :: line
    character(len=*),optional,intent(in) :: str1
    character(len=*),optional,intent(in) :: str2

    character(len=256) lstr1
    character(len=256) lstr2
    character(len=*),parameter :: myname_='check_pioerr'

    lstr1 = ''
    if (present(str1)) then
       lstr1 = trim(str1)
    endif
    lstr2 = trim(lstr1)
    if (present(str2)) then
       lstr2 = trim(str2)
    endif

    if(ierr /= PIO_noerr) then
       write(*,*) trim(myname_),':: ERROR on my_task=',my_task,' ierr=',ierr,'  ',trim(lstr1)
       call piodie(file,line,trim(lstr2))
    endif

  end subroutine check_pioerr
  !=============================================================================
#endif

/**
 * @file      namelist.h
 *
 * @brief     Class to parse PIO namelists
 *
 * Class definition for parsing PIO namelists.
 * Class io_nml contains functions for parsing PIO namelists along with
 * the data structures into which those namelist quantities are inserted
 *
 * @version    $Id:  $
 *
 * License: GPL v3.0 (http://www.gnu.org/licenses/gpl.html)
 */

#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include "namelist.h"
#include "pio.h"

#define MAX_NAMELIST_LINELENGTH 512

const std::string io_nml::trim(const std::string& pString,
                               const std::string& pWhitespace = " \t")
{
  const size_t beginStr = pString.find_first_not_of(pWhitespace);
  if (beginStr == std::string::npos) {
    // no content
    return "";
  }

  const size_t endStr = pString.find_last_not_of(pWhitespace);
  const size_t range = endStr - beginStr + 1;

  return pString.substr(beginStr, range);
}
std::string io_nml:: readInputLine(ifstream &infile, i4 &ierror,
                                   const char *filename) {
  char *input[MAX_NAMELIST_LINELENGTH];
  std::string output = "";
  infile.getline(input, MAX_NAMELIST_LINELENGTH);
  linenum++;
  if (infile.rdstate() & ifstream::failbit) {
    std::cerr << "Error reading " << filename << " at line "
              << linenum << ", premature end of file" << std::endl;
    ierror = -1;
  } else if (ifs.rdstate() & ifstream::bad) {
    std::cerr << "Error reading " << filename << " at line "
              << linenum << std::endl;
    ierror = -1;
  } else if (ifs.rdstate() & ifstream::eofbit) {
    ierror = 0;
  }
  else {
    output = trim(input);
  }
  return output;
}

bool io_nml::parseInputLine(std::string &line,
                            std::string &varString, std::string &varValue) {
  bool retval = false;
  size_t eqpos;

  // Find an equal sign
  eqpos = line.find("=");
  if (eqpos != std::string::npos) {
    varString = trim(line.substr(0, eqpos));
    varValue = trim(line.substr((eqpos + 1)));
    retval = true;
  }
  // No else needed, just return false

  return retval;
}

io_nml:io_nml() {
  //--------------------------------------------------
  // set default values for namelist io_nml variables 
  //--------------------------------------------------

  async = false;
  DebugLevel = 2;
  stride = 0;
  base = 0;
  nx_global = 3600;
  ny_global = 2400;
  nz_global = 1;
  num_iotasks = -1;
  num_aggregator = 4;
  nprocsIO = 0;
  num_iodofs = 1;
  strcpy(compdof_input, "namelist");
  strcpy(part_input, "null");
  strcpy(iodof_input, "internal");
  strcpy(compdof_output, "none");
  nvars = 10;

  max_buffer_size = -1;  // use default value;
  block_size = -1;       // use default value;

  npr_yz = { nprocs, 1, 1, nprocs };
  set_mpi_values = 0 ; // Set to one for true;
  strcpy(mpi_cb_buffer_size, "");

  strcpy(set_romio_values, 0);  // Set to one for true
  strcpy(romio_cb_write, "");   // Default is "automatic"
  strcpy(romio_cb_read, "");    // Default is "automatic"
  strcpy(romio_direct_io, "");  // Default is "automatic"

  set_ibm_io_values = 0; // Set to one for true
  strcpy(ibm_io_buffer_size, "");
  strcpy(ibm_io_largeblock_io, "");  // Default is "false"
  strcpy(ibm_io_sparse_access, "");  // Default is false

  set_lustre_values = 0;
  lfs_ost_count = 1;

  strcpy(ioFMT, "bin");
  strcpy(dir, "./");
  strcpy(casename, "");
  strcpy(rearr, "box");
  maxiter = 10;

  linenum = 0;
}

void io_nml::assignValue(std::string &varName, std::string &varValue) {
  bool error = false;
  if (0 == varName.compare("async")) {
    std::transform(varValue.begin(), varValue.end(),
                   varValue.begin(), ::tolower);
    async = (0 == varValue.compare(".false."));
  } else if (0 == varName.compare("stride")) {
    stride = atoi(varValue.c_str());
  } else if (0 == varName.compare("base")) {
    base = atoi(varValue.c_str());
  } else if (0 == varName.compare("num_aggregator")) {
    num_aggregator = atoi(varValue.c_str());
  } else if (0 == varName.compare("nx_global")) {
    nx_global = atoi(varValue.c_str());
  } else if (0 == varName.compare("ny_global")) {
    ny_global = atoi(varValue.c_str());
  } else if (0 == varName.compare("nz_global")) {
    nz_global = atoi(varValue.c_str());
  } else if (0 == varName.compare("nvars")) {
    nvars = atoi(varValue.c_str());
  } else if (0 == varName.compare("dir")) {
    dir = varValue;
  } else if (0 == varName.compare("max_buffer_size")) {
    max_buffer_size = atoi(varValue.c_str());
  } else if (0 == varName.compare("block_size")) {
    block_size = atoi(varValue.c_str());
  } else if (0 == varName.compare("casename")) {
    casename = varValue;
  } else if (0 == varName.compare("maxiter")) {
    maxiter = atoi(varValue.c_str());
  } else if (0 == varName.compare("ioFMT")) {
    ioFMT = varValue;
  } else if (0 == varName.compare("rearr")) {
    rearr = varValue;
  } else if (0 == varName.compare("nprocsIO")) {
    nprocsIO = atoi(varValue.c_str());
  } else if (0 == varName.compare("num_iodofs")) {
    num_iodofs = atoi(varValue.c_str());
  } else if (0 == varName.compare("compdof_input")) {
    compdof_input = varValue;
  } else if (0 == varName.compare("compdof_output")) {
    compdof_output = varValue;
  } else if (0 == varName.compare("iodof_input")) {
    iodof_input = varValue;
  } else if (0 == varName.compare("part_input")) {
    part_input = varValue;
  } else if (0 == varName.compare("DebugLevel")) {
    DebugLevel = atoi(varValue.c_str());
  } else if (0 == varName.compare("npr_yz")) {
    npr_yz = varValue;
  } else if (0 == varName.compare("set_mpi_values")) {
    set_mpi_values = atoi(varValue.c_str());
  } else if (0 == varName.compare("mpi_cb_buffer_size")) {
    mpi_cb_buffer_size = atoi(varValue.c_str());
  } else if (0 == varName.compare("set_romio_values")) {
    set_romio_values = atoi(varValue.c_str());
  } else if (0 == varName.compare("romio_cb_write")) {
    romio_cb_write = varValue;
  } else if (0 == varName.compare("romio_cb_read")) {
    romio_cb_read = varValue;
  } else if (0 == varName.compare("romio_direct_io")) {
    romio_direct_io = varValue;
  } else if (0 == varName.compare("set_ibm_io_values")) {
    set_ibm_io_values = atoi(varValue.c_str());
  } else if (0 == varName.compare("ibm_io_buffer_size")) {
    ibm_io_buffer_size = varValue;
  } else if (0 == varName.compare("ibm_io_largeblock_io")) {
    ibm_io_largeblock_io = varValue;
  } else if (0 == varName.compare("ibm_io_sparse_access")) {
    ibm_io_sparse_access = varValue;
  } else if (0 == varName.compare("set_lustre_values")) {
    set_lustre_values = atoi(varValue.c_str());
  } else if (0 == varName.compare("lfs_ost_count")) {
    lfs_ost_count = atoi(varValue.c_str());
  } else {
    std::cerr << "ERROR: Bad variable name, " << varName
              << " found in namelist, ignoring." << std::endl;
    error = true;
  }
  if (!error) {
    std::cout << varName << " = " << varValue << std::endl;
  }
}

void io_nml::ReadTestPIO_Namelist(i4 nprocs, char *filename,
                                  char *caller, i4 &ierror) {

  const std::string myname = "namelist_mod"; //'ReadPIO_Namelist' ??goldy??
  std::string namelist_name = "io_nml";

  namelist_name = "&" + namelist_name;

  ifstream ifs (filename , ifstream::in);

  if((ifs.rdstate() & ifstream::failbit) != 0) {
    std::cerr << caller << "->" << myname << ":: Error opening file "
              << filename << std::endl;
    ierror = -1;
  } else {
    ierror = 1;
  }
    
  std::cout << "namelist input from " << filename << std::endl;
  
  bool in_namelist = false;
  std::string InputLine;
  while (ierror > 0) {
    InputLine = readInputLine(ifs, ierror);
    if (ierror > 0) {
      // Process line
      if (in_namelist) {
        // parse line
        if (0 == inpline.compare("/")) {
          in_namelist = false;
          // We are done with the namelist, might as well ignore rest of file
          ierror = 0;
        } else {
          // We should have a var = value line but if not, ignore
          std::string varName;
          std::string varValue;
          if (parseInputLine(inpline, varName, varValue)) {
            assignValue(varName, varValue);
          }
          // No else, just ignore malformed lines (for now).
        }
      } else {
        // Look for namelist tag
        if (0 == inpline.compare(namelist_name)) {
          in_namelist = true;
        }
        // No else, we just assume we haven't gotten to the namelist yet
      }
    }
  }

  ifs.close();

  if(nvars > 99999) {
    std::cout << "nvars exceeds limit of 99999, resetting" << std::endl;
    nvars = 99999;
  } else if (nvars < 1) {
    std::cout <<  "nvars < 1, resetting" << std::endl;
    nvars = 1;
  }

    // string = "namelist_input"
    // write(*,*) " "
    // write(*,*) trim(string)," async      = ",async
    // write(*,*) trim(string)," casename   = ",trim(casename)
    // write(*,*) trim(string)," nx_global  = ",nx_global
    // write(*,*) trim(string)," ny_global  = ",ny_global
    // write(*,*) trim(string)," nz_global  = ",nz_global
    // write(*,*) trim(string)," nvars      = ",nvars
    // write(*,*) trim(string)," ioFMT      = ",ioFMT
    // write(*,*) trim(string)," rearr      = ",rearr
    // write(*,*) trim(string)," nprocsIO   = ",nprocsIO
    // write(*,*) trim(string)," base       = ",base
    // write(*,*) trim(string)," stride     = ",stride
    // write(*,*) trim(string)," num_aggregator = ",num_aggregator
    // write(*,*) trim(string)," num_iodofs = ",num_iodofs
    // write(*,*) trim(string)," maxiter    = ",maxiter
    // write(*,*) trim(string)," dir        = ",trim(dir)
    // write(*,*) trim(string)," npr_yz     = ",npr_yz
    // write(*,*) trim(string)," DebugLevel = ",DebugLevel
    // write(*,*) trim(string)," DebugLevel = ",DebugLevel
    // write(*,*) trim(string)," compdof_input  = ",trim(compdof_input)
    // write(*,*) trim(string)," compdof_output = ",trim(compdof_output)
    // write(*,*) trim(string)," iodof_input = ",trim(iodof_input)
    // write(*,*) trim(string)," part_input =", trim(part_input)
    // if (set_mpi_values /= 0) then
    //    if (mpi_cb_buffer_size /= "") then
    //       write(*,*) trim(string)," mpi_cb_buffer_size = ", &
    //            trim(mpi_cb_buffer_size)
    //    end if
    // end if

    // if (set_romio_values /= 0) then
    //    if (romio_cb_write /= "") then
    //       write(*,*) trim(string)," romio_cb_write = ", romio_cb_write
    //    end if

    //    if (romio_cb_read /= "") then
    //       write(*,*) trim(string)," romio_cb_read = ", romio_cb_read
    //    end if

    //    if (romio_direct_io /= "") then
    //       write(*,*) trim(string)," romio_direct_io = ", romio_direct_io
    //    end if
    // end if

    // if (set_ibm_io_values /= 0) then
    //    if (ibm_io_buffer_size /= "") then
    //       write(*,*) trim(string),"ibm_io_buffer_size = ", &
    //            trim(ibm_io_buffer_size)
    //    end if

    //    if (ibm_io_largeblock_io /= "") then
    //       write(*,*) trim(string),"ibm_io_largeblock_io = ", &
    //            trim(ibm_io_largeblock_io)
    //    end if

    //    if (ibm_io_sparse_access /= "") then
    //       write(*,*) trim(string),"ibm_io_sparse_access = ", &
    //            trim(ibm_io_sparse_access)
    //    end if
    // end if

    // if (set_lustre_values /= 0) then
    //    write(*,*) trim(string),"lfs_ost_count = ", lfs_ost_count
    // endif

    // write(*,*) " "

    // string = "derived_input"
    // select case(trim(rearr))
    // case("none")
    //    rearr_type=PIO_rearr_none
    //    write(*,*) trim(string)," rearr_type = ","PIO_rearr_none"
    // case("box")
    //    rearr_type=PIO_rearr_box
    //    write(*,*) trim(string)," rearr_type = ","PIO_rearr_box"
    // case default
    //    write(*,"(6a)") caller,"->",myname,":: Value of Rearranger type rearr = ",rearr, &
    //         "not supported."
    //    call piodie(__FILE__,__LINE__)
    // end select
    // write(*,*) trim(string)," rearr_type = ",rearr_type

    // iofmtd = iofmt
    // select case(ioFMT)
    //   case("bin") ! binary format
    //      iotype = iotype_pbinary
    //      write(*,*) trim(string)," iotype     = ","iotype_pbinary"
    //   case("pnc") !Parallel netCDF
    //      iotype = iotype_pnetcdf
    //      ioFmtd = "nc"
    //      write(*,*) trim(string)," iotype     = ","iotype_pnetcdf"
    //   case("snc") ! serial netCDF
    //      iotype = iotype_netcdf
    //      ioFmtd = "nc"
    //      write(*,*) trim(string)," iotype     = ","iotype_netcdf"
    //   case("nc4p") ! netCDF4 parallel
    //      iotype = PIO_iotype_netcdf4p
    //      ioFmtd = "nc"
    //      write(*,*) trim(string)," iotype     = ","PIO_iotype_netcdf4p"
    //   case("nc4c") ! netCDF4 compressed
    //      iotype = PIO_iotype_netcdf4c
    //      ioFmtd = "nc"
    //      write(*,*) trim(string)," iotype     = ","PIO_iotype_netcdf4c"
    //   case default
    //      write(*,"(4a,i8)") caller,"->",myname,":: Unrecognized value of ioFMT =",ioFMT
    //      call piodie(__FILE__,__LINE__)
    // end select
    // write(*,*) trim(string)," iofmtd     = ",trim(iofmtd)

  num_iotasks = -1;
  if (nprocsIO > 0) {
    num_iotasks = nprocsIO;
    if ((stride <= 0) || (stride > nprocs)) {
      stride = (nprocs - base) / num_iotasks;
    }
  } else (nprocsIO <= 0) {
#ifdef BGx 
      // A negative value for num_iotasks has a special meaning on Blue Gene
      num_iotasks = nprocsIO;
#else
      if ((stride <= 0) || (stride > nprocs)) {
        num_iotasks = nprocs;
        stride = 1;
        base=0;
      } else {
        num_iotasks = max(1, (nprocs - base) / stride);
      }
#endif
    }

  //------------------------------------------------
  // reset stride if there are not enough processors 
  //------------------------------------------------
  if (base + num_iotasks * (stride-1) > nprocs-1) {
    stride = FLOOR(real((nprocs - 1 - base),kind=r8)/real(num_iotasks,kind=r8));
  }

  //-------------------------------------------------------
  // If rearrangement is "none" reset to the proper values
  //-------------------------------------------------------
  if(trim(rearr) == "none") {
    stride = 1;
    num_iotasks = nprocs	;
  }

    // write(*,*) trim(string)," n_iotasks  = ",num_iotasks,"  (updated)"
    // write(*,*) trim(string)," base       = ",base,"  (updated)"
    // write(*,*) trim(string)," stride     = ",stride,"  (updated)"
    // write(*,*) " "

    //--- error check

//    string = "namelist_ERROR:"
//    print *,"ReadTestPIO_Namelist: at the end"

}

subroutine Broadcast_Namelist(caller, myID, root, comm, ierror)

  use pio ! _EXTERNAL
#ifndef NO_MPIMOD
  use mpi ! _EXTERNAL
#endif
  implicit none
#ifdef NO_MPIMOD
  include "mpif.h" ! _EXTERNAL
#endif
  character(len=*), intent(IN) :: caller
  integer(i4),      intent(IN)  :: myID
  integer(i4),      intent(IN)  :: root
  integer(i4),      intent(IN)  :: comm

  integer(i4),      intent(OUT) :: ierror

  character(len=*), parameter :: myname_=myname//'Broadcast_Namelist'
  integer(i4) :: itmp

  !------------------------------------------
  ! broadcast namelist info to all processors 
  !------------------------------------------

  if(async) then
     itmp=1
  else
     itmp=0
  end if

  call MPI_Bcast(itmp, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(async)",ierror,__FILE__,__LINE__)

  if(itmp==1) then
     async=.true.
  else
     async=.false.
  end if


  call MPI_Bcast(num_iotasks, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(num_iotasks)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(num_iodofs, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(num_iodofs)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(num_aggregator, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(num_aggregator)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(stride, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(stride)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(base, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(base)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(nx_global, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(nx_global)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(ny_global, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(ny_global)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(nz_global, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(nz_global)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(nvars, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(nvars)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(set_mpi_values, 1, MPI_INTEGER, root, comm,ierror)
  call CheckMPIReturn("Call to MPI_Bcast(set_mpi_values)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(mpi_cb_buffer_size, buffer_size_str_len, MPI_CHARACTER, &
                   root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(mpi_cb_buffer_size)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(set_romio_values, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(set_romio_values)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(romio_cb_write, romio_str_len, MPI_CHARACTER, root, &
                   comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(romio_cb_write)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(romio_cb_read, romio_str_len, MPI_CHARACTER, root, &
                   comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(romio_cb_read)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(romio_direct_io, romio_str_len, MPI_CHARACTER, root, &
                   comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(romio_direct_io)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(set_ibm_io_values, 1, MPI_INTEGER, root, comm, &
                   ierror)
  call CheckMPIReturn("Call to MPI_Bcast(set_ibm_io_values)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(ibm_io_buffer_size, buffer_size_str_len, MPI_CHARACTER, &
                   root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(ibm_io_buffer_size)", ierror, &
         __FILE__, __LINE__)

  call MPI_Bcast(ibm_io_largeblock_io, true_false_str_len, MPI_CHARACTER, &
                   root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(ibm_io_largeblock_io)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(ibm_io_sparse_access, true_false_str_len, MPI_CHARACTER, &
                   root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(ibm_io_sparse_access)", ierror, &
                        __FILE__, __LINE__)

  call MPI_Bcast(set_lustre_values,1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcase(set_lustre_values)", ierror, __FILE__, __LINE__)

  call MPI_Bcast(lfs_ost_count,1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcase(lfs_ost_count)", ierror, __FILE__, __LINE__)

  call MPI_Bcast(iotype, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(iotype)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(ioFMTd, 4, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(ioFMTd)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(rearr, 8, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(rearr)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(rearr_type, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(rearr_type)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(dir, 80, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(dir)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(compdof_input, 80, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(compdof_input)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(compdof_output, 80, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(compdof_output)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(iodof_input, 80, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(iodof_input)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(part_input, 256, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(part_input)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(casename, 256, MPI_CHARACTER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(casename)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(DebugLevel, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(DebugLevel)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(maxiter, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(maxiter)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(nprocsIO, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(nprocsIO)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(npr_yz, 4, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(npr_yz)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(max_buffer_size, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(npr_yz)",ierror,__FILE__,__LINE__)

  call MPI_Bcast(block_size, 1, MPI_INTEGER, root, comm, ierror)
  call CheckMPIReturn("Call to MPI_Bcast(npr_yz)",ierror,__FILE__,__LINE__)

  if(max_buffer_size>0) then
     if(myid==0) print *,"Setting buffer_size_limit to : ",max_buffer_size
     call pio_set_buffer_size_limit(max_buffer_size)
  end if
  if(block_size>0) then
     if(myid==0) print *,"Setting blocksize to : ",block_size
     call pio_set_blocksize(block_size)
  end if

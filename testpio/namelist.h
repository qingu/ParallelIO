/**
 * @file      namelist.h
 *
 * @brief     Class to parse PIO namelists
 *
 * Class declaration for parsing PIO namelists.
 * Class io_nml contains functions for parsing PIO namelists along with
 * the data structures into which those namelist quantities are inserted
 *
 * @version    $Id:  $
 *
 * License: GPL v3.0 (http://www.gnu.org/licenses/gpl.html)
 */

#ifndef __NAMELIST_H_INCLUDED_
#define __NAMELIST_H_INCLUDED_

#ifdef BGP
#define BGx
#endif
#ifdef BGL
#define BGx
#endif

#include <iostream>
#include <fstream>

#include "pio_kinds.h"

// Modules from PIO package that are used by this application
//    use pio_support, only : piodie, CheckMPIReturn ! _EXTERNAL


// Namelist definitions
#define BUFFER_SIZE_STR_LEN (20)
#define TRUE_FALSE_STR_LEN (6)
#define ROMIO_STR_LEN (10)
#define FNAME_LEN (80)
    
class io_nml {
public:
  // Functions for dealing with PIO namelists

  io_nml(int nprocs);

  void Broadcast_Namelist(char * caller, int myID, int root,
                          int comm, int &ierror);

  void ReadTestPIO_Namelist(int nprocs, char *filename,
                            char *caller, int &ierror);
private:
  std::string trim(const std::string& pString,
                   const std::string& pWhitespace = " \t");

  std::string readInputLine(std::ifstream &infile, int &ierror,
                            const char *filename);

  bool parseInputLine(std::string &line,
                      std::string &varString, std::string &varValue);

  void assignValue(std::string &varName, std::string &varValue);
  int linenum;

public:
  // Data (all public for now)
  bool async;
  int nx_global,ny_global,nz_global;
  int rearr_type;
  int num_iotasks;
  int stride;
  int base;
  int DebugLevel;
  int maxiter;
  int num_aggregator;
  int iotype;
  int num_iodofs;
  int nvars;
  int npr_yz[4];   // To simulate cam fv decompositions

  int set_mpi_values; // Set to one for true
  char mpi_cb_buffer_size[BUFFER_SIZE_STR_LEN];
  int set_romio_values; // Set to one for true
  char romio_cb_write[ROMIO_STR_LEN];
  char romio_cb_read[ROMIO_STR_LEN];
  char romio_direct_io[ROMIO_STR_LEN];
  int set_ibm_io_values; // Set to one for true
  char ibm_io_buffer_size[BUFFER_SIZE_STR_LEN];
  char ibm_io_largeblock_io[TRUE_FALSE_STR_LEN];
  char ibm_io_sparse_access[TRUE_FALSE_STR_LEN];

  int set_lustre_values; // Set to one for true
  int lfs_ost_count;
    
  char compdof_input[80];
  char compdof_output[80];
  char iodof_input[80] ;
  char part_input[256];
  char casename[256];
  char dir[80];
  char ioFMT[4];
  char ioFMTd[4];

  int nprocsIO;
  int PrintRec;
  char fname1[FNAME_LEN], fname2[FNAME_LEN];
  int max_buffer_size;
  int block_size;
};

#endif // __NAMELIST_H_INCLUDED_

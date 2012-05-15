MAINTAINERCLEANFILES = Makefile.in

TOPDIR=.
export TOPDIR

if TIMING
  TIMINGDIR = timing
else
  TIMINGDIR =
endif

if PIOCPP
  PIOCPPDIR = piocpp
else
  PIOCPPDIR =
endif

if TESTING
  PIOTESTDIR = testpio
else
  PIOTESTDIR =
endif

SUBDIRS = pio $(PIOCPPDIR) $(TIMINGDIR) $(PIOTESTDIR)

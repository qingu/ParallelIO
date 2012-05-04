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

SUBDIRS = pio $(TIMINGDIR) $(PIOCPPDIR)

MAINTAINERCLEANFILES = Makefile.in

AC_CONFIG_FILES([Makefile pio/Makefile timing/Makefile piocpp/Makefile])

SUBDIRS = pio

if BUILD_TIMING
  SUBDIRS: += $(SUBDIRS) timing
endif

if BUILD_PIOCPP
  SUBDIRS:+= $(SUBDIRS) piocpp
endif

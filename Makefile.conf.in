ifeq ($(TIMING),yes)
  SUBDIRS = timing,pio
else
  SUBDIRS = pio
endif

ifeq ($(PIOCPP),yes)
  SUBDIRS:+= $(SUBDIRS),piocpp
endif

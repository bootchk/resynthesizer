## Process this file with automake to produce Makefile.in

## lkk 2011
## Note many .h files are not just declarations but definitions, that is, not pure header files.

noinst_LIBRARIES = libimagesynth.a

# Standalone engine is a static library
libimagesynth_a_SOURCES = \
  imageSynth.c \
  engine.c \
  engineParams.c \
  imageFormat.c

# Set buildswitches.h and use glibProxy.c for a library not dependent on glib


bin_PROGRAMS = resynthesizer resynthesizer_gui

bindir = $(GIMP_LIBDIR)/plug-ins

# resynthesizer is a GIMP plugin calling libimagesynth
resynthesizer_SOURCES = \
  resynthesizer.c \
  engineParams.c \
  imageFormat.c


# For certain configurations (not using glib),
# resynthesizer_SOURCES should include
#   glibProxy.c

# lkk Note glibProxy.c not needed for resynthesizer for GIMP
# but IS needed when using GIMP as a test harness to the standalone engine
  
# lkk 2011 These are 'sources' but not compiled, just included
# Files included by engine.c
#  mapIndex.h
#  orderTarget.h
#  passes.h
#  refiner.h
#  engineTypes.h
#  stats.h

# These are for the resynthesizer plugin adapter to the engine
#  resynth-pdb.h
#  resynth-parameters.c
#  resynth-constants.h


# resynthesizer_gui is a plugin that calls resynthesizer plugin
resynthesizer_gui_SOURCES = \
  resynth-gui.c \
  engineParams.c

# Files included by resynthesizer_gui
#  resynth-gui.h
#  resynth-parameters.c
#  resynth-constants.h 

# Work in progress building a shared dynamic library
#lib_LTLIBRARIES = libimagesynth-1.0.la
#libimagesynth_1_0_la_SOURCES = \
#  imageSynth.c \
#  engine.c \
#  glibProxy.c \
#  engineParams.c \
#  imageFormat.c


AM_CPPFLAGS = \
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"

INCLUDES =\
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir)

# resynthesizer depends on imagesynth library
LDADD = $(GIMP_LIBS) libimagesynth.a




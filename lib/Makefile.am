## Process this file with automake to produce Makefile.in

## lkk 2011
## Note many .h files are not just declarations but definitions, that is, not pure header files.

# engine is a 'convenience' (not installed), static library

noinst_LIBRARIES = libresynthesizer.a

libresynthesizer_a_SOURCES = \
  imageSynth.c \
  engine.c \
  engineParams.c \
  imageFormat.c

# Set buildswitches.h and use glibProxy.c for a library not dependent on glib
  
# lkk 2011 These are 'sources' but not compiled, just included
# Files included by engine.c
#  mapIndex.h
#  orderTarget.h
#  passes.h
#  refiner.h
#  engineTypes.h
#  stats.h


# Work in progress building a shared dynamic library
#lib_LTLIBRARIES = libresynthesizer-1.0.la
#libresynthesizer_1_0_la_SOURCES = \
#  imageSynth.c \
#  engine.c \
#  glibProxy.c \
#  engineParams.c \
#  imageFormat.c


# library does not require localization
AM_CPPFLAGS = \
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"

libresynthesizer_a_CPPFLAGS =\
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir)



# resynthesizer library depends on math and gthread libraries.
# Make explicit dependency on libm for atan2() and libgthread
# Fixes change to linker ld on some platforms that causes libm not to be found through other libraries.
# Does NOT depend on $(GIMP_LIBS)

# LIBADD is for additional objects to link into library.
# This library creates a dependency, but doesn't link these external libraries.
#libresynthesizer_a_LIBADD = -lm -lgthread-2.0




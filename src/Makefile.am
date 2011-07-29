## Process this file with automake to produce Makefile.in

## lkk 2011
## Note the two .c files are 'main.c' files and many .h files are not just declarations but definitions, that is, not pure header files.

bin_PROGRAMS = resynthesizer resynthesizer_gui 

## Don't make synth since it is not a plugin to be installed in the plug-in directory


bindir = $(GIMP_LIBDIR)/plug-ins

resynthesizer_SOURCES = \
  resynthesizer.c \
  engineParams.c \
  imageFormat.c \
  engine.c \
  map.c


# For certain configurations (not using glib),
# resynthesizer_SOURCES should include
#   glibProxy.c

# lkk Note glibProxy.c not needed for resynthesizer for GIMP
# but IS needed when using GIMP as a test harness to the standalone engine
  
# lkk 2011 These are 'sources' but not compiled, just included
#  resynth-pdb.h \
#  resynth-parameters.c \
#  resynth-constants.h \
#  resynth-types.h \
#  resynth-map-types.h \
#  resynth-order-target.h

resynthesizer_gui_SOURCES = \
  resynth-gui.c \
  engineParams.c

# Standalone engine i.e. library
imageSynth_SOURCES = \
  imageSynth.c \
  engine.c \
  glibProxy.c \
  engineParams.c \
  imageFormat.c
  
  
#  resynth-gui.h \
#  resynth-parameters.c \
#  resynth-constants.h 


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

LDADD = $(GIMP_LIBS)




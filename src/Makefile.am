## Process this file with automake to produce Makefile.in

## lkk 2011
## Note the two .c files are 'main.c' files and many .h files are not just declarations but definitions, that is, not pure header files.

bin_PROGRAMS = resynthesizer resynthesizer-gui

bindir = $(GIMP_LIBDIR)/plug-ins

resynthesizer_SOURCES = \
  resynthesizer.c \
  engine.c
  
# lkk 2011 These are 'sources' but not compiled, just included
#  resynth-pdb.h \
#  resynth-parameters.c \
#  resynth-constants.h \
#  resynth-types.h \
#  resynth-map-types.h \
#  resynth-order-target.h

resynthesizer_gui_SOURCES = \
  resynth-gui.c
  
#  resynth-gui.h \
#  resynth-parameters.c \
#  resynth-constants.h 

AM_CPPFLAGS = \
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"

INCLUDES =\
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir)

LDADD = $(GIMP_LIBS)




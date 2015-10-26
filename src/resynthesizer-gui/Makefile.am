## Process this file with automake to produce Makefile.in

bin_PROGRAMS = resynthesizer_gui

bindir = $(GIMP_LIBDIR)/plug-ins

# resynthesizer_gui is plugin that calls resynthesizer plugin
resynthesizer_gui_SOURCES = resynth-gui.c

# Files included by resynthesizer_gui
#  resynth-gui.h
#  resynth-parameters.c
#  resynth-constants.h 

AM_CPPFLAGS = \
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"

resynthesizer_gui_CPPFLAGS =\
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"  \
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir)


# resynthesizer-gui GIMP plugin calls GIMP lib but NOT convenience i.e. local libresynthesizer
# resynthesizer-gui invokes resynthesizer plugin via GIMP.
LDADD = $(GIMP_LIBS)






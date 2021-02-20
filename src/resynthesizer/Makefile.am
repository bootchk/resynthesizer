## Process this file with automake to produce Makefile.in

bin_PROGRAMS = resynthesizer

bindir = $(GIMP_LIBDIR)/plug-ins

# resynthesizer is a GIMP plugin calling libresynthesizer
resynthesizer_SOURCES = resynthesizer.c plugin.c debug.c
# TODO for 2.99 also need pluginParams.c


# lkk Note glibProxy.c not needed for resynthesizer for GIMP
# but IS needed when using GIMP as a test harness to the standalone engine
  

# These are for the resynthesizer plugin adaption to engine
#  resynth-pdb.h
#  resynth-parameters.c
#  resynth-constants.h

# resynthesizer plugin uses libresynthesizer
resynthesizer_CPPFLAGS =\
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"  \
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir) \
	-I$(srcdir)/../../lib


# resynthesizer GIMP plugin calls GIMP lib and convenience library (i.e. local) libresynthesizer
# libresynthesizer in turn depends on math and gthread-2.0
LDADD = $(GIMP_LIBS) ../../lib/libresynthesizer.a -lm -lgthread-2.0






## Process this file with automake to produce Makefile.in

bin_PROGRAMS = resynthesizer_gettext

bindir = $(GIMP_LIBDIR)/plug-ins

resynthesizer_gettext_SOURCES = resynthesizer-gettext.c

AM_CPPFLAGS = \
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"

resynthesizer_gettext_CPPFLAGS =\
	-DLOCALEDIR=\""$(LOCALEDIR)"\"		\
	-DDATADIR=\""$(DATADIR)"\"  \
	-I$(top_srcdir)		\
	@GIMP_CFLAGS@		\
	-I$(includedir)

LDADD = $(GIMP_LIBS)

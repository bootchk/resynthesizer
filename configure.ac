dnl Process this file with autoconf to produce a configure script.

dnl lkk 2011 Hacked from gimp-plugin-template configure.in
dnl changed name and added PluginScript/Makefile

AC_PREREQ(2.54)

dnl Name your plug-in here
dnl lkk 2011 Changed name and version digits
m4_define([plugin_name], [resynthesizer])

dnl These three define the plug-in version number
m4_define([plugin_major_version], [1])
m4_define([plugin_minor_version], [0])
m4_define([plugin_micro_version], [0])

m4_define([plugin_version],
          [plugin_major_version.plugin_minor_version.plugin_micro_version])

AC_INIT([plugin_name], [plugin_version])

AC_DEFINE(PLUGIN_NAME, PACKAGE_NAME, [Plug-In name])
AC_DEFINE(PLUGIN_VERSION, PACKAGE_VERSION, [Plug-In version])

AC_DEFINE(PLUGIN_MAJOR_VERSION, plugin_major_version, [Plug-In major version])
AC_DEFINE(PLUGIN_MINOR_VERSION, plugin_minor_version, [Plug-In minor version])
AC_DEFINE(PLUGIN_MICRO_VERSION, plugin_micro_version, [Plug-In micro version])

dnl lkk This macro only confirms that the given file is in configure's notion of source dir
AC_CONFIG_SRCDIR([src/resynthesizer/resynthesizer.c])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE(no-define)

AC_ISC_POSIX
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC


ACLOCAL="$ACLOCAL $ACLOCAL_FLAGS"

AM_MAINTAINER_MODE


dnl Use -Wall if we have gcc.
changequote(,)dnl
if test "x$GCC" = "xyes"; then
  case " $CFLAGS " in
  *[\ \	]-Wall[\ \	]*) ;;
  *) CFLAGS="$CFLAGS -Wall" ;;
  esac
fi
changequote([,])dnl


GIMP_REQUIRED_VERSION=2.2.0

PKG_CHECK_MODULES(GIMP,
  gimp-2.0 >= $GIMP_REQUIRED_VERSION gimpui-2.0 >= $GIMP_REQUIRED_VERSION,
  ,AC_MSG_ERROR(GIMP development libraries not found; please install.))

AC_SUBST(GIMP_CFLAGS)
AC_SUBST(GIMP_LIBS)

GIMP_LIBDIR=`$PKG_CONFIG --variable=gimplibdir gimp-2.0`
AC_SUBST(GIMP_LIBDIR)


dnl i18n stuff

dnl lkk 2011 Changed name here
GETTEXT_PACKAGE=resynthesizer
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE",
				    [The gettext translation domain.])

dnl Add the languages which your application supports here.
ALL_LINGUAS="es fr ru"

AC_PROG_INTLTOOL
AM_GLIB_GNU_GETTEXT

AC_CHECK_FUNCS(bind_textdomain_codeset)

dnl Fix for changes in GLib 2.31 
AC_CHECK_LIB(gthread-2.0, g_thread_init)

dnl lkk 2011 I copied this from the main gimp configure.in
dnl where $PACKAGE is gimp and gimp_plugin_version is 2.0
dnl gimpplugindir="$libdir/$PACKAGE/gimp_plugin_version"
gimpplugindir="$libdir/gimp/2.0"
AC_SUBST(gimpplugindir)

LOCALEDIR='${datadir}/locale'
DATADIR='${datadir}/plugin_name'

AC_SUBST(LOCALEDIR)
AC_SUBST(DATADIR)


AC_MSG_CHECKING([if GTK+ is version 2.7.0 or newer])
if $PKG_CONFIG --atleast-version=2.7.0 gtk+-2.0; then
  have_gtk_2_7=yes
else
  have_gtk_2_7=no
fi
AC_MSG_RESULT($have_gtk_2_7)

if test "x$have_gtk_2_7" != "xyes"; then
  CPPFLAGS="$CPPFLAGS -DG_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED"
fi


AC_MSG_CHECKING([if GIMP is version 2.3.0 or newer])
if $PKG_CONFIG --atleast-version=2.3.0 gimp-2.0; then
  have_gimp_2_3=yes
else
  have_gimp_2_3=no
fi
AC_MSG_RESULT($have_gimp_2_3)

if test "x$have_gimp_2_3" != "xyes"; then
  CPPFLAGS="$CPPFLAGS -DGIMP_DISABLE_DEPRECATED"
fi

dnl lkk July 2011
AC_PROG_RANLIB
dnl AC_PROG_LIBTOOL

dnl lkk 2011 Added PluginScripts/Makefile
dnl lkk 2015 Break src into lib, src/resynthesizer, src/resynthesizer-gui
AC_CONFIG_FILES([
Makefile
lib/Makefile
src/Makefile
src/resynthesizer/Makefile
src/resynthesizer-gui/Makefile
po/Makefile.in
help/Makefile
help/en/Makefile
help/images/Makefile
PluginScripts/Makefile
])

AC_OUTPUT

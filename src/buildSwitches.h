
/*
Compiling switches.

!!! Inner engine is now independent of GIMP, not: #include <libgimp/gimp.h>

The GIMP plugin resynthesizer (an engine, but wraps inner engine) depends on:
GNU build system, gimp.h, and glib.h.

There are three configurations:

1) resynthesizer plugin and engine uses gimp and glib and not glibProxy (original configuration)
2) resynthesizer plugin uses gimp but engine uses glibProxy to redefine most of glib
	(This is for testing the gimp independent engine and glibProxy)
3) Engine uses glibProxy, not gimp or glib.  (a platform independent engine.)
*/

/* 
Please comment out all but one of these configurations. 
Also, for config 1, edit Makefile.am to remove glibproxy.c as resynthesizer_SOURCE
then run >automake from the top directory.
*/

/* config 1 */
// #define SYNTH_USE_GLIB TRUE

/* config 2 */
/*
#define SYNTH_USE_GLIB TRUE
#define USE_GLIB_PROXY TRUE
*/

/* config 3 */
#define USE_GLIB_PROXY TRUE



// #define ADAPT_SIMPLE TRUE // Adapt engine to simpleAPI. 
// Only for using resynthesizer plugin for testing adaption to independent engine.


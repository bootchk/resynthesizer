
/*
Compiling switches.

!!! Inner engine is now independent of GIMP, not: #include <libgimp/gimp.h>

The GIMP plugin resynthesizer (an engine, but wraps inner engine) depends on:
GNU build system, gimp.h, and glib.h.

There are three configurations:

1) resynthesizer plugin and engine uses gimp and glib and not glibProxy (original configuration)
2) resynthesizer plugin uses gimp but engine uses glibProxy to redefine most of glib
	(This is for testing using a gimp plugin harness to the gimp independent engine and glibProxy)
3) libsynth.a (inner engine) uses glibProxy, not gimp or glib.  (platform independent engine.)

  Copyright (C) 2010, 2011  Lloyd Konneker

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* 
Please comment out all but one of these configurations. 
Also, for config 1, edit Makefile.am to remove glibproxy.c as resynthesizer_SOURCE
then run >automake from the top directory.
*/
#ifdef SYNTH_LIB_ALONE
#define USE_GLIB_PROXY TRUE
#else
/* config 1 */
#define SYNTH_USE_GLIB TRUE

/* config 2 */
/*
#define SYNTH_USE_GLIB TRUE

#define USE_GLIB_PROXY TRUE
*/

/* config 3 */
// #define USE_GLIB_PROXY TRUE
#endif


// #define ADAPT_SIMPLE TRUE // Adapt engine to simpleAPI. 
// Only for using resynthesizer plugin for testing adaption to independent engine.

// Bring in alternative code: experimental, debugging, etc.
#define DEEP_PROGRESS // call progressCallback often, from inside synthesis()
// #define ANIMATE    // Animate image while processing, for debugging.
// #define DEBUG

// VECTORIZED requires SYMMETRIC_METRIC_TABLE
// #define SYMMETRIC_METRIC_TABLE
// #define VECTORIZED

/*
Threading.
Requires file refinerThreaded.h
Primarily affects file synthesize.h
Whether threading is POSIX threads or glib threads depends on SYNTH_USE_GLIB

On Linux and other platforms having glib, define SYNTH_THREADED TRUE and SYNTH_USE_GLIB_THREADS
On other platforms not having glib, define SYNTH_THREADED TRUE and use glibproxy
The latter has not been tested.
*/
#define SYNTH_THREADED TRUE
// If not defined, uses POSIX threads.  Moot unless SYNTH_THREADED
#if !defined(_WIN32) && !defined(_WIN64)
#define SYNTH_USE_GLIB_THREADS
#endif
// Count threads to start.
#ifdef SYNTH_THREADED
  // A reasonable guess that most current processors have no more than 8 cores.
  // More threads than cores does not seem to hurt performance.
  // glib doesn't seem to support knowing the count of cores
  #define THREAD_LIMIT    8
#else
  // This MUST be defined to 1 if not threaded, it affects how synthesize() iterates over target
  #define THREAD_LIMIT 1
#endif


/*
Uncomment this before release.  
Or disables assertions on the command line to gcc.
Also, uncomment when using splint.
Leave it commented for development and testing, to enable assertions.
*/
// #define G_DISABLE_ASSERT      // To disable g_assert macro, uncomment this.
// #define NDEBUG                // Or this if not using glib


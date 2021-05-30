# GIMP Resynthesizer Plugin Suite

A set of plugins for the GIMP image editing app.

  Copyright 2000 2008  Paul Francis Harrison  
  Copyright 2002  Laurent Despeyroux  
  Copyright 2002  David Rodríguez García  
  Copyright 2010-2021  Lloyd Konneker 

[![Build Status](https://travis-ci.org/bootchk/resynthesizer.svg?branch=master)](https://travis-ci.org/bootchk/resynthesizer)

A library implementing the "resynthesizer" algorithm for texture transfer among images.

And a suite of plugins, that use the algorithm, for the GIMP image processing application.
The plugins do _not_ come with GIMP, they are "third-party" plugins.


## Resources

[Quick user's guide to the Resynthesizer plugins for GIMP](https://github.com/bootchk/resynthesizer/wiki/Quick-user's-guide-to-the-Resynthesizer-plugins-for-GIMP)



## Installation

[install flatpaked Resynthesizer for Linux](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#flatpak)

[install Resynthesizer for Windows](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#windows)

[install Resynthesizer for MacOS](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#mac-osx)

[build from source](https://github.com/bootchk/resynthesizer/wiki/Build-Resynthesizer-from-source)

## Acknowledgements

Paul Harrison had the original idea and implementation.  He did the hard work, took the leap of faith and experimented with a new algorithm.  His [website](http://www.logarithmic.net/pfh/) has many examples of using the Resynthesizer, links to an explanation of the algorithm, and other cool ideas.

I fixed a few bugs (due to changes in GIMP), rewrote the Scheme plugins into Python, wrote a few plugins, made it better deal with alpha (transparency), threaded it, and made it into a reentrant library in C.

Rob Antonishen contributed the basis for one plugin.

Several people have helped with the build process and with translations.





## Programming or hacking

### Architecture

Run-time dependencies: resynthesizer.exe is the base plugin.  All other plugins only depend on it as the engine.  All other plugins are optional.  Resynthesizer-gui.exe is the control panel for the engine, offering the "Map>Resynthesize" menu item.  Without it, all other plugins will still work.

This is the tree:

* libresynthesizer.a   a convenience library implementing the algorithm
 * resynthesizer.exe    a GIMP plugin calling the library.  Adapts the library to GIMP data structures and calling conventions.
   * resynthesizer-gui.exe  a GIMP plugin written in C, a control panel for, and invoking resynthesizer.exe via GIMP pdb
   * plugin-_foo_.py  a GIMP plugin written in Python, invoking resynthesizer.exe via GIMP pdb
 * FUTURE: other plugins that adapt the library to other application's data structures and plugin architecture

The source files are structured similarly, and have similar dependencies.

### Library dependencies

The resynthesizer library uses the C math library (libm) and optionally (when threaded) libgthreads.

The library also uses glib (for simple things like gassert and g_uint) but the repository includes hacky, compile-time options to obviate that.

The plugins depend on GIMP libraries, but the resynthesizer library does not.

### How it works

The algorithm does patch matching.  A patch is a pixel and a small set of surrounding pixels.  The patch need not be square or even contiguous.

The algorithm makes many passes.  In this algorithm, during the first pass, the target is _not_ filled completely in, it gradually gets filled in.  The empty pixels are processed in random order.  In the first pass, a patch is like a shotgun pattern: a small set of nearest pixels, but not necessarily nearby.  A new value for a pixel in the target is found by searching the corpus for the best matching patch to the target pixel's patch (including its surrounding that may not be in the target area per se.)

In subsequent passes, the target is already filled in, but with imperfect results.  But the patches are all regular, contiguous shapes (except for image borders.)  Subsequent passes also process pixels at random.

The algorithm uses a search heuristic: a new best match for a pixel is often found (in the corpus) at the opposite offset from a pixel (in the target) to a neighbor (in the target) pixel's best match (in the corpus.)

The algorithm also uses a special distance measure function (see Paul Harrison's thesis.)

There are other patch matching algorithms.  One starts by completely filling in the target with random guesses from the corpus.  Then the target is processed in a raster scan (scan line) order.  Again, matching patches.  A simple distance measure is used between patches.   Processing in raster order lets the algorithm reuse some of computations for pixels above and to the left.  Completely filling in the initial target lets the algorithm use square, vectorizable patches.  The simple distance measure is also vectorizable.

(I briefly explored vectorization of the resynthesizer code.  There are some remnants in the repository.)

You may also be interested in 'bidirectional similarity', which uses patch matching (and supplements it.)  It also seeks to "plausibly reconstruct" an image, and has many uses in image editing.  These algorithms in some sense attempt to understand 'objects' in the image.

Unlike many image filters, these algorithms need a global view of the image.  Thus they are very memory intensive, and not simple to multi-process.

### Future directions

The control panel plugin might break if GTK changes.  It should be rewritten in Python, PyGTK.

The resynthesizer library will support plugins for other applications.

The algorithm knows (but does not return) the coordinates of a pixel's best match.  That might be used in combination with Hough transforms to identify objects (lines, circles, etc.) in an image.

Divorce the library from dependencies on glib and gthreads.


### Using vagga to test building in containerized build environments

See the vagga directory.


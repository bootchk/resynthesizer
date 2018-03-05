# GIMP Resynthesizer Plugin Suite

A set of plugins for the GIMP image editing app.

  Copyright 2000 2008  Paul Francis Harrison  
  Copyright 2002  Laurent Despeyroux  
  Copyright 2002  David Rodríguez García  
  Copyright 2010-2011  Lloyd Konneker <bootch@nc.rr.com>  

[![Build Status](https://travis-ci.org/bootchk/resynthesizer.svg?branch=master)](https://travis-ci.org/bootchk/resynthesizer)


## Quickstart

This repository is for developers and users who are familiar with building, especially on Linux.  

If you are a GIMP user who would like to download an installer for just the Resynthesizer plugin, please see:

* Linux: http://registry.gimp.org/node/27986
* Windows: http://registry.gimp.org/node/27986

If you want to install a package of third-party plugins (that includes Resynthesizer):

* Linux: install the package "gimp-plugin-registry" (available for most distributions, including Debian and Ubuntu).  

If you use OSX and want to install GIMP and a set of third-party plugins (including Resynthesizer) see:

* http://gimp.lisanet.de/Website/Download.html

Building this repository for Windows and OSX is not easy, and not documented here.

## Acknowledgements

Paul Harrison had the original idea and implementation.  He did the hard work, took the leap of faith and experimented with a new algorithm.  His [website](http://www.logarithmic.net/pfh/) has many examples of using the Resynthesizer, links to an explanation of the algorithm, and other cool ideas.

I fixed a few bugs (due to changes in GIMP), rewrote the Scheme plugins into Python, wrote a few plugins and gathered a few from others, made it better deal with alpha (transparency), threaded it, and made it into a reentrant library in C.

Several people have helped with the build process.

## For users

### Acquiring

The plugins do _not_ come with GIMP.   You often can get them precompiled for your platform, with an installer.  You often can get them included in a larger package of plugins, such as the package gimp-plugin-registry for Linux, or GIMP Extensions Pack for Windows at SourceForge.

### What they do

Includes many plugins that do seemingly unrelated things, but all based on one powerful algorithm.  Provides these menu items in the GIMP Filter menu:

* Enhance>Heal selection...  Retouch.  Like the heal tool, but automatic.  Replaces the selection to look like its surroundings.  Probably the most popular and useful.
* Enhance>Heal transparency...  Like above, but you don't need to select anything, it heals every transparent pixel.
* Enhance>Uncrop...  Makes the canvas larger and heals the new transparent border.  Not exactly the inverse of cropping, but almost:  reconstructs what might have been cropped earlier.
* Enhance>Sharpen by synthesis... A variant of sharpening that _might_ give more plausible detail.
* Enhance>Enlarge & sharpen...  Enlarge (increase the resolution) and sharpen.  _Might_ restore more plausible detail than simple scaling up and using other sharpens.

* Map>Resynthesize...   A control panel.  The hardest to understand, but the most powerful.  Displays every control of the Resynthesizer engine.  Here, you can experiment with other uses of the algorithm.  The other plugins are simplified, special uses of this plugin.
* Map>Style...  transfer the 'style' of one image to another.

* Render>Texture..  Creates a new image having the texture from another image.  In other words, a seamless, irregular wallpaper (if the source image is smaller.)  (You can also use Map>Resynthesize to create tiles, and then you can create a regular but seamless pattern.)

* Edit>Fill with pattern seamless...  (In the Edit menu, not the Filter menu.)  Somewhat similar to Render>Texture.  Fills with an irregular pattern from another image.  From RobA.



### Installing

Instructions for most users who received a pre-built distribution (with resynthesizer.exe and resynthesizer-gui.exe already compiled) but without an installer.

1.  Remove any older versions (see below.)

2.  Copy all files to your home gimp directory for plugins (and make the Python files executable):

~/.gimp-2.6/plug-ins (Linux)  
c:\Doc...Settings\username\gimp-2.6\plug-ins (Windows)  


### Removing older versions


You should manually remove these older versions before installing this package.  They probably do NOT still work, and are duplicates at best.

Author Paul Harrison

* smart-remove.scm   Enhance>Smart remove object  
* smart-enlarge.scm  Enhance>Smart enlarge        
* smart-sharpen.scm  Enhance>Smart sharpen       
* resynthesizer.exe  Map>Resynthesize  

Author L. Konneker

* smart-remove.scm          Enhance>Heal selection (patch)
* plugin-map-style.scm      Map>Style
* plugin-render-texture.scm Render>Texture 
* uncrop-gimp-plugin.scm    Enhance>Uncrop

Author Rob Antonishen:

* resynth-pattern-fill.scm  Edit>Fill with resynthesized pattern

They might be found in the usual places:

Linux

~/.gimp-2.x/plug-ins (.exe and .py)  
~/.gimp-2.x/scripts  (.scm)  
/usr/share/gimp/2.0/scripts (.scm)  
/usr/local/lib/gimp/2.0/plugins (.exe and .py)  

Windows

c:\Program Files\GIMP-2.0\lib\gimp\2.0\plug-ins (.exe and .py)  
c:\Program Files\GIMP-2.0\share\gimp\2.0\plug-ins (.scm)  
c:\Doc...Settings\username\gimp-2.6\plug-ins  (.exe and .py)  
c:\Doc...Settings\username\gimp-2.6\scripts  (.scm)  




## Building

### Versions

Version 2.0 is a complete rewrite and is multithreaded.  Otherwise, functionally the same except when compiled threaded it is nondeterministic.  See ChangeLog for more discussion.

Version 1.0 derives from version 0.16 maintained by the original author.

See the NEWS file.

### Linux

Builds with GNU build tools.  Many of the build files (Makefile.am, configure.ac, etc.) were hacked from the gimp-plugin-template 2.2.0.

To build and install a clean distribution, just ...

```
./autogen.sh
./configure
make
make install
```

Installs to the shared Gimp directories.  Except on Ubuntu, it installs to /usr/local/lib/gimp/2.0/plug-ins and you may need to set the GIMP Edit>Preferences>Folders>Plugins> to include that path.

Note the default gcc flags seem to be: -g -O2 -Wall.
To optimize more, pass the CFLAGS (perogative of the builder to alter compilation):

make CFLAGS=-O3


### Other platforms

Other people build and package for Windows and OSX.

I once used MinGW and MSYS.
However it doesn't install plugins properly.
You will need to copy files to the proper Gimp plugin install directory,
typically from:  c:\MSYS\1.0\usr\local\lib\gimp\2.0\plug-ins to: c:\Doc...Settings\yourname\.gimp-2.6\plug-ins

### Build chain

When you run autogen.sh, it may complain of missing packages.

To build resynthesizer (or any GIMP plugin) you need these developer packages:

* automake
* libglib2.0-dev
* libgimp2.0-dev
* intltool


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



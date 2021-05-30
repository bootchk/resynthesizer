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

[Developer's guide](https://github.com/bootchk/resynthesizer/wiki/Developer's-guide-to-the-Resynthesizer-code-and-dependencies)

[Resynthesizer and GIMP version 3](https://github.com/bootchk/resynthesizer/wiki/Resynthesizer-and-GIMP-version-3)

More topics are in the [Resynthesizer wiki](https://github.com/bootchk/resynthesizer/wiki)

## Installation

[install flatpaked Resynthesizer for Linux](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#flatpak)

[install Resynthesizer for Windows](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#windows)

[install Resynthesizer for MacOS](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#mac-osx)

[build from source](https://github.com/bootchk/resynthesizer/wiki/Build-Resynthesizer-from-source)

## Acknowledgements

Paul Harrison had the original idea and implementation.  He did the hard work, took the leap of faith and experimented with a new algorithm.  His [website](http://www.logarithmic.net/pfh/) has many examples of using the Resynthesizer, links to an explanation of the algorithm, and other cool ideas.

I fixed a few bugs (due to changes in GIMP), rewrote the Scheme plugins into Python, wrote a few plugins, made it better deal with alpha (transparency), threaded it, and made it into a reentrant library in C.

Rob Antonishen contributed the basis for one plugin.  Several people have helped with the build process and with translations.

## License

GNU General Public License v3.0


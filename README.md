# GIMP Resynthesizer Plugin Suite

A set of plugins for the free GIMP image editing app.

  Copyright 2000 2008  Paul Francis Harrison  
  Copyright 2002  Laurent Despeyroux  
  Copyright 2002  David Rodríguez García  
  Copyright 2010-2025  Lloyd Konneker  
  Copyright 2022 itr-tert

[![Build Status](https://travis-ci.org/bootchk/resynthesizer.svg?branch=master)](https://travis-ci.org/bootchk/resynthesizer)

A library implementing the "resynthesizer" algorithm for texture transfer among images.

And a suite of plugins, that use the algorithm, for the GIMP image processing application.
The plugins do _not_ come with GIMP, they are "third-party" plugins.

## News

The initial release of Resynthesizer for GIMP 3.0 API is tagged v3.0 in the branch "resynthesizer3."

The GIMP API (libgimp) is stable between major releases.
So Resynthesizer v3.0 should be compatible with future GIMP releases.
You don't need to update Resynthesizer for every release of GIMP.
Future releases of Resynthesizer should be compatible with any version of GIMP after 3.0.
You don't need to update Resynthesizer unless you want bug fixes or new features.

Translations (internationalizations) are not ported yet
(waiting on a fix to GIMP 3 ScriptFu.)

An MS Windows build is not tested and not in the repo.
It might be built in this repo when libgimp-3.0 becomes available in an image for runners of build jobs.

[Read about the branches](https://github.com/bootchk/resynthesizer/wiki/The-branches-of-the-repository)

The development model is: the resynthesizer3 branch is the main development branch.
Fixes and enhancements will be made in that branch.
You can expect the latest commit will work.
Occasionally, I will tag a commit as a released version.
The latest commit in the branch will have less real world testing than a tagged version.

The branch "master" is for GIMP 2.0 API and is essentially frozen.

## Resources

[Quick user's guide to the Resynthesizer plugins for GIMP](https://github.com/bootchk/resynthesizer/wiki/Quick-user's-guide-to-the-Resynthesizer-plugins-for-GIMP)

[Developer's guide](https://github.com/bootchk/resynthesizer/wiki/Developer's-guide-to-the-Resynthesizer-code-and-dependencies)

More topics are in the [Resynthesizer wiki](https://github.com/bootchk/resynthesizer/wiki)

## Installation

[install flatpaked Resynthesizer for Linux](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#flatpak)

[install Resynthesizer for Windows](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#windows)

[install Resynthesizer for MacOS](https://github.com/bootchk/resynthesizer/wiki/Install-Resynthesizer#mac-osx)

[build from source](https://github.com/bootchk/resynthesizer/wiki/Build-Resynthesizer-from-source)

## Acknowledgements

Paul Harrison had the original idea and implementation.  He did the hard work, took the leap of faith and experimented with a new algorithm.  His [website](http://www.logarithmic.net/pfh/) has many examples of using the Resynthesizer, links to an explanation of the algorithm, and other cool ideas.

I fixed a few bugs (due to changes in GIMP), rewrote the Scheme plugins into Python, wrote a few plugins, made it better deal with alpha (transparency), threaded it, and made it into a reentrant library in C.

Rob Antonishen contributed the basis for one plugin.  

Many people have helped with the build process and with translations.

"itr-tert" ported the plugins to Scheme from Python 2.

## License

GNU General Public License v3.0


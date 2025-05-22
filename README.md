# Resynthesizer Plugins for GIMP 3


[![Build Status](https://travis-ci.org/bootchk/resynthesizer.svg?branch=master)](https://travis-ci.org/bootchk/resynthesizer)

This is the branch for development of Resynthesizer compatible with the GIMP application v3.0.0 and later.

See README.md in branch "master" for more general information about the Resynthesizer suite.

See NEWS and ChangeLog in this branch for discussion of what changed from Resynthesizer v2.

This branch will not build or install localization data (i18n) until about GIMP 3.2, 
since ScriptFu in GIMP needs a fix to support translation of Scheme scripts.
In other words, the language of the GUI is English, until a future release.
The translations for Resynthesizer exist for a few languages, but they won't work, and they need editing.

This is the development branch for Resynthesizer version 3, for GIMP version 3.
The "master" branch is for GIMP 2.
The master branch is essentially frozen and probably won't change even to fix bugs.
The GIMP 2.10 branch is also frozen.

The development model is: only one branch is the current release, this one for GIMP 3.0, or master for GIMP 2.10.
Any other branches in this repo are for development and experiments; they might not work.
This branch is intended to build and work.
This branch may lag releases of the GIMP app.

Resynthesizer v3 depends on the GIMP library (typical package name libgimp-3.0-dev.)
The API for the GIMP library is intended to be API stable.
No changes to the GIMP app should affect the Resynthesizer.
The next breaking change to the GIMP API should not come until the next major release of GIMP, v4.0.
But we can't rule out that the GIMP 3.0 API changes, requiring more changes to the Resynthesizer.
And as noted above, release 3.2 of GIMP may trigger a release 3.2 of Resynthesizer for the feature "internationalized."

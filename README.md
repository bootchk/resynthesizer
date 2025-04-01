# Resynthesizer Plugins for Gimp 3


[![Build Status](https://travis-ci.org/bootchk/resynthesizer.svg?branch=master)](https://travis-ci.org/bootchk/resynthesizer)

Work in progress. 

A Flatpak has been built, and others have built it.
Issues have been reported.
Only the most important plugins (in the suite) have been ported to GIMP 3 PDB API.
I am still resolving issues, porting plugins, and testing.
Once that is done, I will tag a version.

It will not be internationalized until about GIMP 3.2, since ScriptFu in GIMP needs a fix to support translation of Scheme scripts.
(The translations for Resynthesizer exist for a few languages, but they won't work, and they need editing.)

This is the development branch for Resynthesizer version 3, for Gimp version 3.
The branch might later be renamed main or master.
For now, the master branch is for Gimp 2.
The master branch is essentially frozen and probably won't be changed even to fix bugs, like the Gimp 2.10 branch.

The development model is: there is only one branch that ships, this one for GIMP 3.0, or master for GIMP 2.10.
Any other branches in this repo are for development and experiments; they might not work.
This branch is intended to build and substantially work.
But it lags the release of GIMP 3, and is not yet complete or officially versioned.

The Resynthesizer v3 depends on the GIMP library (typically package is named libgimp-3.0-dev.)
The API for the GIMP library is intended to be API stable.
No changes to the GIMP app should affect the Resynthesizer.
The next breaking change to the GIMP API should not come until the next major release of GIMP, v4.0.
But we can't rule out that the GIMP 2.0 API changes, requiring more changes to the Resynthesizer.

Also, since the CI of Github (and Gitlab) typically uses a pre-built image for the host of the build system,
and the existing images don't yet have GIMP 3 and libgimp-3.0-dev,
it will not be easy to have Github hosted CI for Resynthesizer (on Linux, Windows, or MacOS)
without the CI job also building GIMP to get libgimp, or without a cross-repo CI job.

If you want plugins for Gimp 2 that don't require Python 2, see itr-tert/gimp-resynthesizer-scm repo.



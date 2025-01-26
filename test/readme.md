## testing the outer plugins and engine plugin

testResynth.py tests the resynthesizer outer plugins.
It calls them with a known image
and compares the result to expected reference image.

in_images dir contains input images.
There are various combinations of image mode and transparency.

reference_out_images contains expected output images from tests.
You should not commit new reference images without much care
(don't commit an image that doesn't represent what the algorithm should do.)
There are more reference output images than input images.
Many-to-one: some tests use a common input image.
Reference out images are named by catting the testname with the input image name.

Changelog: in earlier versions (v2 using GimpFu)
the test harness was run in batch mode.
Now it is a plugin itself, and much changed.

## testing the library

testSynth.c is a C program that tests the engine library.
It submits little pixmaps to the engine, and examines the result.
See meson.build and the code.

## testing i18n localization

First install a language package, such as Ubuntu language-pack-fr-base
which is for the French family of language dialects.
French translation for resynthesizer is more complete, Spanish is poorly translated.

Set the environment variable LANG:
```
export LANG="fr_FR.UTF-8"
```

You don't need to build and install GIMP fresh.
GIMP's user specific .config caches PDB Procedures and their stale translations.
But expect GIMP to re query plugins if the language was changed.

Run GIMP.  
Expect it's GUI strings to be translated.

Expect the menu item is French phrase: Filtres>Ameliorization>Corregir la selection..
That is translated with the resynthesizer plugin.
If instead "Heal Selection...", then i18n is not working.

Hover on the menu item.
Expect the tooltip is in French.

Run resynthesizer plugin "Heal Selection."
Expect the rest of it's GUI strings to be in French language.



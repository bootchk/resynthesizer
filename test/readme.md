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


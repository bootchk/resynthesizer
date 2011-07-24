/*
Header for libimagesynth

With SimpleAPI: only one image passed in.
*/


#include "imageBuffer.h"
#include "imageFormat.h"

int
imageSynth(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA Pixels described by imageFormat
  ImageBuffer * mask,         // IN one mask Pixelel
  TImageFormat imageFormat,
  TImageSynthParameters* parameters
  );

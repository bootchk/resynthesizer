/*
Header for libimagesynth
*/


#include "imageBuffer.h"
#include "imageFormat.h"

int
imageSynth(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA four Pixelels
  ImageBuffer * mask,         // IN one mask Pixelel
  TImageFormat imageFormat
  // EngineControls temporarily not passed: see setDefaultParams
  );

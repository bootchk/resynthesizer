/*
Parameters of the engine.

The first three parameter are parameters of the Gimp plugin,
not the engine.
See resynth-parameters.h.
*/


#include "engineParams.h"

#ifndef FALSE
  #define FALSE 0
#endif

void
setDefaultParams(
  TImageSynthParameters *param)
{
  param->isMakeSeamlesslyTileableHorizontally = FALSE;
  param->isMakeSeamlesslyTileableVertically   = FALSE;
  param->matchContextType                     = 1;   // Match context, synthesize randomly
  param->mapWeight                            = 0.5;
  param->sensitivityToOutliers                = 0.117; // 30/256
  param->patchSize                            = 30;
  param->maxProbeCount                        = 200;
}


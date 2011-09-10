/*
Parameters of the engine.
*/

#include "engineParams.h"


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


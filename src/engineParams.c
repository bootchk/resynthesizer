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
  Parameters *param)
{
  param->corpus_id = -1;
  param->input_map_id = -1;
  param->output_map_id = -1;
  param->v_tile = FALSE;  // lkk was TRUE
  param->h_tile = FALSE;
  param->use_border = 1;    // lkk was true, now is an enum
  param->map_weight = 0.5;
  param->autism = 0.117; /* 30/256 */
  param->neighbours = 30;
  param->trys = 200;
}

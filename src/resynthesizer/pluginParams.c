
#include "pluginParams.h"

gboolean
get_engine_specific_parameters(
  const GimpValueArray   *args,              // IN
  TGimpAdapterParameters *pluginParameters)  // OUT
{
pluginParameters->h_tile     = GIMP_VALUES_GET_INT      (args, 0);
pluginParameters->v_tile     = GIMP_VALUES_GET_INT      (args, 1);
pluginParameters->use_border = GIMP_VALUES_GET_INT      (args, 2);
pluginParameters->corpus     = GIMP_VALUES_GET_DRAWABLE (args, 3);
pluginParameters->input_map  = GIMP_VALUES_GET_DRAWABLE (args, 4);
pluginParameters->output_map = GIMP_VALUES_GET_DRAWABLE (args, 5);
pluginParameters->map_weight = GIMP_VALUES_GET_FLOAT    (args, 6);
pluginParameters->autism     = GIMP_VALUES_GET_FLOAT    (args, 7);
pluginParameters->neighbours = GIMP_VALUES_GET_INT      (args, 8);
pluginParameters->trys       = GIMP_VALUES_GET_INT      (args, 9);
}

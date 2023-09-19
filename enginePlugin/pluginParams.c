
// Functions to convert plugin parameters (GIMP plugin API) to resynthesizer parameters

#include <libgimp/gimp.h>
#include "pluginParams.h"


// in v3 gimp API:
//    run_func has signature of type GimpRunFunc
//    params are a GimpValueArray
//    run_mode, image, drawables not passed separately
//    target drawable is in the GVA

gboolean
get_engine_specific_parameters(
  const GimpValueArray    *args,              // IN
  TGimpAdapterParameters  *pluginParameters)  // OUT
{
  //g_assert( args->n_values  == 10 );

  g_debug ("%s", G_STRFUNC);

  pluginParameters->target     = GIMP_VALUES_GET_DRAWABLE (args, 0);
  pluginParameters->h_tile     = GIMP_VALUES_GET_BOOLEAN  (args, 1);
  pluginParameters->v_tile     = GIMP_VALUES_GET_BOOLEAN  (args, 2);
  pluginParameters->use_border = GIMP_VALUES_GET_INT      (args, 3);
  pluginParameters->corpus     = GIMP_VALUES_GET_DRAWABLE (args, 4);
  pluginParameters->input_map  = GIMP_VALUES_GET_DRAWABLE (args, 5);
  pluginParameters->output_map = GIMP_VALUES_GET_DRAWABLE (args, 6);
  pluginParameters->map_weight = GIMP_VALUES_GET_DOUBLE   (args, 7);
  pluginParameters->autism     = GIMP_VALUES_GET_DOUBLE   (args, 8);
  pluginParameters->neighbours = GIMP_VALUES_GET_INT      (args, 9);
  pluginParameters->trys       = GIMP_VALUES_GET_INT      (args, 10);
  return TRUE;
}


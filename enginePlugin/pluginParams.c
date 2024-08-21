
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
  GimpProcedureConfig     *config,          // IN
  TGimpAdapterParameters  *pluginParameters)  // OUT
{
  g_debug ("%s", G_STRFUNC);

  // Param names see gimp_procedure_add_*_argument in plugin.c
  g_object_get(config,
    "drawable", &pluginParameters->target,
    "h_tile", &pluginParameters->h_tile,
    "v_tile", &pluginParameters->v_tile,
    "use_border", &pluginParameters->use_border,
    "corpus_drawable", &pluginParameters->corpus,
    "input_map", &pluginParameters->input_map,
    "output_map", &pluginParameters->output_map,
    "map_weight", &pluginParameters->map_weight,
    "autism", &pluginParameters->autism,
    "neighbours", &pluginParameters->neighbours,
    "trys", &pluginParameters->trys,
    NULL
  );

  return TRUE;
}


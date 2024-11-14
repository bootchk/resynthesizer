

// Functions to convert plugin parameters (GIMP plugin API) to resynthesizer parameters


#include <libgimp/gimp.h>

#include "pluginParams.h"



// hacky test that version is less than 2.99.xx
#if GIMP_MINOR_VERSION < 99


// in v2 gimp API, params are array of GimpParam

#include "../resynth-constants.h"
#include "../resynth-parameters.h" // get_new_parameters_from_list.  requires resynth-constants.h

#include "debug.h"


// Get the parameters other than run mode and in_drawable: the slice param[3:]
/*
  The engine should not be run interactively so no need to store last values.
  I.E. the meaning of "last" is "last values set by user interaction".
*/
gboolean
get_engine_specific_parameters(
  gint32                  run_mode,
  gint                    nparams,
	GimpParam        *param,
  GimpDrawable     *in_drawable,
  TGimpAdapterParameters *pluginParameters
  )
{
  gboolean result;

  switch(run_mode)
  {
    case GIMP_RUN_INTERACTIVE :
    case GIMP_RUN_WITH_LAST_VALS :
      gimp_message("Resynthesizer engine can not be called interactively");
      result = FALSE;
      break;
    case GIMP_RUN_NONINTERACTIVE :
      debug("get_new_parameters_from_list");
      result = get_new_parameters_from_list(pluginParameters, nparams, param);
      break;
    default:
      result = FALSE;
  }
  return result;
}

/*
CRUFT
 case GIMP_RUN_INTERACTIVE :
      result = get_last_parameters(pluginParameters, in_drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME);
      // TODO restore ID's to GimpDrawable*
      gimp_message("Resynthesizer engine should not be called interactively");
      // But keep going with last (or default) parameters, really no harm.
      break;
    case GIMP_RUN_NONINTERACTIVE :
      result = get_parameters_from_list(pluginParameters, nparams, param);
      break;
    case GIMP_RUN_WITH_LAST_VALS :
      result = get_last_parameters(pluginParameters,in_drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME);
      // TODO restore ID's to GimpDrawable*
      break;
    default:
      result = FALSE;
*/



#else



// in v3 gimp API, params are a GimpValueArray

gboolean
get_engine_specific_parameters(
  GimpProcedureConfig   *args,              // IN
  TGimpAdapterParameters *pluginParameters)  // OUT
{
  // Fails to compile:
  // g_assert( args->length()  == 10 );

  // args does not have prefix: run mode, image, drawable
  g_object_get(
    args, 
    "h-tile", &pluginParameters->h_tile,
    "v-tile", &pluginParameters->v_tile,
    "use-border", &pluginParameters->use_border,
    "corpus-drawable", &pluginParameters->corpus,
    "input-map", &pluginParameters->input_map,
    "output-map", &pluginParameters->output_map,
    "map-weight", &pluginParameters->map_weight,
    "autism", &pluginParameters->autism,
    "neighbours", &pluginParameters->neighbours,
    "trys", &pluginParameters->trys);
  
  return TRUE;
}


#endif

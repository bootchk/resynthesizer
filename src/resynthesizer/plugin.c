


#include "../../config.h" // GNU buildtools local configuration
#include "../plugin-intl.h" // i18n macros
#include <libgimp/gimp.h>



#include "../resynth-constants.h"

#include "imageFormat.h"
#include "imageFormatIndicies.h"
#include "map.h"
#include "engineParams.h"
#include "engine.h" // requires map.h
#include "../resynth-parameters.h" // requires engine.h

#include "resynthesizer.h"  // inner_run




// hacky test that version is less than 2.99.xx
#if GIMP_MAJOR_VERSION < 99



// Get the parameters other than run mode and in_drawable: the slice param[3:]
/* 
  The engine should not be run interactively so no need to store last values. 
  I.E. the meaning of "last" is "last values set by user interaction".
*/
static gboolean
get_engine_specific_parameters(
  gint32                  run_mode,
  gint                    nparams,
	const GimpParam        *param,
  const GimpDrawable     *in_drawable,
  TGimpAdapterParameters *pluginParameters
  )
{
  gboolean result;

  switch(run_mode) 
  {
    case GIMP_RUN_INTERACTIVE :
      result = get_last_parameters(pluginParameters, in_drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME);
      gimp_message("Resynthesizer engine should not be called interactively");
      /* But keep going with last (or default) parameters, really no harm. */
      break;
    case GIMP_RUN_NONINTERACTIVE :
      result = get_parameters_from_list(pluginParameters, nparams, param); 
      break;
    case GIMP_RUN_WITH_LAST_VALS :
      result = get_last_parameters(pluginParameters,in_drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME); 
      break;
    default:
      result = FALSE;
  }
  return result;
}




/*
Plugin run func.
This is what GIMP calls.
Adapts to a generic resynthesizer plugin.
Liable to change as GIMP plugin API changes.
*/

// API for Gimp-2.0

static void run(
  const gchar *     name,
  gint              nparams,
	const GimpParam * param,
	gint *            nreturn_vals,  // OUT
	GimpParam **      return_vals)   // OUT
{
  const char       *result;           // inner result
  static GimpParam values[2];   // Gimp return values. !!! Allow 2: status and error message.
  gint32           run_mode;
  // WIP ID or Drawable* since 2.10???
  GimpDrawable *in_drawable = NULL;
  // gint32        drawableID;
  TGimpAdapterParameters pluginParameters;

  run_mode = param[0].data.d_int32;

  // deprecated
  // Drawable* from ID
  in_drawable = gimp_drawable_get(param[2].data.d_drawable);
  // drawable = gimp_drawable_get_by_id(param[2].data.d_drawable);

  if ( ! get_engine_specific_parameters(run_mode, nparams, param, in_drawable, &pluginParameters) )
    result = _("Resynthesizer failed to get parameters.");
  else
    result = inner_run(
      name, // nparams, param, 
      run_mode, 
      in_drawable, 
      &pluginParameters);
  
  // Cram result into the error object
  // return_vals is a pointer to array.
  // Always pass pointer to array of size two, and tell how many elements are valid.
  values[0].type = GIMP_PDB_STATUS;
  *return_vals = values;
  if (strcmp(result, "success") == 0)
  {
    *nreturn_vals = 1;
    values[0].data.d_status = GIMP_PDB_SUCCESS;
  }
  else
  {
    *nreturn_vals           = 2;
    values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;
    values[1].type          = GIMP_PDB_STRING;
    values[1].data.d_string = result ;
    g_debug(result);
  }

  return;
}




  #include "resynthPDBv2.h"
#else


  #include "resynthPDBv3.h"
#endif



#include "../../config.h" // GNU buildtools local configuration
#include "../plugin-intl.h" // i18n macros
#include <libgimp/gimp.h>

#include "../resynth-constants.h"
#include "debug.h"




// hacky test that version is less than 2.99.xx
#if GIMP_MINOR_VERSION < 99

#include "pluginParams.h"

// v2 only
// defines get_new_parameters_from_list()
#include "../resynth-parameters.h" // requires engine.h

#include "resynthesizer.h"  // inner_run



/*
Plugin run func.
This is what GIMP calls.
Adapts to a generic resynthesizer plugin.
Liable to change as GIMP plugin API changes.
*/

// API for Gimp-2.0

static void run(
  gchar *     name,
  gint              nparams,
	GimpParam * param,
	gint *            nreturn_vals,  // OUT
	GimpParam **      return_vals)   // OUT
{
  char       *result;           // inner result
  static GimpParam values[2];   // Gimp return values. !!! Allow 2: status and error message.
  gint32           run_mode;
  // WIP ID or Drawable* since 2.10???
  GimpDrawable     *in_drawable = NULL;
  // gint32        drawableID;
  TGimpAdapterParameters pluginParameters;

  run_mode = param[0].data.d_int32;

  // deprecated
  // Drawable* from ID
  in_drawable = gimp_drawable_get(param[2].data.d_drawable);
  // drawable = gimp_drawable_get_by_id(param[2].data.d_drawable);

  debug("get_engine_specific_parameters");
  if ( ! get_engine_specific_parameters(run_mode, nparams, param, in_drawable, &pluginParameters) )
    result = _("Resynthesizer failed to get parameters.");
  else
  {
    result = inner_run(
      name, // nparams, param, 
      run_mode, 
      in_drawable, 
      &pluginParameters);
  }
  
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
    debug(result);
  }

  return;
}

// just the registration
#include "resynthPDBv2.h"


#else



  // v3, includes both run() and PDB registration
  #include "resynthPDBv3.h"



#endif


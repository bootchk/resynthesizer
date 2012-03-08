/*
  Settings (control panel) GUI for the resynthesizer.

  Resynthesizer GUI is a separate plugin from the engine.
  There are many other plugins that use the resynthesizer engine.
  Only this plugin exposes all the settings of the engine, for expert users.

  The engine and gui have the same parameters (settings).
  The gui, when called interactively, 
  lets the use change the settings before invoking engine.
  The gui, when called noninteractively, simply passes settings on to engine.
  
  TODO replace this with a Python language GUI.

  Copyright (C) 2010  Lloyd Konneker
  Copyright (C) 2000 2008  Paul Francis Harrison

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "config.h" // GNU buildtools local configuration
#include "plugin-intl.h"  // i18n macros

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

/* Shared with engine */
#include "imageFormat.h"
#include "map.h"
#include "engineParams.h"
#include "engine.h"

/* Shared with engine plugin. */
#include "resynth-constants.h"
#include "resynth-parameters.h"

/* Resynthesizer GUI gtk code: ask user for parameters. */
#include "resynth-gui.h"  


static void run(
  const gchar *     name,
  gint              nparams,  // !!! Always 3 for INTERACTIVE, not lengthof(param)?
	const GimpParam * param,
	gint *            nreturn_vals,
	GimpParam **      return_vals)
{
  static GimpParam values[1]; // return values persist after run() returns
  TGimpAdapterParameters parameters;
  gboolean ok;
  
  /* Unless anything goes wrong, result is success */
  *nreturn_vals = 1;
  *return_vals = values;
  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_SUCCESS;

    /*  Initialize i18n support  */
#if defined(G_OS_WIN32)
  bindtextdomain (GETTEXT_PACKAGE, gimp_locale_directory());
#else
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
#endif
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
  textdomain (GETTEXT_PACKAGE);
  
  /* Don't really need the drawable or its tiles until we get to the engine. */
  gint32 drawable_id;
  drawable_id = param[2].data.d_drawable;
  
  /* Assert the image type is OK because Gimp checked it. */
  
  /* Deal with run mode */
  ok = FALSE;  
  switch(param[0].data.d_int32) 
  {
    case GIMP_RUN_INTERACTIVE :
      get_last_parameters(&parameters, drawable_id, RESYNTH_CONTROLS_PDB_NAME );
      init_gtk();
      /* GUI dialog. */
      ok = get_parameters_by_asking(&parameters,drawable_id);
      if (ok) /* Not canceled and not an exception. */
        set_last_parameters(&parameters, drawable_id);
      break;
    case GIMP_RUN_NONINTERACTIVE :
      /* GUI CAN be called noninteractively, in batch mode or otherwise. */
      ok = get_parameters_from_list(&parameters, nparams, param); 
      break;
    case GIMP_RUN_WITH_LAST_VALS :
      ok = get_last_parameters(&parameters,drawable_id, RESYNTH_CONTROLS_PDB_NAME);
      break;
  }
  
  if (!ok)
  {
    /* No message displayed here because it is usually Cancel button, else the resynthesizer
    control panel GUI already displayed an error message?
    */
    values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR;
    return;
  }
  
  /* Prepare params for engine: augment passed params with user's choices. */
  static GimpParam temp[RESYNTH_PARAMETER_COUNT];
  set_parameters_to_list(&parameters, param, nparams, GIMP_RUN_NONINTERACTIVE, temp);
  
  /* Call resynthesizer engine. */
  GimpParam * engine_result;
  gint engine_n_return_vals;
  engine_result = gimp_run_procedure2(
    RESYNTH_ENGINE_PDB_NAME,
    &engine_n_return_vals,
    RESYNTH_PARAMETER_COUNT,  /* !!! Not nparams */
    temp
    );
    
  /* Always a status. */
  if (engine_result[0].data.d_status != GIMP_PDB_SUCCESS)
  {
    /* Reraise engine error. */
    *nreturn_vals = engine_n_return_vals;
    *return_vals = engine_result;
  }
  else 
  {
    /* Free engine result, return our own. */
    gimp_destroy_params(engine_result, engine_n_return_vals);
  }
}

/* Register plugin to Procedural DataBase */
static void 
query() 
{
  /* resynth_paramdefs defined in resynth-parameters.h */
  GimpParamDef *return_vals = NULL;
  gint nargs = sizeof(resynth_paramdefs)/sizeof(resynth_paramdefs[0]);
  gint nreturn_vals = 0;

  gimp_plugin_domain_register (RESYNTH_DOMAIN_NAME, LOCALEDIR);
  
  gimp_install_procedure(
    RESYNTH_CONTROLS_PDB_NAME,
    N_("Make tiles, apply themes, render texture, remove features, etc."),
		"The resynthesizer control panel.",
		"Paul Francis Harrison, Lloyd Konneker",
		"2000 Paul Francis Harrison, 2010 Lloyd Konneker",
		"2010",
		N_("_Resynthesize..."), 
		"RGB*, GRAY*",
		GIMP_PLUGIN,
		nargs, nreturn_vals,
		resynth_paramdefs, return_vals);
		
		gimp_plugin_menu_register(RESYNTH_CONTROLS_PDB_NAME, "<Image>/Filters/Map");
}



static GimpPlugInInfo PLUG_IN_INFO = {
  NULL, /* init_proc */
  NULL, /* quit_proc */
  query, /* query_proc */
  run, /* run_proc */
  };


/* Macro to define the usual plugin main function */
MAIN()


/*
Parameters for resynthesizer Gimp plugin.
Hides indexing of GimpParam via internal ParametersStruct with named fields.
Also stores last values as Gimp data under the name of the plugin.

This is shared by the gui control plugin and the engine plugin.

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


/*
Parameters passed from a Gimp plugin
to the adapter from Gimp to the innermost engine.

!!!  v2
*/
#if GIMP_MAJOR_VERSION == 2 && GIMP_MINOR_VERSION < 99


typedef struct GIMPAdapterParametersStructOld {
  
  int h_tile;
  int v_tile; 
  int use_border;   
  
  int corpus_id, input_map_id, output_map_id;

  double map_weight;
  double autism;
  
  int neighbours;
  int trys;
} TGimpAdapterParametersOld;




// Registered parameters of the v2 plugin
static GimpParamDef resynth_paramdefs[] = 
{
  { GIMP_PDB_INT32,     (gchar*)"run_mode",   (gchar*)"Interactive, non-interactive" },
  { GIMP_PDB_IMAGE,     (gchar*)"image",      (gchar*)"Input image" },
  { GIMP_PDB_DRAWABLE,  (gchar*)"drawable",   (gchar*)"Input drawable" },

  { GIMP_PDB_INT32,     (gchar*)"vtile",      (gchar*)"Make tilable vertically 0,1" },
  { GIMP_PDB_INT32,     (gchar*)"htile",      (gchar*)"Make tilable horizontally 0,1" },
  /*
  This parameter tells whether and how to make the edges match the context.
  It is moot if there is no selection, i.e. the entire image is synthesized.
  With version 1.0, it also has expanded meaning, with values beyond 0,1 meaning
  which direction to proceed with synthesis.  E.G. 2 means inward, giving more coherence
  with context.
  */
  { GIMP_PDB_INT32,     (gchar*)"use_context",   (gchar*)"Make edges match context 0,1,2,..." },
  /* The next three should more specifically be GIMP_PDB_DRAWABLE.
  But for backward compatibility, maintain as GIMP_PDB_INT32 for the drawable ID.
  From python drawable object, pass drawable.ID or -1 (not None)
  */
  { GIMP_PDB_INT32,     (gchar*)"corpus",     (gchar*)"Layer to use as corpus" },	
  { GIMP_PDB_INT32,     (gchar*)"inmask",     (gchar*)"Layer to use as input mask, -1 for none" }, 
  { GIMP_PDB_INT32,     (gchar*)"outmask",    (gchar*)"Layer to use as output mask, -1 for none" },
  { GIMP_PDB_FLOAT,     (gchar*)"map_weight", (gchar*)"Weight to give to map, if map is used" },
  { GIMP_PDB_FLOAT,     (gchar*)"autism",     (gchar*)"Sensitivity to outliers" },
  { GIMP_PDB_INT32,     (gchar*)"neighbourhood", (gchar*)"Neighbourhood size" },
  { GIMP_PDB_INT32,     (gchar*)"trys",       (gchar*)"Search thoroughness" }
};
  


/*
Set the default parameters for the GIMP plugin that adapts the engine.
The parameters to the plugin are distinct from parameters to the engine.
*/
static void
setDefaultPluginParams(
  TGimpAdapterParametersOld* param
  )
{
  /* 
  Following are unique to the plugin.
  These are 3 of the 4 images that can be passed to the engine.
  The 4th image is always passed to every plugin and is not listed here.
  */
  param->corpus_id = -1;
  param->input_map_id = -1;
  param->output_map_id = -1;
  // Following are similar to parameters to engine.
  param->v_tile = FALSE;  // lkk was TRUE
  param->h_tile = FALSE;
  param->use_border = 1;    // lkk was true, now is an enum
  param->map_weight = 0.5;
  param->autism = 0.117; // 30/256 
  param->neighbours = 30;
  param->trys = 200;
}


/* Restore the last parameters used, get from GIMP. */
static gboolean 
get_last_parameters(
  TGimpAdapterParametersOld* param, 
  gint default_drawable,
  gchar * plugin_name
  ) 
{
  /* Defaults in case this is our first run */
  setDefaultPluginParams(param);
 
  if ( ! gimp_get_data(plugin_name, param))
    /* No data was stored previously, not an exception. */
    gimp_message("No last settings, using defaults.");

  /* If image was resynthesized from itself last time, resythesize this image from itself too */
  if (param->corpus_id == -1)
    param->corpus_id = default_drawable;

  return TRUE;  /* TODO should be void. */
}


/* 
Remember the last parameters the user entered. 
Only called for runmode INTERACTIVE, by the control plugin,
not by the engine plugin.
*/
static void
set_last_parameters(
  TGimpAdapterParametersOld* params, 
  gint drawable_id
  )
{
  /* 
  The GUI defaults the corpus drawable to the same as the target drawable.
  We really want to store the fact that the user did not choose a different corpus drawable.
  Use -1 to signify that, then we can restore that condition, see get_last_parameters().
  
  TODO
  We can't store a drawable_id meaningfully since they are not persistent ID's.
  What if the drawable has been deleted in this session?
  Also, test what happens with map drawable IDs.
  */
  if (params->corpus_id == drawable_id)
    params->corpus_id = -1;
  gimp_set_data(RESYNTH_CONTROLS_PDB_NAME, params, sizeof(TGimpAdapterParametersOld));
  if (params->corpus_id == -1)
    params->corpus_id = drawable_id;
}


/* Convert argument list into internal parameters */
static gboolean 
get_old_parameters_from_list(
  TGimpAdapterParametersOld *param, 
  gint n_args, 
  const GimpParam *args
  ) 
{
  if (n_args != RESYNTH_PARAMETER_COUNT)
  {
    gimp_message("Wrong parameter count.");
    return FALSE;
  }

  param->v_tile = args[3].data.d_int32;
  param->h_tile = args[4].data.d_int32;
  param->use_border = args[5].data.d_int32;
  param->corpus_id = args[6].data.d_int32;      /* a d_drawable? */
  param->input_map_id = args[7].data.d_int32;
  param->output_map_id = args[8].data.d_int32;
  param->map_weight = args[9].data.d_float;
  param->autism = args[10].data.d_float;
  param->neighbours = args[11].data.d_int32;
  param->trys = args[12].data.d_int32;

  return TRUE;
}

/* Convert argument list into internal parameters */
static gboolean 
get_new_parameters_from_list(
  TGimpAdapterParameters *param, 
  gint n_args, 
  const GimpParam *args
  ) 
{
  if (n_args != RESYNTH_PARAMETER_COUNT)
  {
    gimp_message("Wrong parameter count.");
    return FALSE;
  }

  param->v_tile = args[3].data.d_int32;
  param->h_tile = args[4].data.d_int32;
  param->use_border = args[5].data.d_int32;

  // Convert ID's to pointers using deprecated function
  // v2 plugin API to our internal params compatible with v3 plugin API
  param->corpus = gimp_drawable_get(args[6].data.d_int32);      /* a d_drawable? */
  {
    gint id;
    id = args[7].data.d_int32;
    if (id != -1)
      param->input_map = gimp_drawable_get(id);
    else
      param->input_map = NULL;
    id = args[8].data.d_int32;
    if (id != -1)
      param->output_map = gimp_drawable_get(id);
    else
      param->output_map = NULL;
    // assert input_map and outputmap are valid pointers or NULL
  }
  param->map_weight = args[9].data.d_float;
  param->autism = args[10].data.d_float;
  param->neighbours = args[11].data.d_int32;
  param->trys = args[12].data.d_int32;

  return TRUE;
}


/* 
Copy passed parameters and user entered parameters to new Gimp params struct,
to pass to the engine. 
We can't use the const GimpParam passed in: a compiler warning about writing a const,
and its too small (nargs ==3 for INTERACTIVE.)
*/
static void
set_parameters_to_list(
  const TGimpAdapterParametersOld *param, // IN  parameter values user chose
  const GimpParam *args,  // IN   args: parameters passed to control plugin
  const gint nargs,       // IN
  gint runmode,           // IN
  GimpParam temp[]        // OUT
  ) 
{
  gint i;
  
  /* !!! Note c won't let you check lengh of temp, it must be RESYNTH_PARAMETER_COUNT. */
  g_assert(nargs>=3 && nargs<=RESYNTH_PARAMETER_COUNT);
  
  /* Copy from read only to writeable. */
  for (i=0; i<nargs; i++)
  {
    temp[i] = args[i];  /* Copy type and data. */
    // g_printf("type %d value %d\n", args[i].type, args[i].data);
  }
 
  /* Set runmode specially. */ 
  temp[0].data.d_int32 = runmode;
  
  /* leave 1,2 the same: image, drawable. */
  
  /* Set the data. */
  temp[3].data.d_int32 = param->v_tile;
  temp[4].data.d_int32 = param->h_tile;
  temp[5].data.d_int32 = param->use_border;
  // ID, not pointer
  temp[6].data.d_int32 = param->corpus_id;
  temp[7].data.d_int32 = param->input_map_id;
  temp[8].data.d_int32 = param->output_map_id;
  temp[9].data.d_float = param->map_weight;
  temp[10].data.d_float = param->autism;
  temp[11].data.d_int32 = param->neighbours;
  temp[12].data.d_int32 = param->trys;
  
  /* Set the type of user enterable params to the PDB declared type. */
  for (i=3; i<RESYNTH_PARAMETER_COUNT; i++)
  {
    temp[i].type = resynth_paramdefs[i].type;
  }
}

#endif



/*
Resynthesizer engine plug-in

Derived from 2.99 gimp/extensions/goat-exercises
and gimp/plug-ins/common/compose.c
*/

/*
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include <libgimp/gimp.h>

// TODO should this be: #include <libgimp/stdplugins-intl.h>
#include "plugin-intl.h"

#include "pluginParams.h"
#include "resynthesizer.h"  // inner_run


#define PLUG_IN_BINARY "resynthesizer"
#define PLUG_IN_SOURCE PLUG_IN_BINARY ".c"
#define PLUG_IN_PROC   "plug-in-resynthesizer"
#define PLUG_IN_ROLE   "resynthesizer"




typedef struct _Resynthesizer      Resynthesizer;
typedef struct _ResynthesizerClass ResynthesizerClass;

struct _Resynthesizer
{
  GimpPlugIn      parent_instance;
};

struct _ResynthesizerClass
{
  GimpPlugInClass parent_class;
};



// Declare local functions.


#define RESYNTHESIZER_TYPE  (resynthesizer_get_type ())
#define RESYNTHESIZER (obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), RESYNTHESIZER_TYPE, Resynthesizer))

GType                   resynthesizer_get_type         (void) G_GNUC_CONST;

static GList          * resynthesizer_query_procedures (GimpPlugIn           *plug_in);
static GimpProcedure  * resynthesizer_create_procedure (GimpPlugIn           *plug_in,
                                                        const gchar          *name);

static GimpValueArray * resynthesizer_run     (GimpProcedure        *procedure,
#ifdef IMAGE_PROCEDURE
                                               GimpRunMode           run_mode,
                                               GimpImage            *image,
                                               gint                  n_drawables,
                                               GimpDrawable        **drawables,
#endif
                                               const GimpValueArray *args,
                                               gpointer              run_data);


G_DEFINE_TYPE (Resynthesizer, resynthesizer, GIMP_TYPE_PLUG_IN)

GIMP_MAIN (RESYNTHESIZER_TYPE)



static void
resynthesizer_class_init (ResynthesizerClass *klass)
{
  GimpPlugInClass *plug_in_class = GIMP_PLUG_IN_CLASS (klass);

  plug_in_class->query_procedures = resynthesizer_query_procedures;
  plug_in_class->create_procedure = resynthesizer_create_procedure;
}

static void
resynthesizer_init (Resynthesizer *resynthesizer)
{
  // resynthesizer does nothing at init time
}




static GList *
resynthesizer_query_procedures (GimpPlugIn *plug_in)
{
  return g_list_append (NULL, g_strdup (PLUG_IN_PROC));
}



/*
Alternatives for declared class of PDB procedure:

superclass GimpProcedure.
You must declare all the args.
You can declare taking a single drawable.
The run_func signature has only other args, is type GimpRunFunc.

subclass GimpImageProcedure.
Hides some "usual" args.
You only declare the other args.
Usual args has a container of drawables.
The run_func signature has run_mode, image, count, drawables array, and array of other args.
The type of run_func is GimpRunImageFunc ?
*/

/*
plugin is an engine, without GUI.
No need for image_types, menu_label, icon_name, menu_path, sensitivity
*/
static GimpProcedure *
resynthesizer_create_procedure (GimpPlugIn  *plug_in,
                                const gchar *name)
{
  GimpProcedure *procedure = NULL;

  if (! strcmp (name, PLUG_IN_PROC))
    {
      // = gimp_image_procedure_new (plug_in, name,
      procedure = gimp_procedure_new (plug_in, name,
                                            GIMP_PDB_PROC_TYPE_PLUGIN,
                                            resynthesizer_run, NULL, NULL);

      gimp_procedure_set_documentation (procedure,
                                        N_("Resynthesizer engine"),
                                        "Image search/replace by patch match",
                                        PLUG_IN_PROC);
      gimp_procedure_set_attribution (procedure,
                                      "Lloyd Konneker",
                                      "Lloyd Konneker",
                                      "2021");

      // Common args
      #ifdef OLD
      GIMP_PROC_ARG_ENUM (
        procedure, "run-mode",
        "Run mode",
        "The run mode",
        GIMP_TYPE_RUN_MODE,
        GIMP_RUN_NONINTERACTIVE,
        G_PARAM_READWRITE);
      GIMP_PROC_ARG_IMAGE (
        procedure,
        "image",
        "Image to heal",
        "Image",
        TRUE,
        G_PARAM_READWRITE);
      #endif

      GIMP_PROC_ARG_DRAWABLE (
        procedure,
        "drawable",
        "Layer or channel to synthesize",
        "Target of algorithm",
        TRUE,
        G_PARAM_READWRITE);

      GIMP_PROC_ARG_BOOLEAN (procedure, "h_tile",
                         "Create image tileable horizontally?",
                         "Boolean",
                         FALSE,
                         G_PARAM_READWRITE);
      GIMP_PROC_ARG_BOOLEAN (procedure, "v_tile",
                         "Create image tileable vertically?",
                         "Boolean",
                         FALSE,
                         G_PARAM_READWRITE);
      GIMP_PROC_ARG_INT (procedure, "use_border",
                         "Enumerated order/directions of synthesis",
                         "See documents.",
                         0, 100, 1, // TODO what is the real range
                         G_PARAM_READWRITE);
      GIMP_PROC_ARG_DRAWABLE (procedure, "corpus_drawable",
                              "Image to search",
                              "Usually the surroundings of target.",
                              TRUE,
                              G_PARAM_READWRITE);
      GIMP_PROC_ARG_DRAWABLE (procedure, "input_map",
                              "Map of weightings for target.",
                              "Same size as target.",
                              TRUE,
                              G_PARAM_READWRITE);
      GIMP_PROC_ARG_DRAWABLE (procedure, "output_map",
                              "Map of weightings for corpus.",
                              "Same size as corpus.",
                              TRUE,
                              G_PARAM_READWRITE);
      GIMP_PROC_ARG_DOUBLE (procedure, "map_weight",
                            "Weighting for any in and out maps",
                            "How much to use maps while matching.",
                            0.0, 1.0, 0.5,
                            G_PARAM_READWRITE);
      GIMP_PROC_ARG_DOUBLE (procedure, "autism",
                            "Sensitivity to outliers of distance measure",
                            "Parameter of distance measure",
                            0.0, 1.0, 0.117,
                            G_PARAM_READWRITE);
      GIMP_PROC_ARG_INT (procedure, "neighbours",
                         "Count of pixels in a patch",
                         "More is high quality but slow",
                         1, 100, 9,
                         G_PARAM_READWRITE);
      GIMP_PROC_ARG_INT (procedure, "trys",
                         "Max search probes per pass",
                         "More is high quality but slow",
                         1, 10000, 200,
                         G_PARAM_READWRITE);
    }

  // No need to set sensitivity: an engine without a presence in the Gimp app's GUI
  // gimp_procedure_set_sensitivity_mask (procedure, GIMP_PROCEDURE_SENSITIVE_DRAWABLE);

  return procedure;
}


static GError *
new_gerror_for_resynthesizer_and_string(const char * msg)
{
  GQuark domain = g_quark_from_string("Resynthesizer");
  return g_error_new_literal(domain, 0, msg);
}


/*
Plugin run func.
This is what GIMP calls.

Signature is type GimpRunFunc

Adapts to a generic resynthesizer plugin.
Liable to change as GIMP plugin API changes.
*/

static GimpValueArray *
resynthesizer_run (
  GimpProcedure        *procedure,
  const GimpValueArray *args,
  gpointer              run_data)
{
  const char            *result;           // result of call to inner
  TGimpAdapterParameters pluginParameters;
  GimpDrawable          *drawable;
  const gchar           *name = gimp_procedure_get_name (procedure);

  // INIT_I18N();

  // Ignore run_mode: has no dialog.

  // Ignore settings (ProcedureConfig): not callable except by other plugins
  // with newly devised args.

  // Adapt args in GimpValueArray to a more generic struct.
  if ( ! get_engine_specific_parameters(args, &pluginParameters) )
    result = _("Resynthesizer failed to get parameters.");
  else
    result = inner_run(
      name,
      &pluginParameters);

  // inner_run returns string either "success" or an error message
  if (strcmp(result, "success") == 0)
  {
    return gimp_procedure_new_return_values (procedure, GIMP_PDB_SUCCESS, NULL);
  }
  else
  {
    GError * gerror;

    g_debug("%s : %s", G_STRFUNC, result);

    // GError having result as the message
    gerror = new_gerror_for_resynthesizer_and_string(result);
    return gimp_procedure_new_return_values (procedure, GIMP_PDB_EXECUTION_ERROR, gerror);
  }
}

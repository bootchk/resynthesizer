/*
 * Copyright (C) 2022 itr-tert
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <string.h>
#include <libgimp/gimp.h>
#include "config.h"         // GNU buildtools local configuration
#include "../plugin-intl.h" // i18n macros
#include "../resynth-path.h"

static void
query (void)
{
  static GimpParamDef args_def[] = {
       {GIMP_PDB_STRING, "msgid", "message to translate"}
  };

  static GimpParamDef returns_def[] = {
       // GIMP_PDB_STATUS is declared implicitly.
       {GIMP_PDB_STRING, "msgstr", "translated message or untranslated msgid as is"}
  };

  gimp_install_procedure (
       "plug-in-resynthesizer-gettext",
       "gettext for script-fu scripts of resynthersizer",
       "", // long description
       "itr-tert",
       "copyright 2022 itr-tert",
       "2022",
       "menu label",
       "*",
       GIMP_PLUGIN,
       G_N_ELEMENTS (args_def), G_N_ELEMENTS (returns_def),
       args_def,                returns_def);
}


static void
run (const gchar      *name,
     gint              nparams,
     const GimpParam  *param,
     gint             *nreturn_vals,
     GimpParam       **return_vals)
{
     /*  Initialize i18n support  */
     bindtextdomain (GETTEXT_PACKAGE, get_resynthesizer_locale_dir());
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
     bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
     textdomain (GETTEXT_PACKAGE);

     static GimpParam  values[2];
     *nreturn_vals = 2;
     *return_vals  = values;

     values[1].type = GIMP_PDB_STRING;
     values[1].data.d_string = gettext(param[0].data.d_string);

     values[0].type = GIMP_PDB_STATUS;
     values[0].data.d_status = GIMP_PDB_SUCCESS;
}


GimpPlugInInfo PLUG_IN_INFO = {
     NULL,
     NULL,
     query,
     run
};


MAIN()

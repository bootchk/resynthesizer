/*
Registration and MAIN() for the resynthesizer.

Copyright (C) 2000 2008  Paul Francis Harrison
Copyright (C) 2010 Lloyd Konneker

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

/* Register plugin to Procedural DataBase */
static void 
query() 
{
  GimpParamDef *return_vals = NULL;
  gint nargs = sizeof(resynth_paramdefs)/sizeof(resynth_paramdefs[0]);
  gint nreturn_vals = 0;

  // No need to gimp_plugin_domain_register (PLUGIN_NAME, LOCALEDIR) since no menu item
  
  gimp_install_procedure(
    RESYNTH_ENGINE_PDB_NAME,
    "Engine for resynthesizer kind of texture synthesis.",
	  "The resynthesizer engine.",
	  "Paul Francis Harrison, Lloyd Konneker",
	  "2000 Paul Francis Harrison, 2010 Lloyd Konneker",
	  "2010",
    "",    /* no menu item */
	  "RGB*, GRAY*",
	  GIMP_PLUGIN,
	  nargs, nreturn_vals,
	  resynth_paramdefs, return_vals);
}


static GimpPlugInInfo PLUG_IN_INFO = {
  NULL, /* init_proc */
  NULL, /* quit_proc */
  query, /* query_proc */
  run, /* run_proc */
  };


/* Macro to define the usual plugin main function */
MAIN()


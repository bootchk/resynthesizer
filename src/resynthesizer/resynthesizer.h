


const char *
inner_run(
  const gchar *              name,
  gint32                     run_mode,
  const GimpDrawable        *in_drawable,
  TGimpAdapterParameters    *pluginParameters
	);

void 
post_results_to_gimp(
  GimpDrawable *drawable,
  Map targetMap);

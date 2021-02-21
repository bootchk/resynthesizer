



/*
Parameters passed from a Gimp plugin
to the adapter from Gimp to the innermost engine.

!!!  v3
Difference from v2 is int ID => GimpDrawable*
*/
typedef struct GIMPAdapterParametersStruct {
  int                 h_tile;
  int                 v_tile;
  int                 use_border;

  const GimpDrawable *corpus;       // <<<< v2 was int ID
  const GimpDrawable *input_map;
  const GimpDrawable *output_map;

  double              map_weight;
  double              autism;

  int                 neighbours;
  int                 trys;
} TGimpAdapterParameters;





#if GIMP_MINOR_VERSION < 99



gboolean
get_engine_specific_parameters(
  gint32                  run_mode,
  gint                    nparams,
	const GimpParam        *param,          // <<<<<<
  const GimpDrawable     *in_drawable,
  TGimpAdapterParameters *pluginParameters
  );



#else



gboolean
get_engine_specific_parameters(
  const GimpValueArray   *args,              // IN   // <<<<<
  TGimpAdapterParameters *pluginParameters); // OUT


#endif

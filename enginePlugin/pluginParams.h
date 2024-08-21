
/*
Parameters passed from a Gimp plugin
to the adapter from Gimp to the innermost engine.

!!!  v3
Difference from v2 is int ID => GimpDrawable*
And target drawable now is in the GimpValueArray instead of separate.
*/
typedef struct GIMPAdapterParametersStruct {
  int                 h_tile;
  int                 v_tile;
  int                 use_border;

  GimpDrawable *target;
  GimpDrawable *corpus;       // <<<< v2 was int ID
  GimpDrawable *input_map;
  GimpDrawable *output_map;

  double              map_weight;
  double              autism;

  int                 neighbours;
  int                 trys;
} TGimpAdapterParameters;



gboolean
get_engine_specific_parameters(
  GimpProcedureConfig    *config,            // IN
  TGimpAdapterParameters *pluginParameters); // OUT





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
  
  const GimpDrawable *corpus;
  const GimpDrawable *input_map;
  const GimpDrawable *output_map;

  double              map_weight;
  double              autism;
  
  int                 neighbours;
  int                 trys;
} TGimpAdapterParameters;


gboolean
get_engine_specific_parameters(
  const GimpValueArray   *args,              // IN
  TGimpAdapterParameters *pluginParameters); // OUT

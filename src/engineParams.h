

typedef struct ParametersStruct {
  /* c++ bool */
  int h_tile;
  int v_tile; 
  int use_border;   
  
  /* GIMP.  gint32 */
  int corpus_id, input_map_id, output_map_id;

  double map_weight;
  double autism;
  
  int neighbours;
  int trys;
} Parameters;


extern void
setDefaultParams(
  Parameters *param);

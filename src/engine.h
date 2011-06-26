
extern Map image;
extern Map corpus;
extern Map image_mask;
extern Map corpus_mask;

extern gboolean is_alpha_image; // WAS = FALSE;
extern gboolean is_alpha_corpus;  // WAS = FALSE;



typedef struct ParametersStruct {
  gint h_tile, v_tile; /* c++ was bool */
  gint use_border;   /* c++ was bool */
  
  gint32 corpus_id, input_map_id, output_map_id;

  gdouble map_weight;
  gdouble autism;
  gint32 neighbours, trys;
} Parameters;

// typedef struct ParametersStruct Parameters;

/* 
bpp i.e. count of bytes (channels) per pixel or index thereof . 
See data layout in resynth_types.h
*/
typedef guint BppType;



extern BppType color_end_bip; 
extern BppType alpha_bip; 
extern BppType map_start_bip;
extern BppType map_end_bip;

extern BppType img_match_bpp;
extern BppType map_match_bpp;
extern BppType total_bpp;

extern int
engine(
  Parameters
  //, GimpDrawable *  // ANIMATE, but no Gimp types here
  );

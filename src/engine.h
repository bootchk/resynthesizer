
// extern Map image;
extern Map corpus;
extern Map image_mask;
extern Map corpus_mask;

extern gboolean is_alpha_image; // WAS = FALSE;
extern gboolean is_alpha_corpus;  // WAS = FALSE;



extern int
engine(
  Parameters parameters,
  TFormatIndices* indices,
  Map* targetMap
  //, GimpDrawable *  // ANIMATE, but no Gimp types here
  );

/*
Image format (RGB, RGBA, Grey, etc.)

Currently setting global flag variables.
*/

void
prepareImageFormatIndices(
  GimpDrawable *targetDrawable,
  GimpDrawable *sourceDrawable,
  gboolean isMap,
  GimpDrawable *mapInDrawable
  )
{
  /*
  Dynamic counts and indexes of pixelels.  Depends on the in drawables.
  !!! These are the same for target and corpus, even if the in drawables differ in alphas.
  !!! See resynth_types.h.
  bpp means bytes per pixel
  bip means byte index in pixel
  !!! Note the end is not the index of the last, but the index after the last
  
  [0]                         mask pixelel
  [1,color_end_bip)           image color pixelels, up to 3 (RGB)
  optional alpha byte         
  [map_start_bip, total_bpp)  map color pixelels
  optional map alpha byte     !!! discard
  [0, total_bpp)              entire pixel
  
  [1, color_end_bip)  color pixelels compared
  [map_start_bip, map_end_bip)      map pixelels compared
  
  Examples:
  RGBA with RGB maps                RGB with GRAY maps                RGB with no maps
  0 Mask (selection)                M                                 M
  1 R FIRST_PIXELEL_INDEX           R FIRST_PIXELEL_INDEX             R
  2 G                               G                                 G
  3 B                               B                                 B
  4 A alpha_bip, color_end_bip      W color_end_bip, map_start_bip   4  color_end, map_start, map_end, total
  5 R map_start_bip                5  map_end_bip, total_bpp
  6 G
  7 B
  8   map_end_bip, total_bpp
  
  !!! alpha_bip is undefined unless is_alpha_corpus or is_alpha_image
  
  TODO Possibly pad pixel to size 8 for memory alignment, especially if vectorized.
  */
  
  
  /* 
  Set flags for presence of alpha channels. 
  This is an optimization: we could access the image ever time we needed to know whether it had an alpha.
  Also, we could instead of using a flag, standardize the internal pixmap to ALWAYS have an alpha pixelel
  and initialize it to VISIBLE.
  See  */
  is_alpha_image = gimp_drawable_has_alpha(targetDrawable->drawable_id);
  is_alpha_corpus = gimp_drawable_has_alpha(sourceDrawable->drawable_id); // WAS parameters.corpus_id);
  
  
  /* !!! Not drawable->bpp because it includes other channels. */
  /* Don't compare alpha */
  img_match_bpp = count_color_channels(targetDrawable);
    
  /* Index of first color pixelel: 1, follows mask, use constant FIRST_PIXELEL_INDEX */
  color_end_bip   = FIRST_PIXELEL_INDEX + img_match_bpp;
  
  if ( is_alpha_image || is_alpha_corpus )
  {
    /* Allocate a pixelel for alpha. */
    alpha_bip = color_end_bip;
    map_start_bip = 1 + color_end_bip;
  }
  else
    /* alpha_bip is undefined. */
    map_start_bip = color_end_bip;
   
  /* Count pixelels to compare in maps. */
  if ( isMap )
  {
    /* 
    Either, none, or both maps can have alpha, but it is discarded. 
    Both maps must have same count color pixelels, checked earlier. 
    */
    map_match_bpp = count_color_channels(mapInDrawable);
  }
  else
    map_match_bpp =0;
   
  map_end_bip   = map_start_bip + map_match_bpp;
  total_bpp  = map_end_bip;  
  g_assert( total_bpp <= MAX_RESYNTH_BPP);
}

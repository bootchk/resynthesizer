/*
Internal image format.
*/
/*
#ifdef SYNTH_USE_GLIB
  // Use glib via gimp.h
  #include <libgimp/gimp.h>
#else
  #include "glibProxy.h"
#endif

#include "imageFormat.h"
*/

void
prepareImageFormatIndices(
  TFormatIndices* indices,  // OUT
  guint count_color_channels_target,  // Must be same count in source
  guint count_color_channels_map,
  gboolean is_alpha_target,
  gboolean is_alpha_source,
  gboolean isMap
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
  [1,colorEndBip)           image color pixelels, up to 3 (RGB)
  optional alpha byte         
  [map_start_bip, total_bpp)  map color pixelels
  optional map alpha byte     !!! discard
  [0, total_bpp)              entire pixel
  
  [1, colorEndBip)  color pixelels compared
  [map_start_bip, map_end_bip)      map pixelels compared
  
  Examples:
  RGBA with RGB maps                RGB with GRAY maps                RGB with no maps
  0 Mask (selection)                M                                 M
  1 R FIRST_PIXELEL_INDEX           R FIRST_PIXELEL_INDEX             R
  2 G                               G                                 G
  3 B                               B                                 B
  4 A alpha_bip, colorEndBip      W colorEndBip, map_start_bip   4  color_end, map_start, map_end, total
  5 R map_start_bip                5  map_end_bip, total_bpp
  6 G
  7 B
  8   map_end_bip, total_bpp
  
  !!! alpha_bip is undefined unless is_alpha_corpus or is_alpha_target
  
  TODO Possibly pad pixel to size 8 for memory alignment, especially if vectorized.
  */

  /* !!! Not drawable->bpp because it includes other channels. */
  /* Don't compare alpha */
  //img_match_bpp = count_color_channels_target;
  indices->img_match_bpp = count_color_channels_target;
    
  /* Index of first color pixelel: 1, follows mask, use constant FIRST_PIXELEL_INDEX */
  guint colorEndBip = FIRST_PIXELEL_INDEX + indices->img_match_bpp;
  indices->colorEndBip = colorEndBip;
  
  if ( is_alpha_target || is_alpha_source )
  {
    /* Allocate a pixelel for alpha. */
    //alpha_bip = colorEndBip;
    //map_start_bip = 1 + colorEndBip;
    indices->alpha_bip = colorEndBip;
    indices->map_start_bip = 1 + colorEndBip;
  }
  else
    /* alpha_bip is undefined. */
    //map_start_bip = colorEndBip;
    indices->map_start_bip = colorEndBip;
   
  /* Count pixelels to compare in maps. */
  if ( isMap )
  {
    /* 
    Either, none, or both maps can have alpha, but it is discarded. 
    Both maps must have same count color pixelels, checked earlier. 
    */
    //map_match_bpp = count_color_channels_map;
    indices->map_match_bpp = count_color_channels_map;
  }
  else
    //map_match_bpp =0;
    indices->map_match_bpp =0;
   
  //map_end_bip   = map_start_bip + map_match_bpp;
  //total_bpp  = map_end_bip;
  indices->map_end_bip   = indices->map_start_bip + indices->map_match_bpp;
  indices->total_bpp  = indices->map_end_bip;
  
  indices->isAlphaTarget = is_alpha_target;
  indices->isAlphaSource = is_alpha_source;
  
  g_assert( indices->total_bpp <= MAX_RESYNTH_BPP);
}


/*
Internal image format.

The count of pixelels in a pixel depends on the format type.
The engine uses a struct that tells where Pixelels are in an internal Pixel.
The internal Pixel aggregates color, mask, and map pixelels; for memory locality.

  Copyright (C) 2010, 2011  Lloyd Konneker

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

// Compiling switch #defines
#include "buildSwitches.h"
#ifdef SYNTH_USE_GLIB
  // Use glib via gimp.h
  #include <libgimp/gimp.h>
#else
  #include "glibProxy.h"
#endif

#include "imageSynthConstants.h"
#include "engineParams.h"
#include "imageFormat.h"

extern unsigned int
countPixelelsPerPixelForFormat(
  TImageFormat format // IN
  )
{
switch (format) {
  case T_RGB:
    return 3;
  case T_RGBA:
    return 4;
  case T_Gray:
    return 1;
  case T_GrayA:
    return 2;
  default:
    return 0; // Unhandled format type
  }
}


/*
Generate a TFormatIndices struct from a format code.

This is only for adapting to the SimpleAPI.
When adapting from Gimp, formatIndices is generated another way.
*/
extern int
prepareImageFormatIndicesFromFormatType(
  TFormatIndices* formatIndices,  // OUT
  TImageFormat format
  )
{
  switch (format) {
  case T_RGB:
    prepareImageFormatIndices( formatIndices,
      3,    // count color pixelels
      0,    // no map colors
      FALSE, // is_alpha_target,
      FALSE, // is_alpha_source,
      FALSE // isMap
      );
    break;
  case T_RGBA:
    prepareImageFormatIndices( formatIndices,
      3,    // count color pixelels
      0,    // no map colors
      TRUE, // is_alpha_target,
      // Since we are adapting by duplicating, if target has alpha, so does source
      TRUE, // is_alpha_source, 
      FALSE // isMap
      );
    break;
  case T_Gray:
    prepareImageFormatIndices( formatIndices,
      1,    // count color pixelels
      0,    // no map colors
      FALSE, // is_alpha_target,
      FALSE, // is_alpha_source,
      FALSE // isMap
      );
    break;
  case T_GrayA:
    prepareImageFormatIndices( formatIndices,
      1,    // count color pixelels
      0,    // no map colors
      TRUE, // is_alpha_target,
      TRUE, // is_alpha_source,
      FALSE // isMap
      );
    break;
  default:
    return IMAGE_SYNTH_ERROR_INVALID_IMAGE_FORMAT;
  }
  return 0;
}
  

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
  
  // The patch struct has a limit on pixelels per pixel
  g_assert( indices->total_bpp <= MAX_IMAGE_SYNTH_BPP);
}


// Test harness setter of pixel indices
void
prepareDefaultFormatIndices(
  TFormatIndices* formatIndices
  )
  /*
  Default is RGBA
  For testing.
  
  Engine internal pixel is:
  MRGBA
  012345
  */
{
// !!! MASK_PIXELEL_INDEX is a constant (0); mask pixelel is first, always.

formatIndices->img_match_bpp = 3;  // Match RGB but not A
formatIndices->colorEndBip = 4;  // Pixelels 1-3 (2nd through 4th)
formatIndices->alpha_bip = 4;			// Alpha bip must be defined: read but not synthesize
formatIndices->map_match_bpp = 0;  // No map pixelels
formatIndices->map_start_bip = 5;
formatIndices->map_end_bip = 5;

formatIndices->total_bpp = 5;

formatIndices->isAlphaTarget = TRUE; // Does target have alpha?
formatIndices->isAlphaSource = TRUE;

// !!! isMap is not a field of format indices, use map_match_bpp
}



/*
Prepare indices into our pixel format.

Synthesizer engine pixel contains mask pixelel and map pixelels
(Not just the color and alpha pixelels.)

IN: Image format (RGB, RGBA, Grey, etc.)
OUT: global index variables.

Not depend on Gimp

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


#ifndef __SYNTH_IMAGE_FORMAT_INDICIES_H__
#define __SYNTH_IMAGE_FORMAT_INDICIES_H__

/* 
bpp i.e. count of bytes (channels) per pixel or index thereof.
bpp is bytes per pixel
bip is byte index within pixel.
See data layout in resynth_types.h
*/
typedef unsigned char TPixelelIndex;

/*
Struct of indices of pixelels within pixels in internal image format.
Also flags.
One is used for the target, one for the source.
*/
typedef struct indicesStruct {
  TPixelelIndex colorEndBip; /* Index of last color pixelels in target/context image. */
  TPixelelIndex alpha_bip;      /* Index of target alpha pixelel */
  TPixelelIndex map_start_bip;  /* Index of map pixelels */
  TPixelelIndex map_end_bip;

  TPixelelIndex img_match_bpp; /* bpp to match in image. */
  TPixelelIndex map_match_bpp; /* bpp to match in map. */
  TPixelelIndex total_bpp;     /* Total pixelels */
  
  gboolean isAlphaTarget; // Does target have alpha?
  gboolean isAlphaSource; // Does source have alpha?
} TFormatIndices;

extern unsigned int
countPixelelsPerPixelForFormat(
  TImageFormat format // IN
  );
  
extern int
prepareImageFormatIndicesFromFormatType(
  TFormatIndices* indices,  // OUT
  TImageFormat format // IN
  );
  
extern void
prepareImageFormatIndices(
  TFormatIndices* indices,  // OUT
  guint count_color_channels_target,  // Must be same count in source
  guint count_color_channels_map,
  gboolean is_alpha_target,
  gboolean is_alpha_source,
  gboolean isMap
  );

extern void
prepareDefaultFormatIndices(
  TFormatIndices* formatIndices
  );
  
#endif /* __SYNTH_IMAGE_FORMAT_H__ */


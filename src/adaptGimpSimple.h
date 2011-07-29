/*
Adapt GIMP to SimpleAPI.

Probably throwaway, just harness for testing SimpleAPI driven from GIMP.

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


/*
Given ImageBuffers: image and mask.
Put it in Maps (image synthesis engine internal format.)
*/
void
adaptImageToBuffer(
  GimpDrawable *drawable,
  Map          *image,
  Map          *image_mask,
  ImageBuffer  *imageBuffer,
  ImageBuffer  *maskBuffer
  )
{
  // Adapt GIMP to existing API: GIMP drawable=>Map
  // Many parameters are global vars
  // Assert image and image_mask invalid, uninitialized.
  fetch_image_mask_map(drawable, image, total_bpp, image_mask, MASK_TOTALLY_SELECTED, 
      NULL /*map_out_drawable*/, map_start_bip);
  // assert image and image_mask now valid, same dimensions
  // assert mask represents selection in drawable
      
  // Adapt existing API to SimpleAPI: Map=>ImageBuffer
  initBufferAndAntiAdapt(imageBuffer, image, 1, 4);
  initBufferAndAntiAdapt(maskBuffer, image_mask, 0, 1);
  
  // Discard, since caller will re adapt SimpleAPI to existing API
  free_map(image);
  free_map(image_mask);
}


/*
Given one Gimp drawable,
put it in ImageBuffers: image and mask. 
*/
void
adaptGimpToSimple(
  GimpDrawable *image_drawable, 
  ImageBuffer* imageBuffer, 
  ImageBuffer* maskBuffer
  )
{
  // Adapt image from GIMP drawable to ImageBuffer
  
  // Only works for RGBA
  g_assert(image_drawable->bpp == 4);
  
  // Assert buffers have NULL data
  adaptImageToBuffer(
    image_drawable,
    &image, // global
    &image_mask, // global
    imageBuffer,
    maskBuffer
    );
  // Caller will adapt to SimpleAPI
  // Caller does not need separate corpus
}

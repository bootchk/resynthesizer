/*
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

#include <libgimp/gimp.h>

#include "drawable.h"
#include "coordinates.h"
#include "engineTypes2.h"
#include "map.h"
#include "mapIndex.h"
#include "byte_sequence.h"
#include "drawable.h"
#include "format.h"
#include "imageSynthConstants.h"

#include "adaptGimp.h"

/*
Adapt GIMP plugin to texture synthesis engine.

Functions to read and write pixmaps and bytemaps from and to Gimp.
*/



/*
Copy some channels of pixmap to GimpDrawable.
(Usually just the color and alpha channels, omitting the map channel and other channels.)

Unlike the original code:
- C instead of C++
- the origin is fixed at 0,0
- the Pixelel offset is fixed at 0.
The count of Pixelels moved is what drawable specifies, might be less than in pixmap.
That is, copy a slice of pixmap to drawable.
*/
/*
FUTURE We are copying the entire target image.
We only need to copy the selection (what was synthesized.)
The rest (the context) is unchanged.

FUTURE: copy the pixel values from the source to the target,
by the coordinates of the best match.
*/
static void
pixmap_to_drawable(
  Map           map,
  GimpDrawable *drawable,
  guint         pixelel_offset  // Index of starting Pixelel (channel) within Pixel sequence to move
  )
{
  guchar *raw_image_bytes = empty_byte_sequence_for_pixmap (map, drawable);

  /* Fill raw_image_bytes from pixmap, winnowing out mask and map pixelels. */
  get_byte_sequence_from_pixmap  (map, drawable, raw_image_bytes, pixelel_offset);

  byte_sequence_to_drawable (drawable, raw_image_bytes);

  g_free(raw_image_bytes);
}


gboolean
raw_bytes_all_zero (guchar *bytes, gint size)
{
  gboolean result;

  result = 1;
  for (gint i=0; i<size; i++)
    if (bytes[i] != 0)
    {
      result = 0;
      break;
    }
  return result;
}

/*
Adapt GimpDrawable to pixmap.

Where drawable is not a selection mask.

Can omit Pixelels (alpha) of the drawable.
Can offset Pixelels in the Pixel of the pixmap.
Can convert the format of the drawable to the format of the pixmap (bit depth, color model.)

Usually called many times, for an image, then other drawables,
to interleave many drawables into one pixmap.

Copy a sub-rect from the drawable.
*/
static void
pixmap_from_nonmask_drawable(
  Map                 map,
  GimpDrawable       *drawable,
  gint                pixelel_offset,          /* Which pixelels to copy to. */
  guint               pixelel_count_to_copy    /* Count of pixels to copy, might omit the alpha. */
  )
{
  guchar *raw_image_bytes;
  gint    raw_bytes_size;

  g_debug ("%s pixel offset %d pixel count %d", 
    G_STRFUNC, pixelel_offset, pixelel_count_to_copy);

  raw_image_bytes = byte_sequence_from_drawable_w_conversion (drawable, &raw_bytes_size);

  g_debug ("%s raw bytes empty? %d", G_STRFUNC, raw_bytes_all_zero (raw_image_bytes, raw_bytes_size));

  /* !!! Note our pixmap is same width, height as drawable, but depths may differ. */
  
  g_assert(raw_bytes_size > 0);

  /* Will fit in working Pixel */
  g_assert( pixelel_count_to_copy + pixelel_offset <= map.depth );

  /* 
  Drawable has enough bytes to copy.
  !!! This is not very strong, since the byte order may be strange.
  This prevents us from reading out of range.
  */
  g_assert( pixelel_count_to_copy <= get_bytes_per_pixel_for_drawable (drawable) );

  // assert raw_image_bytes holds bytes of pixels from a region of drawable

  set_byte_sequence_to_pixmap (map, drawable, raw_image_bytes,
    pixelel_count_to_copy,  // e.g. copy only three color bytes
    pixelel_offset,         // e.g. skip mask byte in destination
    raw_bytes_size);        // !!! Size of raw_image_bytes
  
  // assert map holds expanded pixels from a region of drawable
  // i.e. we converted format to our working format.
  // FUTURE can gegl/babl do an equivalent conversion?
  // Probably not, our format conversion is interleaving image and mask
  g_free(raw_image_bytes);
}


/*
Adapt GimpChannel to pixmap.
Where channel is a selection mask.
Format conversion can occur, from high bit-depth to bit-depth 8 bits.
Requires channel is drawable have exactly component: the mask value.
This copies exactly one component from channel to pixmap.

Can offset Pixelel into the Pixel of the pixmap.

Copy a sub-rect from the channel.
The sub-rect dimensions are taken from the pixmap.
The sub-rect must be inside the channel: x + sub-rect.width < channel.width
*/
static void
pixmap_from_mask_channel(
  Map                 map,          // OUT
  GimpChannel        *channel,      // IN
  gint                subrect_ULX,  // origin of rect to copy.
  gint                subrect_ULY,
  gint                pixelel_offset  // Which pixelel to copy to.
  /* count of pixelels is NOT an argument, is 1. */
  )
{
  guchar *raw_image_bytes;
  gint    raw_bytes_size;

  g_debug ("%s pixel offset %d origin x %d origin y %d", 
    G_STRFUNC, pixelel_offset, subrect_ULX, subrect_ULY);

  /* 
  This is the crux difference from pixmap_from_nonmask_drawable:
  get a byte_sequence of one byte per pixel (byte depth), i.e. one component.
  From a subrect of the SelectionMask channel.
  */
  raw_image_bytes = byte_sequence_from_mask_subrect (
    (GimpDrawable*)channel,
    subrect_ULX, subrect_ULY,
    map.width, map.height,
    &raw_bytes_size);

  g_debug ("%s raw bytes empty? %d", G_STRFUNC, raw_bytes_all_zero (raw_image_bytes, raw_bytes_size));

  /* !!! Note our pixmap is same width, height as channel, but bit-depths may differ. */
  
  g_assert(raw_bytes_size > 0);

  /* Require depth of passed map greater then 1 moved byte after offset. */
  g_assert( 1 + pixelel_offset <= map.depth );

  // assert raw_image_bytes holds one byte per pixel from a region of channel

  set_byte_sequence_to_pixmap (map, (GimpDrawable*) channel, raw_image_bytes,
    1,                      // e.g. copy one byte
    pixelel_offset,         // e.g. skip mask byte in destination
    raw_bytes_size);        // !!! Size of raw_image_bytes
  
  g_free(raw_image_bytes);
}


/*
Returns whether a selection exists
and it intersects the drawable.

User can create a selection outside of any specific layer.
*/
gboolean
is_intersecting_selection(
  GimpDrawable *drawable
  )
{
  gboolean result;
  gboolean is_selection;
  gboolean is_selection_intersect;

  /* Bug: original code did this:
  has_selection = gimp_drawable_mask_bounds(drawable->drawable_id,&x1,&y1,&x2,&y2);
  but that returns True if there is a selection that does not intersect.
  That led to blitting out of bounds of our mask copy.
  */
  /*
  If the corpus is a separate layer that does not intersect selection (for target)
  select the whole layer (what the user intends.)
  If user is required to select, it should be checked earlier (in calling plugins.)
  */

  {
    gint x1, y1, x2, y2;  // Not used, out from selection_bounds
    
    is_selection = selection_bounds(drawable, &x1, &y1, &x2, &y2);
    /* is_selection is True when there is a selection,
     * but it doesn't mean it intersects the drawable.
     */
  }
  {
    // Not used, out from selection_intersect()
    gint drawable_relative_x, drawable_relative_y;
    gint intersect_width, intersect_height;
  
    is_selection_intersect = selection_intersect(drawable,
      &drawable_relative_x, &drawable_relative_y, 
      &intersect_width, &intersect_height);
    /* is-selection_intersect is True
     * when (is selection AND intersects) OR is no selection.
     */
  }

  result = ( is_selection && is_selection_intersect);
  return result;
}

gboolean
is_pixmap_all_zero (Map map)
{
  gboolean result = 1;

  for (guint pixel=0; pixel<map.width*map.height; pixel++)
    if (g_array_index(map.data, Pixelel, pixel) != 0)
    {
      result = 0;
      break;
    }
  return result;
}

/*
Fills a Map with the selection's intersection with the drawable.  
The Map is a ByteMap, bit depth 8.
The Map is passed in with unknown values. 
Requires the Map is the same size as the Drawable.

Fills the Map entirely.
Zero, except non-zero values where the user's selection intersects the drawable.
Values where selection intersects range from 1-255, allowing shades of selection. 

When the user's selection does not intersect the drawable,
the ByteMap is all zero.

The user's selection is itself a Drawable.
*/
static void
get_selection_mask_pixmap_from_drawable (
  Map          *mask_pixmap,  // IN/OUT side effect is filling
  GimpDrawable *drawable)     
{
  GimpChannel *selection_mask_channel;
  Map          temp_bytemap;

  // !!! The coordinates are in the coordinate system of the drawable, not the image !!!
  gint intersect_ULX_DCS, intersect_ULY_DCS, intersect_width, intersect_height;
  
  /* Build a mask_pixmap that is unselected where the selection channel doesn't intersect,
  and having the value of the selection channel where it does intersect.
  */

  /* Initially Unselect full mask_pixmap */
  set_bytemap(mask_pixmap, MASK_UNSELECTED);

  /* Get the bounds of the SelectionMask intersection with drawable. */
  (void) /* Not use whether the intersection is empty. */
    gimp_drawable_mask_intersect(
      drawable, 
      &intersect_ULX_DCS, &intersect_ULY_DCS,
      &intersect_width, &intersect_height);

  /* Get new temp_bytemap the size of the selection intersection's.
   * temp_bytemap can be smaller than drawable.
   * temp_bytemap is not initialized.
   */
  new_bytemap (&temp_bytemap, intersect_width, intersect_height);

  /* 
  Get Gimp Channel/Drawable for SelectionMask channel.  
  It is in image/canvas coords.
  It can be larger than any drawable in the canvas.
  It can be smaller than the given drawable.
  It does NOT cover all drawables, when drawables lie outside the image/canvas.
  The offset of the drawable is always 0,0 i.e. origin in the image/canvas coordinate system.

  Passing the drawable, but SelectionMask is for the image of the drawable.
  */
  selection_mask_channel = (GimpChannel*) get_selection (drawable);

  // Assert the mask is a channel, i.e. a drawable of one component.
  // But the mask may be high-bit depth.

  /*
  Copy selection intersection from Gimp to our temp_bytemap.

  Destination pixelel is the first. 
  The source mask has one pixelel per pixel, and so does the destination bytemap.
  */
  {
    gint drawable_x_offset_ICS, drawable_y_offset_ICS;
    gint sub_rect_ULX_ICS, sub_rect_ULY_ICS;

    offsets(drawable, &drawable_x_offset_ICS, &drawable_y_offset_ICS); // Offset of layer in canvas

    /* Mask channel is image size and image coordinate system.
     * Convert intersection coordinates in Drawable coordinate system to coordinate in Image coordinate system.
     * Add drawable offset (can be negative) to coordinates in DCS
     */
    sub_rect_ULX_ICS = intersect_ULX_DCS + drawable_x_offset_ICS;
    sub_rect_ULY_ICS = intersect_ULY_DCS + drawable_y_offset_ICS;
  
  pixmap_from_mask_channel(temp_bytemap, selection_mask_channel,
    sub_rect_ULX_ICS, 
    sub_rect_ULY_ICS, 
    MASK_PIXELEL_INDEX);  // Dest pixelel is the first.
  }

  /* Blit the selection intersection onto our mask_pixmap, which is layer size, not image size. 
   * The layer can be larger than image.
   *
   * The mask_pixmap and the temp_bytemap are both byte_maps, one byte depth.
   * blit_map moves the entire Pixel, which is one Pixelel i.e. byte.
   */
  blit_map(mask_pixmap, &temp_bytemap, intersect_ULX_DCS, intersect_ULY_DCS);

  free_map(&temp_bytemap);
}



/*
Get drawable and a selection mask for it from GIMP.

May 2010 lkk Heavily revised:
 - to fix handling of selection
 - C++ => C
 - break into separate routines
 - interleave the mask pixelel into the Pixel

Fetch a local copy of a selection channel (or other mask?) from Gimp,
or create one if no selection exists or selection exists but does not intersect drawable.
*/

static void
fetch_mask(
  GimpDrawable *drawable,
  Map          *mask_pixmap, // OUT
  Pixelel       default_mask_value
  )
{
  g_debug ("%s", G_STRFUNC);

  new_bytemap(mask_pixmap, width(drawable), height(drawable));

  if (is_intersecting_selection(drawable))
  {
    /* Is a selection and it intersects drawable. */
    get_selection_mask_pixmap_from_drawable (
      mask_pixmap,
      drawable);
  }
  else
  { 
    /* User made no selection, or selected outside the drawable. */
    set_bytemap(mask_pixmap, default_mask_value);
    
    /* This is confusing enough to users and programmers that it deserves a debug message.
    On all platforms, this only prints if a console is already open.
    */
    g_debug("Drawable without intersecting selection, using entire drawable.");
  }
  
  g_debug ("%s: is all zero %d", G_STRFUNC, is_pixmap_all_zero(*mask_pixmap));

  // mask_pixmap is size of drawable, but a selection mask: Pixel is one byte.
}



static void
fetch_image_and_mask(
  GimpDrawable       *drawable,          // IN
  Map                *pixmap,            // OUT our color pixmap of drawable
  guint               pixelel_count,     // IN total count mask+image+map Pixelels in our Pixel
  Map                *mask,              // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel             default_mask_value // IN default value for any created mask
  )
{

  /* Both OUT pixmaps same 2D dimensions.  Depth pixelel_count includes a mask byte. */
  g_debug ("%s", G_STRFUNC);

  new_pixmap(pixmap, width(drawable), height(drawable), pixelel_count);

  /* Get color, alpha channels */
  pixmap_from_nonmask_drawable(*pixmap, drawable, FIRST_PIXELEL_INDEX, 
                        get_working_pixelels_per_pixel_for_target_or_corpus (drawable));

  fetch_mask(drawable, mask, default_mask_value); /* Get mask channel */

  debugPixmap("mask", mask);
  debugPixmap("pixmap", pixmap);
  interleave_mask(pixmap, mask);  /* Insert mask byte into our Pixels */
}


/* 
Test Gegl format conversions to and from byte_sequence. 

Used only during development.
Get byte sequence from drawable, and then back to drawable.
The drawable has-a BablFormat.
The round trip converts to the resynthesizer working format, and back.
Afterwards, the drawable should have the original format,
but some accuracy might have been lost.
*/
void
testGegl (
  GimpDrawable *drawable)   
{
  guchar *raw_image_bytes;
  gint    raw_bytes_size;

  raw_image_bytes = byte_sequence_from_drawable_w_conversion (drawable, &raw_bytes_size);
  byte_sequence_to_drawable(drawable, raw_image_bytes);
  g_free(raw_image_bytes);
}



/*
Create our internal pixmap having color, mask, and map Pixelels in one Pixel.
Memory locality improves performance.

Each of our pixmap elements (pixel) is a sequence of channel pixelels,
e.g.
  MRGBA if color w alpha w no map,
  MRGBARGB if color, alpha, and color map (note discard map alpha)
  MRGBW if color, no alpha, w greyscale map
Max of eight pixelels.
Note we prepend the mask byte.
*/
void
fetch_image_mask_map(
  GimpDrawable       *image_drawable,     // IN image: target or corpus drawable
  Map                *pixmap,             // OUT our pixmap of drawable
  guint               pixelel_count,      // IN count channels in image + map
  Map                *mask,               // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel             default_mask_value, // IN default value for any created mask
  GimpDrawable       *map_drawable,       // IN map drawable, target or corpus
  guint               map_offset          // IN index in our Pixel to first map Pixelel
  )
{
  /* Fetch image.  If no selection mask, create one defaulted to SELECTED.
  The selection mask distinguishes the target from the context (which is optional.)
  */
  fetch_image_and_mask(image_drawable, pixmap, pixelel_count, mask, default_mask_value);

  /*
  Append some of the map channels (Pixelels) to our Pixel.  map_offset is the destination Pixelel.
  !!! Note the alpha channel of the map gets discarded.
  */
  if (map_drawable)
  {
    /* Count map channels excluding alpha. */
    guint pixelels_to_copy = get_working_pixelels_per_pixel_for_weight_map (map_drawable);
    
    pixmap_from_nonmask_drawable(*pixmap, map_drawable, map_offset, pixelels_to_copy);
  }
}



/*
Update Gimp image from local pixmap. Canonical postlude for plugins.
!!! Called in the postlude but also for debugging: animate results during processing.
*/
void
post_results_to_gimp(
  GimpDrawable *drawable,
  Map           targetMap)
{
  // our pixels back to Gimp.  Since 2.10, using GeglBuffers, and this flushes them
  g_debug("%s", G_STRFUNC);

  pixmap_to_drawable(targetMap, drawable, FIRST_PIXELEL_INDEX);

  /*
  Flush the drawable to the screen
  This is the only way to see the results of the plugin.
  This is canonical boilerplate for a Gimp plugin.
  It is nearly the last thing the plugin does.
  */
  if ( ! merge_shadow(drawable) )
    g_debug ("%s fail merge shadow", G_STRFUNC);
  update(drawable, 0, 0, targetMap.width, targetMap.height);
  gimp_displays_flush();
}
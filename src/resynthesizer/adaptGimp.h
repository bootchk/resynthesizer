/*
Adapt GIMP plugin to texture synthesis engine.

Functions to read and write pixmaps and bytemaps from and to Gimp.

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

// Engine and Gimp types must be included previously


/*
Blit (copy a sub-rect) of source map to destination map.
Here, the entire pixel element of the map is copied.

original c++ code, which only copied a byte:
    for(gint y=0;y<temp_mask.height;y++)
      for(gint x=0;x<temp_mask.width;x++)
        mask.at(x+drawable_relative_x, y+drawable_relative_y)[0] = 
          temp_mask.at(x,y)[0]; // Only one channel (byte)
*/
void
blit_map(
  Map* dest_map,
  Map* source_map,
  gint offset_x,
  gint offset_y
  )
{
  guint y;
  guint x;
  
  for(y=0;y<source_map->height;y++)
    for(x=0;x<source_map->width;x++)
    {
      Coordinates coords = {x,y};
      Coordinates dest_coords = {x+offset_x, y+offset_y};
      
      *pixmap_index(dest_map, dest_coords) = *pixmap_index(source_map, coords);
    }
}


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
*/
void 
pixmap_to_drawable(
  Map                 map,
  GimpDrawable *drawable, 
  guint               pixelel_offset  // Index of starting Pixelel (channel) within Pixel sequence to move
  )
{
  guchar *img;

  /* Create a new, non-sparse sequence of Pixelels for Gimp. */
  guint width = map.width;
  guint height = map.height;
  /* Count Pixelels to copy, whatever drawable wants, we have optional alpha Pixelel in our Pixel. */
  guint pixelel_count = bpp(drawable); 
  gint size = width * height * pixelel_count;  // !!! Size of drawable, not the pixmap
  
  g_assert( pixelel_offset + pixelel_count <= map.depth ); // Pixmap has more pixelels than offset + count
  
  
  img = g_malloc(size);
  
  {
  guint i;
  guint j;
  
  for(i=0; i<width*height; i++)  // Iterate over map as a sequence
    for(j=0; j<pixelel_count; j++)  // Iterate over Pixelels
      img[i*pixelel_count+j] = 
        g_array_index(map.data, Pixelel, i*map.depth+pixelel_offset+j);
  }
        
  /* Send seq of Pixelels to Gimp, including flush */
  {
    GeglBuffer *dest_buffer;
    const Babl *format;

    // 2.10 wants ID, 2.99 wants *
    
    // Can't explain yet why it must be a shadow buffer??
    dest_buffer = get_shadow_buffer(drawable);

    // Need format.  We are not changing it, but gegl_buffer_set wants it
    format = gegl_buffer_get_format(dest_buffer);

    // gimp_pixel_rgn_set_rect(&region, img, 0,0, width, height);
    gegl_buffer_set(dest_buffer,
            GEGL_RECTANGLE(0,0, width, height),
            0,  // scale, 0 or 1?
            format,
            img,
            GEGL_AUTO_ROWSTRIDE);
    // unref is documented to flush automatically, but it doesn't, so do it explicitly
    gegl_buffer_flush(dest_buffer);
    g_object_unref (dest_buffer);
  }

  g_free(img);
}


/*
Copy SOME channels of GimpDrawable to pixmap, possibly offsetting them in the Pixel.
(Usually called many times, for image, then mask, then other drawables,
to interleave many drawables into one pixmap.)
Copy a sub-rect from the drawable.
*/
static void 
pixmap_from_drawable(
  Map                 map,
  GimpDrawable *drawable,
  gint                x,                       /* origin of rect to copy. */
  gint                y,
  gint                pixelel_offset,          /* Which pixelels to copy to. */
  guint               pixelel_count_to_copy    /* Count of pixels to copy, might omit the alpha. */
  )
{
  GeglBuffer *buffer;
  const Babl *format;
  guchar *img;
  
  /* !!! Note our pixmap is same width, height as drawable, but depths may differ. */
  guint width = map.width;
  guint height = map.height;
  guint drawable_size = width * height * bpp(drawable);

  g_assert(drawable_size > 0);
  /* Will fit in our Pixel */
  g_assert( pixelel_count_to_copy + pixelel_offset <= map.depth );
  /* Drawable has enough to copy */
  g_assert( pixelel_count_to_copy <= bpp(drawable) );
  
  buffer = get_buffer(drawable);
  // , x,y, map.width, map.height, FALSE,FALSE);
  format = gegl_buffer_get_format(buffer);

  img = g_malloc(drawable_size);
  
  /* 
  Get all the pixelels from drawable into img sequence.
  Note x1,y1 are in drawable coords i.e. relative to drawable
  The drawable may be offset from the canvas and other drawables.
  */
  // gimp_pixel_rgn_get_rect(&region, img, x,y, width,height);
  gegl_buffer_get(buffer,
            GEGL_RECTANGLE(x,y, width, height),
            1,  // scale
            format,
            img,
            GEGL_AUTO_ROWSTRIDE,
            GEGL_ABYSS_NONE);
  g_object_unref (buffer);
  // assert img holds pixels from a region of drawable

  {
  guint i;
  guint j;
  
  /* Copy SOME of the pixels from img sequence to our pixmap, OFFSET them. */
  for(i=0; i<width*height; i++)
    for(j=0; j<pixelel_count_to_copy; j++)  /* Count can be different from strides. */
        g_array_index(map.data, Pixelel, i*map.depth+pixelel_offset+j)  /* Stride is depth of pixmap. */
          = img[i*bpp(drawable)+j];   /* Stride is bpp */
  }
  // assert map holds expanded pixels from a region of drawable
  // i.e. we converted format.  
  // FUTURE can gegl/babl do an equivalent conversion?
  // Probably not, our format conversion is interleaving image and mask
  g_free(img);
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
  Map                *mask,
  Pixelel             default_mask_value
  ) 
{
  gint drawable_relative_x, drawable_relative_y;
  
  gboolean is_selection;
  gboolean is_selection_intersect;
  gint intersect_width, intersect_height;

  new_bytemap(mask, width(drawable), height(drawable));
  
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
  gint x1, y1, x2, y2;
  //is_selection = gimp_drawable_mask_bounds(drawable->drawable_id, &x1, &y1, &x2, &y2);
  is_selection = selection_bounds(drawable, &x1, &y1, &x2, &y2);
  }
 
  //is_selection_intersect = gimp_drawable_mask_intersect(drawable->drawable_id, 
  //    &drawable_relative_x, &drawable_relative_y, &intersect_width, &intersect_height);
  is_selection_intersect = selection_intersect(drawable, 
     &drawable_relative_x, &drawable_relative_y, &intersect_width, &intersect_height);
      
  if ( ! is_selection || ! is_selection_intersect) {
    set_bytemap(mask, default_mask_value);
    /* This is confusing enough to users and programmers that it deserves a debug message. 
    On all platforms, this only prints if a console is already open.
    */
    g_debug("Drawable without intersecting selection, using entire drawable.");
  }
  else /* Is a selection and it intersects drawable. */
  {
    Map temp_mask;
    GimpDrawable *mask_drawable;
    // GeglBuffer *mask_buffer;
    gint xoff,yoff;
    
    /* Build a mask that is unselected where the selection channel doesn't intersect,
    and having the value of the selection channel where it does intersect.
    */
    
    /* Initially Unselect full mask */
    set_bytemap(mask, MASK_UNSELECTED);
    
    /* Get the selection intersection's bytemap into temp_mask */
    new_bytemap(&temp_mask, intersect_width, intersect_height);
    
    /* Get Gimp drawable for selection channel.  It is in image coords, i.e. anchored at 0,0 image */
    // OLD mask_drawable_id = gimp_image_get_selection(gimp_item_get_image(drawable->drawable_id));
    // mask_drawable = gimp_drawable_get(mask_drawable_id);
    mask_drawable = get_selection(drawable);
    g_assert(bpp(mask_drawable) == 1);   /* Masks have one channel. */

    //gimp_drawable_offsets(drawable->drawable_id, &xoff, &yoff); // Offset of layer in image
    offsets(drawable, &xoff, &yoff); // Offset of layer in image

    /* 
    Copy selection intersection bytemap from Gimp to our temp_mask bytemap.
    
    Destination is the first channel.  Assert only one channel in the drawable.
    
    Since mask_drawable is image size, in image coords, calculate coords of intersection in image coords =
    (offset of drawable plus drawable relative coords of selection)
    */
    pixmap_from_drawable(temp_mask, mask_drawable, 
      drawable_relative_x+xoff, drawable_relative_y+yoff, MASK_PIXELEL_INDEX, bpp(mask_drawable));
    
    // Obsolete gimp_drawable_detach(mask_drawable);

    /* Blit the selection intersection onto our mask, which is only layer size, not image size. */
    blit_map(mask, &temp_mask, drawable_relative_x, drawable_relative_y);
    
    free_map(&temp_mask);
    }
}



void 
fetch_image_and_mask(
  GimpDrawable *drawable,          // IN
  Map                *pixmap,            // OUT our color pixmap of drawable
  guint               pixelel_count,     // IN total count mask+image+map Pixelels in our Pixel
  Map                *mask,              // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel             default_mask_value // IN default value for any created mask
  ) 
{
   
  /* Both OUT pixmaps same 2D dimensions.  Depth pixelel_count includes a mask byte. */
  new_pixmap(pixmap, width(drawable), height(drawable), pixelel_count);
  
  /* Get color, alpha channels */
  pixmap_from_drawable(*pixmap, drawable, 0,0, FIRST_PIXELEL_INDEX, bpp(drawable));  
  fetch_mask(drawable, mask, default_mask_value); /* Get mask channel */
  interleave_mask(pixmap, mask);  /* Insert mask byte into our Pixels */
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
  GimpDrawable *image_drawable,     // IN image: target or corpus drawable
  Map                *pixmap,             // OUT our pixmap of drawable
  guint               pixelel_count,      // IN count channels in image + map
  Map                *mask,               // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel             default_mask_value, // IN default value for any created mask
  GimpDrawable *map_drawable,       // IN map drawable, target or corpus
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
    guint pixelels_to_copy = bpp(map_drawable);
    if ( has_alpha(map_drawable) )
      pixelels_to_copy--;
    pixmap_from_drawable(*pixmap, map_drawable, 0,0, map_offset, pixelels_to_copy);
  }
}


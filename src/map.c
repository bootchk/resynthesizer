/*
Maps i.e. 2D, coordinate addressable, arrays of element.

lkk I couldn't find a suitable library that implement this.
So this is a conventional implementation using pointer arithmentic on a 1-D array.
Here, the 1-D array is a GArray.

In earlier resynthesizer c++ coding, this used templates.
That is, a Bitmap was a 2D array class parameterized by the type of the element:
  Pixel (byte array) 
  Coordinates
  int
  boolean (represented by single byte)
Here, there are separate functions for creating and indexing each type of map.

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
  #include "../config.h" // GNU buildtools local configuration
  // Use glib via gimp.h
  #include <libgimp/gimp.h>
#endif

#include "imageSynthConstants.h"
#include "map.h"

void
free_map (Map *map)
{
  g_array_free(map->data, TRUE);
  map->data = (GArray *) NULL;
}
  

/*  Create new Pixmap having Pixel of depth. IE a 3D array */
void
new_pixmap(
  Map * map,
  guint width, 
  guint height, 
  guint depth
  )
{
  map->width = width;
  map->height = height;
  map->depth = depth;
  /*
  Equivalently, but not if Pixelel size changes.
  IOW the code now depends on Pixelel equal one byte.
   guint size = width * height * depth;
   map->data = g_array_sized_new (FALSE, TRUE, sizeof(Pixelel), size);
  */
  map->data = g_array_sized_new (FALSE, TRUE, depth, width * height);
}


/* Create dynamic 2-D array of guint. */
void
new_intmap(
  Map * map,
  guint width, 
  guint height
  )
{
  map->width = width;
  map->height = height;
  map->depth = sizeof(guint);   // Not used
  map->data = g_array_sized_new (FALSE, TRUE, sizeof(guint), width * height);
}

/* Create dynamic 2-D array of Coordinates. */
void
new_coordmap(
  Map * map,
  guint width, 
  guint height
  )
{
  map->width = width;
  map->height = height;
  map->depth = sizeof(Coordinates);   // Not used
  map->data = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), width * height);
}
  
/* Create dynamic 2-D array of guchar. */
void
new_bytemap(
  Map * map,
  guint width, 
  guint height
  )
{
  /* Implement bytemap as pixmap of depth 1 */
  new_pixmap(map, width, height, 1);
}


/* Set all elements of bytemap to a value. 
TODO use memset?
*/
void
set_bytemap(
  Map* map,
  guchar value
  )
{
  guint y;
  guint x;
  
  for (y=0; y<map->height; y++)
    for (x=0; x<map->width; x++)
    {
      Coordinates coords = {x,y};
      *bytemap_index(map, coords) = value;
    }
}

void 
invert_bytemap(
  Map* map
  )
{
  guint y;
  guint x;
  
  for (y=0; y<map->height; y++)
    for (x=0; x<map->width; x++)
    {
      Coordinates coords = {x,y};
      // Ones complement: bitwise negation
      *bytemap_index(map, coords) = ~ *bytemap_index(map, coords);
    }
}


/*
Interleave one pixelel of mask pixmap into pixelels of pixmap.

lkk Mask bytemap was separate.  Interleaved them for better memory locality.
The map pixmap was interleaved with the color pixmap, so why not the mask too.
*/
void
interleave_mask(
  Map *pixmap,
  Map *mask
  )
{
  guint i;
  
  guint size = pixmap->height * pixmap->width;
  g_assert( size == mask->height * mask->width);  /* Same dimensions. */

  for (i=0; i < size; i++)
    /* Copy one byte */
    g_array_index(pixmap->data, Pixelel, i*pixmap->depth + MASK_PIXELEL_INDEX) =
          g_array_index(mask->data, Pixelel, i*mask->depth);
}


/*
Indexing into a Map

This is dictated by use of glib GArray
instead of conventional malloc and pointer arithmetic.
Presumably glib does good error checking for malloc.
Here we also do arithmetic for indexing a dynamic multi-dimensional array.
Here we use static inline rather than a macro.
!!! g_array_index is a macro that casts types.
!!! g_array_index returns an element, we return address of. 
*/


/* 
!!! Note in this case the 3rd dimension, depth, varies. 
i.e. a Pixel is a variable-length array of Pixelels.
*/
inline Pixelel*
pixmap_index(
  const Map * const map,
  const Coordinates coords
  )
{
  guint index = (coords.x + coords.y * map->width) * map->depth;
  return &g_array_index(map->data, Pixelel, index);
}
  
/* Return pointer to guint at coordinates in map. */
inline guint*
intmap_index(
  Map* map,
  const Coordinates coords
  )
{
  guint index = coords.x + coords.y * map->width;
  return &g_array_index(map->data, guint, index);
}
  
/* Return pointer to coordinates at coordinates in map. */
inline Coordinates*
coordmap_index(
  Map* map,
  const Coordinates coords
  )
{
  guint index = coords.x + coords.y * map->width;
  return &g_array_index(map->data, Coordinates, index);
}

/* Return pointer to boolean at coordinates in Pixmap. */
/* Use guchar as boolean, which corresponds to current use of bytes for masks in Gimp. */
inline guchar*
bytemap_index(
  Map* map,
  const Coordinates coords
  )
{
  guint index = coords.x + coords.y * map->width;
  return &g_array_index(map->data, guchar, index);
}



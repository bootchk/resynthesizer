/*

mapIndex.h

These are included from engine.c and imageSynth.c so they can be inlined.
This is a simple, portable model for inlining: Declared/defined in one place.  
If the compiler does not inline, may be separate instances of the function in separate compilation units.

Indexing into a Map

This is dictated by use of glib GArray
instead of conventional malloc and pointer arithmetic.
Presumably glib does good error checking for malloc.
Here we also do arithmetic for indexing a dynamic multi-dimensional array.
Here we use static inline rather than a macro.
!!! g_array_index is a macro that casts types.
!!! g_array_index returns an element, we return address of.

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
!!! Note in this case the 3rd dimension, depth, varies. 
i.e. a Pixel is a variable-length array of Pixelels.
*/
static inline Pixelel*
pixmap_index(
  const Map * const map,
  const Coordinates coords
  )
{
  guint index = (coords.x + coords.y * map->width) * map->depth;
  return &g_array_index(map->data, Pixelel, index);
}
  
/* Return pointer to guint at coordinates in map. */
static inline guint*
intmap_index(
  Map* map,
  const Coordinates coords
  )
{
  guint index = coords.x + coords.y * map->width;
  return &g_array_index(map->data, guint, index);
}
  
/* Return pointer to coordinates at coordinates in map. */
static inline Coordinates*
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
static inline guchar*
bytemap_index(
  Map* map,
  const Coordinates coords
  )
{
  guint index = coords.x + coords.y * map->width;
  return &g_array_index(map->data, guchar, index);
}



/*
This is dictated by the need to use glib GArray
instead of conventional malloc and pointer arithmetic.
Presumably glib does good error checking for malloc.
Here we also do arithmetic for indexing a dynamic multi-dimensional array.
Here we use static inline rather than a macro.
!!! g_array_index is a macro that casts types.
!!! g_array_index returns an element, we return address of. 
*/


/* Return pointer to Pixelel at coordinates in Pixmap. */
/*
Accessors of dynamic 2D arrays.
*/

/* 
!!! Note in this case the 3rd dimension, depth, varies. 
i.e. a Pixel is a variable-length array of Pixelels.
*/
static inline Pixelel*
pixmap_index(
  Map * map,
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


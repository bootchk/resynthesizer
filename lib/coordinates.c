
/* Methods on Coordinate class. */

#include <glib.h>

#include "coordinates.h"


/* 
Convert bool to a qsort style compare result required by glib g_array_sort().
Note that we never return 0 for equal,
but that should not hurt the sort.
*/
static inline CompareResult
to_sort_result
  (gboolean value)
{
  return ( (value == 1) ? -1 : 1);  // equality not a case
}

/* !!! Note you can't use the not operator (!) on a CompareResult */
static inline CompareResult
to_invert_sort_result
  (CompareResult value)
{
  return ( (value == 1) ? -1 : 1);  // equality not a case
}




/*
Compare funcs for sorting.
!!! Note return a gint (-1, 0, 1) rather than a bool as for c++
Returns -1 for first arg is less than second arg, 0 for equal, 1 if first arg is greater than second arg
Also, for c++ vector sort, take a Coordinates, not a *Coordinates
*/

/*  
These functions cannot be inlined because they are passed to sort. 
Besides, the sorting is not a bottleneck.
*/

/* less/more 2D distance: from center for offsets: x^2 + y^2 */
CompareResult 
lessCartesian(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  return to_sort_result((a->y * a->y) + (a->x * a->x) < (b->y * b->y) + (b->x * b->x));
}

CompareResult
moreCartesian(
  const Coordinates *a,
  const Coordinates *b
  ) 
{
  return to_invert_sort_result(lessCartesian(a,b));
}

/* 
less/more proportional distance along ray to the center
!!! Requires a sort element with that value
*/
CompareResult
lessInward(
  const TSortElementStruct *a,
  const TSortElementStruct *b
  ) 
{
  return to_sort_result( a->proportionToCenter < b->proportionToCenter );
}

CompareResult
moreInward(
  const TSortElementStruct *a,
  const TSortElementStruct *b
  ) 
{
  return to_invert_sort_result( lessInward(a,b) );
}

/* less/more horizontal distance: from center for offsets */
CompareResult 
lessHorizontal(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  return to_sort_result((a->x * a->x) < (b->x * b->x));
}
    
CompareResult
moreHorizontal(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  return to_invert_sort_result(lessHorizontal(a,b));
}
    
CompareResult 
lessVertical(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  /* less vertical distance: from center for offsets */
  return to_sort_result((a->y * a->y) < (b->y * b->y));
}

CompareResult
moreVertical(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  return to_invert_sort_result(lessVertical(a,b));
}







/*
Coordinate and offset arithmetic
*/
Coordinates 
add_points (const Coordinates a, const Coordinates b) 
{
  Coordinates coords;
  coords.x = a.x + b.x;
  coords.y = a.y + b.y;
  return coords;
}
    
Coordinates 
subtract_points (const Coordinates a, const Coordinates b) 
{
  Coordinates coords;
  coords.x = a.x - b.x;
  coords.y = a.y - b.y;
  return coords;
}

gboolean 
equal_points(const Coordinates a, const Coordinates b) 
{
  return (a.x == b.x) && (a.y == b.y); 
}
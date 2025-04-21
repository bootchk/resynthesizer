
#include <glib.h>

#include "engineTypes2.h"  // PointVector
#include "coordinates.h"

#include "bounds.h"


/* get bounds of vector of points. */
Bounds
get_bounds ( 
  PointVector points, 
  guint       size 
  ) 
{
  Bounds bounds = {G_MAXINT, G_MAXINT, 0 ,0};
  
  guint i;
  for(i=0; i<size; i++) 
  {
    // c++ coords = points[i];
    Coordinates coords = g_array_index(points, Coordinates, i);
    bounds.ulx = MIN(bounds.ulx, coords.x);
    bounds.uly = MIN(bounds.uly, coords.y);
    bounds.lrx = MAX(bounds.lrx, coords.x);
    bounds.lry = MAX(bounds.lry, coords.y);
  }
  // NOT g_assert(bounds.ulx >= 0); since this may be called for offsets, not just positive coordinates
  return bounds;
}


/* get center of bounds of vector of points. */
Coordinates
get_center ( 
  PointVector points, 
  guint       size 
  ) 
{
  Coordinates center;
  Bounds bounds = get_bounds(points, size);
  center.x = (bounds.lrx - bounds.ulx) / 2 + bounds.ulx;
  center.y = (bounds.lry - bounds.uly) / 2 + bounds.uly;
  return center;
}

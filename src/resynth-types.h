/*
resynth-types.c

Some type definitions, mainly Coordinates and pointVector.
In earlier versions, this was mostly c++ vector and operators on it.

    The Resynthesizer - A GIMP plug-in for resynthesizing textures
    Copyright (C) 2010 Lloyd Konneker
    Copyright (C) 2000 2008  Paul Francis Harrison
    Copyright (C) 2002  Laurent Despeyroux
    Copyright (C) 2002  David Rodríguez García

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


typedef guint8 Pixelel;

/*
Coordinates class

!!! These are signed ints, can represent offsets or positive coordinates 
Similar to GdkPoint.
*/
typedef struct {
    gint x;
    gint y;
} Coordinates;


gboolean 
equal_points(const Coordinates a, const Coordinates b) 
{
  return (a.x == b.x) && (a.y == b.y); 
}

/*
Compare funcs for sorting.
!!! Note return a gint (-1, 0, 1) rather than a bool as for c++
Returns -1 for first arg is less than second arg, 0 for equal, 1 if first arg is greater than second arg
Also, for c++ vector sort, take a Coordinates, not a *Coordinates
*/
typedef gint CompareResult;

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


/*The grad (radial from 0..400) or angle of this vector (point) */
static guint
grad (Coordinates a)
{ 
  /* !!! x and y order in atan2 */
  return (guint) (atan2( (gfloat) a.y, (gfloat) a.x) * 200 / G_PI + 200);
}


/* Array of computed maximum distances over all points along a ray (radius) */
guint max_cartesian_along_ray[401];

/* 
Ratio of this offset's distance from center to max distance of any point along
the radial through this offset.
Float in range (0,1]
Note a is an offset, i.e. a vector from center, not from the origin.
*/
static gfloat
proportion_inward(const Coordinates *a)
{
  guint ray = grad(*a);
  g_assert( max_cartesian_along_ray[ray] > 0 );
  return (gfloat) ((a->y * a->y) + (a->x * a->x)) / max_cartesian_along_ray[ray];
}

/*  
These functions cannot be inline because they are passed to sort. 
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
less/more proportional distance along radii to the center 
!!! Require a prior call to prepare_max_cartesian_along_ray
*/
CompareResult
lessInward(
  const Coordinates *a, 
  const Coordinates *b
  ) 
{
  return to_sort_result( proportion_inward(a) < proportion_inward(b) );
}

CompareResult
moreInward(
  const Coordinates *a, 
  const Coordinates *b
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



/*
Class: Bounds
*/
typedef struct {
  gint ulx, uly, lrx, lry;
  } Bounds;




/*
Class 1D array (vector or sequence) of Coordinates.
*/
typedef GArray * pointVector;


/* get bounds of vector of points. */
Bounds
get_bounds ( 
  pointVector points, 
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
  g_assert(bounds.ulx >= 0);  /* TODO other checks */
  return bounds;
}


/* get center of bounds of vector of points. */
Coordinates
get_center ( 
  pointVector points, 
  guint       size 
  ) 
{
  Coordinates center;
  Bounds bounds = get_bounds(points, size);
  center.x = (bounds.lrx - bounds.ulx) / 2 + bounds.ulx;
  center.y = (bounds.lry - bounds.uly) / 2 + bounds.uly;
  return center;
}
  


/* swap two elements of a point vector */
inline void
swap_vector_elements(
  pointVector vector,
  guint size,
  guint i, 
  guint j
  ) 
{
  Coordinates temp;
  
  g_assert(i<size);
  g_assert(j<size);  
  
  temp = g_array_index(vector, Coordinates, i);
  g_array_index(vector, Coordinates, i) = g_array_index(vector, Coordinates, j);
  g_array_index(vector, Coordinates, j) = temp;
}







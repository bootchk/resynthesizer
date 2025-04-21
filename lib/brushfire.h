/*
Order target points for (thinning) brushfire order of synthesis.
Order by proportional distance from center to edge along rays.

Ordering by distance from center makes synthesis proceed circularly.
This ordering proceeds as a brushfire, uniformly from or toward edges.

Formerly this was spread out in orderTarget.h and engineTypes.h.
See also changes there.

This change was made to eliminate the static variable max_cartesian_along_ray,
which caused a bug during threaded execution.

It is complicated by the fact that qsort compare funcs only take two parameters.
So we create a struct having thing to be sorted (a target point coordinate)
and having the associated value to be sorted by (proportional distance),
and pass that to the compare func which sorts on the latter.
*/

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
#if FALSE
static void
dumpTargetPoints(PointVector targetPoints)
{
  guint i;
  for(i=0; i<targetPoints->len; i++)
      {
      Coordinates point = g_array_index(targetPoints, Coordinates, i);
      g_printf("%d %d\n", point.x, point.y);
      }
}
static void
dumpSortArray(GArray* sortArray)
{
  guint i;
  g_printf("Sort array:\n");
  for(i=0; i<sortArray->len; i++)
      {
      TSortElementStruct sortElement = g_array_index(sortArray, TSortElementStruct, i) ;
      g_printf("%d %d %f\n", sortElement.targetPoint.x, sortElement.targetPoint.y, sortElement.proportionToCenter );
      }
}
#endif

/*The grad (radial from 0..400) or angle of this vector (point) */
static guint
grad (Coordinates a)
{
  /* !!! x and y order in atan2 */
  return (guint) (atan2( (gfloat) a.y, (gfloat) a.x) * 200 / G_PI + 200);
}



/* Array of computed maximum distances over all points along a ray (radius) */
static void
prepare_max_cartesian_along_ray(
    PointVector targetPoints,   // Already offsets from center
    guint* max_cartesian_along_ray
    )
{
  guint i;

  for(i=0; i<401; i++)
    max_cartesian_along_ray[i] = 0;

  for(i=0; i<targetPoints->len; i++)
  {
    Coordinates point = g_array_index(targetPoints, Coordinates, i);
    // Assert point is an offset from center
    guint cartesian = point.x * point.x + point.y * point.y;
    guint ray = grad(point);
    if ( cartesian > max_cartesian_along_ray[ray] )
      max_cartesian_along_ray[ray] = cartesian;
  }
  /*
   * Not all rays in grad units will have points on them, i.e. may have 0 max cartesian.
   * But all rays on which some targetPoint falls WILL have non-zero max.
   * Except for edge case of a single pixel target, which is handled safely in proportion_inward().
   * We only index into this array by the rays for targetPoints (not every ray.)
   */
  /* dump_max_grad(); */
}


/*
Ratio of this offset's distance from center to max distance of any point along
the radial through this offset.
Float in range (0,1] or NaN
Note a is an offset, i.e. a vector from center, not from the origin.
*/
static gfloat
proportion_inward(
  const Coordinates *a,
  guint* max_cartesian_along_ray
  )
{
  guint ray = grad(*a);
  /*
   * Note that max_cartesian_along_ray[ray] MAY equal zero, for a target having only one pixel, etc.
   * Floating division yields NaN which always compares FALSE.
   */
  return (gfloat) ((a->y * a->y) + (a->x * a->x)) / max_cartesian_along_ray[ray];
}

/*
Copy targetPoint coordinates to sorting struct.
Precompute another field to sort on: proportional distance from center.
*/
static GArray*
targetPointsToSortArray(
  PointVector targetPoints
  )
{
  GArray* sortArray = g_array_sized_new (FALSE, TRUE, sizeof(TSortElementStruct), targetPoints->len);

  guint max_cartesian_along_ray[401];
  prepare_max_cartesian_along_ray(targetPoints, max_cartesian_along_ray);

  guint i;
  TSortElementStruct sortElement;
  Coordinates point;
  for(i=0; i<targetPoints->len; i++)
    {
    point = g_array_index(targetPoints, Coordinates, i); // Allocates, but len is zero: no elements
    sortElement.targetPoint = point;
    sortElement.proportionToCenter = proportion_inward(&point, max_cartesian_along_ray);
    g_array_append_val(sortArray, sortElement);
    // g_printf("%d %d %d\n", sortElement.targetPoint.x, sortElement.targetPoint.y, sortElement.proportionToCenter );
    }
  return sortArray;
}

// Extract coordinates from sort array, and free the sort array
static void
targetPointsFromSortArray(
    PointVector targetPoints,
    GArray* sortArray
  )
{
  guint i;
  for(i=0; i<targetPoints->len; i++)
    {
    g_array_index(targetPoints, Coordinates, i) = g_array_index(sortArray, TSortElementStruct, i).targetPoint;
    }
  g_array_free(sortArray, TRUE);
}




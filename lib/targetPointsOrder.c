/*
Order target points.
The order of synthesis affects the results:
first synthesizing target points near the context can give *better* results
but can also lead to artifacts: objects bleeding from the context into the target.
Originally, there was only one method of ordering: random over the entire target.
Added methods of ordering by distance from center.

TODO ordering by a thinning, or brushfire, algorithm, i.e. distance from context, not from center.

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

#include <glib.h>
#include <math.h>   // brushfire.h needs atan2

#include "coordinates.h"
#include "engineTypes2.h"  // PointVector, Pixelel
#include "bounds.h"   // Bounds, inlined

#include "engineParams.h"         // TImageSynthParameters
#include "imageSynthConstants.h"  // IMAGE_SYNTH_BAND_FRACTION

#include "targetPointsOrder.h"

// Declarations and definitions
#include "brushfire.h"



/* swap two elements of PointVector */
static inline void
swap_vector_elements(
  PointVector vector,
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


/*
Order vector of target pixels: shuffle randomly
This is the single, original method of randomizing.
*/
static void 
orderTargetPointsRandom(
  PointVector targetPoints,
  GRand *prng
  ) 
{
  guint i;

  g_debug ("%s", G_STRFUNC);

  for(i=0; i<targetPoints->len; i++)
  {
    guint j = g_rand_int_range(prng, 0, targetPoints->len);
    swap_vector_elements(targetPoints, targetPoints->len, i, j);
  }
}


/*
For image processing: randomize to avoid image artifacts, but retain some context from edges.
Generically: randomize a vector by shuffling from source nearby (within a band around the destination)
instead of shuffling from source anywhere in the vector.
Note that elements CAN move all the way to the back, but not vice versa: 
elements can only move the band size forward.
TODO another method of random bands that is symmetric.
*/
static void randomizeBandsTargetPoints(
  PointVector targetPoints,
  GRand *prng
  ) 
{
  gint last = targetPoints->len - 1;
  gint halfBand = targetPoints->len * IMAGE_SYNTH_BAND_FRACTION;
  gint i;

  g_debug ("%s", G_STRFUNC);
  
  for(i=0; i<=last; i++) 
  {
    // An interval of width halfBand*2 surrounding i, but lying in interval [0,last].
    // The band is only halfBand wide at the front and back of sweeping the vector.
    // E.G. [0, halfband] ...[i-halfband, i+halfband]... [last-halfband, last]
    // !!! Note use signed arithmetic, but the results is in the range [0,last].
    // TODO better bounds checking if small targetPoints
    gint bandStart = MAX(i-halfBand, 0);  // bandStart in [0, last-halfBand]
    gint bandEnd = MIN(i+halfBand, last); // bandEnd in [halfBand, last]
    gint bandSize = bandEnd - bandStart;
    gint j = bandStart + g_rand_int_range(prng, 0, bandSize);
    swap_vector_elements(targetPoints, targetPoints->len, i, j);
  }
}

#ifdef TODO
/*
This is an alternate version that doesn't move front elements indefinitely far to the back.
Seems to make no difference in practice.
*/
void randomizeBandsTargetPoints2(
  GRand *prng
  ) 
{
  gint last = targetPoints->len - 1;
  gint halfBand = targetPoints->len * IMAGE_SYNTH_BAND_FRACTION;
  gint i;
  
  GArray * moved;
  moved = g_array_sized_new(TRUE, TRUE, sizeof(gboolean), targetPoints->len);
  
  for(i=0; i<=last; i++) 
  {
    /* If the source is not the original (another has been moved already here already), skip */
    if ( g_array_index(moved, gboolean, i) )
      continue;
    
    gint bandStart = MAX(i-halfBand, 0);  // bandStart in [0, last-halfBand]
    gint bandEnd = MIN(i+halfBand, last); // bandEnd in [halfBand, last]
    gint bandSize = bandEnd - bandStart;
    gint j = bandStart + g_rand_int_range(prng, 0, bandSize);
    
    
    swap_vector_elements(targetPoints, targetPoints->len, i, j);
    /* Mark both sides moved. */
    g_array_index(moved,gboolean,i) = TRUE;
    g_array_index(moved,gboolean,j) = TRUE;
  }
}
#endif

/*
Coordinates to and from offsets.
Add or subtract a point (e.g. a center.)
*/
static void
targetPoints_to_offsets(
  Coordinates center,
  PointVector targetPoints
  )
{
  // Coords => offsets from center
  guint i;
  for(i=0; i<targetPoints->len; i++)
    // c++ targetPoints[i] = subtract_points(targetPoints[i], center);
    g_array_index(targetPoints, Coordinates, i) = 
      subtract_points(g_array_index(targetPoints, Coordinates, i), center);
}

static void
targetPoints_from_offsets(
  Coordinates center,
  PointVector targetPoints
  )
  {
  guint i;
  // offsets from center => Coords
  for(i=0; i<targetPoints->len; i++)
    g_array_index(targetPoints, Coordinates, i) = 
      add_points(g_array_index(targetPoints, Coordinates, i), center);
  }



/*
Order target points by 2D distance from target center, then randomize in bands.
Note the compare function changes the direction, eg inward, outward, horizontal, vertical, etc.
*/
static void 
orderTargetPointsRandomDirectional(
  // c++ bool (*compare) (const Coordinates, const Coordinates)
  gint (*compare)(const void*, const void*),
  PointVector targetPoints,
  GRand *prng
  ) 
{
  /*
  Get rough coords for the center.
  !!! Of the target, not the entire source context nor the corpus.
  The target, full source, and corpus can have different coordinate systems, need not be the same size,
  and may have different centers.
  */
  Coordinates center = get_center(targetPoints, targetPoints->len);
  
  targetPoints_to_offsets(center, targetPoints);
  
  // ascending or descending, outward or inward, concentric or linear, depending on compare function
  // c++ sort(targetPoints.begin(), targetPoints.end(), compare);  
  g_array_sort(targetPoints, compare);
    
  targetPoints_from_offsets(center, targetPoints);
  randomizeBandsTargetPoints(targetPoints, prng);
}





static void
orderTargetPointsRandomBrushfire(
  // c++ bool (*compare) (const Coordinates, const Coordinates)
  gint (*compare)(const void*, const void*),
  PointVector targetPoints,
  GRand *prng
  )
{
  Coordinates center = get_center(targetPoints, targetPoints->len);
  targetPoints_to_offsets(center, targetPoints);
  // Coordinates are now offsets.
  GArray* sortArray = targetPointsToSortArray(targetPoints);
  g_array_sort(sortArray, compare);  // outward or inward  compare function
  targetPointsFromSortArray(targetPoints, sortArray);
  targetPoints_from_offsets(center, targetPoints);
  randomizeBandsTargetPoints(targetPoints, prng);
}




/* 
Random squeeze: order the target points both directions, in and out by distance from center.
Used if the target is a donut, with context inside and outside.
*/
static void orderTargetPointsRandomSqueeze(
  PointVector targetPoints,
  GRand *prng
  )
{
  guint i;
  PointVector target_temp;
  
  Coordinates center = get_center(targetPoints, targetPoints->len);
  
  targetPoints_to_offsets(center, targetPoints); // Temporarily: Coords => to offsets from center
  GArray* sortArray = targetPointsToSortArray(targetPoints);
      
  // Algorithm: shuffle (one from each end alternatively, not random.)
  
  // Sort ascending on distance from center
  // c++ sort(targetPoints.begin(), targetPoints.end(), lessCartesian);   
  g_array_sort(sortArray, (gint (*)(const void*, const void*)) moreInward);
  
  // Copy targetPoints vector
  target_temp = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), targetPoints->len);
  
  for(i=0; i<targetPoints->len; i++)
    g_array_index(target_temp, Coordinates, i) = g_array_index(targetPoints, Coordinates, i);
  
  // Shuffle back into the original
  {
  guint pointCount = targetPoints->len;
  gint frontIndex = 0;
  gint backIndex = pointCount-1;
  
  i = 0;
  while (TRUE) 
    {
    // Copy from front.
    // c++ targetPoints[i] = targetTemp[frontIndex];
    g_array_index(targetPoints, Coordinates, i) = g_array_index(target_temp, Coordinates, frontIndex);
    i++;
    frontIndex++;
    if ( i>= pointCount) break;
    // Copy from back.
    // c++ targetPoints[i] = targetTemp[backIndex]; // Backwards
    g_array_index(targetPoints, Coordinates, i) = g_array_index(target_temp, Coordinates, backIndex);
    i++;
    backIndex--;
    if ( i>= pointCount) break;
    }
  }
  
  targetPointsFromSortArray(targetPoints, sortArray);
  targetPoints_from_offsets(center, targetPoints); // offsets => coordinates
  randomizeBandsTargetPoints(targetPoints, prng);
  g_array_free(target_temp, TRUE);
}

/*
Order the vector of target points in one of many ways
specified by parameter use_border.
*/
int 
orderTargetPoints(
  TImageSynthParameters* parameters,
  PointVector targetPoints,
  GRand *prng
  ) 
{
  switch (parameters->matchContextType) 
  {
    case 0: /* Random order, not using context in matches. */
    case 1: /* Random order, using context in matches. */
        orderTargetPointsRandom(targetPoints, prng);  
        break;
    case 2: /* Randomized bands, concentric, inward */
        orderTargetPointsRandomBrushfire(
          (gint (*)(const void*, const void*)) moreInward,
          targetPoints,
          prng
          );
        /* Formerly moreCartesian */
        break;
    case 3:
        orderTargetPointsRandomDirectional( 
          (gint (*)(const void*, const void*)) moreHorizontal,
          targetPoints,
          prng
          );
        // randomized bands, horizontally, inwards.  IE squeezing from top and bottom
        break;
    case 4:
        orderTargetPointsRandomDirectional( 
          (gint (*)(const void*, const void*)) moreVertical,
          targetPoints,
          prng
          );
        // randomized bands, vertically, inwards.  IE squeezing from sides.
        break;
    case 5:
        orderTargetPointsRandomBrushfire(
          (gint (*)(const void*, const void*)) lessInward,
          targetPoints,
          prng
          );
        // randomized bands, concentric, outward (eg for uncrop)
        break;
    case 6:
        orderTargetPointsRandomDirectional( 
          (gint (*)(const void*, const void*)) lessHorizontal,
          targetPoints,
          prng
          );
        // randomized bands, horizontally, outwards.   IE expanding to top and bottom
        break;
    case 7:
        orderTargetPointsRandomDirectional( 
          (gint (*)(const void*, const void*)) lessVertical,
          targetPoints,
          prng
          );
        // randomized bands, vertically, outwards.  IE expanding to sides
        break;
    case 8:
        orderTargetPointsRandomSqueeze(
          targetPoints,
          prng
          );   
        // randomized bands, concentric squeezing in and out a donut
        break;
    default:
        // no gimp: gimp_message("Parameter use_border out of range."); 
        // Critical, no i18n
        // return g_assert(FALSE);
        return IMAGE_SYNTH_ERROR_MATCH_CONTEXT_TYPE_RANGE;
  }
  return 0; // SUCCESS
}



// resynth-order-target.h

/*
Order target points.
The order of synthesis affects the results:
first synthesizing target points near the context can give *better* results
but can also lead to artifacts: objects bleeding from the context into the target.
Originally, there was only one method of ordering: random over the entire target.
Added methods of ordering by distance from center.
TODO ordering by a thinning, or brushfire, algorithm, i.e. distance from context, not from center.
*/


/*
Order vector of target pixels: shuffle randomly
This is the single, original method of randomizing.
*/
void 
orderTargetPointsRandom() 
{
  guint i;
  for(i=0; i<target_points_size; i++)
  {
    guint j = g_rand_int_range(prng, 0, target_points_size);
    swap_vector_elements(target_points, target_points_size, i, j);
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
void randomizeBandsTargetPoints() 
{
  gint last = target_points_size - 1;
  gint halfBand = target_points_size * RESYNTH_BAND_FRACTION;
  gint i;
  for(i=0; i<=last; i++) 
  {
    // An interval of width halfBand*2 surrounding i, but lying in interval [0,last].
    // The band is only halfBand wide at the front and back of sweeping the vector.
    // E.G. [0, halfband] ...[i-halfband, i+halfband]... [last-halfband, last]
    // !!! Note use signed arithmetic, but the results is in the range [0,last].
    // TODO better bounds checking if small target_points
    gint bandStart = MAX(i-halfBand, 0);  // bandStart in [0, last-halfBand]
    gint bandEnd = MIN(i+halfBand, last); // bandEnd in [halfBand, last]
    gint bandSize = bandEnd - bandStart;
    gint j = bandStart + g_rand_int_range(prng, 0, bandSize);
    swap_vector_elements(target_points, target_points_size, i, j);
  }
}

#ifdef TODO
/*
This is an alternate version that doesn't move front elements indefinitely far to the back.
Seems to make no difference in practice.
*/
void randomizeBandsTargetPoints2() 
{
  gint last = target_points_size - 1;
  gint halfBand = target_points_size * RESYNTH_BAND_FRACTION;
  gint i;
  
  GArray * moved;
  moved = g_array_sized_new(TRUE, TRUE, sizeof(gboolean), target_points_size);
  
  for(i=0; i<=last; i++) 
  {
    /* If the source is not the original (another has been moved already here already), skip */
    if ( g_array_index(moved, gboolean, i) )
      continue;
    
    gint bandStart = MAX(i-halfBand, 0);  // bandStart in [0, last-halfBand]
    gint bandEnd = MIN(i+halfBand, last); // bandEnd in [halfBand, last]
    gint bandSize = bandEnd - bandStart;
    gint j = bandStart + g_rand_int_range(prng, 0, bandSize);
    
    
    swap_vector_elements(target_points, target_points_size, i, j);
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
void
target_points_to_offsets(Coordinates center)
{
  // Coords => offsets from center
  guint i;
  for(i=0; i<target_points_size; i++)
    // c++ target_points[i] = subtract_points(target_points[i], center);
    g_array_index(target_points, Coordinates, i) = 
      subtract_points(g_array_index(target_points, Coordinates, i), center);
}

void
target_points_from_offsets(Coordinates center)
  {
  guint i;
  // offsets from center => Coords
  for(i=0; i<target_points_size; i++)
    g_array_index(target_points, Coordinates, i) = 
      add_points(g_array_index(target_points, Coordinates, i), center);
  }

/* Calc array of max distance along radii over all target points. */
static void
prepare_max_cartesian_along_ray()
{
  guint i;
  Coordinates center = get_center(target_points, target_points_size);
  
  for(i=0; i<401; i++)
    max_cartesian_along_ray[i] = 0;
  
  for(i=0; i<target_points_size; i++)
  {
    Coordinates point = g_array_index(target_points, Coordinates, i);
    Coordinates offset = subtract_points(point, center);
    guint cartesian = offset.x * offset.x + offset.y * offset.y;
    guint ray = grad(offset);
    if ( cartesian > max_cartesian_along_ray[ray] )
      max_cartesian_along_ray[ray] = cartesian; 
  }
  /* Not all radii in grad units will have points on them, i.e. may have 0 max cartesian */
  /* dump_max_grad(); */
}

/*
Order target points by 2D distance from target center, then randomize in bands.
Note the compare function changes the direction, eg inward, outward, horizontal, vertical, etc.
*/
void 
orderTargetPointsRandomDirectional(
  // c++ bool (*compare) (const Coordinates, const Coordinates)
  gint (*compare)(const void*, const void*)
  ) 
{
  /*
  Get rough coords for the center.
  !!! Of the target, not the entire source context nor the corpus.
  The target, full source, and corpus can have different coordinate systems, need not be the same size,
  and may have different centers.
  */
  Coordinates center = get_center(target_points, target_points_size);
  
  target_points_to_offsets(center);
  
  // ascending or descending, outward or inward, concentric or linear, depending on compare function
  // c++ sort(target_points.begin(), target_points.end(), compare);  
  g_array_sort(target_points, compare);
    
  target_points_from_offsets(center);
  randomizeBandsTargetPoints();
}




/* 
Random squeeze: order the target points both directions, in and out by distance from center.
Used if the target is a donut, with context inside and outside.
*/
void orderTargetPointsRandomSqueeze()
{
  guint i;
  pointVector target_temp;
  
  Coordinates center = get_center(target_points, target_points_size);
  
  prepare_max_cartesian_along_ray();
  
  target_points_to_offsets(center); // Temporarily: Coords => to offsets from center
      
  // Algorithm: shuffle (one from each end alternatively, not random.)
  
  // Sort ascending on distance from center
  // c++ sort(target_points.begin(), target_points.end(), lessCartesian);   
  g_array_sort(target_points, (gint (*)(const void*, const void*)) moreInward);
  
  // Copy target_points vector
  target_temp = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), target_points_size);
  
  for(i=0; i<target_points_size; i++)
    g_array_index(target_temp, Coordinates, i) = g_array_index(target_points, Coordinates, i);
  
  // Shuffle back into the original
  {
  guint pointCount = target_points_size;
  gint frontIndex = 0;
  gint backIndex = pointCount-1;
  
  i = 0;
  while (TRUE) 
    {
    // Copy from front.
    // c++ target_points[i] = targetTemp[frontIndex];
    g_array_index(target_points, Coordinates, i) = g_array_index(target_temp, Coordinates, frontIndex);
    i++;
    frontIndex++;
    if ( i>= pointCount) break;
    // Copy from back.
    // c++ target_points[i] = targetTemp[backIndex]; // Backwards
    g_array_index(target_points, Coordinates, i) = g_array_index(target_temp, Coordinates, backIndex);
    i++;
    backIndex--;
    if ( i>= pointCount) break;
    }
  }
  
  target_points_from_offsets(center); // offsets => coordinates
  randomizeBandsTargetPoints();
  g_array_free(target_temp, TRUE);
}

/*
Order the vector of target points in one of many ways
specified by parameter use_border.
*/
void order_target_points(
  Parameters *parameters
  ) 
{
  switch (parameters->use_border) 
  {
    case 0: /* Random order, not using context in matches. */
    case 1: /* Random order, using context in matches. */
        orderTargetPointsRandom();  
        break;
    case 2: /* Randomized bands, concentric, inward */
        prepare_max_cartesian_along_ray();
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) moreInward);
        /* Formerly moreCartesian */
        break;
    case 3:
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) moreHorizontal);
        // randomized bands, horizontally, inwards.  IE squeezing from top and bottom
        break;
    case 4:
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) moreVertical);
        // randomized bands, vertically, inwards.  IE squeezing from sides.
        break;
    case 5:
        prepare_max_cartesian_along_ray();
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) lessInward);
        // randomized bands, concentric, outward (eg for uncrop)
        break;
    case 6:
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) lessHorizontal);
        // randomized bands, horizontally, outwards.   IE expanding to top and bottom
        break;
    case 7:
        orderTargetPointsRandomDirectional( (gint (*)(const void*, const void*)) lessVertical);
        // randomized bands, vertically, outwards.  IE expanding to sides
        break;
    case 8:
        orderTargetPointsRandomSqueeze();   // randomized bands, concentric squeezing in and out a donut
        break;
    default:
        // FIXME no gimp: gimp_message("Parameter use_border out of range."); // Critical, no i18n
        g_assert(FALSE);
  }
}



// Match result kind
typedef enum  BettermentKindEnum 
{
  PERFECT_MATCH,  // Patches equal
  NO_BETTERMENT,  // Match worse than previous best
  GENERIC_BETTERMENT,
  NEIGHBORS_SOURCE,
  RANDOM_CORPUS,
  MAX_BETTERMENT_KIND
} tBettermentKind;

/* 
Neighbor (patch element)
Element of array of points from target image (selection and context).
Copied from target image for better memory locality.
*/
typedef struct neighborStruct {
  Pixelel pixel[MAX_RESYNTH_BPP] __attribute__((aligned(8))); // Copy of target pixel
  Coordinates offset;   // Offset from patch center
  // Coords of corpus point this target synthed from, or -1 if this neighbor is context
  Coordinates sourceOf; 
  } TNeighbor;


/*
Class neighbor_source
Similar to sourceOfMap target points, but for neigbhors.
*/

static inline gboolean
has_source_neighbor ( 
  guint j,
  const TNeighbor neighbors[]   // Index in neighbors array (the patch)
  )
{
  //c++ return neighbour_statuses[j]->has_source
  return neighbors[j].sourceOf.x != -1;
  // A neighbor only has a source if it is also in the target and has been synthed.
}


/* Copy the source of a neighbor point into the neighbor array. */
static inline void
set_neighbor_state (
  guint n_neighbour,          // index in neighbors
  Coordinates neighbor_point,  // coords in image (context or target)
  Map* sourceOfMap,
  TNeighbor neighbors[]
  ) 
{
  /* Assert neighbor point has values (we already checked that the candidate neighbor had a value.) */
  // c++: neighbour_statuses[n_neighbour] = status.at(neighbor_point);
  neighbors[n_neighbour].sourceOf = getSourceOf(neighbor_point, sourceOfMap);
}
  
/* Create a neighbor.  Initialize: offset, status, and pixel. */
static inline void
new_neighbor(
  const guint index,
  Coordinates offset,
  Coordinates neighbor_point,
  TFormatIndices* indices,
  Map* targetMap,
  Map* sourceOfMap,
  TNeighbor neighbors[]
  )
{
  neighbors[index].offset = offset;
  set_neighbor_state(index, neighbor_point, sourceOfMap, neighbors);
  // !!! Copy the whole Pixel, all the pixelels
  {
  TPixelelIndex k;
  for (k=0; k<indices->total_bpp; k++)
    // c++ neighbour_values[countNeighbors][k] = data.at(neighbor_point)[k];
    neighbors[index].pixel[k] = pixmap_index(targetMap, neighbor_point)[k];
  }
}


/*
Prepare array of neighbors with values, both inside the target, and outside i.e. in the context (if use_border).
Neighbors are in the source (the target or its context.)
If repeating a pixel, now might have more, different, closer neighbors than on the first pass.
Neighbours array is global, used both for heuristic and in synthing every point ( in try_point() )
Neighbours describes a patch, a shotgun pattern in the first pass, or a contiguous patch in later passes.
*/
static guint 
prepare_neighbors(
  Coordinates position, // IN target point
  TImageSynthParameters *parameters, // IN
  TFormatIndices* indices,
  Map* targetMap,
  Map* hasValueMap,
  Map* sourceOfMap,
  pointVector sortedOffsets,
  TNeighbor neighbors[]
  ) 
{
  guint j;
  guint count = 0;
  
  for(j=0; j<sortedOffsets->len; j++)
  {
    // c++ Coordinates offset = sortedOffsets[j];
    Coordinates offset = g_array_index(sortedOffsets, Coordinates, j);
    Coordinates neighbor_point = add_points(position, offset);

    // !!! Note side effects: wrap_or_clip might change neighbor_point coordinates !!!
    if (wrap_or_clip(parameters, targetMap, &neighbor_point)  // is neighbor in target image or wrappable
        &&  getHasValue(neighbor_point, hasValueMap)   // is neighbor outside target (context) 
            // or inside target with already synthed value
      ) 
    {
      new_neighbor(count, offset, neighbor_point, indices, targetMap, sourceOfMap, neighbors);
      count++;
      if (count >= (guint) parameters->patchSize) break;
    }
  }
  
  /*
  Note the neighbors are in order of distance from the target pixel.
  But the matching does not depend on the order, it only matters that neighbors are nearest.
  Experiment to sort the nearest neighbors in other orders, such as in row major order
  (so there might be better memory locality) didn't seem to help speed.
  
  Note we can't assert(countNeighbors==parameters.patchSize)
  If use_border, there is a full neighborhood unless context or corpus small, that is, 
  there are usually plenty of distant neighbors in the context and corpus.
  If not use_border, there is a full neighborhood except for first n_neighbor synthesis tries.
  */
  return count;
}





  
/*
This is the inner crux: comparing target patch to corpus patch, pixel by pixel.
Also the bottleneck in performance.

The following discussion depends on how repetition (passes) are configured:
if the first pass is not a complete pass over the target, it doesn't apply.
On the first pass the candidate patch might be a shotgun pattern, to distant context.
On subsequent passes, the candidate patch is a rectangular pixmap (since the target is filled in.)
But since pixels can be masked, the actual patch tested might be irregularly shaped.

Note that size of patch (n_neighbors) is usually the same for each target pixel, 
but in rare cases, it might not be.
(If there is no context, the first probe has 0 neighbors, the second probe 1 neighbor, ...)
Then does it make sense to also use MAX_WEIGHT for missing neighbors?
*/
static inline gboolean // TODO tBettermentKind, but very subtle 
try_point(
  const Coordinates point, 
  const TFormatIndices * const indices,
  const Map * const corpusMap,
  guint * const bestPatchDiff,  // OUT
  Coordinates * const bestMatchCorpusPoint, // OUT
  const guint countNeighbors,
  const TNeighbor const neighbors[],
  tBettermentKind* latestBettermentKind,
  const tBettermentKind bettermentKind
  ) 
{
  guint sum = 0;
  guint i; 
  
#ifdef STATS
  countSourceTries++;
#endif
  // Iterate over neighbors of candidate point. Sum grows as more neighbors tested.
  for(i=0; i<countNeighbors; i++)
  {
    Coordinates off_point = add_points(point, neighbors[i].offset);
    if (clippedOrMaskedCorpus(off_point, corpusMap)) 
    {    
      /* Maximum difference for this neighbor outside corpus */
      sum += MAX_WEIGHT*indices->img_match_bpp + map_diff_table[0]*indices->map_match_bpp;   
    } 
    else  
    {
      const Pixelel * corpus_pixel;
      const Pixelel * image_pixel;
      
      corpus_pixel = pixmap_index(corpusMap, off_point);
      // ! Note target pixel comes not from targetPoints, but from copy neighbors
      image_pixel = neighbors[i].pixel; // pixel is array, yields Pixelel pointer
      #ifndef VECTORIZED
      /* If not the target point (its own 0th neighbor).
      !!! On the first pass, the target point as its own 0th neighbor has no meaningful, unbiased value.
      Even if e.g. we initialize target to all black, that biases the search.
      */
      if (i) 
      {
        TPixelelIndex j;
        for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
          sum += diff_table[256u + image_pixel[j] - corpus_pixel[j]];
      }
      if (indices->map_match_bpp > 0)
      {
        TPixelelIndex j;
        for(j=indices->map_start_bip; j<indices->map_end_bip; j++)  // also sum mapped difference
          sum += map_diff_table[256u + image_pixel[j] - corpus_pixel[j]];
      }
      #else
      const Pixelel * __restrict__ corpus_pixel = pixmap_index(&corpus, off_point);
      const Pixelel  * __restrict__ image_pixel = neighbour_values[i];
      #define MMX_INTRINSICS_RESYNTH
      #include "resynth-vectorized.h"
      #endif
    }
 
    /* 
    lkk !!! bestMatchCorpusPoint not set.
    Note: equals.
    If this source is same as prior source for target or different from prior source
    ( whether picked randomly or for a repeat) 
    AND all neighbors checked (iteration completed) without finding a lesser bestPatchDiff (but maybe an equal bestPatchDiff)
    bestMatchCorpusPoint is not changed even if it is a different source.
    ??? Study how many different but equal sources are found.  
    Are different source in later repeats closer distance?
    */
    if (sum >= *bestPatchDiff) return FALSE;  // !!! Short circuit for neighbors
  }

  // Assert sum strictly < bestPatchDiff
  *bestPatchDiff = sum;
  *latestBettermentKind = bettermentKind;
  
  // bestMatchCorpusPoint might already equal point, but might be smaller sum because different neighbors or different neighbor values
  *bestMatchCorpusPoint = point;
  if (sum <=0) 
  {
#ifdef STATS
    bettermentStats[PERFECT_MATCH]+=1;
#endif
    return TRUE;  // PERFECT_MATCH
  }
  else 
    return FALSE; // GENERIC_BETTERMENT;
}


/*
The heart of the algorithm.
Called repeatedly: many passes over the data.
*/
static guint
synthesize(
  guint pass,
  TImageSynthParameters *parameters, // IN,
  TRepetionParameters repetition_params,
  TFormatIndices* indices,
  Map * targetMap,
  Map* corpusMap,
  Map* recentProberMap,
  Map* hasValueMap,
  Map* sourceOfMap,
  pointVector targetPoints,
  pointVector corpusPoints,
  pointVector sortedOffsets,
  GRand *prng
  )
{
  guint target_index;
  Coordinates position;
  guint repeatCountBetters = 0;
  
  tBettermentKind latestBettermentKind; // matchResult;
  gboolean isPerfectMatch = FALSE;
  
  // Best match in this pass search for a matching patch.
  guint bestPatchDiff;   
  Coordinates bestMatchCorpusPoint = {0,0};
  
  /* 
  Neighbors  (patch)
  Copied from source image for better memory locality.
  */
  // TODO this is large and allocated on the stack
  TNeighbor neighbors[RESYNTH_MAX_NEIGHBORS];
  guint countNeighbors = 0;
  
  /* ALT: count progress once at start of pass countTargetTries += repetition_params[pass][1]; */
  reset_color_change();
  
  for(target_index=0; target_index<repetition_params[pass][1]; target_index++) 
  {
#ifdef STATS
    countTargetTries += 1;
#endif
    
    #ifdef PROGRESS
    if ((target_index&4095) == 0) 
    {
      /* Progress over all passes, not just within this pass.
      Towards the maximum expected tries, but we might omit latter passes.
      */
      gimp_progress_update((float)countTargetTries/total_targets);
      #ifdef ANIMATE
        post_results_to_gimp(drawable);
      #endif
    }
    #endif
    
    position = g_array_index(targetPoints, Coordinates, target_index);
     
    /*
    This means we are about to give it a value (and a source),
    but also means that below, we put offset (0,0) in vector of neighbors !!!
    i.e. this makes a target point it's own neighbor (with a source in later passes.)
    */
    setHasValue(&position, TRUE, hasValueMap);  
    
    countNeighbors = prepare_neighbors(position, parameters, indices, 
      targetMap, hasValueMap, sourceOfMap, sortedOffsets,
      neighbors
      );
    
    /*
    Repeat a pixel even if found an exact match last pass, because neighbors might have changed.
    
    On passes after the first, we don't explicitly start with best of the previous match,
    but since a pixel is it's own first neighbor, the first best calculated will a be
    from the source that gave the previous best, and should be a good starting best.
    */
    bestPatchDiff = G_MAXUINT; /* A very large positive number.  Was: 1<<30 */       
    // matchResult = NO_BETTERMENT;
    isPerfectMatch = FALSE;
    latestBettermentKind = NO_BETTERMENT;
    /*
    Heuristic 1, try neighbors of sources of neighbors of target pixel.
    
    Subtle: The target pixel is its own first neighbor (offset 0,0).
    It also has_value (since we set_has_value() above, but it really doesn't have color on the first pass.)
    On the first pass, it has no source.
    On subsequent passes, it has a source and thus its source is the first corpus point to be probed again,
    and that will set bestPatchDiff to a low value!!!
    */
    {
    guint neighbor_index;
    
    // TODO check for zero here is redundant
    for(neighbor_index=0; neighbor_index<countNeighbors && bestPatchDiff != 0; neighbor_index++)
      // If the neighbor is in the target (not the context) and has a source in the corpus
      if ( has_source_neighbor(neighbor_index, neighbors) ) {
        /*
        Coord arithmetic: corpus source minus neighbor offset.
        corpus_point is a pixel in the corpus with opposite offset to corpus source of neighbor
        as target position has to this target neighbor.
        !!! Note corpus_point is raw coordinate into corpus: might be masked.
        !!! It is not an index into unmasked corpusPoints.
        */
        Coordinates corpus_point = subtract_points(neighbors[neighbor_index].sourceOf, 
          neighbors[neighbor_index].offset);
        
        /* !!! Must clip corpus_point before further use, its only potentially in the corpus. */
        if (clippedOrMaskedCorpus(corpus_point, corpusMap)) continue;
        if (*intmap_index(recentProberMap, corpus_point) == target_index) continue; // Heuristic 2
        isPerfectMatch = try_point(corpus_point, indices, corpusMap,
          &bestPatchDiff, &bestMatchCorpusPoint,
          countNeighbors, neighbors, 
          &latestBettermentKind, NEIGHBORS_SOURCE
          );
        // TODO stats: if bettered, is kind NEIGHBORS_SOURCE 
        // if ( matchResult == PERFECT_MATCH ) break;  // Break neighbors loop
        if ( isPerfectMatch ) break;  // Break neighbors loop
        /*
        !!! Remember we probed corpus pixel point for target point target_index.
        Heuristic 2: all target neighbors with values might come from the same corpus locus.
        */
        *intmap_index(recentProberMap, corpus_point) = target_index;
      }
      // Else the neighbor is not in the target (has no source) so we can't use the heuristic 1.
    }
      
    // if ( matchResult != PERFECT_MATCH )
    if ( ! isPerfectMatch )
    {
      /* Try random source points from the corpus */
      gint j;
      for(j=0; j<parameters->maxProbeCount; j++)
      {
        isPerfectMatch = try_point(randomCorpusPoint(corpusPoints, prng), 
          indices, corpusMap,
          &bestPatchDiff, &bestMatchCorpusPoint,
          countNeighbors, neighbors,
          &latestBettermentKind, RANDOM_CORPUS
          );
        if ( isPerfectMatch ) break;  /* Break loop over random corpus points */
        // if ( matchResult == PERFECT_MATCH ) break;  /* Break loop over random corpus points */
        // Not set recentProberMap(point) since heuristic rarely works for random source.
        // TODO if bettered is kind RANDOM_CORPUS
      }
    }
    
    store_betterment_stats(matchResult);
    /* DEBUG dump_target_resynthesis(position); */
    
    /*
    Store best match.
    Compared to match from a previous pass:
     The best match may be no better.
     The best match may be the same source point.
     The best match may be the same color from a different source point.
     The best match may be the same source but a better match because the patch changed.
    These are all independent.
    We distinguish some of these cases: only store a better matching, new source.
    */
    // if (matchResult != NO_BETTERMENT )
    if (latestBettermentKind != NO_BETTERMENT )
    {
      /* if source different from previous pass */
      if ( ! equal_points(getSourceOf(position, sourceOfMap), bestMatchCorpusPoint) ) 
      {
        repeatCountBetters++;   /* feedback for termination. */
        integrate_color_change(position); // Must be before we store the new color values.
        /* Save the new color values (!!! not the alpha) for this target point */
        {
          TPixelelIndex j;
          
          // For all color pixelels (channels)
          for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
            // Overwrite prior with new color
            pixmap_index(targetMap, position)[j] = 
              pixmap_index(corpusMap, bestMatchCorpusPoint)[j];  
        }
        setSourceOf(position, bestMatchCorpusPoint, sourceOfMap); /* Remember new source */
      } /* else same source for target */
    } /* else match is same or worse */
  } /* end for each target pixel */
  return repeatCountBetters;
}


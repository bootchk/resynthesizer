/*
Innermost routines of image synthesis.

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


#ifdef VECTORIZED

#include <mmintrin.h> // intrinsics for assembly language MMX op codes, for sse2 xmmintrin.h
#endif

#include <pthread.h>

pthread_mutex_t mutex;

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
Is point in the target image or wrapped into it.

!!! Note it can have side effects, wrapping the point into the target image when tiling.

If tiling, wrap the coords into the pixmap (and return True.)
Otherwise, return whether the resulting coords are in target pixmap (whether not clipped.)
!!! See elsewhere, tiling is only pertinent if matchContext is False.

IN image parameter is the target and context pixmap
IN/OUT point parameter is a neighbor (target point plus offset). 
Point can be outside the pixmap, requiring clipping or wrapping.
Here clipping means: return false if outside the pixmap.

Note this is not in the innermost, bottleneck.
TODO It makes the engine more capable,
at the cost of slightly slowing down the most frequent use: healing to matchContext.
*/
static inline gboolean 
clipToTargetOrWrapIfTiled (
  const TImageSynthParameters *parameters,   
  Map *image,
  Coordinates *point // !!! IN/OUT
  )
{ 
  while(point->x < 0)
    if (parameters->isMakeSeamlesslyTileableHorizontally)
      point->x += image->width;
    else
      return FALSE;
  
  while(point->x >= (gint) image->width)
    if (parameters->isMakeSeamlesslyTileableHorizontally)
      point->x -= image->width;
    else
      return FALSE;
  
  while(point->y < 0)
    if (parameters->isMakeSeamlesslyTileableVertically)
      point->y += image->height;
    else
      return FALSE;
  
  while(point->y >= (gint) image->height)
    if (parameters->isMakeSeamlesslyTileableVertically)
      point->y -= image->height;
    else
      return FALSE;

  return TRUE;
}

/* 
Neighbor (patch element)
Element of array of points from target image (selection and context).
Copied from target image for better memory locality.
*/
typedef struct neighborStruct {
  Pixelel pixel[MAX_IMAGE_SYNTH_BPP] __attribute__((aligned(8))); // Copy of target pixel
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
  pthread_mutex_lock(&mutex);  // Set color and source atomically
  set_neighbor_state(index, neighbor_point, sourceOfMap, neighbors);
  // !!! Copy the whole Pixel, all the pixelels
  {
  TPixelelIndex k;
  for (k=0; k<indices->total_bpp; k++)
    neighbors[index].pixel[k] = pixmap_index(targetMap, neighbor_point)[k];
  }
  pthread_mutex_unlock(&mutex);
}


/*
Prepare patch (array of neighbors) with values, both inside the target, and outside i.e. in the context (if use_border).
Neighbors are in the source (the target or its context.)
If repeating a pixel, now might have more, different, closer neighbors than on the first pass.
Neighbors array is global, used both for heuristic and in synthing every point ( in computeBestFit() )
Neighbors describes a patch, a shotgun pattern in the first pass, or a contiguous patch in later passes.
It is stored in an array, but is not necessarily a square, contiguous patch.
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
  Coordinates offset;
  Coordinates neighbor_point;
  
  // Target point is always its own first neighbor, even though on startup and first pass it doesn't have a value.
  offset = g_array_index(sortedOffsets, Coordinates, 0);
  new_neighbor(count, offset, position, indices, targetMap, sourceOfMap, neighbors);
  count++;
  
  for(j=1; j<sortedOffsets->len; j++) // !!! Start at 1
  {
    offset = g_array_index(sortedOffsets, Coordinates, j);
    neighbor_point = add_points(position, offset);

    // !!! Note side effects: clipToTargetOrWrapIfTiled might change neighbor_point coordinates !!!
    if (clipToTargetOrWrapIfTiled(parameters, targetMap, &neighbor_point)  // is neighbor in target image or wrappable
        &&  getHasValue(neighbor_point, hasValueMap)   
          // AND ( is neighbor outside target (context) OR inside target with already synthed value )
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
  
  Note we can't assert(count==parameters.patchSize)
  If use_border, there is a full neighborhood unless context or corpus small, that is, 
  there are usually plenty of distant neighbors in the context and corpus.
  If not use_border, there is a full neighborhood except for first n_neighbor synthesis tries
  on the very first pass.
  */
  return count;
}




  
/*
This is the inner crux: comparing target patch to corpus patch, pixel by pixel.
Also the bottleneck in performance.

Computing a best fit metric, with early out when exceed known best.

Because of a log transform of a product, this is a summing.

The following discussion depends on how repetition (passes) are configured:
if the first pass is not a complete pass over the target, it doesn't apply.
On the first pass the candidate patch might be a shotgun pattern, to distant context.
On subsequent passes, the candidate patch is often a rectangular pixmap (since the target is filled in.)
But since pixels can be masked, the actual patch tested might be irregularly shaped.

Note that size of patch (n_neighbors) is usually the same for each target pixel, 
but in rare cases, it might not be.
(If there is no context, the first probe has 0 neighbors, the second probe 1 neighbor, ...)
Then does it make sense to also use MAX_WEIGHT for missing neighbors?
*/
static inline gboolean // TODO tBettermentKind, but very subtle 
computeBestFit(
  const Coordinates point, 
  const TFormatIndices * const indices,
  const Map * const corpusMap,
  guint * const bestPatchDiff,  // OUT
  Coordinates * const bestMatchCorpusPoint, // OUT
  const guint countNeighbors,
  const TNeighbor const neighbors[],
  tBettermentKind* latestBettermentKind,
  const tBettermentKind bettermentKind,
  const TPixelelMetricFunc corpusTargetMetric,  // array pointers
  const TMapPixelelMetricFunc mapsMetric
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
      /* 
      Maximum weighted difference for this neighbor outside corpus.
      !!! Note even if no maps are passed to engine, we weight by the map,
      for this case of an invalid corpus point.
      !!! Note the mapsMetric function is not scaled, 
      so we can't use a constant such as MAX_MAP_DIFF,'
      but instead mapMetric[...], the extreme max value of the metric.
      !!! Which will be zero if mapWeight parameter is zero.
      */
      #ifdef SYMMETRIC_METRIC_TABLE
      // mapsMetric[256] is the max
      sum += MAX_WEIGHT*indices->img_match_bpp + mapsMetric[LIMIT_DOMAIN]*indices->map_match_bpp;
      #else
      sum += MAX_WEIGHT*indices->img_match_bpp + mapsMetric[0]*indices->map_match_bpp;
      #endif
    } 
    else  
    {
#ifndef VECTORIZED
      // Iterate over color pixelels to compute weighted difference
      const Pixelel * corpus_pixel;
      const Pixelel * image_pixel;
      #ifdef SYMMETRIC_METRIC_TABLE
      gshort diff;
      #endif
      
      corpus_pixel = pixmap_index(corpusMap, off_point);
      // ! Note target pixel comes not from targetPoints, but from copy neighbors
      image_pixel = neighbors[i].pixel; // pixel is array, yields Pixelel pointer
      

      /* If not the target point (its own 0th neighbor).
      !!! On the first pass, the target point as its own 0th neighbor has no meaningful, unbiased value.
      Even if e.g. we initialize target to all black, that biases the search.
      */
      if (i) 
      {
        TPixelelIndex j;
        for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
        {
          #ifdef SYMMETRIC_METRIC_TABLE
          diff = (gshort) image_pixel[j] - (gshort) corpus_pixel[j];
          sum += corpusTargetMetric[ ((diff < 0) ? (-diff) : (diff)) ];
          // OR sum += corpusTargetMetric[ abs(diff) ]; // abs() a macro? from stddef.h
          #else
          sum += corpusTargetMetric[ 256u + image_pixel[j] - corpus_pixel[j] ];
          #endif
        }
      }
      if (indices->map_match_bpp > 0) // If maps
      {
        TPixelelIndex j;
        for(j=indices->map_start_bip; j<indices->map_end_bip; j++)  // also sum mapped difference
        {
          #ifdef SYMMETRIC_METRIC_TABLE
          diff = (gshort) image_pixel[j] - (gshort) corpus_pixel[j];
          sum += mapsMetric[ ((diff < 0) ? (-diff) : (diff)) ];
          // sum += mapsMetric[ abs(diff) ];
          #else
          sum += mapsMetric[256u + image_pixel[j] - corpus_pixel[j]];
          #endif
        }
      }
#else
      const Pixelel * __restrict__ corpus_pixel = pixmap_index(corpusMap, off_point);
      const Pixelel  * __restrict__ image_pixel = neighbors[i].pixel;
      #define MMX_INTRINSICS_RESYNTH
      #include "resynth-vectorized.h"
#endif
      /*
      !!! Very subtle: on the very first pass and very first target point, with no context,
      the patch is only one point, the being synthesized pixel.
      Above, the loop will execute exactly once.
      At "if (i)", it will not compute a weighted difference.
      Hence the sum will be zero, i.e. a perfect match, and the very first probe will be the best match.
      In other words, it will be completely at random, with no actual searching.
      */
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


static inline void
setColor(
  TFormatIndices* indices,
  Map* targetMap,
  Coordinates targetPosition,
  Map* corpusMap,
  Coordinates corpusPosition
  )
{
  TPixelelIndex j;
  
  // For all color pixelels (channels)
  for(j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)
    // Overwrite prior with new color
    pixmap_index(targetMap, targetPosition)[j] = 
      pixmap_index(corpusMap, corpusPosition)[j];  
}


/*
The heart of the algorithm.
Called repeatedly: many passes over the data.
*/
static guint
synthesize(
  TImageSynthParameters *parameters,  // IN
  gboolean isStartup,
  guint startTargetIndex,  // IN
  guint endTargetIndex,  // IN
  TFormatIndices* indices,  // IN
  Map * targetMap,      // IN/OUT
  Map* corpusMap,       // IN
  Map* recentProberMap, // IN/OUT
  Map* hasValueMap,     // IN/OUT
  Map* sourceOfMap,     // IN/OUT
  pointVector targetPoints, // IN
  pointVector corpusPoints, // IN
  pointVector sortedOffsets, // IN
  GRand *prng,
  TPixelelMetricFunc corpusTargetMetric,  // array pointers
  TMapPixelelMetricFunc mapsMetric
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
  TNeighbor neighbors[IMAGE_SYNTH_MAX_NEIGHBORS];
  guint countNeighbors = 0;
  
  /* ALT: count progress once at start of pass countTargetTries += repetition_params[pass][1]; */
  reset_color_change();
  
  // On startup, set the color of the first target pixel to a random color
  // The colors aren't matched anyway
  /*
  if (isStartup)
    setColor( indices, targetMap, position, corpusMap, bestMatchCorpusPoint);
  */
    
  for(target_index=startTargetIndex; target_index<endTargetIndex; target_index++) 
  {
#ifdef STATS
    countTargetTries += 1;
#endif
    
    #ifdef DEEP_PROGRESS
    if ((target_index&4095) == 0) 
    {
      /* Progress over all passes, not just within this pass.
      Towards the maximum expected tries, but we might omit latter passes.
      */
      // gimp_progress_update((float)countTargetTries/total_targets);
      progressUpdate(0, (void*) 0); 
    }
    #endif
    
    position = g_array_index(targetPoints, Coordinates, target_index);
     
    /*
    This means we are about to give it a value (and a source),
    but also means that below, we put offset (0,0) in vector of neighbors !!!
    i.e. this makes a target point it's own neighbor (with a source in later passes.)
    */
    /* */
    // setHasValue(&position, TRUE, hasValueMap);
    
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
    In other words, continue building this region of the target
    from the corpus region (continuation) where neighbors of the target pixel came from.
    
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
      // If the neighbor is in the target (not the context) and has a source in the corpus (already synthesized.)
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
        isPerfectMatch = computeBestFit(corpus_point, indices, corpusMap,
          &bestPatchDiff, &bestMatchCorpusPoint,
          countNeighbors, neighbors, 
          &latestBettermentKind, NEIGHBORS_SOURCE,
          corpusTargetMetric, mapsMetric
          );
        // TODO stats: if bettered, is kind NEIGHBORS_SOURCE 
        // if ( matchResult == PERFECT_MATCH ) break;  // Break neighbors loop
        if ( isPerfectMatch ) break;  // Break neighbors loop
        /*
        !!! Remember we probed corpus pixel point for target point target_index.
        Heuristic 2: all target neighbors with values might come from the same corpus locus,
        called a "continuation" in Harrison's thesis.
        */
        *intmap_index(recentProberMap, corpus_point) = target_index;
      }
      // Else the neighbor is not in the target (has no source) so we can't use the heuristic 1.
    }
      
    // if ( matchResult != PERFECT_MATCH )
    if ( ! isPerfectMatch )
    {
      /* 
      Match patches at random source points from the corpus.
      In later passes, many will be earlyouts.
      */
      gint j;
      for(j=0; j<parameters->maxProbeCount; j++)
      {
        isPerfectMatch = computeBestFit(randomCorpusPoint(corpusPoints, prng), 
          indices, corpusMap,
          &bestPatchDiff, &bestMatchCorpusPoint,
          countNeighbors, neighbors,
          &latestBettermentKind, RANDOM_CORPUS,
          corpusTargetMetric, mapsMetric
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
        pthread_mutex_lock(&mutex);
        setColor( indices, targetMap, position, corpusMap, bestMatchCorpusPoint);
        setSourceOf(position, bestMatchCorpusPoint, sourceOfMap); /* Remember new source */
        pthread_mutex_unlock(&mutex);
      } /* else same source for target */
    } /* else match is same or worse */
    setHasValue(&position, TRUE, hasValueMap);
  } /* end for each target pixel */
  return repeatCountBetters;
}


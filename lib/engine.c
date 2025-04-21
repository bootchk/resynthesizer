/*
  A texture synthesizing engine for bitmapped images.

  The algorithm is due to Paul Harrison and others.

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
Inner engine.
Parameters include target and corpus pixmaps.
Pixmaps have internal pixel format, which includes mask, color, alpha, and map.
(where map means an additional e.g. grayscale value for mapping corpus to target.)
Target and corpus pixmaps have the same pixel format.
(An adapter can relax the restriction that both have the same format,
by adding e.g. an alpha channel to one.)
*/

/*
Notes:

The selection:

A caller of the engine may be an application supporting "selection."
Then a caller should adapt the selection mask to separate masks for the target and corpus.
Generally, in such a case, the masks should generally be mutually exclusive,
especially regarding partial selection.
But this is not a concern of the engine.

The alpha:

In prior versions the alpha was treated like a color channel, and matched during synthesis.
Transparent pixels (which Gimp arbitrarily gives the color black in some circumstances)
were not distinguished.  In certain cases  with transparency, transparent pixels were synthesized
into the target, as good matches for black.

Here, we don't match the alpha channel between target and corpus.
We don't generate any alpha in the target, instead we leave the target alpha unaltered.
We use the alpha to determine what pixels are in the target and corpus,
(similar to a selection mask.)
Any totally transparent pixel in the target selection IS synthesized,
I.E. a color is generated (but since it is totally transparent, you don't see it.)
Any partially transparent target pixel is also synthesized, except as stated,
the alpha is not matched (so colors from opaque areas of the corpus
could be synthesized into partially transparent areas of the target.)
Any totally transparent pixel in the corpus is not in the corpus, i.e. never matched.
Any partially transparent pixel in the corpus is a candidate for matching.
A color from a partially transparent pixel in the corpus could be synthesized
into an opaque area of the target.
Again, the transparency of the target is retained even as new colors are synthesized.

Tiling: (see parameters horizontal and vertical tiling)
This means we synthesize a target that is *seamlessly* tileable.
We treat the target as a sphere, wrapping a coord outside the target around
to the opposite side.  See wrapOrClipTarget.
It doesn't make tiles in the target, it makes a target that is suitable as a tile.
*/

#include "../resynth-config.h"

#include <math.h>

#ifdef SYNTH_USE_GLIB
  #include <glib.h>
#else
  /* Use a proxy for GLib. */
  #include <stddef.h>   // size_t
  #include "glibProxy.h"  // Redefines the glib structs and routines used here
  #include <math.h> // atan2(), log()
  /*
  More proxy.  Redefine GRand routines
  On platform Linux, used Glib g_rand
  On platform OSX (when using stdc but not Glib), proxy calls stdc rand()
  */
  #define g_rand_new_with_seed(s) s_rand_new_with_seed(s)
  #define g_rand_int_range(r,u,l) s_rand_int_range(r,u,l)
#endif

#include "engineTypes2.h"

// Count threads to start.
#ifdef SYNTH_THREADED
  // A reasonable guess that most current processors have no more than this.
  // More threads than cores does not seem to hurt performance.
  // glib doesn't seem to support knowing the count of threads.
  #define THREAD_LIMIT    12
#else
  // Must be defined to 1 if not threaded, it affects how synthesize() iterates over target
  #define THREAD_LIMIT 1
#endif



/* Shared with resynth-gui, engine plugin, and engine */
#include "imageSynthConstants.h"

// True header files
#include "imageFormat.h"
#include "imageFormatIndicies.h"

// Order is important 
#include "coordinates.h"
#include "map.h"
#include "engineParams.h"
#include "engine.h"
#include "targetPixels.h"
#include "stats.h"  // For debugging, statistics
#include "targetPointsOrder.h"


/*
Source not compiled separately. Is separate to reduce file sizes and coupling.
Also, some functions are inlined.
*/

#include "mapIndex.h" // inlined, used in innermost loop
#include "matchWeighting.h"





/*
Class hasValue

Whether a pixel in the image is ready to be matched.
Value more or less means a color; not an undefined color.
Pixels in the context: if they are not clipped, not transparent, etc.
Pixels in the target: if they have been synthesized.

TODO
Alternate way of computing hasValue from targetMask, alpha channel, and whether synthesized (has_source.)
Might use less memory and have better locality.
But hasValue is only called in prepare_neighbors, not as costly as rest of search.

TODO hasValue array is larger than it needs to be?
It could it be just the size of the target plus a band, since it is only used for neighbors.
Would require different wrapOrClipTarget().
Might affect performance of cache or memory swapping
*/

static inline void
setHasValue( Coordinates *coords, guchar value, Map* hasValueMap)
{
  *bytemap_index(hasValueMap, *coords) = value;
}

static inline gboolean
getHasValue(Coordinates coords, Map* hasValueMap)
{
  return (* bytemap_index(hasValueMap, coords));
}

static inline void
prepareHasValue(Map* targetMap, Map* hasValueMap)
{
  new_bytemap(hasValueMap, targetMap->width, targetMap->height);
}


/*
Class sourceOfMap

Whether a target pixel has a source in the corpus (from synthesis).
TODO Possibly we can use index into corpusPoints instead of coordinates.
TODO The map only needs to be the size of the target?
But we are calling getSourceOf(neighbor_point), which can be points outside
of the target (context), whose source is always themselves.
However, the extra memory is probably not a resource problem,
and probably not a performance problem because it is only used in prepare_neighbors,
the source are copied to a dense structure neighbor_sources for the inner search.
*/


static inline void
setSourceOf (
  Coordinates target_point,
  Coordinates source_corpus_point,
  Map* sourceOfMap
  )
{
  *coordmap_index(sourceOfMap, target_point) = source_corpus_point;
}


static inline Coordinates
getSourceOf (
  Coordinates target_point,
  Map* sourceOfMap
  )
{
  return *coordmap_index(sourceOfMap, target_point);
}


/* Initially, no target points have source in corpus, i.e. none synthesized. */
static void
prepare_target_sources(
  Map* targetMap,
  Map* sourceOfMap)
{
  guint x;
  guint y;
  Coordinates null_coords = {-1, -1};

  new_coordmap(sourceOfMap, targetMap->width, targetMap->height);

  for(y=0; y<targetMap->height; y++)
    for(x=0; x<targetMap->width; x++)
      {
      Coordinates coords = {x,y};
      setSourceOf(coords, null_coords, sourceOfMap);
      }
}


#include "selection.h"

/*
Return whether this pixel has any opacity.
If it has some opacity (not totally transparent) than it contributes to the visible.
Our strategy is to synthesize target pixels with any opacity,
and use context pixels with any opacity.
*/
static inline gboolean
not_transparent_image(
  Coordinates coords,
  TFormatIndices* indices,
  Map * targetMap
  )
{
  return ( indices->isAlphaTarget ? pixmap_index(targetMap, coords)[indices->alpha_bip] != ALPHA_TOTAL_TRANSPARENCY : TRUE);
}

static inline gboolean
not_transparent_corpus(
  Coordinates coords,
  TFormatIndices* indices,
  Map* corpusMap
  )
{
  return ( indices->isAlphaSource ? pixmap_index(corpusMap, coords)[indices->alpha_bip] != ALPHA_TOTAL_TRANSPARENCY : TRUE);
}


/* Included here because it depends on some routines above. */
// If STATS is not defined, it redefines stat function calls to nil
#include "stats.h"


/*
Array of index of most recent target point that probed this corpus point
(recentProberMap[corpus x,y] = target)
!!! Larger than necessary if the corpus has holes in it.  TODO very minor.
!!! Note recentProberMap is unsigned, -1 == 0xFFFFFF should not match any target index.
*/
static void
prepareRecentProber(Map* corpusMap, Map* recentProberMap)
{
  guint x;
  guint y;

  new_intmap(recentProberMap, corpusMap->width, corpusMap->height);
  for(y=0; y< (guint) corpusMap->height; y++)
    for(x=0; x< (guint) corpusMap->width; x++)
    {
      Coordinates coords = {x,y};
      *intmap_index(recentProberMap, coords) = -1;
    }
}


/*
Prepare target AND initialize hasValueMap.
This is misnamed and is really two concerns: the target (what is synthesized)
and the context (the surroundings.)
Both come from the target image.  But the *target* is not *target image*.
Prepare a vector of target points.
Initialize hasValueMap for all target points.
*/
static void
prepareTargetPoints(
  gboolean is_use_context,
  TFormatIndices* indices,
  Map* targetMap,
  Map* hasValueMap,
  PointVector* targetPoints
  )
{
  guint x;
  guint y;

  /* Count selected pixels in the image, for sizing a vector */
  guint size = 0;
  for(y=0; y<targetMap->height; y++)
    for(x=0; x<targetMap->width; x++)
      {
      Coordinates coords = {x,y};
      if (isSelectedTarget(coords, targetMap))
        size++;
      }

  *targetPoints = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), size); /* reserve */

  prepareHasValue(targetMap, hasValueMap);  /* reserve, initialize to value: unknown */

  for(y=0; y<targetMap->height; y++)
    for(x=0; x<targetMap->width; x++)
    {
      Coordinates coords = {x,y};

      /*
      Remember whether use this image point for matching target neighbors.
      Initially, no target points have value, and some context points will have value.
      Later, synthesized target points will have values also.
      */
      setHasValue(&coords,
        (
          is_use_context // ie use_border ie match image neighbors outside the selection (the context)
          && ! isSelectedTarget(coords, targetMap)  // outside the target
          /* !!! and if the point is not transparent (e.g. background layer) which is arbitrarily black !!! */
          && not_transparent_image(coords, indices, targetMap)
        ),
        hasValueMap);

      /*
      Make vector targetPoints
      !!! Note we do NOT exclude transparent.  Will synthesize color (but not alpha)
      for all selected pixels in target, regardless of transparency.
      */
      if (isSelectedTarget(coords, targetMap))
        g_array_append_val(*targetPoints, coords);
    }
}




/*
Scan corpus pixmap for selected && nottransparent pixels, create vector of coords.
Used to sample corpus.
*/
void
prepareCorpusPoints (
  TFormatIndices* indices,
  Map* corpusMap,
  PointVector* corpusPoints
  )
{
  /* Reserve size of pixmap, but excess, includes unselected. */
  *corpusPoints = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates),
   corpusMap->height*corpusMap->width);

  {
  guint x;
  guint y;

  for(y=0; y<corpusMap->height; y++)
    for(x=0; x<corpusMap->width; x++)
    {
      Coordinates coords = {x, y};
      /* In prior versions, the user's mask was inverted to establish the corpus,
      I.E. this was "not is_selected"
      */
      if (isSelectedCorpus(coords, corpusMap)
        && not_transparent_corpus(coords, indices, corpusMap) /* Exclude transparent from corpus */
        )
      {
        g_array_append_val(*corpusPoints, coords);
      }
    }
  }
  // Size is checked by caller.
}


static inline Coordinates
randomCorpusPoint (
  PointVector corpusPoints,
  GRand * prng
  )
{
  /* Was rand()%corpusPoints_size but thats not uniform. */
  gint index = g_rand_int_range(prng, 0, corpusPoints->len);
  return g_array_index(corpusPoints, Coordinates, index);
}




/*
Vector of offsets to surrounding points of a point, i.e. to a patch around a point.
Used as candidates to compute neighbors vector (nearby points that are selected and with values.)
Which is used in two places: 1) neighbor heuristic 2) try_point.

Sorted ascending on distance from (0,0) (see the < operator for class Coordinates).

!!! Note that offset 0,0 included, is the first element in sorted vector.
That makes a point it's own neighbor, i.e. part of the patch for (surrounding) a point.

Spans twice the min of corpus and image (target).
Why?  So from one corner, the offsets will reach fully across.

TODO, for uncropping, where the target surrounds the corpus,
this might be vastly many more offsets than are needed for good synthesis.
But at worst, if not used they get paged out from virtual memory.
*/
static void
prepareSortedOffsets(
  Map* targetMap,
  Map* corpusMap,
  PointVector* sortedOffsets
  )
{
  // Minimum().  Use smaller dimension of corpus and target.
  gint width = (corpusMap->width < targetMap->width ? corpusMap->width : targetMap->width);
  gint height = (corpusMap->height < targetMap->height ? corpusMap->height : targetMap->height);
  guint allocatedSize = (2*width-1)*(2*height-1);   // eg for width==3, [-2,-1,0,1,2], size==5

  *sortedOffsets = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), allocatedSize); //Reserve

  {
  gint x; // !!! Signed offsets
  gint y;

  for(y=-height+1; y<height; y++)
    for(x=-width+1; x<width; x++)
      {
      Coordinates coords = {x,y};
      g_array_append_val(*sortedOffsets, coords);
      }
  }
  g_assert((*sortedOffsets)->len == allocatedSize);  // Completely filled
  g_array_sort(*sortedOffsets, (gint (*)(const void*, const void*)) lessCartesian);

  /* lkk An experiment to sort the offsets in row major order for better memory
  locality didn't help performance.
  Apparently the cpu cache holds many rows of the corpus.
  */
}


/*
Return True if point is clipped or masked (not selected) in the corpus.
Point created by coordinate arithmetic, and can be negative or clipped.
!!! Note this is called in the bottleneck of try_point, crucial to speed.
*/
static inline gboolean
clippedOrMaskedCorpus(
  const Coordinates point,
  const Map * const corpusMap)
{
  return (
    point.x < 0
    || point.y < 0
    || point.x >= (gint) corpusMap->width
    || point.y >= (gint) corpusMap->height /*  Clipped */
    ||  ! isSelectedCorpus(point, corpusMap) /* Masked */
    );
}


// Included source (function declarations, not just definitions.)
// Descending levels of the engine
// imageSynth()->engine()->refiner()->synthesize
#include "passes.h"
#include "progress.h"
#include "synthesize.h"
// Both files define the same function refiner()
#ifdef SYNTH_THREADED
  #include "refinerThreaded.h"
#else
  #include "refiner.h"
#endif

/*
The engine.
Independent of platform, calling app, and graphics libraries.
This is mostly preparation: real work done by refiner() and synthesize().
*/

int
engine(
  TImageSynthParameters parameters,
  TFormatIndices* indices,
  Map* targetMap,
  Map* corpusMap,
  void (*progressCallback)(int, void*),
  void *contextInfo,
  int *cancelFlag
  )
{
  // Engine private data. On stack (and heap), not global, so engine is reentrant.

  /*
  A map on the corpus yielding indexes of target points.
  For a point in the corpus, which target point (index!) most recently probed the corpus point.
  recentProbe[coords corpus point] -> index of target_point that most recently probed corpus point
  Heuristic#2.

  2-D array of int, addressable by Coordinates.
  */
  Map recentProberMap;

  /*
  Flags for state of synthesis of image pixels.

  Does source pixel have value yet, to match (depends on selection and state of algorithm.)
  Map over entire target image (target selection and context.)
  */
  Map hasValueMap;

  /*
  Does this target pixel have a source yet: yields corpus coords.
  (-1,-1) indicates no source.
  */
  Map sourceOfMap;

  /*
  1-D array (vector) of Coordinates.
  Subsets of image and corpus, subsetted by selection and alpha.
  */
  PointVector targetPoints;   // For synthesizing target in an order (ie random)
  PointVector corpusPoints;   // For sampling corpus randomly.
  PointVector sortedOffsets;  // offsets (signed coordinates) for finding neighbors.

  GRand *prng;  // pseudo random number generator

  // Arrays, lookup tables for quantized functions
  TPixelelMetricFunc corpusTargetMetric;
  TMapPixelelMetricFunc mapMetric;

  // check parameters in range
  if ( parameters.patchSize > IMAGE_SYNTH_MAX_NEIGHBORS)
    return IMAGE_SYNTH_ERROR_PATCH_SIZE_EXCEEDED;

  // Create an ordered list of points in the target
  prepareTargetPoints(parameters.matchContextType, indices, targetMap,
    &hasValueMap,
    &targetPoints);
  
  // Initialize the pixels of the target, depending on mode.
  prepareTargetPixels (indices->colorEndBip, *targetMap);
  
  /*
  Rare user error: no target selected (mask empty.)
  This error NOT occur in GIMP if selection does not intersect, since then we use the whole drawable.
  */
  if ( !targetPoints->len )
  {
    g_array_free(targetPoints, TRUE);
    free_map(&hasValueMap);
    return IMAGE_SYNTH_ERROR_EMPTY_TARGET;
  }

  // Initialize the source of, i.e. coords of, best matches of target points
  prepare_target_sources(targetMap, &sourceOfMap);


  // source prep
  prepareCorpusPoints(indices, corpusMap, &corpusPoints);
  /*
  Rare user error: all corpus pixels transparent or not selected (mask empty.) Which means we can't synthesize.
  This error NOT occur in GIMP if selection does not intersect, since then we use the whole drawable.
  */
  if (!corpusPoints->len )
  {
    g_array_free(targetPoints, TRUE);
    free_map(&hasValueMap);
    free_map(&sourceOfMap);
    g_array_free(corpusPoints, TRUE);
    return IMAGE_SYNTH_ERROR_EMPTY_CORPUS;
  }

  // prep things not images
  prepareSortedOffsets(targetMap, corpusMap, &sortedOffsets); // Depends on image size
  quantizeMetricFuncs(
    parameters.sensitivityToOutliers,
    parameters.mapWeight,
    corpusTargetMetric,
    mapMetric
    );

  // Now we need a prng, before order_targetPoints
  /* Originally: srand(time(0));   But then testing is non-repeatable.
  TODO the seed should be a hash of the input or a user parameter.
  Then it would be repeatable, but changeable by the user.
  */
  prng = g_rand_new_with_seed(1198472);

  int error = orderTargetPoints(&parameters, targetPoints, prng);
  // A programming error that we don't clean up.
  if (error) return error;

  prepareRecentProber(corpusMap, &recentProberMap);  // Must follow prepare_corpus

  // Preparations done, begin actual synthesis
  print_processor_time();

  // Commented out, no longer works since progress() is not visible in engine
  // progress(_("Resynthesizer: synthesizing"));

  refiner(
    parameters,
    indices,
    targetMap,
    corpusMap,
    &recentProberMap,
    &hasValueMap,
    &sourceOfMap,
    targetPoints,
    corpusPoints,
    sortedOffsets,
    prng,
    corpusTargetMetric,
    mapMetric,
    progressCallback,
    contextInfo,
    cancelFlag
    );

  // Free internal mallocs.
  // Caller must free the IN pixmaps since the targetMap holds synthesis results
  free_map(&recentProberMap);
  free_map(&hasValueMap);
  free_map(&sourceOfMap);

  g_array_free(targetPoints, TRUE);
  g_array_free(corpusPoints, TRUE);
  g_array_free(sortedOffsets, TRUE);

  #ifdef SYNTH_USE_GLIB
  g_rand_free(prng);
  #endif

  return 0; // Success, even if canceled
}



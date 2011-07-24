/*
  The Resynthesizer - A GIMP plug-in for resynthesizing textures
  
  Copyright (C) 2010  Lloyd Konneker
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
 
/*
Notes:

The selection:

In prior versions, you could pass the same layer as the target and corpus.
Since there is only one selection, the selection was the target and the inverse
of the selection was the corpus.  But if you wanted to pass a different image and layer
as the corpus, you needed to invert the selection in that image.

This feature was a source of confusion for users and programmers.
Here, this feature is abolished.  The selection in the corpus layer is the corpus,
not the inverse of the selection.

This only eliminates one use: synthesizing a selection from the inverse of the selection
in the same drawable.  If you need to do that, copy the drawable to another image and
create a selection there that is the inverse of the selection in the original.
The heal selection plugin does that for you.

The alpha:

In prior versions the alpha was treated like a color channel, and matched during resynthesis.
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
to the opposite side.  See wrap_or_clip.
It doesn't make tiles in the target, it makes a target that is suitable as a tile.
*/

// Compiling switch #defines
#include "buildSwitches.h"


// #include <glib/gprintf.h>

// Bring in alternative code: experimental, debugging, etc.
// #define ANIMATE    // Animate image while processing, for debugging.
// #define DEBUG

/*
Uncomment this before release.  
I don't think the GNU build environment disables assertions on the command line to gcc.
Also, uncomment when using splint.
Leave it commented for development and testing, to enable assertions.
*/
// #define G_DISABLE_ASSERT      // To disable g_assert macro, uncomment this.

#ifdef SYNTH_USE_GLIB
  #include "../config.h" // GNU buildtools local configuration
#endif

/*
On platform Linux, used Glib grand so results are consistent across test invocations.
On platform OSX (when using stdc but not Glib), use stdc rand()
*/

#ifdef SYNTH_USE_GLIB
  // Use glib via gimp.h
  #include <libgimp/gimp.h>
#endif
  
#ifdef USE_GLIB_PROXY
  #include <stddef.h>   // size_t
  // This immediately redefines all but a few glib structs
  #include "glibProxy.h"
  #include <math.h> // atan2(), log()
  // More proxy.  Redefines all but GRand struct
  #define g_rand_new_with_seed(s) s_rand_new_with_seed(s)
  #define g_rand_int_range(r,u,l) s_rand_int_range(r,u,l)
#endif

/* Shared with resynth-gui, engine plugin, and engine */
#include "resynth-constants.h"

// True header files
#include "imageFormat.h"
#include "map.h"
#include "mapIndex.h"
#include "engineParams.h"
#include "engine.h"

/* Source not compiled separately. Is separate to reduce file sizes and later, coupling. */
#include "resynth-types.h"
#include "resynth-map-types.h"
#include "matchWeighting.h"


/*
Whether using an alpha channel.
!!! Complicated.  The user might want to synthesize a transparent target,
but they probably don't want to synthesize a new transparency.
Transparent areas in the corpus are problematic:
Totally transparent areas in the corpus should not match anything since color is black (Gimp default).
Partially transparent areas might have SOME user provided color to match,
but what the user sees is not what we might be matching against, is that what user intended?
*/
// TODO every adapter must set these.  The Resynthesizer does.  The SimpleAPI doesn't yet.  They default to TRUE for prototype.
//gboolean is_alpha_image = TRUE;
//gboolean is_alpha_corpus = TRUE;

// clock_t start_time;

#ifdef STATS
/* Stats */
guint countSourceTries = 0;
guint countTargetTries = 0;
guint total_targets = 0;  /* Total target attempts over all passes, barring short circuit. */

// For statistics, remember the most recent kind of corpus point that bettered the previous best
guint bettermentStats[MAX_BETTERMENT_KIND];
#endif

/* 
Macro to cleanup for abort. 
Return value is already PDB_ERROR.
Return an error string to Gimp, which will display an alert dialog.
Also log message in case engine is called non-interactively.
Note this must be used in the scope of nreturn_vals and value, in main().
*/
#define ERROR_RETURN(message)   { \
  detach_drawables(drawable, corpus_drawable, map_in_drawable, map_out_drawable); \
  *nreturn_vals           = 2; \
  values[1].type          = GIMP_PDB_STRING; \
  values[1].data.d_string = message ; \
  g_debug(message); \
  return; \
  }



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
Would require different wrap_or_clip().
Might affect performance of cache or memory swapping
*/

static inline void
setHasValue( Coordinates *coords, guchar value, Map* hasValueMap)
{
  // c++ *hasValueMap.at(coords) = value;
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
  // c++ status.at(position)->has_source = TRUE;
  // c++ status.at(position)->source = source;
  *coordmap_index(sourceOfMap, target_point) = source_corpus_point;
}
  

static inline Coordinates
getSourceOf ( 
  Coordinates target_point,
  Map* sourceOfMap
  )
  {
  // c++ return *sourceOfMap.at(target_point);
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

static inline gboolean
has_source (
  Coordinates target_point,
  Map* sourceOfMap
  )
{
  //c++ return status.at(target_point)->has_source;
  return (getSourceOf(target_point, sourceOfMap).x != -1) ;
}




/* 
Is the pixel selected in the corpus? 
!!! Note dithered, partial selection: only one value is totally unselected or selected.
Here, only pixels fully selected return True.
This is because when target/corpus are differentiated by the same selection,
partially selected will be in the target,
fully selected in inverse? will be the corpus.
!!! Note this is called from the bottleneck.
TODO still allow same selection 
*/
static inline gboolean
is_selected_corpus ( 
  const Coordinates coords,
  const Map* const corpusMap
  )
{
  /*
  Formerly used a separate bitmap for corpus_mask.
  Now use our interleaved copy of the mask for better memory locality. 
  */
  /* 
  Was:  != MASK_UNSELECTED); i.e. partially selected was included.
  Now: if partially selected, excluded from corpus.
  */
  return (pixmap_index(corpusMap, coords)[MASK_PIXELEL_INDEX] == MASK_TOTALLY_SELECTED);
  }


/* 
Is the pixel selected in the image? 
Distinguishes target from context.
*/
static inline gboolean
is_selected_image ( 
  Coordinates coords,
  Map * targetMaskMap )
{
  return (*bytemap_index(targetMaskMap, coords) != MASK_UNSELECTED);
}


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
#include "resynth-stats.h"
// Included here because depends on global variables
#include "resynth-order-target.h"

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
  Map* targetMaskMap,
  Map* hasValueMap,
  pointVector* targetPoints
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
      if (is_selected_image(coords, targetMaskMap)) 
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
          && ! is_selected_image(coords, targetMaskMap)  // outside the target
          /* !!! and if the point is not transparent (e.g. background layer) which is arbitrarily black !!! */
          && not_transparent_image(coords, indices, targetMap)
        ),
        hasValueMap);
      
      /* 
      Make vector targetPoints 
      !!! Note we do NOT exclude transparent.  Will synthesize color (but not alpha)
      for all selected pixels in target, regardless of transparency.
      */
      if (is_selected_image(coords, targetMaskMap)) 
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
  pointVector* corpusPoints
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
      I.E. this was not is_selected
      */
      if (is_selected_corpus(coords, corpusMap)
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
  pointVector corpusPoints,
  GRand * prng
  )
{
  /* Was rand()%corpusPoints_size but thats not uniform. */
  gint index = g_rand_int_range(prng, 0, corpusPoints->len);
  return g_array_index(corpusPoints, Coordinates, index);
}



/*
Is point in the source image or wrapped into it.

!!! Note it can have side effects, wrapping the point into the target image when tiling.

If tiling, wrap the coords into the pixmap (and return True.)
Otherwise, return whether the resulting coords are in the pixmap (whether not clipped.)

IN image parameter is the target and context pixmap
IN/OUT point parameter is a neighbor (target point plus offset). The offset potentially needs clipping.

In the original c++, all parameters were passed by reference.
*/
static inline gboolean 
wrap_or_clip (
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
  pointVector* sortedOffsets
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
Return True if clipped or masked (not selected.) 
!!! Note this is called in the bottleneck of try_point, crucial to speed.
*/
static inline gboolean 
clippedOrMaskedCorpus(
  const Coordinates point, 
  const Map * const corpusMap) 
{
  return (
    point.x < 0 || point.y < 0 
    || point.x >= (gint) corpusMap->width 
    || point.y >= (gint) corpusMap->height /*  Clipped */
    ||  ! is_selected_corpus(point, corpusMap) /* Masked */
    ); 
}


// FIXME defined here but called from resynthesizer adapter
// #include "imageFormat.h"



// Engine
#include "passes.h"
#include "synthesize.h"
#include "refiner.h"

/*
The engine.
Independent of platform, calling app, and graphics libraries.

Temporarily, uses globals set by adapters.
*/

int
engine(
  TImageSynthParameters parameters,
  TFormatIndices* indices,
  Map* targetMap,
  Map* corpusMap,
  Map* targetMaskMap
  )
{
  // Engine private data. On stack (and heap), not global, so engine is reentrant.
  
  /*
  A map on the corpus yielding indexes of target points.
  For a point in the corpus, which target point (index!) most recently probed the corpus point.
  recentProbe[coords corpus point] -> index of target_point that most recently probed corpus point

  2-D array of int, addressable by Coordinates.
  c++: static Bitmap<guint> recentProber;
  */    
  Map recentProberMap;
  
  /*
  Flags for state of synthesis of image pixels.
  c++ static Bitmap<Status> status;
  */     
  /* 
  Does source pixel have value yet, to match (depends on selection and state of algorithm.)
  Map over entire target image (target selection and context.)
  */
  Map hasValueMap;
  /* Does this target pixel have a source yet: yields corpus coords. */
  Map sourceOfMap;   

  /* 
  1-D array (vector) of Coordinates.
  Subsets of image and corpus, subsetted by selection and alpha.
  */
  pointVector targetPoints;   // For synthesizing target in an order (ie random)
  pointVector corpusPoints;   // For sampling corpus randomly.
  pointVector sortedOffsets;  // offsets (signed coordinates) for finding neighbors.
  
  GRand *prng;  // pseudo random number generator
  
  
  // check parameters in range
  if ( parameters.patchSize > RESYNTH_MAX_NEIGHBORS)
    return IMAGE_SYNTH_ERROR_PATCH_SIZE_EXCEEDED;
  
  // target prep
  prepareTargetPoints(parameters.matchContextType, indices, targetMap, 
    targetMaskMap, &hasValueMap, &targetPoints);
  #ifdef ANIMATE
  clear_target_pixels(indices->color_end_bip);  // For debugging, blacken so new colors sparkle
  #endif
  free_map(targetMaskMap); // Assert not used later
  prepare_target_sources(targetMap, &sourceOfMap);
  
  // source prep
  prepareCorpusPoints(indices, corpusMap, &corpusPoints);

  /* 
  Rare user error: all pixels transparent (which we ignore.)
  This error NOT occur if selection does not intersect, since then we use the whole drawable.
  */
  if (!corpusPoints->len )
  {
    return IMAGE_SYNTH_ERROR_EMPTY_CORPUS;
  }
  if ( !targetPoints->len ) 
  {
    return IMAGE_SYNTH_ERROR_EMPTY_TARGET;
  }
  
  // prep unrelated to images
  prepareSortedOffsets(targetMap, corpusMap, &sortedOffsets); // Depends on image size
  make_diff_table(parameters.sensitivityToOutliers, parameters.mapWeight);
 
  // Now we need a prng, before order_targetPoints
  /* Originally: srand(time(0));   But then testing is non-repeatable. 
  TODO the seed should be a hash of the input or a user parameter.
  Then it would be repeatable, but changeable by the user.
  */
  prng = g_rand_new_with_seed(1198472);
  
  orderTargetPoints(&parameters, targetPoints, prng);
  prepareRecentProber(corpusMap, &recentProberMap);  // Must follow prepare_corpus
  
  // Preparations done, begin actual synthesis
  print_processor_time();
  
  // progress(_("Resynthesizer: synthesizing"));

  refiner(
    // targetDrawable, // ANIMATE
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
    prng
    );
    
  // Free all but the IN pixmaps
  // Caller must free the IN pixmaps since the targetMap holds synthesis results
  free_map(&recentProberMap);
  /*
    &hasValueMap,
    &sourceOfMap,
  */
  
  return 0; // Success
}



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
#include "map.h"
#include "mapIndex.h"
#include "engineParams.h"
#include "engine.h"

/* Source not compiled separately. Is separate to reduce file sizes and later, coupling. */
#include "resynth-types.h"
#include "resynth-map-types.h"
#include "matchWeighting.h"


BppType color_end_bip; /* Index of last color pixelels in target/context image. */
BppType alpha_bip;      /* Index of target alpha pixelel */
BppType map_start_bip;  /* Index of map pixelels */
BppType map_end_bip;

BppType img_match_bpp; /* bpp to match in image. */
BppType map_match_bpp; /* bpp to match in map. */
BppType total_bpp;     /* Total pixelels */


/* 
Local copies of pixmaps (not using gimp regions.) 
2-D arrays of Pixel, addressable by Coordinates (Point).
c++: static Bitmap<Pixelel>
*/
Map image;         /* Entire image, includes selection (target) and context (non-target) */
Map corpus;        /* Source.  Might be distinct from image. */
Map image_mask;    /* Selection channel for image */
Map corpus_mask;   /* Selection channel for corpus */


/*
2-D array of int, addressable by Coordinates.
c++: static Bitmap<guint> tried;
*/    
static Map tried;   /* tried[coords of corpus point] -> index of target_points that most recently tried corpus point */


/*
Flags for state of synthesis of image pixels.
c++ static Bitmap<Status> status;
*/     
static Map has_value;   /* Whether to match a source pixel (depends on selection and state of algorithm.) */
static Map source_of;   /* Whether this target pixel has a source yet, and the source coords. */


/* 
1-D array (vector) of Coordinates.
These are subsets of image and corpus, subsetted by selection and alpha,
and a vector of offsets.
*/
static pointVector target_points;   /* For synthesizing target in a particular order (ie random) */
static pointVector corpus_points;   /* For sampling corpus randomly. */
static pointVector sorted_offsets;  /* For finding neighbors. */

guint target_points_size = 0;
guint corpus_points_size = 0;
guint sorted_offsets_size = 0;


/* 
Neighbors  (patches) 
An array of points from the source image.
Copied from the source image for better memory locality.
*/
static Coordinates neighbours[RESYNTH_MAX_NEIGHBORS];   // !!! Offsets to neighbors
static Pixelel neighbour_values[RESYNTH_MAX_NEIGHBORS][MAX_RESYNTH_BPP] __attribute__((aligned(8)));  
Coordinates neighbour_source[RESYNTH_MAX_NEIGHBORS];

static guint n_neighbours;

/* Data about the best match in the search for a matching patch. */
static guint best;        /* Best patch diff over all search. */
static Coordinates best_point;


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
gboolean is_alpha_image = TRUE;
gboolean is_alpha_corpus = TRUE;

// clock_t start_time;

GRand *prng;

/* Stats */
guint countSourceTries = 0;
guint countTargetTries = 0;
guint total_targets = 0;  /* Total target attempts over all passes, barring short circuit. */

// Ways of finding a better corpus point
typedef enum  BettermentKindEnum 
{
  NO_BETTERMENT,
  NEIGHBORS_SOURCE,
  RANDOM_CORPUS,
  PERFECT_MATCH,
  MAX_BETTERMENT_KIND
} tBettermentKind;


// For statistics, remember the most recent kind of corpus point that bettered the previous best
tBettermentKind latestBettermentKind;
guint bettermentStats[MAX_BETTERMENT_KIND];


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
Class has_value

Whether a pixel in the image is ready to be matched.
Value more or less means a color; not an undefined color.
Pixels in the context: if they are not clipped, not transparent, etc.
Pixels in the target: if they have been synthesized.

TODO 
Alternate way of computing has_value from image_mask, alpha channel, and whether synthesized (has_source.)
Might use less memory and have better locality.
But has_value is only called in prepare_neighbors, not as costly as rest of search.

TODO has_value array is larger than it needs to be? 
It could it be just the size of the target plus a band, since it is only used for neighbors.
Would require different wrap_or_clip().
Might affect performance of cache or memory swapping
*/

static inline void
set_has_value( Coordinates *coords, guchar value)
{
  // c++ *has_value.at(coords) = value;
  *bytemap_index(&has_value, *coords) = value;
}

static inline gboolean
get_has_value(Coordinates coords)
{
  return (* bytemap_index(&has_value, coords));
}

static inline void
prepare_has_value()
{
  new_bytemap(&has_value, image.width, image.height);
}


/*
Class source_of

Whether a target pixel has a source in the corpus (from synthesis).
TODO Possibly we can use index into corpus_points instead of coordinates.
TODO The map only needs to be the size of the target?
But we are calling get_source_of(neighbor_point), which can be points outside
of the target (context), whose source is always themselves.
However, the extra memory is probably not a resource problem,
and probably not a performance problem because it is only used in prepare_neighbors,
the source are copied to a dense structure neighbor_sources for the inner search.
*/


static inline void
set_source (
  Coordinates target_point,
  Coordinates source_corpus_point
  )
{
  // c++ status.at(position)->has_source = TRUE;
  // c++ status.at(position)->source = source;
  *coordmap_index(&source_of, target_point) = source_corpus_point;
}
  

static inline Coordinates
get_source_of ( 
  Coordinates target_point
  )
  {
  // c++ return *source_of.at(target_point);
  return *coordmap_index(&source_of, target_point);
  }
  

/* Initially, no target points have source in corpus, i.e. none synthesized. */
static void
prepare_target_sources()
{
  guint x;
  guint y;
  Coordinates null_coords = {-1, -1};
  
  new_coordmap(&source_of, image.width, image.height);
  
  for(y=0; y<image.height; y++)
    for(x=0; x<image.width; x++) 
      {
      Coordinates coords = {x,y}; 
      set_source(coords, null_coords);
      }
}

static inline gboolean
has_source (
  Coordinates target_point
  )
{
  //c++ return status.at(target_point)->has_source;
  return (get_source_of(target_point).x != -1) ;
}


/*
Class neighbor_source
Similar to source_of target points, but for neigbhors.
*/


static inline gboolean
has_source_neighbor ( 
  guint j   // Index in neighbors array (the patch)
  )
{
  //c++ return neighbour_statuses[j]->has_source
  return neighbour_source[j].x != -1;
  // A neighbor only has a source if it is also in the target and has been synthed.
}


/* Copy the source of a neighbor point into the neighbor array. */
static inline void
set_neighbor_state (
  guint n_neighbour,          // index in neighbors
  Coordinates neighbor_point  // coords in image (context or target)
  ) 
{
  /* Assert neighbor point has values (we already checked that the candidate neighbor had a value.) */
  // c++: neighbour_statuses[n_neighbour] = status.at(neighbor_point);
  neighbour_source[n_neighbour] = get_source_of(neighbor_point);
}





/* 
Is the pixel selected in the corpus? 
!!! Note dithered, partial selection: only one value is totally unselected or selected.
Here, only pixels fully selected return True.
This is because when target/corpus are differentiated by the same selection,
partially selected will be in the target,
fully selected in inverse? will be the corpus.
TODO still allow same selection 
*/
static inline gboolean
is_selected_corpus ( Coordinates coords )
{
  /* Formerly used a separate bitmap for corpus_mask:
  c++ return (corpus_mask.at(coords)[0] == MASK_SELECTED);
  return (*bitmap_index(&corpus_mask, coords) != MASK_UNSELECTED);
  */
  
  /* Use our interleaved copy of the mask for better memory locality. */
  /* Was:  != MASK_UNSELECTED); */
  return (pixmap_index(&corpus, coords)[MASK_PIXELEL_INDEX] == MASK_TOTALLY_SELECTED);
  }


/* 
Is the pixel selected in the image? 
Distinguishes target from context.
*/
static inline gboolean
is_selected_image ( Coordinates coords )
{
  return (*bytemap_index(&image_mask, coords) != MASK_UNSELECTED);
}


/*
Return whether this pixel has any opacity.
If it has some opacity (not totally transparent) than it contributes to the visible.
Our strategy is to synthesize target pixels with any opacity,
and use context pixels with any opacity.
*/
static inline gboolean
not_transparent_image(Coordinates coords)
{
  return ( is_alpha_image ? pixmap_index(&image, coords)[alpha_bip] != ALPHA_TOTAL_TRANSPARENCY : TRUE);
}

static inline gboolean
not_transparent_corpus(Coordinates coords)
{
  return ( is_alpha_corpus ? pixmap_index(&corpus, coords)[alpha_bip] != ALPHA_TOTAL_TRANSPARENCY : TRUE);
}



/* Included here because it depends on some routines above. */
#include "resynth-stats.h"
// Included here because depends on global variables
#include "resynth-order-target.h"

/*
Array of index of most recent target point tried for this corpus point (tried[corpus x,y] = target)
!!! Larger than necessary if the corpus has holes in it.  TODO very minor.
!!! Note tried is unsigned, -1 == 0xFFFFFF should not match any target index.
*/
static void
prepare_tried()
{
  guint x;
  guint y;
  
  new_intmap(&tried, corpus.width, corpus.height);
  for(y=0; y< (guint) corpus.height; y++)
    for(x=0; x< (guint) corpus.width; x++)
    {
      Coordinates coords = {x,y};
      *intmap_index(&tried, coords) = -1;
    } 
}


/*
Prepare target AND initialize has_value.
This is misnamed and is really two concerns: the target (what is synthesized)
and the context (the surroundings.)
Both come from the target image.  But the *target* is not *target image*.
Prepare a vector of target points.
Initialize has_value for all image points.
*/
static void
prepare_target_points( gboolean is_use_context)
{
  guint x;
  guint y;
  
  /* Count selected pixels in the image, for sizing a vector */
  target_points_size = 0;
  for(y=0; y<image.height; y++)
    for(x=0; x<image.width; x++)
      {
      Coordinates coords = {x,y};
      if (is_selected_image(coords)) 
        target_points_size++;
      }
        
  target_points = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), target_points_size); /* reserve */
  
  prepare_has_value();  /* reserve, initialize to value: unknown */
  
  for(y=0; y<image.height; y++)
    for(x=0; x<image.width; x++) 
    {
      Coordinates coords = {x,y};
      
      /*
      Remember whether use this image point for matching target neighbors.
      Initially, no target points have value, and some context points will have value.
      Later, synthesized target points will have values also.
      */
      set_has_value(&coords,
        (
          is_use_context // ie use_border ie match image neighbors outside the selection (the context)
          && ! is_selected_image(coords)  // outside the target
          /* !!! and if the point is not transparent (e.g. background layer) which is arbitrarily black !!! */
          && not_transparent_image(coords)
        ));
      
      /* 
      Make vector target_points 
      !!! Note we do NOT exclude transparent.  Will synthesize color (but not alpha)
      for all selected pixels in target, regardless of transparency.
      */
      if (is_selected_image(coords)) 
        g_array_append_val(target_points, coords);
    }
}




/* Scan corpus pixmap for selected pixels, create vector of coords */
void
prepare_corpus_points () 
{
  /* Reserve size of pixmap, but excess, includes unselected. */
  corpus_points = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), corpus.height*corpus.width);
  
  corpus_points_size = 0;
  
  {
  guint x;
  guint y;
  
  for(y=0; y<corpus.height; y++)
    for(x=0; x<corpus.width; x++)
    {
      Coordinates coords = {x, y};
      /* In prior versions, the user's mask was inverted to establish the corpus,
      I.E. this was not is_selected
      */
      if (is_selected_corpus(coords)
        && not_transparent_corpus(coords) /* Exclude transparent from corpus */
        ) 
      {
        corpus_points_size++;
        g_array_append_val(corpus_points, coords);
      }
    }
  }
  // Size is checked by caller. 
}




static inline Coordinates
random_corpus_point ()
{
  /* Was rand()%corpus_points_size but thats not uniform. */
  gint index = g_rand_int_range(prng, 0, corpus_points_size);
  return g_array_index(corpus_points, Coordinates, index);
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
  const Parameters *parameters,   
  Map *image,
  Coordinates *point // !!! IN/OUT
  )
{ 
  while(point->x < 0)
    if (parameters->h_tile)
      point->x += image->width;
    else
      return FALSE;
  
  while(point->x >= (gint) image->width)
    if (parameters->h_tile)
      point->x -= image->width;
    else
      return FALSE;
  
  while(point->y < 0)
    if (parameters->v_tile)
      point->y += image->height;
    else
      return FALSE;
  
  while(point->y >= (gint) image->height)
    if (parameters->v_tile)
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
prepare_sorted_offsets(void) 
{
  // Minimum().  Use smaller dimension of corpus and image.
  gint width = (corpus.width<image.width ? corpus.width : image.width);
  gint height = (corpus.height<image.height ? corpus.height : image.height);
  
  sorted_offsets_size = 2*2*width*height; 
  sorted_offsets = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), sorted_offsets_size); /* Reserve */
  
  {
  gint x;
  gint y;
  
  for(y=-height+1; y<height; y++)
    for(x=-width+1; x<width; x++) 
      {
      Coordinates coords = {x,y};
      g_array_append_val(sorted_offsets, coords);
      }
  }
  g_array_sort(sorted_offsets, (gint (*)(const void*, const void*)) lessCartesian);
  
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
clippedOrMaskedCorpus(const Coordinates point) 
{
  return (
    point.x < 0 || point.y < 0 
    || point.x >= (gint) corpus.width 
    || point.y >= (gint) corpus.height /*  Clipped */
    ||  ! is_selected_corpus(point) /* Masked */
    ); 
}





/*
This is the crux: comparing target patch to corpus patch, pixel by pixel.
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
static inline gboolean 
try_point(
  const Coordinates point, 
  tBettermentKind pointKind
  ) 
{
  guint sum = 0;
  guint i; 
  
  countSourceTries++;   // Stats
  // Iterate over neighbors of candidate point. Sum grows as more neighbors tested.
  for(i=0; i<n_neighbours; i++)
  {
    Coordinates off_point = add_points(point, neighbours[i]);
    if (clippedOrMaskedCorpus(off_point)) 
    {    
      /* Maximum difference for this neighbor outside corpus */
      sum += MAX_WEIGHT*img_match_bpp + map_diff_table[0]*map_match_bpp;   
    } 
    else  
    {
      const Pixelel * corpus_pixel;
      const Pixelel * image_pixel;
      
      corpus_pixel = pixmap_index(&corpus, off_point);
      // ! Note the target values come not from target_points, but the smaller copy neighbour_values
      image_pixel = neighbour_values[i];
      #ifndef VECTORIZED
      /* If not the target point (its own 0th neighbor).
      !!! On the first pass, the target point as its own 0th neighbor has no meaningful, unbiased value.
      Even if e.g. we initialize target to all black, that biases the search.
      */
      if (i) 
      {
        BppType j;
        for(j=FIRST_PIXELEL_INDEX; j<color_end_bip; j++)
          sum += diff_table[256u + image_pixel[j] - corpus_pixel[j]];
      }
      if (map_match_bpp > 0)
      {
        BppType j;
        for(j=map_start_bip; j<map_end_bip; j++)  // also sum mapped difference
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
    lkk !!! best_point not set.
    Note: equals.
    If this source is same as prior source for target or different from prior source
    ( whether picked randomly or for a repeat) 
    AND all neighbors checked (iteration completed) without finding a lesser best (but maybe an equal best)
    best_point is not changed even if it is a different source.
    ??? Study how many different but equal sources are found.  
    Are different source in later repeats closer distance?
    */
    if (sum >= best) return FALSE;  // !!! Short circuit for neighbors
  }

  // Assert sum strictly < best
  best = sum;
  latestBettermentKind = pointKind;
  
  // best_point might already equal point, but might be smaller sum because different neighbors or different neighbor values
  best_point = point;
  if (sum <=0) 
  {
    bettermentStats[PERFECT_MATCH]+=1;
    return TRUE;
  }
  else 
    return FALSE;
}





/* Create a neighbor.  Initialize: offset, status, and pixel. */
static inline void
new_neighbor(
  guint        index,
  Coordinates offset,
  Coordinates neighbor_point
  )
{
  neighbours[index] = offset;
  set_neighbor_state(index, neighbor_point);
  // !!! Copy the whole Pixel, all the pixelels
  
  {
  BppType k;
  for (k=0; k<total_bpp; k++)
    // c++ neighbour_values[n_neighbours][k] = data.at(neighbor_point)[k];
    neighbour_values[index][k] = pixmap_index(&image, neighbor_point)[k];
  }
}



  

/*
Prepare array of neighbors with values, both inside the target, and outside i.e. in the context (if use_border).
Neighbors are in the source (the target or its context.)
If repeating a pixel, now might have more, different, closer neighbors than on the first pass.
Neighbours array is global, used both for heuristic and in synthing every point ( in try_point() )
Neighbours describes a patch, a shotgun pattern in the first pass, or a contiguous patch in later passes.
*/
void prepare_neighbors(
  Coordinates position, // IN target point
  Parameters *parameters // IN
  ) 
{
  guint j;
  
  n_neighbours = 0; // global
  
  for(j=0; j<sorted_offsets_size; j++)
  {
    // c++ Coordinates offset = sorted_offsets[j];
    Coordinates offset = g_array_index(sorted_offsets, Coordinates, j);
    Coordinates neighbor_point = add_points(position, offset);

    // !!! Note side effects: wrap_or_clip might change neighbor_point coordinates !!!
    if (wrap_or_clip(parameters, &image, &neighbor_point)  // is neighbor in target image or wrappable
        &&  get_has_value(neighbor_point)   // is neighbor outside target (context) 
            // or inside target with already synthed value
      ) 
    {
      new_neighbor(n_neighbours, offset, neighbor_point);
      n_neighbours++;
      if (n_neighbours >= (guint) parameters->neighbours) break;
    }
  }
  
  /*
  Note the neighbors are in order of distance from the target pixel.
  But the matching does not depend on the order, it only matters that neighbors are nearest.
  Experiment to sort the nearest neighbors in other orders, such as in row major order
  (so there might be better memory locality) didn't seem to help speed.
  
  Note we can't assert(n_neighbours==parameters.neighbours)
  If use_border, there is a full neighborhood unless context or corpus small, that is, 
  there are usually plenty of distant neighbors in the context and corpus.
  If not use_border, there is usually a full neighborhood except for the first n_neighbor synthesis tries.
  */
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
  Parameters parameters
  // , GimpDrawable *targetDrawable  // ANIMATE
  )
{
  // target prep
  prepare_target_points(parameters.use_border); // Uses image_mask
  #ifdef ANIMATE
  clear_target_pixels(color_end_bip);  // For debugging, blacken so new colors sparkle
  #endif
  free_map(&image_mask); // Assert no more references to image_mask.
  prepare_target_sources();
  
  // source prep
  prepare_corpus_points();

  /* 
  Rare user error: all pixels transparent (which we ignore.)
  This error NOT occur if selection does not intersect, since then we use the whole drawable.
  */
  if (!corpus_points_size )
  {
    return 1;
  }
  if ( !target_points_size ) 
  {
    return 2;
  }
  
  // prep unrelated to images
  prepare_sorted_offsets(); // Depends on image size
  make_diff_table(parameters.autism, parameters.map_weight);
 
  // Now we need a prng, before order_target_points
  /* Originally: srand(time(0));   But then testing is non-repeatable. 
  TODO the seed should be a hash of the input or a user parameter.
  Then it would be repeatable, but changeable by the user.
  */
  prng = g_rand_new_with_seed(1198472);
  
  order_target_points(&parameters);
  prepare_tried();  // Must follow prepare_corpus
  
  // Preparations done, begin actual synthesis
  print_processor_time();
  
  // progress(_("Resynthesizer: synthesizing"));

  refiner(
    // targetDrawable, // ANIMATE
    parameters);
    
  // TODO free pixmaps
  
  return 0; // Success
}



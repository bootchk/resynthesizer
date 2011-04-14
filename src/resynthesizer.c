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

// Bring in alternative code: experimental, debugging, etc.
// #define ANIMATE    // Animate image while processing, for debugging.
// #define DEBUG
/*
Uncomment this before release.  I'm not sure if it the build environment
defines it on the command line to gcc.
Also, uncomment when using splint.
Leave it commented for development and testing, to enable assertions.
*/
#define G_DISABLE_ASSERT      // To disable g_assert macro, uncomment this.
// #define VECTORIZED // Doesn't help performance yet.

#ifdef VECTORIZED
#include <mmintrin.h> // intrinsics for assembly language MMX op codes, for sse2 xmmintrin.h
#endif

#include "../config.h" // GNU buildtools local configuration
#include "plugin-intl.h" // i18n macros

#include <libgimp/gimp.h>
#include <glib/gprintf.h>

/* Shared with resynth-gui */
#include "resynth-constants.h"
#include "resynth-parameters.h"

/* Source not compiled separately. Is separate to reduce file sizes and later, coupling. */
#include "resynth-types.h"
#include "resynth-map-types.h"

/* See below for more includes. */

/* 
Lookup tables for the statistical function.
For unvectorized, table size is doubled, for negative and positive signed differences,
and a difference is offset by 256 to enter this table.
!!! diff_table is unsigned short, typically 16-bit, no less than 16-bit by C standard.
*/
static gushort diff_table[512];
static guint map_diff_table[512];
#ifdef VECTORIZED
static gushort diff_table2[256];
#endif


/* 
bpp i.e. count of bytes (channels) per pixel or index thereof . 
See data layout in resynth_types.h
*/
typedef guint BppType;


static BppType color_end_bip; /* Index of last color pixelels in target/context image. */
static BppType alpha_bip;      /* Index of target alpha pixelel */
static BppType map_start_bip;  /* Index of map pixelels */
static BppType map_end_bip;

static BppType img_match_bpp; /* bpp to match in image. */
static BppType map_match_bpp; /* bpp to match in map. */
static BppType total_bpp;     /* Total pixelels */


/* 
Local copies of pixmaps (not using gimp regions.) 
2-D arrays of Pixel, addressable by Coordinates (Point).
c++: static Bitmap<Pixelel>
*/
static Map image;         /* Entire image, includes selection (target) and context (non-target) */
static Map corpus;        /* Source.  Might be distinct from image. */
static Map image_mask;    /* Selection channel for image */
static Map corpus_mask;   /* Selection channel for corpus */


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
static gboolean is_alpha_image = FALSE;
static gboolean is_alpha_corpus = FALSE;

clock_t start_time;

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
  
  new_coordmap(&source_of, image.width, image.height);
  
  Coordinates null_coords = {-1, -1};
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
  // Size is checked by caller. 
}




static inline Coordinates
random_corpus_point ()
{
  /* Was rand()%corpus_points_size but thats not uniform. */
  gint index = g_rand_int_range(prng, 0, corpus_points_size);
  return g_array_index(corpus_points, Coordinates, index);
}


static double 
neglog_cauchy(double x) 
{
    return log(x*x+1.0);
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
  
  gint x;
  gint y;
  
  for(y=-height+1; y<height; y++)
    for(x=-width+1; x<width; x++) 
      {
      Coordinates coords = {x,y};
      g_array_append_val(sorted_offsets, coords);
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
      const Pixelel * __restrict__ corpus_pixel = pixmap_index(&corpus, off_point);
      // ! Note the target values come not from target_points, but the smaller copy neighbour_values
      const Pixelel  * __restrict__ image_pixel = neighbour_values[i];
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



static void
progress(gchar * message)
{
  gimp_progress_init(message);
  gimp_progress_update(0.0);
#ifdef DEBUG
  /* To console.  On Windows, it annoyingly opens a console.  
  On Unix it dissappears unless console already open.
  */
  g_printf(message);  
  g_printf("\n");
#endif
}



/* Return count of color channels, exclude alpha and any other channels. */
static guint
count_color_channels(GimpDrawable *drawable)
{
  GimpImageType type = gimp_drawable_type(drawable->drawable_id);
  switch(type)
  {
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      return 3;
    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:
      return 1;
    default:
      g_assert(FALSE);
  }
  return 0;
}

/*
Return whether drawables have the same base type.
*/
static gboolean
equal_basetypes(
  GimpDrawable *first_drawable,
  GimpDrawable *second_drawable
  )
{
  /* !!! Not drawable->bpp minus one if alpha, because there  might be other channels. */
  return (count_color_channels(first_drawable) == count_color_channels(second_drawable));
}


/* 
Lookup tables for function of pixel differences.
Weight Pixel differences by a statistical distribution: cauchy (not gaussian, see thesis.)
Table is [0, 511]
Pixel difference is [-255, 255].
Symmetrical, inverted bell with min at index 256, max at 1 (-255 + 256) and 511 (+255 + 256)
diff_table[0] is extraordinary: used to hold a limiting MAX,
since the most negative pixel difference of -255 looks up diff_table[1].
Original code used diff_table[0] for a MAX instead of a constant.
*/
void 
make_diff_table(
  gfloat autism, 
  gfloat map_weight
  ) 
{
  gint i;
  
  for(i=-256; i<256; i++) 
  {
      gdouble value = neglog_cauchy(i/256.0/autism) 
                     / neglog_cauchy(1.0/autism) * (float)MAX_WEIGHT;
      diff_table[256+i] = (gushort)value;
      // Note guint: multiplying by MAP_MULTIPLIER carries gushort into guint
      map_diff_table[256+i] = (guint)(i*i*map_weight*MAP_MULTIPLIER);
  }
}



#ifdef VECTORIZED
/* Calculating absolute value of pixel difference so table is only [0,255] */
void make_diff_table2(float autism, float map_weight) 
{
  gint i;
  
  for(i=0;i<256;i++)
  {
    double value = neglog_cauchy(i/256.0/autism) 
                     / neglog_cauchy(1.0/autism) * (float)MAX_WEIGHT;
    diff_table2[i] = (gushort) value;
  }
}
#endif



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
  BppType k;
  for (k=0; k<total_bpp; k++)
    // c++ neighbour_values[n_neighbours][k] = data.at(neighbor_point)[k];
    neighbour_values[index][k] = pixmap_index(&image, neighbor_point)[k];
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
  n_neighbours = 0;
  guint j;
  
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



/*
Prepare parameters of repetition (passes over the target.)
The algorithm repeats synthesis because very early synthesized points
might be wild guesses, more or less random colors,
especially if the context is far away.

Repeat over full target, then repeat over smaller prefix of target, etc.
If synthesizing directionally (target_points ordered instead of random)
then later passes do not cover the target uniformly.
Instead, later passes repeat a prefix, which when directionally ordered,
means repeating target points near the context.
It could be argued that this is good, since those points
are often a transition and need the most work to produce a good result.

The original code just lengthened target_points vector by duplicating a prefix:
  for(int n=target_points_size;n>0;) {
    n = n*3/4; // <- note magic number... the more passes, the higher the quality, maybe
    for(int i=0;i<n;i++)
      target_points.push_back(target_points[i]);
  }
and just iterated over the target_point vector, either forward or back:
for(int i=target_points_size-1;i>=0;i--) { // do a fraction, then more, etc. then all
for(int i=0; i<=int(target_points_size-1); i++) {  // do all, then repeat a fraction, etc.

The new code is nearly the same as the original except:
-the second pass resynthesizes all points.
-the number of passes is limited to 6

TODO experiments on other ways of repeating synthesis.
*/

#define MAX_PASSES 6
static guint repetition_params[MAX_PASSES][2];

static void
prepare_repetition_parameters()
{ 
  guint n = target_points_size;
  
  /* First pass over all points  */
  repetition_params[0][0] = 0;  
  repetition_params[0][1] = n;
  total_targets = n;
  
  /* Second pass over all points, subsequent passes over declining numbers at an exponential rate. */
  guint i;
  for (i=1; i<MAX_PASSES; i++) 
  {
    repetition_params[i][0] = 0;    /* Start index of iteration. Not used, starts at 0 always, see the loop. */
    repetition_params[i][1] = n;    /* End index of iteration. */
    total_targets += n;
    n = (guint) n*3/4;
  }
}


/* 
Update Gimp image from local pixmap. Canonical postlude for plugins.
!!! Called in the postlude but also for debugging: animate results during processing.
*/
static void 
post_results_to_gimp(GimpDrawable *drawable) 
{
  pixmap_to_drawable(image, drawable, FIRST_PIXELEL_INDEX);   // our pixels to region
  gimp_drawable_flush(drawable);    // regions back to core
  gimp_drawable_merge_shadow(drawable->drawable_id,TRUE);   // temp buffers merged
  gimp_drawable_update(drawable->drawable_id,0,0,image.width,image.height);
  gimp_displays_flush();
}






/*
The heart of the algorithm.
Called repeatedly: many passes over the data.
*/
static guint
synthesize(
  guint pass,
  Parameters *parameters, // IN,
  GimpDrawable *drawable  // IN for ANIMATE
  )
{
  guint target_index;
  guint repeatCountBetters = 0;
  /* ALT: count progress once at start of pass countTargetTries += repetition_params[pass][1]; */
  reset_color_change();
  
  for(target_index=0; target_index<repetition_params[pass][1]; target_index++) 
  {
    countTargetTries += 1;  /* ALT count progress at each target point. */
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
    
    Coordinates position = g_array_index(target_points, Coordinates, target_index);
     
    /*
    This means we are about to give it a value (and a source),
    but also means that below, we put offset (0,0) in vector of neighbors !!!
    i.e. this makes a target point it's own neighbor (with a source in later passes.)
    */
    set_has_value(&position, TRUE);  
    
    prepare_neighbors(position, parameters);
    
    /*
    Repeat a pixel even if found an exact match last pass, because neighbors might have changed.
    
    On passes after the first, we don't explicitly start with best of the previous match,
    but since a pixel is it's own first neighbor, the first best calculated will a be
    from the source that gave the previous best, and should be a good starting best.
    */
    best = G_MAXUINT; /* A very large positive number.  Was: 1<<30 */       
    gboolean is_perfect_match = FALSE;
    latestBettermentKind = NO_BETTERMENT;
    
    /*
    Heuristic 1, try neighbors of sources of neighbors of target pixel.
    
    Subtle: The target pixel is its own first neighbor (offset 0,0).
    It also has_value (since we set_has_value() above, but it really doesn't have color on the first pass.)
    On the first pass, it has no source.
    On subsequent passes, it has a source and thus its source is the first corpus point to be tried again,
    and that will set best to a low value!!!
    */
    guint neighbor_index;
    for(neighbor_index=0; neighbor_index<n_neighbours && best != 0; neighbor_index++)
      // If the neighbor is in the target (not the context) and has a source in the corpus
      if ( has_source_neighbor(neighbor_index) ) {
        /*
        Coord arithmetic: corpus source minus neighbor offset.
        corpus_point is a pixel in the corpus with opposite offset to corpus source of neighbor
        as target position has to this target neighbor.
        !!! Note corpus_point is raw coordinate into corpus: might be masked.
        !!! It is not an index into unmasked corpus_points.
        */
        Coordinates corpus_point = subtract_points(neighbour_source[neighbor_index], neighbours[neighbor_index]);
        
        /* !!! Must clip corpus_point before further use, its only potentially in the corpus. */
        if (clippedOrMaskedCorpus(corpus_point)) continue;
        if (*intmap_index(&tried, corpus_point) == target_index) continue;  /* Heuristic 2 */
        is_perfect_match = try_point(corpus_point, NEIGHBORS_SOURCE);
        if ( is_perfect_match ) break;  // Break neighbors loop
        
        /*
        !!! Remember we tried corpus pixel point for target point target_index.
        Heuristic 2: all target neighbors with values might come from the same corpus locus.
        */
        *intmap_index(&tried, corpus_point) = target_index;
      }
      // Else the neighbor is not in the target (has no source) so we can't use the heuristic 1.
      
    if ( ! is_perfect_match )
    {
      /* Try random source points from the corpus */
      gint j;
      for(j=0; j<parameters->trys; j++)
      {
        is_perfect_match = try_point(random_corpus_point(), RANDOM_CORPUS);
        if ( is_perfect_match ) break;  /* Break loop over random corpus points */
        /* Not set tried(point) because that heuristic rarely works for random source. */
      }
    }
    
    store_betterment_stats(latestBettermentKind);
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
    if (latestBettermentKind != NO_BETTERMENT )
    {
      /* if source different from previous pass */
      if ( ! equal_points(get_source_of(position), best_point) ) 
      {
        repeatCountBetters++;   /* feedback for termination. */
        integrate_color_change(position); /* Stats. Must be before we store the new color values. */
        /* Save the new color values (!!! not the alpha) for this target point */
        BppType j;
        for(j=FIRST_PIXELEL_INDEX; j<color_end_bip; j++)  // For all color pixelels (channels)
          pixmap_index(&image, position)[j] = pixmap_index(&corpus, best_point)[j];  // Overwrite prior with new color
        set_source(position, best_point); /* Remember new source */
      } /* else same source for target */
    } /* else match is same or worse */
  } /* end for each target pixel */
  return repeatCountBetters;
}


static void
detach_drawables(
  GimpDrawable * out,
  GimpDrawable * in,
  GimpDrawable * out_map,
  GimpDrawable * in_map
  )
{
  if (out)
    gimp_drawable_detach(out);
  if (in)
    gimp_drawable_detach(in);
  if (out_map)
    gimp_drawable_detach(out_map);
  if (in_map)
    gimp_drawable_detach(in_map);
}


/* Main */

static void run(
  const gchar *     name,
  gint              nparams,
	const GimpParam * param,
	gint *            nreturn_vals,
	GimpParam **      return_vals)
{
  static GimpParam values[2];   /* Gimp return values. !!! Allow 2: status and error message. */
  Parameters parameters;
  
  GimpDrawable *drawable = NULL;
  GimpDrawable *corpus_drawable = NULL; 
  GimpDrawable *map_in_drawable= NULL; 
  GimpDrawable *map_out_drawable= NULL; 
  gboolean ok, with_map;
  
  #ifdef DEBUG
  gimp_message_set_handler(1); // To console instead of GUI
  start_time = clock();
  #endif
  
  /* Originally: srand(time(0));   But then testing is non-repeatable. 
  TODO the seed should be a hash of the input or a user parameter.
  Then it would be repeatable, but changeable by the user.
  */
  prng = g_rand_new_with_seed(1198472);
  
  // internationalization i18n
  // Note these constants are defined in the build environment.
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
  #ifdef HAVE_BIND_TEXTDOMAIN_CODESET
    bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  #endif
  textdomain (GETTEXT_PACKAGE);   // Equivalent to: textdomain("resynthesizer") ;  

  *nreturn_vals = 1;
  *return_vals = values;
  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = GIMP_PDB_EXECUTION_ERROR; /* Unless everything succeeds. */
  
  drawable = gimp_drawable_get(param[2].data.d_drawable);

  /* Check image type (could be called non-interactive) */
  if (!gimp_drawable_is_rgb(drawable->drawable_id) &&
      !gimp_drawable_is_gray(drawable->drawable_id)) 
  {
    ERROR_RETURN(_("Incompatible image mode."));
  }


  /* Deal with run mode */
  ok = FALSE;
  switch(param[0].data.d_int32) 
  {
    case GIMP_RUN_INTERACTIVE :
      ok = get_last_parameters(&parameters,drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME);
      gimp_message("Resynthesizer engine should not be called interactively");
      /* But keep going with last (or default) parameters, really no harm. */
      break;
    case GIMP_RUN_NONINTERACTIVE :
      ok = get_parameters_from_list(&parameters, nparams, param); 
      break;
    case GIMP_RUN_WITH_LAST_VALS :
      ok = get_last_parameters(&parameters,drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME); 
      break;
  }

  if (!ok) 
  {
    ERROR_RETURN(_("Resynthesizer failed to get parameters."));
  }
  
  /* Limit neighbours parameter to size allocated. */
  if (parameters.neighbours > RESYNTH_MAX_NEIGHBORS )
    parameters.neighbours = RESYNTH_MAX_NEIGHBORS;
  
  dump_parameters(&parameters);
  
  corpus_drawable = gimp_drawable_get(parameters.corpus_id);
  
  /* The target and corpus must have the same base type.
  In earlier version, they must have the same bpp.
  But now we don't compare the alphas, so they can differ in presence of alpha.
  */
  if (! equal_basetypes(drawable, corpus_drawable) )
  {
    ERROR_RETURN(_("The input texture and output image must have the same number of color channels."));
  }
  
  
  with_map = (parameters.input_map_id != -1 && parameters.output_map_id != -1);
  /* If only one map is passed, it is ignored quietly. */
  map_in_drawable=0;
  map_out_drawable=0;

  if (with_map) 
  {
    map_in_drawable = gimp_drawable_get(parameters.input_map_id);
    map_out_drawable = gimp_drawable_get(parameters.output_map_id);
    /* All these can be wrong at the same time.  
    Forego userfriendliness for ease of programming: abort on first error
    */
    if ( ! equal_basetypes(map_in_drawable, map_out_drawable) )
    {
      /* Maps need the same base type. Formerly needed the same bpp. */
      ERROR_RETURN(_("The input and output maps must have the same mode"));
    } 
    if (map_in_drawable->width != corpus_drawable->width || 
               map_in_drawable->height != corpus_drawable->height) 
    {
      ERROR_RETURN(_("The input map should be the same size as the input texture image"));
    } 
    if (map_out_drawable->width != drawable->width || 
               map_out_drawable->height != drawable->height) 
    {
      ERROR_RETURN(_("The output map should be the same size as the output image"));
    }
  }

  /* Set flags for presence of alpha channels. */
  is_alpha_image = gimp_drawable_has_alpha(drawable->drawable_id);
  is_alpha_corpus = gimp_drawable_has_alpha(parameters.corpus_id);
  
  /* 
  The engine should not be run interactively so no need to store last values. 
  I.E. the meaning of "last" is "last values set by user interaction".
  */
    
  /*
  Dynamic counts and indexes of pixelels.  Depends on the in drawables.
  !!! These are the same for target and corpus, even if the in drawables differ in alphas.
  !!! See resynth_types.h.
  bpp means bytes per pixel
  bip means byte index in pixel
  !!! Note the end is not the index of the last, but the index after the last
  
  [0]                         mask pixelel
  [1,color_end_bip)           image color pixelels, up to 3 (RGB)
  optional alpha byte         
  [map_start_bip, total_bpp)  map color pixelels
  optional map alpha byte     !!! discard
  [0, total_bpp)              entire pixel
  
  [1, color_end_bip)  color pixelels compared
  [map_start_bip, map_end_bip)      map pixelels compared
  
  Examples:
  RGBA with RGB maps                RGB with GRAY maps                RGB with no maps
  0 Mask (selection)                M                                 M
  1 R FIRST_PIXELEL_INDEX           R FIRST_PIXELEL_INDEX             R
  2 G                               G                                 G
  3 B                               B                                 B
  4 A alpha_bip, color_end_bip      W color_end_bip, map_start_bip   4  color_end, map_start, map_end, total
  5 R map_start_bip                5  map_end_bip, total_bpp
  6 G
  7 B
  8   map_end_bip, total_bpp
  
  !!! alpha_bip is undefined unless is_alpha_corpus or is_alpha_image
  
  TODO Possibly pad pixel to size 8 for memory alignment, especially if vectorized.
  */
  
  /* !!! Not drawable->bpp because it includes other channels. */
  /* Don't compare alpha */
  img_match_bpp = count_color_channels(drawable);
    
  /* Index of first color pixelel: 1, follows mask, use constant FIRST_PIXELEL_INDEX */
  color_end_bip   = FIRST_PIXELEL_INDEX + img_match_bpp;
  
  if ( is_alpha_image || is_alpha_corpus )
  {
    /* Allocate a pixelel for alpha. */
    alpha_bip = color_end_bip;
    map_start_bip = 1 + color_end_bip;
  }
  else
    /* alpha_bip is undefined. */
    map_start_bip = color_end_bip;
   
  /* Count pixelels to compare in maps. */
  if ( with_map )
  {
    /* 
    Either, none, or both maps can have alpha, but it is discarded. 
    Both maps must have same count color pixelels, checked earlier. 
    */
    map_match_bpp = count_color_channels(map_in_drawable);
  }
  else
    map_match_bpp =0;
   
  map_end_bip   = map_start_bip + map_match_bpp;
  total_bpp  = map_end_bip;  
  g_assert( total_bpp <= MAX_RESYNTH_BPP);
  
  /* target/context prep */
  fetch_image_mask_map(drawable, &image, total_bpp, &image_mask, MASK_TOTALLY_SELECTED, 
    map_out_drawable, map_start_bip);
  prepare_target_points(parameters.use_border); // This uses image_mask
  #ifdef ANIMATE
  clear_target_pixels(color_end_bip);  // For debugging, blacken so new colors sparkle
  #endif
  free_map(&image_mask); // Assert no more references to image_mask.
  prepare_target_sources();
  
  /*  corpus prep */
  fetch_image_mask_map(corpus_drawable, &corpus, total_bpp, &corpus_mask, MASK_TOTALLY_SELECTED, 
      map_in_drawable, map_start_bip);
  free_map(&corpus_mask);
  prepare_corpus_points();

  /* 
  Rare user error: all pixels transparent (which we ignore.)
  This error NOT occur if selection does not intersect, since then we use the whole drawable.
  */
  if (!corpus_points_size )
  {
    ERROR_RETURN(_("The texture source is empty. Does any selection include non-transparent pixels?"));
  }
  if ( !target_points_size ) 
  {
    ERROR_RETURN(_("The output layer is empty. Does any selection have visible pixels in the active layer?"));
  }
  
  prepare_sorted_offsets();
  make_diff_table(parameters.autism, parameters.map_weight);
  #ifdef VECTORIZED
  make_diff_table2(parameters.autism, parameters.map_weight);
  #endif
  order_target_points(&parameters);
  prepare_tried();  // Must follow prepare_corpus
  prepare_repetition_parameters();
  
  /* Preparations done, begin actual synthesis */
  print_processor_time();
  progress(_("Resynthesizer: synthesizing"));

  guint pass;
  for (pass=0; pass<MAX_PASSES; pass++)
  {
    guint betters = synthesize(pass, &parameters, drawable);
  
    print_pass_stats(pass, repetition_params[pass][1], betters);
  
    /* Break if a small fraction of target is bettered
    This is a fraction of total target points, 
    not the possibly smaller count of target attempts this pass.
    Or break on small integral change: if ( target_points_size / integralColorChange < 10 ) {
    */
    if ( (float) betters / target_points_size < (RESYNTH_TERMINATE_FRACTION) )
      break;
  }

  /* dump_target_points(); */ /* detailed debugging. */
  print_post_stats();
    
  post_results_to_gimp(drawable); /* Update Gimp image from local pixmap */
  
  /* Clean up */
  detach_drawables(drawable, corpus_drawable, map_in_drawable, map_out_drawable);
  gimp_progress_end();
  values[0].data.d_status = GIMP_PDB_SUCCESS;
} 

/* PDB registration and MAIN() */
#include "resynth-pdb.h"


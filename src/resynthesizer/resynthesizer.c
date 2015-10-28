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
to the opposite side.
It doesn't make tiles in the target, it makes a target that is suitable as a tile.
*/

//#include "buildSwitches.h"      // Affects debug, assertions, use of glib, threading, etc.

#include "../../config.h" // GNU buildtools local configuration
#include "../plugin-intl.h" // i18n macros

#include <libgimp/gimp.h>
#include <glib/gprintf.h>

/* Shared with resynth-gui plugin, resynthesizer engine plugin. */
#include "../resynth-constants.h"

/*
True header files: types, function prototypes, and in-line functions only.
No definitions of non in-line functions or data.

Types, etc. from resynthesizer (image_synth) library
*/
// FIXME need to include glibProxy.h here so everything else uses glibless Map?
#ifdef USE_GLIB_PROXY
	#include "glibProxy.h"
#endif
#include "imageFormat.h"
#include "imageFormatIndicies.h"
#include "map.h"
#include "engineParams.h"
#include "engine.h" // requires map.h
#include "imageSynthConstants.h"

/* 
Source included, not compiled separately. 
Is separate to reduce file sizes and later, coupling. 
*/

#include "mapIndex.h"	// from resynthesizer library
#include "adaptGimp.h"  // requires mapIndex.h
#include "../resynth-parameters.h" // requires engine.h
#include "adaptParameters.c"

/* See below for more includes. */

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

#ifdef ANIMATE
/*
Use in debugging with DEEP_PROGRESS and ANIMATE.
Blacken target, then animate pixels as they are synthesized.
*/

GimpDrawable * targetDrawableCopy;
Map*            targetMapCopy;

static void 
post_results_to_gimp(
  GimpDrawable *drawable,
  Map targetMap);
  
/* 
Clear target pixels. So see them get synthesized when animated debugging. 
Note the initial values of the target are never used, but totally synthesized.
*/
static void
clear_target_pixels(guint bpp)
{
  guint x;
  guint y;
  
  for(y=0;y<targetMapCopy->height;y++)
    for(x=0;x<targetMapCopy->width;x++)
    {
      Coordinates coords = {x,y};
      if (pixmap_index(targetMapCopy, coords)[MASK_PIXELEL_INDEX] != MASK_UNSELECTED)
      // if (isSelectedTarget(coords, targetMapCopy)) 
      {
        guint pixelel;
        Pixelel * pixel = pixmap_index(targetMapCopy, coords);
        for (pixelel = FIRST_PIXELEL_INDEX; pixelel < bpp; pixelel++) // Color channels only
          pixel[pixelel] = PIXELEL_BLACK;
      }
    }
}

#endif


/*
Progress functions.
*/

// Called before any real progress is made
static void
progressStart(gchar * message)
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

// Called repeatedly as progress is made
void  // Not static, in test build, called from inside engine
progressUpdate( int percent, void * contextInfo)
{
  gimp_progress_update((float)percent/100);

  #ifdef ANIMATE
  post_results_to_gimp(targetDrawableCopy, *targetMapCopy);
  #endif

}


/* Return count of color channels, exclude alpha and any other channels. */
static guint
count_color_channels(GimpDrawable *drawable)
{
  g_assert(drawable); // Not null
  
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
Update Gimp image from local pixmap. Canonical postlude for plugins.
!!! Called in the postlude but also for debugging: animate results during processing.
*/
static void 
post_results_to_gimp(
  GimpDrawable *drawable,
  Map targetMap) 
{
  pixmap_to_drawable(targetMap, drawable, FIRST_PIXELEL_INDEX);   // our pixels to region
  gimp_drawable_flush(drawable);    // regions back to core
  gimp_drawable_merge_shadow(drawable->drawable_id,TRUE);   // temp buffers merged
  gimp_drawable_update(drawable->drawable_id,0,0,targetMap.width,targetMap.height);
  gimp_displays_flush();
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

#ifdef ADAPT_SIMPLE
  #include "imageBuffer.h"
  #include "adaptSimple.h"
  #include "adaptGimpSimple.h"
#endif

/* 
Plugin main.
This adapts the texture synthesis engine to a Gimp plugin.
*/

static void run(
  const gchar *     name,
  gint              nparams,
	const GimpParam * param,
	gint *            nreturn_vals,
	GimpParam **      return_vals)
{
  static GimpParam values[2];   /* Gimp return values. !!! Allow 2: status and error message. */
  TGimpAdapterParameters pluginParameters;
  TImageSynthParameters engineParameters;
  
  GimpDrawable *drawable = NULL;
  GimpDrawable *corpus_drawable = NULL; 
  GimpDrawable *map_in_drawable= NULL; 
  GimpDrawable *map_out_drawable= NULL; 
  gboolean ok, with_map;
  
  /* 
  Local copies of pixmaps (not using gimp regions.) 
  2-D arrays of Pixel, addressable by Coordinates (Point).
  c++: static Bitmap<Pixelel>
  */
  Map targetMap;
  Map corpusMap;
  Map targetMaskMap;
  Map corpusMaskMap;
  
  int cancelFlag = 0;
  
  #ifdef SYNTH_THREADED
  // This is as early as it can be called.  Not sure it needs to be called.  See later call to it.
  // Call it early since calls to gdk, gtk might require this?
  g_thread_init(NULL);
  #endif

  #ifdef DEBUG
  gimp_message_set_handler(1); // To console instead of GUI
  start_time = clock();
  #endif
  
  // internationalization i18n
  // Note these constants are defined in the build environment.
  /*  Initialize i18n support  */
#if defined(G_OS_WIN32)
  bindtextdomain (GETTEXT_PACKAGE, gimp_locale_directory());
#else
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
#endif
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
  textdomain (GETTEXT_PACKAGE);

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
      ok = get_last_parameters(&pluginParameters,drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME);
      gimp_message("Resynthesizer engine should not be called interactively");
      /* But keep going with last (or default) parameters, really no harm. */
      break;
    case GIMP_RUN_NONINTERACTIVE :
      ok = get_parameters_from_list(&pluginParameters, nparams, param); 
      break;
    case GIMP_RUN_WITH_LAST_VALS :
      ok = get_last_parameters(&pluginParameters,drawable->drawable_id, RESYNTH_ENGINE_PDB_NAME); 
      break;
  }

  if (!ok) 
  {
    ERROR_RETURN(_("Resynthesizer failed to get parameters."));
  }
  
  /* Limit neighbours parameter to size allocated. */
  if (pluginParameters.neighbours > IMAGE_SYNTH_MAX_NEIGHBORS )
    pluginParameters.neighbours = IMAGE_SYNTH_MAX_NEIGHBORS;
  
  corpus_drawable = gimp_drawable_get(pluginParameters.corpus_id);
  
  /* The target and corpus must have the same base type.
  In earlier version, they must have the same bpp.
  But now we don't compare the alphas, so they can differ in presence of alpha.
  */
  if (! equal_basetypes(drawable, corpus_drawable) )
  {
    ERROR_RETURN(_("The input texture and output image must have the same number of color channels."));
  }
  
  
  with_map = (pluginParameters.input_map_id != -1 && pluginParameters.output_map_id != -1);
  /* If only one map is passed, it is ignored quietly. */
  map_in_drawable=0;
  map_out_drawable=0;

  if (with_map) 
  {
    map_in_drawable = gimp_drawable_get(pluginParameters.input_map_id);
    map_out_drawable = gimp_drawable_get(pluginParameters.output_map_id);
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

  /* 
  The engine should not be run interactively so no need to store last values. 
  I.E. the meaning of "last" is "last values set by user interaction".
  */
  
  #ifdef ANIMATE
  // Copy local pointer vars to globals
  targetDrawableCopy = drawable;
  targetMapCopy = &targetMap;
  #endif
  
  /* Error checks done, initialization work begins.  So start progress callbacks. */
  progressStart("Initializing...");
  
  /* 
  Set flags for presence of alpha channels. 
  The flag is an optimization.  Alternatives:
  - a function
  - OR standardize the internal pixmap to ALWAYS have an alpha pixelel
  initialized to VISIBLE and set from any alpha pixelel.
  */
  gboolean is_alpha_image = gimp_drawable_has_alpha(drawable->drawable_id);
  gboolean is_alpha_corpus = gimp_drawable_has_alpha(corpus_drawable->drawable_id);
  
  // Image adaption requires format indices
  // WAS  prepareImageFormatIndices(drawable, corpus_drawable, with_map, map_in_drawable);
  TFormatIndices formatIndices;
  
  guint map_count = (with_map? count_color_channels(map_in_drawable) : 0 );
    
  prepareImageFormatIndices(
    &formatIndices,
    count_color_channels(drawable),
    map_count,
    is_alpha_image,
    is_alpha_corpus,
    with_map
    );
  
  #ifdef ADAPT_SIMPLE
    /* Adapt Gimp to an engine with a simpler interface. */
    setDefaultParams(&parameters);
    ImageBuffer imageBuffer;
    ImageBuffer maskBuffer;
    
    // TODO change to new signature
    adaptGimpToSimple(drawable, &imageBuffer, &maskBuffer);  // From Gimp to simple
    g_printf("Here3\n");
    adaptSimpleAPI(&imageBuffer, &maskBuffer);        // From simple to existing engine API
    
  #else
    g_printf("Gimp adaption\n");
    /* target/context adaption */
    fetch_image_mask_map(drawable, &targetMap, formatIndices.total_bpp, 
      &targetMaskMap, 
      MASK_TOTALLY_SELECTED, 
      map_out_drawable, formatIndices.map_start_bip);
    
      #ifdef ANIMATE
      clear_target_pixels(formatIndices.colorEndBip);  // For debugging, blacken so new colors sparkle
      #endif
  
    /*  corpus adaption */
    fetch_image_mask_map(corpus_drawable, &corpusMap, formatIndices.total_bpp, 
      &corpusMaskMap,
      MASK_TOTALLY_SELECTED, 
      map_in_drawable, formatIndices.map_start_bip);
      
    // TODO These are artifacts of earlier design, not used.
    free_map(&corpusMaskMap);
    free_map(&targetMaskMap);
    
    adaptPluginToLibraryParameters(&pluginParameters, &engineParameters);
    
  #endif
  
  // After possible adaption, check size again
  g_assert(targetMap.width * targetMap.height); // Image is not empty
  g_assert(corpusMap.width * corpusMap.height); // Corpus is not empty
  
  // Done with adaption: now main image data in canonical pixmaps, etc.
  // Begin real work
  progressStart("synthesizing...");
  
  int result = engine(
    engineParameters, 
    &formatIndices, 
    &targetMap, 
    &corpusMap,
    progressUpdate,
    (void *) 0,
    &cancelFlag
    );
  
  if (result == IMAGE_SYNTH_ERROR_EMPTY_CORPUS)
  {
    ERROR_RETURN(_("The texture source is empty. Does any selection include non-transparent pixels?"));
  }
  else if  (result == IMAGE_SYNTH_ERROR_EMPTY_TARGET )
  {
    ERROR_RETURN(_("The output layer is empty. Does any selection have visible pixels in the active layer?"));
  }
  
  // Normal post-process adaption follows

  /* dump_target_points(); */ /* detailed debugging. */
  // print_post_stats();
  
  // Update Gimp image from local pixmap
  // Note this works even for test harness where ADAPT_SIMPLE
  // but then it does NOT test returning results in buffer.
  
  /* 
  We could test antiAdaptImage() here.
  But antiAdaptImage() has already been tested once on the incoming side.
  So no compelling need to test it again here.
  */
  post_results_to_gimp(drawable, targetMap); 
  
  /* Clean up */
  // Adapted
  free_map(&targetMap);
  free_map(&corpusMap);
  // GIMP
  detach_drawables(drawable, corpus_drawable, map_in_drawable, map_out_drawable);
  gimp_progress_end();
  values[0].data.d_status = GIMP_PDB_SUCCESS;
} 

/* PDB registration and MAIN() */
#include "../resynth-pdb.h"


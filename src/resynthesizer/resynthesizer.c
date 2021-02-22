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

// TODO temporarily disable build configuration
// #include "buildSwitches.h"      // Affects debug, assertions, use of glib, threading, etc.

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
// TODO these are headers for libresynthesizer
// but here we are getting them from the source tree
// Better to get them from installed headers
#include "imageFormat.h"
#include "imageFormatIndicies.h"
#include "map.h"
#include "engineParams.h"
#include "engine.h" // requires map.h
#include "imageSynthConstants.h"

#include "drawable.h" // compatibility
#include "debug.h"

/*
Source included, not compiled separately.
Is separate to reduce file sizes and later, coupling.
*/

#include "mapIndex.h"	// from resynthesizer library
#include "drawable.h" // compatibility
#include "adaptGimp.h"  // requires mapIndex.h
//#include "../resynth-parameters.h" // requires engine.h
#include "pluginParams.h"
#include "adaptParameters.c"

#include "resynthesizer.h"

/* See below for more includes. */



/*
Update Gimp image from local pixmap. Canonical postlude for plugins.
!!! Called in the postlude but also for debugging: animate results during processing.
*/
static void
post_results_to_gimp(
  GimpDrawable *drawable,
  Map                 targetMap)
{
  // our pixels back to Gimp.  Since 2.10, using GeglBuffers, and this flushes them
  debug("pixmap to drawable");
  pixmap_to_drawable(targetMap, drawable, FIRST_PIXELEL_INDEX);

  debug("flush");
  // temp buffers merged
  if ( ! merge_shadow(drawable) )
      debug("fail merge shadow");
  update(drawable, 0, 0, targetMap.width, targetMap.height);
  gimp_displays_flush();
}


#ifdef ANIMATE
/*
Use in debugging with DEEP_PROGRESS and ANIMATE.
Blacken target, then animate pixels as they are synthesized.
*/

GimpDrawable * targetDrawableCopy;
Map*            targetMapCopy;



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

  GimpImageType type = imageType(drawable);
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


// Do drawables have the same base type?
static gboolean
equal_basetypes(
  GimpDrawable *first_drawable,
  GimpDrawable *second_drawable
  )
{
  /* !!! Not drawable->bpp minus one if alpha, because there  might be other channels. */
  return (count_color_channels(first_drawable) == count_color_channels(second_drawable));
}







#ifdef ADAPT_SIMPLE
  #include "imageBuffer.h"
  #include "adaptSimple.h"
  #include "adaptGimpSimple.h"
#endif





/*
Plugin generic main.

Generic: used by both v2 and v3 GIMP plugin API.
Note some params are GimpDrawable* that formerly were ID's.

This adapts the texture synthesis engine to a Gimp plugin.
*/

char *
inner_run(
  gchar *                 name,
  gint32                        run_mode,
  GimpDrawable           *in_drawable,
  TGimpAdapterParameters       *pluginParameters  // Not const, we further constrain it
	)
{
  TImageSynthParameters engineParameters;

  GimpDrawable *corpus_drawable = pluginParameters->corpus;
  // Require target and corpus not NULL
  g_assert(in_drawable != NULL);
  g_assert(corpus_drawable != NULL);

  GimpDrawable *map_in_drawable= NULL;
  GimpDrawable *map_out_drawable= NULL;
  gboolean      with_map;

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

  debug("inner_run");

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
// TODO revise for Gimp 3
// TODO maybe newer is INIT_I18N ();
/*
#if defined(G_OS_WIN32)
  bindtextdomain (GETTEXT_PACKAGE, gimp_locale_directory());
#else
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
#endif
*/
// TODO temporarily always call gimp
bindtextdomain (GETTEXT_PACKAGE, gimp_locale_directory());
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
  textdomain (GETTEXT_PACKAGE);


  /* Check image type (could be called non-interactive) */
  if (!is_rgb(in_drawable) &&
      !is_gray(in_drawable))
    return _("Incompatible image mode.");

  /* Limit neighbours parameter to size allocated. */
  if (pluginParameters->neighbours > IMAGE_SYNTH_MAX_NEIGHBORS )
    pluginParameters->neighbours = IMAGE_SYNTH_MAX_NEIGHBORS;

  /*
  Require target and corpus have the same base type.
  In earlier version, they must have the same bpp.
  But now we don't compare the alphas, so they can differ in presence of alpha.
  */
  if (! equal_basetypes(in_drawable, corpus_drawable) )
    return _("The input texture and output image must have the same number of color channels.");

  with_map = (pluginParameters->input_map != NULL && pluginParameters->output_map != NULL);
  /* If only one map is passed, it is ignored quietly. */

  if (with_map)
  {
    //map_in_drawable = gimp_drawable_get(pluginParameters->input_map_id);
    //map_out_drawable = gimp_drawable_get(pluginParameters->output_map_id);
    map_in_drawable = pluginParameters->input_map;
    map_out_drawable = pluginParameters->output_map;
    /* All these can be wrong at the same time.
    Forego userfriendliness for ease of programming: abort on first error
    */
    if ( ! equal_basetypes(map_in_drawable, map_out_drawable) )
      /* Maps need the same base type. Formerly needed the same bpp. */
      return _("The input and output maps must have the same mode");
    if (   width(map_in_drawable)  != width(corpus_drawable)
        || height(map_in_drawable) != height(corpus_drawable)
       )
      return _("The input map should be the same size as the input texture image");
    if (   width(map_out_drawable) != width(in_drawable)
        || height(map_out_drawable) != height(in_drawable)
       )
      return _("The output map should be the same size as the output image");
  }

  #ifdef ANIMATE
  // Copy local pointer vars to globals
  targetDrawableCopy = in_drawable;
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
  gboolean is_alpha_image =  has_alpha(in_drawable);
  gboolean is_alpha_corpus = has_alpha(corpus_drawable);

  // Image adaption requires format indices
  // WAS  prepareImageFormatIndices(drawable, corpus_drawable, with_map, map_in_drawable);
  TFormatIndices formatIndices;

  guint map_count = (with_map? count_color_channels(map_in_drawable) : 0 );

  prepareImageFormatIndices(
    &formatIndices,
    count_color_channels(in_drawable),
    map_count,
    is_alpha_image,
    is_alpha_corpus,
    with_map
    );

  // Since 2.10, use gegl
  gegl_init (NULL, NULL);

  #ifdef ADAPT_SIMPLE
    /* Adapt Gimp to an engine with a simpler interface. */
    setDefaultParams(&parameters);
    ImageBuffer imageBuffer;
    ImageBuffer maskBuffer;

    // TODO change to new signature
    adaptGimpToSimple(in_drawable, &imageBuffer, &maskBuffer);  // From Gimp to simple
    g_printf("Here3\n");
    adaptSimpleAPI(&imageBuffer, &maskBuffer);        // From simple to existing engine API

  #else
    g_printerr("Gimp version %d\n", GIMP_MAJOR_VERSION);
    debug("adapt target/context");
    
    fetch_image_mask_map(in_drawable, &targetMap, formatIndices.total_bpp,
      &targetMaskMap,
      MASK_TOTALLY_SELECTED,
      map_out_drawable, formatIndices.map_start_bip);

      #ifdef ANIMATE
      clear_target_pixels(formatIndices.colorEndBip);  // For debugging, blacken so new colors sparkle
      #endif

    debug("adapt corpus");
    fetch_image_mask_map(corpus_drawable, &corpusMap, formatIndices.total_bpp,
      &corpusMaskMap,
      MASK_TOTALLY_SELECTED,
      map_in_drawable, formatIndices.map_start_bip);

    // TODO These are artifacts of earlier design, not used.
    free_map(&corpusMaskMap);
    free_map(&targetMaskMap);

    debug("adapt parameters");
    adaptPluginToLibraryParameters(pluginParameters, &engineParameters);

  #endif

  // After possible adaption, check size again
  g_assert((targetMap.width * targetMap.height) > 0); // Image is not empty
  g_assert((corpusMap.width * corpusMap.height) > 0); // Corpus is not empty

  // Done with adaption: now main image data in canonical pixmaps, etc.
  // Begin real work
  progressStart(_("synthesizing..."));

  debug("call engine");
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
    return _("The texture source is empty. Does any selection include non-transparent pixels?");
  else if  (result == IMAGE_SYNTH_ERROR_EMPTY_TARGET )
    return _("The output layer is empty. Does any selection have visible pixels in the active layer?");
  // else continue

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
  debug("post results");
  post_results_to_gimp(in_drawable, targetMap);

  /* Clean up */
  // Adapted
  free_map(&targetMap);
  free_map(&corpusMap);
  // GIMP
  // Since 2.10, not need to detach.
  // detach_drawables(in_drawable, corpus_drawable, map_in_drawable, map_out_drawable);
  gimp_progress_end();

  debug("return success");
  return "success";
}

#include "../resynth-config.h"

#include <glib.h>
#include <glib/gprintf.h>

#include "coordinates.h"
#include "engineTypes2.h"
#include "engineParams.h"
#include "imageSynthConstants.h"
#include "imageFormat.h"
#include "imageFormatIndicies.h"

#include "map.h"
#include "mapIndex.h"

#include "selection.h"

#include "stats.h"


/*
Functions for debugging.

Formerly inlined.
Now, we don't care what the performance is.

The engine is not reentrant when using these functions,
since use static vars.
*/


#ifdef SYNTH_DEBUG


// static vars.

guint countSourceTries = 0;
guint countTargetTries = 0;
guint total_targets = 0;  /* Total target attempts over all passes, barring short circuit. */

// remember the most recent kind of corpus point that bettered the previous best
guint bettermentStats[MAX_BETTERMENT_KIND];

guint integralColorChange = 0;



void
store_betterment_stats(tBettermentKind betterment_kind)
{
  bettermentStats[betterment_kind]+=1;
}

void
reset_color_change()
{
  integralColorChange = 0;
}

void
integrate_color_change(Coordinates position, TFormatIndices *indices, Map *image, Map* corpus, Coordinates bestMatchCoords)
{
  for(guint j=FIRST_PIXELEL_INDEX; j<indices->colorEndBip; j++)  // For all color pixelels (channels)
    integralColorChange += abs(pixmap_index(image, position)[j] - pixmap_index(corpus, bestMatchCoords)[j]);
}


/* Stats for this pass */
void
print_pass_stats(
  gint repeat,
  gint target_count,
  gint repeatCountBetters
  )
{
  g_printf("Repetition # %d targets %d bettered %d color change %d\n",
    repeat, target_count, repeatCountBetters, integralColorChange);
  g_printf("Cumulative processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}


void
print_processor_time()
{
  g_printf("Processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}


#ifdef OLD_DEBUG
  
This was formerly included source, in a context where the values were defined.
Now it is compiled separately, and the values are not defined.
The values now need to be passed.
And other changes required.

void
print_final_stats()
{
  g_printf("Target pixels %d\n", target_points_size);
  g_printf("Corpus pixels %d\n", corpus_points_size);
  g_printf("Target pixels tried %d\n", countTargetTries);
  g_printf("Corpus pixels tried %d\n", countSourceTries);
  /* Which part of the algorithm or heuristic found the source. */
  g_printf("Bettered by random %d\n", bettermentStats[RANDOM_CORPUS]);
  g_printf("Bettered by neighbor's source %d\n", bettermentStats[NEIGHBORS_SOURCE]);
  // g_printf("Bettered by neighbor itself %d\n", bettermentStats[NEIGHBOR_ITSELF]);
  // g_printf("Bettered by prior source %d\n", bettermentStats[PRIOR_REP_SOURCE]);
  g_printf("Not bettered %d\n", bettermentStats[NO_BETTERMENT]);

  g_printf("Perfect matches %d\n", bettermentStats[PERFECT_MATCH]);
  g_printf("Processor seconds %f\n", 1.0 * clock() / CLOCKS_PER_SEC );
}



/*
Dump methods.

These are for debugging, and are not called in release mode.
They are not statistics, but rather debugging information
*/

void
dump_parameters(const TImageSynthParameters *parameters)
{
  g_printf("Parameter: Corpus ID: %d\n", parameters->corpus_id);
  g_printf("Parameter: Neighbor count: %d\n", parameters->neighbours);
  g_printf("Parameter: Max tries: %d\n", parameters->trys);
  g_printf("Parameter: Use border: %d\n", parameters->use_border);
  g_printf("Parameter: Horiz tile: %d\n",parameters->h_tile);
}


void
dump_target_points()
{
  /* dump source for each target, in order in which synthed  */
  guint i;

  for (i=0; i<target_points_size; i++)
  {
    // c++ Coordinates position = target_points[i];
    Coordinates position = g_array_index(target_points, Coordinates, i);
    Coordinates position2 = get_source_of(position);
    g_printf("Target, source Coords: %d %d , %d %d\n", position.x, position.y, position2.x, position2.y);
  }
}

void
dump_max_grad()
{
  guint i;
  for(i=0; i<401; i++)
    g_printf("Grad %d max %d\n", i, max_cartesian_along_ray[i]);
}



/* Print the results for one attempt at resynthesizing a target point. For debug, study. */
void
dump_target_resynthesis(Coordinates position)
{
  if ( latestBettermentKind != NO_BETTERMENT) {
      g_printf("Point %d %d source %d %d best %d betterment %d \n", position.x, position.y, best_point.x, best_point.y, best, latestBettermentKind);
      /* myDebug4(position, target_points, "Delta color", image.at(position)[0] - corpus.at(best_point)[0] ); // One channel */
      gint distance = neighbours[n_neighbours-1].x*neighbours[n_neighbours-1].x + neighbours[n_neighbours-1].y*neighbours[n_neighbours-1].y;
      g_printf("Count neighbors %d max distance %d\n", n_neighbours, distance);
  }
}

#endif


#else


/* 
No debug code.
Empty functions, that linker will optimize away.
*/

void store_betterment_stats(tBettermentKind betterment_kind) {};
void reset_color_change() {};
void integrate_color_change(Coordinates position, TFormatIndices* indicies, Map *image, Map* corpus, Coordinates bestMatchCoords) {};
void dump_parameters       (const TImageSynthParameters *parameters) {};
void print_pass_stats      (gint repeat, gint target_count, gint repeatCountBetters) {};
void print_processor_time() {};
void print_final_stats() {};
void dump_target_points() {};
void dump_max_grad() {};
void dump_target_resynthesis(Coordinates position) {};


#endif
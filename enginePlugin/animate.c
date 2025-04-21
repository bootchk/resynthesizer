#include "../resynth-config.h"

#include <libgimp/gimp.h>

#include "engineTypes2.h"  // Pixelel
#include "coordinates.h"
#include "map.h"
#include "adaptGimp.h"

#include "animate.h"




#ifdef SYNTH_ANIMATE

static GimpDrawable *targetDrawableCopy;
static Map          *targetMapCopy;

/*
For debugging, we animate during processing.

Animation is a capabilility of the plugin, not the engine.
But the engine assists by clearing target pixels.
*/

void 
init_animate (GimpDrawable *targetDrawable, Map *targetMap)
{
  // Remember drawable and map to globals
  targetDrawableCopy = targetDrawable;
  targetMapCopy      = targetMap;
}

void 
animate_results(void)
{
  post_results_to_gimp(targetDrawableCopy, *targetMapCopy);
}


#else


void init_animate (GimpDrawable *targetDrawable, Map *targetMap) {};
void animate_results(void) { }


#endif
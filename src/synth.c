/*
Texture synthesis engine.

This is the outer "engine", a wrapper.
(The inner engine is engine.c.)
With simple API, for healing only from the full context of the target.

To make: 
- edit buildswitches.h to use glibProxy.h
- make -f Makefile.synth
*/
#include <stddef.h>  // size_t

// Non code defining, true headers: macros, declarations, and static inline functions
#include "imageBuffer.h"
#include "resynth-constants.h"

// synth always uses glibProxy
// Redefine parts of glib that we use
#include "glibProxy.h"  // glibProxy.c

#include "imageFormat.h"
#include "map.h"  // header for resynth-map-types.h included by engine.c
#include "engineParams.h" // engineParams.c
#include "mapIndex.h" // inline funcs depending on map.h
#include "engine.h" // engine.c


// Code defining, could be compiled separately
#include "adaptSimple.h"




extern int
imageSynth(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA four Pixelels
  ImageBuffer * mask,         // IN one mask Pixelel
  TImageFormat imageFormat
  // EngineControls temporarily not passed: see setDefaultParams
  )
{
  Parameters parameters;
  Map targetMap;
  Map targetMaskMap;
  Map corpusMap;
  Map corpusMaskMap;
  TFormatIndices formatIndices;
  
  // Sanity: mask and imageBuffer same dimensions
  assert(imageBuffer->width == mask->width);
  assert(imageBuffer->height == mask->height);
  
  setDefaultParams(&parameters);
  
  // TODO catch exception
  prepareImageFormatIndicesFromFormatType(&formatIndices, imageFormat);
  /*
  prepareImageFormatIndices(&formatIndices
    &formatIndices,
    3,    // count color pixelels
    0,    // no map colors
    TRUE, // is_alpha_target,
    TRUE, // is_alpha_source,
    FALSE // isMap
  );
  */
  
  // prepareDefaultFormatIndices(&formatIndices);
  
  // Adapt: put (imageBuffer, mask) into pixmaps etc.
  // pixmaps were global.
  adaptSimpleAPI(imageBuffer, mask, 
    &targetMap, &targetMaskMap,
    &corpusMap, &corpusMaskMap,
    countPixelelsPerPixelForFormat(imageFormat)
    );
 
  int result = engine(
    parameters,
    &formatIndices, 
    &targetMap, 
    &corpusMap,
    &targetMaskMap,
    &corpusMaskMap
    );
  
  // Now the synthesized pixels are in the unmasked portion of the global image pixmap.
  // Post adapt: in imageBuffer, replace target pixels from global image pixmap
  antiAdaptImage(
    imageBuffer, 
    &targetMap, // !!! this is the global pixmap named "image"
    1,      // Offset in source pixel (skip the interleaved mask pixelel)
    /*
    !!! We didn't synthesize an alpha pixelel.
    But it is still the same in the internal buffer.
    Go ahead and move it back, simpler than trying to omit alpha from the move.
    */
    countPixelelsPerPixelForFormat(imageFormat)       
    );
  /*
  TODO Still a question here whether the alpha still in imageBuffer is correct,
  if we have changed the color pixels and they are pre-multiplied by alpha.
  */
      
  return result;  // TODO return error code instead of assertions
}



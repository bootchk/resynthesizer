/*
Texture synthesis engine.

This is the outer "engine", a wrapper.
The inner engine is engine.c.

With simple API, for healing only from the full context of the target.
*/
#include <stddef.h>  // size_t

// Non code defining, true headers: macros, declarations, and static inline functions
#include "imageBuffer.h"
#include "resynth-constants.h"
#include "glibProxy.h"
#include "map.h"
#include "engineParams.h"
#include "mapIndex.h"
#include "engine.h"

// Code defining includes, could be compiled separately
#include "adaptSimple.h"


int
main(
  ImageBuffer * imageBuffer, // IN/OUT RGBA four Pixelels
  ImageBuffer * mask   // IN one Pixelel
  // RGBAFormat temporarily not passed: imageBuffer must be RGBA and mask must be a single byte
  // EngineControls temporarily not passed: see setDefaultParams
  )
{
  Parameters parameters;
  
  // Sanity: mask and imageBuffer same dimensions
  assert(imageBuffer->width == mask->width);
  assert(imageBuffer->height == mask->height);
  
  setDefaultParams(&parameters);
  
  // Adapt: put (imageBuffer, mask) into global pixmaps etc.
  adaptSimpleAPI(imageBuffer, mask);
 
  int result = engine(parameters);
  
  // Now the synthesized pixels are in the unmasked portion of the global image pixmap.
  // Post adapt: in imageBuffer, replace target pixels from global image pixmap
  antiAdaptImage(
    imageBuffer, 
    &image, // !!! this is the global pixmap named "image"
    1,      // Offset in source pixel (skip the interleaved mask pixelel)
    3       // !!! pixelel_count is 3, we didn't synthesize an alpha pixelel, still in imageBuffer
    );
  // !!! The above assumes that RGB precedes A.  If that changes, need code changes.
  /*
  Still a question here whether the alpha still in imageBuffer is correct,
  if we have changed the color pixels and they are pre-multiplied by alpha.
  */
  
      
  return result;  // TODO return error code instead of assertions
}

/*
Texture synthesis engine.

This is the outer "engine", a wrapper. (The inner engine is engine.c.)

With simple API, for healing only from the full context of the target.
That is, it only takes one image.
The inner engine takes four images.

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
  TImageFormat imageFormat,
  TImageSynthParameters* parameters  // or NULL to use defaults
  // EngineControls temporarily not passed: see setDefaultParams
  )
{
  Map targetMap;
  Map corpusMap;
  TFormatIndices formatIndices;
  int error;
  
  // Sanity: mask and imageBuffer same dimensions
  if (imageBuffer->width != mask->width || imageBuffer->height != mask->height)
    return IMAGE_SYNTH_ERROR_IMAGE_MASK_MISMATCH;
  
  // Use defaults if NULL parameters
  if (!parameters) {
    static TImageSynthParameters defaultParameters;
    setDefaultParams(&defaultParameters);
    parameters = &defaultParameters;
    }
  
  error = prepareImageFormatIndicesFromFormatType(&formatIndices, imageFormat);
  if ( error ) return error;
  
  // Adapt: put (imageBuffer, mask) into pixmaps etc.
  adaptSimpleAPI(imageBuffer, mask, 
    &targetMap,
    &corpusMap,
    countPixelelsPerPixelForFormat(imageFormat)
    );
  
  error = engine(
    *parameters,
    &formatIndices, 
    &targetMap, 
    &corpusMap
    );
  
  if (! error)
  {
    // assert targetMap holds results
    
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
  }
  
  // Cleanup internal malloc's done by adaption
  // See above, masks already freed
  free_map(&targetMap);
  free_map(&corpusMap);
   
  return error;
}



/*
Texture synthesis engine.

This is the outer "engine", a wrapper. (The inner engine is engine.c.)

SimpleAPI, for healing only from the full context of the target.
Oonly takes one image.

FullAPI, takes four images.

The inner engine takes four images.

The engine is reentrant, thread safe.
However, an image passed must not be shared between threads in the caller.
The engine does not lock the image passed and does write to it at completion.

To make: 
- edit buildswitches.h to use glibProxy.h
- make -f Makefile.synth

  Copyright (C) 2010, 2011  Lloyd Konneker

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
#include <stddef.h>  // size_t

// Non code defining, true headers: macros, declarations, and static inline functions
#include "imageBuffer.h"
#include "imageSynthConstants.h"

// synth always uses glibProxy
// Redefine parts of glib that we use
#include "glibProxy.h"  // glibProxy.c

#include "imageFormat.h"
#include "imageFormatIndicies.h"
#include "map.h"  // header for mapOps.h included by engine.c
#include "engineParams.h" // engineParams.c
#include "engine.h" // engine.c


// Code defining, could be compiled separately
#include "mapIndex.h" // inline funcs depending on map.h
#include "adaptSimple.h"  // requires mapIndex.h




extern int
imageSynth(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA four Pixelels
  ImageBuffer * mask,         // IN one mask Pixelel
  TImageFormat imageFormat,
  TImageSynthParameters* parameters,  // or NULL to use defaults
  void (*progressCallback)(int, void*),   // int percentDone, void *contextInfo
  void *contextInfo,
  int *cancelFlag // flag to check periodically for abort
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
    &corpusMap,
    progressCallback,
    contextInfo,
    cancelFlag
    );
  
  if (! error && ! (*cancelFlag))
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

extern int
imageSynth2(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA four Pixelels
  ImageBuffer * mask,         // IN one mask Pixelel
  ImageBuffer * mask2,         // IN one mask Pixelel
  TImageFormat imageFormat,
  TImageSynthParameters* parameters,  // or NULL to use defaults
  void (*progressCallback)(int, void*),   // int percentDone, void *contextInfo
  void *contextInfo,
  int *cancelFlag // flag to check periodically for abort
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
  adaptSimpleAPI2(imageBuffer, mask, mask2,
    &targetMap,
    &corpusMap,
    countPixelelsPerPixelForFormat(imageFormat)
    );
  
  error = engine(
    *parameters,
    &formatIndices, 
    &targetMap, 
    &corpusMap,
    progressCallback,
    contextInfo,
    cancelFlag
    );
  
  if (! error && ! (*cancelFlag))
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




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

// synth always uses glibProxy
// Redefine parts of glib that we use
#include "glibProxy.h"  // glibProxy.c

#include "map.h"  // header for resynth-map-types.h included by engine.c
#include "engineParams.h" // engineParams.c
#include "mapIndex.h" // inline funcs depending on map.h
#include "engine.h" // engine.c


// Code defining, could be compiled separately
#include "adaptSimple.h"


// Throwaway setter of pixel indices
// TODO derive from passed in PixelFormat and return a struct, not globals, for reentrancy
void
prepareDefaultFormatIndices()
  /*
  Default is RGBA
  For testing.
  
  Engine pixel is:
  MRGBA
  012345
  */
{
// !!! MASK_PIXELEL_INDEX is a constant (0); mask pixelel is first, always.

img_match_bpp = 3;  // Match RGB but not A
color_end_bip = 4;  // Pixelels 1-3 (2nd through 4th)

map_match_bpp = 0;   // No map pixelels
map_start_bip = 5;
map_end_bip = 5;

total_bpp = 5;
}




int
synth(
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
  
  prepareDefaultFormatIndices();
  
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

void
dumpBuffer(ImageBuffer* buffer)
{
	for (int row=0; row<buffer->height; row++)
	{
		for (int col=0; col<buffer->width; col++)
		{
			for (int pixelel=0; pixelel<4; pixelel++)
				printf("%x", buffer->data[row*buffer->rowBytes + col*4 + pixelel]);
		  printf(" ");
		}
	printf("\n");
	}
}

// Dump engine's image pixmap
dumpImage()
{
	for (int i=0; i<45; i++)
		printf("%x", image.data[i]);
}

// Test harness, small image
int main(
	)
{
	unsigned char image[42] = {
		0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,	// 3*RGBA and 2 trailing pad byte
		0,0,0,0, 1,1,1,1, 0,0,0,0, 0,0,
		0,0,0,0, 0,0,0,0, 0,0,0,8, 0,0
		};
		
	unsigned char mask[12] = {
		0,0,		0, 0, 	// 3*mask and trailing pad byte
		0,0xFF,	0, 0,	// 0xFF == totally selected
		0,0,		0, 0
		};
		 
	ImageBuffer testImage = { &image, 3, 3, 14 };
	ImageBuffer testMask= { &mask, 3, 3, 4 };
	
	// 13 == 3 * 4 pixelels + one byte padding
	// 4 == 3 * 1 pixelels + one byte padding
	
	synth(&testImage, &testMask);
	dumpImage();
	dumpBuffer(&testImage);
	
	// print two pixels (0,1) and (1,1) 
	printf(" %x %x %x %x \n %x %x %x %x \n", image[4],image[5],image[6],image[7],  image[17],image[18],image[19],image[20]);
	/*
	(1,1) should be changed to 0,0,0,1
	i.e. the alpha byte 1 should not be changed
	but the color should be 0,0,0 from unmasked region
	*/
}


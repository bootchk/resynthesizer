/*
Texture synthesis engine.

This is the outer "engine", a wrapper.
The inner engine is engine.c.

With simple API, for healing only from the full context of the target.
*/
#include <stddef.h>  // size_t
#include <stdio.h>	// printf

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
alpha_bip = 4;			// Alpha bip must be defined: read but not synthesize
map_match_bpp = 0;  // No map pixelels
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
	int row;
	int col;
	int pixelel;
	
	for (row=0; row<buffer->height; row++)
	{
		for (col=0; col<buffer->width; col++)
		{
			for (pixelel=0; pixelel<4; pixelel++)
				printf("%02hhx ", buffer->data[row*buffer->rowBytes + col*4 + pixelel]);
		  printf(" ");
		}
	printf("\n");
	}
}

// Dump engine's image pixmap
void
dumpImage(unsigned int count)
{
	int i;
	
	for (i=0; i<count; i++)
	{
		// hh is char, x is hex notation, O is left pad with zeroes, 2 is field width
		printf("%02hhx ", (char) image.data->data[i]);
		if ( i % 5 == 4)
			printf("\n");	// spaces after every 5 pixelels, ie after a pixel
	}
	printf("\n");
}

// Test harness, small images
// !!! Here Alpha FF is total opacity.  Alpha 0 is total transparency.
int main(
	)
{
	// A 3x3 image where the center pixel will be synthesized
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
	
	// A 1x3 image where the middle pixel will be synthesized.
	// First pixel opaque
	// Third pixel transparent
	unsigned char image2[14] = {
		128,128,128,0xFF, 1,1,1,1, 0,0,0,0, 0,0	// 3*RGBA and 2 trailing pad byte
		};
	unsigned char mask2[12] = {
		0,0xFF,0, 0 	// 3*mask and trailing pad byte
		};
	
	// A 1x3 image where the all pixel transparent and the middle pixel will be synthesized.
	unsigned char image3[14] = {
		128,128,128,0, 1,1,1,1, 0,0,0,0, 0,0	// 3*RGBA and 2 trailing pad byte
		};
		
	ImageBuffer testImage = { (unsigned char*) &image, 3, 3, 14 };
	ImageBuffer testMask= { (unsigned char*) &mask, 3, 3, 4 };
	
	// !!! Note width, height in that order
	ImageBuffer testImage2 = { (unsigned char*) &image2, 3, 1, 14 };
	ImageBuffer testMask2= { (unsigned char*) &mask2, 3, 1, 4 };
	
	ImageBuffer testImage3 = { (unsigned char*) &image3, 3, 1, 14 };
	
	// 13 == 3 * 4 pixelels + one byte padding
	// 4 == 3 * 1 pixelels + one byte padding
	
	printf("\nTest center pixel synthesized but alpha unchanged.\n");
	printf("Before\n");
	dumpBuffer(&testImage);
	synth(&testImage, &testMask);
	// dumpImage(45);	// 45 pixelels 3x3x5
	printf("After\n");
	dumpBuffer(&testImage);

	/*
	(1,1) should be changed to 0,0,0,1
	i.e. the alpha byte 1 should not be changed
	but the color should be 0,0,0 from unmasked and partially opaque region.
	Other pixels are not changed.
	*/
	
	printf("\nTest full transparency\n");
	printf("Before\n");
	dumpBuffer(&testImage2);
	synth(&testImage2, &testMask2);
	// dumpImage(15);	// 1x3x5
	printf("After\n");
	dumpBuffer(&testImage2);
	
	printf("\nTest corpus empty because is fully transparent\n");
	printf("Doesn't raise exception (shouldn't it?)\n");
	printf("Finds no pixels in corpus with any opacity and fails to synthesize center pixel.\n");
	printf("Before\n");
	dumpBuffer(&testImage3);
	synth(&testImage3, &testMask2);
	// dumpImage(15);	// 1x3x5
	printf("After\n");
	dumpBuffer(&testImage3);
	
	return(0);
}


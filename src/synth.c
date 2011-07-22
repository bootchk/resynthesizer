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
#include <stdio.h>	// printf

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




int
synth(
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

void
dumpBuffer(
  ImageBuffer* buffer,
  unsigned int pixelelsPerPixel
  )
{
	int row;
	int col;
	int pixelel;
	
	for (row=0; row<buffer->height; row++)
	{
		for (col=0; col<buffer->width; col++)
		{
			for (pixelel=0; pixelel<pixelelsPerPixel; pixelel++)
				printf("%02hhx ", buffer->data[row*buffer->rowBytes + col*pixelelsPerPixel + pixelel]);
		  printf(" ");
		  // TODO print pad bytes
		}
	printf("\n");
	}
}

// Dump engine's image pixmap
void
dumpImage(
  unsigned int count,
  Map targetMap)
{
	int i;
	
	for (i=0; i<count; i++)
	{
		// hh is char, x is hex notation, O is left pad with zeroes, 2 is field width
		printf("%02hhx ", (char) targetMap.data->data[i]);
		if ( i % 5 == 4)
			printf("\n");	// spaces after every 5 pixelels, ie after a pixel
	}
	printf("\n");
}

static void test(
  char * description,
  ImageBuffer* buffer,
  ImageBuffer* mask,
  TImageFormat format,
  unsigned int pixelelCount,
  char * expect
  )
{
  printf("\n");  printf(description); printf("\n");
	printf("Before:\n");
	dumpBuffer(buffer, pixelelCount);
	synth(buffer, mask, format);
	printf("Expect:\n"); printf(expect); printf("\n");
	printf("Result:\n");
	dumpBuffer(buffer, pixelelCount);
}


// Test harness, small images
// !!! Here Alpha FF is total opacity.  Alpha 0 is total transparency.
int main(
	)
{
	// A 3x3 image where the center pixel will be synthesized
	unsigned char image[42] = {
		0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,	// 3*RGBA and 2 trailing pad byte = 14
		0,0,0,0, 1,1,1,1, 0,0,0,0, 0,0,
		0,0,0,0, 0,0,0,0, 0,0,0,8, 0,0
		};
		
	unsigned char mask[12] = {
		0,0,		0, 0, 	// 3*mask and trailing pad byte = 4
		0,0xFF,	0, 0,	// 0xFF == totally selected
		0,0,		0, 0
		};
	
	// A 1x3 image where the middle pixel will be synthesized.
	// First pixel opaque
	// Third pixel transparent
	unsigned char image2[14] = {
		128,128,128,0xFF, 1,1,1,1, 0,0,0,0, 0,0	// 3*RGBA and 2 trailing pad byte = 14
		};
	unsigned char mask2[4] = {
		0,0xFF,0, 0 	// 3*mask and 1 trailing pad byte = 4
		};
		
	unsigned char mask3[8] = {
		0,   0,0, 0, 	// 3*mask and 1 trailing pad byte = 4
		0,0xFF,0, 0
		};
	
	// A 1x3 image where the all pixel transparent and the middle pixel will be synthesized.
	unsigned char image3[14] = {
		128,128,128,0, 1,1,1,1, 0,0,0,0, 0,0	// 3*RGBA and 2 trailing pad byte
		};
		
	// A 2x3 RGB image 
	unsigned char imageRGB[22] = {
		128,128,128, 1,1,1, 2,2,2, 5,5,	// 3*RGB and 2 trailing pad byte = 11
		64,64,64, 4,4,4, 3,3,3, 7,7
		};
		
	// 1x3 Grey alpha
	// First pixel opaque, second to be synthesized, third transparent
	unsigned char imageGrayA[8] = {
		128,0xFF, 64,1, 1,0, 0,0	// 3*GA and 2 trailing pad byte = 8
		};
		
  // 1x3 Grey 
	unsigned char imageGray[5] = {
		128, 64, 1, 0,0	// 3*GA and 2 trailing pad byte = 8
		};
	
	// !!! Note width, height, rowBytes in that order
	ImageBuffer testImage = { (unsigned char*) &image, 3, 3, 14 };
	ImageBuffer testImage2 = { (unsigned char*) &image2, 3, 1, 14 };
	ImageBuffer testImage3 = { (unsigned char*) &image3, 3, 1, 14 };
	ImageBuffer testImageRGB = { (unsigned char*) &imageRGB, 3, 2, 11 };
	ImageBuffer testImageGrayA = { (unsigned char*) &imageGrayA, 3, 1, 8 };
	ImageBuffer testImageGray = { (unsigned char*) &imageGray, 3, 1, 5 };
	
	ImageBuffer testMask= { (unsigned char*) &mask, 3, 3, 4 };
	ImageBuffer testMask2= { (unsigned char*) &mask2, 3, 1, 4 };
	ImageBuffer testMask3= { (unsigned char*) &mask3, 3, 2, 4 };
	
	printf("\nTest center pixel synthesized but alpha unchanged.\n");
	printf("Before\n");
	dumpBuffer(&testImage, 4);
	synth(&testImage, &testMask, T_RGBA);
	// dumpImage(45);	// 45 pixelels 3x3x5
	printf("After\n");
	dumpBuffer(&testImage, 4);

	/*
	(1,1) should be changed to 0,0,0,1
	i.e. the alpha byte 1 should not be changed
	but the color should be 0,0,0 from unmasked and partially opaque region.
	Other pixels are not changed.
	*/
	
	// Note these tests destroy the input image, can't be used twice
	
  test("Test mix of full transparency and opaque", &testImage2, &testMask2, T_RGBA, 4,
	  "80 80 80 ff  80 80 80 01  00 00 00 00");
	
	// TODO engine should throw error? Corpus is entirely transparent
  test("All transparent", &testImage3, &testMask2, T_RGBA, 4,
	  "should be unchanged.");
	
	test("Test RGB w/o alpha", &testImageRGB, &testMask3, T_RGB, 3,
	  "80 80 80  01 01 01  02 02 02\n40 40 40  01 01 01  03 03 03");
	  
	test("Test Gray w/ alpha", &testImageGrayA, &testMask2, T_GrayA, 2,
	  "80 ff  80 01  01 00");
	  
	test("Test Gray w/o alpha", &testImageGray, &testMask2, T_Gray, 1,
	  "80  01  01");
  
	return(0);
}


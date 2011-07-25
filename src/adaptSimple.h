/*
Adapts SimpleAPI to existing engine API.

SimpleAPI takes only one image and mask.
The one image is both the target/context and the corpus/source.
The mask selects the target.
The inverse of the mask selects the corpus.

Adaption to existing API is:
- duplicating image into corpus with inverted mask.
- copying to global vars used by existing engine
- converting from row-padded pixmaps to unpadded pixmaps
- converting from separate image and mask pixmaps to an interleaved pixmap

Some of this might be discarded.
It will certainly be changed because the final engine
will not use static global vars so it is reentrant.
*/

#include <stdlib.h>

/*
Adapt pixmap that is row padded to pixmap:
- NOT row padded
- optionally offset color pixelels beyond 0th byte (an interleaved mask byte)
Convert between slightly different pixmap types.

!!!! This is not fully general.  Strides should be passed in.
It just happens to work for now: when we are moving all pixels of the source.
*/
static void
adaptImage(
  ImageBuffer * image,              // IN image: target or corpus drawable
  Map          *pixmap,             // OUT NON-rowpadded pixmap
  guint        offset,              // IN Offset in destination pixel. !!! Can be zero
  guint        pixelel_count        // IN count pixelels to move
  )
{
  guint row;
  guint col;
  guint pixelel;
  
  guint srcPixel;
  guint destPixel;
  
  /* 
  Copy SOME of the pixels from img sequence to our pixmap (optionally exclude alpha).
  Allow for row padding in source.
  OFFSET pixels in destination. 
  */
  /* 
  Note the src and dest pixel indices are incremented differently, ie pixel strides different. 
  src: row padded: row stride greater than pixels*pixelelsperpixel
  dest: not row padded 
  */
  destPixel = offset; // dest pixel index starts at offset
  guint srcPixelStride = pixelel_count;
  guint destPixelStride = pixelel_count + offset; // dest is offset
  
  for(row=0; row<image->height; row++) 
  { 
    srcPixel = row * image->rowBytes; // srcPixel index computed for START of each row
    // dest pixel index continues at next byte
    for(col=0; col<image->width; col++) 
    {
      for (pixelel=0; pixelel < pixelel_count; pixelel++)
        // Copy one pixelel, but possibly offset in destination
        g_array_index(
          pixmap->data, 
          Pixelel, 
          destPixel+pixelel
          )
          = 
          image->data[srcPixel+pixelel];  // src data is array of uchar
          
      srcPixel += srcPixelStride;
      destPixel += destPixelStride;
    }
  }
}


/*
Reverse of above: copy from engine existing API pixmap format to external API pixmap format.
Src is non row padded and offset
Dest is row padded.

This is used:
- for the test harness from a GIMP plugin on Linux
- in the final simpleAPI to write results back to the passed in ImageBuffer->data

!!! This is not fully general, the strides are hard coded below.
*/
static void
antiAdaptImage(
  ImageBuffer*  imageBuffer,      // OUT image: target or corpus drawable
  Map*          pixmap,           // IN NON-rowpadded pixmap
  guint         offset,           // IN Offset in internal source pixel
  guint         pixelel_count     // IN count pixelels to move 
        // !!! Not size of pixel in src or dest since we omit alpha
  )
{
  guint row;
  guint col;
  guint pixelel;
  
  // Indexes that move by pixel strides, but might point offset inside a pixel
  guint srcPixel;
  guint destPixel;
  
  /* 
  Copy ALL of the pixels from our pixmap to buffer(including alpha which is unaltered.)
  Row pad destination.
  OFFSET pixels in source. 
  */
  /* 
  Note the src and dest pixel indices are incremented differently, ie pixel strides different. 
  src: NOT row padded
  dest: row padded 
  */
  
  /*
  Assert imageBuffer is properly initialized with dimensions and data.
  The data can be unitialized, or valid image data to be overwritten here.
  */
  
  srcPixel = offset;
  // source stride is offset plus pixels moved
  guint srcPixelStride = pixelel_count + offset;  
  guint destPixelStride = pixelel_count;
  
  for(row=0; row<imageBuffer->height; row++) 
  { 
    destPixel = row * imageBuffer->rowBytes; // destPixel index computed for START of each row
    // dest pixel index continues at next byte
    for(col=0; col<imageBuffer->width; col++) 
    {
      for (pixelel=0; pixelel < pixelel_count; pixelel++)
        // Copy one pixelel, but offset in src
        imageBuffer->data[destPixel+pixelel]
        = 
        g_array_index(
          pixmap->data, 
          Pixelel,
          srcPixel+pixelel
          );
          
       srcPixel += srcPixelStride;
       destPixel += destPixelStride;
     }
   }
}

/*
For test purposes, initialize buffer and antiAdapt into the buffer.

This is used:
- for the test harness from a GIMP plugin on Linux

The test harness adapts in order:
- 1 from GIMP to existing API
- 2 from existing to simple API (this adaption is throwaway, not used except in testing)
- 3 from simple to existing API
initBufferAndAntiAdapt() is step 2.
*/
static void
initBufferAndAntiAdapt(
  ImageBuffer* imageBuffer,        // OUT image: target or corpus drawable
  Map*         pixmap,             // IN NON-rowpadded pixmap
  guint        offset,             // IN Offset in destination pixel
  guint        pixelel_count       // IN count pixelels to move
  )
{
  guint rowBytes;
  
  // !! imageBuffer struct exists but it is unitialized
  
  // Use dimensions of IN pixmap to size the buffer
  // !! Note the depth of the buffer is only the pixelels moved, 
  // not the pixelel count of pixmap, which may be more.
  rowBytes = pixmap->width * pixelel_count + 11;  // arbitrarily pad row, testing
  int reservedSize = rowBytes * pixmap->height;
  
  // Set attributes imageBuffer
  imageBuffer->data = calloc(reservedSize, 1);
  g_assert(imageBuffer->data);
  imageBuffer->width = pixmap->width;
  imageBuffer->height = pixmap->height;
  imageBuffer->rowBytes = rowBytes;

  antiAdaptImage(
    imageBuffer,
    pixmap,
    offset,
    pixelel_count
    );
}




/*
Adapt imageBuffer and its mask to our internal pixmap.

Note the mask is kept separately and also interleaved into the pixmap,
because a preparation step uses the mask separately.
See prepareTarget.
*/
void
adaptImageAndMask(
  ImageBuffer * image,    // IN 
  ImageBuffer *   mask,   // IN 
  Map *imagePixmap,       // OUT our color pixmap of drawable, w/ interleaved mask
  Map *maskPixmap,        // OUT our selection bytemap (only one channel ie pixelel ie byte ie depth)
  guint pixelelPerPixel    // IN pixelels in the image e.g. 4 for RGBA
  ) 
{
  // Note our internal map includes mask pixelel so +1
  
  // Both OUT pixmaps same 2D dimensions.  
  // imagePixmap includes a mask byte.
  new_pixmap(imagePixmap, image->width, image->height, pixelelPerPixel+1 );
  
  // Get color, alpha channels.  Offset them past mask byte. 4 bytes of RGBA.
  adaptImage(image, imagePixmap, FIRST_PIXELEL_INDEX, pixelelPerPixel);
  
  new_pixmap(maskPixmap, image->width, image->height, 1 );
  
  // Get mask channel. Offset by 0 (mask byte first.)  Byte count 1.
  adaptImage(mask, maskPixmap, 0, 1);
  
  // Assert two mallocs need to be freed
}


/*
Adapt simpleAPI to existingAPI:
- Duplicate the single image of the simpleAPI into two images (target and corpus) of existingAPI.
- Invert the mask of the corpus
- Interleave the masks into the main pixmap (but keep the mask pixmap also.)
Inner engine (existingAPI) is more general and wants separate corpus and separate selection masks.
*/

void
adaptSimpleAPI(
  ImageBuffer * imageBuffer,
  ImageBuffer * maskBuffer,
  Map * targetMap,
  Map * corpusMap,
  guint pixelelPerPixel // In imageBuffer
  )
{
  // Assert image and mask are same size, not need to initialize empty mask with a value
  // (as is the case when mask is smaller).
  Map targetMaskMap;
  
  // Copy image and mask to global pixmaps
  adaptImageAndMask(
    imageBuffer, 
    maskBuffer,
    targetMap, 
    &targetMaskMap, 
    pixelelPerPixel
    );
  
  // For performance (cache memory locality), interleave mask into pixmap.
  interleave_mask(targetMap, &targetMaskMap);  /* Interleave mask byte into our Pixels */
  free_map(&targetMaskMap);
  
  
  // Duplicate image to corpus with inverted mask
  Map corpusMaskMap;
  
  adaptImageAndMask(
    imageBuffer, 
    maskBuffer,
    corpusMap,
    &corpusMaskMap,
    pixelelPerPixel
    );
  
  // !!!! 
  // For the simple API,  invert corpus mask: corpus is inverse of target selection
  invert_bytemap(&corpusMaskMap);
  
  // For performance (cache memory locality), interleave mask into pixmap.
  interleave_mask(corpusMap, &corpusMaskMap);  /* Interleave mask byte into our Pixels */
  free_map(&corpusMaskMap);
  
  // assert two mallocs
}




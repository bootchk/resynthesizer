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
*/
static void
adaptImage(
  ImageBuffer * image,              // IN image: target or corpus drawable
  Map          *pixmap,             // OUT NON-rowpadded pixmap
  guint        offset,              // IN Offset in destination pixel
  guint        pixelel_count        // IN count pixelels to move
  )
/*
  Map          *mask,               // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel      default_mask_value   // IN default value for any created mask
  )
*/
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
  
  for(row=0; row<image->height; row++) 
  { 
    srcPixel = row * image->rowBytes; // srcPixel index computed for START of each row
    // dest pixel index continues at next byte
    for(col=0; col<image->width; col++) 
    {
      for (pixelel=0; pixelel < pixelel_count; pixelel++)
        // Copy one pixelel, but offset in destination
        // TODO is this one byte?????
        g_array_index(
          pixmap->data, 
          Pixelel, 
          destPixel+pixelel
          )
          = 
          image->data[srcPixel+pixelel];  // src data is array of uchar
          
       srcPixel+= pixelel_count;  // Src pixel stride
       destPixel += (pixelel_count + offset); // Dest pixel stride
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
*/
static void
antiAdaptImage(
  ImageBuffer*  imageBuffer,      // OUT image: target or corpus drawable
  Map*          pixmap,           // IN NON-rowpadded pixmap
  guint         offset,           // IN Offset in source pixel
  guint         pixelel_count     // IN count pixelels to move
  )
{
  guint row;
  guint col;
  guint pixelel;
  
  guint srcPixel;
  guint destPixel;
  
  /* 
  Copy SOME of the pixels from our pixmap to buffer(optionally exclude alpha).
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
          
       srcPixel += (pixelel_count + offset);  // Src pixel stride
       destPixel += pixelel_count; // Dest pixel stride
     }
   }
}

/*
For test purposes, initialize buffers and antiAdapt into the buffers.

This is used:
- for the test harness from a GIMP plugin on Linux

The test harness adapts in order:
- 1 from GIMP to existing API
- 2 from existing to simple API
- 3 from simple to existing API
This is used in step 2.
*/
static void
createBuffersAndAntiAdapt(
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
  rowBytes = pixmap->width * pixelel_count + 11;  // arbitrarily pad row
  int reservedSize = rowBytes * pixmap->height;
  
  // Set attributes imageBuffer
  imageBuffer->data = calloc(reservedSize, 1);
  g_assert(imageBuffer->data);
  imageBuffer->width = pixmap->width;
  imageBuffer->height = pixmap->height;
  imageBuffer->rowBytes = rowBytes;

}




/*
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
  guint pixelel_count,    // IN total count mask+image+map Pixelels in our Pixel
  Pixelel default_mask_value  // IN default value for any created mask
  ) 
{
  /* Both OUT pixmaps same 2D dimensions.  Depth pixelel_count includes a mask byte. */
  new_pixmap(imagePixmap, image->width, image->height, 5 /* pixelel_count */ );
  
  // Get color, alpha channels.  Offset them past mask byte. 
  adaptImage(image, imagePixmap, FIRST_PIXELEL_INDEX, 4); // TODO 4 is hardcoded for RGBAFormat
  
  new_pixmap(maskPixmap, image->width, image->height, 1 /* pixelel_count */ );
  
  // Get mask channel. Offset by 0.  Byte count 1.
  adaptImage(mask, maskPixmap, 0, 1);
}


void
adaptSimpleAPI(
  ImageBuffer * imageBuffer,
  ImageBuffer * maskBuffer
  )
{
  // Copy image and mask to global pixmaps
  adaptImageAndMask(
    imageBuffer, 
    maskBuffer,
    &image,       // global in engine.c
    &image_mask,  // "
    5,    // total count mask+image Pixelels in our Pixel
    MASK_TOTALLY_SELECTED
    );
  
  // For performance (cache memory locality), interleave mask into pixmap.
  interleave_mask(&image, &image_mask);  /* Interleave mask byte into our Pixels */
  
  // Duplicate image to corpus with inverted mask
  // Inner engine is more general and wants separate corpus with separate selection mask.
  adaptImageAndMask(
    imageBuffer, 
    maskBuffer,
    &corpus,       // global in engine.c
    &corpus_mask,        // "
    5,    // total count mask+image Pixelels in our Pixel
    MASK_TOTALLY_SELECTED
    );
  
  // !!!! For the simple API,  invert corpus mask: corpus is inverse of target selection
  invert_bytemap(&corpus_mask);
  
  // For performance (cache memory locality), interleave mask into pixmap.
  interleave_mask(&corpus, &corpus_mask);  /* Interleave mask byte into our Pixels */
}




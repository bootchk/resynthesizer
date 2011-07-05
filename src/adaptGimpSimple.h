/*
Adapt GIMP to SimpleAPI

Probably throwaway, just harness for testing SimpleAPI from GIMP.
*/



void adaptAPIToSimple(
  Map*          pixmap,
  ImageBuffer*  buffer,
  int           srcOffset,
  int           byteCount
  )
{
  // malloc and set all fields of buffer
  // and put pixmap into row-padded ImageBuffer
  createBuffersAndAntiAdapt(buffer, pixmap, srcOffset, byteCount);
}



void
adaptImageToBuffer(
  GimpDrawable *drawable,
  Map          *image,
  Map          *image_mask,
  ImageBuffer  *imageBuffer,
  ImageBuffer  *maskBuffer
  )
{
  // Adapt GIMP to existing API
  // Many parameters are global vars
  fetch_image_mask_map(drawable, image, total_bpp, image_mask, MASK_TOTALLY_SELECTED, 
      NULL /*map_out_drawable*/, map_start_bip);
      
  // Adapt existing API to SimpleAPI
  adaptAPIToSimple(image, imageBuffer, 1, 4);
  adaptAPIToSimple(image_mask, maskBuffer, 0, 1);
  
  // Discard, since caller will re adapt SimpleAPI to existing API
  free_map(image);
  free_map(image_mask);
}



void
adaptGimpToSimple(
  GimpDrawable *image_drawable, 
  ImageBuffer* imageBuffer, 
  ImageBuffer* maskBuffer
  )
{
  // Adapt image from GIMP drawable to ImageBuffer
  
  // Only works for RGBA
  g_assert(image_drawable->bpp == 4);
  
  // Assert buffers have NULL data
  adaptImageToBuffer(
    image_drawable,
    &image, // global
    &image_mask, // global
    imageBuffer,
    maskBuffer
    );
  // Caller will adapt to SimpleAPI
  // Caller does not need separate corpus
}

/*
Adapt GIMP to SimpleAPI

Probably throwaway, just harness for testing SimpleAPI driven from GIMP.
*/



void
adaptImageToBuffer(
  GimpDrawable *drawable,
  Map          *image,
  Map          *image_mask,
  ImageBuffer  *imageBuffer,
  ImageBuffer  *maskBuffer
  )
{
  // Adapt GIMP to existing API: GIMP drawable=>Map
  // Many parameters are global vars
  // Assert image and image_mask invalid, uninitialized.
  fetch_image_mask_map(drawable, image, total_bpp, image_mask, MASK_TOTALLY_SELECTED, 
      NULL /*map_out_drawable*/, map_start_bip);
  // assert image and image_mask now valid, same dimensions
  // assert mask represents selection in drawable
      
  // Adapt existing API to SimpleAPI: Map=>ImageBuffer
  initBufferAndAntiAdapt(imageBuffer, image, 1, 4);
  initBufferAndAntiAdapt(maskBuffer, image_mask, 0, 1);
  
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

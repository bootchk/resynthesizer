#pragma once

void
fetch_image_mask_map(
  GimpDrawable       *image_drawable,     // IN image: target or corpus drawable
  Map                *pixmap,             // OUT our pixmap of drawable
  guint               pixelel_count,      // IN count channels in image + map
  Map                *mask,               // OUT our selection bytemap (only one channel ie byte ie depth)
  Pixelel             default_mask_value, // IN default value for any created mask
  GimpDrawable       *map_drawable,       // IN map drawable, target or corpus
  guint               map_offset          // IN index in our Pixel to first map Pixelel
  );

void
post_results_to_gimp(
  GimpDrawable *drawable,
  Map           targetMap);
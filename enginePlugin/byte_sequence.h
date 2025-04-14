#pragma once


void
debugPixmap (char * name, Map *map);


guchar* 
empty_byte_sequence_for_pixmap(
  Map           map,
  GimpDrawable *drawable
  );

void
get_byte_sequence_from_pixmap (
  Map           map, 
  GimpDrawable *drawable, 
  guchar       *raw_image_bytes,
  gint          pixelel_offset);



void
set_byte_sequence_to_pixmap (
  Map           dest_pixmap, 
  GimpDrawable *drawable, 
  guchar       *raw_image_bytes,
  gint          pixelel_count_to_copy,
  gint          pixelel_offset,
  gint          raw_size_bytes);



void
byte_sequence_to_drawable(
  GimpDrawable *drawable,
  guchar       *raw_image_bytes
  );

guchar*
byte_sequence_from_drawable_w_conversion(
  GimpDrawable *drawable,
  gint         *raw_bytes_size  // OUT size of byte_sequence
  );

guchar*
byte_sequence_from_mask_no_conversion (
  GimpDrawable *drawable,
  gint         *raw_bytes_size  // OUT size of byte_sequence
  );
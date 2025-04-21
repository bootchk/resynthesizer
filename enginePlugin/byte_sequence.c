#include <libgimp/gimp.h>

#include "coordinates.h"
#include "engineTypes2.h" // Pixelel

#include "map.h"       // Map
#include "drawable.h"  // get_buffer etc.
#include "format.h"    // get_working_format_for_drawable etc.

#include "byte_sequence.h"

/*
Functions adapting from ByteSequence.

To and from GimpDrawable.
To and from Pixmap.

A ByteSequence is a sequence of Pixelel, which is a byte
(at least in the Resynthesizer, for its current working formats.)
A ByteSequence itself doesn't know its Pixel boundaries,
it may have one or more bytes per pixel.

A ByteSequence is depth 8-bit,
whereas a Drawable can have greater depth in bytes per pixelel.

gegl_buffer_get and set deliver a ByteSequence,
when the Format is a working format of resynthesizer!!!
More generally, gegl_buffer_get and set will deliver
a stream of bytes where each pixelel is many bytes (higher bit-depth.)
Formerly, in GIMP 2, a pixel_rgn was similar to a ByteSequence.

Since ByteSequence is from an image/drawable,
You can think of it as having a shape (rows and columns)
but it doesn't know its shape.

A ByteSequence can have size 1,2,3,...
since the drawable can be one row tall.
A ByteSequence of size zero is a programming error.
*/

void
debugPixmap (char * name, Map *map)
{
  g_debug ("%s %s w: %d h: %d d: %d", G_STRFUNC,
    name,
    map->width,
    map->height,
    map->depth);  // depth is not bit-depth of pixelel
}


/* 
Return a new, empty, allocated non-sparse ByteSequence 
of the color and alpha Pixelels from a working Pixmap.
Caller must free it.

Note the byte_sequence is the same depth as the pixmap (8-bit)
but only the color and alpha Pixelels are copied.
The byte_sequence is NOT necessarily the same depth as the drawable.
*/
guchar*
empty_byte_sequence_for_pixmap(
  Map map,
  GimpDrawable *drawable
  )
{
  /* 
  The count of Pixelels to copy, is only the color and alpha,
  not the mask and map Pixelels in the working Pixel.
  We pass the drawable, but it has a different depth than our pixmap.
  This is the working depth, not the drawable's depth.
  */
  guint pixelel_count = get_working_pixelels_per_pixel_for_target_or_corpus (drawable);
  gint size = map.width * map.height * pixelel_count;  // !!! Size of byte_sequence, not the pixmap

  // Pixmap has more pixelels than offset + count
  // g_assert( pixelel_offset + pixelel_count <= map.depth ); 
  
  return g_malloc(size);
}

/*
Copy from the pixmap into a ByteSequence. 
Skipping over leading mask pixelel and trailing map pixelels of the source pixmap,
but into contingous bytes of the destination byte_sequence.
*/
void
get_byte_sequence_from_pixmap (
  Map           map, 
  GimpDrawable *drawable, 
  guchar       *raw_image_bytes,
  gint          pixelel_offset)
{
  guint pixel_count   = map.width * map.height;
  guint dest_pixelel_count = get_working_pixelels_per_pixel_for_target_or_corpus (drawable);
  
  for(guint pixel=0; pixel<pixel_count; pixel++)  // Iterate over map as a sequence of Pixels
    for(guint pixelel=0; pixelel<dest_pixelel_count; pixelel++)  // Iterate over Pixelels
      raw_image_bytes[pixel*dest_pixelel_count + pixelel] =
        g_array_index(
          map.data,                       // source data
          Pixelel,                        // cast result to guint8
          pixel*map.depth + pixelel_offset + pixelel);  // index into source array
}


/* 
Copy all of the pixelels from raw_image_bytes sequence
to our pixmap, OFFSET them within each pixel,
skipping some pixelels in the destination pixel.

The pixelel_count_to_copy is the count of Pixelels in the source Pixel.
Typically 3 for color and 1 for alpha, but can be different.
*/
void
set_byte_sequence_to_pixmap (
  Map           dest_pixmap, 
  GimpDrawable *drawable, 
  guchar       *raw_image_bytes,
  gint          pixelel_count_to_copy,
  gint          pixelel_offset,
  gint          raw_size_bytes) 
{
  gint raw_size_pixels = dest_pixmap.width * dest_pixmap.height;

  debugPixmap("dest", &dest_pixmap);

  g_debug ("%s raw_size_pixels %d raw_size_bytes %d dest_pixmap size bytes %d", 
    G_STRFUNC, raw_size_pixels, raw_size_bytes,
    dest_pixmap.width * dest_pixmap.height * dest_pixmap.depth);

  /*
  dest_pixmap is same dimensions as raw_image_bytes, but depth in bytes of dest_pixmap may be greater.
  Size in bytes of dest_pixmap is width*height*depth.
  Size in bytes of raw_size_pixels*pixelel_count_to_copy == raw_size_bytes.
  */ 
  g_assert (raw_size_bytes <= dest_pixmap.width * dest_pixmap.height * dest_pixmap.depth);

  for(gint pixel =0; pixel<raw_size_pixels; pixel++)
    for(gint pixelel=0; pixelel<pixelel_count_to_copy; pixelel++)  /* Count can be different from strides. */
      /* Dest stride is depth of pixmap. */
      g_array_index(dest_pixmap.data, Pixelel, pixel*dest_pixmap.depth + pixelel_offset + pixelel) 
       /* src stride is count of pixelels to copy. */
       = raw_image_bytes[pixel*pixelel_count_to_copy + pixelel];
}





/*
byte_sequence to/from GimpDrawable.

!!! Note assymetry in implementation:
when reading, use the regular buffer, but when writing, use the shadow buffer.

Apparently, reading from the shadow buffer is not necessary,
and furthermore does not even work: it returns all zeros.

There is not a good Gegl document explaining why this is so.
*/


/* 
Send seq of Pixelels to GimpDrawable.
Convert the Pixels to the drawable's format.
Also flush.

This is only used for color, grayscale Drawables, (possibly with alpha.)
the target or result of the resynthesizer.
We don't use this for the mask or weight_map.

Formerly used: gimp_pixel_rgn_set_rect(&region, img, 0,0, width, height);
*/
void
byte_sequence_to_drawable(
  GimpDrawable *drawable,
  guchar       *raw_image_bytes
  )
{
  GeglBuffer *dest_buffer;
  GeglBuffer *shadow_buffer;

  const Babl *byte_sequence_format;

  dest_buffer   = get_buffer(drawable);
  shadow_buffer = gimp_drawable_get_shadow_buffer (drawable);

  /*
  The GeglBuffer has a format, but it may be higher bit depth
  than the byte_sequence.
  Gegl will convert the byte_sequence to the format of the buffer.
  */
  byte_sequence_format = get_working_format_for_drawable (drawable);
  debugBablFormat ("byte_sequence", byte_sequence_format);
  
  gegl_buffer_set(shadow_buffer,
          GEGL_RECTANGLE(0,0, gimp_drawable_get_width (drawable), gimp_drawable_get_height (drawable)),
          0,  // mipmap level, 0 => 1:1
          byte_sequence_format,   // format of the source byte_sequence data
          raw_image_bytes,
          GEGL_AUTO_ROWSTRIDE);

  // unref is documented to flush automatically.
  g_object_unref (shadow_buffer);
  g_object_unref (dest_buffer);
}


/*
Get a working_byte_sequence from a drawable.

The drawable may be in a different format:
different color model and higher bit-depths.
This converts to the working format of the engine.

The drawable can be a corpus or target or a weight_map.
The drawable should not be a selection mask,
which is never converted, always 1 byte per pixel.

!!! Note not using a shadow buffer, when reading.
*/
guchar *
byte_sequence_from_drawable_w_conversion(
  GimpDrawable *drawable,
  gint         *raw_bytes_size  // OUT size of byte_sequence
  )
{
  GeglBuffer *buffer;
  const Babl *workingFormat;    // The format that engine works with
  gint        width = gimp_drawable_get_width (drawable);
  gint        height = gimp_drawable_get_height (drawable);
  guchar     *raw_image_bytes;

  buffer        = get_buffer(drawable);

  *raw_bytes_size = width * height * get_working_pixelels_per_pixel_for_drawable (drawable);
  raw_image_bytes = g_malloc(*raw_bytes_size);

  g_debug ("%s w: %d h: %d byte size: %d", G_STRFUNC, width, height, *raw_bytes_size);

  // For debugging only
  {
    const Babl *imageFormat; 
    imageFormat = gegl_buffer_get_format (buffer);
    debugBablFormat ("image", imageFormat);
  }

  workingFormat = get_working_format_for_drawable (drawable);
  debugBablFormat ("working", workingFormat);

  /*
  Get all the pixelels from drawable into raw_image_bytes sequence.
  Note x1,y1 are in drawable coords i.e. relative to drawable
  The drawable may be offset from the canvas and other drawables.
  */
  gegl_buffer_get (buffer,
            NULL, // Entire extent of buffer  GEGL_RECTANGLE(0, 0, width, height),
            1.0,  // scale, float 1.0 is pixel for pixel
            workingFormat,   // convert format of buffer to working format.
            raw_image_bytes,
            GEGL_AUTO_ROWSTRIDE,
            GEGL_ABYSS_NONE);

  g_object_unref (buffer);

  return raw_image_bytes;
}

/* Assert that a drawable is a mask with one component. 
 * Not checking that it is not non-linear "Y' u8"
 */
static void
assert_has_mask_format (GimpDrawable *mask_drawable)
{
  const Babl *maskFormat;
  maskFormat = gimp_drawable_get_format (mask_drawable);
  debugBablFormat ("mask", maskFormat);

  g_assert (babl_format_get_n_components (maskFormat) == 1);
}


/*
Get a working_byte_sequence from a drawable that is a selection mask.

The drawable must be a selection mask, having one component.

The mask may have higher bit depth than 1 byte per pixel (e.g. 16-bit).
GIMP seems to allow higher bit depth, but the engine does not use it.
Convert to 8-bit depth.

!!! Note not using a shadow buffer, when reading.
*/
guchar *
byte_sequence_from_mask (
  GimpDrawable *mask_drawable,
  gint         *raw_bytes_size  // OUT size of byte_sequence
  )
{
  GeglBuffer *buffer;
  gint        width = gimp_drawable_get_width (mask_drawable);
  gint        height = gimp_drawable_get_height (mask_drawable);
  guchar     *raw_image_bytes;

  buffer        = get_buffer(mask_drawable);

  assert_has_mask_format (mask_drawable);
  
  *raw_bytes_size = width * height * 1;  // 1 byte per pixel
  raw_image_bytes = g_malloc(*raw_bytes_size);

  g_debug ("%s w: %d h: %d byte size: %d", G_STRFUNC, width, height, *raw_bytes_size);

  /*
  Get all the pixelels from drawable into raw_image_bytes sequence.
  Note x1,y1 are in drawable coords i.e. relative to drawable
  The drawable may be offset from the canvas and other drawables.
  */
  gegl_buffer_get (buffer,
            NULL, // Entire extent of buffer  GEGL_RECTANGLE(0, 0, width, height),
            1.0,  // scale, float 1.0 is pixel for pixel
            get_working_format_for_mask (),   // conversion to lower bit depth used by engine
            raw_image_bytes,
            GEGL_AUTO_ROWSTRIDE,
            GEGL_ABYSS_NONE);

  g_object_unref (buffer);

  return raw_image_bytes;
}
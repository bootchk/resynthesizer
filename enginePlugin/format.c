
#include <libgimp/gimp.h>

#include "drawable.h"

#include "format.h"


/* 
Functions about the format i.e. structure of pixels.

In GIMP, the format of pixels is a Babl* to a Format.
Drawables have a Format.  All the pixels of the Drawable have that format.
The format denotes many things: the bit-depth and the order of the color components,
and the types of the color components (e.g. RGB versus HSV) i.e. the color model.

The Resynthesizer has a "working" format and its own notion of Drawable, a Pixmap.
Resynthesizer adapts from a GIMP Drawable to Pixmap,
reducing any of many Babl Formats to the working format.

In other words, these functions understand how the resynthesizer
copies pixelels from a drawable to a pixmap.

!!! This is in terms of pixelels, not bytes.
Bit-depth of colors is relevant.
A drawable in GIMP allows varying bit-depth per channel,
but the engine works with only 8-bits, one byte, per channel.

Since adaption takes place only a few times, not overly concerned with performance.
We don't compute the working format once, and then return its values as needed.
Contrast with the "indicies" code,
where we compute the offsets into the working format pixel once.

Drawables have different roles for resynthesizer: 
    corpus or target
    selection mask
    weighting map
These functions compute the working format for a drawable in a particular role.
*/

/* 
Describe a Babl Format, naming it.
The format may come from a Drawable,
may be the format of a ByteSequence,
or may be a working format.
*/
void
debugBablFormat (char * name, const Babl *format)
{
  g_debug ("%s %s %s %d %d", G_STRFUNC,
    name,
    babl_format_get_encoding (format),
    babl_format_get_bytes_per_pixel (format),
    babl_format_get_n_components (format));
}

/*
For given drawable used in the role of a weight map,
return the count of pixelels
that the resynthesizer engine will work with.
(To be copied from the drawable to working pixmap.)

Agnostic of colorspace and coordinate systems: 
each non-gray colorspace has three dimensions (and coordinates)
even the colorspaces that used polar coordinates.

The engine does not work with any alpha channel of the map.

This implementation assumes that the layout of the pixelels in the drawable
starts with the color channels, then the alpha channel.
*/
gint
get_working_pixelels_per_pixel_for_weight_map (GimpDrawable *map_drawable)
{
  gint result = 0;

   if (is_rgb (map_drawable) )
    result = 3;
  else
    // Greyscale or indexed
    result = 1;
  return result;
}

/*
For given drawable used in role of target or corpus,
return the count of pixelels
that the resynthesizer engine will work with.
(To be copied from the drawable to working pixmap.)

The engine reads and works with any alpha channel,
but does not synthesize it and writes back the original.

This implementation assumes that the layout of the pixelels in the drawable
starts with the color channels, then the alpha channel.

!!! This works with RGB, GRAY, or INDEXED modes.

RGB   3
RGBA  4
GRAY  1
GRAYA 2
INDEXED  1
INDEXEDA 2
*/
gint
get_working_pixelels_per_pixel_for_target_or_corpus (GimpDrawable *drawable)
{
  // TODO just call the generic one.
  
  gint result = 0;

  const Babl *working_format = get_working_format_for_drawable (drawable);

  result = babl_format_get_n_components (working_format);

  g_debug ("%s %d", G_STRFUNC, result);
  return result;
}

/*
Generic, for either a target or corpus, or a selection mask.

!!! Should not be used for a weight map
*/
gint
get_working_pixelels_per_pixel_for_drawable (GimpDrawable *drawable)
{
  gint result = 0;

  const Babl *working_format = get_working_format_for_drawable (drawable);

  result = babl_format_get_n_components (working_format);

  g_debug ("%s %d", G_STRFUNC, result);

  return result;
}


/*
For given drawable used in role of a selection mask,
return the count of pixelels in the mask.
By definition, a mask has only one pixelel.
This returns 1.

More generally, the count of bytes in a pixelel of a drawable
depends on the count of bytes in the color pixelels,
i.e. on the bit-depth of the image encoding.
!!! The count is not always one, i.e. 8 bits.
gimp_drawable_get_bpp() may return 1, 2, or 4.
So we don't use it here.
*/
gint
get_working_pixelels_per_pixel_for_selection_mask (GimpDrawable *mask_drawable)
{
  gint result;
  const Babl *working_format = get_working_format_for_drawable (mask_drawable);

  result = babl_format_get_n_components (working_format);

  g_debug ("%s %d", G_STRFUNC, result);

  /* When called with a drawable that is NOT a mask, this will fail. */
  g_assert (result == 1);

  return result;
}

/*
For given drawable,
return the count of bytes per pixel.

This depends on the color depth of the drawable,
and whether the drawable has an alpha channel.
Can be calculated from the babl format of the drawable:
the number of components times the bytes per component.
*/
gint
get_bytes_per_pixel_for_drawable (GimpDrawable *drawable)
{
  return gimp_drawable_get_bpp (drawable);
}


/* 
Return the babl format that the resynthesizer engine
will work with for a given drawable.
The engine will convert the drawable's data to this format,
for data used in the engine.

!!! This is for images, not masks.

The drawable may be in a different format:
different color model and higher bit-depths.
For example "R'G'B' u16" has different color model and higher bit-depth.
For example "HSV u16" also is different.

Note the difference between linear and non-linear color models.
'R'G'B' is non-linear, i.e. sRGB.
The engine uses the nonlinear sRGB color model, i.e. R'G'B',
because it is the same as engine used previously in GIMP 2.

The engine uses 8-bits per channel.
The lower bit-depth means less memory, and better memory locality,
i.e. the hardware memory cache works better.
This also knows the color model the engine uses: RGB

The result format has the same count of components as the format of the drawable.
*/
const Babl *
get_working_format_for_drawable (GimpDrawable *drawable)
{
  const Babl *result;

  if (is_indexed (drawable))
  {
    // Single byte per pixel, 256 colors
    // !!! Not Y' u8, which is non-linear
    result = babl_format ("Y u8");
    return result;
  }
  else 
    {
      // Gray or RGB image, and not a selection mask

      const Babl *drawable_format;
      gint        component_count;
      
      drawable_format = gimp_drawable_get_format (drawable);
      component_count = babl_format_get_n_components (drawable_format);

      switch (component_count)
      {
        case 1:
          // Greyscale, non-linear color model is Y' (luminance)
          result = babl_format ("Y' u8");
          break;
        case 2:
          // Greyscale with alpha.
          // The format is Y'A, not Y'A'
          result = babl_format ("Y'A u8");
          break;
        case 3:
          // RGB non-linear (sRGB) is R'G'B'.
          // The engine uses sRGB, the same as GIMP.
          result = babl_format ("R'G'B' u8");
          break;
        case 4:
          // RGBA with alpha
          // The format has linear alpha, not A'
          result = babl_format ("R'G'B'A u8");
          break;
        default:
          g_assert_not_reached ();
      }
  }

  g_debug ("%s working babl format %s bytes per pixel %d component count %d", 
    G_STRFUNC,
    babl_format_get_encoding (result),
    babl_format_get_bytes_per_pixel (result),
    babl_format_get_n_components (result));

  return result;
}


/* 
Return count of color channels, exclude alpha and any other channels.

This is not the same as the count of components in the format.
The format may have more channels than the color channels.
For example, the format may have an alpha channel.

For example, the RGBA format has 4 channels, but only 3 color channels.
The GRAYA format has 2 channels, but only 1 color channel.
*/
guint
count_color_channels (GimpDrawable *drawable)
{
  g_assert(drawable); // Not null

  GimpImageType type = imageType(drawable);
  switch(type)
  {
    case GIMP_RGB_IMAGE:
    case GIMP_RGBA_IMAGE:
      return 3;

    case GIMP_GRAY_IMAGE:
    case GIMP_GRAYA_IMAGE:

    case GIMP_INDEXED_IMAGE:
    case GIMP_INDEXEDA_IMAGE:
      return 1;

    default:
      g_assert(FALSE);
  }
  return 0;
}
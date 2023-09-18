
// compatibility functions from Gimp 2 to 3

#include <libgimp/gimp.h>




// hacky test that version is less than 2.99.xx
#if GIMP_MINOR_VERSION < 99


gint          bpp      (GimpDrawable *d) { return d->bpp;     }
gint          width    (GimpDrawable *d) { return d->width;   }
gint          height   (GimpDrawable *d) { return d->height;  }

GimpImageType  imageType(GimpDrawable *d) { return gimp_drawable_type     (d->drawable_id); }
gboolean       is_rgb   (GimpDrawable *d) { return gimp_drawable_is_rgb   (d->drawable_id); }
gboolean       is_gray  (GimpDrawable *d) { return gimp_drawable_is_gray  (d->drawable_id); }
gboolean       has_alpha(GimpDrawable *d) { return gimp_drawable_has_alpha(d->drawable_id); }


GeglBuffer *
get_buffer(GimpDrawable *d)        { return gimp_drawable_get_buffer(d->drawable_id);        }
GeglBuffer *
get_shadow_buffer(GimpDrawable *d) { return gimp_drawable_get_shadow_buffer(d->drawable_id); }

gboolean
merge_shadow(GimpDrawable *d)
{
  return gimp_drawable_merge_shadow(
    d->drawable_id,
    TRUE); // hardcoded
}

void
update(
  GimpDrawable *d,
  gint lx,
  gint ly,
  gint rx,
  gint ry)
{
  gimp_drawable_update(d->drawable_id, lx, ly, rx, ry);
}

// Offset of layer in image
void
offsets(
  GimpDrawable *d,
  gint         *x,
  gint         *y )
{
  gimp_drawable_get_offsets( d->drawable_id, x, y );
}

GimpDrawable *
get_selection(GimpDrawable * d)
{
  // selection is a new drawable derived from image of a drawable
  gint id = gimp_image_get_selection(gimp_item_get_image(d->drawable_id));
  return gimp_drawable_get(id);
}

gboolean
selection_bounds(
  GimpDrawable *d,
  gint *lx,
  gint *ly,
  gint *rx,
  gint *ry)
{
  return gimp_drawable_mask_bounds(d->drawable_id, lx, ly, rx, ry);
}

// More generally, any mask.  Here a selection
gboolean
selection_intersect(
  GimpDrawable *d,
  gint        *drawable_relative_x,
  gint        *drawable_relative_y,
  gint        *intersect_width,
  gint        *intersect_height)
{
return gimp_drawable_mask_intersect(d->drawable_id,
  drawable_relative_x, drawable_relative_y,
  intersect_width, intersect_height);
}



#else



gint          bpp(GimpDrawable *d)       { return gimp_drawable_get_bpp   (d); }
gint          width    (GimpDrawable *d) { return gimp_drawable_get_width (d); }
gint          height   (GimpDrawable *d) { return gimp_drawable_get_height(d); }

GimpImageType  imageType(GimpDrawable *d) { return gimp_drawable_type     (d); }
gboolean       is_rgb(GimpDrawable *d)    { return gimp_drawable_is_rgb   (d); }
gboolean       is_gray(GimpDrawable *d)   { return gimp_drawable_is_rgb   (d); }
gboolean       has_alpha(GimpDrawable *d) { return gimp_drawable_has_alpha(d); }

GeglBuffer *
get_buffer(GimpDrawable *d)        { return gimp_drawable_get_buffer(d);        }
GeglBuffer *
get_shadow_buffer(GimpDrawable *d) { return gimp_drawable_get_shadow_buffer(d); }

gboolean
merge_shadow(GimpDrawable *d)
{
  return gimp_drawable_merge_shadow(
    d,
    TRUE); // hardcoded
}

void
update(
  GimpDrawable *d,
  gint lx,
  gint ly,
  gint rx,
  gint ry)
{
  gimp_drawable_update(d, lx, ly, rx, ry);
}

// Offset of layer in image
void
offsets(
  GimpDrawable *d,
  gint         *x,
  gint         *y )
{
  gimp_drawable_get_offsets( d, x, y );
}

GimpDrawable *
get_selection(GimpDrawable * d)
{
  // selection is a new drawable derived from image of a drawable
  // TODO this is not right for v3 ??
  // return gimp_image_get_selection(gimp_item_get_image(d->drawable_id));
  return gimp_image_get_selection(gimp_item_get_image(d));
}

gboolean
selection_bounds(
  GimpDrawable *d,
  gint *lx,
  gint *ly,
  gint *rx,
  gint *ry)
{
  return gimp_drawable_mask_bounds(d, lx, ly, rx, ry);
}

// More generally, any mask.  Here a selection
gboolean
selection_intersect(
  GimpDrawable *d,
  gint        *drawable_relative_x,
  gint        *drawable_relative_y,
  gint        *intersect_width,
  gint        *intersect_height)
{
return gimp_drawable_mask_intersect(d,
  drawable_relative_x, drawable_relative_y,
  intersect_width, intersect_height);
}



#endif

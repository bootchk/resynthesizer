

// Functions on GimpDrawable

// Many of these gint should be guint, but Gimp declares gint, so to avoid compiler warnings use gint

// Obsolete concept: bytes per pixel
// gint             bpp   (GimpDrawable *d);

gint             width (GimpDrawable *d);
gint             height(GimpDrawable *d);

GimpImageType     imageType(GimpDrawable *d);
gboolean          is_rgb   (GimpDrawable *d);
gboolean          is_gray  (GimpDrawable *d);
gboolean          is_indexed(GimpDrawable *d);
gboolean          has_alpha (GimpDrawable *d);

GeglBuffer * get_buffer       (GimpDrawable *d);
GeglBuffer * get_shadow_buffer(GimpDrawable *d);
gboolean     merge_shadow     (GimpDrawable *d);

void        update(
  GimpDrawable *d,
  gint lx,
  gint ly,
  gint rx,
  gint ry);

void offsets(
  GimpDrawable *d,
  gint         *x,
  gint         *y);

GimpDrawable *
get_selection(GimpDrawable * d);

gboolean
selection_bounds(
  GimpDrawable *d,
  gint *lx,
  gint *ly,
  gint *rx,
  gint *ry);

gboolean
selection_intersect(
  GimpDrawable *d,
  gint        *drawable_relative_x,
  gint        *drawable_relative_y,
  gint        *intersect_width,
  gint        *intersect_height);




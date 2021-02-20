

// Functions on GimpDrawable

guint             bpp   (GimpDrawable *d);
guint             width (GimpDrawable *d);
guint             height(GimpDrawable *d);

GimpImageType     imageType(GimpDrawable *d);
gboolean          is_rgb   (GimpDrawable *d);
gboolean          is_gray  (GimpDrawable *d);
gboolean          has_alpha(GimpDrawable *d);

GeglBuffer * get_buffer       (GimpDrawable *d);
GeglBuffer * get_shadow_buffer(GimpDrawable *d);
gboolean     merge_shadow     (GimpDrawable *d);

void        update(
  GimpDrawable *d,
  guint lx,
  guint ly,
  guint rx,
  guint ry);

void offsets(
  GimpDrawable *d,
  gint         *x,
  gint         *y);

GimpDrawable *
get_selection(GimpDrawable * d);

gboolean
selection_bounds(
  GimpDrawable *d,
  guint *lx,
  guint *ly,
  guint *rx,
  guint *ry);

gboolean
selection_intersect(
  GimpDrawable *d,
  guint        *drawable_relative_x,
  guint        *drawable_relative_y,
  guint        *intersect_width,
  guint        *intersect_height);




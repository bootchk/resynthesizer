
#pragma once

void  debugBablFormat                                     (char * name, const Babl *format);

gint  get_working_pixelels_per_pixel_for_weight_map       (GimpDrawable *map_drawable);
gint  get_working_pixelels_per_pixel_for_target_or_corpus (GimpDrawable *drawable);
gint  get_working_pixelels_per_pixel_for_selection_mask   (GimpDrawable *mask_drawable);
gint  get_working_pixelels_per_pixel_for_drawable         (GimpDrawable *mask_drawable);

guint count_color_channels                                (GimpDrawable *drawable);

gint  get_bytes_per_pixel_for_drawable                    (GimpDrawable *drawable);

const Babl * get_working_format_for_drawable              (GimpDrawable *drawable);


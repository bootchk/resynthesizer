/*
Prepare indices into our pixel format.

Synthesizer engine pixel contains mask pixelel and map pixelels
(Not just the color and alpha pixelels.)

IN: Image format (RGB, RGBA, Grey, etc.)
OUT: global index variables.

Not depend on Gimp
*/

/* 
bpp i.e. count of bytes (channels) per pixel or index thereof.
bpp is bytes per pixel
bip is byte index within pixel.
See data layout in resynth_types.h
*/
typedef unsigned char TPixelelIndex;

/*
Struct of indices of pixelels within pixels in internal image format.
Also flags.
One is used for the target, one for the source.
*/
typedef struct indicesStruct {
  TPixelelIndex colorEndBip; /* Index of last color pixelels in target/context image. */
  TPixelelIndex alpha_bip;      /* Index of target alpha pixelel */
  TPixelelIndex map_start_bip;  /* Index of map pixelels */
  TPixelelIndex map_end_bip;

  TPixelelIndex img_match_bpp; /* bpp to match in image. */
  TPixelelIndex map_match_bpp; /* bpp to match in map. */
  TPixelelIndex total_bpp;     /* Total pixelels */
  
  gboolean isAlphaTarget; // Does target have alpha?
  gboolean isAlphaSource; // Does source have alpha?
} TFormatIndices;


extern void
prepareImageFormatIndices(
  TFormatIndices* indices,  // OUT
  guint count_color_channels_target,  // Must be same count in source
  guint count_color_channels_map,
  gboolean is_alpha_target,
  gboolean is_alpha_source,
  gboolean isMap
  );



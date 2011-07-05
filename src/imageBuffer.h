
typedef struct _ImageBuffer 
{
  unsigned char * data; // data must be sequential RGBA for image, sequence of bytes for a mask
  unsigned int width;
  unsigned int height;
  size_t rowBytes;    // Row stride.  unsigned int? doesn't really describe size of a type, but count of bytes in pixmap row
}
ImageBuffer;

// Temporarily, mask same as image except only one pixelel per pixel

/*
typedef struct _ImageMask 
{
}
ImageMask;
*/

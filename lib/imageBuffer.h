/*
A generic data structure for passing images.

Data is a stream of unpadded pixels.
Interpretation of pixels (order and count of pixelels (unsigned bytes)) is given by a enum of type TImageFormat.
Rows padded.
*/

typedef struct _ImageBuffer 
{
  unsigned char * data; // data must be sequential RGBA for image, sequence of bytes for a mask
  unsigned int width;
  unsigned int height;
  size_t rowBytes;    // Row stride.  unsigned int? doesn't really describe size of a type, but count of bytes in pixmap row
}
ImageBuffer;

// Mask same as image except only one pixelel per pixel (different, or assumed format code of on pixelel per pixel.)


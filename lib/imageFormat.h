/*
Prepare indices into our pixel format.

Synthesizer engine pixel contains mask pixelel and map pixelels
(Not just the color and alpha pixelels.)

IN: Image format (RGB, RGBA, Grey, etc.)
OUT: global index variables.

Not depend on Gimp

  Copyright (C) 2010, 2011  Lloyd Konneker

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


#ifndef __SYNTH_IMAGE_FORMAT_H__
#define __SYNTH_IMAGE_FORMAT_H__

// Enumerates different layouts of pixelels within pixels, for in images
// This type is exported, needed by callers of library
// The library lays pixelels out in its own internal format
typedef enum  ImageFormat 
{
  T_RGB,
  T_RGBA,
  T_Gray,
  T_GrayA
} TImageFormat;
  
#endif /* __SYNTH_IMAGE_FORMAT_H__ */


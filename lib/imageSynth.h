/*
Header for the simple API to libresynthesizer

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

// The simple API takes one image and heals the selection.
// The full API takes two images (target and corpus) and can do many things.
// The simple API munges one image into two and calls the full API.

// Type defs of struct you must pass
#include "imageBuffer.h"
#include "imageFormat.h"

// Signature of the only simple API function
int
imageSynth(
  ImageBuffer * imageBuffer,  // IN/OUT RGBA Pixels described by imageFormat
  ImageBuffer * mask,         // IN one mask Pixelel
  TImageFormat imageFormat,
  TImageSynthParameters* parameters,
  void (*progressCallback)(int, void*),   // int percentDone, void *contextInfo
  void *contextInfo,	// opaque to engine, passed in progressCallback
  int *cancelFlag		// polled by engine: engine quits if ever becomes True
  );

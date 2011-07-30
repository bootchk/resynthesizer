/* 
Constants for inner  image synthesis engine.

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

/*
Max size of neighborhood (patch)
Allocated on stack in reentrant version.
Engine returns error if parameter.patchSize exceeds this.
*/
#define IMAGE_SYNTH_MAX_NEIGHBORS 64


/*
Constants for selection masks
and transparency pixelel.
These should match the calling program's constants.

!!! Partially selected and partially transparent.
*/
#define MASK_TOTALLY_SELECTED 0xFF
#define MASK_UNSELECTED 0

#define ALPHA_TOTAL_TRANSPARENCY 0

#define PIXELEL_BLACK 0


/* 
These specify the layout of our Pixel, which is not the same as Gimp !!!
e.g. MRGBW
These are the only constants: other counts and indexes are dynamic:
  index of the first and last color and map pixelels
  index of the alpha pixelel
  count of color and map pixelel
*/
#define MASK_PIXELEL_INDEX  0
#define FIRST_PIXELEL_INDEX 1	/* Starting color pixelel (usually Red) */
/* 
The most pixelels imageSynth will store and use to match. 
1 mask + 3 colors + 1 alpha + 3 map colors
*/
#define MAX_IMAGE_SYNTH_BPP 8


/*
Constants of the synthesis algorithm.
TODO in future versions, these might be parameters to the engine.
*/

/*
Maximum domain value of metric function for pixel difference.
Was metric[0] which was typically 65418, for the setting that affects weighting (autism.)
65535 is max number representable in 16-bit unsigned int.
!!! Note mapMetric[0] is the max domain value for the map metric.
The mapMetric domain is not gushort and values depend on a user given parameter.
*/
#define MAX_WEIGHT G_MAXUSHORT

/*
A constant multiplying factor of the map metric function.
*/
#define MAP_MULTIPLIER 4.0
/*
TODO scale the mapMetric and use a constant for the extreme value,
for slightly better performance??
#define MAX_MAP_DIFF 65569    // G_MAXUINT = 4,294,967,295 = 32^2 - 1
*/

/*
The fraction of target points that must be bettered on a pass
else terminate repeated passes over the target.
*/
#define IMAGE_SYNTH_TERMINATE_FRACTION 0.1

/*
The fraction ( count of points in a band / total target points) 
for banded randomization of target points.
*/
#define IMAGE_SYNTH_BAND_FRACTION 0.1



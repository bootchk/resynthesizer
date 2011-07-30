/* 
Fundamental types of the engine.

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
2-D array of type, where type has size depth.
Bytemap: type is char (often used as a bool)
Pixmap: type is Pixel
Intmap: type is int
Coordmap: type is Coordinates
*/
typedef struct {
  guint width;
  guint height;
  guint depth; 
  GArray * data;
  } Map;

typedef guint8 Pixelel;

/*
Coordinates class

!!! These are signed ints, can represent offsets or positive coordinates 
Similar to GdkPoint.
*/
typedef struct {
    gint x;
    gint y;
} Coordinates;


extern void
free_map (Map *);

extern void
new_pixmap(
  Map *,
  guint, 
  guint, 
  guint
  );

extern void
new_bytemap(
  Map *,
  guint, 
  guint
  );
  
extern void
new_intmap(
  Map *,
  guint, 
  guint
  );

extern void
new_coordmap(
  Map *,
  guint, 
  guint
  );


/* Misc map operations. */

extern void
set_bytemap(
  Map*,
  guchar
  );

extern void 
invert_bytemap(
  Map* map
  );
  
extern void
interleave_mask(
  Map *pixmap,
  Map *mask
  );



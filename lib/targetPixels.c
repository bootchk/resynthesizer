
#include "../resynth-config.h"

#include <glib.h>

#include "engineTypes2.h"
#include "imageSynthConstants.h"
#include "imageFormat.h"
#include "map.h"
#include "mapIndex.h"
#include "selection.h"

#include "targetPixels.h"

/*
Clear target pixels. So see them get resynthesized when animated debugging.
Note the initial values of the target are never used, but totally resynthesized.
*/
static void
clear_target_pixels(guint bpp, Map target)
{
  guint x;
  guint y;

  for(y=0;y<target.height;y++)
    for(x=0;x<target.width;x++)
    {
      Coordinates coords = {x,y};
      if (isSelectedTarget(coords, &target))
      {
        guint pixelel;
        Pixelel * pixel = pixmap_index(&target, coords);
        for (pixelel = FIRST_PIXELEL_INDEX; pixelel < bpp; pixelel++) // Color channels only
          pixel[pixelel] = PIXELEL_BLACK;
      }
    }
}

/*
Prepare pixels of the selection in the target.

This depends on the mode of the engine, and whether animating.

For most modes, don't really need to clear the target pixels,
since the engine doesn't read them.
A target pixel is not part of an initial patch, on the first pass.

Clearing them (to black) only necessary for animation debugging.
But it doesn't hurt the performance much to clear them.

FUTURE: for mode falseColor, do not clear the pixels.
*/
void 
prepareTargetPixels (guint bpp, Map targetMap)
{
  clear_target_pixels(bpp, targetMap);
}

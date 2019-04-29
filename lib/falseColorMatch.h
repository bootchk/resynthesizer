
/*
This .h file is not a header per se but is included in engine.c
at the appropriate point where many of the referenced types and functions
are already defined.
*/







/*
Scale coordinates to fit in a pixelel (8-bits)
Given coordinates must be in the frame of the given image.
Typically, image is the corpus image.

May scale up (when image is less than 255x255)
as well as down (when image is greater than 255x255).

Reversible (by the caller) with some loss of accuracy.

Signed integer arithmetic, with truncation.
Coordinates are signed (but they shouldn't be.)
They are gint, same as C int, which are 4 bytes on most platforms.
TODO overflow
*/
static inline Coordinates
scaleCoordinatesToEightBits(Map* map, Coordinates coords)
{
  Coordinates scaledCoordinates;

  scaledCoordinates.x = (coords.x * 255 ) / map->width;
  scaledCoordinates.y = (coords.y * 255 ) / map->height;
  // assert 0<=scaledCoordinates<=255
  return scaledCoordinates;
}

#ifdef OLD
I decided this should be done outside of the engine.
All the information is available outside the engine (e.g. in a Python plugin.)
/*
Convert coords into corpus to scaled offset from coords in target.
*/
static inline Coordinates
scaleCoordinatesToEightBitOffsets(Map* map, Coordinates targetCoords, Coordinates corpusCoords)
{
  Coordinates scaledOffset;

  // Corpus is usually larger, but need not be
  gint offsetX = ( corpusCoords.x - targetCoords.x + map->width) / 2;
  gint offsetY = ( corpusCoords.y - targetCoords.y + map->height) / 2;
  // adding dimension of target insures is positive 
  // dividing by 2 insures is in range of targetCoords
  // It is not scaled to 8 bits yet.

  scaledOffset.x = offsetX;
  scaledOffset.y = offsetY;

  return scaleCoordinatesToEightBits(map, scaledOffset);
}
#endif


/*
Set color of target pixel to a false color given by scaled coordinates of best match in corpus.

Note require coordinates in frame of corresponding map.
targetCoords in frame of targetMap
matchCoords in frame of corpusMap
*/
static inline void
setFalseColor(Map* targetMap, Map* corpusMap, Coordinates targetCoords, Coordinates matchCoords)
{
   Coordinates resultCoordinates;

   
   resultCoordinates= scaleCoordinatesToEightBits(corpusMap, matchCoords);
   // TEMP resultCoordinates = matchCoords;  // Not scaled
  
   // TODO image must be color
 

   // For two color pixelels (channels), R and G set value to coordinate value
   // !!! FUTURE use symbolic instead of hardcoded indices, i.e. don't assume RGB order.
   // Note 0 is the selection mask in the engines pixel format.
   // Note C loss of data if coordinates not scaled, since assigning a gint to a byte
   pixmap_index(targetMap, targetCoords)[1] = resultCoordinates.x;
   pixmap_index(targetMap, targetCoords)[2] = resultCoordinates.y;
   // Erase B pixelel (no blue)
   // FUTURE max value i.e. full blue more pleasing?
   pixmap_index(targetMap, targetCoords)[3] = 0;
}


/*
Color target with false colors where colors R,G are scaled coordinates 
derived from coordinates of best match in corpus.

sourceOfMap is not an image, it is a map from targetCoords to corpusCoords.
It is the elemental result of resynthesis.
The purpose of this function is to return the elemental result,
(in a kludgey form)
so the user has more freedom to do other image analysis.
Until this function was added, the elemental result was lost to the user.
Previously, the only use (but the most useful use)
of the elemental result is to transfer pixels (color) from corpus to target,
using the sourceOfMap as a guide.
*/
static void
falseColorMatch(Map* targetMap, Map* corpusMap, Map* sourceOfMap)
{
  guint x;
  guint y;

  // iterate over target
  for(y=0; y< (guint) targetMap->height; y++)
    for(x=0; x< (guint) targetMap->width; x++)
    {
      
      Coordinates targetCoords = {x,y};

      // TODO also not isTransparent ???
      if (isSelectedTarget(targetCoords, targetMap))
      {
         // assert every selected and nontransparent pixel in target has_source() == true

         Coordinates bestMatchCoords = getSourceOf(targetCoords, sourceOfMap);
         // bestMatchCoords are in frame of corpusMap
         setFalseColor(targetMap, corpusMap, targetCoords, bestMatchCoords);
      }
    } 
}

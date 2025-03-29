/*
Types fundamental to engine.
*/


/* The engine uses bit-depth 8 */
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



/*
Kind of match result.

Whenever compare existing best match patch
with a candidate matching patch,
we keep stats about how it compared,
one of these values.
*/
typedef enum  BettermentKindEnum 
{
  PERFECT_MATCH,  // Patches equal
  NO_BETTERMENT,  // Match worse than previous best
  GENERIC_BETTERMENT,
  NEIGHBORS_SOURCE,
  RANDOM_CORPUS,
  MAX_BETTERMENT_KIND
} tBettermentKind;
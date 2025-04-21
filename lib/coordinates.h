#pragma once


/* The return type of a comparision function for sort. */
typedef gint CompareResult;


/*
Coordinates class

!!! These are signed ints, can represent offsets or positive coordinates 
Similar to GdkPoint.
*/
typedef struct {
  gint x;
  gint y;
} Coordinates;


/* Type supporting sorting of a PointVector. */
typedef struct SortElementStruct {
  Coordinates   targetPoint;
  gfloat        proportionToCenter;
} TSortElementStruct;



Coordinates  add_points      (const Coordinates a, const Coordinates b);
Coordinates  subtract_points (const Coordinates a, const Coordinates b);
gboolean     equal_points    (const Coordinates a, const Coordinates b);


CompareResult lessCartesian   (const Coordinates *a, const Coordinates *b);
CompareResult moreCartesian   (const Coordinates *a, const Coordinates *b);
CompareResult lessInward      (const TSortElementStruct *a, const TSortElementStruct *b);
CompareResult moreInward      (const TSortElementStruct *a, const TSortElementStruct *b);
CompareResult lessHorizontal  (const Coordinates *a, const Coordinates *b);
CompareResult moreHorizontal  (const Coordinates *a, const Coordinates *b);
CompareResult lessVertical    (const Coordinates *a, const Coordinates *b);
CompareResult moreVertical    (const Coordinates *a, const Coordinates *b);
/*
Inline predicate functions about selection.
On a Pixmap.
*/

/*
!!! Note dithered, partial selection: 8-bits of selection mask, not just a single bit.

Only two values, 0 and 255, are totally unselected or selected.
*/

/*
Inlined since called from the bottleneck.
*/

/*
Is the pixel selected in the corpus?

Here, only pixels fully selected return True.
This is because when target/corpus are differentiated by the same selection,
partially selected will be in the target,
only fully selected (the inverse) will be the corpus.
In the corpus map, distinguishes undesired corpus from desired corpus.
*/
static inline gboolean
isSelectedCorpus (
  const Coordinates coords,
  const Map* const corpusMap
  )
{
  /*
  Was:  != MASK_UNSELECTED); i.e. partially selected was included.
  Now: if partially selected, excluded from corpus.
  */
  return (pixmap_index(corpusMap, coords)[MASK_PIXELEL_INDEX] == MASK_TOTALLY_SELECTED);
  }


/*
Is the pixel selected in the image?
The selection is the mask, created by user to distinguish.
In the target map, distinguishes target(what is synthesized) from context.
*/
static inline gboolean
isSelectedTarget(
  Coordinates coords,
  Map * imageMap )
{
  return (pixmap_index(imageMap, coords)[MASK_PIXELEL_INDEX] != MASK_UNSELECTED);
}

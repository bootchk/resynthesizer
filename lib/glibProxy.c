/*
Proxy for /usr/include/glib-2.0

This proxy obviates recoding to eliminate existing glib use.

Glib is for portability, on top of ANSI C.
This is an alternative.
This is without GPL license.
This is not as robust as Glib: little checking is done.

This is a limited subset: only what is used in imageSynth.
*/

#include "../resynth-config.h"

// Certain configurations use glib defines of structs GRand and GArray
#ifdef SYNTH_USE_GLIB
	#include <libgimp/gimp.h>
#endif


#include <stdlib.h>   // size_t, calloc
#include <string.h>   // memcpy
// Redefines some of glib if gimp.h included above
#include "glibProxy.h"

/*
PRNG
*/
GRand *
s_rand_new_with_seed(guint seed)
{
  srand(seed);
  return (void*) NULL;  // not used
}

guint
s_rand_int_range(
  GRand * prng,   // not used
  guint lowerBound, // Inclusive
  guint upperBound  // !!! Exclusive
  )
{
  if (upperBound<1) return 0; // Prevent division by zero when both bounds 0

  // return rand() % upperBound;   // POOR not adequate if upperBound > RAND_MAX, and cyclical
  upperBound -= 1; // Make it inclusive
  // Conventional formula for random int in range [lowerBound, upperBound] inclusive
  return lowerBound + rand() / (RAND_MAX / (upperBound - lowerBound + 1) + 1);
}

/*
GArray
*/

GArray*
s_array_sized_new (
  gboolean zero_terminated, // unused
  gboolean clear, // unused, always cleared
  guint    elt_size,
  guint    reserved_size)
{
  GRealArray *array = calloc(1, sizeof(GRealArray));
  array->data = calloc(reserved_size, elt_size);
  array->len = 0;
  array->alloc = reserved_size;
  array->elt_size = elt_size;
  return (GArray*) array;
}

void
s_array_free(
  GArray * array,
  int   cascade
  )
{
  // Ignore cascade: always free both
  assert(array->data);
  assert(array);
  free(array->data);
  free(array);  // free GRealArray
  array = NULL;
}


GArray*
s_array_append_vals (
  GArray           *array,
  TConstPointer     data,
  int len
  )
{
  // TODO if each array is typed: (((t*) (void *) array->data [array->len] = data
  GRealArray *rarray = (GRealArray*) array;

  memcpy (
    array->data + rarray->elt_size * array->len,  // pointer arithmetic
    data,
	  rarray->elt_size
	  );
  array->len += 1;
  return array;
}

void
s_array_sort (
  GArray *     array,
	TCompareFunc  compare_func)
{
  GRealArray *rarray = (GRealArray*) array;

  qsort (
    array->data,
	  array->len,
	  rarray->elt_size,
	  compare_func);
}

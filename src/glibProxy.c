/*
Proxy for /usr/include/glib-2.0

Glib is for portability, on top of ANSI C.
This is an alternative.
This is without GPL license.
This is not as robust as Glib: little checking is done.

This is a limited subset: only what is used in synth.
*/

#include <stdlib.h>   // size_t, calloc
#include <string.h>   // memcpy
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
  GArray *array = calloc(1, sizeof(GArray));
  array->data = calloc(reserved_size, elt_size);
  array->len = 0;
  array->reserved_count = reserved_size;
  array->element_size = elt_size;
  return array;
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
  free(array);
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
  memcpy (
    array->data + array->element_size * array->len,  // pointer arithmetic
    data, 
	  array->element_size
	  );
  array->len += 1;
  return array;
}

void
s_array_sort (
  GArray *     array,
	TCompareFunc  compare_func)
{
  qsort (
    array->data,
	  array->len,
	  array->element_size,
	  compare_func);
}

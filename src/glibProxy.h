/*
Proxy for /usr/include/glib-2.0/glib.h

Glib is for portability, on top of std C.
This is an alternative: does a subset of the same thing but without GPL license.
*/

#define guint unsigned int
#define gint int
#define gint32 int
#define gushort short unsigned int

#define gfloat float
#define gdouble double

#define guint8 unsigned char
#define guchar unsigned char
#define gchar char

#define gboolean int
#define FALSE 0
#define TRUE 1

#define G_PI 3.141
/*
#define G_MAXINT 1  // FIXME
G_MAXUINT
G_MAXUSHORT
*/
#define NULL (void*) 0

/*
#define MAX
#define MIN
*/

/*
Dynamic 1D array (sequence, vector.
*/
typedef struct _GArray
{
  gchar *data;
  guint len;
} GArray;
/*
a :
	a GArray.

t :
	the type of the elements.

i :
	the index of the element to return.

Returns :
	the element of the GArray a at the index given by i.
	Cast to type t
*/

#define g_array_index(a,t,i)      (((t*) (void *) (a)->data) [(i)])


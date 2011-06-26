/* 
Fundamental types of the engine.
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

// Functions defined in resynth-map-types

extern void free_map (Map *);

extern
void
new_pixmap(
  Map *,
  guint, 
  guint, 
  guint
  );

extern
void
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
  
extern
void
set_bytemap(
  Map*,
  guchar
  );
  

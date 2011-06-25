/*
Weight a pixelel difference statistically.
*/

/* 
Lookup tables for the statistical function.
For unvectorized, table size is doubled, for negative and positive signed differences,
and a difference is offset by 256 to enter this table.
!!! diff_table is unsigned short, typically 16-bit, no less than 16-bit by C standard.
*/
static gushort diff_table[512];
static guint map_diff_table[512];
#ifdef VECTORIZED
static gushort diff_table2[256];
#endif


static double 
neglog_cauchy(double x) 
{
    return log(x*x+1.0);
}


/* 
Lookup tables for function of pixel differences.
Weight Pixel differences by a statistical distribution: cauchy (not gaussian, see thesis.)
Table is [0, 511]
Pixel difference is [-255, 255].
Symmetrical, inverted bell with min at index 256, max at 1 (-255 + 256) and 511 (+255 + 256)
diff_table[0] is extraordinary: used to hold a limiting MAX,
since the most negative pixel difference of -255 looks up diff_table[1].
Original code used diff_table[0] for a MAX instead of a constant.
*/
void 
make_diff_table(
  gfloat autism, 
  gfloat map_weight
  ) 
{
  gint i;
  
  for(i=-256; i<256; i++) 
  {
      gdouble value = neglog_cauchy(i/256.0/autism) 
                     / neglog_cauchy(1.0/autism) * (float)MAX_WEIGHT;
      diff_table[256+i] = (gushort)value;
      // Note guint: multiplying by MAP_MULTIPLIER carries gushort into guint
      map_diff_table[256+i] = (guint)(i*i*map_weight*MAP_MULTIPLIER);
  }
}



#ifdef VECTORIZED
/* Calculating absolute value of pixel difference so table is only [0,255] */
void make_diff_table2(float autism, float map_weight) 
{
  gint i;
  
  for(i=0;i<256;i++)
  {
    double value = neglog_cauchy(i/256.0/autism) 
                     / neglog_cauchy(1.0/autism) * (float)MAX_WEIGHT;
    diff_table2[i] = (gushort) value;
  }
}
#endif


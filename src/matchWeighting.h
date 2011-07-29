/*
Weight a pixelel difference statistically.
*/


/* 
Upper limit to the finite integral domain of the quantized metric function.
Depends on the size of a Pixelel.
*/
#define LIMIT_DOMAIN 256

// !!! One extra, this makes map[256] the extreme value
// A historical artifact, why not make map[255] the extreme value?
#define LIMIT_DOMAIN_MAP LIMIT_DOMAIN+1   






/*
A function that is proportional to the negative of the natural log of the cauchy PDF
Note in C log() is natural log (ln in some math notation.)

cauchyParam, is a user-passed parameter to the algorithm,
In earlier versions, cauchyParam was hardcoded scaled by 256.
If the size of a Pixelel changes (if we go from 8-bit color to 16-bit), this code might still work.

In math, the denominator is the parameter to the cauchy PDF.
Here, the user's cauchyParam is a fraction (say 0.117).
*/
static inline double 
proportionToNegLnCauchy(
  double x,
  float cauchyParam
  ) 
{
    return log( 
       (x/(cauchyParam*LIMIT_DOMAIN))
      *(x/(cauchyParam*LIMIT_DOMAIN))  // squared
      + 1.0
      );
}

/* 
Upper limit to the finite integral domain of the quantized metric function.
Depends on the size of a Pixelel.
*/


#ifdef SYMMETRIC_METRIC_TABLE
/* 
Lookup tables for metric functions.
For unvectorized, table size is doubled, for negative and positive signed differences,
and a difference is offset by 256 to enter this table.
!!! diff_table is unsigned short, typically 16-bit, no less than 16-bit by C standard.
*/
typedef gushort TPixelelMetricFunc[LIMIT_DOMAIN];
typedef guint TMapPixelelMetricFunc[LIMIT_DOMAIN_MAP];  


/*
Since the metric function has a real (including negative) domain
but is symmetric, generate only the positive side and use abs(diff) elsewhere.
*/
static void 
quantizeImageMetricFunc(
  gfloat cauchyParam, 
  TPixelelMetricFunc corpusTargetMetric
  )
{
  gint i;
  
  for(i=0; i<LIMIT_DOMAIN; i++) 
  {
    /* 
    Scale the metric to the size of int holding range values. 
    metric(i) / metric( domain value yielding maximum range value) * maximum range value
    */
    gdouble value = proportionToNegLnCauchy(i, cauchyParam) / proportionToNegLnCauchy(LIMIT_DOMAIN, cauchyParam) * (float)MAX_WEIGHT;
    corpusTargetMetric[i] = (gushort)value; // Round to quantize
    // TODO rename MAX_WEIGHT and used a typedef for gushort
  }
}

/* 
Similar for the metric function on the maps.
But for maps, the metric function is simply proportional to the square.
!!! Note the domain is not scaled.
*/
static void 
quantizeMapMetricFunc(
  gfloat mapWeightParam,
  TMapPixelelMetricFunc mapsMetric
  )
{
  gint i;
  
  for(i=0; i<LIMIT_DOMAIN_MAP; i++)
  {
    mapsMetric[i] = (guint)(i*i*mapWeightParam*MAP_MULTIPLIER);
  }
}

#else

/* 
Lookup tables for metric functions.
For unvectorized, table size is doubled, for negative and positive signed differences,
and a difference is offset by 256 to enter this table.
!!! diff_table is unsigned short, typically 16-bit, no less than 16-bit by C standard.
*/
// Note this is 255 values on each side of zero, plus zero, is 511 values, with an extreme value at metric[0]
typedef gushort TPixelelMetricFunc[512];
typedef guint TMapPixelelMetricFunc[512];

/*
Since the metric function has a real (including negative) domain
but is symmetric, generate the positive side and use abs(diff) elsewhere.
*/
static void 
quantizeImageMetricFunc(
  gfloat cauchyParam, 
  TPixelelMetricFunc corpusTargetMetric
  )
{
  gint i;
  
  for(i=-LIMIT_DOMAIN; i<LIMIT_DOMAIN; i++)
  {
    /* 
    Scale the metric to the size of int holding range values. 
    metric(i) / metric( domain value yielding maximum range value) * maximum range value
    */
    gdouble value = proportionToNegLnCauchy(i, cauchyParam) / proportionToNegLnCauchy(LIMIT_DOMAIN, cauchyParam) * (float)MAX_WEIGHT;
    corpusTargetMetric[LIMIT_DOMAIN+i] = (gushort)value; // Round to quantize
  }
}

/* 
Similar for the metric function on the maps.
But for maps, the metric function is simply proportional to the square.
!!! Note the domain is not scaled.
*/
static void 
quantizeMapMetricFunc(
  gfloat mapWeightParam,
  TMapPixelelMetricFunc mapsMetric
  )
{
  gint i;
  
  for(i=-LIMIT_DOMAIN; i<LIMIT_DOMAIN; i++)
  {
    mapsMetric[LIMIT_DOMAIN+i] = (guint)(i*i*mapWeightParam*MAP_MULTIPLIER);
  }
}

#endif




/*
Quantize the metric functions.
Two functions: for target/corpus match, and for targetmap/corpusmap match.
The base metric function is continuous in range and domain.
Quantize domain to a finite set of integers (size of Pixelel)
quantize range to the limits of an integer type (gushort).
The integer type for the range should be large enough
so that no two domain values quantize to the same range value.
*/
void 
quantizeMetricFuncs(
  gfloat cauchyParam, 
  gfloat mapWeightParam,
  TPixelelMetricFunc corpusTargetMetric,  // array pointers
  TMapPixelelMetricFunc mapsMetric
  ) 
{
   quantizeImageMetricFunc(cauchyParam, corpusTargetMetric);
   quantizeMapMetricFunc(mapWeightParam, mapsMetric);
}




/*
The outer engine does passes with a closed feedback loop to quit early if no improvement is made.

First pass: target is empty and patches are shotgun patterns, i.e. sparse, mostly from outside the target.

Second pass: target is synthesized but poorly.  Use patches from the poor target to refine the target.  
Patches are non-sparse i.e. contiguous, but not necessarily square or symmetric.
Second pass refines every pixel in the target.

Third and further passes refine a subset of the target.
It is debatable whether third and subsequent passes should continue to refine every pixel in the target.
*/

static void 
refiner(
  TImageSynthParameters parameters,
  TFormatIndices* indices,
  Map* targetMap,
  Map* corpusMap,
  Map* recentProberMap,
  Map* hasValueMap,
  Map* sourceOfMap,
  pointVector targetPoints,
  pointVector corpusPoints,
  pointVector sortedOffsets,
  GRand *prng,
  TPixelelMetricFunc corpusTargetMetric,  // array pointers
  TMapPixelelMetricFunc mapsMetric
  ) 
{
  guint pass;
  TRepetionParameters repetition_params;
  
  prepare_repetition_parameters(repetition_params, targetPoints->len);
  
  for (pass=0; pass<MAX_PASSES; pass++)
  { 
    // See def of synthesize() to understand which parameters are 
    // initialized before the first pass and updated by synthesize()
    guint betters = synthesize(
      pass, 
      &parameters, 
      repetition_params,
      indices,
      targetMap,
      corpusMap,
      recentProberMap,
      hasValueMap,
      sourceOfMap,
      targetPoints,
      corpusPoints,
      sortedOffsets,
      prng,
      corpusTargetMetric, mapsMetric
      );
  
    // nil unless DEBUG
    print_pass_stats(pass, repetition_params[pass][1], betters);
  
    /* Break if a small fraction of target is bettered
    This is a fraction of total target points, 
    not the possibly smaller count of target attempts this pass.
    Or break on small integral change: if ( targetPoints_size / integralColorChange < 10 ) {
    */
    if ( (float) betters / targetPoints->len < (IMAGE_SYNTH_TERMINATE_FRACTION) )
      break;
  }
}

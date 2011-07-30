
/*
Engine does repeated passes with a closed feedback loop to quit early if no improvement is made.

First pass: target is empty and patches are shotgun patterns, i.e. sparse, mostly from outside the target.

Second pass: target is synthesized but poorly.  Use patches from the poor target to refine the target.  
Patches are non-sparse i.e. contiguous, but not necessarily square or symmetric.
Second pass refines every pixel in the target.

Third and further passes refine a subset of the target.
It is debatable whether third and subsequent passes should continue to refine every pixel in the target.

Note the original made passes of increasing size,
resynthesizing early synthesized pixels early.

  Copyright (C) 2010, 2011  Lloyd Konneker

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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


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

/*
Non threaded version, but with same signature and calls to synthesize()
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
  TMapPixelelMetricFunc mapsMetric,
  void (*progressCallback)(int, void*),
  void *contextInfo
  ) 
{
  guint pass;
  TRepetionParameters repetition_params;
  
  // For progress
  guint estimatedPixelCountToCompletion;
  guint completedPixelCount = 0;
  guint priorReportedPercentComplete = 0;

  
  /*
   * Nested function is gcc extension.
   * Called from inside synthesis every 4k target pixels.
   * Convert to a percent of estimated total pixels to synthesis.
   * Callback invoking process every 1 percent.
   * Note synthesis may quit early: then progress makes a large jump.
   */
  void
  deepProgressCallback()
  {
    // !!! Note if estimatedPixelCountToCompletion is small
    // this calls back once for each pass with a percentComplete greater than 100.
    completedPixelCount += IMAGE_SYNTH_CALLBACK_COUNT;
    guint percentComplete = ((float)completedPixelCount/estimatedPixelCountToCompletion)*100;
    if ( percentComplete > priorReportedPercentComplete )
    {
      progressCallback((int) percentComplete, contextInfo);  // Forward callback to calling process
      priorReportedPercentComplete = percentComplete;
    }
  }


  prepare_repetition_parameters(repetition_params, targetPoints->len);
  estimatedPixelCountToCompletion = estimatePixelsToSynth(repetition_params);
  
  for (pass=0; pass<MAX_PASSES; pass++)
  { 
    guint endTargetIndex = repetition_params[pass][1];
    gulong betters = 0; // gulong so can be cast to void *
    
    betters = synthesize(
        &parameters,
        0,      // Unthreaded synthesis is threadIndex 0
        0,      // Unthreaded synthesis startTargetIndex is 0
        endTargetIndex,
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
        corpusTargetMetric,
        mapsMetric,
        deepProgressCallback
        );

    // nil unless DEBUG
    print_pass_stats(pass, repetition_params[pass][1], betters);
    // printf("Pass %d betters %ld\n", pass, betters);
    
    /* Break if a small fraction of target is bettered
    This is a fraction of total target points, 
    not the possibly smaller count of target attempts this pass.
    Or break on small integral change: if ( targetPoints_size / integralColorChange < 10 ) {
    */
    if ( (float) betters / targetPoints->len < (IMAGE_SYNTH_TERMINATE_FRACTION) ) 
    {
      // printf("Quitting early after %d passes. Betters %ld\n", pass+1, betters);
      break;
    }
    
    // Simple progress: percent of passes complete.
    // This is not ideal, a maximum of MAX_PASSES callbacks, typically six.
    // And the later passes are much shorter than earlier passes.
    // progressCallback( (int) ((pass+1.0)/(MAX_PASSES+1)*100), contextInfo);
  } // end pass
}

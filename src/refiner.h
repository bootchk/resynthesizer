
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
Threading.
*/
#include <pthread.h>

// Struct for args to synthesize
typedef struct synthArgsStruct {
  TImageSynthParameters *parameters;  // IN
  gboolean isStartup;
  guint startTargetIndex;
  guint endTargetIndex;  // IN // array pointers
  TFormatIndices* indices;  // IN
  Map * targetMap;      // IN/OUT
  Map* corpusMap;       // IN
  Map* recentProberMap; // IN/OUT
  Map* hasValueMap;     // IN/OUT
  Map* sourceOfMap;     // IN/OUT
  pointVector targetPoints; // IN
  pointVector corpusPoints; // IN
  pointVector sortedOffsets; // IN
  GRand *prng;
  gushort * corpusTargetMetric;   // array pointers TPixelelMetricFunc
  guint * mapsMetric;             // TMapPixelelMetricFunc 
} SynthArgs;

// Chunk args into struct to pass to thread func
static void
newSynthesisArgs(
  SynthArgs* args,
  TImageSynthParameters *parameters,  // IN
  gboolean isStartup,
  guint startTargetIndex,
  guint endTargetIndex,  // IN
  TFormatIndices* indices,  // IN
  Map * targetMap,      // IN/OUT
  Map* corpusMap,       // IN
  Map* recentProberMap, // IN/OUT
  Map* hasValueMap,     // IN/OUT
  Map* sourceOfMap,     // IN/OUT
  pointVector targetPoints, // IN
  pointVector corpusPoints, // IN
  pointVector sortedOffsets, // IN
  GRand *prng,
  TPixelelMetricFunc corpusTargetMetric,  // array pointers
  TMapPixelelMetricFunc mapsMetric
  )
{
  
  args->parameters = parameters;
  args->isStartup = isStartup;
  args->startTargetIndex = startTargetIndex; 
  args->endTargetIndex = endTargetIndex; 
  args->indices = indices; 
  args->targetMap = targetMap; 
  args->corpusMap = corpusMap;      
  args->recentProberMap = recentProberMap;
  args->hasValueMap = hasValueMap;
  args->sourceOfMap = sourceOfMap;
  args->targetPoints = targetPoints;
  args->corpusPoints = corpusPoints;
  args->sortedOffsets = sortedOffsets;
  args->prng = prng;
  args->corpusTargetMetric = corpusTargetMetric;
  args->mapsMetric = mapsMetric;
}


static void
splitTargetPoints(
  pointVector targetPoints,
  pointVector targetPoints1,
  pointVector targetPoints2
  )
{
  guint size = 0;
  
  targetPoints1 = g_array_sized_new (FALSE, TRUE, sizeof(Coordinates), size); /* reserve */
  /*for (i=0; 
  g_array_append_val(*targetPoints1, coords);
  */
}


static void *
synthesisThread(void * uncastArgs)
{
  SynthArgs* args = (SynthArgs *) uncastArgs;
  
// Unpack args
  TImageSynthParameters * parameters  = args->parameters;
  gboolean isStartup                  = args->isStartup;
  guint startTargetIndex              = args->startTargetIndex;
  guint endTargetIndex                = args->endTargetIndex;
  TFormatIndices* indices             = args->indices; 
  Map * targetMap                     = args->targetMap; 
  Map* corpusMap                      = args->corpusMap;      
  Map* recentProberMap                = args->recentProberMap;
  Map* hasValueMap                    = args->hasValueMap;
  Map* sourceOfMap                    = args->sourceOfMap;
  pointVector targetPoints            = args->targetPoints;      
  pointVector corpusPoints            = args->corpusPoints;
  pointVector sortedOffsets           = args->sortedOffsets;
  GRand *prng                         = args->prng;
  gushort * corpusTargetMetric        = args->corpusTargetMetric; // array pointers TPixelelMetricFunc
  guint * mapsMetric                  = args->mapsMetric;  
  
  gulong betters = synthesize(  // gulong so can be cast to void *
      parameters,
      isStartup,
      startTargetIndex,
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
      mapsMetric
      );
return (void*) betters;
}

static void
startThread(
  SynthArgs* args,
  pthread_t* thread,
  guint start,
  guint end,
  guint pass,
  TImageSynthParameters* parameters,
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
  newSynthesisArgs(
    args,
    parameters,
    (pass == 0),
    start,      // thread specific
    end,        // thread specific
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
    mapsMetric
    );
    pthread_create(thread, NULL, synthesisThread, (void * __restrict__) args);
}


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
  
  SynthArgs args1;
  SynthArgs args2;
  SynthArgs args3;
  SynthArgs args4;
  
  prepare_repetition_parameters(repetition_params, targetPoints->len);
    
  // Get progress started with small percent
  progressCallback( 2, contextInfo);
  
  for (pass=0; pass<MAX_PASSES; pass++)
  { 
    guint endTargetIndex = repetition_params[pass][1];
    gulong betters = 0;
    gulong temp;
    
    // Synthesize in threads
    pthread_t threadID1;
    pthread_t threadID2;
    pthread_t threadID3;
    pthread_t threadID4;
    
    pthread_mutex_init(&mutex, NULL);
    
    // Threads split the work in the middle of targetPoints
    guint split1 = endTargetIndex / 4 + 1;
    guint split2 = endTargetIndex / 2 + 1;
    guint split3 = (3/4.0 ) * endTargetIndex + 1;
    
    startThread(
      &args1, &threadID1, 0, split1, pass, // thread specific
      &parameters,
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
   
    startThread(
      &args2, &threadID2, split1, split2, pass,  // thread specific
      &parameters,
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
      
    startThread(
      &args3, &threadID3, split2, split3, pass,  // thread specific
      &parameters,
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
      
    startThread(
      &args4, &threadID4, split3, endTargetIndex, pass,  // thread specific
      &parameters,
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
    
    pthread_join(threadID1, (void**)&temp);
    betters += temp;
    pthread_join(threadID2, (void**)&temp);
    betters += temp;
    pthread_join(threadID3, (void**)&temp);
    betters += temp;
    pthread_join(threadID4, (void**)&temp);
    betters += temp;
    
    // See def of synthesize() to understand which parameters are 
    // initialized before the first pass and updated by synthesize()
    /*
    gulong betters = synthesize(
      &parameters, 
      lastTargetInPass,
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
    */
    
    // nil unless DEBUG
    print_pass_stats(pass, repetition_params[pass][1], betters);
    printf("Pass %d betters %ld\n", pass, betters);
    
    /* Break if a small fraction of target is bettered
    This is a fraction of total target points, 
    not the possibly smaller count of target attempts this pass.
    Or break on small integral change: if ( targetPoints_size / integralColorChange < 10 ) {
    */
    if ( (float) betters / targetPoints->len < (IMAGE_SYNTH_TERMINATE_FRACTION) ) 
    {
      printf("Quitting early after %d passes. Betters %ld\n", pass+1, betters);
      break;
    }
    
    // Simple progress: percent of passes complete.
    // This is not ideal, a maximum of MAX_PASSES callbacks, typically six.
    // And the later passes are much shorter than earlier passes.
    progressCallback( (int) ((pass+1.0)/(MAX_PASSES+1)*100), contextInfo);
  }
}


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
Threaded version, using alternative 1.

Alternative 1:
Each pass divides targetPoints among threads and rejoins before the next pass.
Here, one thread may be reading pixels that another thread is synthesizing,
but no two threads are synthesizing the same pixel.

Alternative 2:
one thread is started for each pass, with each thread working on a prefix of the same targetPoints.
This is more like Harrison's implementation,
where early synthesized pixels are repeated sooner rather than after all pixels have been synthesized once.
There would be more contention, and write contention, over target pixels.
Probably each thread would need to lag already running threads.
Each thread would not have the same amount of work.
Sept. 2011.  One experiment shows it is no faster, and produces different, grainy results.
*/

#ifndef SYNTH_USE_GLIB_THREADS
  // !!! Note if not def: if using glib threads, glib.h already included
  #include <pthread.h>
#endif

// TODO this is a hack
#include "../enginePlugin/debug.h"

// When synthesize() is threaded, it needs a single argument.
// Wrapper struct for single arg to synthesize
typedef struct synthArgsStruct {
  TImageSynthParameters *parameters;  // IN
  guint threadIndex;
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
  void (*deepProgressCallback)();         // void func(void)
  ProgressRecordT *progressRecord;
  int* cancelFlag;  // flag set when canceled
} SynthArgs;




// Pack args into wrapper struct to pass to thread func
static void
newSynthesisArgs(
  SynthArgs* args,
  TImageSynthParameters *parameters,  // IN
  guint threadIndex,
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
  TMapPixelelMetricFunc mapsMetric,
  void (*deepProgressCallback)(),
  ProgressRecordT* progressRecord,
  int* cancelFlag
  )
{
  args->parameters = parameters;
  args->threadIndex = threadIndex;
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
  args->deepProgressCallback = deepProgressCallback;
  args->progressRecord = progressRecord;
  args->cancelFlag = cancelFlag;
}



static void *
synthesisThread(void * uncastArgs)
{
  SynthArgs* args = (SynthArgs *) uncastArgs;

  // Unpack wrapped args
  TImageSynthParameters * parameters  = args->parameters;
  guint threadIndex                   = args->threadIndex;
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
  void (*deepProgressCallback)()      = args->deepProgressCallback;
  ProgressRecordT * progressRecord    = args->progressRecord;
  int* cancelFlag                     = args->cancelFlag;


  gulong betters = synthesize(  // gulong so can be cast to void *
      parameters,
      threadIndex,
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
      mapsMetric,
      deepProgressCallback,
      progressRecord,	// parameters to progress callback.  progressRecord is in stack frame of refinerThreaded().
      cancelFlag
      );
  return (void*) betters;
}

static void
startThread(
  SynthArgs* args,
#ifdef SYNTH_USE_GLIB_THREADS
  GThread** thread,
#else
  pthread_t* thread,
#endif
  guint threadIndex,
  guint startIndexTargetsPrefix,
  guint endIndexTargetsPrefix,
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
  TMapPixelelMetricFunc mapsMetric,
  void (*deepProgressCallback)(),
  ProgressRecordT *progressRecord,
  int* cancelFlag
  )
{
  g_debug ("%s", G_STRFUNC);

  newSynthesisArgs(
    args,
    parameters,
    threadIndex, 
    startIndexTargetsPrefix,
    endIndexTargetsPrefix,
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
    deepProgressCallback,
    progressRecord,
    cancelFlag
    );

#ifdef SYNTH_USE_GLIB_THREADS
  GError* error = NULL;

  // Deprecated and not needed since glib-2.32
  // g_assert(g_thread_supported());

  // old, deprecated: *thread = g_thread_create(synthesisThread, (void * __restrict__) args, TRUE, &error);
  *thread = g_thread_try_new(NULL, synthesisThread, (void * __restrict__) args, &error);
  if (error != NULL)
  {
    printf("Error creating thread: %s\n", error->message);
    // Only for debugging:  app will likely crash.  Caller does not handle this error sufficiently.
    // *thread is probably NULL
  }
#else
    pthread_create(thread, NULL, synthesisThread, (void * __restrict__) args);
#endif
}





// Alternative 1

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
  void *contextInfo,
  int* cancelFlag
  )
{
  guint pass;
  TRepetionParameters repetition_params;

  // Threaded: use atomic add and mutexProgress when updating progress
  // !!! This is owned by parent, updated by child threads executing callback function deepProgressCallback.
  ProgressRecordT progressRecord;


  // Synthesize in threads.  Note proxies in glibProxy.h for POSIX threads
#ifdef SYNTH_USE_GLIB_THREADS
  GThread* threads[THREAD_LIMIT];
#else
  pthread_t threads[THREAD_LIMIT];
#endif
  // If not using glib proxied to pthread by glibProxy.h
  g_mutex_init(&mutex);  // defined in synthesize.h

  g_debug ("%s max threads %d", G_STRFUNC, THREAD_LIMIT);
#ifdef SYNTH_USE_GLIB_THREADS
  static GMutex mutexProgress;
#else
  pthread_mutex_t mutexProgress;
#endif
  g_mutex_init(&mutexProgress);


  SynthArgs synthArgs[THREAD_LIMIT];

  debug("refinerThreaded\n");

  prepare_repetition_parameters(repetition_params, targetPoints->len);

  initializeThreadedProgressRecord(
    &progressRecord,
    repetition_params,
    progressCallback,
    contextInfo,
    &mutexProgress);

  // Assert threading system is init at startup time, after glib 2.32

  for (pass=0; pass<MAX_PASSES; pass++)
  {
    guint endTargetIndex = repetition_params[pass][1];
    gulong betters = 0;


    for (guint threadIndex=0; threadIndex<THREAD_LIMIT; threadIndex++)
    {
      startThread(
        &synthArgs[threadIndex], 
        &threads[threadIndex],  // Thread instance 
        threadIndex,            // zero based ordinal of thread
        /*
        Every thread gets a prefix of targetPoints [0, endTargetIndex],
        since each repetition i.e. pass has a different endTargetIndex,
        i.e. works on a shrinking prefix of targetPoints.

        Each thread works on a different subset of the prefix, splits it modulo threadIndex.
        I.E. starts at the target point of its thread ordinal and skips every THREAD_LIMIT target points.
        I.E. combs the prefix, not slices it.
        
        !!! 0 and endTargetIndex describe the size of the prefix of target points,
        not which target points the thread combs over.
        */
        0, 
        endTargetIndex,      
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
        corpusTargetMetric, mapsMetric,
        deepProgressCallback,
        &progressRecord,
        cancelFlag
        );
   }

   // Wait for threads to complete; rejoin them
   for (guint threadIndex=0; threadIndex<THREAD_LIMIT; threadIndex++)
   {
     gulong temp;

#ifdef SYNTH_USE_GLIB_THREADS
     temp = (gulong) g_thread_join(threads[threadIndex]);       // cast return value from gpointer to gulong
#else
     pthread_join(threads[threadIndex], (void**)&temp);
#endif
     betters += temp;
   }


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
  }
}









#ifdef SYNTH_THREADED2
// Alternative 2


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
  void *contextInfo,
  int* cancelFlag
  )
{
  TRepetionParameters repetition_params;


  // Synthesize in threads.  Note proxies in glibProxy.h for POSIX threads
#ifdef SYNTH_USE_GLIB_THREADS
  GThread* threads[THREAD_LIMIT];
#else
  pthread_t threads[THREAD_LIMIT];
#endif
  // If not using glib proxied to pthread by glibProxy.h
  GMutex mutexProgress;
  g_mutex_init(&mutex);  // defined in synthesize.h
  g_mutex_init(&mutexProgress);


  SynthArgs synthArgs[THREAD_LIMIT];


  // For progress
  guint estimatedPixelCountToCompletion;
  guint completedPixelCount = 0;
  guint priorReportedPercentComplete = 0;

  /*
   * Nested function is gcc extension.
   * Called from inside synthesize() every 4k target pixels.
   * Convert to a percent of estimated total pixels to synthesis.
   * Callback invoking process every 1 percent.
   * Note synthesis may quit early: then progress makes a large jump.
   */
  void
  deepProgressCallback()
  {
    completedPixelCount += IMAGE_SYNTH_CALLBACK_COUNT;
    guint percentComplete = ((float)completedPixelCount/estimatedPixelCountToCompletion)*100;
    if ( percentComplete > priorReportedPercentComplete )
    {
      g_mutex_lock(&mutexProgress);       // mutex calls to GUI i.e. gdk, gtk which are thread aware but not thread safe
      // Alternatively, use gdk_thread_enter()
      progressCallback((int) percentComplete, contextInfo);  // Forward deep progress callback to calling process
      g_mutex_unlock(&mutexProgress);
      priorReportedPercentComplete = percentComplete;
    }
  }

  prepare_repetition_parameters(repetition_params, targetPoints->len);
  estimatedPixelCountToCompletion = estimatePixelsToSynth(repetition_params);

  // Start one thread for what were formerly passes
  g_assert(THREAD_LIMIT > MAX_PASSES);

  gulong betters = 0;
  guint threadIndex;
  for (threadIndex=0; threadIndex<MAX_PASSES; threadIndex++)
  {
    startThread(
      &synthArgs[threadIndex], &threads[threadIndex],
      threadIndex,        // Every thread does not split modulo, see refinerThreaded.c changes
      0, repetition_params[threadIndex][1], // Every thread works on a prefix of targetPoints
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
      corpusTargetMetric, mapsMetric,
      deepProgressCallback,
      cancelFlag
      );
  }

  // Wait for threads to complete; rejoin them
  for (threadIndex=0; threadIndex<MAX_PASSES; threadIndex++)
  {
    gulong temp;

  #ifdef SYNTH_USE_GLIB_THREADS
     temp = (gulong) g_thread_join(threads[threadIndex]);       // cast return value from gpointer to gulong
  #else
     pthread_join(threads[threadIndex], (void**)&temp);
  #endif
     betters += temp;
  }
}
#endif
